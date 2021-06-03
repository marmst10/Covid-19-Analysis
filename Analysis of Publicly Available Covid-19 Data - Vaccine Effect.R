
#attach needed libraries
library(tidyverse)#DATA MANIPULATION
library(ggplot2)#PLOTS
library(dplyr)#DATA MANIPULATION
library(ggpubr)#PLOTS
library(car)#ANOVA
library(nortest)#NORMALITY TESTS
library(moments)#SKEWNESS
library(geoR)#BOXCOXTRANSFORMATIONS
library(gamlss)#checking fit of distributions
library(gamlss.dist)#checking fit of distributions
library(gamlss.add)#checking fit of distributions
library(lubridate)#for week aggregating
library(Hmisc)#lag function
library(caret)#RMSE and R2 functions
library(ggpmisc)#show equation and r2 on graph
library(graphics)#curved bestfit lines
library(animation)
library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(transformr)

#read in source data (csv)
covid <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Analytics Showcase/owid-covid-data 03 02 21.csv")

#remove world totals
covid2 <- subset(covid, continent!="")

#make NA values 0
covid2$new_cases_per_million[is.na(covid2$new_cases_per_million)] <- 0
covid2$people_fully_vaccinated_per_hundred[is.na(covid2$people_fully_vaccinated_per_hundred)] <- 0

#need to reformat date for date filtering
covid2$date <- as.Date(covid2$date, format = "%Y-%m-%d")

#sort by date then location
covid2 <- covid2 %>% arrange(location, date)

#add in running maximum
covid2$people_fully_vaccinated_per_hundred <- ave(covid2$people_fully_vaccinated_per_hundred, 
                                                  covid2$location, 
                                                  FUN=cummax)


#write a loop to iterate over many days, pull R^2 and other parameters from many combinations of days
#also create a 3d map of vaccines, cases, days


covid22 <- covid2[,c("location", "date", "new_cases_per_million", "people_fully_vaccinated_per_hundred")]

#empty lists for fit parameters across days
intercept <- rep(NA, 35)
vaccine_effect_coef <- rep(NA, 35)
rsquared <- rep(NA, 35)
adjrsquared <- rep(NA, 35)


for(i in 1:28){
  
  covidloop <- covid22 %>%
    group_by(location) %>%
    mutate(dif_new_cases_per_million = new_cases_per_million - Lag(new_cases_per_million, shift = i),
           lag_people_fully_vaccinated_per_hundred = Lag(people_fully_vaccinated_per_hundred, shift = i)
    )
  
  covidloop <- subset(covidloop, !is.na(lag_people_fully_vaccinated_per_hundred))
  covidloop <- subset(covidloop,lag_people_fully_vaccinated_per_hundred != 0)
  
  covidloop$log_lag_people_fully_vaccinated_per_hundred <- log(covidloop$lag_people_fully_vaccinated_per_hundred)
  
  covidloop$lag <- i
  
  fitloop <- lm(dif_new_cases_per_million ~ log_lag_people_fully_vaccinated_per_hundred, data=covidloop)
  
  intercept[i] <- fitloop$coefficients[1]
  vaccine_effect_coef[i] <- fitloop$coefficients[2]
  rsquared[i] <- summary(fitloop)$r.squared
  adjrsquared[i] <- summary(fitloop)$adj.r.squared
  
  if(i==1){
    covid_combined_lag <- covidloop
  } else {
    covid_combined_lag <- rbind(covid_combined_lag, covidloop)
  }
  
}

intercept
vaccine_effect_coef
rsquared
adjrsquared

list <- rep(NA, 35)

for(i in 1:35){list[i] <- i
}


covid_combined_lag_2 <- subset(covid_combined_lag, lag > 6)
max(covid_combined_lag_2$log_lag_people_fully_vaccinated_per_hundred)

p <- ggplot(
  covid_combined_lag_2, 
  aes(x = log_lag_people_fully_vaccinated_per_hundred, y=dif_new_cases_per_million)
) +
  geom_point() + 
  scale_x_continuous(limits = c(-4.60518,3.6889),
                     breaks = c(-4.60518, -2.303, 0, 2.303, 3.6889), 
                     label=c("0.01 %","0.1 %","1 %","10 %","40 %")) +
  theme(text = element_text(size=20)) +  
  labs(x = "Percent Vaccinated (Log Scale)", y = "Difference in New Cases per Million") + 
  geom_smooth(method='lm', formula= y~x)

p

anim <- p +
  transition_states(lag,
                    transition_length = 1,
                    state_length = 4) +
  ggtitle('{closest_state} Day Difference')

anim2 <- animate(anim,
                 nframes = 300, 
                 height = 400, 
                 width = 800)

anim_save("vaccine_animation_03_02_21.gif")
anim2


#how many observations per lag?
covid_combined_lag_3 <- covid_combined_lag_2 %>%
  dplyr::group_by(lag) %>%
  dplyr::count(lag)





covid_loop_28 <- subset(covid_combined_lag_2, lag == 28)

#these don't work in this script, need to filter by lag to get these.
summary(covid_loop_28)

covidcomp1 <- subset(covid_loop_28, 
                     lag_people_fully_vaccinated_per_hundred < 8)

covidcomp2 <- subset(covid_loop_28, 
                     lag_people_fully_vaccinated_per_hundred >= 8 & 
                     lag_people_fully_vaccinated_per_hundred < 16)

covidcomp3 <- subset(covid_loop_28, 
                     lag_people_fully_vaccinated_per_hundred >= 16)

covidcomp <- c(mean(covidcomp1$dif_new_cases_per_million),
               mean(covidcomp2$dif_new_cases_per_million),
               mean(covidcomp3$dif_new_cases_per_million))

covidcomp

plot(c(1,2,3), covidcomp)

covidcomp1$group <- as.factor(1)
covidcomp2$group <- as.factor(2)
covidcomp3$group <- as.factor(3)

covidcomp4 <- rbind(covidcomp1,covidcomp2,covidcomp3)
levels(covidcomp4$group)

ggboxplot(covidcomp4, x = "group", y = "dif_new_cases_per_million", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Difference in Cases Per Million", xlab = "Percent Vaccinated Bins")

res.aov <- aov(dif_new_cases_per_million ~ group, data = covidcomp4)
# Summary of the analysis
summary(res.aov)#group affects cases

#residuals versus fitted
plot(res.aov, 1)#not the best

#From the output above we can see that the p-value is not less than the significance 
#level of 0.05. This means that there is no evidence to suggest that the variance across 
#groups is statistically significantly different. Therefore, we can assume the homogeneity 
#of variances in the different treatment groups.
leveneTest(dif_new_cases_per_million ~ group, data = covidcomp4)

#normality?
plot(res.aov, 2)#this is very problematic

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )#normality is violated

#kruskal wallis rank sum test - shows group is significant
kruskal.test(dif_new_cases_per_million ~ group, data = covidcomp4)

quantile(covidcomp4$lag_people_fully_vaccinated_per_hundred, c(.1,.2,.3,.4,.5,.6,.7,.8,.9)) 

plot(density(covidcomp4$log_lag_people_fully_vaccinated_per_hundred))

#for bc plot in appendix
bc <- boxcox(covid_loop_20$lag_people_fully_vaccinated_per_hundred ~ covid_loop_20$dif_new_cases_per_million)
(lambda <- bc$x[which.max(bc$y)])


fitloop.res = resid(fitloop)


plot(covid_loop_28$log_lag_people_fully_vaccinated_per_hundred, fitloop.res, 
            ylab="Residuals", xlab="ln(Percent Vaccinated)") 
abline(0, 0)

leveneTest(dif_new_cases_per_million ~ log_lag_people_fully_vaccinated_per_hundred, data = covid_loop_28)

qplot(covid2$new_vaccinations,#xaxis
      covid2$new_cases,#yaxis
      geom=c("point"),
      colour = covid2$continent,
      main="Scatterplot of Daily Case Numbers by Daily Vaccinations",#title
      xlab="Daily New Vaccines",
      ylab="Daily New Cases")

plot(list, vaccine_effect_coef)


qplot(list,#xaxis
      vaccine_effect_coef,#yaxis
      geom=c("point", "line"),
      main="Model Coefficients over Difference in Days",#title
      xlab="Number of Days Difference",
      ylab="Predictor Coefficient (Slope)")

qplot(list,#xaxis
      adjrsquared,#yaxis
      geom=c("point", "line"),
      main="R Squared Adjusted over Difference in Days",#title
      xlab="Number of Days Difference",
      ylab="R Squared Adjusted")














