
#not certain these are needed
library(tidyverse)#DATA MANIPULATION
library(ggplot2)#PLOTS
library(animation)

#these are definitely needed
library(ggplot2)
library(gganimate)
library(transformr)

theme_set(theme_bw())

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


anim2












