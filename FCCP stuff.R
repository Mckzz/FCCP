install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
rm(X1uM)

FCCP_high <- as_tibble(X1uM) %>%
  rename(treatment = sac)

print(FCCP_high, n= 50)

#making % change column
FCCP_high.pct <- FCCP_high %>%
  group_by(treatment, antpost, indvd) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  ungroup() #why the ungroup?

print(FCCP_high.pct, n=100)
str(FCCP_high.pct)

#Change the treatment category treatment to baf
FCCP_high.pct$treatment[FCCP_high.pct$treatment == 'treatment'] <- 'baf'

print(FCCP_high.pct, n= 48)

exp <- subset(FCCP_high.pct, exposure == "experimental", 
                      select = c(treatment, antpost, area, indvd, area.pct.change))
print(exp, n=24)

## compute means and sds
means.sd <-
  FCCP_high.pct %>% #still has baseline areas
  select(-indvd) %>% 
  group_by(exposure, treatment, antpost) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), #how does the across everything work? is it that chr columns get created based on num columns that are summarized?
                   tibble::lst(mean = mean, sd = sd)))

print(means.sd, n= 50)

#take only esperimental means/ sd 
exp.mean.sd <- subset(means.sd, exposure == "experimental", 
              select = c(treatment, antpost, area_mean, area_sd, area.pct.change_mean, area.pct.change_sd))

print(exp.mean.sd, n=24)



### plotting ###
# makes strip charts with mean and stdv
ggplot(exp, aes(y = area.pct.change, x = treatment, colour= antpost)) +
  geom_jitter(size = 2, pch = 1, position = position_dodge(width = 0.7)) +
  labs(x = "treatment", y = "% change") + #labels axes
  stat_summary(
    fun.data = mean_sdl, position = position_dodge(width = 0.5), geom = "errorbar", width = 0.1, fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 3) +
  theme_classic()   #takes out background

#not quite working with trying to offset the anteriors and posteriors. tried dodge and nudge 
exp %>%
  ggplot(aes(x= treatment)) +
  geom_point(position = position_jitter(width = 0.03),
             pch= 1,  
             aes(y = area.pct.change, 
                 group= antpost, 
                 colour = antpost,)) 

  geom_point(position = position_jitter(width = 0.03), 
             pch= 1, colour = "red", 
             aes(y = area, group = larva)) +
  geom_line(data = means, size= 1, color= "red", 
            aes(pH, area_mean)) +
  geom_line(data = means, size= 1, color= "blue", 
            aes(pH, width_mean)) +
  geom_point(data = means, pch= 19, color= "red", size= 4, 
             aes(pH, area_mean)) +
  geom_point(data = means, pch= 19, color= "blue", size= 4, 
             aes(pH, width_mean)) +
  geom_errorbar(data = means, 
                mapping = aes(x = pH,
                              ymin = area_mean - area_sd,
                              ymax = area_mean + area_sd), 
                width = 0.05,
                size = 0.75) +
  geom_errorbar(data = means, 
                mapping = aes(x = pH,
                              ymin = width_mean - width_sd,
                              ymax = width_mean + width_sd), 
                width = 0.05,
                size = 0.75) +
  labs(x = "pH", y = "% change") +
  labs(color="Dimension") +
  theme_classic()








