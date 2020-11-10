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
  ungroup()

print(FCCP_high.pct, n=100)
str(FCCP_high.pct)

#Change the treatment category treatment to baf
FCCP_high.pct$treatment[FCCP_high.pct$treatment == 'treatment'] <- 'baf'


#make data frame of 
exp <- subset(FCCP_high.pct, exposure == "experimental", 
              select = c(treatment, antpost, indvd, area, area.pct.change))

print(exp, n=24)

# makes separate anterior/ posterior dataframes (may not need with better mean and sd calcs below)
anterior <- subset(FCCP_high.pct, antpost == "ant", 
                   select = c(exposure, treatment, indvd, area, area.pct.change))

posterior <- subset(FCCP_high.pct, antpost == "post", 
                    select = c(exposure, treatment, indvd, area, area.pct.change))

print(anterior, n=30)
print(posterior, n=30)

## compute means
means.sd <-
  FCCP_high.pct %>%
  select(-indvd) %>% ## exclude indvd
  group_by(exposure, treatment, antpost) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), 
                   tibble::lst(mean = mean, sd = sd)))

print(means.sd, n= 50)

#plot the two dataframes together
ggplot(data = anterior, aes(y= area.pct.change , x= min, group= larva, colour= treatment)) +
  geom_line() +
  geom_line(data = posterior, linetype= "dashed") +
  labs(x = "min", y = "% change") +
  theme_classic()

