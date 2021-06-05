install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(lme4)

dropplets <- as_tibble(X1uM) %>%
  rename(treatment = sac)

print(dropplets, n= 50)

#making % change column
dropplets.pct <- dropplets %>%
  group_by(drug, treatment, antpost, indvd) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  ungroup() #why the ungroup? 

print(dropplets.pct, n=100)
str(dropplets.pct)

#Change the treatment category treatment to name of what ever drug
#dropplets.pct$treatment[dropplets.pct$treatment == 'treatment'] <- 'FCCP'

#print(dropplets.pct, n= 48)

#dataframe with only the experimental (non-zero) %changes (so can drop exposure column)
exp <- subset(dropplets.pct, exposure == "experimental", 
                      select = c(drug, treatment, antpost, area, indvd, area.pct.change))
print(exp, n=48)

#make means of ant and post %changes
means.antpost <-
  exp %>% #still has baseline areas
  select(-antpost) %>% 
  group_by(drug, treatment, indvd) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), 
                   tibble::lst(mean = mean))) %>%
  mutate(treatment = as.factor(treatment))

print(means.antpost, n= 50)

## compute means and sds (not used for jitter with stat summary)
means.sd <-
  means.antpost %>% #still has baseline areas
  select(-indvd) %>% 
  group_by(drug, treatment) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), 
                   tibble::lst(mean = mean, sd = sd)))

print(means.sd, n= 50)

#take only esperimental means/ sd 
##exp.mean.sd <- subset(means.sd, exposure == "experimental", 
  ##            select = c(treatment, antpost, area_mean, area_sd, area.pct.change_mean, area.pct.change_sd))

##print(exp.mean.sd, n=24)



### plotting ###
# makes strip charts with mean and stdv
ggplot(means.antpost, 
       aes(y = area.pct.change_mean, 
           x = drug, colour= treatment)) +
  geom_jitter(size = 2, 
              pch = 1, 
              position = position_dodge(width = 0.7)) +
  labs(x = "treatment", y = "% change") + #labels axes
  stat_summary(
    fun.data = mean_sdl, 
    position = position_dodge(width = 0.5), 
    geom = "errorbar", 
    width = 0.1, 
    fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    position = position_dodge(width = 0.5),
    size = 3) +
  theme_classic() + #takes out background
  theme(legend.position = "none")







#not quite working (using self calculated means and SD) with trying to offset the anteriors and posteriors. tried dodge and nudge 

ggplot(means.antpost, aes(y = area.pct.change_mean, x = treatment, colour= treatment)) +
  geom_jitter(size = 2, 
              pch = 1, 
              position = position_dodge(width = 0.7)) +
  geom_point(data= means.sd, 
             aes(y = area.pct.change_mean_mean, 
                 x = treatment, 
                 colour= treatment)) +
  geom_errorbar(data= means.sd,
                aes(x= treatment, 
                    ymin= area.pct.change_mean_mean - area.pct.change_mean_sd, 
                    ymax= area.pct.change_mean_mean + area.pct.change_mean_sd), 
                group= "treatment",
                width= 2) +
  theme_classic()






########### stats ###########

stats_data <- 
  exp %>% 
  mutate(treatment = as_factor(treatment)) %>% 
  mutate(antpost = as_factor(antpost)) %>%
  mutate(indvd = as_factor(indvd))
  
  
print(stats_data, n= 24)


# doesn't account for random effects
#mod1 <-
#  lm(area.pct.change ~ treatment * indvd, 
#     data = stats_data)
#summary(mod1)

#mod1.aov <- aov(mod1)
#TukeyHSD(mod1.aov)

####

mcmod <-
  MCMCglmm::MCMCglmm(
    area.pct.change ~ treatment, random = ~indvd,
    data = stats_data, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
# produces a mean for the tratment that is the 
# difference from the intercept (here, the control), actual mean
# is ~18.7. this - control mean = ~5.7


summary(mcmod)

mean(mcmod$VCV[,1]/(mcmod$VCV[,1] + mcmod$VCV[,2]))

print(stats_data, n= 24)

stats_data <- stats_data %>%
  group_by(treatment) %>%
  sd(area.pct.change)

#trying supressing the intercept
mcmod.sup <-
  MCMCglmm::MCMCglmm(
    area.pct.change ~ treatment-1, random = ~indvd,
    data = stats_data, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE)

summary(mcmod.sup)

#trying other glms

lmemod <- lmer(area.pct.change ~ treatment, 
               random = ~ 1|indvd, data = stats_data)



#### just playing around ####
# making mean zero control data, but with same spread 
# by subtracting control mean from all data

stats_data_minmean <- stats_data %>% 
  mutate(
    area.pct.change = ((area.pct.change - 5.686)))

print(stats_data_minmean, n=24)

mcmod.minmean <-
  MCMCglmm::MCMCglmm(
    area.pct.change ~ treatment, random = ~indvd,
    data = stats_data_minmean, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )

summary(mcmod.minmean)

#Iterations = 300001:1299001
#Thinning interval  = 1000
#Sample size  = 1000 

#DIC: 148.5457 

#G-structure:  ~indvd

#post.mean  l-95% CI u-95% CI eff.samp
#indvd    0.7256 5.968e-17    1.574     1000

#R-structure:  ~units

#           post.mean l-95% CI u-95% CI eff.samp
#units     26.48    12.82    45.16     1000

#Location effects: area.pct.change ~ treatment 

#                       post.mean l-95% CI u-95% CI eff.samp  pMCMC    
#(Intercept)            5.757    2.728    8.211     1000 <0.001 ***    ## control
#  treatmenttreatment   12.750    9.097   16.754     1000 <0.001 ***   ## treatment
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## post.mean is the posterior mean (not sac, but a stat thing). it's like an effect size, but is difference from zero. 
## so subtract control from treatment for effect size of drug