library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(janitor)
library(car)
library(igraph)
library(ggsci)
library(netdiffuseR)

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20))

male_sum_dat <- read.csv("males_summer_2023/data/male_summary_data.csv") %>% 
                filter(day == "both") %>% 
                mutate(insem_rate = inseminations/2) %>% 
                mutate(male_mount_rate = male_mounts/mounts) %>% 
                mutate(attempt_avoid_rate = attempt_avoid/female_mounts) %>% 
                mutate(avoid_success_rate = success_avoid/attempt_avoid) %>% 
                mutate(abort_rate = aborts/possible_aborts) %>% 
                mutate(mount_rate = mounts/2)

male_sum_dat$replicate <- as.factor(male_sum_dat$replicate)

################################## VISUALIZING DATA ############################################## 
## 1) Proportion of mounts directed at other males
ggplot(data = male_sum_dat, aes(x = treatment, y = male_mount_rate, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Male mount rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5)

## 2) Proportion of mounts where females attempted to avoid that were successful
ggplot(data = male_sum_dat, aes(x = treatment, y = avoid_success_rate, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Female escape rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5)

## 3) Insemination rate (inseminations per day)
ggplot(data = male_sum_dat, aes(x = treatment, y = inseminations, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Inseminations per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + ylim(0, 6) + 
       geom_jitter(width = 0.1, alpha = 0.5)

## 5) Mount rate (mounts per day)
ggplot(data = male_sum_dat, aes(x = treatment, y = mount_rate, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Mounts per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5)

## 5) Opposite sex strength
ggplot(data = male_sum_dat, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Opposite-sex strength", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5)

################################## MODELS #########################################################
male_daily_data <- read.csv("males_summer_2023/data/male_summary_data.csv", stringsAsFactors = TRUE) %>% 
                   filter(day == 1 | day == 2)

# 1) Proportion of mounts directed at other males
male_mount_mod <- glmer(data = male_daily_data, cbind(male_mounts, female_mounts) ~ treatment*day +
                        (1|replicate/ID), 
                        family = binomial())

plot(simulateResiduals(male_mount_mod))
summary(male_mount_mod)
Anova(male_mount_mod)

## 2) Proportion of mounts where females attempted to avoid that were successful (NEED TO REVIST, TRY GLMMTMB)
avoid_model <- glmer(data = male_daily_data, cbind((attempt_avoid - success_avoid), success_avoid) ~ treatment*day + 
                              (1|replicate/ID), family = binomial())

plot(simulateResiduals(avoid_model))
summary(avoid_model)
Anova(avoid_model)


## 3) Insemination rate (number of inseminations)
insem_model <- glmmTMB(data = male_daily_data, inseminations ~ treatment*day + (1|replicate/ID), family = poisson())

plot(simulateResiduals(insem_model))
summary(insem_model)
Anova(insem_model)

## 3) Mount rate (number of mounts)
mount_model <- glmmTMB(data = male_daily_data, mounts ~ treatment*day + (1|replicate/ID), family = poisson())

plot(simulateResiduals(mount_model))
Anova(mount_model)

tapply(male_sum_dat$mounts/2, male_sum_dat$treatment, mean)
tapply(male_sum_dat$mounts/2, male_sum_dat$treatment, sd)

15.287093/sqrt(24)
7.638545/sqrt(24)

### OPPOSITE-SEX STRENGTH CORRELATION WITH INSEMINATION SUCCESS
ggplot(data = male_sum_dat, aes(y = insem_rate, x = oppo_sex_strength)) + geom_point() + 
      geom_smooth(method = "lm")
