library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(janitor)
library(car)
library(emmeans)

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20))

## DATA FOR PLOTTING ONLY (because it combines data across day 1 and 2)
male_sum_dat <- read.csv("males_summer_2023/data/male_summary_data.csv") %>% 
                filter(day == "both") %>% 
                mutate(insem_rate = inseminations/2) %>% 
                mutate(male_mount_rate = male_mounts/mounts) %>% 
                mutate(attempt_avoid_rate = attempt_avoid/female_mounts) %>% 
                mutate(avoid_success_rate = success_avoid/attempt_avoid) %>% 
                mutate(abort_rate = aborts/possible_aborts) %>% 
                mutate(mount_rate = mounts/2)

male_sum_dat$replicate <- as.factor(male_sum_dat$replicate)

## DATA FOR ANALYSES
male_daily_data <- read.csv("males_summer_2023/data/male_summary_data.csv", stringsAsFactors = TRUE) %>% 
                filter(day == 1 | day == 2)

################################## VISUALIZING DATA ############################################## 
## 1) Proportion of mounts directed at other males
ggplot(data = male_sum_dat, aes(x = treatment, y = male_mount_rate, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Male mount rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5, size = 2, height = 0)

## 2) Proportion of mounts where females attempted to avoid that were successful
ggplot(data = male_sum_dat, aes(x = treatment, y = avoid_success_rate, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Female escape rate", x = NULL) + 
       scale_y_continuous(limits = c(0,1)) +
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 2)

## 3) Insemination rate (inseminations per day)
ggplot(data = male_daily_data, aes(x = day, y = inseminations, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Inseminations per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + ylim(-.05, 8.5) + 
       geom_point(position = position_jitterdodge(jitter.height = 0.05, jitter.width = 0.3), alpha = 0.5)

## 5) Mount rate (mounts per day)
ggplot(data = male_sum_dat, aes(x = treatment, y = mount_rate, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Mounts per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5)

## 5) Opposite sex strength
ggplot(data = male_sum_dat, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
       geom_boxplot(outlier.color = NA) + labs(y = "Opposite-sex strength", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + 
       geom_jitter(width = 0.1, alpha = 0.5, size = 2)

################################## MODELS #########################################################
###### 1) Proportion of mounts directed at other males
male_mount_mod <- glmmTMB(data = male_daily_data, cbind(male_mounts, female_mounts) ~ treatment*day +
                        (1|replicate/ID), 
                        family = binomial()) 

plot(simulateResiduals(male_mount_mod)) # Looks good
Anova(male_mount_mod) # Type II bc no sig interaction

##################################################################################################
####### 2) Proportion of mounts where females attempted to avoid that were successful
avoid_model <- glmmTMB(data = male_daily_data, cbind((attempt_avoid - success_avoid), success_avoid) ~ treatment*day + 
                              (1|replicate/ID), family = binomial())

plot(simulateResiduals(avoid_model)) # Looks good
Anova(avoid_model) # Type II bc no sig interaction

#################################################################################################
## 3) Insemination rate (number of inseminations)
insem_model <- glmmTMB(data = male_daily_data, inseminations ~ treatment*day + (1|replicate/ID), family = poisson())

plot(simulateResiduals(insem_model)) # Looks good
Anova(insem_model) # Type II bc no sig interaction

## Mean +/- SE for inseminations
tapply(male_sum_dat$inseminations/2, male_sum_dat$treatment, mean)
tapply(male_sum_dat$inseminations/2, male_sum_dat$treatment, sd)

1.4024758/sqrt(24)
0.8657639/sqrt(24)

#################################################################################################
## 4) Mount rate (number of mounts)
mount_model <- glmmTMB(data = male_daily_data, mounts ~ treatment*day + (1|replicate/ID), family = poisson())

plot(simulateResiduals(mount_model)) # Looks good
summary(mount_model) # Type III bc significant interaction
Anova(mount_model, type = 3)

# Pairwise comparison for mount model
em_mount <- emmeans(mount_model, specs = ~ treatment*day)
pairs(em_mount, simple = "treatment")

## Mean +/- SE for mount rate per male
male_daily_data_1 <- male_daily_data %>%
                  filter(day == 1)

tapply(male_daily_data_1$mounts, male_daily_data_1$treatment, mean)
tapply(male_daily_data_1$mounts, male_daily_data_1$treatment, sd)

15.96645/sqrt(24)
11.72511 /sqrt(24)


male_daily_data_2 <- male_daily_data %>%
  filter(day == 2)

tapply(male_daily_data_2$mounts, male_daily_data_2$treatment, mean)
tapply(male_daily_data_2$mounts, male_daily_data_2$treatment, sd)

21.446217/sqrt(24)
7.722426/sqrt(24)


