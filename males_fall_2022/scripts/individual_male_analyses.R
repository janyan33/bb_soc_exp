library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(glmmTMB)
library(emmeans)

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20))

## DATA USED FOR PLOTTING ONLY
male_data <- read.csv("males_fall_2022/data/combined_individual_data.csv", stringsAsFactors = TRUE) %>% 
             filter(day == "both") %>% 
             mutate(insem_rate = inseminations/2) %>% 
             mutate(mount_rate = mounts/2)

male_data$replicate <- as.factor(male_data$replicate)

################################## VISUALIZING DATA ############################################## 
## Plot data
## 1) Proportion of mounts directed at other males
ggplot(data = male_data, aes(x = treatment, y = prop_male, fill = treatment)) + 
      geom_boxplot(outlier.colour = NA) + labs(y = "Male mount rate", x = NULL) + ylim(0, 1) +
      My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4")) + 
      geom_jitter(width = 0.1, alpha = 0.3, size = 2)

## 2) Proportion of mounts where females attempted to avoid that were successful
ggplot(data = male_data, aes(x = treatment, y = avoid_success_rate, fill = treatment)) + 
       geom_boxplot(outlier.colour = NA) + labs(y = "Female escape rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values = c("lightblue1", "deepskyblue4")) +
       geom_jitter(width = 0.1, alpha = 0.3, size = 2, height = 0)

## 3) Insemination rate (inseminations per male)
ggplot(data = male_data, aes(x = treatment, y = inseminations, fill = treatment)) + 
       geom_boxplot(fatten = 3, outlier.colour = NA) + labs(y = "Inseminations per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4")) + 
       geom_jitter(width = 0.1, alpha = 0.3, size = 2)

# 4) Mount rate
ggplot(data = male_data, aes(x = treatment, y = mount_rate, fill = treatment)) + 
       geom_boxplot(outlier.colour = NA) + labs(y = "Mounts per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4")) + 
       geom_jitter(width = 0.1, alpha = 0.3, size = 2)

## 5) Opposite sex networks
ggplot(data = male_data, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
       geom_boxplot(outlier.colour = NA) + labs(y = "Opposite-sex strength", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4")) +
       geom_jitter(width = 0.1, alpha = 0.3, size = 2)

################################## MODELS #####################################
## Load in data for analyses
male_summary_data <- read.csv("males_fall_2022/data/combined_individual_data.csv", stringsAsFactors = TRUE) %>% 
                     filter(day == 1 | day == 2) # include day 1 and 2 data and exclude the rows that sum both

## 1) Proportion of mounts directed at other males
male_mount_mod <- glmmTMB(data = male_summary_data, cbind(male_mounts, female_mounts) ~ 
                                                  treatment*day + (1|replicate/ID), 
                                                  family = binomial())

plot(simulateResiduals(male_mount_mod)) # Looks good
summary(male_mount_mod)
Anova(male_mount_mod)

em_male_mount <- emmeans(male_mount_mod, specs = ~ treatment*day)
pairs(em_male_mount, simple = "treatment") # no differences between treatments in either day

## 2) Proportion of mounts where females attempted to avoid that were successful
female_avoid_model <- glmmTMB(data = male_summary_data, cbind((attempted_avoid - mounts_evaded), mounts_evaded) 
                                                      ~ treatment*day + (1|replicate/ID), 
                                                      family = binomial())

plot(simulateResiduals(female_avoid_model)) # Looks good
summary(female_avoid_model)
Anova(female_avoid_model)

## 3) Number of inseminations per male
# Used glmmTMB instead of glmer here bc I got a singular fit with glmer
insem_model <- glmmTMB(data = male_summary_data, inseminations ~ treatment*day + 
                                                 (1|replicate/ID), family = poisson())

plot(simulateResiduals(insem_model)) # Looks good
summary(insem_model)
Anova(insem_model)

## 3) Number of mounts per day
# Used glmmTMB instead of glmer here bc I got a singular fit with glmer
mount_model <- glmmTMB(data = male_summary_data, mounts ~ treatment*day + 
                                                (1|replicate/ID), family = nbinom2())

plot(simulateResiduals(mount_model))
summary(mount_model)
Anova(mount_model)

# Pairwise comparison for mount model
em_mounts <- emmeans(mount_model, specs = ~ treatment*day)
plot(em_mounts)
pairs(em_mounts, simple = "treatment")


# Get mean +- SD for mounts
tapply(male_data$mount_rate, male_data$treatment, mean)
tapply(male_data$mount_rate, male_data$treatment, sd)

6.877442/sqrt(36)
4.389970/sqrt(34)

# Get mean +- SD for inseminations
tapply(male_data$insem_rate, male_data$treatment, mean)
tapply(male_data$insem_rate, male_data$treatment, sd)

1.0159674/sqrt(36)
0.7499257/sqrt(34)

