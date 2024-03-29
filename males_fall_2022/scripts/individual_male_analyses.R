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

## DATA USED FOR PLOTTING ONLY (used for plotting because it combines day 1 and day 2 data)
male_data <- read.csv("males_fall_2022/data/combined_individual_data.csv", stringsAsFactors = TRUE) %>% 
             filter(day == "both") %>% 
             mutate(insem_rate = inseminations/2) %>% 
             mutate(mount_rate = mounts/2)

male_data$replicate <- as.factor(male_data$replicate)

## DATA USED IN ANALYSES
male_summary_data <- read.csv("males_fall_2022/data/combined_individual_data.csv", stringsAsFactors = TRUE) %>% 
                     filter(day == 1 | day == 2) # include day 1 and 2 data and exclude the rows that sum both

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
ggplot(data = male_summary_data, aes(x = day, y = inseminations, fill = treatment)) + 
       geom_boxplot(fatten = 3, outlier.colour = NA) + labs(y = "Inseminations per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4")) + 
       geom_point(position = position_jitterdodge(jitter.height = 0.1, jitter.width = 0.4), alpha = 0.3)

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
####### 1) Proportion of mounts directed at other males
male_mount_mod <- glmmTMB(data = male_summary_data, cbind(male_mounts, female_mounts) ~ 
                                                  treatment*day + (1|replicate/ID), 
                                                  family = binomial())

plot(simulateResiduals(male_mount_mod)) # Looks good
Anova(male_mount_mod, type = 3) # Interaction significant so I put the Type III anova results in the Supp. (qualitative results the same)

# Pairwise test looking at interaction
em_male_mount <- emmeans(male_mount_mod, specs = ~ treatment*day)
pairs(em_male_mount, simple = "treatment") # no differences between treatments in either day

###############################################################################
####### 2) Proportion of mounts where females attempted to avoid that were successful
female_avoid_model <- glmmTMB(data = male_summary_data, cbind((attempted_avoid - mounts_evaded), mounts_evaded) 
                                                      ~ treatment*day + (1|replicate/ID), 
                                                      family = binomial())

plot(simulateResiduals(female_avoid_model)) # Looks good
Anova(female_avoid_model) # Type II


###############################################################################
####### 3) Number of inseminations per male
insem_model <- glmmTMB(data = male_summary_data, inseminations ~ treatment*day + 
                                                 (1|replicate/ID), family = poisson())

plot(simulateResiduals(insem_model)) # Looks good
summary(insem_model)
Anova(insem_model, type = 3) # Interaction significant so I put the Type III anova results in the Supp. (qualitative results the same)

# Pairwise test looking at interaction
em_insem <- emmeans(insem_model, specs = ~treatment*day)
pairs(em_insem, simple = "treatment")

## Mean +- SD for inseminations
male_data_1 <- male_summary_data %>% 
               filter(day == 1)

tapply(male_data_1$inseminations, male_data_1$treatment, mean)
tapply(male_data_1$inseminations, male_data_1$treatment, sd)

1.482972/sqrt(36)
1.136417/sqrt(34)

male_data_2 <- male_summary_data %>% 
               filter(day == 2)

tapply(male_data_2$inseminations, male_data_2$treatment, mean)
tapply(male_data_2$inseminations, male_data_2$treatment, sd)

1.1166134/sqrt(36)
0.7836338/sqrt(34)

###############################################################################
####### 4) Number of mounts per day
# Used glmmTMB instead of glmer here bc I got a singular fit with glmer
mount_model <- glmmTMB(data = male_summary_data, mounts ~ treatment*day + 
                                                (1|replicate/ID), family = nbinom2())

plot(simulateResiduals(mount_model)) # Looks good
summary(mount_model)
Anova(mount_model, type = 3)

# Pairwise comparison for mount model
em_mounts <- emmeans(mount_model, specs = ~ treatment*day)
pairs(em_mounts, simple = "treatment")

# Get mean +- SD for mounts
tapply(male_data_1$mounts, male_data_1$treatment, mean)
tapply(male_data_1$mounts, male_data_1$treatment, sd)

12.865945/sqrt(36)
7.222526/sqrt(34)

tapply(male_data_2$mounts, male_data_2$treatment, mean)
tapply(male_data_2$mounts, male_data_2$treatment, sd)

6.877442/sqrt(36)
4.389970/sqrt(34)

