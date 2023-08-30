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
       geom_boxplot() + labs(y = "Male mount rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008"))

## 2) Proportion of mounts where females attempted to avoid that were successful
ggplot(data = male_sum_dat, aes(x = treatment, y = avoid_success_rate, fill = treatment)) + 
       geom_boxplot() + labs(y = "Female escape rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008"))

## 3) Insemination rate (inseminations per day)
ggplot(data = male_sum_dat, aes(x = treatment, y = insem_rate, fill = treatment)) + 
       geom_boxplot() + labs(y = "Inseminations per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008")) + ylim(0, 6)

## 5) Mount rate (mounts per day)
ggplot(data = male_sum_dat, aes(x = treatment, y = mount_rate, fill = treatment)) + 
  geom_boxplot() + labs(y = "Mounts per day", x = NULL) + 
  My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008"))

## 5) Opposite sex strength
ggplot(data = male_sum_dat, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
  geom_boxplot() + labs(y = "Opposite-sex strength", x = NULL) + 
  My_Theme + scale_fill_manual(values =c("#B7E5A7", "#268008"))

################################## MODELS #########################################################
all_male_dat <- read.csv("males_summer_2023/data/male_all_data.csv")

all_male_dat$behaviour <- as.factor(all_male_dat$behaviour)
all_male_dat$partner_sex <- as.factor(all_male_dat$partner_sex)

# 1) Proportion of mounts directed at other males
male_mount_mod <- glmer(data = all_male_dat, partner_sex ~ treatment + (1|replicate:patch_focal) + (1|replicate)
                          , family = binomial())

plot(simulateResiduals(male_mount_mod))
Anova(male_mount_mod)
summary(male_mount_mod)

## 2) Proportion of mounts where females attempted to avoid that were successful (NEED TO REVIST, TRY GLMMTMB)
all_data_avoid <- all_male_dat %>% 
                  filter(partner_avoid == "y" | partner_avoid == "Y") # filters for only cases where females attempted to avoid

all_data_avoid$avoid_success[all_data_avoid$avoid_success == "Y"] <- "y" # Turns uppercase to lowercase
all_data_avoid$avoid_success[all_data_avoid$avoid_success == "N"] <- "n" # Turns uppercase to lowercase

all_data_avoid$avoid_success <- as.factor(all_data_avoid$avoid_success)

avoid_model <- glmer(data = all_data_avoid, avoid_success ~ treatment + (1|replicate) + 
                    (1|replicate:patch_focal), family = binomial())

plot(simulateResiduals(avoid_model))
summary(avoid_model)

## 3) Insemination rate (inseminations per day)
insem_model <- lmer(data = male_sum_dat, insem_rate ~ treatment + (1|replicate))

plot(simulateResiduals(insem_model))
Anova(insem_model)

## 3) Mount rate (mounts per day)
mount_model <- lmer(data = male_sum_dat, log(mount_rate) ~ treatment + (1|replicate))

plot(simulateResiduals(mount_model))
Anova(mount_model)


### OPPOSITE-SEX STRENGTH CORRELATION WITH INSEMINATION SUCCESS
ggplot(data = male_sum_dat, aes(y = insem_rate, x = oppo_sex_strength)) + geom_point() + 
      geom_smooth(method = "lm")






