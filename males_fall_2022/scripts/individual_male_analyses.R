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
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20))

## Load data in
male_data <- read.csv("males_fall_2022/data/combined_individual_data.csv", stringsAsFactors = TRUE) %>% 
             filter(day == "both") %>% 
             mutate(insem_rate = inseminations/2) %>% 
             mutate(mount_rate = mounts/2)

male_data$replicate <- as.factor(male_data$replicate)

################################## VISUALIZING DATA ############################################## 
## Plot data
## 1) Proportion of mounts directed at other males
ggplot(data = male_data, aes(x = treatment, y = prop_male, fill = treatment)) + 
      geom_boxplot() + labs(y = "Male mount rate", x = NULL) + ylim(0, 1) +
      My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4"))

## 2) Proportion of mounts where females attempted to avoid that were successful
ggplot(data = male_data, aes(x = treatment, y = avoid_success_rate, fill = treatment)) + 
       geom_boxplot() + labs(y = "Female escape rate", x = NULL) + ylim(0, 1) +
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4"))

## 3) Insemination rate (inseminations per)
ggplot(data = male_data, aes(x = treatment, y = insem_rate, fill = treatment)) + 
       geom_boxplot() + labs(y = "Inseminations per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4"))

## 4) Mount rate
ggplot(data = male_data, aes(x = treatment, y = mount_rate, fill = treatment)) + 
       geom_boxplot() + labs(y = "Mounts per day", x = NULL) + 
       My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4"))

## 5) Opposite sex networks
ggplot(data = male_data, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
  geom_boxplot() + labs(y = "Opposite-sex strength", x = NULL) + 
  My_Theme + scale_fill_manual(values =c("lightblue1", "deepskyblue4"))


################################## MODELS #########################################################
## 1) Proportion of mounts directed at other males
all_data_male_mounts <- read.csv("males_fall_2022/data/all_data_combined.csv") %>% 
                        filter(behaviour == "mount" | behaviour == "insemination")  # removes one data-entry where behaviour was "other"
                       
all_data_male_mounts$behaviour <- as.factor(all_data_male_mounts$behaviour)
all_data_male_mounts$partner_sex <- as.factor(all_data_male_mounts$partner_sex)

# Model
male_mount_mod <- glmer(data = all_data_male_mounts, partner_sex ~ treatment + (1|replicate) +
                   (1|replicate:patch_focal), family = binomial())

plot(simulateResiduals(male_mount_mod))
summary(male_mount_mod)
Anova(male_mount_mod)


## 2) Proportion of mounts where females attempted to avoid that were successful
all_data_avoid <- read.csv("males_fall_2022/data/all_data_combined.csv") %>% 
                  filter(behaviour == "mount" | behaviour == "insemination") %>%  # removes one data-entry where behaviour was "other"
                  filter(partner_avoid...Y.N. == "y" | partner_avoid...Y.N. == "Y") # filters for only cases where females attempted to avoid

all_data_avoid$avoid_success[all_data_avoid$avoid_success == "Y"] <- "y" # Turns uppercase to lowercase
all_data_avoid$avoid_success[all_data_avoid$avoid_success == "N"] <- "n" # Turns uppercase to lowercase

all_data_avoid$avoid_success <- as.factor(all_data_avoid$avoid_success)
 
# Model
avoid_model <- glmer(data = all_data_avoid, avoid_success ~ treatment + (1|replicate) + 
                                         (1|replicate:patch_focal), family = binomial())
plot(simulateResiduals(avoid_model))
summary(avoid_model)
Anova(avoid_model)

## 3) Insemination rate (inseminations per day)
insem_model <- lmer(data = male_data, insem_rate ~ treatment + (1|replicate))

plot(simulateResiduals(insem_model))
Anova(insem_model)
summary(insem_model)

## 3) Mount rate (Mounts per day)
mount_model <- lmer(data = male_data, mount_rate ~ treatment + (1|replicate))

plot(simulateResiduals(mount_model))
summary(mount_model)
Anova(mount_model)




