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

## Load data in
male_data <- read.csv("males_fall_2022/data/combined_individual_data.csv", stringsAsFactors = TRUE) %>% 
             filter(day == "both")

male_data$replicate <- as.factor(male_data$replicate)


################################## VISUALIZING DATA ############################################## 

## Plot data
## 3) Proportion of mounts where females attempted to avoid that were successful
ggplot(data = male_data, aes(x = treatment, y = avoid_success_rate)) + 
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + ylab("Female evasion success rate") + ylim(0, 1)

## Proportion of mounts that were females attempted to evade
# Probably the worse measure
ggplot(data = male_data, aes(x = treatment, y = prop_attempt_avoid)) +
  geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
  scale_color_nejm() + 
  ylab("Proportion of mounts directed at females where females attempted to evade") + 
  ylim(0, 1)

## Proportion of mounts that were avoided by females
# Probably the worse measureS
ggplot(data = male_data, aes(x = treatment, y = prop_avoided)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + 
       ylab("Proportion of mounts directed at females that were evaded") + 
       ylim(0, 1)

## 2) Proportion of mounts where males had a choice that were aborted
ggplot(data = male_data, aes(x = treatment, y = abort_rate)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + ylab("Male abort rate") + ylim(0, 1)

## 4) Proportion of mounts directed at other males
ggplot(data = male_data, aes(x = treatment, y = prop_male)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) +
       scale_color_nejm() + ylab("Proportion of mounts directed at other males") + ylim(0, 1)

## Proportion of all mounts that led to insemination
ggplot(data = male_data, aes(x = treatment, y = prop_successful)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + ylab("Proportion of all mounts that resulted in insemination") + ylim(0, 0.5)

## Proportion of mounts directed at females that led to insemination
ggplot(data = male_data, aes(x = treatment, y = prop_success_female_only)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + ylab("Proportion of female mounts that resulted in insemination") + ylim(0, 1)

## Number of inseminations
ggplot(data = male_data, aes(x = treatment, y = inseminations)) +
  geom_boxplot()+geom_point(aes(color = replicate), size = 3, alpha = 0.5) +
  scale_color_nejm() + ylab("Number of inseminations") + ylim(0, 10)

## 1) Number of mounts
ggplot(data = male_data, aes(x = treatment, y = mounts)) +
  geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.5) + 
  scale_color_nejm() + ylab("Number of mounts")



############################################## MODELLING DATA #########################################################

####  MALE REJECTION ####
all_data_reject <- read.csv("males_fall_2022/data/all_data_combined.csv") %>% 
                   filter(male_abort == "y" | male_abort == "n" | male_abort == "Y" | male_abort == "N")

all_data_reject$male_abort[all_data_reject$male_abort == "Y"] <- "y"
all_data_reject$male_abort[all_data_reject$male_abort == "N"] <- "n"

unique(all_data_reject$male_abort)

all_data_reject$male_abort <- as.factor(all_data_reject$male_abort)
all_data_reject$replicate <- as.factor(all_data_reject$replicate)
all_data_reject$patch_partner <- as.factor(all_data_reject$patch_partner)
all_data_reject$patch_focal <- as.factor(all_data_reject$patch_focal)

## Logistic regression model ##
reject_lr <- glmer(data = all_data_reject, male_abort ~ treatment + (1|replicate) +
                          (1|replicate/patch_focal), family = binomial()) 
plot(simulateResiduals(reject_lr))

Anova(reject_lr)

## Regular linear model ##
reject_model <- lmer(data = male_data, abort_rate ~ treatment + (1|replicate))
plot(simulateResiduals(reject_model))

summary(reject_model)
Anova(reject_model)




#### FEMALE AVOIDANCE SUCCESS RATE ####
all_data_avoid <- read.csv("males_fall_2022/data/all_data_combined.csv") %>% 
                   filter(avoid_success == "y" | avoid_success == "n" | avoid_success == "Y" | avoid_success == "N")

all_data_avoid$avoid_success[all_data_avoid$avoid_success == "Y"] <- "y"
all_data_avoid$avoid_success[all_data_avoid$avoid_success == "N"] <- "n"

unique(all_data_avoid$avoid_success)

all_data_avoid$avoid_success <- as.factor(all_data_avoid$avoid_success)
 
# Logistic regression model ##
avoid_lr <- glmer(data = all_data_avoid, avoid_success ~ treatment + (1|replicate) + 
                                         (1|replicate/patch_focal) + (1|replicate/patch_partner), family = binomial())
plot(simulateResiduals(avoid_lr))

Anova(avoid_lr)

# Regular linear model ##
avoid_model <- lmer(data = male_data, avoid_success_rate ~ treatment + (1|replicate))
plot(simulateResiduals(avoid_model))

summary(avoid_model)
Anova(avoid_model)



#### PROP MALE MOUNT MODEL ####
all_data_male_mounts <- read.csv("males_fall_2022/data/all_data_combined.csv") %>% 
                        filter(behaviour == "mount" | behaviour == "insemination")

all_data_male_mounts$behaviour <- as.factor(all_data_male_mounts$behaviour)
all_data_male_mounts$partner_sex <- as.factor(all_data_male_mounts$partner_sex)


## Logistic regression model ##
male_lr <- glmer(data = all_data_male_mounts, partner_sex ~ treatment + (1|replicate) +
                                              (1|replicate/patch_focal) + (1|replicate/patch_partner), family = binomial())

plot(simulateResiduals(male_lr))
Anova(male_lr)

#### Regular linear model ####
male_model <- lmer(data = male_data, prop_male ~ treatment + (1|replicate))
plot(simulateResiduals(male_model))

summary(male_model)
Anova(male_model)



#### NUMBER OF INSEMINATIONS ####
#### Regular linear model ####
insem_model <- lmer(data = male_data, mounts ~ treatment + (1|replicate)) #family = poisson(link = "log"))

plot(simulateResiduals(insem_model))
Anova(insem_model)






