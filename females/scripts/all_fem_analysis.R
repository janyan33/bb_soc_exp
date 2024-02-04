library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(janitor)
library(car)
library(igraph)
library(ggsci)
library(netdiffuseR)
library(ggpubr)

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20))

## DATA FOR PLOTTING ONLY
fem_sum_dat <- read.csv("females/data/fem_summary_data.csv") %>% 
               filter(day == "both") %>% 
               mutate(insem_rate = inseminations/2)

fem_sum_dat$avoid_success <- as.numeric(fem_sum_dat$avoid_success)
fem_sum_dat$replicate <- as.factor(fem_sum_dat$replicate)
fem_sum_dat$treatment <- as.factor(fem_sum_dat$treatment)

######################################### FIGURES ###############################################
### 1) Attempted avoidance rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = prop_avoid, fill = treatment)) + 
      geom_boxplot(alpha = 0.9, outlier.color = NA) + ylim(0, 1) + My_Theme + 
      labs(y = "Attempted avoidance rate", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) +
      geom_jitter(size = 2, alpha = 0.3, width = 0.2)

### 2) Avoidance success rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = avoid_success, fill = treatment)) + 
      geom_boxplot(alpha = 0.9, outlier.color = NA) + ylim(0, 1) + My_Theme + 
      labs(y = "Avoidance success rate", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) +
      geom_jitter(size = 2, alpha = 0.3, width = 0.2, height = 0)

#### 3) Insemination rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = insem_rate, fill = treatment)) + 
       geom_boxplot(alpha = 0.9, outlier.colour = NA) + My_Theme + 
       labs(y = "Inseminations per day", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) + 
       geom_jitter(size = 2, alpha = 0.3, width = 0.2)

#### 4) Opposite-sex association matrices
ggplot(data = fem_sum_dat, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
       geom_boxplot(alpha = 0.9, outlier.colour = NA) + My_Theme + 
       labs(y = "Opposite-sex strength", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) + 
       geom_jitter(size = 2, alpha = 0.3, width = 0.2)

######################################### ANALYSES ##############################################
fem_model_data <- read.csv("females/data/fem_summary_data.csv", stringsAsFactors = TRUE) %>% 
                  filter(day == 1 | day == 2)

##### 3) Insemination rate (number of inseminations per day) #####
insem_model <- glmmTMB(data = fem_model_data, inseminations ~ treatment*day + (1|replicate/ID), family = poisson())

plot(simulateResiduals(insem_model)) # Looks good
summary(insem_model)
Anova(insem_model, type = "II") # We use this result bc we want type II SS

##### 1) Attempted avoidance rate #####
attempt_model <- glmer(data = fem_model_data, cbind(attempt_avoid, (mounts - attempt_avoid)) ~
                                              treatment*day + (1|replicate/ID), family = binomial())
                         
plot(simulateResiduals(attempt_model)) # Looks good
summary(attempt_model)
Anova(attempt_model)

##### 2) Avoidance success rate #####
success_model <- glmer(data = fem_model_data, cbind(success_avoid, (attempt_avoid - success_avoid)) ~
                         treatment*day + (1|replicate/ID), family = binomial())

plot(simulateResiduals(success_model))

summary(success_model)
Anova(success_model)


