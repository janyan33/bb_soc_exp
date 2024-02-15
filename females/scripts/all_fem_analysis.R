library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(glmmTMB)

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

## DATA USED FOR ANALYSES
fem_model_data <- read.csv("females/data/fem_summary_data.csv", stringsAsFactors = TRUE) %>% 
                  filter(day == 1 | day == 2)

fem_model_data <- fem_model_data %>% 
                  mutate(male_aborts = (mounts - inseminations - success_avoid)) %>% 
                  mutate(possible_aborts = (mounts - success_avoid)) # For abort rate model

fem_model_data <- fem_model_data %>% 
                  mutate(male_rate = (male_aborts/possible_aborts)) # For abort rate figure

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
ggplot(data = fem_model_data, aes(x = day, y = inseminations, fill = treatment)) + 
       geom_boxplot(alpha = 0.9, outlier.colour = NA) + My_Theme + 
       labs(y = "Inseminations per day", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) + 
       geom_point(position = position_jitterdodge(jitter.height = 0.1, jitter.width = 0.4), alpha = 0.3)

#### 4) Opposite-sex association matrices
ggplot(data = fem_sum_dat, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + 
       geom_boxplot(alpha = 0.9, outlier.colour = NA) + My_Theme + 
       labs(y = "Opposite-sex strength", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) + 
       geom_jitter(size = 2, alpha = 0.3, width = 0.2)

#### 5) Rate at which males aborted mounts with social vs. isolated females
ggplot(data = fem_model_data, aes(x = day, y = male_rate, fill = treatment)) + 
       geom_boxplot(alpha = 0.9, outlier.colour = NA) + My_Theme + ylim(0, 1) +
       labs(y = "Male rejection rate", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414")) + 
       geom_point(position = position_jitterdodge(jitter.height = 0, jitter.width = 0.4), alpha = 0.3)

######################################### ANALYSES ##############################################
##### 3) Insemination rate (number of inseminations per day) #####
insem_model <- glmmTMB(data = fem_model_data, inseminations ~ treatment*day + (1|replicate/ID), family = poisson())

plot(simulateResiduals(insem_model)) # Looks good
Anova(insem_model, type = 3) # Type 3 bc sig interaction

insem_em <- emmeans(insem_model, specs = ~ treatment*day)

pairs(insem_em, simple = "treatment")

## Get mean +/- SE for # of inseminations per female
fem_model_data_1 <- fem_model_data %>% 
                    filter(day == 1)

tapply(fem_model_data_1$inseminations, fem_model_data_1$treatment, mean)
tapply(fem_model_data_1$inseminations, fem_model_data_1$treatment, sd)

1.318074/sqrt(24)
1.398109/sqrt(24)

fem_model_data_2 <- fem_model_data %>% 
                    filter(day == 2)

tapply(fem_model_data_2$inseminations, fem_model_data_2$treatment, mean)
tapply(fem_model_data_2$inseminations, fem_model_data_2$treatment, sd)

0.9630868/sqrt(24)
1.5580553/sqrt(24)


##### 1) Attempted avoidance rate #####
attempt_model <- glmmTMB(data = fem_model_data, cbind(attempt_avoid, (mounts - attempt_avoid)) ~
                                              treatment*day + (1|replicate/ID), family = binomial())
                         
plot(simulateResiduals(attempt_model)) # Looks good
summary(attempt_model)
Anova(attempt_model) # Type II bc no sig interaction

##### 2) Avoidance success rate #####
success_model <- glmmTMB(data = fem_model_data, cbind(success_avoid, (attempt_avoid - success_avoid)) ~
                         treatment*day + (1|replicate/ID), family = binomial())

plot(simulateResiduals(success_model))

summary(success_model)
Anova(success_model)

#### 4) Rate of getting mounted
mount_model <- glmmTMB(data = fem_model_data, mounts ~ treatment*day + (1|replicate) + (1|replicate/ID), 
                       family = nbinom1())

plot(simulateResiduals(mount_model))
summary(mount_model)
Anova(mount_model) # Type II bc no sig interaction


tapply(fem_model_data$mounts, fem_model_data$treatment, mean)
tapply(fem_model_data$mounts, fem_model_data$treatment, sd)

8.089235/sqrt(24)
6.708699/sqrt(24)

##### 5) Male abort rate
abort_model <- glmmTMB(data = fem_model_data, cbind(male_aborts, (possible_aborts - male_aborts)) ~
                           treatment*day + (1|replicate/ID), family = binomial())

summary(abort_model)
Anova(abort_model) # Type 3 bc

em_abort <- emmeans(abort_model, specs = ~treatment*day)
pairs(em_abort, simple = "treatment")
