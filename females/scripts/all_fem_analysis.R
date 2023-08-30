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

## Loading data in 
fem_sum_dat <- read.csv("females/data/fem_summary_data.csv") %>% 
               filter(day == "both") %>% 
               mutate(insem_rate = inseminations/2)

fem_sum_dat$avoid_success <- as.numeric(fem_sum_dat$avoid_success)
fem_sum_dat$replicate <- as.factor(fem_sum_dat$replicate)
fem_sum_dat$treatment <- as.factor(fem_sum_dat$treatment)

######################################### FIGURES ###############################################
### 1) Attempted avoidance rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = prop_avoid, fill = treatment)) + geom_boxplot(alpha = 0.9) + ylim(0, 1) + My_Theme + 
      labs(y = "Attempted avoidance rate", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414"))

### 2) Avoidance success rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = avoid_success, fill = treatment)) + geom_boxplot(alpha = 0.9) + ylim(0, 1) + My_Theme + 
      labs(y = "Avoidance success rate", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414"))

#### 3) Insemination rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = insem_rate, fill = treatment)) + geom_boxplot(alpha = 0.9) + My_Theme + 
       labs(y = "Inseminations per day", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414"))

#### 4) Opposite-sex association matrices
ggplot(data = fem_sum_dat, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + geom_boxplot(alpha = 0.9) + My_Theme + 
       labs(y = "Opposite-sex strength", x = NULL) + scale_fill_manual(values=c("#f9c784", "#e36414"))


######################################### ANALYSES ##############################################
##### 3) Insemination rate (number of inseminations per day) #####
insem_model <- lmer(data = fem_sum_dat, insem_rate ~ treatment + (1|replicate))

plot(simulateResiduals(insem_model))
summary(insem_model)
Anova(insem_model)

##### 1) Attempted avoidance rate #####
all_fem_data <- read.csv("females/data/all_fem_data.csv", stringsAsFactors = TRUE) %>% 
                mutate(female_ID = paste(replicate, patch_partner, sep = "_")) %>% 
                filter(avoid != "NA") # removes 1/779 entries where we didn't catch the female's reponse bc it was during break

all_fem_data$avoid[all_fem_data$avoid == "Y"] <- "y" # turns upper to lowercase
all_fem_data$avoid[all_fem_data$avoid == "N"] <- "n" # turns upper to lowercase
all_fem_data$avoid <- droplevels(all_fem_data$avoid) # drops leftover extra factor level
all_fem_data$replicate <- as.factor(all_fem_data$replicate)

# Model
attempt_model <- glmer(data = all_fem_data, avoid ~ treatment + (1|replicate) + 
                              (1|replicate:patch_partner),
                              family = binomial(link = "logit")) 

plot(simulateResiduals(attempt_model))
summary(attempt_model)
Anova(attempt_model)

##### 2) Avoidance success rate #####
success_data <- all_fem_data %>% 
                filter(success != "NA") # filter for only data where females attempted to avoid

success_data$success[success_data$success == "Y"] <- "y" # turns upper to lowercase
success_data$success[success_data$success == "N"] <- "n" # turns upper to lowercase

success_data$success<- droplevels(success_data$success) # drops leftover extra factor level

# Model
success_model <- glmer(data = success_data, success ~ treatment + (1|replicate) + (1|replicate:patch_partner),
                      #(1|replicate:patch_focal),
                     family = binomial(link = "logit")) 

plot(simulateResiduals(success_model))

summary(success_model)
Anova(success_model)


