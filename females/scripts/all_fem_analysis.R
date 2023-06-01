library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(janitor)
library(car)
library(igraph)
library(ggsci)
library(netdiffuseR)

#  Script that allows igraph plots to change arrow size
source("arrow_hack/igraphplot2.R")
environment(plot.igraph2) <- asNamespace('igraph')
environment(igraph.Arrows2) <- asNamespace('igraph')

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 18))

fem_sum_dat <- read.csv("females/data/fem_summary_data.csv")

fem_sum_dat$avoid_success <- as.numeric(fem_sum_dat$avoid_success)
fem_sum_dat$replicate <- as.factor(fem_sum_dat$replicate)
fem_sum_dat$treatment <- as.factor(fem_sum_dat$treatment)

ggplot(data = fem_sum_dat, aes(x = treatment, y = prop_avoid, fill = treatment)) + geom_boxplot(alpha = 0.9) + ylim(0, 1) + My_Theme + 
      ylab("Attempted avoidance rate") + scale_fill_manual(values=c("#f8ad9d", "#9e2a2b"))

ggplot(data = fem_sum_dat, aes(x = treatment, y = avoid_success, fill = treatment)) + geom_boxplot(alpha = 0.9) + ylim(0, 1) + My_Theme + 
     ylab("Avoidance success rate") + scale_fill_manual(values=c("#f8ad9d", "#9e2a2b"))

ggplot(data = fem_sum_dat, aes(x = treatment, y = inseminations/2, fill = treatment)) + geom_boxplot(alpha = 0.9) + My_Theme + 
    ylab("Inseminations per day") + ylim(0, 4) + scale_fill_manual(values=c("#f8ad9d", "#9e2a2b"))


# GLMMs for insem

all_fem_data <- read.csv("females/data/all_fem_data.csv", stringsAsFactors = TRUE) %>% 
                mutate(female_ID = paste(replicate, patch_partner, sep = "_"))

# Convert capital Y to lowercase y in success column
all_fem_data$success[all_fem_data$success == "Y"] <- "y"
all_fem_data$success<- droplevels(all_fem_data$success)
all_fem_data$replicate <- as.factor(all_fem_data$replicate)

# Model for number of inseminations
insem_model <- glmer(data = all_fem_data, behaviour ~ treatment + (1|replicate) +
                                                                  (1|replicate:patch_partner), 
                                                                  #(1|replicate:patch_focal), 
                                                                  family = binomial(link = "logit")) 
summary(insem_model)

# Model for propensity to evade
evade_data <- all_fem_data %>% 
              filter(avoid != "NA")

evade_data$avoid[evade_data$avoid == "Y"] <- "y"
evade_data$avoid[evade_data$avoid == "N"] <- "n"
evade_data$avoid <- droplevels(evade_data$avoid)


evade_model <- glmer(data = evade_data, avoid ~ treatment + (1|replicate) + (1|replicate:patch_partner) +
                            (1|replicate:patch_focal), 
                          family = binomial(link = "logit")) 
summary(evade_model)


# Model for avoidance success rate
success_data <- all_fem_data %>% 
                filter(success != "NA")

success_data$success <- droplevels(success_data$success)

success_model <- glmer(data = success_data, success ~ treatment + (1|replicate) + (1|replicate:patch_partner),
                     family = binomial(link = "logit")) 

summary(success_model)

