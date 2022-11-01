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
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))

## Load data in
male_data <- read.csv("data/combined_individual_data.csv") %>% 
             filter(day == "both")

male_data$replicate <- as.factor(male_data$replicate)

## Plot data
## Proportion of mounts where females attempted to avoid that were successful
# Probably the better measure
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
# Probably the worse measure
ggplot(data = male_data, aes(x = treatment, y = prop_avoided)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + 
       ylab("Proportion of mounts directed at females that were evaded") + 
       ylim(0, 1)

## Proportion of mounts where males had a choice that were aborted
ggplot(data = male_data, aes(x = treatment, y = abort_rate)) +
       geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
       scale_color_nejm() + ylab("Male abort rate") + ylim(0, 1)

## Proportion of mounts directed at other males
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
       scale_color_nejm() + ylab("Proportion of female moutns that resulted in insemination") + ylim(0, 1)

ggplot(data = male_data, aes(x = treatment, y = inseminations)) +
  geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
  scale_color_nejm() + ylab("Number of inseminations")

ggplot(data = male_data, aes(x = treatment, y = mounts)) +
  geom_boxplot() + geom_point(aes(color = replicate), size = 3, alpha = 0.7) + 
  scale_color_nejm() + ylab("Number of mounts")


