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
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 18))

all_male_dat <- read.csv("males_summer_2023/data/male_summary_data.csv") %>% 
                filter(day == "both") %>% 
                mutate(insem_rate = inseminations/mounts) %>% 
                mutate(male_mount_rate = male_mounts/mounts) %>% 
                mutate(attempt_avoid_rate = attempt_avoid/female_mounts) %>% 
                mutate(avoid_success_rate = success_avoid/attempt_avoid) %>% 
                mutate(abort_rate = aborts/possible_aborts)

ggplot(data = all_male_dat, aes(x = treatment, y = avoid_success_rate, fill = treatment)) + geom_boxplot(alpha = 0.9) + My_Theme

