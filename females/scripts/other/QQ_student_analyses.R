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

fem_sum_dat <- read.csv("females/data/fem_summary_data.csv") %>% 
  filter(day == "both")


fem_sum_dat$avoid_success <- as.numeric(fem_sum_dat$avoid_success)
fem_sum_dat$replicate <- as.factor(fem_sum_dat$replicate)
fem_sum_dat$treatment <- as.factor(fem_sum_dat$treatment)

# VISUALIZATION
# Boxplot for proportion of mounts attempted to avoid
ggplot(data = fem_sum_dat, aes(x = treatment, y = prop_avoid)) + geom_boxplot(outlier.color = NA) +
   geom_point(aes(color = replicate), size = 4, alpha = 0.5) + scale_color_nejm() + ylim(0, 1) + My_Theme + 
   ylab("Proportion of mounts females tried to avoid")

# Boxplot for avoidance success rate
ggplot(data = fem_sum_dat, aes(x = treatment, y = avoid_success)) + geom_boxplot(outlier.color = NA) +
   geom_point(aes(color = replicate), size = 4, alpha = 0.5) + scale_color_nejm() + ylim(0, 1) + My_Theme + 
   ylab("Proportion of mounts sucessfully avoided")

# Boxplot for number of inseminations
ggplot(data = fem_sum_dat, aes(x = treatment, y = inseminations)) + geom_boxplot(outlier.color = NA) +
   geom_jitter(aes(color = replicate), size = 4, alpha = 0.4, width = 0.06, height = 0) + scale_color_nejm() + My_Theme + 
   ylab("Number of inseminations") + ylim(0, 8)

# ANALYSIS (mixed-models)
# Proportion of mounts attempted to avoid
avoid_model <- lmer(data = fem_sum_dat, asin(sqrt(prop_avoid)) ~ treatment + (1|replicate))
plot(simulateResiduals(avoid_model))
Anova(avoid_model)

# Avoidance success rate
success_model <- lmer(data = fem_sum_dat, asin(sqrt(avoid_success)) ~ treatment + (1|replicate))
plot(simulateResiduals(success_model))
Anova(success_model)

# Number of inseminations
insem_model <- glmer(data = fem_sum_dat, inseminations ~ treatment + (1|replicate), family = poisson())
plot(simulateResiduals(insem_model))
Anova(insem_model)


# ANALYSIS (t.test)
# Proportion avoided
t.test(data = fem_sum_dat, prop_avoid ~ treatment)

# Proportion successful
t.test(data = fem_sum_dat, avoid_success ~ treatment)

# Number of inseminations
t.test(data = fem_sum_dat, inseminations ~ treatment, var.equal = TRUE)


