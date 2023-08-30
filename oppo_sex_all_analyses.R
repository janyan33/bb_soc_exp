library(tidyverse)
library(ggplot2); theme_set(theme_classic())

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20), 
  axis.text.y = element_text(size = 20))

oppo_sex_dat <- read.csv("oppo_sex_strength.csv") %>% 
                filter(sex == "male") %>% 
                mutate(exp_rep = paste(experiment, replicate, sep = "_"))

# Inseminations as a function of opposite-sex strength fig
ggplot(data = oppo_sex_dat, aes(x = opposite_sex_strength, y = inseminations)) +
       geom_point() + geom_smooth(method = "lm", size = 1.5, alpha = 0.5) + My_Theme

# Inseminations as a function of opposite-sex degree fig
ggplot(data = oppo_sex_dat, aes(x = oppo_sex_degree, y = inseminations)) +
       geom_point() + geom_smooth(method = "lm", size = 1.5, alpha = 0.5) + My_Theme
