## setwd("C:/Users/janya/Desktop/R/bb_soc_exp")

library(tidyverse)
library(asnipe)
library(igraph)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(glmmTMB)
library(assortnet)
library(janitor)
library(ggsci)
library(car)

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 18))


####### LOADING OPPOSITE-SEX ASSOCIATION MATRICES IN
assoc_mat_1 <- as.matrix(read.csv("females/data/assoc_mat_r1_fem.csv", row.names = 1))
assoc_mat_2 <- as.matrix(read.csv("females/data/assoc_mat_r2_fem.csv", row.names = 1))
assoc_mat_3 <- as.matrix(read.csv("females/data/assoc_mat_r3_fem.csv", row.names = 1))
assoc_mat_4 <- as.matrix(read.csv("females/data/assoc_mat_r4_fem.csv", row.names = 1))
assoc_mat_5 <- as.matrix(read.csv("females/data/assoc_mat_r5_fem.csv", row.names = 1))
assoc_mat_6 <- as.matrix(read.csv("females/data/assoc_mat_r6_fem.csv", row.names = 1))

assoc_matrices <- list(assoc_mat_1, assoc_mat_2, assoc_mat_3, assoc_mat_4, assoc_mat_5, assoc_mat_6) # Combine matrices into one list


## FUNCTION TO TURN MATRICES INTO NETWORK FIGURES
func_oppo_network <- function(assoc_mat){
  
# Turn matrix into igraph object 
  igraph <- graph_from_adjacency_matrix(assoc_mat, diag = FALSE, weighted = TRUE, mode = "undirected")
  
# Assign igraph attributes
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male", "female"))
  
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male",
                                           ifelse(V(igraph)$name %in% LETTERS[9:12], "social", "isolated")))
  
  igraph <- set_vertex_attr(igraph, "strength", value = strength(igraph))
  
# Customize plots
  V(igraph)$color <- ifelse(V(igraph)$treatment == "social", "#e6722a", 
                            ifelse(V(igraph)$treatment == "isolated", "#facc8f", "grey25"))
  
  V(igraph)$names<- FALSE
  V(igraph)$size <- V(igraph)$strength*12
  E(igraph)$width <- E(igraph)$weight*10
  E(igraph)$color <- "dimgray"
  
  return(igraph)
}

igraph_list <- lapply(assoc_matrices, func_oppo_network)
plot(igraph_list[[1]], vertex.label = NA)

tkplot(igraph_list[[4]], vertex.label = NA)

# Export strength values
strength(igraph_list[[4]])

######### STRENGTH ANALYSES ############
fem_all_data <- read.csv("females/data/fem_summary_data.csv") %>% 
  filter(day == "both")

fem_all_data$replicate <- as.factor(fem_all_data$replicate)

ggplot(data = fem_all_data, aes(x = treatment, y = oppo_sex_strength, fill = treatment)) + geom_boxplot(alpha = 0.9) +
  scale_fill_manual(values=c("#f8ad9d", "#9e2a2b")) + ylab("Aggregation network strength") + My_Theme

strength_model <- lmer(data = fem_all_data, oppo_sex_strength ~ treatment + (1|replicate))

plot(simulateResiduals(strength_model))
Anova(strength_model)

############################################################################################
ggplot(data = fem_all_data, aes(x = oppo_sex_strength, y = inseminations)) + geom_point() + geom_smooth(method = "lm") 


oppo_sex_dat <- read.csv("oppo_sex_strength.csv") %>% 
                filter(sex == "male") 

ggplot(data = oppo_sex_dat, aes(x = oppo_sex_degree, y = inseminations)) + geom_smooth(method = "lm") + geom_point()

oppo_sex_dat$replicate <- as.factor(oppo_sex_dat$replicate)
oppo_sex_dat$experiment <- as.factor(oppo_sex_dat$experiment)

mod <- glmer(data = oppo_sex_dat, inseminations ~ oppo_sex_degree + (1|experiment:replicate), family = poisson())

plot(simulateResiduals(mod))

Anova(mod)


