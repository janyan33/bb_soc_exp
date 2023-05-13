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

##################### INPUTTING AND ORGANIZING DATA #######################
## Data for aggregation-based networks
groups_agg <- read.csv("females/data/aggregations.csv") %>%  
  remove_empty("cols")

groups_agg_reps <- split(groups_agg, groups_agg$replicate)

#################### VISUALIZING SOCIAL NETWORKS ############################
## Function for turning group data into igraph objects
func_igraph <- function(rep_groups){
  group_list <- strsplit(rep_groups$members, " ")
  gbi_matrix <- get_group_by_individual(group_list, data_format = "groups")
  ibi_matrix <- get_network(gbi_matrix, data_format = "GBI")
  ibi_matrix <- ibi_matrix[order(rownames(ibi_matrix)) , order(colnames(ibi_matrix))] # alphabetical order
  igraph <- graph_from_adjacency_matrix(ibi_matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male", "Female"))

  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male",
                                           ifelse(V(igraph)$name %in% LETTERS[9:12], "social", "isolated")))
  
  V(igraph)$color <- ifelse(V(igraph)$treatment == "male", "#118ab2", 
                            ifelse(V(igraph)$treatment == "social", "#f77f00", "#f6bd60"))
  strength <- strength(igraph)
  igraph <- set_vertex_attr(igraph, "strength", value = strength)
  V(igraph)$size <- V(igraph)$strength*12
  V(igraph)$label.color <- "black"
  E(igraph)$width <- E(igraph)$weight*6
  
  return(c(igraph, strength))
}


## Visualizing aggregation-based networks
igraph_objects_agg <- func_igraph(groups_agg_reps[[6]])
func_igraph(groups_agg_reps[[6]])

tkplot(igraph_objects_agg)


######### STRENGTH ANALYSES ############
fem_all_data <- read.csv("females/data/fem_summary_data.csv") %>% 
                filter(day == "both")

fem_all_data$replicate <- as.factor(fem_all_data$replicate)

ggplot(data = fem_all_data, aes(x = treatment, y = sna_strength)) + geom_boxplot(outlier.color = NA) +
  geom_point(size = 3, alpha = 1) + scale_color_nejm()

strength_model <- lm(data = fem_all_data, sna_strength ~ treatment + replicate)

plot(strength_model)
Anova(strength_model)




