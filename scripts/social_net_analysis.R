## setwd("C:/Users/janya/Desktop/R/bb_soc_exp")

library(tidyverse)
library(asnipe)
library(igraph)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(glmmTMB)
library(assortnet)
library(janitor)

##################### INPUTTING AND ORGANIZING DATA #######################

## Data for aggregation-based networks
groups_agg <- read.csv("data/aggregations.csv") %>%  
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
  
  #This chunk is only for R1 who's letter ID assignments are different than the rest
  #igraph <- set_vertex_attr(igraph, "treatment", 
  #                          value = ifelse(V(igraph)$name %in% LETTERS[13:24], "female",
  #                                        ifelse(V(igraph)$name == "A" | V(igraph)$name == "C" | V(igraph)$name == "D" | 
  #                                                  V(igraph)$name == "G" | V(igraph)$name == "H" | V(igraph)$name == "J",        
  #                                               "social", "isolated")))
  
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[13:24], "female",
                                          ifelse(V(igraph)$name %in% LETTERS[1:6], "social", "isolated")))
  
  V(igraph)$color <- ifelse(V(igraph)$treatment == "female", "sandybrown", 
                            ifelse(V(igraph)$treatment == "social", "deepskyblue4", "lightblue1"))
  strength <- strength(igraph)
  igraph <- set_vertex_attr(igraph, "strength", value = strength)
  V(igraph)$size <- V(igraph)$strength*8
  V(igraph)$label.color <- "black"
  E(igraph)$width <- E(igraph)$weight*6
  
  return(igraph)
}


## Visualizing aggregation-based networks
igraph_objects_agg <- func_igraph(groups_agg_reps[[1]])


tkplot(igraph_objects_agg)



