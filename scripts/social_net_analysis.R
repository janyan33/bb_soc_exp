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
  strength <- strength(igraph)
  igraph <- set_vertex_attr(igraph, "strength", value = strength)
  return(igraph)
}

## Function for visualizing networks
func_plot_network <- function(igraph_object, node_size){
  V(igraph_object)$color <- ifelse(V(igraph_object)$sex == "Female", "sandybrown", "skyblue3")
  V(igraph_object)$size <- V(igraph_object)$strength*node_size
  V(igraph_object)$label.color <- "white"
  E(igraph_object)$width <- E(igraph_object)$weight*6
  plot(igraph_object, edge.color = "dimgrey") #, vertex.label = NA) allows us to turn on/off vertex labels
}

## Visualizing aggregation-based networks
igraph_objects_agg <- func_igraph(groups_agg_reps[[1]])
rep_2 <- lapply(igraph_objects_agg, node_size = 8, func_plot_network)

tkplot(igraph_objects_agg)


