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
library(DHARMa)

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 18))

##################### INPUTTING AND ORGANIZING DATA #######################
## Data for aggregation-based networks
groups_agg <- read.csv("males_fall_2022/data/aggregations_by_shelter_male soc exp 2022.csv") %>%  
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
                            value = ifelse(V(igraph)$name %in% LETTERS[1:6], "social",
                                                   ifelse(V(igraph)$name %in% LETTERS[7:12], "isolated", "female")))
  
  V(igraph)$color <- ifelse(V(igraph)$treatment == "social", "#268008", 
                            ifelse(V(igraph)$treatment == "isolated", "#B7E5A7", "gray85"))
  strength <- strength(igraph)
  igraph <- set_vertex_attr(igraph, "strength", value = strength)
  V(igraph)$size <- V(igraph)$strength*12
  V(igraph)$label.color <- NA
  E(igraph)$width <- E(igraph)$weight*6
  
  return(igraph)
}


## Visualizing aggregation-based networks
igraph_objects_agg <- func_igraph(groups_agg_reps[[6]])
plot(func_igraph(groups_agg_reps[[6]]))

#tkplot(igraph_objects_agg)

## Turning igraphs into adjacency matrices
assoc_mat <- as.matrix(as_adjacency_matrix(igraph_objects_agg, attr = "weight"))

write.csv(assoc_mat, "assoc_mat_r6.csv")


#### Export separate network with modifed function for replicate 1 (because I was dumb and didn't use the same ID key)

func_igraph_rep1 <- function(rep_groups){
  group_list <- strsplit(rep_groups$members, " ")
  gbi_matrix <- get_group_by_individual(group_list, data_format = "groups")
  ibi_matrix <- get_network(gbi_matrix, data_format = "GBI")
  ibi_matrix <- ibi_matrix[order(rownames(ibi_matrix)) , order(colnames(ibi_matrix))] # alphabetical order
  igraph <- graph_from_adjacency_matrix(ibi_matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male", "Female"))
  
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[13:24], "female",
                                           ifelse(V(igraph)$name == "A" | V(igraph)$name == "C" | V(igraph)$name == "D" | 
                                                    V(igraph)$name == "G" | V(igraph)$name == "H" | V(igraph)$name == "J",        
                                                  "social", "isolated")))
  
  V(igraph)$color <- ifelse(V(igraph)$treatment == "social", "#268008", 
                            ifelse(V(igraph)$treatment == "isolated", "#B7E5A7", "gray85"))
  strength <- strength(igraph)
  igraph <- set_vertex_attr(igraph, "strength", value = strength)
  V(igraph)$size <- V(igraph)$strength*12
  V(igraph)$label.color <- NA
  E(igraph)$width <- E(igraph)$weight*6
  
  return(igraph)
}

## Visualizing aggregation-based networks
igraph_rep1 <- func_igraph_rep1(groups_agg_reps[[6]])
igraph_rep1 <- delete_vertices(igraph_rep1, "G")

assoc_mat <- as.matrix(as_adjacency_matrix(igraph_rep1, attr = "weight"))

write.csv(assoc_mat, "assoc_mat_r1.csv")






