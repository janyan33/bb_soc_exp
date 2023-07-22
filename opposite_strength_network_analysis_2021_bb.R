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

####### LOADING OPPOSITE-SEX ASSOCIATION MATRICES FROM 2021 EXPERIMENT IN
assoc_mat_1 <- as.matrix(read.csv("assoc_mat_12_1_2021_bb.csv", row.names = 1))
assoc_mat_2 <- as.matrix(read.csv("assoc_mat_12_2_2021_bb.csv", row.names = 1))
assoc_mat_3 <- as.matrix(read.csv("assoc_mat_12_3_2021_bb.csv", row.names = 1))


assoc_matrices <- list(assoc_mat_1, assoc_mat_2, assoc_mat_3) # Combine matrices into one list


## FUNCTION TO TURN MATRICES INTO NETWORK FIGURES
func_oppo_network <- function(assoc_mat){
  
  # Turn matrix into igraph object 
  igraph <- graph_from_adjacency_matrix(assoc_mat, diag = FALSE, weighted = TRUE, mode = "undirected")
  
  # Assign igraph attributes
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:12], "male", "female"))
  
  igraph <- set_vertex_attr(igraph, "strength", value = strength(igraph))
  
  # Customize plots
  V(igraph)$color <- ifelse(V(igraph)$sex == "male", "skyblue3", "sandybrown")
  
  V(igraph)$names<- FALSE
  V(igraph)$size <- V(igraph)$strength*12
  E(igraph)$width <- E(igraph)$weight*10
  E(igraph)$color <- "dimgray"
  
  return(igraph)
}

igraph_list <- lapply(assoc_matrices, func_oppo_network)
plot(igraph_list[[3]], vertex.label = NA)

tkplot(igraph_list[[4]], vertex.label = NA)

# Export strength values
degree(igraph_list[[3]])
