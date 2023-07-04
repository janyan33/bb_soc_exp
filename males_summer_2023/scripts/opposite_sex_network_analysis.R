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
assoc_mat_1 <- as.matrix(read.csv("males_summer_2023/opposite_sex_networks/assoc_mat_r1.csv", row.names = 1))
assoc_mat_2 <- as.matrix(read.csv("males_summer_2023/opposite_sex_networks/assoc_mat_r2.csv", row.names = 1))
assoc_mat_3 <- as.matrix(read.csv("males_summer_2023/opposite_sex_networks/assoc_mat_r3.csv", row.names = 1))
assoc_mat_4 <- as.matrix(read.csv("males_summer_2023/opposite_sex_networks/assoc_mat_r4.csv", row.names = 1))
assoc_mat_5 <- as.matrix(read.csv("males_summer_2023/opposite_sex_networks/assoc_mat_r5.csv", row.names = 1))
assoc_mat_6 <- as.matrix(read.csv("males_summer_2023/opposite_sex_networks/assoc_mat_r6.csv", row.names = 1))

assoc_matrices <- list(assoc_mat_1, assoc_mat_2, assoc_mat_3, assoc_mat_4, assoc_mat_5, assoc_mat_6) # Combine matrices into one list


## FUNCTION TO TURN MATRICES INTO NETWORK FIGURES
func_oppo_network <- function(assoc_mat){
                      
                     # Turn matrix into igraph object 
                     igraph <- graph_from_adjacency_matrix(assoc_mat, diag = FALSE, weighted = TRUE, mode = "undirected")
                     
                     # Assign igraph attributes
                     igraph <- set_vertex_attr(igraph, "sex", 
                                value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male", "female"))
  
                     igraph <- set_vertex_attr(igraph, "treatment", 
                               value = ifelse(V(igraph)$name %in% LETTERS[1:4], "social",
                                       ifelse(V(igraph)$name %in% LETTERS[5:8], "isolated", "female")))
                     
                     igraph <- set_vertex_attr(igraph, "strength", value = strength(igraph))
                     
                     # Customize plots
                     V(igraph)$color <- ifelse(V(igraph)$treatment == "social", "#268008", 
                                        ifelse(V(igraph)$treatment == "isolated", "#B7E5A7", "gray85"))
  
                     V(igraph)$names<- FALSE
                     V(igraph)$size <- V(igraph)$strength*12
                     E(igraph)$width <- E(igraph)$weight*10
                     E(igraph)$color <- "dimgray"
                     
  
  return(igraph)
}

igraph_list <- lapply(assoc_matrices, func_oppo_network)
plot(igraph_list[[6]], vertex.label = NA)

tkplot(igraph_list[[6]], vertex.label = NA)

strength(igraph_list[[6]])
