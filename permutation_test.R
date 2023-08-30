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

# Function that shuffles males in association matrices
func_shuffle_males <- function(assoc_mat){
  
     shuf_ID <- sample(colnames(assoc_mat)[1:6])

     colnames(assoc_mat)[1:6] <- shuf_ID
     rownames(assoc_mat)[1:6] <- shuf_ID
     
return(assoc_mat)
}

# Function that turns association matrices into igraph objects and assigns sex, treatment, and strength values to all nodes
func_create_igraph <- function(assoc_mat){
  # Turn matrix into igraph object 
  igraph <- graph_from_adjacency_matrix(assoc_mat, diag = FALSE, weighted = TRUE, mode = "undirected")
  
  # Assign igraph attributes
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male", "female"))
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:4], "social",
                                           ifelse(V(igraph)$name %in% LETTERS[5:8], "isolated", "female")))
  igraph <- set_vertex_attr(igraph, "strength", value = strength(igraph))

  return(igraph)
}

shuf_assoc_matrices <- lapply(assoc_matrices, func_shuffle_males)

shuf_igraph <- lapply(shuf_assoc_matrices, func_create_igraph)

as.data.frame(get.vertex.attribute(shuf_igraph[[1]]))


