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
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))

# Load data in
rep_2_dat <- read.csv("females/data/raw/soc_exp_fem_r2.csv") %>% 
  filter(behaviour != "TRIAL START")

patch_table_focals <- read.csv("females/data/patch_tables/patch_focal_f2.csv")
patch_table_partner <- read.csv("females/data/patch_tables/patch_partner_f2.csv")

rep_2_dat <- rep_2_dat %>% 
  left_join(patch_table_focals, by = "focal") %>% 
  left_join(patch_table_partner, by = "partner") 

## CREATING MATING AND MOUNTING MATRICES
## Creating a function that turns data into edgelists and then into insemination matrices 
func_insem_mat <- function(all_data) {
    all_data <- all_data %>% 
    filter(behaviour == "insemination") %>% 
    select(c(patch_focal, patch_partner)) %>% 
    mutate(edge_weight = 1)
  mount_edgelist <- aggregate(data = all_data, edge_weight ~ patch_focal + patch_partner, FUN = sum)
  mount_matrix <- edgelist_to_adjmat(mount_edgelist[1:2], w = mount_edgelist$edge_weight, 
                                     undirected = FALSE)
  return(as.matrix(mount_matrix))
}

## Creating a function that turns data into edgelists and then into mount matrices
func_mount_mat <- function(all_data, Day) {
  all_data <- all_data %>% 
    filter(behaviour == "insemination" | behaviour == "mount") %>% 
    select(c(patch_focal, patch_partner)) %>% 
    mutate(edge_weight = 1)
  mount_edgelist <- aggregate(data = all_data, edge_weight ~ patch_focal + patch_partner, FUN = sum)
  mount_matrix <- edgelist_to_adjmat(mount_edgelist[1:2], w = mount_edgelist$edge_weight, 
                                     undirected = FALSE)
  return(as.matrix(mount_matrix))
}

insem_matrix <- func_insem_mat(rep_2_dat)
mount_matrix <- func_mount_mat(rep_2_dat)

## FUNCTION TO TURN MATRICES INTO NETWORKS
func_matrix_to_igraph <- function(matrix, mode, behaviour){
  igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = mode)
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male",
                                           ifelse(V(igraph)$name %in% LETTERS[9:12], "social", "isolated")))
  strength <- strength(igraph, mode = "all")
  out_strength <- strength(igraph, mode = "out")
  igraph <- set_vertex_attr(igraph, behaviour, value = strength)
  V(igraph)$color <- ifelse(V(igraph)$treatment == "male", "#118ab2", 
                            ifelse(V(igraph)$treatment == "social", "#f77f00", "#f6bd60"))
  V(igraph)$label.color <- "black"
  V(igraph)$size <- strength*5 # CHANGE MULTIPLIER FOR MOUNT VS. INSEM NETWORKS
  E(igraph)$width <- E(igraph)$weight*3
  plot(igraph, edge.color = "dimgrey", edge.arrow.size = 0.3)
  return(igraph)
}
# GENERATE THE TWO IGRAPH OBJECTS 
mount_network <- func_matrix_to_igraph(mount_matrix, mode = "directed", behaviour = "mount")
insem_network <- func_matrix_to_igraph(insem_matrix, mode = "undirected", behaviour = "insemination")

tkplot(insem_network)

