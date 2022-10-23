library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(janitor)
library(car)
library(igraph)
library(ggsci)
library(netdiffuseR)

# Script that allows igraph plots to change arrow size
#source("scripts/igraphplot2.R")
     #  environment(plot.igraph2) <- asNamespace('igraph')
      # environment(igraph.Arrows2) <- asNamespace('igraph')

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))

# Load data in
rep_2_dat <- read.csv("data/bb_soc_exp_rep_2.csv") %>% 
  filter(behaviour != "trial start")

patch_table_focals <- read.csv("data/patch_focal_2.csv")
patch_table_partner <- read.csv("data/patch_partner_2.csv")

rep_2_dat <- rep_2_dat %>% 
  left_join(patch_table_focals, by = "focal") %>% 
  left_join(patch_table_partner, by = "partner") 

rep_2_dat <- rep_2_dat %>% 
  filter(patch_focal != "D") %>% 
  mutate(partner_sex = ifelse(patch_partner %in% LETTERS[1:12], "male", "female"))

## CREATING MATING AND MOUNTING MATRICES
## Creating a function that turns data into edgelists and then into insemination matrices 
func_insem_mat <- function(all_data, Day) {
  all_data <- all_data %>% 
    filter(behaviour == "insemination") %>% 
    filter(day == Day) %>% 
    filter(patch_partner != "D") %>% 
    filter(patch_focal != "D") %>% 
    #filter(patch_partner != NA) %>% 
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
    filter(day == Day) %>% 
    filter(patch_partner != "D") %>% 
    filter(patch_focal != "D") %>% 
    #filter(patch_partner != NA) %>% 
    select(c(patch_focal, patch_partner)) %>% 
    mutate(edge_weight = 1)
  mount_edgelist <- aggregate(data = all_data, edge_weight ~ patch_focal + patch_partner, FUN = sum)
  mount_matrix <- edgelist_to_adjmat(mount_edgelist[1:2], w = mount_edgelist$edge_weight, 
                                     undirected = FALSE)
  return(as.matrix(mount_matrix))
}

mount_matrix <- func_mount_mat(rep_2_dat, Day = 1)
insem_matrix <- func_insem_mat(rep_2_dat, Day = 2)

## FUNCTION TO TURN MATRICES INTO NETWORKS
func_matrix_to_igraph <- function(matrix, mode, behaviour){
  igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = mode)
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male", "Female"))
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[13:24], "female",
                                           ifelse(V(igraph)$name %in% LETTERS[1:6], "social", "isolated")))
  strength <- strength(igraph, mode = "all")
  igraph <- set_vertex_attr(igraph, behaviour, value = strength)
  #V(igraph)$color <- ifelse(V(igraph)$sex == "Female", "sandybrown", "skyblue3")
  V(igraph)$color <- ifelse(V(igraph)$treatment == "female", "sandybrown", 
                            ifelse(V(igraph)$treatment == "social", "deepskyblue4", "lightblue1"))
  V(igraph)$label.color <- "black"
  V(igraph)$size <- strength
  E(igraph)$width <- E(igraph)$weight
  plot(igraph, edge.color = "dimgrey")
  return(igraph)
}

# GENERATE THE TWO IGRAPH OBJECTS 
mount_network <- func_matrix_to_igraph(mount_matrix, mode = "undirected", behaviour = "mount")
insem_network <- func_matrix_to_igraph(insem_matrix, mode = "undirected", behaviour = "insemination")

# SET COORDINATES FOR BOTH NETWORKS
id <- insem_network
tkplot(id)

tk_coords(id) # Saves coordinates of current network
tk_set_coords(id, coords) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

tk_set_coords(id, coords_42) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)



V(mount_network)
get.vertex.attribute(insem_network)



id <- tkplot(insem_network)
tkplot.fit.to.screen(id)
