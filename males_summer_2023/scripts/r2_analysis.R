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
rep_2_dat <- read.csv("males_summer_2023/data/soc_exp_male_r2.csv") %>% 
  filter(behaviour != "trial start")

patch_table_focals <- read.csv("males_summer_2023/data/patch_tables/patch_focal_r2.csv")
patch_table_partner <- read.csv("males_summer_2023/data/patch_tables/patch_partner_r2.csv")

# Change number IDs to letter IDs
rep_2_dat <- rep_2_dat %>% 
  left_join(patch_table_focals, by = "focal") %>% 
  left_join(patch_table_partner, by = "partner")

# Looking for typos
unique(rep_2_dat$patch_focal)

# Add sex column
rep_2_dat <- rep_2_dat %>% 
  mutate(partner_sex = ifelse(patch_partner %in% LETTERS[9:16], "female", "male"))

# Generate csv file with added columns (patch ID and sex of partner)
write.csv(rep_2_dat, "rep_2_Rdata.csv")


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
func_mount_mat <- function(all_data) {
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
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "Male", "Female"))
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[9:16], "female",
                                           ifelse(V(igraph)$name %in% LETTERS[1:4], "social", "isolated")))
  strength <- strength(igraph, mode = "all")
  out_strength <- strength(igraph, mode = "out")
  igraph <- set_vertex_attr(igraph, behaviour, value = out_strength)
  #V(igraph)$color <- ifelse(V(igraph)$sex == "Female", "sandybrown", "skyblue4")
  V(igraph)$color <- ifelse(V(igraph)$treatment == "female", "sandybrown", 
                            ifelse(V(igraph)$treatment == "social", "deepskyblue4", "lightblue1"))
  V(igraph)$label.color <- "black"
  V(igraph)$size <- strength*5 # USE FOR INSEMINATION NETWORKS
  #V(igraph)$size <- ifelse(V(igraph)$sex == "Female", 15, out_strength/2) # USE FOR MOUNT NETWORKS
  E(igraph)$width <- E(igraph)$weight
  plot(igraph, edge.color = "dimgrey", edge.arrow.size = 0.3)
  return(igraph)
}

# GENERATE THE TWO IGRAPH OBJECTS 
mount_network <- func_matrix_to_igraph(mount_matrix, mode = "directed", behaviour = "mount")
insem_network <- func_matrix_to_igraph(insem_matrix, mode = "undirected", behaviour = "insemination")

tkplot(insem_network)
