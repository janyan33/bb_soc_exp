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
rep_1_dat <- read.csv("data/bb_soc_exp_rep_1.csv") %>% 
             filter(behaviour != "trial start")

patch_table_focals <- read.csv("data/patch_focal_1.csv")
patch_table_partner <- read.csv("data/patch_partner_1.csv")

rep_1_dat <- rep_1_dat %>% 
                left_join(patch_table_focals, by = "focal") %>% 
                left_join(patch_table_partner, by = "partner") 

rep_1_dat <- rep_1_dat %>% 
             filter(patch_focal != "G") %>% 
             mutate(partner_sex = ifelse(patch_partner %in% LETTERS[1:12], "male", "female"))

## CREATING MATING AND MOUNTING MATRICES
## Creating a function that turns data into edgelists and then into interaction matrices
func_mount_mat <- function(all_data, behav) {
                  all_data <- all_data %>% 
                  filter(behaviour == behav) %>% 
                  filter(patch_partner != "G") %>% 
                  #filter(day == 2) %>% 
                  filter(patch_focal != "G") %>% 
                  #filter(patch_partner != NA) %>% 
                  select(c(patch_focal, patch_partner)) %>% 
                  mutate(edge_weight = 1)
                  mount_edgelist <- aggregate(data = all_data, edge_weight ~ patch_focal + patch_partner, FUN = sum)
                  mount_matrix <- edgelist_to_adjmat(mount_edgelist[1:2], w = mount_edgelist$edge_weight, 
                                     undirected = FALSE)
  return(as.matrix(mount_matrix))
}

mount_matrix <- func_mount_mat(rep_1_dat, behav = "mount")
insem_matrix <- func_mount_mat(rep_1_dat, behav = "insemination")

## FUNCTION TO TURN MATRICES INTO NETWORKS
func_matrix_to_igraph <- function(matrix, mode, behaviour){
           igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = mode)
           igraph <- set_vertex_attr(igraph, "sex", 
                                     value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male", "Female"))
           igraph <- set_vertex_attr(igraph, "treatment", 
                                     value = ifelse(V(igraph)$name %in% LETTERS[13:24], "female",
                                             ifelse(V(igraph)$name == "A" | V(igraph)$name == "C" | V(igraph)$name == "D" | 
                                                    V(igraph)$name == "G" | V(igraph)$name == "H" | V(igraph)$name == "J",        
                                                    "social", "isolated")))
           strength <- strength(igraph, mode = "all")
           out_strength <- strength(igraph, mode = "out")
           igraph <- set_vertex_attr(igraph, behaviour, value = out_strength)
           igraph <- set_vertex_attr(igraph, behaviour, value = strength)
           #V(igraph)$color <- ifelse(V(igraph)$sex == "Female", "sandybrown", "skyblue3")
           V(igraph)$color <- ifelse(V(igraph)$treatment == "female", "sandybrown", 
                                     ifelse(V(igraph)$treatment == "social", "deepskyblue4", "lightblue1"))
           V(igraph)$label.color <- "black"
           #V(igraph)$size <- strength*3 # USE FOR INSEMINATION NETWORKS
           V(igraph)$size <- ifelse(V(igraph)$sex == "Female", 13, out_strength/2) # USE FOR MOUNT NETWORKS
           E(igraph)$width <- E(igraph)$weight/1.5
           plot(igraph, edge.color = "dimgrey", edge.arrow.size = 0.3)
  return(igraph)
}

mount_network <- func_matrix_to_igraph(mount_matrix, mode = "directed", behaviour = "mount")
insem_network <- func_matrix_to_igraph(insem_matrix, mode = "directed", behaviour = "insemination")

tkplot(mount_network)
tkplot(insem_network)




