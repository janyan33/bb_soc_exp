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

##################### INPUTTING AND ORGANIZING DATA #######################
## Data for aggregation-based networks
groups_agg <- read.csv("males_summer_2023/data/aggregations_by_shelter.csv") %>%  
  remove_empty("cols")

groups_agg_reps <- split(groups_agg, groups_agg$replicate)

rep_groups <- groups_agg_reps[[1]]

#################### VISUALIZING SOCIAL NETWORKS ############################
## Function for turning group data into igraph objects
func_igraph <- function(rep_groups){
  group_list <- strsplit(rep_groups$members, " ")
  gbi_matrix <- get_group_by_individual(group_list, data_format = "groups")
  ibi_matrix <- get_network(gbi_matrix, data_format = "GBI")
  ibi_matrix <- ibi_matrix[order(rownames(ibi_matrix)) , order(colnames(ibi_matrix))] # alphabetical order
  igraph <- graph_from_adjacency_matrix(ibi_matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "Male", "Female"))
  
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:4], "social",
                                           ifelse(V(igraph)$name %in% LETTERS[5:8], "isolated", "female")))
  
  V(igraph)$color <- ifelse(V(igraph)$treatment == "social", "blue", 
                            ifelse(V(igraph)$treatment == "isolated", "lightblue", "yellow"))
  
  strength <- strength(igraph)
  igraph <- set_vertex_attr(igraph, "strength", value = strength)
  V(igraph)$size <- V(igraph)$strength*12
  V(igraph)$label.color <- NA
  E(igraph)$width <- E(igraph)$weight*6
  
  ## Test bipartite
  V(igraph)$type <- V(igraph)$sex == "Male"
  
  return(igraph)
}


## Visualizing aggregation-based networks
igraph_objects_agg <- func_igraph(groups_agg_reps[[4]])
plot(func_igraph(groups_agg_reps[[4]]))





bipartite.projection(igraph_objects_agg)








## BIPARTITE NETWORKS
assoc_mat_1 <- read.csv("males_summer_2023/networks/assoc_mat_r1.csv", row.names = 1)
assoc_mat_1 <- as.matrix(assoc_mat_1)


bi_graph_1 <- graph_from_adjacency_matrix(assoc_mat_1, diag = FALSE, weighted = TRUE, mode = "undirected")

bi_graph_1 <- set_vertex_attr(bi_graph_1, "sex", 
                          value = ifelse(V(bi_graph_1)$name %in% LETTERS[1:8], "Male", "Female"))

bi_graph_1 <- set_vertex_attr(bi_graph_1, "treatment", 
                          value = ifelse(V(bi_graph_1)$name %in% LETTERS[1:4], "social",
                                         ifelse(V(bi_graph_1)$name %in% LETTERS[5:8], "isolated", "female")))

V(bi_graph_1)$color <- ifelse(V(bi_graph_1)$treatment == "social", "blue", 
                          ifelse(V(bi_graph_1)$treatment == "isolated", "lightblue", "yellow"))

bi_graph_1 <- set_vertex_attr(bi_graph_1, "strength", value = strength(bi_graph_1))
V(bi_graph_1)$size <- V(bi_graph_1)$strength*15


plot(bi_graph_1)
tkplot(bi_graph_1)



write.csv(assoc_matrix_1, "assoc_mat_r1.csv")


assoc_mat_1 <- read.csv("males_summer_2023/networks/assoc_mat_r1.csv", row.names = 1)
assoc_mat_1 <- as.matrix(assoc_mat_1)


plotweb(assoc_mat_1, labsize = 3, col.high = "blue", col.low = "sandybrown")







##################
plot(func_igraph(groups_agg_reps[[1]]))

tkplot(igraph_objects_agg)


######### STRENGTH ANALYSES ############
fem_all_data <- read.csv("females/data/fem_summary_data.csv") %>% 
  filter(day == "both")

fem_all_data$replicate <- as.factor(fem_all_data$replicate)

ggplot(data = fem_all_data, aes(x = treatment, y = sna_strength, fill = treatment)) + geom_boxplot(alpha = 0.9) +
  scale_fill_manual(values=c("#f8ad9d", "#9e2a2b")) + ylab("Aggregation network strength") + My_Theme

strength_model <- lm(data = fem_all_data, sna_strength ~ treatment + replicate)

plot(strength_model)
Anova(strength_model)