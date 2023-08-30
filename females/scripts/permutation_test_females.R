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

####### LOADING OPPOSITE-SEX ASSOCIATION MATRICES IN
assoc_mat_1 <- as.matrix(read.csv("females/data/assoc_mat_r1_fem.csv", row.names = 1))
assoc_mat_2 <- as.matrix(read.csv("females/data/assoc_mat_r2_fem.csv", row.names = 1))
assoc_mat_3 <- as.matrix(read.csv("females/data/assoc_mat_r3_fem.csv", row.names = 1))
assoc_mat_4 <- as.matrix(read.csv("females/data/assoc_mat_r4_fem.csv", row.names = 1))
assoc_mat_5 <- as.matrix(read.csv("females/data/assoc_mat_r5_fem.csv", row.names = 1))
assoc_mat_6 <- as.matrix(read.csv("females/data/assoc_mat_r6_fem.csv", row.names = 1))

assoc_matrices <- list(assoc_mat_1, assoc_mat_2, assoc_mat_3, assoc_mat_4, assoc_mat_5, assoc_mat_6) # Combine matrices into one list

####### FUNCTIONS THAT GET USED IN LOOP COMPONENT OF THE PERMUTATION TEST
# Function that shuffles MALES in association matrices
func_shuffle_males <- function(assoc_mat){
  
  shuf_ID <- sample(colnames(assoc_mat)[9:16])
  
  colnames(assoc_mat)[9:16] <- shuf_ID
  rownames(assoc_mat)[9:16] <- shuf_ID
  
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
                            value = ifelse(V(igraph)$name %in% LETTERS[1:8], "male",
                                           ifelse(V(igraph)$name %in% LETTERS[9:12], "social", "isolated")))
  igraph <- set_vertex_attr(igraph, "strength", value = strength(igraph))
  
  return(igraph)
}

# Function that takes the list of six igraph objects and puts their attributes into a single data frame
func_attr <- function(igraph_objects){
  attr <- data.frame()
  for (i in 1:length(igraph_objects)){
    igraph_objects[[i]] <- set_vertex_attr(igraph_objects[[i]], "replicate", value = i) # add rep column
    attr <- rbind(attr, vertex_attr(igraph_objects[[i]])) # add rep to growing data frame
    attr <- attr %>% 
            filter(treatment != "male")
  }
  return(attr)
}

####### CALCULATING OBSERVED VALUE
obs_igraphs <- lapply(assoc_matrices, func_create_igraph)
obs_attr <- func_attr(obs_igraphs) %>% 
            filter(treatment != "male")

obs_lmm <- lmer(data = obs_attr, strength ~ treatment + (1|replicate)) # observed lmm
obs_coef <- summary(obs_lmm)$coefficient[2,1] # store observed model coef

######### BUILDING LOOP (WORK IN PROGRESS)
n_sim <- 999
set.seed(33) # selected a priori
coefs <- numeric(n_sim)

for (i in 1:n_sim){
  random_assoc_mats <- lapply(assoc_matrices, func_shuffle_males)
  random_igraphs <- lapply(random_assoc_mats, func_create_igraph)
  new_attr <- func_attr(random_igraphs)
  
  shuf_lmm <- lmer(data = new_attr, strength ~ treatment + (1|replicate))
  coefs[i] <- summary(shuf_lmm)$coefficient[2,1] # model coef for social treatment
}     

# Getting p-value (two-tailed test)
ifelse(obs_coef >= mean(coefs), p <- 2*mean(coefs >= obs_coef), p <- 2*mean(coefs <= obs_coef))

# Graphing histogram of results from loop
coefs <- append(coefs, obs_coef)
hist(unlist(coefs), xlim = c(min(coefs), max(coefs)), ylim = c(0, 250), col = "aliceblue", 
     xlab = "Coefficient value for treatment(social)", main = NA)

lines(x = c(obs_coef, obs_coef), col = "red", lty = "dashed", lwd = 2, y = c(0, 250))
text(0.4, 150, "p = 0.40")
