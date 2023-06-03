# Tripartite network for top down representation of themes-RQ-RH

# Visualize network of hypotheses merging urban ecology and invasion biology hypotheses from 3 publications: Enders et al. 2020, Daly et al. 2022 and Lokatis et al. 2023.

# Libraries
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(visNetwork)
library(dplyr)
library(grDevices)

# Import network of 39 hyps by Enders et al.  ####
hyp_mat <- readr::read_csv("resources/additional data/Roxane hyps merged with attributes.csv",
                           trim_ws = TRUE,skip_empty_rows = TRUE)
hyp_mat <- as.data.frame(hyp_mat)

# get unique rownames
hyp_mat[which(duplicated(hyp_mat$Acronym)),] %>%
  select(Acronym, Hypothesis,Definition)

#####
# Acronym                                         Hypothesis
# 25       DS                                     Matrix species
# 59       HP                           Herbivore proliferation*
# 71       IS Non-native species hypothesis* aka Invader species
# 96       PO                ﻿Pest outbreaks - defense-free space
# 99      PPH                             Predator proliferation
# 118      SP                                     Suburban peak*

# replace redundant acronyms
hyp_mat[which(duplicated(hyp_mat$Acronym)),"Acronym"] <- c(
  "MS",    # Matrix Species
  "HbP",   # Herbivore Proliferation
  "UIS",   # Urban Invader Species
  "DFS",   # Defense Free Space
  "HPPC",  # High Predator Proliferation in Cities"
  "SUP"    # SubUrban Peak
  )

rownames(hyp_mat) <- hyp_mat$Acronym

#Extract the columns corresponding to an incidence matrix
hyp_mat_inc <- hyp_mat %>%
  select(`Behavioral traits`:`in general...64`)

# convert data form Enders PhD (coded with A and B) as 0 and 1: 
hyp_mat_inc[grep("1B", hyp_mat_inc)] <- 1
hyp_mat_inc[grep("B", hyp_mat_inc)] <- 0
hyp_mat_inc[grep("A", hyp_mat_inc)] <- 1
hyp_mat_inc[grep("C", hyp_mat_inc)] <- 0

# remove all trailing spaces
hyp_mat_inc <- apply(hyp_mat_inc ,
                     2 ,
                     stringr::str_replace_all, pattern = " ", "",
                     simplify = TRUE)
       

# convert all to numeric
hyp_mat_inc <- apply(hyp_mat_inc ,
                     2 ,
                     as.numeric,
                     simplify = TRUE)

rownames(hyp_mat_inc) <- hyp_mat$Acronym


# build the graph object ####
net_hyp_trait <- graph_from_incidence_matrix(
  hyp_mat_inc,
  directed = FALSE,mode = "out")

plot(net_hyp_trait ,
     layout= layout_with_kk(graph = net_hyp_trait , dim = 3)
)



# with vizNetwork
# convert to networkD3 DOES nOT work here! Introduces false links...
# net <- igraph_to_networkD3(network, group = vertex_attr(network)$type)

V(net_hyp_trait)$label = V(net_hyp_trait)$name

#nodes_hyp_trait
nodes_hyp_trait <- data.frame(
  id = 1 : length(vertex_attr(net_hyp_trait)$name),
  as.data.frame(vertex_attr(net_hyp_trait))
  )

nodes_hyp_trait <- rename(nodes_hyp_trait, label = name )
nodes_hyp_trait$name <- c(hyp_mat$Hypothesis, rep(NA,ncol(hyp_mat_incidence)-1))
nodes_hyp_trait$def <- c(hyp_mat$Definition, rep(NA,ncol(hyp_mat_incidence)-1))
nodes_hyp_trait$ref <-c(hyp_mat$`Key ref`, rep(NA,ncol(hyp_mat_incidence)-1)) 

# type of node
nodes_hyp_trait$layer =  c(1,2)[nodes_hyp_trait$type + 1]
nodes_hyp_trait$style =  c("hypothesis","attribute")[nodes_hyp_trait$type + 1]

# give full node names 

# format vis
nodes_hyp_trait <-  data.frame(
  nodes_hyp_trait,
  # control shape of nodes_hyp_trait
  shape = c("square","ellipse")[nodes_hyp_trait$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p><i>",
                 nodes_hyp_trait$style,
                 "</i><br><b>",
                 nodes_hyp_trait$name,
                 "</b><br> </p>"
                 ),
  
  # color
  color = c("firebrick", "lightgrey")[nodes_hyp_trait$layer],
  
  # texf format
  font.color = c("black", "black")[nodes_hyp_trait$layer], 
  # font.size = c(10,20)[nodes_hyp_trait$group+1],
  # shadow
  shadow = FALSE 
)

#Edges
edges_hyp_trait <- as.data.frame(as_edgelist(net_hyp_trait, names = FALSE))
colnames(edges_hyp_trait) <- c("from", "to")

# plot

plot_hyp_trait_network <- function(n = nodes_hyp_trait, e = edges_hyp_trait) {
  
 p <- visNetwork::visNetwork(
   n ,
   e,
   height = "600px",
   width = "100%",
   main = list(
     text = "Research questions and hypotheses in Invasion Ecology",
     style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;")) %>%
   visNodes(
     font = list(size = 80),
      
   ) %>%
   visEdges(
     shadow = FALSE,
     color = list(color = "#0085AF", highlight = "#C62F4B")
   ) %>%
   visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
              autoResize = TRUE,
              manipulation = FALSE) %>%
   visPhysics(enabled = FALSE,
              solver = "forceAtlas2Based", 
              forceAtlas2Based = list(gravitationalConstant = -200)) %>%
   visInteraction(navigationButtons = TRUE) %>%
   visIgraphLayout(layout = layout_with_kk)
   
 return(p)
}

p <- plot_hyp_trait_network ()

p
