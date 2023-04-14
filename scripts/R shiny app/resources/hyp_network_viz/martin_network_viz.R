# Visualize network of hypotheses from Enders et al.

# Libraries
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(tidyverse)
library(visNetwork)


# Import network of 39 hyps by Enders et al.  ####
martin_matrix <- readr::read_csv("resources/additional data/geb13082-sup-0001-tables1.csv")
martin_matrix_abb <- martin_matrix[, c(1,2)]
martin_matrix <- martin_matrix[, -c(1,2)]

# format matrix ####
martin_matrix <- as.matrix(martin_matrix)
rownames(martin_matrix) <- colnames(martin_matrix) <- martin_matrix_abb$abb

diag(martin_matrix) <- 0

f <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  m
}

martin_matrix <- f(martin_matrix)
martin_matrix[upper.tri(martin_matrix)] <- t(martin_matrix)[upper.tri(martin_matrix)]

# choose a consensus network ####
martin_matrix[abs(martin_matrix)<0.5] <- 0
martin_matrix[abs(martin_matrix)>=0.5] <- 1


# add clustering based on the publication figure: 
cluster_enders <-  readr::read_csv("resources/additional data/enders_hyp_cluster_bipartite.csv")

cluster <- list(
  Trait = cluster_enders$Hypothesis[which(cluster_enders$Trait == 1)],
  Darwin = cluster_enders$Hypothesis[which(cluster_enders$`Darwin's` == 1)],
  Resource_availability = cluster_enders$Hypothesis[which(cluster_enders$`Resource availability` == 1)],
  Biotic_interactions = cluster_enders$Hypothesis[which(cluster_enders$`Biotic interactions` == 1)],
  Propagule = cluster_enders$Hypothesis[which(cluster_enders$`Propagule` == 1)]
)

# simplified clustering: 
hyp_def <-  readr::read_csv("resources/additional data/hyp_def.csv")
hyp_def <- as.data.frame(hyp_def)
names(hyp_def) <- make.names(names(hyp_def))

# build the graph object ####
network <- graph_from_adjacency_matrix(martin_matrix,
                                       mode='lower',
                                       weighted = TRUE,
                                       diag=F)

# # transform edge to have only positive values
edge_num <- E(network)$weight
E(network)$weight <-  abs(edge_num)


# attempt a clustering in groups : to be done with Martin's resutls?
# cluster <- cluster_louvain(network)

#  # plot with igraph
# par(mar=c(0,0,0,0))
# plot(network,
#      layout = layout.fruchterman.reingold,
#      edge.color = c("red","NA","grey60")[sign(edge_num)+2],
#      edge.width = abs(edge_num)*5,
#      edge.lty=c("solid"),
#      vertex.color = "grey95",
#      vertex.frame.color = "grey95",
#      vertex.label.color = "black",
#      vertex.label.family = "serif"
#  )
# 
# # plot with gggraph ####
# ggraph(network) +
#   geom_edge_link( aes(edge_width=E(network)$weight), edge_colour="black", edge_alpha=0.3) +
#   geom_node_point( color="#69b3a2", size=5) +
#   geom_node_text( aes(label=name), repel = TRUE, size=8, color="#69b3a2") +
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(rep(1,4), "cm")
#   )
# 
# # Plot with network3D ####
# network <- graph_from_adjacency_matrix(martin_matrix,
#                                        mode='lower',
#                                        weighted = TRUE,
#                                        diag=F)
# # convert to networkD3 (needs groups)
# net <- igraph_to_networkD3(network,group = membership(cluster))
# 
# # add real names
# net$nodes$long_name <-  martin_matrix_abb$hyp[match(net$nodes$name, martin_matrix_abb$abb)]
# 
# # interactive visualization:
# forceNetwork(Links = net$links, Nodes = net$nodes,
#              Source = "source", Target = "target",
#              Value = "value", NodeID = "long_name",
#              Group = "group", opacity = 0.8,
#              zoom = TRUE)
# 
# # # for R shiny:
# # forceNetworkOutput(outputId, width = "100%", height = "500px")
# # renderForceNetwork(expr, env = parent.frame(), quoted = FALSE)


# with vizNetwork

# make absolute value edges:
edge_num <- E(network)$weight
E(network)$weight <-  abs(edge_num)

## with vizNetwork
# convert to networkD3 DOES nOT work here! Introduces false links...
# net <- igraph_to_networkD3(network,group = rep(1,length(network)))

# add real names
# net$nodes$long_name <-  martin_matrix_abb$hyp[match(net$nodes$name, martin_matrix_abb$abb)]

#Nodes
nodes <- data.frame(
  id = 1 : length(vertex_attr(network)$name),
  label = vertex_attr(network)$name
)

# add node info: 
nodes$name = hyp_def[match(nodes$label,hyp_def$Hypothesis),"Name"]
nodes$def = hyp_def[match(nodes$label,hyp_def$Hypothesis),"Definition"]
nodes$Wikidata = hyp_def[match(nodes$label,hyp_def$Hypothesis),"Wikidata.ID"]

# ender's clusters
nodes$group = hyp_def[match(nodes$label,hyp_def$Hypothesis),
                      "Cluster.in.Enders.et.al..2020"]

#Edges
edges <- as.data.frame(as_edgelist(network, names = FALSE))
colnames(edges) <- c("from", "to")

nodes$clusters <- paste(
  stringr::str_replace_all(nodes$group,
                         pattern = ";", 
                         replace = " cluster, "),
      "cluster")

# format vis
nodes <-  data.frame(
  nodes,
   # tooltip (html or character), when the mouse is above
  title = paste0("<p><b>", nodes$name,
                 "</b><br> <i>",
                 nodes$clusters,
                 "</i></p>"),
  
  # font.size = c(10,20)[nodes$group+1],
  # shadow
  shadow = FALSE 
)


# #plot network

plot_uni_network <-  function(n = nodes, e = edges,g = "group", w = "100%") {
  p <-  visNetwork(
    nodes,
    edges,
    main = list(
      text = "Similarity among hypotheses in Invasion Ecology",
      style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;"),
    submain = list(
      text = "according to Enders et al. 2020",
      style = "font-family:Roboto slab;color:#0085AF;font-size:11px;text-align:center;")
  ) %>%
  visIgraphLayout() %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B"),
  ) %>%
  visNodes(
    shape = "dot",
    group = "group",
    color = list(
      border = "#013848",
      highlight = "#FF8000"),
    shadow = list(enabled = TRUE, size = 10),
    label = "label", 
    font = list(size = 40)
  ) %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             #selectedBy = list(variable = "name", main = "Select hypothesis"),
             autoResize = TRUE) %>%
  visPhysics(stabilization = FALSE) %>%
  visLayout(randomSeed = 11)

return(p)
}


