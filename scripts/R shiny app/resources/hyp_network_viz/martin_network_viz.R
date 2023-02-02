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
martin_matrix[upper.tri(martin_matrix)] <- martin_matrix[lower.tri(martin_matrix)]
martin_matrix <- as.matrix(martin_matrix)

# choose a consensus network ####
 martin_matrix[abs(martin_matrix)<0.5] <- 0

 # build the graph object ####
 network <- graph_from_adjacency_matrix(martin_matrix,
                                        mode='lower',
                                        weighted = TRUE,
                                        diag=F)
 
# # transform edge to have only positive values
#  edge_num <- E(network)$weight
#  E(network)$weight <-  abs(edge_num)

#  # attempt a clustering in groups : to be done with Martin's resutls?
#  #cluster <- cluster_louvain(network)
# 
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
# 

# with vizNetwork

 # make absolute value edges:
edge_num <- E(network)$weight
E(network)$weight <-  abs(edge_num)

# convert to networkD3 (needs groups)
net <- igraph_to_networkD3(network,group = membership(cluster))
net <- igraph_to_networkD3(network,group = rep(1,length(network)))

# add real names
net$nodes$long_name <-  martin_matrix_abb$hyp[match(net$nodes$name, martin_matrix_abb$abb)]

#Nodes
nodes <- data.frame(id = rownames(net$nodes),
                    net$nodes,
                    stringsAsFactors = FALSE)
names(nodes) <- c("id","label","group","name")
nodes$title = paste0("<p>", nodes$label,"<br>",nodes$name ," hypothesis</p>")

#Edges
edges <- as.data.frame(net$links)
colnames(edges) <- c("from", "to", "width")
edges$width <-( edges$width - min(edges$width))/(max(edges$width) - min(edges$width))  *20


# #plot network
# visNetwork(nodes, edges, width = "100%") %>%
#   visIgraphLayout() %>%
#   visNodes(
#     shape = "dot",
#     color = list(
#       background = "#0085AF",
#       border = "#013848",
#       highlight = "#FF8000"
#     ),
#     shadow = list(enabled = TRUE, size = 10),
#     label = "name",font = list(size = 40)
#   ) %>%
#   visEdges(
#     shadow = FALSE,
#     color = list(color = "#0085AF", highlight = "#C62F4B")
#   ) %>%
#   visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
#              selectedBy = list(variable = "name", main = "Select hypothesis"),
#              autoResize = TRUE) %>% 
#   visPhysics(stabilization = FALSE) %>%
#   visLayout(randomSeed = 11)
