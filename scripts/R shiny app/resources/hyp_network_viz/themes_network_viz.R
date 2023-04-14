# Visualize network of hypotheses from Enders et al.

# Libraries
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(tidyverse)
library(visNetwork)


# Import network of 39 hyps by Enders et al.  ####
rhrq_mat <- readr::read_csv("resources/additional data/RH-RQ.csv")
theme_rq_mat <- as.data.frame(readr::read_csv("resources/additional data/Theme-RQ.csv"))
hyp_def <- as.data.frame(readr::read_csv("resources/additional data/hyp_def.csv",
                           trim_ws = TRUE
                           ))

# format matrix ####
rhrq_mat <- as.data.frame(rhrq_mat)
rownames(rhrq_mat) <- rhrq_mat$Hypothesis
rhrq_mat <- rhrq_mat[ ,-1]

# build the graph object ####
 network <- graph_from_incidence_matrix(as.matrix(rhrq_mat))
# plot(network)


# with vizNetwork
# convert to networkD3 DOES nOT work here! Introduces false links...
# net <- igraph_to_networkD3(network, group = vertex_attr(network)$type)

#Nodes
nodes <- data.frame(
  id = 1 : length(vertex_attr(network)$name),
  as.data.frame(vertex_attr(network), col.names = c("group","label")))

# type of node
nodes$type =  c("Hypothesis","Research question")[nodes$group+1]

# give full node names 
nodes$name = theme_rq_mat[match(nodes$label,theme_rq_mat$RQ_abb),"Research question"]

nodes[is.na(nodes$name), "name"] <- hyp_def$Name[
  match(nodes$label[is.na(nodes$name)],
        hyp_def$Hypothesis)]

# theme
nodes$theme = theme_rq_mat[match(nodes$label,theme_rq_mat$RQ_abb),"Theme"]


# format vis
nodes <-  data.frame(
  nodes,
  # control shape of nodes
  shape = c("ellipse", "ellipse")[nodes$group+1],
  
  # tooltip (html or character), when the mouse is above
  title = paste0("<p><i>", nodes$type,"</i><br><b>", nodes$name, "</b><br> </p>"),
  
  # color
  color = c("#0085AF", "white")[nodes$group+1],
  
  # texf format
  font.color = c("black", "firebrick")[nodes$group+1], 
  # font.size = c(10,20)[nodes$group+1],
  # shadow
  shadow = FALSE 
)

#Edges
edges <- as.data.frame(as_edgelist(network, names = FALSE))
colnames(edges) <- c("from", "to")

# plot

plot_RQRH_network <- function(n = nodes, e = edges) {
  
 p <- visNetwork(n , e,
           height = "800px",
           width = "100%",
           main = list(
             text = "Research questions and hypotheses in Invasion Ecology",
             style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;")) %>%
  visIgraphLayout() %>%
  visNodes(
    font = list(size = 40)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             autoResize = TRUE) %>%
  visPhysics(stabilization = FALSE) %>%
  visLayout(randomSeed = 11)
 
  return(p)
 }


