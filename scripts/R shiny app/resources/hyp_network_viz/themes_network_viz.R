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
theme_rq_mat <- readr::read_csv("resources/additional data/Theme-RQ.csv")

# format matrix ####
rhrq_mat <- as.data.frame(rhrq_mat)
rownames(rhrq_mat) <- rhrq_mat$Hypothesis
rhrq_mat <- rhrq_mat[ ,-1]

# build the graph object ####
 network <- graph_from_incidence_matrix(as.matrix(rhrq_mat))
plot(network)


# with vizNetwork
# convert to networkD3 DOES nOT work here!
net <- igraph_to_networkD3(network, group = vertex_attr(network)$type)

#Nodes
nodes <- data.frame(id = 1 : length(vertex_attr(network)$name),
                    as.data.frame(vertex_attr(network)),
                    stringsAsFactors = FALSE)
names(nodes) <- c("id","group","label")
nodes$type <-  c("Hyp","RQ")[nodes$group+1]


#Edges
edges <- as.data.frame(as_edgelist(network, names = FALSE))
colnames(edges) <- c("from", "to")

# plot
visNetwork(nodes , edges) %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    color = list(
      background = "orange",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 10),
    label = "name", 
    font = list(size = 40)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  )
