# Visualize network of hypotheses from Enders et al.

# error themes_network_viz.R#43

# Libraries
require(readr)
require(igraph)
require(networkD3)
require(ggraph)
require(visNetwork)


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

#nodes_rhrq
nodes_rhrq <- data.frame(
  id = 1 : length(vertex_attr(network)$name),
  as.data.frame(vertex_attr(network), col.names = c("group","label")))

# type of node
nodes_rhrq$type =  c("Hypothesis","Research question")[nodes_rhrq$group+1]

# give full node names 
nodes_rhrq$name = theme_rq_mat[match(nodes_rhrq$label,theme_rq_mat$RQ_abb),"Research question"]

nodes_rhrq[is.na(nodes_rhrq$name), "name"] <- hyp_def$Hypothesis[
  match(nodes_rhrq$label[is.na(nodes_rhrq$name)],
        hyp_def$Hypothesis)]

# theme
nodes_rhrq$theme = theme_rq_mat[match(nodes_rhrq$label,theme_rq_mat$RQ_abb),"Theme"]


# format vis
nodes_rhrq <-  data.frame(
  nodes_rhrq,
  # control shape of nodes_rhrq
  shape = c("ellipse", "ellipse")[nodes_rhrq$group+1],
  
  # tooltip (html or character), when the mouse is above
  title = paste0("<p><i>", nodes_rhrq$type,"</i><br><b>", nodes_rhrq$name, "</b><br> </p>"),
  
  # color
  color = c("#0085AF", "white")[nodes_rhrq$group+1],
  
  # texf format
  font.color = c("black", "firebrick")[nodes_rhrq$group+1], 
  # font.size = c(10,20)[nodes_rhrq$group+1],
  # shadow
  shadow = FALSE 
)

#Edges
edges_rhrq <- as.data.frame(as_edgelist(network, names = FALSE))
colnames(edges_rhrq) <- c("from", "to")

# plot

plot_rhrq_network <- function(n = nodes_rhrq, e = edges_rhrq) {
  
 p <- visNetwork::visNetwork(
   n ,
   e,
   height = "600px",
   width = "100%",
   main = list(
     text = "Research questions and hypotheses in Invasion Ecology",
     style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;")) %>%
   visNodes(
     font = list(size = 40)
   ) %>%
   visEdges(
     shadow = FALSE,
     color = list(color = "#0085AF", highlight = "#C62F4B")
   ) %>%
   visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
              autoResize = TRUE,
              manipulation = FALSE,
              selectedBy = "theme") %>%
   visPhysics(solver = "forceAtlas2Based", 
              forceAtlas2Based = list(gravitationalConstant = -90)) %>%
   visInteraction(navigationButtons = TRUE) 
 #%>%
 # visIgraphLayout(layout = "layout_with_kk")
 return(p)
}

plot_rhrq_network()
