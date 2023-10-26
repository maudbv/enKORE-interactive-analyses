# Tripartite network for top down representation of themes-RQ-RH

# Libraries
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(visNetwork)
library(dplyr)
library(grDevices)


# Import data ####
source("resources/Hypothesis index.R")

# Build the graph objects ####

# RQ-Hyp network
network_hypRQ <- graph_from_incidence_matrix(
  as.matrix(rhrq_mat),
  directed = FALSE,
  mode = "out")

plot(network_hypRQ)

# Theme network
network_themeRQ <- graph_from_data_frame(
  as.matrix(na.omit(theme_rq_mat[,c("Theme","RQ_abb")])),
  directed = FALSE)

plot(network_themeRQ)

# combine networks into 3 layers ####
network_3layers <- igraph::union(a = network_themeRQ , b = network)
V(network_3layers)$layer = c(rep(1, 4), rep(2, 9), rep(3,39))

# Check that layers match the items:
plot.igraph(network_3layers,vertex.color=c("firebrick","#009EEE","black")[V(network_3layers)$layer])
# Create labels
V(network_3layers)$label = V(network_3layers)$name
V(network_3layers)$label[1:4] = c("Invasion\nimpacts","Invasion\nsuccess","Introduction\npathways", "Management")
V(network_3layers)$label <- stringr::str_replace(V(network_3layers)$label, pattern = " ", replacement = "\n")


# with igraph ####

# Custom layout:
MyLO = matrix(0, nrow=vcount(network_3layers), ncol=2)

## Horizontal position is determined by layer
MyLO[,1] = V(network_3layers)$layer

## Vertical position is defined by pre-existing order in the table
for(i in 1:3) {
  L  = which(V(network_3layers)$layer ==i)
  OL = 1: length(V(network_3layers)$name[L])
  MyLO[ L,2] = (OL-max(OL))/(min(OL)-max(OL))
}

# MyLO[,1] = c(3,2,1) [MyLO[,1]] # for changing order of layers
par(mar = c(0,0,0,0))
plot(network_3layers ,
     layout=MyLO[,c(1,2)],
     vertex.color=c("lightgrey","coral","black")[V(network_3layers)$layer],
     vertex.shape=c("circle","rectangle","none")[V(network_3layers)$layer],
     vertex.size=c(50,60,15)[V(network_3layers)$layer],
     vertex.label = V(network_3layers)$label,
     vertex.label.dist = c(0,0,0)[V(network_3layers)$layer],
     vertex.label.degree = c(0,0,pi)[V(network_3layers)$layer],
     vertex.label.cex = c(1,0.8,0.8)[V(network_3layers)$layer],
     vertex.label.color = "black",
     # vertex.label.family = "Roboto slab",
     vertex.label.family = "Helvetica",
     vertex.label.font = c(2,1,1)[V(network_3layers)$layer]
     )




MyLO_vert <- MyLO[,c(2,1)]
MyLO_vert[,2] = c(3,2,1) [MyLO_vert[,2]] # for changing order of layers
MyLO_vert[,1] <-MyLO_vert[,1] *3
par(mar = c(0,0,0,0))
plot(network_3layers , xlim = c(-0.6, 0.6), ylim = c(-1,1),
     layout= MyLO_vert,
     vertex.color=c("lightgrey","coral","black")[V(network_3layers)$layer],
     vertex.shape=c("circle","rectangle","none")[V(network_3layers)$layer],
     vertex.size=c(30,20,15)[V(network_3layers)$layer],
     vertex.label = V(network_3layers)$label,
     vertex.label.dist = c(0,0,0)[V(network_3layers)$layer],
     vertex.label.cex = c(0.8,0.7,0.5)[V(network_3layers)$layer],
     vertex.label.color = "black",
     # vertex.label.family = "Roboto slab",
     vertex.label.family = "Helvetica",
     vertex.label.font = c(2,1,1)[V(network_3layers)$layer]
)

## with bipartite network 
library(bipartite )

# extract simple adjacency matrices
web = rhrq_mat
web2 = as.matrix(as_adjacency_matrix(network_themeRQ))[-(1:4),1:4]

plotweb(web)
plotweb(web2)

plotweb2(web, web2,
         arrow = "down")

# UGLY

# with vizNetwork ####
# convert to networkD3 DOES nOT work here! Introduces false links...
# net <- igraph_to_networkD3(network, group = vertex_attr(network)$type)

#nodes_3L
nodes_3L <- data.frame(
  id = 1 : length(vertex_attr(network_3layers)$name),
  as.data.frame(vertex_attr(network_3layers))[,-1]
  )

nodes_3L <- rename(nodes_3L, label = name )

# type of node

nodes_3L$type =  c("Theme", "Research question","Hypothesis")[nodes_3L$layer]

# give full node names 
nodes_3L$name <- "NA"
nodes_3L$name[nodes_3L$type =="Research question"] =  theme_rq_mat[
  match(nodes_3L$label[nodes_3L$type =="Research question"],
        theme_rq_mat$RQ_abb),
  "Research question"]

nodes_3L$name[nodes_3L$type =="Hypothesis"] =  hyp_def[
  match(nodes_3L$label[nodes_3L$type =="Hypothesis"],
        hyp_def$Hypothesis),
  "Name"]


nodes_3L$name[nodes_3L$type =="Theme"] =  theme_rq_mat[
  match(nodes_3L$label[nodes_3L$type =="Theme"],
        theme_rq_mat$RQ_abb),
  "Theme"]

# simplify theme labels

nodes_3L$label[1:4] <- c("Impact","Success","Pathways","Management")

# define layers:

nodes_3L$level = nodes_3L$llayer

# format vis
nodes_3L <-  data.frame(
  nodes_3L,
  # control shape of nodes_3L
  shape = c("square","ellipse", "ellipse")[nodes_3L$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p><i>",
                 nodes_3L$type,
                 "</i><br><b>",
                 nodes_3L$name,
                 "</b><br> </p>"
                 ),
  
  # color
  color = c("firebrick","#0085AF", "white")[nodes_3L$layer],
  
  # texf format
  font.color = c("black", "black","black")[nodes_3L$layer], 
  # font.size = c(10,20)[nodes_3L$group+1],
  # shadow
  shadow = FALSE 
)

#Edges
edges_3L <- as.data.frame(as_edgelist(network_3layers, names = FALSE))
colnames(edges_3L) <- c("from", "to")

# plot

plot_3L_network <- function(n = nodes_3L, e = edges_3L) {
  
 p <- visNetwork::visNetwork(
   n,
   e,
   height = "600px",
   width = "100%",
   main = list(
     text = "Research questions and hypotheses in Invasion Ecology",
     style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;")) %>%
   visNodes(
     font = list(size = 80)
   ) %>%
   visEdges(
     shadow = FALSE,
     color = list(color = "#0085AF", highlight = "#C62F4B")
   ) %>%
   # visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
   #            autoResize = TRUE,
   #            manipulation = FALSE) %>%

    visHierarchicalLayout(parentCentralization = FALSE,
                         levelSeparation = 200 )
 return(p)
}

p <- plot_3L_network ()

p



visPhysics(enabled = FALSE,
           solver = "forceAtlas2Based", 
           forceAtlas2Based = list(gravitationalConstant = -200)) %>%
  visInteraction(navigationButtons = TRUE) %>%
  
visHierarchicalLayout(levelSeparation = 15000,
                      treeSpacing = 250,
                      nodeSpacing = 100,
                      edgeMinimization = FALSE,
                      shakeTowards = "roots",
                      parentCentralization = TRUE,
                      blockShifting = FALSE,
                      direction = "LR")