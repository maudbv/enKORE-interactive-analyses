# Tripartite network for top down representation of themes-RQ-RH

# Libraries
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(visNetwork)
library(dplyr)
library(grDevices)
library(tcltk)

# Import data ####
source("resources/Hypothesis index.R")

# Build the graph objects ####

# RQ-Hyp network
network_hypRQ <- graph_from_incidence_matrix(
  as.matrix(rhrq_mat),
  directed = TRUE,
  mode = "in")

plot(network_hypRQ)

# Theme network
network_themeRQ <- graph_from_data_frame(
  as.matrix(na.omit(theme_rq_mat[,c("Theme","RQ_abb")])),
  directed = TRUE)

plot(network_themeRQ)

# combine networks into 3 layers ####
network_3layers <- igraph::union(a = network_themeRQ , b = network_hypRQ)
V(network_3layers)$layer = c(rep(1, 4), rep(2, 9), rep(3,39))

V(network_3layers)[1:4] <- V(network_3layers)[4:1]

# Check that layers match the items:
plot.igraph(network_3layers,vertex.color=c("firebrick","#009EEE","white")[V(network_3layers)$layer])

# Match hypothesis names
V(network_3layers)$full_name <- "NA"

V(network_3layers)$full_name[V(network_3layers)$layer == 1] <- 
  V(network_3layers)$name[V(network_3layers)$layer == 1]

V(network_3layers)$full_name [V(network_3layers)$layer == 2] <-  
  theme_rq_mat[match(V(network_3layers)$name[V(network_3layers)$layer == 2],
                     theme_rq_mat$RQ_abb),"Research question"]

V(network_3layers)$full_name[V(network_3layers)$layer == 3] <-  
  hyp_mat[match(V(network_3layers)$name[V(network_3layers)$layer == 3],
        hyp_mat$Acronym),"Hypothesis_label"]

# Create labels
V(network_3layers)$label = V(network_3layers)$full_name
V(network_3layers)$label[1:4] = c("Introduction\npathways","Invasion\nsuccess","Invasion\nimpacts", "Management")
V(network_3layers)$label[V(network_3layers)$layer == 2] <- 
  as.character(theme_rq_mat[match(V(network_3layers)$full_name[V(network_3layers)$layer == 2],  theme_rq_mat$`Research question`),"Research question_2lines"])

# add ltext afjustment?
V(network_3layers)$adj = 0.5
V(network_3layers)$adj[V(network_3layers)$layer == 3] <- 0

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

#horizontal
# MyLO[,1] = c(3,2,1) [MyLO[,1]] # for changing order of layers
par(mar = c(0,0,0,4), adj = 0)
plot.igraph(network_3layers ,
     layout=MyLO,
     vertex.color=c("lightgrey","coral","black")[V(network_3layers)$layer],
     vertex.shape=c("none","rectangle","none")[V(network_3layers)$layer],
     vertex.size=c(30,60,15)[V(network_3layers)$layer],
     vertex.label = gsub("\\\\n", "\n", V(network_3layers)$label),
     vertex.label.dist = c(-3,-3.5,0)[V(network_3layers)$layer],
     vertex.label.degree = c(0,0,0)[V(network_3layers)$layer],
     vertex.label.cex = c(0.8,0.5,0.5)[V(network_3layers)$layer],
     vertex.label.color = "black",
     # vertex.label.family = "Roboto slab",
     vertex.label.family = "Helvetica",
     vertex.label.font = c(2,1,1)[V(network_3layers)$layer],
     edge.arrow.size = 0
     )

# VisNetwork  #########
#nodes_3L
nodes_3L <- data.frame(
  id = 1 : length(vertex_attr(network_3layers)$name),
  as.data.frame(vertex_attr(network_3layers))[,-1]
  )


# type of node

nodes_3L$type =  c("Theme", "Research question","Hypothesis")[nodes_3L$layer]

nodes_3L$level = nodes_3L$layer

# format vis
nodes_3L <-  data.frame(
  nodes_3L,
  # control shape of nodes_3L
  shape = c("box","box", "box")[nodes_3L$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p><i>",
                 nodes_3L$type,
                 "</i><br><b>",
                 nodes_3L$full_name,
                 "</b><br> </p>"
                 ),
  
  # color
  color = c("grey","coral", "white")[nodes_3L$layer],
  
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
     font = list(size = 50)
   ) %>%
   visEdges(
     shadow = FALSE,
     color = list(color = "#0085AF", highlight = "#C62F4B")
   ) %>% 
   visPhysics(enabled = FALSE,
              solver = "forceAtlas2Based", 
              forceAtlas2Based = list(gravitationalConstant = -200)) %>%
   visInteraction(navigationButtons = FALSE) %>%
   visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
              autoResize = TRUE,
              manipulation = FALSE) %>%

   visHierarchicalLayout(levelSeparation = 1000,
                         treeSpacing = 100,
                         nodeSpacing = 100,
                         edgeMinimization = FALSE,
                         shakeTowards = "roots",
                         parentCentralization = TRUE,
                         blockShifting = TRUE,
                         direction = "LR")
 return(p)
}

p <- plot_3L_network ()

p



  
visHierarchicalLayout(levelSeparation = 15000,
                      treeSpacing = 250,
                      nodeSpacing = 100,
                      edgeMinimization = FALSE,
                      shakeTowards = "roots",
                      parentCentralization = TRUE,
                      blockShifting = FALSE,
                      direction = "LR")