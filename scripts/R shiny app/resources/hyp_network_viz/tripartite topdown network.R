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
  directed = TRUE,
  mode = "in")

# plot(network_hypRQ)

# Theme network
network_themeRQ <- graph_from_data_frame(
  as.matrix(na.omit(theme_rq_mat[,c("Theme","RQ_abb")])),
  directed = TRUE)

# plot(network_themeRQ)

# combine networks into 3 layers ####
network_3layers <- igraph::union(a = network_themeRQ , b = network_hypRQ)
V(network_3layers)$layer = c(rep(1, 4), rep(2, 9), rep(3,39))


# # Check that layers match the items:
# plot.igraph(network_3layers,
#             vertex.color=c("firebrick","#009EEE","white")[V(network_3layers)$layer],)

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

V(network_3layers)$def[V(network_3layers)$layer == 3] <-  
  hyp_mat[match(V(network_3layers)$name[V(network_3layers)$layer == 3],
                hyp_mat$Acronym),"Definition"]

V(network_3layers)$def[V(network_3layers)$layer == 1] <- ""
V(network_3layers)$def[V(network_3layers)$layer == 2] <- ""

# Create labels
V(network_3layers)$label = V(network_3layers)$full_name
V(network_3layers)$label[1:4] = c("Introduction\npathways","Invasion\nsuccess","Invasion\nimpacts", "Management")
V(network_3layers)$label[V(network_3layers)$layer == 2] <- 
  as.character(theme_rq_mat[match(V(network_3layers)$full_name[V(network_3layers)$layer == 2],  theme_rq_mat$`Research question`),"Research question_2lines"])

# add ltext afjustment?
V(network_3layers)$adj = 0.5
V(network_3layers)$adj[V(network_3layers)$layer == 3] <- 0

# Interactive hierarchical plot with VisNetwork  #########
#nodes_3L
nodes_3L <- data.frame(
  id = 1 : length(vertex_attr(network_3layers)$name),
  as.data.frame(vertex_attr(network_3layers))[,-1]
  )

# # Invert order of nodes
# nodes_3L$id = length(vertex_attr(network_3layers)$name):1
# nodes_3L$id[1:4] = nodes_3L$id[4:1] # reorder themes

# type of node
nodes_3L$type =  c("Theme", "Research question","Hypothesis")[nodes_3L$layer]
nodes_3L$level = nodes_3L$layer

# make levels into groups for formatting
nodes_3L$group = nodes_3L$level

# Format
nodes_3L$shape = c("box","box", "box")[nodes_3L$level]
nodes_3L$color = c("grey","coral", "white")[nodes_3L$level]
nodes_3L$font.color = c("white", "black","black")[nodes_3L$level]
nodes_3L$font.size = c(60,50, 40)[nodes_3L$level]
nodes_3L$shadow = FALSE 
nodes_3L$font.align = c("center", "center", "left")[nodes_3L$level]

# tooltip (html or character), when the mouse is above
# nodes_3L$title = paste0("<p><i>",
#                         nodes_3L$type,
#                         "</i><br><b>",
#                         nodes_3L$full_name,
#                         "</b><br> </p>",
#                         nodes_3L$def,
#                         "</b><br> </p>"
# )


nodes_3L$title = paste0("<p style=\"display: block; word-wrap:break-word;  width:350px; white-space: normal\"> <i>",
                        nodes_3L$type,
                        "</i><br><b>",
                        nodes_3L$full_name,
                        "</b><br>",
                        nodes_3L$def,
                        "</b><br> </p>"
)


# correct labels to be displayed with line returns
nodes_3L$label = gsub("\\\\n", "\n", nodes_3L$label)


#Edges
edges_3L <- as.data.frame(as_edgelist(network_3layers, names = TRUE))
colnames(edges_3L) <- c("from", "to")
edges_3L$from <- nodes_3L$id[match(edges_3L$from, nodes_3L$name)]
edges_3L$to <- nodes_3L$id[match(edges_3L$to, nodes_3L$name)]


# # plot
# visNetwork::visNetwork(
#   nodes_3L,
#   edges_3L,
#   height = "600px",
#   width = "100%",
#   main = list(
#     text = "Research questions and hypotheses in Invasion Ecology",
#     style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;")) %>%
#   visPhysics(enabled = TRUE,
#              solver = "forceAtlas2Based" , 
#              forceAtlas2Based = list(gravitationalConstant = -80,
#                                      avoidOverlap = 0.1,
#                                      springLength = 1,
#                                      springConstant = 0.1))
# 
# # NOT quite working like I want: physics are annoying


plot_3L_network <- function(n = nodes_3L, e = edges_3L) {
 p <- visNetwork::visNetwork(
   n,
   e,
   main = list(
     text = "Conceptual scheme for Invasion Ecology",
     style = "font-family:Roboto slab;color:#0085AF;font-size:18px;text-align:center;"),
   margin = -0.2) %>%
   visEdges(
     shadow = FALSE,
     color = list(color = "#0085AF", highlight = "#C62F4B")
   ) %>%
   visOptions(highlightNearest = list(enabled = T, degree = 1, hover = FALSE),
              autoResize = FALSE,
              manipulation = FALSE) %>%
   visInteraction(navigationButtons = TRUE) %>%
   visHierarchicalLayout(levelSeparation = 1400,
                         nodeSpacing = 1,
                         treeSpacing = 20,
                         edgeMinimization =TRUE,
                         shakeTowards = "roots",
                         parentCentralization = TRUE,
                         blockShifting = TRUE,
                         direction = "LR") %>%
   visPhysics(enabled = TRUE,
              stabilization = TRUE,
              solver = "hierarchicalRepulsion",
              hierarchicalRepulsion = list(
                nodeDistance = 100,
                avoidOverlap = 0.3)) %>%
   visEvents(type = "once", startStabilizing = "function() {
            this.moveTo({scale:1.2})}") 

   # visHierarchicalLayout(levelSeparation = 1400,
   #                       nodeSpacing = 100,
   #                       treeSpacing = 50,
   #                       edgeMinimization = FALSE,
   #                       shakeTowards = "leaves",
   #                       parentCentralization = TRUE,
   #                       blockShifting = TRUE,
   #                       direction = "LR") %>% 
   # visPhysics(enabled = FALSE) 

return(p)
}
(p <- plot_3L_network ()) 


write.csv(nodes_3L, "nodes_3L.csv")
write.csv(edges_3L, "edges_3L.csv")

library(jsonify)
library(jsonlite)



data <- list(nodes = nodes_3L,
            links = edges_3L %>% rename(source = from, target = to)
            )

toJSON(edges_3L)
jsonData <- toJSON(data)
data <- fromJSON(jsonData)
write(jsonData, 'jsonData.json')
# # Horizontal plot with igraph ####
# 
# # Custom layout:
# MyLO = matrix(0, nrow=vcount(network_3layers), ncol=2)
# 
# ## Horizontal position is determined by layer
# MyLO[,1] = V(network_3layers)$layer
# 
# ## Vertical position is defined by pre-existing order in the table
# for(i in 1:3) {
#   L  = which(V(network_3layers)$layer ==i)
#   OL = 1: length(V(network_3layers)$name[L])
#   MyLO[ L,2] = (OL-max(OL))/(min(OL)-max(OL))
# }
# 
# # Horizontal
# # MyLO[,1] = c(3,2,1) [MyLO[,1]] # for changing order of layers
# par(mar = c(0,0,0,4), adj = 0)
# plot.igraph(network_3layers ,
#             layout=MyLO,
#             margin = -0.2,
#             vertex.color=c("lightgrey","coral","black")[V(network_3layers)$layer],
#             vertex.shape=c("none","rectangle","none")[V(network_3layers)$layer],
#             vertex.size=c(1,70,1)[V(network_3layers)$layer],
#             vertex.label = gsub("\\\\n", "\n", V(network_3layers)$label),
#             vertex.label.dist = c(-5,-4.5,0)[V(network_3layers)$layer],
#             vertex.label.degree = c(0,0,0)[V(network_3layers)$layer],
#             vertex.label.cex = c(0.8,0.5,0.5)[V(network_3layers)$layer],
#             vertex.label.color = "black",
#             # vertex.label.family = "Roboto slab",
#             vertex.label.family = "Helvetica",
#             vertex.label.font = c(2,1,1)[V(network_3layers)$layer],
#             edge.arrow.size = 0
# )
# 
# 
# # With Network 3D ####

