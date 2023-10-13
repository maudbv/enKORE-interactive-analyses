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

# Import hypohteses table from Roxane Vial  #####
# source("resources/Hypothesis index.R")

# import table of simplified attributes
hyp_att <- as.data.frame(
  readr::read_csv(
    "resources/additional data/hyp attribute selection.csv",
    trim_ws = TRUE,
    skip_empty_rows = TRUE
    )
  )

# create replacement vector for column names 
lookup <- as.vector(hyp_att$attribute)
names(lookup) <- hyp_att$abb

# change column names
hyp_mat  <- hyp_mat %>%
  rename( all_of(lookup))

# transform matrix by grouping columns



#Extract the columns corresponding to an incidence matrix ####
hyp_mat_inc <- hyp_mat %>%
  select(`Behavior`:`invasion`)

# convert data form Enders PhD (coded with A and B) as 0 and 1: 
hyp_mat_inc <- hyp_mat_inc %>% 
  mutate(across(colnames(hyp_mat_inc),
                ~ stringr::str_replace_all(.x, pattern ="1B", "1"))) %>% 
  mutate(across(colnames(hyp_mat_inc),
           ~ stringr::str_replace_all(.x, pattern ="1B", "1"))) %>% 
  mutate(across(colnames(hyp_mat_inc),
                ~ stringr::str_replace_all(.x, pattern ="B", "0"))) %>% 
  mutate(across(colnames(hyp_mat_inc),
                ~ stringr::str_replace_all(.x, pattern ="A", "1"))) %>% 
  mutate(across(colnames(hyp_mat_inc),
                ~ stringr::str_replace_all(.x, pattern ="C", "0")))%>% 
  mutate(across(colnames(hyp_mat_inc), as.numeric))

# simplify attributes ####
temp <- as.data.frame(t(hyp_mat_inc),stringsAsFactors = FALSE)
temp$type <- hyp_att$type 
temp$group <- hyp_att$group

temp <- as.data.frame(temp  %>% 
    filter(type == "focal entity") %>%
      group_by(group) %>%
  summarise(across("AA":"XE", max)))

rownames(temp) <- temp$group
temp <- temp[,-1]

hyp_mat_redux <- as.data.frame(t(temp), stringsAsFactors = FALSE)


# build the igraph object ####
net_hyp_trait <- graph_from_incidence_matrix(
  hyp_mat_redux,
  directed = FALSE,mode = "out")

plot(net_hyp_trait ,
     layout= layout_with_fr)

 
# with vizNetwork #####
# convert to networkD3 DOES nOT work here! Introduces false links...
# net <- igraph_to_networkD3(network, group = vertex_attr(network)$type)

V(net_hyp_trait)$label = V(net_hyp_trait)$name

#nodes_hyp_trait
nodes_hyp_trait <- data.frame(
  id = 1 : length(vertex_attr(net_hyp_trait)$name),
  as.data.frame(vertex_attr(net_hyp_trait))
  )

# type of node
nodes_hyp_trait$layer =  c(1,2)[nodes_hyp_trait$type + 1]
nodes_hyp_trait$style =  c("hypothesis","attribute")[nodes_hyp_trait$type + 1]

# add info about nodes
nodes_hyp_trait$def <- hyp_mat[match(nodes_hyp_trait$label,
                                     hyp_mat$Acronym),
                               "Definition"]
nodes_hyp_trait$def[ nodes_hyp_trait$type] <- nodes_hyp_trait$name[ nodes_hyp_trait$type]


nodes_hyp_trait$name <- hyp_mat[match(nodes_hyp_trait$label,
                                     hyp_mat$Acronym),
                               "Hypothesis"]
nodes_hyp_trait$name [ nodes_hyp_trait$type] <-    nodes_hyp_trait$def[ nodes_hyp_trait$type]

#nodes_hyp_trait$ref <-c(hyp_mat$`Key ref`, rep(NA,ncol(hyp_mat_inc))) 


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
                 "</b><br>" ,
                 nodes_hyp_trait$def,
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
     text = "141 hypotheses from invasion and urban ecology grouped by their focal entity",
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
              forceAtlas2Based = list(gravitationalConstant = 10)) %>%
   visInteraction(navigationButtons = TRUE) %>%
   visIgraphLayout()
   
 return(p)

 }

p <- plot_hyp_trait_network ()
p


# # clustering with Walktrap #### ASK SOPHIE
# 
# wt_cluster <- cluster_walktrap(
#   net_hyp_trait,
#   weights = NULL,
#   steps = 15,
#   merges = TRUE,
#   modularity = TRUE,
#   membership = TRUE
# )
# modularity(wt_cluster)
# sizes(wt_cluster)
# is_hierarchical(wt_cluster)
# 
# ## S3 method for class 'communities'
# plot(as.dendrogram(wt_cluster, hang = -1, use.modularity = TRUE))
# 
# ## S3 method for class 'communities'
# plot(as.hclust(wt_cluster, hang = -1, use.modularity = TRUE))
# plot(as_phylo(wt_cluster), cex = 0.5)
# 
# ## S3 method for class 'communities'
# library(ggraph)
# layout = layout_nicely(net_hyp_trait)
# layout = layout.mds(net_hyp_trait)
# par(mar = c(0,0,0,0))
# plot(
#   wt_cluster,
#   net_hyp_trait,
#   col = membership(wt_cluster),
#   mark.groups = communities(wt_cluster),
#   edge.color = c("black", "red")[crossing(wt_cluster, net_hyp_trait) + 1],
#   vertex.label.cex =  c(0.4,0.8)[nodes_hyp_trait$layer],
#   vertex.label.color = c("blue","black")[nodes_hyp_trait$layer],
#   vertex.shape =  c("circle","rectangle")[nodes_hyp_trait$layer],
#   vertex.size = c(10,40)[nodes_hyp_trait$layer],
#   layout = layout
# )
# 
# # Similarity plotting



