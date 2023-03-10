# function to plot ender's network 

plot_network <-  function(n,e, bipartite = FALSE, g = "type", w = "100%") {

   p <-  visNetwork(nodes = n, edges = e, width = w) %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    group = g,
    color = list(
      background = c("grey", "orange")[n$group+1],
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
   
   if (bipartite == FALSE) {
  p <-  p %>% visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = list(variable = "group", main = "Select grouping"),
             autoResize = TRUE) %>%
  visPhysics(stabilization = FALSE) %>%
  visLayout(randomSeed = 11)
   }
   
   if (bipartite == TRUE) {
   p <-   p %>% visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                      selectedBy = list(variable = "label", main = "Select node"),
                      autoResize = TRUE) %>%
       visPhysics(stabilization = FALSE) %>%
       visLayout(randomSeed = 11)
   }
   
  return(p)
}
