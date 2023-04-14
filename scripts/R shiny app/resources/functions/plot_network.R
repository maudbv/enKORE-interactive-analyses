# function to plot ender's network 

plot_uni_network <-  function(n = nodes, e = edges,g = "group", w = "100%") {
    p <-  visNetwork(nodes , edges ) %>%
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
        label = "name", 
        font = list(size = 40)
      ) %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = list(variable = "name", main = "Select hypothesis"),
                 autoResize = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visLayout(randomSeed = 11)

  return(p)
}


