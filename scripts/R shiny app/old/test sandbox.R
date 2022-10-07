input <-  list(hyp = "Enemy release",
            taxa = "All",
            hab = "All",
            method = "experimental"
)


filtered_df <-  total_df %>%
  dplyr::filter(hypothesis == input$hyp) %>%
  { if (! "All" %in% input$taxa) {
    dplyr::filter(., .detect_items(taxa, input$taxa))} else {.} 
  } %>%
  { if (! "All" %in% input$hab) {
    dplyr::filter(., .detect_items(Habitat, input$hab))} else {.}
  } %>%
  { if (! "All" %in% input$method) {
    dplyr::filter(., .detect_items(Research_Method, input$method))} else {.}
  }

plot_chrono(df = filtered_df, hyp= input$hyp)
