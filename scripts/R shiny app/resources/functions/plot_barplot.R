# radial plot splitting the results by groups

plot_barplot <-
  function(df,
           group_col = "Habitat_list",
           grouping = habitat_groups,
           legend.show = TRUE,
           title_text  = NULL) {
    library(plotly)
    
    names(df)[names(df) == group_col] <-  "grouping_col"
    
  # counting which groups are represented in each studies, one study can represent multiple groups
    group_counts <- tibble(support = df$support_for_hypothesis,
                           as.data.frame(sapply(grouping, function(y)
                             .detect_items(df$grouping_col, y))))
    
    # make sure all possible support is counted
    levels(group_counts$support) <- c("Supported","Undecided","Questioned")
    
    # count the number of studies in each group and for each type of support
    data_grouped <- group_counts %>%
      group_by(support,.drop = FALSE) %>%
      summarise(across(.cols = all_of(grouping), ~ sum(.x, na.rm = TRUE)))
    
 
    # reformat table for the figure
    library(tidyr)
    data = data_grouped %>%
      pivot_longer(cols = c(-support),
                   names_to = "group") %>%
      pivot_wider(names_from = c(support))
    
    # # calculate percentages of support per group
    # mutate(data, total = Supported+Undecided+Questioned) %>%
    # mutate(perc.Supported = Supported/total,
    #        perc.Undecided = Undecided/total,
    #        perc.Questioned = Questioned/total)
    
# plot figure
    categories = c("Supported","Undecided","Questioned")
    titles  = c("Supporting","Undecided","Questioning")
    
    fig <- plot_ly(data, type = 'bar',orientation = 'h')
    
   for (i in 1:3) {
     if (categories[i] %in% names(data)) {
     fig <-   add_trace(fig, 
        x = as.formula(paste("~",categories[i])),
        y = ~ group,
        type = 'bar', 
        orientation = 'h',
        name = titles[i],
        marker = list(color = hi_colors$cols[i])
      )
     }
     }
  
      
      fig <-  layout(fig,
          barmode = 'stack',
          title = list(text = title_text,
                       font = list(size = 20),
                       pad = list(b = 0, l = 1, r = 1, t= 1),
                       x = 0.01,
                       y = 0.95),
          legend =  list(text = 'Evidence for the hypothesis'),
          showlegend = legend.show,
          margin = 0.01,
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    return(fig)
  }
