# radial plot splitting the results by groups

plot_radial <-  function(df,
                         group_col = "Habitat_list",
                         grouping = habitat_groups) {
library(plotly)  

  names(df)[names(df) == group_col] <-  "grouping_col"

group_counts <- tibble(support = df$support_for_hypothesis,
                       as.data.frame(sapply(grouping, function(y) .detect_items(df$grouping_col, y)))
                    )

data_grouped <- group_counts %>%
  group_by(support) %>%
  summarise(across(.cols = all_of(grouping), ~ sum(.x, na.rm = TRUE)))
 
# plot

fig <- plot_ly(type="scatterpolar", mode = "lines+markers",
               fill = 'toself') %>%
  add_trace(r = as.numeric(data_grouped[1,grouping]),
            theta = grouping, name = "Supporting",
            color = hi_colors[1,2],
            hovertemplate = "</br> %{r} </br> %{theta}") %>%
  add_trace(r = as.numeric(data_grouped[2,grouping]),
            theta = grouping, name = "undecided",
            color = hi_colors[2,2],
            hovertemplate = "</br> %{r} </br> %{theta}") %>%
  add_trace(r = as.numeric(data_grouped[3,grouping]),
            color = hi_colors[3,2],
            theta = grouping, name = "Questioning",
            hovertemplate = "</br>%{r} </br> %{theta}") %>%
 layout(
   # legend=list(title=list(text='Evidence for the hypothesis')),
   showlegend = FALSE,
   polar = list(
           angularaxis = list( 
             rotation = 0, 
             direction = 'clockwise'
             ),
           radialaxis = list(
             visible = FALSE,
             type = "log"
           )
           ),
   margin = 0.00
   )
 

return(fig)
}


