# radial plot splitting the results by groups

plot_radial <-  function(df, group_col = "Habitat_list", grouping = habitat_groups) {
library(plotly)  

  names(df)[names(df) == group_col] <-  "grouping_col"

group_counts <- tibble(support = df$support_for_hypothesis,
                       as.data.frame(sapply(grouping, function(y) .detect_items(df$grouping_col, y)))
                    )

data_grouped <- group_counts %>%
  group_by(support) %>%
  summarise(across(.cols = all_of(grouping), ~ sum(.x, na.rm = TRUE)))
 
            
fig <- plot_ly(type="scatterpolar", mode = "lines+markers",
               fill = 'toself') %>%
  add_trace(r = as.numeric(data_grouped[1,grouping]),
            theta = grouping,name = "supported" ) %>%
  add_trace(r = as.numeric(data_grouped[2,grouping]),
            theta = grouping, name = "undecided") %>%
  add_trace(r = as.numeric(data_grouped[3,grouping]),
            theta = grouping, name = "grouping") %>%
 layout( title = paste('Support accross habitats'),
         legend=list(title=list(text='Evidence for the hypothesis')),  
         plot_bgcolor='#e5ecf6',   
         xaxis = list(   
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),   
         xaxis = list(   
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff') ,
         polar = list(angularaxis = list( 
             rotation = 0, 
             direction = 'clockwise' 
           )), margin = 0.01) 
return(fig)
}


