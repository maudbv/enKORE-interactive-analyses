# radial plot splitting the results by groups

plot_barplot <-  function(df, group_col = "Habitat_list", grouping = habitat_groups) {

  library(plotly)  

names(df)[names(df) == group_col] <-  "grouping_col"

group_counts <- tibble(support = df$support_for_hypothesis,
                       as.data.frame(sapply(grouping, function(y) .detect_items(df$grouping_col, y)))
                    )

data_grouped <- group_counts %>%
  group_by(support) %>%
  summarise(across(.cols = all_of(grouping), ~ sum(.x, na.rm = TRUE)))

library(tidyr)
data = data_grouped %>%
  pivot_longer(cols=c(-support),
               names_to="group")%>%
  pivot_wider(names_from=c(support))

# # ggplot style:
# data_grouped <-tidyr::pivot_longer(data_grouped, cols = !support,
#                     names_to = "groups",
#                     values_to = "number")
# g <- ggplot(data_grouped, aes(x = groups, y = number,  fill = support))+
#   geom_bar(stat="identity")
# fig <- ggplotly(g)


fig <- plot_ly(data, y = ~group, x = ~Supported,
               type = 'bar',orientation = 'h',
               name = 'Supporting', 
               marker = list(color = hi_colors$cols[1])) %>%
  add_trace(x = ~Undecided, name = 'Undecided', 
            marker = list(color = hi_colors$cols[2]))%>%
  add_trace(x = ~Questioned, name = 'Questioning', 
            marker = list(color = hi_colors$cols[3])) %>%
 layout(
   barmode = 'stack',
   # legend=list(title=list(text='Evidence for the hypothesis')),
   showlegend = FALSE,
   margin = 0.01,
   xaxis = list(title = ""),
   yaxis = list(title = "")
   ) 

return(fig)
}


