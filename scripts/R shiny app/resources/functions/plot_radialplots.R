# radial plot splitting the results by groups

plot_radial <-  function(df, grouping) {
library(plotly)  

  n = which(names(df) == grouping)

  data_grouped <-  df %>%
    group_by(Habitat, support_for_hypothesis)%>%
    summarise(n =n())
  
fig <- plot_ly(data_grouped,  theta = ~Habitat, 
               type="barpolar", color = ~support_for_hypothesis,  
               marker =list(colorscale = 'Accent')) %>%  
  layout(title = 'Part of a continuous color scale used as a discrete sequence',
         legend=list(title=list(text='strength')),  
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
fig


