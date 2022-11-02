# radial plot splitting the results by groups
library(plotly)  
library(rWind)  
data(wind)  

fig <- plot_ly(wind, r = ~r, theta = ~t, type="barpolar", color = ~nms,  
               marker =list(colorscale = 'Accent')) %>%  
  layout(title = 'Part of a continuous color scale used as a discrete sequence',  legend=list(title=list(text='strength')),  
         plot_bgcolor='#e5ecf6',   
         xaxis = list(   
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),   
         xaxis = list(   
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff') , polar = list(angularaxis = list( 
             rotation = 90, 
             direction = 'clockwise' 
           )), margin = 0.01) 
fig





