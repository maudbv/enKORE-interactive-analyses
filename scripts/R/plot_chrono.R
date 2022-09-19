plot_chrono <-  function(df ,hyp  = "Darwin's naturalisation") {
  
#explore visualization for support over time
library(ggplot2)
library(dplyr)

# # Bubble accumulation plot
# ggplot(df, aes(x = support_for_hypothesis,
#                           y = chrono_support/50,
#                           group = Investigated_species)) +
#   geom_jitter(width = .2, height = 0,
#     col = hi_colors$cols[as.numeric(df$support_for_hypothesis)],
#              alpha = 0.60, 
#              size = log(df$Number_of_species+1)) + 
#   xlab("Support") + 
#  scale_x_discrete("support_for_hypothesis",) + 
#   ylab("Time") + 
#   theme_hiK() + 
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()
#   )

# Curve accumulation
p <- ggplot(df, aes(Study_date, chrono_support, colour = support_for_hypothesis)) +
  scale_colour_manual(values = hi_colors$cols) +
  theme_hiK() + 
  theme(axis.title.x = element_blank(),
        panel.grid= element_line(colour = "grey80", linetype = "dotted"))+
  geom_line() +
  geom_line(aes(Study_date, chrono_hyp), col = "grey", lty = "dashed") +
  geom_jitter(width = 0, height = 0.2,
   #col = hi_colors$cols[as.numeric("support_for_hypothesis")],
             alpha = 0.40, 
             size = log(df$Number_of_species + 1)) + 
    xlab("Time") + 
    ylab("Number of studies") 

# adding labels
p <-  p + geom_text(aes(x = max(Study_date), y = max(chrono_hyp),
                   label = paste(max(chrono_hyp), 'studies')),
            col = "grey40", hjust = 1.2 , vjust = 1)
 
  #styling the plot
 p <-  p +  guides(colour = guide_legend( title ="Hypothesis is:"),
                    size = guide_legend(title = "Number of taxa"))
 
return(p)
}