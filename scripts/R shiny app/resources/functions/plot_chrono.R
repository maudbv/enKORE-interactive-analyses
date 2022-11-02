plot_chrono <-  function(df) {
  
#explore visualization for support over time
library(ggplot2)
library(dplyr)
  try(if(is.null(df)) stop("No data"))
  
# Update values for chronological accumulation of studies: ####
df <- mutate(df, chrono_hyp = row_number(Study_date))
df <-   group_by(df, support_for_hypothesis) %>%
    mutate(chrono_support = row_number(Study_date))
df <-  ungroup(df)

latest <-  max(df$Study_date, na.rm = TRUE)
total_count <-  max(df$chrono_hyp, na.rm = TRUE)

# Temporal accumulation curves with bubbles for each study
p <- ggplot(df, aes(Study_date, chrono_support,
                    colour = support_for_hypothesis)) +
  scale_colour_manual(values = hi_colors$cols) +
  theme_hiK() + 
  theme(axis.title.x = element_blank(),
        panel.grid= element_line(colour = "grey80", linetype = "dotted")) +
  geom_line() +
  geom_line(aes(Study_date, chrono_hyp), col = "grey", lty = "dashed") +
  geom_point(aes(size = Number_of_species), alpha = 0.30) + 
  scale_size(name="Number of taxa", 
             range = c(1, 30), breaks = c(1, 10,100,1000),limits = c(1,1000)) +
    xlab("Time") + 
    ylab("Number of studies") #+
 # ggtitle(paste( unique(df$hypothesis), 'hypothesis'))

# adding labels
p <-  p + geom_text(aes(x = latest, y = total_count,
                   label = paste(total_count , 'studies')),
            col = "grey40", hjust = 1.2 , vjust = 1)
 
  #styling the plot
 p <-  p +  guides(colour = guide_legend( title ="Hypothesis is:",order = 1),
                    size = guide_legend(title = "Number of taxa"))
 
return(p)
}



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
