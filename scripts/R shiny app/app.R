library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(sysfonts)

# set app working directory
# setwd("/Users/maud/Documents/Work/Research work/enKORE/enKore project/Interactive analyses/ORKG interactive analyses/scripts/R shiny app")

# import and process data
source('data processing.R')

# Get style themes #
source("resources/ggplot_HiK_theme.R")

# Load custom functions #
source("resources/functions/plot_chrono.R")
source("resources/functions/detect_items.R")

#Set up User Interface
ui <- fluidPage(
  
  titlePanel('Exploring hypotheses in Invasion Biology'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('hyp', 'Select a hypothesis',
                  c(unique(total_df$hypothesis))),
      checkboxGroupInput('taxa', 'Select a taxonomic group',
                    c("All",taxa_groups), selected = "All"),
      checkboxGroupInput('hab', 'Select a habitat',
                  c("All",habitat_groups), selected = "All"),
      selectInput('method', 'Select a research method',
                  c("All",method_groups), selected = "All")
    ),
    
    #### TODO : need to clean data to remove stupid duplicates and rename categories.
    # Also need to refine filtering so it is a grep (looking for a pattern in a list) rather than a string match
    # add "stickers" on the panels with filter options written.
    
  mainPanel(
    tabsetPanel(
      # tabPanel("Support for the hypothesis", plotlyOutput('chronology')) 
      tabPanel("Support for the hypothesis", plotOutput('chronology')) 
  )
  )
  )
)


# server:   ####

server <- function(input, output, session) {
  
  # Select data
  filtered_df <- reactive({
    total_df %>%
      filt = dplyr::filter(hypothesis == input$hyp) %>%
      { if (! "All" %in% input$taxa) {
        dplyr::filter(., .detect_items(taxa, input$taxa))} else {.} 
      } %>%
      { if (! "All" %in% input$hab) {
        dplyr::filter(., .detect_items(Habitat, input$hab))} else {.}
      } %>%
      { if (! "All" %in% input$method) {
        dplyr::filter(., .detect_items(Research_Method, input$method))} else {.}
      }
  })

  # Plot chronology figure
  # output$chronology <- renderPlotly(expr = {
  #   df <-  filtered_df()
  #   ggplotly(plot_chrono(df, input$hyp))
  # })
  
 
  output$chronology <- renderPlot( {
    df <-  filtered_df()
    plot_chrono(df, input$hyp)
  })
  
}

shinyApp(ui = ui, server = server)
