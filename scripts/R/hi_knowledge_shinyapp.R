library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(sysfonts)

# set app working directory
setwd(
  "/Users/maud/Documents/Work/Research work/enKORE/enKore project/Interactive analyses/ORKG interactive analyses/scripts/R"
)

# import and process data
source('data processing.R')

# Get style themes #
source("resources/ggplot_HiK_theme.R")

# Get style themes #
source("plot_chrono.R")


ui <- pageWithSidebar(
  headerPanel('Invasion Biology- Overview on various hypotheses'),
  sidebarPanel(selectInput(
    'hyp', 'Select a hypothesis', unique(total_df$hypothesis)
  ),),
  mainPanel(
    #textOutput("selected"),
    #imageOutput(outputId="images"),
    plotlyOutput('chronology')
  
  )
)


# server:   ####

server <- function(input, output, session) {
  
  # Select data
  filtered_df <- reactive({
    total_df %>%
      dplyr::filter(hypothesis == hyp)
  })
  
  
  # Plot chronology figure
  output$chronology <- renderPlotly(expr = {
    df <-  filtered_df()
    plot_chrono(df, input$hyp)
  })
  
}

shinyApp(ui = ui, server = server)
