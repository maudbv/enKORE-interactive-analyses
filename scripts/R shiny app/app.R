# Shiny app dashboard to explore hypotheses in invasion science
# author: Maud Bernard-Verdier
# source: orkg.org


# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(sysfonts)
library(dplyr)

# import and pre-process data
source('data processing.R')

# Get colours and style themes #
source("resources/ggplot_HiK_theme.R")

# Load custom functions #
source("resources/functions/app_helper.R")

# Load plotting functions
source("resources/functions/plot_chrono.R")

#Set up User Interface
ui <- fluidPage(
  
  titlePanel('Exploring hypotheses in Invasion Biology'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('hyp', 'Select a hypothesis',
                  c(unique(total_df$hypothesis))),
      checkboxGroupInput('taxa', 'Select a taxonomic group',
                     choices = c("All",taxa_groups), selected = "All"),
      uiOutput("habitat_selector"),
      uiOutput("method_selector")
    ),
    
    #### TODO
    # FIX the taxa name typos
    # transform to dashboard
    # switch to plotly interactive
    # add donut plot for summary stats
    # add radar plot for subhyps
    
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
  
 # Conditional filter selection
    hyp_taxa <- reactive({
      req(input$taxa)
      filter(total_df, hypothesis == input$hyp) %>% 
        { if (! "All" %in% input$taxa) {
          dplyr::filter(., .detect_items(taxa, input$taxa))} else {.} 
        }
    })
    
    output$habitat_selector <- renderUI({
      selectInput(inputId = "hab",
                  label = "Select a habitat",
                  choices =  c("All", unique(unlist(hyp_taxa()$Habitat))))
    })
    
    hyp_taxa_hab <- reactive({
      req(hyp_taxa())
      hyp_taxa() %>% 
        { if (! "All" %in% input$hab) {
          dplyr::filter(., .detect_items(Habitat, input$hab))} else {.}
          }
    })
    
    output$method_selector <- renderUI({
      req(hyp_taxa_hab())
      selectInput(inputId = "method",
                  label = "Select a research method",
                  choices =  c("All", unique(unlist( hyp_taxa_hab()$Research_Method))))
    })

  filtered_df <- reactive({
    req(input$method)
   hyp_taxa_hab() %>% 
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
    req(filtered_df())
    plot_chrono(filtered_df())
  })
  
}

shinyApp(ui = ui, server = server)
