# Shiny app dashboard to explore hypotheses in invasion science
# author: Maud Bernard-Verdier
# source: orkg.org

#### TODO
#improve page layout : with sub plots for sub hyps
# sort problem of repeated study names for enemy release for instance

# Load packages ####
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(sysfonts)
library(dplyr)
require(shinyWidgets)
require(shinyjs)
require(shinythemes)
library(DT)

# import and pre-process data ####
source('resources/data processing.R')
source("resources/hyp_network_viz/martin_network_viz.R")
source("resources/functions/plot_network.R")

# Get colours and style themes ####
source("resources/ggplot_HiK_theme.R")

# Load custom functions ####
source("resources/functions/app_helper.R")

# Load plotting functions ####
source("resources/functions/plot_chrono.R")
source("resources/functions/plot_piechart.R")
source("resources/functions/plot_radialplots.R")
source("resources/functions/plot_barplot.R")

# User Interface ####
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"),
             collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Hypotheses in Invasion Biology</a>'),
             id="nav",
             windowTitle = "Exploring hypotheses in Invasion Science",
             
             # First page: Interactive exploration of evidence supporting individual hyps
             tabPanel("Evidence for each hypothesis",
                      sidebarLayout(
                        sidebarPanel(
                          
                          "Explore the evidence available for major hypotheses in Invasion Science.
                          You can filter studies by taxa, habitats or research method",
                          tags$br(),
                          tags$br(),
                          selectInput('hyp', 'Select a hypothesis',
                                      c(unique(total_df$hypothesis))),
                          
                          pickerInput(inputId = 'taxa',
                                      'Select a taxonomic group',
                                      choices = taxa_groups,
                                      multiple = TRUE,
                                      selected = taxa_groups,
                                      options = list(`actions-box` = TRUE,
                                                     `none-selected-text` = "Please make a selection!")
                          ),
                          
                          uiOutput("habitat_selector"),
                          
                          uiOutput("method_selector"),
                          
                          
                          HTML('<a style="text-decoration:none;cursor:default;color:#27596B;" class="active" href="#">This is a project of the Hi Knowledge initiative</a>'),
                          tags$br(),
                          tags$a(href="https://hi-knowledge.org/", "hi-knowledge.org")
                        )
                        ,
                        
                       
                        mainPanel(
                          tabsetPanel(
                          
                            # Panel 1: support for the hypothesis
                           tabPanel("Support for the hypothesis",
                                    tags$br(),
                                    h3(textOutput("hyp_description")),
                                    fluidRow(
                                      column(plotlyOutput('support_piechart', height = "200px"),
                                             width = 5),
                                      column( p(textOutput("support_summary"), 
                                                style="text-align:left;color:#27596B;padding:15px;border-radius:10px"),
                                              width = 5)
                                    ),
                                    fluidRow(plotOutput('chronology'),
                                             width = "95%",
                                             height = "100%"),
                                    tags$br()
                           ),
                           
                           # Panel 2: Filtered data table
                           tabPanel("Distribution",  
                                     
                                    fluidRow( 
                                      column(plotlyOutput("support_habitats"),
                                             width = 10,
                                             height = 5)),
                                    fluidRow(
                                      column( plotlyOutput("support_methods"),  
                                              height = 5,
                                              width = 10)),
                                    fluidRow(
                                      column(plotlyOutput("support_taxa"),
                                             width = 10,
                                             height = 5)),
                                    fluidRow(
                                      column(plotlyOutput("support_continents"),
                                             width = 10,
                                             height = 5)),
                                    tags$br()
                           ),
                           
                           # Panel 2: Filtered data table
                           tabPanel("Data",  
                           DT::dataTableOutput("filtered_data")
                           )
                          )
                        )
                      )
             ),
             # second page: network visualization
             tabPanel("Hypothesis network",
                      fluidRow( 
                        column(visNetworkOutput("network"),
                               width = 12,
                               height = 10)),
                      tags$br(),
                      "Network of similarity between 39 hypotheses according to Enders et al. 2020, Global Ecology and Biogeography"),

             # Third page: about the project
             tabPanel("About the project",
                      "This is a work in progress from the enKORE project, funded by the Volkswagen Stiftung, Germany.",
                      tags$br(),
                      tags$br(),
                      'This interactive website was built using R shiny, with data from the 2018 book "Invasion biology: hypotheses and evidence", by Jeschke & Heger (eds), and currently curated by the', tags$a(href="https://orkg.org", " Open Research Knowledge Graph project"),'.'
             )
  )
)


# Server:   ####

server <- function(input, output, session) {
  
 # Conditional filter selection
    hyp_taxa <- reactive({
      req(input$taxa)
      filter(total_df, hypothesis == input$hyp) %>% 
        { if (! "All" %in% input$taxa) {
          dplyr::filter(., .detect_items(taxa_group, input$taxa))} else {.} 
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

  support_perc <- reactive({
    req(filtered_df)
    df <-  filtered_df()
    counts <- df %>% count(support_for_hypothesis, sort = FALSE)
    return(round(counts$n[which(counts$support_for_hypothesis == "Supported")]/sum(counts$n)*100,2))
  })
  
  # Plot chronology figure
 
  # output$chronology <- renderPlotly( {
  #   req(filtered_df())
  #   plot_chrono(filtered_df())
  # })
  
  output$chronology <- renderPlot( {
    req(filtered_df())
    plot_chrono(filtered_df())
  })
  
  output$support_piechart <- renderPlotly( {
    req(filtered_df())
    plot_piechart(filtered_df())
  })
  
  # comments
  output$hyp_description <- renderText({
    paste(input$hyp,"hypothesis")
  })
  
  output$support_summary <- renderText({
    paste("The hypothesis is supported in ", support_perc(),
          "% of the ",
          length(unique(filtered_df()$study)),
          " studies included in the database."
          )
  })
  
  # Distribution
  output$support_habitats <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "Habitat_list",
                 grouping = habitat_groups,
                 legend.show = FALSE)
  })
  output$support_methods <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "Research_Method",
                 grouping = method_groups,
                 legend.show = FALSE)
  })
  
  output$support_taxa <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "taxa_group",
                 grouping =  taxa_groups,
                 legend.show = FALSE)
  })
  
  output$support_continents <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "continents",
                 grouping = continents_vec,
                 legend.show = FALSE)
  })
  
  # Data table
  output$filtered_data = DT::renderDataTable({
    req(filtered_df())
    display_columns <- c("support_for_hypothesis","Investigated_species","Habitat","Research_Method", "continents") 
    df <-  as.data.frame(filtered_df())
    rownames(df) <- df$study
    df <-  df[, display_columns]
  })
  
  # network
  output$network <- renderVisNetwork({
    plot_network(nodes, edges, width = "100%")
  })
}

shinyApp(ui = ui, server = server)
