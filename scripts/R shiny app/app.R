# Shiny app dashboard to explore hypotheses in invasion biology
# author: Maud Bernard-Verdier
# source: orkg.org

#### TODO
# improve page layout : with sub plots for sub hyps
# sort problem of repeated study names for enemy release for instance

# Load packages ####
library(shiny)
library(ggplot2)
library(plotly)
library(sysfonts)
library(dplyr)
require(shinyWidgets)
require(shinyjs)
require(shinythemes)
library(DT)
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(tidyverse)
library(visNetwork)

# import and pre-process data ####
source("resources/Hypothesis index.R")
source('resources/data processing.R')
source("resources/hyp_network_viz/martin_network_viz.R")
source("resources/hyp_network_viz/themes_network_viz.R")
source("resources/hyp_network_viz/tripartite topdown network.R")
# source("resources/functions/plot_network.R")

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
             windowTitle = "Exploring hypotheses in Invasion Biology",
             
             # First page: Interactive exploration of evidence supporting individual hyps
             tabPanel("Evidence for each hypothesis",
                      sidebarLayout(
                        sidebarPanel(
                          
                          "Explore the evidence available for major hypotheses in Invasion Biology.
                          You can filter studies by taxa, habitats or research method",
                          tags$br(),
                          tags$br(),
                          selectInput('hyp', 'Select a hypothesis',
                                      c(unique(total_df$hypothesis))), #TODO fix labels
                          
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
                           DT::DTOutput("filtered_data") #TODO update type of table output for more interaction +add years
                           )
                          )
                        )
                      )
             ),
             # second page: network visualization
            tabPanel("Hypothesis Networks",
                     tabsetPanel(
                         
                      # # Panel 1: Organisation by Research hypotheses
                      # tabPanel("Research questions",
                      #          fluidRow( 
                      #            column(visNetworkOutput("rhrq_network"),
                      #                   width = 12,
                      #                   height = 10)),
                      #          tags$br(),
                      #          "Bipartite network illustrating the distribution of hypotheses among eight major research questions in invasion biology. The network is based on ongoing expert assessment and classification of hypotheses within the enKORE project"),
                      # 
                      # Panel 2: Tripartite network
                      tabPanel("Conceptual scheme",
                               visNetworkOutput("tripartite_network",width = "auto",height = "700px"),
                               tags$br(),
                               HTML('<a style="text-decoration:none;cursor:default;color:#808080" href="#">Hierarchical scheme illustrating the distribution of hypotheses among nine major research questions and four themes in invasion biology. The network is based on ongoing expert assessment and classification of hypotheses within the enKORE project</a>'),
                               tags$br()
                      ),
                      
                      # Panel 3: Martin Ender's network
                      tabPanel("Similarity network",
                                 visNetworkOutput("martin_network",width = "auto",height = "700px"),
                               tags$br(),
                               "Network of similarity between 39 hypotheses according to Enders et al. 2020, Global Ecology and Biogeography"),
                      tags$br()
                      )
                     ),

             # Third page: about the project
             tabPanel("About the project",
                      "This is a beta version of interactive visualiaztions for the enKORE project, funded by the Volkswagen Stiftung, Germany.",
                      tags$br(),
                      tags$br(),
                      'This interactive website was built by Maud Bernard-Verdier using R shiny, with data from the 2018 book "Invasion biology: hypotheses and evidence", by Jeschke & Heger (eds), and currently curated by the', tags$a(href="https://orkg.org", " Open Research Knowledge Graph project"),'. It also integrates data from the 2020 article by Enders et al. which can be found here:', tags$a(href="https://doi.org/10.1111/geb.13082", "https://doi.org/10.1111/geb.13082"),'.'
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
                  choices =  c("All", unique(unlist(hyp_taxa()$Habitat_list))))
    })
    
    hyp_taxa_hab <- reactive({
      req(hyp_taxa())
      hyp_taxa() %>% 
        { if (! "All" %in% input$hab) {
          dplyr::filter(., .detect_items(Habitat_list, input$hab))} else {.}
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
    paste(input$hyp)
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
                 legend.show = TRUE,
                 title_text = "Habitat"
                 )
  })
  output$support_methods <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "Research_Method",
                 grouping = method_groups,
                 legend.show = FALSE,
                 title_text = "Research method"
    )
  })
  
  output$support_taxa <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "taxa_group",
                 grouping =  taxa_groups,
                 legend.show = FALSE,
                 title_text = "Taxonomic group"
    )
  })
  
  output$support_continents <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "continents",
                 grouping = continents_vec,
                 legend.show = FALSE,
                 title_text = "Continent"
    )
  })
  
  # Data table
  output$filtered_data = DT::renderDT({
    req(filtered_df())
    display_columns <- c("support_for_hypothesis","Investigated_species","Habitat","Research_Method", "continents","Study_date") 
    df <-  as.data.frame(filtered_df())
    rownames(df) <- df$index
    df <-  df[, display_columns]
 datatable(df,
           extensions = 'Buttons',
           options = list(
             dom = 'Bfrtip',
             exportOptions = list(header = ""),
             buttons = list(
               list(
                 extend = "csv", 
                 filename = 'export',
                 text = "Download", 
                 title = NULL
                 )
               )
           )
 )
 
  },            server = FALSE)
  
  # Martin's network
  output$martin_network <- renderVisNetwork({
    plot_martin_network(nodes_martin, edges_martin)
  })
  
  # # RHRQ network of hypotheses
  # output$rhrq_network <- renderVisNetwork({
  #   plot_rhrq_network(nodes_rhrq, edges_rhrq)
  # })
  
  # tripartite network of hypotheses
  output$tripartite_network<- renderVisNetwork({
    plot_3L_network(nodes_3L, edges_3L)
  })
}



shinyApp(ui = ui, server = server)
