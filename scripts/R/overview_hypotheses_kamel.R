library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
setwd("/Users/maud/Documents/Work/Research work/enKORE/enKore project/Interactive analyses/ORKG interactive analyses/scripts/R")

ui <- pageWithSidebar(
  headerPanel('Invasion Biology- Overview on various hypotheses'),
  sidebarPanel(
    selectInput('hyp', 'Select a hypothesis', c("Darwin's naturalisation","Enemy release")),
  ),
  mainPanel(
    #textOutput("selected"),
    #imageOutput(outputId="images"),
    plotlyOutput('continents'),
    plotlyOutput('number'), 
    plotlyOutput('number_top'), 
    plotlyOutput('year')
    
  )
)



darwin <- read.csv(file = "csv/comparison_R53407_Darwin's naturalisation.csv")
darwin$Title <- darwin$publication
names(darwin) <- stringr::str_replace_all(names(darwin), "\\.","_")
darwin <- darwin[!duplicated(darwin$Title),]

enemy <- read.csv(file = "csv/comparison_R58002_Enemy release.csv")
enemy$Title <- enemy$publication
names(enemy) <- stringr::str_replace_all(names(enemy), "\\.","_")
enemy <- enemy[!duplicated(enemy$Title),]



df_list <- list(darwin,enemy)
total <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  df_list
)

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData_continents <- reactive({
    
    total_filtered <- dplyr::filter(total,hypothesis==input$hyp)
    
    total_filtered_gr <- total_filtered %>%
      group_by(Continent) %>%
      summarise(count = n())
    
    
  })
  
  selectedData_number <- reactive({
    total_filtered <- dplyr::filter(total,hypothesis==input$hyp)
    
  })
  
  
  selectedData_number_top <- reactive({
    total_filtered <- dplyr::filter(total,hypothesis==input$hyp)
    total_filtered <- head(total_filtered[order(total_filtered$Number_of_species, decreasing= T),], n = 5)
    

    
  })
  
  
  
  
  selectedData_year <- reactive({
    total_filtered <- dplyr::filter(total,hypothesis==input$hyp)
    
    total_filtered_gr <- total_filtered %>%
      group_by(Study_date) %>%
      summarise(count = n())
  })
  
  
  #  output$selected <- renderPrint({
  #    if(input$hyp == "limiting similarity")
  #      "The invasion success of non-native species is high if they strongly differ from native species , and it is low if they are similar to native species."
  #    else
  #      "Gain weight"
  # })
  
  
  output$continents <- renderPlotly({
    
    ggplotly(ggplot(selectedData_continents(),aes(x=Continent,y=count,fill=Continent))+
               geom_bar(stat='identity')+
               # theme(axis.text.x=element_text(angle =20,face = "bold",size = 12),
               #       axis.text.y=element_text(face = "bold",size = 12),
               #       axis.ticks.y=element_blank(),
               #       plot.title = element_text(color="#800080", size=16, face="bold.italic"),
               #       axis.title.x = element_text(color="black", size=14, face="bold"),
               #       axis.title.y = element_text(color="black", size=14, face="bold"),
               #       legend.position = "none"
               #       )+
               theme_hiK() +
               labs(title="Distribution of articles over continents.",x="Continent",y="Number of articles")    )
    
  })
  
  output$number <- renderPlotly({
    
    ggplotly(ggplot(selectedData_number(),aes(x=Title,y=Number_of_species))+
               geom_bar(stat='identity')+
               theme(axis.ticks.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     plot.title = element_text(color="#800080", size=16, face="bold.italic"),
                     axis.title.x = element_text(color="black", size=14, face="bold"),
                     axis.title.y = element_text(color="black", size=14, face="bold")
                     )+
               labs(title="Number of taxa investigated in each article",x="Article",y="Number of taxa"))
    
  })
  
  
  output$number_top <- renderPlotly({
    
    # 
    ggplotly(ggplot(selectedData_number_top(),aes(x=reorder(Title,-Number_of_species),y=Number_of_species))+
               geom_bar(stat='identity',fill = "#bc1648")+
               theme(axis.ticks.y=element_blank(),
                     plot.title = element_text(color="#800080", size=10, face="bold.italic"),
                     axis.title.x = element_text(color="black", size=12, face="bold"),
                     axis.title.y = element_text(color="black", size=14, face="bold")
                     )+
               labs(title="Top-5 articles with highest numbers of investigated taxa",x="Article",y="Number of taxa")+
               theme(text = element_text(size=8),
                     axis.text.x = element_text(angle=10, hjust=1)) 
    )
    
    
  })
  
  
  output$year <- renderPlotly({
    
    ggplotly(ggplot(selectedData_year(),aes(x=Study_date,y=count,fill=Study_date))+
               geom_bar(stat='identity')+
               labs(title="Distribution of articles over years",x="Puplication year",y="Number of articles")+
               theme(axis.text.x = element_text(angle=45, hjust=1),
                     axis.ticks.y=element_blank(),
                     plot.title = element_text(color="#800080", size=16, face="bold.italic"),
                     axis.title.x = element_text(color="black", size=14, face="bold"),
                     axis.title.y = element_text(color="black", size=14, face="bold")
                     ) 
    )
    
  })
  #    output$images<- renderImage({
  #        if(input$hyp=="limiting similarity") Leg<-"www/limiting.png"
  #        if(input$hyp=="Darwin naturalisation") Leg<-"www/darwin.png"
  #        list(src=Leg)
  #    }, deleteFile = FALSE)
  
}

shinyApp(ui = ui, server = server)
