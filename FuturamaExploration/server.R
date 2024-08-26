#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ############################
  ### Match Stat Filter
  ############################
  ###############
  #### filters
  ##################
  Season.data <- reactive({
    subset(transcipt_data_full, season %in% input$Season)
  })
  
  output$eps_type <- renderUI({
    eps_type <- as.vector( unique(Season.data()$title))
    pickerInput(inputId = "eps_type", 
                choices = unique(eps_type),
                selected = eps_type[1]
                
                , label = "eps_type" , options = list(`actions-box` = TRUE),multiple = T
    )
    
  })
  
  Eps.data <- reactive({
    subset(Season.data(), title %in% input$eps_type)
  })
  
  output$character <- renderUI({
    character <- as.vector( unique(Eps.data()$character))
    pickerInput(inputId = "character", 
                choices = unique(character),
                selected = character[2]
                , label = "character" , options = list(`actions-box` = TRUE),multiple = T
    )   
  })
  
  character.data <- reactive({
    subset(Eps.data(), character %in% input$character)
  })

 
  ####################
  ### Plots
  #####################
  
  
  characterTable <- reactive({
    df <- data.frame(table(character.data()$character))
    
    df
  })
  
  
  output$character_Barchart <- renderPlot({ 
    ggplot(characterTable(), aes(x = reorder(Var1,Freq), y = Freq)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip()+
      theme_minimal() +
      labs(title = "Bar Plot of Amount by Name",
           x = "Name",
           y = "Amount")+
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  ############################

})
