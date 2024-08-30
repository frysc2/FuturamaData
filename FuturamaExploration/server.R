#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)
library(tables)
library(maditr)
library(stringr)
library(ggplot2)
library(readr)
library(tidyverse)
library(htmlwidgets)
library(jsonlite)
library(RColorBrewer) 
library(DT)
SeasonDataTable_list<-read.csv("SeasonDataTable_list.csv")
transcipt_data_full<-read.csv("transcipt_data_full.csv")

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
                
                , label = "Episode(s)" , options = list(`actions-box` = TRUE,   
                                                        `live-search` = TRUE),multiple = T
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
                , label = "Character(s)" , options = list(`actions-box` = TRUE,   
                                                          `live-search` = TRUE),multiple = T
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
    
    
    colnames(df) <- c("Name", "Amount")
    
    top_n <- input$top_n
    df_top10 <- df %>%
      arrange(desc(Amount)) %>%
      head(top_n)
    
    df_top10
  })
  color_mapping <- data.frame(
    Name = c("Fry", "Leela", "Bender", "Farnsworth", "Zoidberg", "Amy", "Kif", "Hermes", "Zapp", "Mom", "Robot Devil", "Lrrr", "Calculon", "Cubert"),
    color = c("orange", "purple", "#c0c0c0", "#b1dedc","#E37775","#F1ACC9", "#B3D08E", "#4B9033", "#F5F073", "#129189", "red", "brown", "gold", "#18E4FA" )
  )
  color_mapping_default <- data.frame(
    Name = "Default",
    color = "black"
  )
  color_mapping <- rbind(color_mapping, color_mapping_default)
  
  characterTable_color <- reactive({
    data_with_colors <- characterTable() %>%
      left_join(color_mapping, by = "Name") %>%
      mutate(color = ifelse(is.na(color), "black", color))
    data_with_colors
  })
  output$characterTable <- renderTable({
    characterTable_color()
  })
  
  output$character_Barchart <- renderPlot({ 
    ggplot(characterTable_color(), aes(x = reorder(Name, Amount), y = Amount, fill = color)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Amount), 
                hjust = -0.1,        
                size = 5,            
                color = "black") +
      coord_flip() +
      scale_fill_identity() +
      theme_minimal() +
      labs(title = paste0("Top Characters by Lines"),
           y = "Lines") +
      xlab(NULL)+
      theme(axis.text.y = element_text(angle = 0, hjust = 1))+  
      theme(
        axis.title.x = element_text(size = 14, color = "black"), 
        axis.title.y = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(size = 14, color = "black"), 
        axis.text.y = element_text(size = 14, color = "black")
      )
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("myplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      g <- ggplot(characterTable_color(), aes(x = reorder(Name, Amount), y = Amount, fill = color)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = Amount), 
                  hjust = -0.1,        
                  size = 5,            
                  color = "black") +
        coord_flip() +
        scale_fill_identity() +
        theme_minimal() +
        labs(title = paste0("Top Characters by Lines"),
             y = "Lines") +
        xlab(NULL)+
        theme(axis.text.y = element_text(angle = 0, hjust = 1))+  
        theme(
          axis.title.x = element_text(size = 14, color = "black"), 
          axis.title.y = element_text(size = 14, color = "black"), 
          axis.text.x = element_text(size = 14, color = "black"), 
          axis.text.y = element_text(size = 14, color = "black")
        )
      ggsave(file, plot = g, device = "png", width = 10, height = 5, units = "in")
    })
  ############################

})
