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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  SeasonDataTable_list<=read.csv("../SeasonDataTable_list.csv")
  transcipt_data_full<=read.csv("../transcipt_data_full.csv")
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
    # Create a data frame with character counts
    df <- data.frame(table(character.data()$character))
    
    # Rename columns for clarity
    colnames(df) <- c("Name", "Amount")
    
    top_n <- input$top_n
    # Arrange the data in descending order of frequency and select the top 10
    df_top10 <- df %>%
      arrange(desc(Amount)) %>%
      head(top_n)
    
    df_top10
  })
  
  output$character_Barchart <- renderPlot({ 
    ggplot(characterTable(), aes(x = reorder(Name, Amount), y = Amount)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste0("Top Characters by Lines"),
           y = "Lines") +
      xlab(NULL)+
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  
  ############################

})
