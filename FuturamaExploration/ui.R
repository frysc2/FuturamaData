#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  
  ###### Application title
  ######
  titlePanel("Futurama"),
  
  div(
    fluidRow(
      column(3,
             
      ),
      column(3,
             
      ),
      column(3,
             
             
      ),
      column(3,
             
             
      )
    )
    , width = 5),
  
  ##### Sidebar with a slider input for number of bins
  #####
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.conditionedPanels==1",
                       helpText("Lines Per Episode"),
                       pickerInput(inputId = "Season", 
                                   choices = unique(transcipt_data_full$season),
                                   selected = unique(transcipt_data_full$season)[1]
                                   , label = "Season", options = list(`actions-box` = TRUE),multiple = T
                       ),
                       uiOutput("eps_type"),
                       uiOutput("character"),
                       numericInput("top_n", "Select Number of Top Entries:", value = 10, min = 1, max = 30),
                       
                       
      ),
      conditionalPanel(condition="input.conditionedPanels==4",
                       helpText("Content Panel 4"),
                       uiOutput("team_name"),
                       uiOutput("player_name"),
                       uiOutput("hero_name"),
                       uiOutput("stat_name") 
      ),
      conditionalPanel(condition="input.conditionedPanels==2",
                       helpText("Content Panel 2"),
                       
      ),
      conditionalPanel(condition="input.conditionedPanels==3",
                       helpText("Content Panel 3"),
                       
                       
                       
      ),
      
      
      
      # selectInput(inputId = "Team2", 
      #              choices = unique(players2021$team_name)
      #             , label = "Teams"
      #), 
      #  uiOutput("player_name2"),
      # uiOutput("hero_name2"),
      #uiOutput("stat_name2")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Lines Per Episode",
                 #                    fluidRow(
                 #                      br(),
                 #                      column(3,
                 #                             uiOutput("stat_for_hist")
                 #                      ),
                 #                      column(3,
                 #                             uiOutput("Hero_for_hist")
                 
                 #                      ),
                 #                    ),
                 
                 
                 plotOutput("character_Barchart"),
                 tableOutput("characterTable"),
                 downloadButton("downloadPlot", "Download Plot"),
                 
                 value = 1
        ),
        tabPanel("Map Table",
                 
                 tableOutput("table"),
                 tableOutput("table2"),
                 value = 4
                 
        ),
        tabPanel("Stats",
                 tableOutput("demo_text"),
                 
                 value = 2
        ),
        tabPanel("Player Stats",
                 textOutput("gamesPlayed"),
                 textOutput("playerStatDataFrame"),
                 textOutput("role"),
                 tableOutput("role1"),
                 tableOutput("playerStatsTable_general"),
                 tableOutput("TimeplayedData"),
                 plotOutput("PlayerHeroTable"),
                 value = 3
        ),
        
        id = "conditionedPanels"
      )
      
      
      
      
    )
  )
))