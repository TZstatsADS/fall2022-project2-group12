#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)

body <- dashboardBody(
  
   tabItems(
     #------------------ Introduction ----------------------------
     tabItem(tabName = "Introduction", fluidPage(
                 fluidRow(box(width = 15, title = "Introduction", status = "primary",
                              solidHeader = TRUE, h3("Title"),
                              h4("By Moustafa-Fahmy Nour, Donglai Xu, Yuli Yin, Xilin Huang, Yudan Zhang"),
                              h5("..."),
                              h5("..."))),
                 fluidRow(box(width = 15, title = "Targeted User", status = "primary", solidHeader=TRUE,
                              h5("We believe that the application would be useful for anyone who are interested in learning more about the effects of Covid 19"))),
                 fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                              solidHeader = TRUE,
                              h5("The application is divided into 4 separate tabs"),
                              tags$div(tags$ul(
                                tags$li("The", strong("first"), "tab: Introduction"),
                                tags$li("The", strong("second"), "tab: ..."),
                                tags$li("The", strong("third"), "tab: ..."),
                                tags$li("The", strong("fourth"),"tab: ...")
                              ))
                 ))
     )),
    
     #------------------ Energy Consumption ----------------------------
     tabItem(tabName = "Energy", fluidPage(
       # App title ----
       titlePanel("Energy Consumption Trend"),
       # Sidebar layout with input and output definitions ----
       sidebarLayout(
         # Sidebar panel for inputs ----
         sidebarPanel(
           # Input: Select for the borough ----
           selectInput(inputId = "Borough",
                       label = "Choose a borough:",
                       choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", 
                                   "Staten Island", "FHA", "Non Development Facility", "All")),
           
           # Input: Select for the enegry type ----
           selectInput(inputId = "Energy",
                       label = "Choose an energy type:",
                       choices = c("Water", "Electric", "Heating Gas", "All"))
         ),
         # Main panel for displaying output ----
         mainPanel(
           # Output: trend of consumption over the years ----
           plotOutput(outputId = "tsPlot")
         )
       )
     ))
   )
 )
 
 ui <- dashboardPage(
   title="Covid 19 and The effect on Energy in NYC",
   skin = "blue", 
   dashboardHeader(title=span("Covid 19 and NYC Business",style="font-size: 16px")),
   dashboardSidebar(sidebarMenu(
     menuItem("Home", tabName = "Introduction", icon = icon("introduction")),
     menuItem("Energy", tabName = "Energy", icon = icon("energy"))
   )),
   body 
 )
    