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
     #------------------ Yuli: Introduction ----------------------------
     tabItem(tabName = "Introduction", fluidPage(
                 fluidRow(box(width = 15, title = "Introduction", status = "primary",
                              solidHeader = TRUE, h3("Covid-19 and Its Impact on Energy Consumption in NYC"),
                              h4("By Moustafa-Fahmy Nour, Donglai Xu, Yuli Yin, Xilin Huang, Yudan Zhang"),
                              h5("In this project, we start from exploring the trend of Covid-19 in NYC over the recent three years by looking into the change of daily-counted cases and antibody percentage,
                                  then, inspiring by the fact that many people worked from home during pandemic and returned to office recently, we try to analyze if this change in working style has any impact 
                                  on the amount of energy consumption in NYC: for example, people spent more time at home and needed to consume more energy. So we investigated into the datasets of electric, water 
                                  and heating gas consumption -- the three main energy in our daily life -- provided by New York City Housing Authority and compared it with the Covid-19 cases to see if there is 
                                  any similiarity in the trend."))),
                 fluidRow(box(width = 15, title = "Targeted User", status = "primary", solidHeader=TRUE,
                              h5("We believe that the application would be useful for anyone who are interested in learning more about the trend of Covid 19 and its impact on energy consumption in NYC from 2020 to 2022."),
                              h5("It may also help government officals to make decisions about adjusting energy prices, promoting energy-efficient activities and other energy management to better respond to the pandemic."))),
                 fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                              solidHeader = TRUE,
                              h5("The application is divided into 7 separate tabs"),
                              tags$div(tags$ul(
                                tags$li("Introduction"),
                                tags$li("Covid-19 daily count yearly trend"),
                                tags$li("Antibody percentage map of NYC"),
                                tags$li("Energy consumption map of NYC"),
                                tags$li("Energy consumption trend over the years"),
                                tags$li("Comparison of Covid-19 cases vs. energy consumptions"),
                                tags$li("Conclusions")
                              ))
                 ))
     )),
    
     #------------------ Yuli: Energy Consumption Yearly Trend ----------------------------
     tabItem(tabName = "Energy_Yearly_Trend", fluidPage(
       # App title ----
       titlePanel("Energy Consumption Trend over the Years"),
       # Sidebar layout with input and output definitions ----
       sidebarLayout(
         # Sidebar panel for inputs ----
         sidebarPanel(
           # Input: Select for the borough ----
           selectInput(inputId = "Borough_yuli",
                       label = "Choose a borough:",
                       choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", 
                                   "Staten Island", "FHA", "Non Development Facility", "All")),
           
           # Input: Select for the enegry type ----
           selectInput(inputId = "Energy_yuli",
                       label = "Choose an energy type:",
                       choices = c("Water", "Electric", "Heating Gas", "All"))
         ),
         # Main panel for displaying output ----
         mainPanel(
           # Output: trend of consumption over the years ----
           plotOutput(outputId = "tsPlot_yuli")
         )
       )
     ))
   )
 )
 
 ui <- dashboardPage(
   title= "Covid 19 and Its Impact on Energy Consumption in NYC",
   skin = "blue", 
   dashboardHeader(title=span("Covid-19 and Energy Consumption",style="font-size: 13px")),
   dashboardSidebar(sidebarMenu(
     menuItem("Home", tabName = "Introduction"),
     menuItem("Energy - Yearly Trend", tabName = "Energy_Yearly_Trend") 
   )),
   body 
 )
    