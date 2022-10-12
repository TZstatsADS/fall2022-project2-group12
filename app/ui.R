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
    )),
    #---Xilin---#
    tabItem(
      tabName='Energy_Location_Trend', 
      fluidPage(
        tabPanel(
          "Energy Consumption Distribution Map_Xilin",
          sidebarLayout(
            sidebarPanel(
              #select energy type
              selectInput("energy_type_Xilin", "Energy type", c("electric", "water","heating gas")),
              selectInput("year_Xilin", "Year", c(2019, 2020, 2021,2022)),
              selectInput("month_Xilin", "Month",c(1,2,3,4,5,6,7,8,9,10,11,12))
            ),
            mainPanel(
              leafletOutput("heatmap_Xilin")
            )
          )
        )
      )
    ),
    #---Sherry---#
    tabItem("Energy_Comparison",
            fluidPage(
              # app title ----
              titlePanel("Energy Consumption vs. Covid Trend by Borough in NYC"),
              sidebarLayout(
                sidebarPanel(
                  h5('Based on the Census 2020, the popolation for the 5 boroughs of NYC 
      (Bronx, Brooklyn, Manhattan, Queens, Staten Island)
      respectively are 1472654, 2736074, 1694263, 2405464, 495747 ; 
      and the density of population (person/km^2) are 13482, 15227, 28872, 8542, 3327.'),
                  br(),
                  # Input: Select for the borough ----
                  selectInput(inputId = "Energy_Sherry",
                              label = "Choose an energy type:",
                              choices = c("Water", "Electric", "Heating Gas")),
                  
                  # Input: Select for the enegry type ----
                  selectInput(inputId = "date_Sherry",
                              label = "Choose an a date (by month):",
                              choices = unique(test_Sherry$Revenue.Month))
                ),
                # Main panel for displaying output ----
                mainPanel(
                  leafletOutput(outputId = "mapPlot_Sherry"),
                  br(),
                  h3("Covid case count by borough"),
                  plotOutput(outputId = "barPlot_Sherry"),
                  br()
                )
              )
            )
            ),
    #---Donglai---#
    tabItem('Covid',  fluidPage(
      # App title 
      titlePanel("Covid 19 STATISTIC"),
      # Sidebar layout 
      sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
          # Input: Select for the health statistics
          selectInput(inputId = "y_donglai",
                      label = "Choose a statistics:",
                      choices = c("Count of patients tested positive for Covid-19"="CASE_COUNT","Count of deaths among confirmed COVID-19 cases"="DEATH_COUNT","Count of COVID-19 patients who were hospitalized"="HOSPITALIZED_COUNT"),
                      selected="Count of patients tested positive for Covid-19"),
          selectInput(inputId = "x_donglai",
                      label = "Date",
                      choices = c("Date"="date_of_interest"),
                      selected="Date")
        ),
        # Main panel for displaying output 
        mainPanel(
          # Output: trend 
          plotOutput(outputId = "tsPlot_donglai")
        )
      )
    )),
    #---Nour---#
    #-----------Antibody Test------#
    tabItem(tabName = "Antibody", 
            fluidPage(
              titlePanel('Antibody test percentage per neighbourhood'),
              fluidRow(
                mainPanel(div(
                  leafletOutput("antibody_ZIP", width = "50%"))),
                sidebarPanel(p('We can see that downtown has some of the highest antibody rates, as well as Midtown and the border between Brooklyn and Queens. The Upper East Side has some of the lowest. Perhaps the antibody rates can be related to the neighbourhood\'s most common occupations. The more front-facing an occupation is, the higher the rate.'))),
              p(),
              titlePanel('Antibody test percentage per neighbourhood poverty level'),
              fluidRow(
                mainPanel(
                  tableOutput(outputId = "antibody_poverty")),
                sidebarPanel(p('In the above graph, we saw that different neighbourhoods had different antibody rates. We know different neighbourhoods in NYC are populated by different age groups and occupations, and although we don\'t have direct data therein on each neighbourhood. However, we do have data on poverty levels and antibody rates. We know that more front-facing jobs are paid less and have more exposure to the virus, so it makes sense that antibody rates rise with poverty levels.'))),
              p(),
              titlePanel('Antibody test percentage per year'), 
              fluidRow(
                mainPanel(
                  plotOutput(outputId='antibody_year')),
                sidebarPanel(
                  selectInput("antibody_year", label = "Antibody Week",
                              choices = c(2020, 2021, 2022), selected = 2020),
                  p('It is interesting to see that antibody rates decrease over time in 2020, but every other year it always increases. Perhaps as more people got vaccinated and were able to see more people, their antibody rates increased.')
                )
              )
            )   
    )
    
  )##end of tabItems


)

ui <- dashboardPage(
  title= "Covid 19 and Its Impact on Energy Consumption in NYC",
  skin = "blue", 
  dashboardHeader(title=span("Covid-19 and Energy Consumption",style="font-size: 13px")),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Introduction"),
    menuItem("Covid", tabName = "Covid"),
    menuItem("Antibody", tabName="Antibody"),
    menuItem("Energy - Yearly Trend", tabName = "Energy_Yearly_Trend"),
    menuItem("Energy - Location Trend", tabName="Energy_Location_Trend"),
    menuItem("Energy and Covid", tabName="Energy_Comparison")
  )),
  body 
)