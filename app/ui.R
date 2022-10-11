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

if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}

body <- dashboardBody(
  
  tabItems(
    #------------------ Introduction ----------------------------
    tabItem(tabName = "Introduction", fluidPage(
      fluidRow(box(width = 15, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("Title"),
                   h4("By Nour Moustafa-Fahmy, Donglai Xu, Yuli Yin, Xilin Huang, Yudan Zhang"),
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
    )),
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
    ),
    ######----Covid tab-------#
    tabItem('Covid', 
            fluidPage(
              # App title 
              titlePanel("Covid 19 STATISTIC"),
              # Sidebar layout 
              sidebarLayout(
                # Sidebar panel for inputs
                sidebarPanel(
                  # Input: Select for the health statistics
                  selectInput(inputId = "y",
                              label = "Choose a statistics:",
                              choices = c("Count of patients tested positive for Covid-19"="CASE_COUNT","Count of deaths among confirmed COVID-19 cases"="DEATH_COUNT","Count of COVID-19 patients who were hospitalized"="HOSPITALIZED_COUNT"),
                              selected="CASE_COUNT"),
                  selectInput(inputId = "x",
                              label = "Date",
                              choices = c("Date"="date_of_interest"),
                              selected="date_of_interest"),
                ),
                # Main panel for displaying output 
                mainPanel(
                  # Output: trend 
                  plotOutput(outputId = "tsPlot")
                )
              )
            )
            )
    #-----#
  )
)

ui <- dashboardPage(
  title="Covid 19 and The effect on Energy in NYC",
  skin = "blue", 
  dashboardHeader(title=span("Covid 19 and NYC Business",style="font-size: 16px")),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Introduction", icon = icon("introduction")),
    menuItem("Energy", tabName = "Energy", icon = icon("energy")),
    menuItem("Covid", tabName="Covid"),
    menuItem("Antibody", tabName = "Antibody")
  )),
  body 
)