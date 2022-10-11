library(shiny)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(mapview)
library(leafsync)
library(tigris)



# Define UI ----
ui <- fluidPage(
  tabPanel(
    "Electricity Consumption Trend",
    sidebarLayout(
      sidebarPanel(
        #select energy type
        selectInput("energy type", "Energy type", c("electricity", "water","heating gas")),
        selectInput("year", "Year", c(2019, 2020, 2021,2022)),
        selectInput("month", "Month",c(1,2,3,4,5,6,7,8,9,10,11,12))
      ),
      mainPanel(
        leafletOutput("heatmap")
      )
    )
  )
)