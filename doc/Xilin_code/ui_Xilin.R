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