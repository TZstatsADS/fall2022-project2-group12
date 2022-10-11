library(shiny)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(mapview)
library(leafsync)
library(tigris)



# Define server logic ----
server <- function(input, output) {
  options(tigris_use_cache = TRUE)
  zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York")
  year <- reactive({
    if (input$`energy type` == "electricity"){
      if (input$year == 2019) {
        return(filter(electricity_data, Reve_Month == input$month & Reve_Year== 2019))
      }
      if (input$year == 2020) {
        return(filter(electricity_data, Reve_Month == input$month & Reve_Year== 2020))
      }
      if (input$year == 2021) {
        return(filter(electricity_data, Reve_Month == input$month & Reve_Year== 2021))
      }
      if (input$year == 2022) {
        return(filter(electricity_data, Reve_Month == input$month & Reve_Year== 2022))
      }
    }
    
    if (input$`energy type` == "water"){
      if (input$year == 2019) {
        return(filter(water_data, Reve_Month == input$month & Reve_Year== 2019))
      }
      if (input$year == 2020) {
        return(filter(water_data, Reve_Month == input$month & Reve_Year== 2020))
      }
      if (input$year == 2021) {
        return(filter(water_data, Reve_Month == input$month & Reve_Year== 2021))
      }
      if (input$year == 2022) {
        return(filter(water_data, Reve_Month == input$month & Reve_Year== 2022))
      }
    }
    
    if (input$`energy type` == "heating gas"){
      if (input$year == 2019) {
        return(filter(heat_data, Reve_Month == input$month & Reve_Year== 2019))
      }
      if (input$year == 2020) {
        return(filter(heat_data, Reve_Month == input$month & Reve_Year== 2020))
      }
      if (input$year == 2021) {
        return(filter(heat_data, Reve_Month == input$month & Reve_Year== 2021))
      }
      if (input$year == 2022) {
        return(filter(heat_data, Reve_Month == input$month & Reve_Year== 2022))
      }
    }
    
  })
  
  output$heatmap <- renderLeaflet({
    
    zcta <- geo_join(zcta, year(), by_sp = "ZCTA5CE10", by_df = "ZIP.CODE", how = "left")
    zcta <- zcta %>% drop_na()
    
    if (input$`energy type` == "electricity") {
      pal <- colorNumeric(
        palette = "Greens",
        domain = zcta$month_consumption)
    }
    
    if (input$`energy type` == "water") {
      pal <- colorNumeric(
        palette = "Blues",
        domain = zcta$month_consumption)
    }
    
    if (input$`energy type` == "heating gas") {
      pal <- colorNumeric(
        palette = "Reds",
        domain = zcta$month_consumption)
    }
    
    
    labels <- 
      paste0(
        "zip code: ",
        zcta$ZCTA5CE10, "<br/>", input$year,"-", input$month,
        ": ",
        zcta$month_consumption) %>%
      lapply(htmltools::HTML)
    
    zcta %>% 
      leaflet %>% 
      # add base map
      addProviderTiles("CartoDB") %>% 
      # add zip codes
      addPolygons(fillColor = ~pal(month_consumption),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 2,
                                               color = "#666",
                                               dashArray = "",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels) %>%
      addLegend(pal = pal,
                values = ~month_consumption,
                opacity = 0.7, 
                title = htmltools::HTML(paste0(input$year,"-",input$month, "<br> 
                                by zip code")),
                position = "bottomright")
  })
}