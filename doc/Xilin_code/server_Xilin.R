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
  zcta_Xilin <- zctas(starts_with = c(10, 11), year = 2010, state = "New York")
  year_Xilin <- reactive({
    if (input$`energy_type_Xilin` == "electric"){
      if (input$year_Xilin == 2019) {
        return(filter(electricity_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2019))
      }
      if (input$year_Xilin == 2020) {
        return(filter(electricity_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2020))
      }
      if (input$year_Xilin == 2021) {
        return(filter(electricity_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2021))
      }
      if (input$year_Xilin == 2022) {
        return(filter(electricity_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2022))
      }
    }
    
    if (input$`energy_type_Xilin` == "water"){
      if (input$year_Xilin == 2019) {
        return(filter(water_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2019))
      }
      if (input$year_Xilin == 2020) {
        return(filter(water_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2020))
      }
      if (input$year_Xilin == 2021) {
        return(filter(water_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2021))
      }
      if (input$year_Xilin == 2022) {
        return(filter(water_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2022))
      }
    }
    
    if (input$`energy_type_Xilin` == "heating gas"){
      if (input$year_Xilin == 2019) {
        return(filter(heat_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2019))
      }
      if (input$year_Xilin == 2020) {
        return(filter(heat_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2020))
      }
      if (input$year_Xilin == 2021) {
        return(filter(heat_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2021))
      }
      if (input$year_Xilin == 2022) {
        return(filter(heat_data_Xilin, Reve_Month == input$month_Xilin & Reve_Year== 2022))
      }
    }
    
  })
  
  output$heatmap_Xilin <- renderLeaflet({
    
    zcta_Xilin <- geo_join(zcta_Xilin, year_Xilin(), by_sp = "ZCTA5CE10", by_df = "ZIP.CODE", how = "left")
    zcta_Xilin <- zcta_Xilin %>% drop_na()
    
    if (input$`energy_type_Xilin` == "electric") {
      pal <- colorNumeric(
        palette = "Greens",
        domain = zcta_Xilin$month_consumption)
    }
    
    if (input$`energy_type_Xilin` == "water") {
      pal <- colorNumeric(
        palette = "Blues",
        domain = zcta_Xilin$month_consumption)
    }
    
    if (input$`energy_type_Xilin` == "heating gas") {
      pal <- colorNumeric(
        palette = "Reds",
        domain = zcta_Xilin$month_consumption)
    }
    
    
    labels <- 
      paste0(
        "zip code: ",
        zcta_Xilin$ZCTA5CE10, "<br/>", input$year_Xilin,"-", input$month_Xilin,
        ": ",
        zcta_Xilin$month_consumption) %>%
      lapply(htmltools::HTML)
    
    zcta_Xilin %>% 
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
                title = htmltools::HTML(paste0(input$year_Xilin,"-",input$month_Xilin, "<br> 
                                by zip code")),
                position = "bottomright")
  })
}