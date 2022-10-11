###install packages
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}

if (!require('ggplot2')) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require('stringr')) {
  install.packages("stringr")  # Install & load stringr
  library("stringr")
}

############################################

antibody_ZIP = read.csv('../data/DOHMH_COVID-19_Antibody-by-Modified_ZIP_Code_Tabulation_Area.csv')
antibody_poverty= read.csv('../data/DOHMH_COVID-19_Antibody-by-Neighborhood_Poverty.csv')
antibody_poverty <- antibody_poverty %>% 
  select(demo_variable, PERCENT_POSITIVE, NUM_PEOP_TEST) %>%
  rename(poverty_level=demo_variable) %>%
  mutate(PERCENT_POSITIVE=PERCENT_POSITIVE*100) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyA_Low', 'low')) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyB_Medium', 'medium')) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyC_High', 'high')) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyD_Very_High', 'very high'))
  
antibody_week <- read.csv('../data/DOHMH_COVID-19_Antibody-by-Week.csv')
antibody_week[c('month', 'day','year')] <- str_split_fixed(antibody_week$WEEKDATE, '/', 3)

shinyServer(function(input, output) {
  output$covid <- renderLeaflet({
    
    antibody_summed <- antibody_ZIP %>%
      group_by(Latitude, Longitude) %>%
      summarise(avg_percent_positive = mean(PERCENT_POSITIVE)) 
    
    map <- antibody_summed %>%
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.9834,40.7504,zoom = 12) 
    
    map %>% addHeatmap(
      lng=~Longitude,
      lat=~Latitude,
      intensity=~avg_percent_positive,
      max=1,
      radius=8,
      blur=10)
  })
  
  output$antibody_poverty <- renderTable({
    antibody_poverty
  })
  
  output$antibody_year <- renderPlot({
    if (input$antibody_year == '2020') {
      antibody_week %>%
        filter(str_detect(WEEKDATE,'2020'))  %>%
        group_by(month) %>%
        summarize(percent_positive_month=mean(PERCENT_POSITIVE)) %>%
        ggplot +
        aes(
          x = month,
          y = percent_positive_month
        ) + 
        geom_point() +
        labs(title='Monthly Rate of Positive Antibody Tests') +
        xlab('Month')+
        ylab('Monthly Rate of Positive Antibody Tests')
    }
    else if (input$antibody_year == '2021') {
      antibody_week %>%
        filter(str_detect(WEEKDATE,'2021'))  %>%
        group_by(month) %>%
        summarize(percent_positive_month=mean(PERCENT_POSITIVE)) %>%
        ggplot +
        aes(
          x = month,
          y = percent_positive_month
        ) + 
        geom_point() +
        labs(title='Monthly Rate of Positive Antibody Tests') +
        xlab('Month')+
        ylab('Monthly Rate of Positive Antibody Tests')
    }
    else {
      antibody_week %>%
        filter(str_detect(WEEKDATE,'2022'))  %>%
        group_by(month) %>%
        summarize(percent_positive_month=mean(PERCENT_POSITIVE)) %>%
        ggplot +
        aes(
          x = month,
          y = percent_positive_month
        ) + 
        geom_point() +
        labs(title='Monthly Rate of Positive Antibody Tests') +
        xlab('Month')+
        ylab('Monthly Rate of Positive Antibody Tests')
    }
  })
  
})