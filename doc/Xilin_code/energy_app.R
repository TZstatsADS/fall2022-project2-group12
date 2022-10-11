#The packages we will use
library(shiny)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(mapview)
library(leafsync)
library(tigris)

####data processing
#load address data
res_address<-read.csv("NYCHA_Residential_Addresses.csv")
address_data<-res_address%>%select(TDS..,ZIP.CODE)
address_data<-distinct(address_data,TDS..,.keep_all = TRUE)

#load electric data
electric<-read.csv("Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv")


electri_comsumption<-electric%>%
  subset(Revenue.Month>=2019-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..KWH.)

#merge two datasets
electricity_data<-merge(x=address_data,y=electri_comsumption,by="TDS..")

#sum of each month consumption
electricity_data<-electricity_data%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..KWH.))

electricity_data$Revenue.Month<-as.Date(paste(electricity_data$Revenue.Month,"-01",sep=""))
electricity_data$ZIP.CODE<-as.character(electricity_data$ZIP.CODE)
electricity_data<-electricity_data %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

#load water data
water<-read.csv("Water_Consumption_And_Cost__2013_-_Feb_2022_.csv")


water_comsumption<-water%>%
  subset(Revenue.Month>=2020-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..HCF.)

#merge two datasets
water_data<-merge(x=address_data,y=water_comsumption,by="TDS..")

#sum of each month consumption
water_data<-water_data%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..HCF.))

water_data$Revenue.Month<-as.Date(paste(water_data$Revenue.Month,"-01",sep=""))
water_data$ZIP.CODE<-as.character(water_data$ZIP.CODE)
water_data<-water_data %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

#load gas data
gas<-read.csv("Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv")


heat_comsumption<-gas%>%
  subset(Revenue.Month>=2020-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..Therms.)

#merge two datasets
heat_data<-merge(x=address_data,y=heat_comsumption,by="TDS..")

#sum of each month consumption
heat_data<-heat_data%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..Therms.))

heat_data$Revenue.Month<-as.Date(paste(heat_data$Revenue.Month,"-01",sep=""))
heat_data$ZIP.CODE<-as.character(heat_data$ZIP.CODE)
heat_data<-heat_data %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )


# Define UI ----
ui <- fluidPage(
  tabPanel(
    "electric Consumption Trend",
    sidebarLayout(
      sidebarPanel(
        #select energy type
        selectInput("energy type", "Energy type", c("electric", "water","heating gas")),
        selectInput("year", "Year", c(2019, 2020, 2021,2022)),
        selectInput("month", "Month",c(1,2,3,4,5,6,7,8,9,10,11,12))
      ),
      mainPanel(
        leafletOutput("heatmap")
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  options(tigris_use_cache = TRUE)
  zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York")
  year <- reactive({
    if (input$`energy type` == "electric"){
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
    
    if (input$`energy type` == "electric") {
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



shinyApp(ui = ui, server = server)