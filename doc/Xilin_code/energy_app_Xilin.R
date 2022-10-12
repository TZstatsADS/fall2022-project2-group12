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
# Import the datasets 
electric_Xilin <- read.csv('Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv')
water_Xilin <- read.csv('Water_Consumption_And_Cost__2013_-_Feb_2022_.csv')
gas_Xilin <- read.csv('Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv')

#load address data
res_address_Xilin<-read.csv("NYCHA_Residential_Addresses.csv")
address_data_Xilin<-res_address_Xilin%>%select(TDS..,ZIP.CODE)
address_data_Xilin<-distinct(address_data_Xilin,TDS..,.keep_all = TRUE)

####electricity####
electri_comsumption_Xilin<-electric_Xilin%>%
  subset(Revenue.Month>=2019-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..KWH.)

#merge two datasets
electricity_data_Xilin<-merge(x=address_data_Xilin,y=electri_comsumption_Xilin,by="TDS..")

#sum of each month consumption
electricity_data_Xilin<-electricity_data_Xilin%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..KWH.))

electricity_data_Xilin$Revenue.Month<-as.Date(paste(electricity_data_Xilin$Revenue.Month,"-01",sep=""))
electricity_data_Xilin$ZIP.CODE<-as.character(electricity_data_Xilin$ZIP.CODE)
electricity_data_Xilin<-electricity_data_Xilin %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

####water####
water_comsumption_Xilin<-water_Xilin%>%
  subset(Revenue.Month>=2020-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..HCF.)

#merge two datasets
water_data_Xilin<-merge(x=address_data_Xilin,y=water_comsumption_Xilin,by="TDS..")

#sum of each month consumption
water_data_Xilin<-water_data_Xilin%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..HCF.))

water_data_Xilin$Revenue.Month<-as.Date(paste(water_data_Xilin$Revenue.Month,"-01",sep=""))
water_data_Xilin$ZIP.CODE<-as.character(water_data_Xilin$ZIP.CODE)
water_data_Xilin<-water_data_Xilin %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

#### gas ####
heat_comsumption_Xilin<-gas_Xilin%>%
  subset(Revenue.Month>=2020-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..Therms.)

#merge two datasets
heat_data_Xilin<-merge(x=address_data_Xilin,y=heat_comsumption_Xilin,by="TDS..")

#sum of each month consumption
heat_data_Xilin<-heat_data_Xilin%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..Therms.))

heat_data_Xilin$Revenue.Month<-as.Date(paste(heat_data_Xilin$Revenue.Month,"-01",sep=""))
heat_data_Xilin$ZIP.CODE<-as.character(heat_data_Xilin$ZIP.CODE)
heat_data_Xilin<-heat_data_Xilin %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )


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



shinyApp(ui = ui, server = server)