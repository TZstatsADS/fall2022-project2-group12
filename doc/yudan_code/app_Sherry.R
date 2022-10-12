# ui ----
source("global_Sherry.R") 

ui <- fluidPage(
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

server <- function(input, output) {
  
  # modified comparison dataset ----
  Locations_data_Sherry <- reactive({
    
    # energy & date selection ----
    ##  water ----
    if ("Water" %in% input$Energy_Sherry) {
      return(test_Sherry %>% filter(Revenue.Month == input$date_Sherry & Energy == 'Water'))
    }
    
    ## electric ----
    if ("Electric" %in% input$Energy_Sherry) {
      return(test_Sherry %>% filter(Revenue.Month == input$date_Sherry & Energy == 'Electric'))
    }
    
    ## heating&gas ----
    if ( "Heating Gas" %in% input$Energy_Sherry) {
      return(test_Sherry %>% filter(Revenue.Month == input$date_Sherry & Energy == 'Heating.Gas'))
    }
  })
  
  # mapPlot ----
  output$mapPlot_Sherry <-renderLeaflet({
    
    dat = Locations_data_Sherry()
    dat <- st_as_sf(dat, crs = st_crs(4326)) # convert data object 
    
    pal <- colorBin("YlOrRd", 5, domain = dat$Avg.Consumption )
    labels <- sprintf("<strong>%s</strong><br/>%g", dat$Borough, dat$Avg.Consumption) %>% 
      lapply(htmltools::HTML)
    
    map_energy_Sherry <- leaflet(dat) %>%
      setView(lng = -73.97, lat = 40.78, zoom = 10) %>%
      addProviderTiles(provider = 'CartoDB.Positron') %>%
      addPolygons(
        label = labels,
        color = "white",
        dashArray = "3",
        smoothFactor = 0.5,
        opacity = 1,
        fillOpacity = 0.7,
        fillColor = ~pal(Avg.Consumption),
        highlightOptions = highlightOptions(weight = 5,
                                            fillOpacity = 0.7,
                                            color = "#666",
                                            opacity = 1,
                                            bringToFront = TRUE)) %>%
      addLegend(pal = pal, 
                values = ~Avg.Consumption, 
                title = 'Avg.Consumption',
                opacity = 0.7, 
                position = "bottomright")
    map_energy_Sherry
  })
  
  # barPlot ----
  output$barPlot_Sherry <-renderPlot({
    dat = Locations_data_Sherry()
    bar_covid_Sherry <- ggplot(data = dat, aes(x = reorder(Borough, -covid_case_count), y = covid_case_count)) +
      geom_bar(aes(fill = covid_case_count), stat = "identity") +
      scale_fill_gradient(low = "yellow", high = '#DC143C' ) + 
      geom_text(aes(label = covid_case_count), color = "black", vjust = 1.6, size = 8) +
      labs(x = "Borough", y = "Covid case count") + 
      theme(legend.position="bottom") +
      theme_minimal()
    bar_covid_Sherry
  })
}

shinyApp(ui, server)