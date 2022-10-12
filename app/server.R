#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("global.R") 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #---Yuli----#
  energy_data_yuli <- reactive({
    # Energy = water
    if ("Manhattan" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "MANHATTAN" & Energy == 'Water'))
    }
    if ("Bronx" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BRONX" & Energy == 'Water'))
    }
    if ("Brooklyn" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BROOKLYN" & Energy == 'Water'))
    }
    if ("Queens" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "QUEENS" & Energy == 'Water'))
    }
    if ("Staten Island" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "STATEN ISLAND" & Energy == 'Water'))
    }
    if ("FHA" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "FHA" & Energy == 'Water'))
    }
    if ("Non Development Facility" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "NON DEVELOPMENT FACILITY" & Energy == 'Water'))
    }
    if ("All" %in% input$Borough_yuli & "Water" %in% input$Energy_yuli) {
      data_yuli <- energy_yuli %>% filter(Energy == 'Water') %>% group_by(Revenue.Month) %>% 
        summarise(mean=sum(Sum.Consumption))
      colnames(data_yuli)[2] <- 'Sum.Consumption'
      return(data_yuli)
    }
    
    # Energy = Electric
    if ("Manhattan" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "MANHATTAN" & Energy == 'Electric'))
    }
    if ("Bronx" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BRONX" & Energy == 'Electric'))
    }
    if ("Brooklyn" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BROOKLYN" & Energy == 'Electric'))
    }
    if ("Queens" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "QUEENS" & Energy == 'Electric'))
    }
    if ("Staten Island" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "STATEN ISLAND" & Energy == 'Electric'))
    }
    if ("FHA" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "FHA" & Energy == 'Electric'))
    }
    if ("Non Development Facility" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "NON DEVELOPMENT FACILITY" & Energy == 'Electric'))
    }
    if ("All" %in% input$Borough_yuli & "Electric" %in% input$Energy_yuli) {
      data_yuli <- energy_yuli %>% filter(Energy == 'Electric') %>% group_by(Revenue.Month) %>% 
        summarise(mean=sum(Sum.Consumption))
      colnames(data_yuli)[2] <- 'Sum.Consumption'
      return(data_yuli)
    }
    
    # Energy = Heating Gas
    if ("Manhattan" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "MANHATTAN" & Energy == 'Heating.Gas'))
    }
    if ("Bronx" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BRONX" & Energy == 'Heating.Gas'))
    }
    if ("Brooklyn" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BROOKLYN" & Energy == 'Heating.Gas'))
    }
    if ("Queens" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "QUEENS" & Energy == 'Heating.Gas'))
    }
    if ("Staten Island" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "STATEN ISLAND" & Energy == 'Heating.Gas'))
    }
    if ("FHA" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "FHA" & Energy == 'Heating.Gas'))
    }
    if ("Non Development Facility" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "NON DEVELOPMENT FACILITY" & Energy == 'Heating.Gas'))
    }
    if ("All" %in% input$Borough_yuli & "Heating Gas" %in% input$Energy_yuli) {
      data_yuli <- energy_yuli %>% filter(Energy == 'Heating.Gas') %>% group_by(Revenue.Month) %>% 
        summarise(mean=sum(Sum.Consumption))
      colnames(data_yuli)[2] <- 'Sum.Consumption'
      return(data_yuli)
    } 
    
    # Energy = All
    if ("Manhattan" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "MANHATTAN"))
    }
    if ("Bronx" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BRONX"))
    }
    if ("Brooklyn" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "BROOKLYN"))
    }
    if ("Queens" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "QUEENS"))
    }
    if ("Staten Island" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "STATEN ISLAND"))
    }
    if ("FHA" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "FHA"))
    }
    if ("Non Development Facility" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      return(energy_yuli %>% filter(Borough == "NON DEVELOPMENT FACILITY"))
    }
    if ("All" %in% input$Borough_yuli & "All" %in% input$Energy_yuli) {
      data_yuli <- energy_yuli %>% group_by(Revenue.Month, Energy) %>% 
        summarise(mean=sum(Sum.Consumption))
      colnames(data_yuli)[3] <- 'Sum.Consumption'
      return(data_yuli)
    }     
  })
  
  # Generate Plot
  output$tsPlot_yuli <-renderPlot({
    dat_yuli = energy_data_yuli()
    if (input$Energy_yuli != 'All') {
      p <- ggplot(data=dat_yuli, aes(x=Revenue.Month, y=Sum.Consumption, group=1)) +
        geom_line(color = "#0099f9", size = 0.5)+
        xlab("Date") + 
        ylab("Sum Consumption") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    }
    if (input$Energy_yuli == 'All') {
      scale_electric <- dat_yuli %>% filter(Energy == 'Electric')
      scale_water <- dat_yuli %>% filter(Energy == 'Water')
      scale <- mean(scale_electric$Sum.Consumption)/mean(scale_water$Sum.Consumption)
      
      p <- ggplot(data=dat_yuli[(dat_yuli$Energy=='Electric')|(dat_yuli$Energy=='Heating.Gas'),],
                  aes(x=Revenue.Month, y=Sum.Consumption, group=Energy)) +
        geom_line(aes(color = Energy)) +
        geom_line(data=dat_yuli[dat_yuli$Energy == 'Water',],
                  aes(x=Revenue.Month, y=Sum.Consumption*scale, group=1, color = Energy)) +
        scale_y_continuous(name = 'Sum Consumption (Electric, Heating Gas)',
                           sec.axis = sec_axis(trans=~.*(1/scale),name='Sum Consumption (Water)')) +
        xlab("Date") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        theme(legend.position = "bottom")
    }
    p
  })
  #---Xilin---#
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
  #---Sherry---#
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
  #---Donglai---#
  output$tsPlot_donglai <-renderPlot({
    ggplot(data=df_daily_case_donglai,aes_string(x=input$x_donglai, y=input$y_donglai))+
      geom_bar(stat="identity",fill="#6baed6")+
      theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))+
      labs(title = "Covid-19 Statistics from 2020 to 2022")+
      ylab("Count")+
      xlab("Date")+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
    
  })
  #---Nour---#
  #-----antibody, by nour-------#
  
  #this first section produces a map of NYC and positive antibody test rates
  output$antibody_ZIP <- renderLeaflet({
    
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
  
  #this second section provides a table of antibody test rates per poverty levels of neighbourhoods
  output$antibody_poverty <- renderTable({
    antibody_poverty
  })
  
  #this last section provides a drop down to observe how antibody rates have changed from 2020-2022
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
