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
  
  energy_data <- reactive({
    # Energy = water
    if ("Manhattan" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "MANHATTAN" & Energy == 'Water'))
    }
    if ("Bronx" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "BRONX" & Energy == 'Water'))
    }
    if ("Brooklyn" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "BROOKLYN" & Energy == 'Water'))
    }
    if ("Queens" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "QUEENS" & Energy == 'Water'))
    }
    if ("Staten Island" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "STATEN ISLAND" & Energy == 'Water'))
    }
    if ("FHA" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "FHA" & Energy == 'Water'))
    }
    if ("Non Development Facility" %in% input$Borough & "Water" %in% input$Energy) {
      return(energy %>% filter(Borough == "NON DEVELOPMENT FACILITY" & Energy == 'Water'))
    }
    if ("All" %in% input$Borough & "Water" %in% input$Energy) {
      data <- energy %>% filter(Energy == 'Water') %>% group_by(Revenue.Month) %>% 
        summarise_at(vars(Avg.Consumption), list(name = mean))
      colnames(data)[2] <- 'Avg.Consumption'
      return(data)
    }
    # Energy = Electric
    if ("Manhattan" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "MANHATTAN" & Energy == 'Electric'))
    }
    if ("Bronx" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "BRONX" & Energy == 'Electric'))
    }
    if ("Brooklyn" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "BROOKLYN" & Energy == 'Electric'))
    }
    if ("Queens" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "QUEENS" & Energy == 'Electric'))
    }
    if ("Staten Island" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "STATEN ISLAND" & Energy == 'Electric'))
    }
    if ("FHA" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "FHA" & Energy == 'Electric'))
    }
    if ("Non Development Facility" %in% input$Borough & "Electric" %in% input$Energy) {
      return(energy %>% filter(Borough == "NON DEVELOPMENT FACILITY" & Energy == 'Electric'))
    }
    if ("All" %in% input$Borough & "Electric" %in% input$Energy) {
      data <- energy %>% filter(Energy == 'Electric') %>% group_by(Revenue.Month) %>% 
        summarise_at(vars(Avg.Consumption), list(name = mean))
      colnames(data)[2] <- 'Avg.Consumption'
      return(data)
    }
    # Energy = Heating Gas
    if ("Manhattan" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "MANHATTAN" & Energy == 'Heating.Gas'))
    }
    if ("Bronx" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "BRONX" & Energy == 'Heating.Gas'))
    }
    if ("Brooklyn" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "BROOKLYN" & Energy == 'Heating.Gas'))
    }
    if ("Queens" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "QUEENS" & Energy == 'Heating.Gas'))
    }
    if ("Staten Island" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "STATEN ISLAND" & Energy == 'Heating.Gas'))
    }
    if ("FHA" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "FHA" & Energy == 'Heating.Gas'))
    }
    if ("Non Development Facility" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      return(energy %>% filter(Borough == "NON DEVELOPMENT FACILITY" & Energy == 'Heating.Gas'))
    }
    if ("All" %in% input$Borough & "Heating Gas" %in% input$Energy) {
      data <- energy %>% filter(Energy == 'Heating.Gas') %>% group_by(Revenue.Month) %>% 
        summarise_at(vars(Avg.Consumption), list(name = mean))
      colnames(data)[2] <- 'Avg.Consumption'
      return(data)
    }    
    # Energy = All
    if ("Manhattan" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "MANHATTAN"))
    }
    if ("Bronx" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "BRONX"))
    }
    if ("Brooklyn" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "BROOKLYN"))
    }
    if ("Queens" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "QUEENS"))
    }
    if ("Staten Island" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "STATEN ISLAND"))
    }
    if ("FHA" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "FHA"))
    }
    if ("Non Development Facility" %in% input$Borough & "All" %in% input$Energy) {
      return(energy %>% filter(Borough == "NON DEVELOPMENT FACILITY"))
    }
    if ("All" %in% input$Borough & "All" %in% input$Energy) {
      data <- energy %>% group_by(Revenue.Month, Energy) %>% 
        summarise_at(vars(Avg.Consumption), list(name = mean))
      colnames(data)[3] <- 'Avg.Consumption'
      return(data)
    }     
  })
  
  output$tsPlot <-renderPlot({
    dat = energy_data()
    if (input$Energy != 'All') {
      p <- ggplot(data=dat, aes(x=Revenue.Month, y=Avg.Consumption, group=1)) +
        geom_line(color = "#0099f9", size = 0.5)+
        geom_point(color = "#0099f9", size = 1) +
        xlab("Date") + 
        ylab("Average Consumption") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    }
    if (input$Energy == 'All') {
      p <- ggplot(data=dat[(dat$Energy=='Electric')|(dat$Energy=='Heating.Gas'),],
                  aes(x=Revenue.Month, y=Avg.Consumption, group=Energy)) +
        geom_line(aes(color = Energy)) +
        geom_line(data=dat[dat$Energy == 'Water',],
                  aes(x=Revenue.Month, y=Avg.Consumption*10, group=1, color = Energy)) +
        scale_y_continuous(name = 'Average Consumption (Electric, Heating Gas)',
                           sec.axis = sec_axis(trans=~.*0.1,name='Average Consumption (Water)')) +
        xlab("Date") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        theme(legend.position = "bottom")
    }
    p
  })
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
  #-----covid by Donglai-----#
  output$tsPlot <-renderPlot({
    ggplot(data=df_daily_case,aes_string(x=input$x, y=input$y))+
      geom_bar(stat="identity",fill="#6baed6")+
      theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))+
      labs(title = "Covid-19 Statistics from 2020 to 2022")+
      ylab("Count")+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
    
  })
})
