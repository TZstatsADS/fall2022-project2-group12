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
})

