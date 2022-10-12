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
})

