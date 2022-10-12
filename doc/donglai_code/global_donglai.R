
library(dplyr)
library(ggplot2)
library(shiny)

df_daily_case_donglai=read.csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
df_daily_case_donglai$date_of_interest<-as.Date(df_daily_case_donglai$date_of_interest,"%m/%d/%Y")

ui_donglai <- fluidPage(
  # App title 
  titlePanel("Covid 19 STATISTIC"),
  # Sidebar layout 
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Select for the health statistics
      selectInput(inputId = "y_donglai",
                  label = "Choose a statistics:",
                  choices = c("Count of patients tested positive for Covid-19"="CASE_COUNT","Count of deaths among confirmed COVID-19 cases"="DEATH_COUNT","Count of COVID-19 patients who were hospitalized"="HOSPITALIZED_COUNT"),
                  selected="Count of patients tested positive for Covid-19"),
      selectInput(inputId = "x_donglai",
                  label = "Date",
                  choices = c("Date"="date_of_interest"),
                  selected="Date")
    ),
    # Main panel for displaying output 
    mainPanel(
      # Output: trend 
      plotOutput(outputId = "tsPlot_donglai")
    )
  )
)

server_donglai <- function(input, output) {
  output$tsPlot_donglai <-renderPlot({
    ggplot(data=df_daily_case_donglai,aes_string(x=input$x_donglai, y=input$y_donglai))+
      geom_bar(stat="identity",fill="#6baed6")+
      theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))+
      labs(title = "Covid-19 Statistics from 2020 to 2022")+
      ylab("Count")+
      xlab("Date")+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
    
  })
}

shinyApp(ui=ui_donglai, server=server_donglai)

