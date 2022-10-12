---
title: "R Notebook"
output: html_notebook
---
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