---
title: "R Notebook"
output: html_notebook
---
library(dplyr)
library(ggplot2)
library(stringr)
library(shiny)

ui <- fluidPage(
  # App title 
  titlePanel("Covid 19 STATISTIC"),
  # Sidebar layout 
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Select for the health statistics
      selectInput(inputId = "y",
                  label = "Choose a statistics:",
                  choices = c("Count of patients tested positive for Covid-19"="CASE_COUNT","Count of deaths among confirmed COVID-19 cases"="DEATH_COUNT","Count of COVID-19 patients who were hospitalized"="HOSPITALIZED_COUNT"),
                  selected="CASE_COUNT"),
      selectInput(inputId = "x",
                  label = "Date",
                  choices = c("Date"="date_of_interest"),
                  selected="date_of_interest"),
    ),
    # Main panel for displaying output 
    mainPanel(
      # Output: trend 
      plotOutput(outputId = "tsPlot")
    )
  )
)
