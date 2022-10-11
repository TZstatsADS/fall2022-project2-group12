
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}

shinyUI(
  fluidPage(
    titlePanel('Antibody percentage per neighbourhood'),
    fluidRow(
      mainPanel(div(
      leafletOutput("covid", width = "50%"))),
    sidebarPanel(p('We can see that downtown has some of the highest antibody rates, as well as Midtown and the border between Brooklyn and Queens. The Upper East Side has some of the lowest. Perhaps the antibody rates can be related to the neighbourhood\'s most common occupations. The more front-facing an occupation is, the higher the rate.'))),
    p(),
    titlePanel('Antibody percentage per neighbourhood poverty level'),
    fluidRow(
      mainPanel(
      tableOutput(outputId = "antibody_poverty")),
    sidebarPanel(p('In the above graph, we saw that different neighbourhoods had different antibody rates. We know different neighbourhoods in NYC are populated by different age groups and occupations, and although we don\'t have direct data therein on each neighbourhood. However, we do have data on poverty levels and antibody rates. We know that more front-facing jobs are paid less and have more exposure to the virus, so it makes sense that antibody rates rise with poverty levels.'))),
    p(),
    titlePanel('Antibody percentage per year'), 
    fluidRow(
      mainPanel(
        plotOutput(outputId='antibody_year')),
    sidebarPanel(
      selectInput("antibody_year", label = "Antibody Week",
                  choices = c(2020, 2021, 2022), selected = 2020),
      p('It is interesting to see that antibody rates decrease over time in 2020, but every other year it always increases. Perhaps as more people got vaccinated and were able to see more people, their antibody rates increased.')
    )
  )
  )
)
    