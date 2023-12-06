library(shiny)
library(shinyWidgets)
library(ggplot2)
library(fmsb)

# Define UI ----
home_page <- fluidPage(
  setBackgroundColor("lightblue"),
  tags$style(
    HTML(
      "
      body {
        margin: 0;
        background-image: url(smoky.png);
        background-position: center;
        background-size: cover;
        height: 100vh;
      }
      #container {
        width: 100%;
        height: 100%;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }"
    )
  ),
  div(
    id = "container",
    title <- h1("How does air pollution and deaths caused by respiratory diseases relate?", style = "color:black"),
    description <- h4("In this project, we will be exploring and analyzing the correlations between the United 
                States respiratory deaths and air quality. Our goal through this analysis is to bring more concern to 
                air pollution and create awareness on the exact numerical impact it has.
                This dataset contains data about the amount of respiratory deaths and air 
                 quality (pm10, pm2.5, and no2 concentration) per U.S. State per year (from 2014-2021).", style = "color:#A020F0")
  )
)

scatter_plot <- fluidPage(
  h1("scatter plot of deaths/pm over years page")
)

interactive_map <- fluidPage(
  h1("interactive map page to compare between states")
)


ui <- navbarPage(
  br(),
  br(),
  tabPanel("Intro Page", home_page),
  tabPanel("Plot to compare pm rates to deaths over the years", scatter_plot),
  tabPanel("Map of the States", interactive_map)
)


# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
