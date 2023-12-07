library(shiny)
library(shinyWidgets)
library(ggplot2)
library(fmsb)
library(plotly)

source("Final.R")

#Load in dataset (DO NOT CHANGE)
df <- read.csv("df.csv")
avg_air_quality_df <- read.csv("avg_air_quality_df.csv")

# Define UI ----
home_page <- fluidPage(
  # setBackgroundColor("lightblue"),
  # tags$style(
  #   HTML(
  #     "
  #     body {
  #       margin: 0;
  #       background-image: url(smoky.png);
  #       background-position: center;
  #       background-size: cover;
  #       height: 100vh;
  #     }
  #     #container {
  #       width: 100%;
  #       height: 100%;
  #       display: flex;
  #       flex-direction: column;
  #       justify-content: center;
  #       align-items: center;
  #     }"
  #   )
  # ),
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

line_plot <- fluidPage(
  h1("linechart of deaths/pm over years page"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state_name",
        label = "Select State",
        choices = df$state_name,
        selected = 3
      ),
      htmlOutput(outputId = "state_info"),
      br()
    ),
    mainPanel(
      h3("Linechart over the years"),
      actionButton(
        input = "pm10",
        label = "PM10"
      ),
      actionButton(
        input = "pm25",
        label = "PM2.5"
      ),
      actionButton(
        input = "no2",
        label = "NO2"
      ),
      plotOutput(outputId = "line"),
      htmlOutput(outputId = "pollutant_info")
    )
  ),
)

interactive_map <- fluidPage(
  h1("interactive map page to compare between states")
)


ui <- navbarPage(
  br(),
  br(),
  tabPanel("Intro Page", home_page),
  tabPanel("Plot to compare pm rates to deaths over the years", line_plot),
  tabPanel("Map of the States", interactive_map)
)


# Define server logic ----
server <- function(input, output) {
  
  
  values <- reactiveValues(
    pollutant_type = "avg_pm10"
  )
  
  observeEvent(input$pm10, {
    values$pollutant_type <- "avg_pm10"
  }) 
  observeEvent(input$pm25, {
    values$pollutant_type <- "avg_pm25"
  })
  observeEvent(input$no2, {
    values$pollutant_type <- "avg_no2"
  })
  
  output$state_info <- renderUI({
    get_state_info(avg_air_quality_df, input$state_name)
  })
  
  output$line <- renderPlot({
    p <- state_over_time_linechart(input$state_name, values$pollutant_type)
    return(p)
  })
  
  output$pollutant_info <- renderUI({
    get_pollutant_info(values$pollutant_type)
  })
  
  # output$pollutant_info <- renderText({
  #   if(input$pm10 == 1){
  #     pollutant_type <- "avg_pm10"
  #     
  #     paste("pm10 is this blah blah blah")
  #   } else if(input$pm25 == 1){
  #     pollutant_type <- "avg_pm25"
  #     output$line
  #     paste("pm2.5 is this blah blah blah")
  #   } else if(input$no2 == 1){
  #     pollutant_type <- "avg_no2"
  #     paste("no2 is this blah blah blah")
  #   }
  # })

  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
