library(shiny)
library(shinyWidgets)
library(ggplot2)
library(fmsb)
library(plotly)
library(maps)
library(mapproj)

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
  theme = bslib::bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "site.css")
  ),
  setBackgroundColor("#ebf6fa"),
  HTML('<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Noto+Sans&family=Questrial&display=swap" rel="stylesheet">'),
  div(class = "test",
    h1(
      strong("OVER THE YEARS..."),
      style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 24pt;
                letter-spacing: .2rem; color: #466378",
      align = "center"
    ),
    style = "padding-bottom: 50px"
  ),
  div(class="container",
    style="font-family: 'Noto Sans'; color: #3d3d3d",
    fluidRow(
        column(4,
          selectInput(
            inputId = "state_name",
            label = "Select State",
            choices = df$state_name,
            selected = 1
          ),
          htmlOutput(outputId = "state_info"),
          br(),
          style="border-right: 2px solid; padding-top: 100px; padding-bottom: 100px"
        ),
        column(8,
          fluidRow(
            p(
              strong("Select air pollution type:"), 
              style = "font-size:15px; padding-bottom: 1px",
              actionButton(
                input = "pm10",
                label = "PM10",
                style= "padding:4px; font-size:80%",
                class = "btn-primary"
              ),
              actionButton(
                input = "pm25",
                label = "PM2.5",
                style= "padding:4px; font-size:80%",
                class = "btn-primary"
              ),
              actionButton(
                input = "no2",
                label = "NO2",
                style= "padding:4px; font-size:80%",
                class = "btn-primary"
              ), 
            ),
            align="center",
          ),
          plotOutput(outputId = "line"),
          htmlOutput(outputId = "pollutant_info")
        )
      )
    )
)

# -------------------------------------------------------------------------


interactive_map <- fluidPage(
  h1("Map of the Contiguous United States"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  label = "Choose a year to display",
                  choices = df$YEAR[1:8],
                  selected = df$YEAR[1:1]),
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 70, value = c(0, 70)),
      htmlOutput(outputId = "US_map_info")
    ),
    mainPanel(textOutput("choice_title"), tags$head(tags$style("#choice_title{font-size:25px;}")),
              actionButton(
                input = "USdeath",
                label = "Death Rate"
              ),
              actionButton(
                input = "USpm10",
                label = "PM10"
              ),
              actionButton(
                input = "USpm25",
                label = "PM2.5"
              ),
              actionButton(
                input = "USno2",
                label = "NO2"
              ),
              plotOutput("map"),
              htmlOutput(outputId = "US_info")
    )
  )
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

  
  ### U.S. Map server stuff
  USvalues <- reactiveValues(
    USchoice = "RATE"
  )
  
  output$choice_title <- renderText({"Chronic Lower Respiratory Disease Death Rate in the United States"})
  
  observeEvent(input$USdeath, {
    USvalues$USchoice <- "RATE"
    output$choice_title <- renderText({"Chronic Lower Respiratory Disease Death Rate in the United States"})
  }) 
  observeEvent(input$USpm10, {
    USvalues$USchoice <- "avg_pm10"
    output$choice_title <- renderText({"Average PM10 Level in the United States"})
  }) 
  observeEvent(input$USpm25, {
    USvalues$USchoice <- "avg_pm25"
    output$choice_title <- renderText({"Average PM2.5 Level in the United States"})
  })
  observeEvent(input$USno2, {
    USvalues$USchoice <- "avg_no2"
    output$choice_title <- renderText({"Average NO2 Level in the United States"})
  })
  
  
  output$US_map_info <- renderUI({
    get_US_map_info(USvalues$USchoice, input$var)
  })
  output$map <- renderPlot({
    percent_map(USvalues$USchoice, input$var, input$range[1], input$range[2])
  })
  output$US_info <- renderUI({
    get_pollutant_info(USvalues$USchoice)
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
