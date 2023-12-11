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
  setBackgroundColor("#ebf6fa"),
  h1(
    strong("Examining the Correlation Between Air Pollution and Mortality Due to Respiratory Diseases: A Comprehensive Data Analysis"),
    style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 24pt;
                letter-spacing: .2rem; color: #466378",
    align = "center"
  ),
  style = "padding-bottom: 50px" ,
  br(), 
  br(),
  img(src = "collage.jpg", height = 475, width = 875, style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  br(),
  h2(
    strong("Unraveling Insights and Trends"),
    style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                  padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 20pt;
                  letter-spacing: .2rem; color: #466378",
    align = "center"
  ),
  style = "padding-bottom: 50px" ,
  br(),
  p("Welcome to our website dedicated to unraveling the critical link between air quality and respiratory deaths. In an era overshadowed by climate",
    br(),
    "change concerns, our project, led by Jack Scott, Andrew Chen, and Aarfan Hussain under Prof. Julia Deeb-Swihart, explores the relationship between",
    br(),
    "air pollution and respiratory illnesses from 2014 to 2019 in the U.S. Leveraging data from the World Health Organization and the U.S. government,",
    br(),
    "we aim to demonstrate the urgent connection between escalating air pollution and rising respiratory deaths. By analyzing PM10, PM2.5, and NO2",
    br(),
    "concentrations alongside respiratory death counts, categorized by specific causes, we seek to uncover compelling trends. Join us in understanding",
    br(),
    "the pressing implications of this critical issue, advocating for timely action to mitigate risks and foster a healthier future. Explore our",
    br(),
    "specifclly collected datasets from the WHO Ambient Air Quality Database and the CDC’s National Vital Statistics System for an in-depth",
    br(),
    "exploration of this crucial intersection.", style = "text-align:center; color:black"),
  br(),
  br(),
  #HTML('<iframe width="560" height="315" src= "
 # https://drive.google.com/file/d/1DYNTpYiQAlbl2CyoOYwmYD3B-U0W6eq-/view?usp=sharing" frameborder="0" 
  #allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
 tabPanel(
   "About",
   mainPanel(
     style = "text-align: center;",
     uiOutput("video"),
     width = 20
     
   )
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
                class = "btn-primary",
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
              textOutput("line_title"), 
              tags$head(tags$style("#line_title{font-size:23px;}"))
            ),
            align="left",
          ),
          plotOutput(outputId = "line"),
          htmlOutput(outputId = "pollutant_info")
        )
      )
    )
)

# -------------------------------------------------------------------------


interactive_map <- fluidPage(
  div(class = "test",
      h1(
        strong("ACROSS THE STATES..."),
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
               tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
               sliderInput("var", 
                           label = "Current Year:",
                           sep = "",
                           min = 2014, max = 2021, value = 2014),
               tags$style(type = "text/css", ".irs-grid-pol.small {height: 4px;}"),
               sliderInput("range", 
                           label = "Range of interest:",
                           min = 0, max = 70, value = c(0, 70)),
               htmlOutput(outputId = "US_map_info"),
               style="border-right: 2px solid; padding-top: 100px; padding-bottom: 100px"
        ),
        column(8,
               fluidRow(
                 p(
                   strong("Select type:"), 
                   style = "font-size:15px; padding-bottom: 1px",
                   actionButton(
                     input = "USdeath",
                     label = "Death Rate",
                     style = "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   actionButton(
                     input = "USpm10",
                     label = "PM10",
                     style= "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   actionButton(
                     input = "USpm25",
                     label = "PM2.5",
                     style= "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   actionButton(
                     input = "USno2",
                     label = "NO2",
                     style= "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   textOutput("choice_title"), 
                   tags$head(tags$style("#choice_title{font-size:23px;}"))
                 ),
                 align="left"
               ),
               plotOutput(outputId = "map"),
               htmlOutput(outputId = "US_info")
        )
      )
  )
)

# -------------------------------------------------------------------------

scatter_plot <- fluidPage(
  div(class = "test",
      h1(
        strong("INSIDE THE CORRELATION..."),
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
               # selectInput("scatter_var", 
               #             label = "Choose a year to display",
               #             choices = df$YEAR[1:8],
               #             selected = df$YEAR[1:1]),
               tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
               sliderInput("scatter_var", 
                           label = "Current Year:",
                           sep = "",
                           min = 2014, max = 2021, value = 2014),
               style="border-right: 2px solid; padding-top: 100px; padding-bottom: 100px"
        ),
        column(8,
               fluidRow(
                 p(
                   strong("Select air pollution type:"), 
                   style = "font-size:15px; padding-bottom: 1px",
                   actionButton(
                     input = "scatter_pm10",
                     label = "PM10",
                     style= "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   actionButton(
                     input = "scatter_pm25",
                     label = "PM2.5",
                     style= "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   actionButton(
                     input = "scatter_no2",
                     label = "NO2",
                     style= "padding:4px; font-size:80%",
                     class = "btn-primary"
                   ),
                   textOutput("scatter_title"), 
                   tags$head(tags$style("#scatter_title{font-size:23px;}"))
                 ),
                 align="left"
               ),
               plotOutput(outputId = "scatter"),
               htmlOutput(outputId = "scatter_info")
        )
      )
  )
)

# -------------------------------------------------------------------------


ui <- tabsetPanel(
  br(),
  br(),
  tabPanel("Intro Page", home_page),
  tabPanel("Line Plot", line_plot),
  tabPanel("Map of the States", interactive_map),
  tabPanel("Scatter Plot", scatter_plot)
)

# -------------------------------------------------------------------------

# Define server logic ----
server <- function(input, output) {
  output$video <- renderUI({
    tags$video(src = "vid.mp4", type = "video/mp4", autoplay = NA, controls = NA)
  })
  values <- reactiveValues(
    pollutant_type = "avg_pm10"
  )
  output$line_title <- renderText({sprintf("Respiratory Death Rate and PM10 Level over time in %s", input$state_name)})
  
  observeEvent(input$pm10, {
    values$pollutant_type <- "avg_pm10"
    output$line_title <- renderText({sprintf("Respiratory Death Rate and PM10 Level over time in %s", input$state_name)})
  }) 
  observeEvent(input$pm25, {
    values$pollutant_type <- "avg_pm25"
    output$line_title <- renderText({sprintf("Respiratory Death Rate and PM2.5 Level over time in %s", input$state_name)})
  })
  observeEvent(input$no2, {
    values$pollutant_type <- "avg_no2"
    output$line_title <- renderText({sprintf("Respiratory Death Rate and NO2 Level over time in %s", input$state_name)})
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

  
  # -------------------------------------------------------------------------
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
  
  
  # ---------------------------------------------------------------------------------
  ### Scatter plot :)
  
  scatter_values <- reactiveValues(
    scatter_choice = "avg_pm10"
  )
  
  output$scatter_title <- renderText({paste("U.S. Respiratory Death Rate and Average PM10 Level in", input$scatter_var)})
  
  observeEvent(input$scatter_pm10, {
    scatter_values$scatter_choice <- "avg_pm10"
    output$scatter_title <- renderText({paste("U.S. Respiratory Death Rate and Average PM10 Level in", input$scatter_var)})
  }) 
  observeEvent(input$scatter_pm25, {
    scatter_values$scatter_choice <- "avg_pm25"
    output$scatter_title <- renderText({paste("U.S. Respiratory Death Rate and Average PM2.5 Level in", input$scatter_var)})
  })
  observeEvent(input$scatter_no2, {
    scatter_values$scatter_choice <- "avg_no2"
    output$scatter_title <- renderText({paste("U.S. Respiratory Death Rate and Average NO2 Level in", input$scatter_var)})
  })
  
  output$scatter <- renderPlot({
    plot(make_scatter(scatter_values$scatter_choice, input$scatter_var))
  })
  output$scatter_info <- renderUI({
    get_pollutant_info(scatter_values$scatter_choice)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
