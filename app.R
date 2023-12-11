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
  div(
        h1(
          strong("Examining the Correlation Between Air Pollution and Mortality Due to Respiratory Diseases: A Comprehensive Data Analysis"),
          style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                    padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 18pt;
                    letter-spacing: .2rem; color: #466378",
          align = "center"
        ),
        style = "padding-bottom: 50px" ,
        br(), 
        br(),
        img(src = "collage.jpg", height = 475, width = 875, style="display: block; margin-left: auto; margin-right: auto;"),
  ),
  div(
        h1(
          strong("EXPOSITION"),
          style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                      padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 24pt;
                      letter-spacing: .2rem; color: #466378",
          align = "center"
        ),
        style = "padding-bottom: 30px" ,
        br(),
        div(
          p("Welcome to our website dedicated to unraveling the critical link between air 
        quality and respiratory deaths. In an era overshadowed by climatechange concerns, 
        our project, led by Jack Scott, Andrew Chen, and Aarfan Hussain under Prof. 
        Julia Deeb-Swihart, explores the relationship between air pollution and respiratory 
        illnesses from 2014 to 2019 in the U.S. Leveraging data from the World Health 
        Organization and the U.S. government,we aim to demonstrate the urgent connection 
        between escalating air pollution and rising respiratory deaths. By analyzing PM10, 
        PM2.5, and NO2 concentrations alongside respiratory death counts, categorized by 
        specific causes, we seek to uncover compelling trends. Join us in understandingthe 
        pressing implications of this critical issue, advocating for timely action to mitigate 
        risks and foster a healthier future. Explore our specificlly collected datasets from 
        the WHO Ambient Air Quality Database and the CDC’s National Vital Statistics System 
        for an in-depth exploration of this crucial intersection.", 
            style = "text-align:justify, center; font-family: 'Noto Sans'; color: #3d3d3d,"),
          style = "margin-right: 50px; margin-left: 50px; text-align: justify"
        ),
  ),
  div(
        h1(
          strong("THE DATA"),
          style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                              padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 24pt;
                              letter-spacing: .2rem; color: #466378",
          align = "center"
        ),
        div(
          fluidRow(
            column(4,
                   h3(
                     img(src = "line.png", height = 275, width = 275, style="display: block; margin-left: auto; margin-right: auto;
                         align = 'center'"),
                     div(
                       strong("OVER THE YEARS"), style = "font-family: 'Questrial';  margin: auto; font-size: 20pt; color: 'black'; 
                       padding-top: 10px",
                       align = "center",
                       p(
                         "Through analyzing the data within a lineplot, it allows us to see and analyze the trends and patterns within
                         many categories of the data over time.",
                         style = "text-align:justify, center; font-family: 'Noto Sans'; color: #3d3d3d; font-size: 12pt; 
                         margin-left: 50px; margin-right: 50px; line-height: 1.5em" 
                       ),
                       actionButton(
                         inputId = "to_line",
                         label = "View Lineplot »",
                         style= "padding:4px; font-size:80%",
                         class = "btn-primary"
                       ),
                     ),
                     
                   ),
            ),
            column(4,
                   h3(
                     img(src = "map.png", height = 275, width = 275, style="display: block; margin-left: auto; margin-right: auto;
                         align = 'center'"),
                     div(
                       strong("BETWEEN THE STATES"), style = "font-family: 'Questrial';  margin: auto; font-size: 20pt; color: 'black'; 
                       padding-top: 10px",
                       align = "center",
                       p(
                         "Through analyzing the data in the format of a graph, it allows us to compare and contrasts the different
                         data types between each of the 50 states.",
                         style = "text-align:justify, center; font-family: 'Noto Sans'; color: #3d3d3d; font-size: 12pt; 
                         margin-left: 50px; margin-right: 50px; line-height: 1.5em" 
                       ),
                       actionButton(
                         inputId = "to_map",
                         label = "View Map »",
                         style= "padding:4px; font-size:80%",
                         class = "btn-primary"
                       ),
                     ),
                     
                   ),
            ),
            column(4,
                   h3(
                     img(src = "scatter.png", height = 275, width = 275, style="display: block; margin-left: auto; margin-right: auto;
                         align = 'center'"),
                     div(
                       strong("INSIDE THE CORRELATION"), style = "font-family: 'Questrial';  margin: auto; font-size: 20pt; color: 'black'; 
                       padding-top: 10px",
                       align = "center",
                       p(
                         "Through analyzing the data within a scatterplot, it allows us to identify the strength of the different correlations
                         and analyze the correlations themselves to compare the air pollutants.",
                         style = "text-align:justify, center; font-family: 'Noto Sans'; color: #3d3d3d; font-size: 12pt; 
                         margin-left: 50px; margin-right: 50px; line-height: 1.5em" 
                       ),
                       actionButton(
                         inputId = "to_scatter",
                         label = "View Scatterplot »",
                         style= "padding:4px; font-size:80%",
                         class = "btn-primary"
                       ),
                     ),
                   ),
            )
          ),
          style = "padding-top: 15px; padding-bottom: 30px"
        )
  ),
  div(
        h1(
          strong("CONCLUSION"),
          style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                          padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 24pt;
                          letter-spacing: .2rem; color: #466378",
          align = "center"
        ),
        style = "padding-bottom: 30px" ,
        br(),
        div(
          p("JACK PLS CARRY ME WRITE THIS THANK YOU AHHHHHHHHHHHHHHHHHHHH", 
            style = "text-align:justify, center; font-family: 'Noto Sans'; color: #3d3d3d,"),
          style = "margin-right: 50px; margin-left: 50px; text-align: justify"
        ),
  ),
  
  div(
        tabPanel(
          "About",
          mainPanel(
            style = "text-align: center;",
            uiOutput("video"),
            width = 20
            
          )
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
  ),
  div(
    h1(
      strong("Analysis"),
      style = "font-family: 'Questrial'; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 18pt;
                letter-spacing: .2rem; color: #466378",
      align = "center"
    ),
    p(
      HTML("One of the first things we learned about in any statistics class is that 
      causation CANNOT be proven through correlation, which means that none of 
      this data can be used to prove the effect of air pollutants on respiratory 
      death rates. Please keep this in mind as you read through the rest of the 
      analysis. <br/><br/>First off, none of the graphs have a r-value greater than 0.5, 
      which means none of the graphs have a stronger than moderate correlation 
      between the air pollution particulate and the state respiratory death rate.
      This is most likely due to respiratory death rate being caused by many 
      other factors aside from pollution particulates, such as other genetics-related 
      chronic diseases. <br/><br/>This also means that we can only be up to 25% 
      (r^2 value of 0.25 maximum) confident that the air pollutant rate is 
      positively (or negatively, in the case of no2) correlated with the 
      respiratory death rate.<br/><br/>With that said, among the three types of air 
      pollution particulate matter, PM2.5 seems to have the highest correlation 
      with respiratory death rate. With the null hypothesis of there being no 
      correlation between air pollution levels and respiratory deaths, most of 
      the PM2.5 graphs (specifically, years 2014, 2016, 2017, and 2021) produced 
      a p-value that is less than 0.05, meaning that we can safely reject the 
      null hypothesis, thus proving that there is statistically significant 
      positive correlation between the PM2.5 levels and Respiratory deaths. 
      <br/><br/>However, this is not the case with any of the other PM10 or NO2 graphs,
      as none of their respective p-values on the graphs are less than 0.05, 
      therefore we cannot safely conclude that there is a correlation between 
      those air pollutants and respiratory deaths. In fact, despite being very 
      weak, the NO2 graphs actually produced negative correlations between air 
      pollutant levels and respiratory death rate. This may just be due to the 
      unpredictable nature of the dataset."),
      style = "text-align:left, center; font-family: 'Noto Sans'; color: #3d3d3d,"),
    style = "margin-right: 50px; margin-left: 50px; margin-top: 100px; text-align: left"
  )
)

# -------------------------------------------------------------------------


ui <- tabsetPanel( id = "tabs",
  br(),
  br(),
  tabPanel("Intro & Context", value = "intro_page", home_page),
  tabPanel("Line Plot", value = "lineplot_page", line_plot),
  tabPanel("US Map", value = "map_page", interactive_map),
  tabPanel("Scatter Plot", value = "scatter_page", scatter_plot)
)

# -------------------------------------------------------------------------

# Define server logic ----
server <- function(input, output, session) {
  
  observeEvent(input$to_line ,{
    updateTabsetPanel(session, "tabs", selected = "lineplot_page")
  })
  observeEvent(input$to_map ,{
    updateTabsetPanel(session, "tabs", selected = "map_page")
  })
  observeEvent(input$to_scatter ,{
    updateTabsetPanel(session, "tabs", selected = "scatter_page")
  })
  
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
shinyApp(ui, server)
