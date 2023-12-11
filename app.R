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
        quality and respiratory deaths. In an era overshadowed by climate change concerns, 
        our project, led by Jack Scott, Andrew Chen, and Aarfan Hussain under Prof. 
        Julia Deeb-Swihart, explores the relationship between air pollution and chronic lower 
        respiratory diseases (chronic obstructive pulmonary disease (COPD), chronic bronchitis, 
        emphysema, and asthma) from 2014 to 2021 in the United States. Leveraging data from the 
        World Health Organization and the U.S. government, we aim to demonstrate the urgent 
        connection between escalating air pollution and rising respiratory deaths. By analyzing 
        PM10, PM2.5, and NO2 concentrations alongside respiratory death counts, categorized by 
        specific causes, we seek to uncover compelling trends. Join us in understanding the 
        pressing implications of this critical issue, advocating for timely action to mitigate 
        risks and foster a healthier future. Explore our specificlly collected datasets from 
        the WHO Ambient Air Quality Database and the CDC’s National Center for Health Statistics 
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
                         "Through analyzing the data in the format of a map, it allows us to compare and contrast the different
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
          p("After reading through our pages, we hope for you to come to the conclusion of how 
            our world is heavily impacted by ourselves. Through the ever-prevalent danger that 
            air pollution has over us, it impacts our day-to-day lives and could even be a cause 
            of death for some people. While overtime respiratory death rates continue to get 
            better and air pollution levels decrease, it is still a very important task to make 
            sure that these rates keep decreasing, so that we can have a future with clean air, 
            where no one gets illnesses because of polluted air. It is the right of every human 
            being to make sure that our children and their future children grow up being happy 
            and healthy, and to make sure that they can breathe fresh air. We hope our readers 
            to have a better understanding of how air quality and respiratory death rates may 
            be correlated, and to keep that in mind when natural disasters like wildfires  
            happen (which produce more PM10 and PM2.5) they can affect people in ways you might 
            have not thought about before. Tiny things like PM10 and PM2.5 can lead to a whole 
            variety of big and bad health effects. We want our readers to think about the impact 
            that they cause in regards to air pollution. Because cars produce a sizable amount 
            of air pollution, try carpooling more often, or do other small things to help the 
            fight against air pollution. In conclusion, make sure that you stay healthy, and 
            to help contribute to a better future by decreasing air pollution.", 
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
  ),
  
  div(
    br(),
    br(),
    h1(
      strong("SOURCES"),
      style = "font-family: 'Questrial'; border-bottom: 3px solid #37ad88; border-top: 3px solid #37ad88;
                          padding-top: 15px; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 24pt;
                          letter-spacing: .2rem; color: #466378",
      align = "center"
    ),
    style = "padding-bottom: 30px" ,
    br(),
    div(
      p(HTML("CDC Chronic Lower Respiratory Disease Mortality by State: https://www.cdc.gov/nchs/pressroom/sosmap/lung_disease_mortality/lung_disease.htm
      <br></br>WHO Ambient Air Quality Database: https://www.who.int/data/gho/data/themes/air-pollution/who-air-quality-database"), 
        style = "text-align:justify, center; font-family: 'Noto Sans'; color: #3d3d3d,"),
      style = "margin-right: 50px; margin-left: 50px; text-align: justify"
    ),
  )
)
  
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


ui <- tabsetPanel( id = "tabs",
  br(),
  br(),
  tabPanel(
    "Intro & Context", 
    value = "intro_page", 
    home_page
  ),
  tabPanel(
    "Line Plot", 
    value = "lineplot_page", 
    fluidPage(
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
            column(3,
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
            column(9,
                   fluidRow(
                     p(
                       strong("Select air pollution type:"), 
                       style = "font-size:15px; padding-bottom: 1px; padding-left: 15px",
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
                       tags$head(tags$style("#line_title{font-size:23px; padding-left: 15px}"))
                     ),
                     align="left",
                   ),
                   plotOutput(outputId = "line"),
                   htmlOutput(outputId = "pollutant_info")
            )
          )
      ),
      div(
        h1(
          strong(HTML("Analysis<br/><br/>Data Story: Change Over Time (Trends) and Contrast")),
          style = "font-family: 'Questrial'; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 18pt;
                letter-spacing: .2rem; color: #466378",
          align = "center"
        ),
        p(
          HTML("Starting off, it is very important to see how each individual U.S. state changes 
      over time. It helps understand whether or not there is a connection between the respiratory 
      death rate of a state and it's pollution levels. As you can see, there seems to be a slight 
      amount of correlation between death rate and pollution levels. In some cases, it seems to 
      be not correlated at all, for example Colorado's PM2.5 levels and its death rate. In other 
      states cases, there is a strong correlation, for example Maine's death rate and PM10 levels.
      <br/><br/>I believe that this difference can be attributed to our datasets. Like mentioned 
      in our other pages, the air quality data may have been gathered from not enough places to 
      properly represent the actual air pollution levels in a given state. This would result in 
      states like Colorado seemingly contradicting itself.<br/><br/>This could also be the reason 
      why there are pollution values that are missing, like how many states do not have NO2 data.
      <br/><br/>It is also interesting to note how PM10 lines and PM2.5 lines are usually very 
      similar to each other, and follow the same trend. This is most likely because PM10 and 
      PM2.5 come from most of the same sources, and an increase in a certain source would lead 
      to both PM10 and PM2.5 levels increasing. On that note, it is also important to take into 
      account natural disasters or special events that effects the data in some way. For example, 
      in 2018 California has a pretty big spike of PM10 and PM2.5 levels, which is most likely 
      due to the 2018 California wildfires, which produced heavy amounts of smoke and with it 
      most likely an increase in PM10 and PM2.5 levels."),
          style = "text-align:left, center; font-family: 'Noto Sans'; color: #3d3d3d,"),
        style = "margin-right: 50px; margin-left: 50px; margin-top: 100px; text-align: left"
      )
    )
  ),
  tabPanel(
    "US Map", 
    value = "map_page", 
    fluidPage(
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
            column(3,
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
            column(9,
                   fluidRow(
                     p(
                       strong("Select type:"), 
                       style = "font-size:15px; padding-bottom: 1px; padding-left: 15px",
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
                       tags$head(tags$style("#choice_title{font-size:23px; padding-left:15px}"))
                     ),
                     align="left"
                   ),
                   plotOutput(outputId = "map"),
                   htmlOutput(outputId = "US_info")
            )
          )
      ),
      div(
        h1(
          strong(HTML("Analysis<br/><br/>Data Story: Change Over Time and Contrast")),
          style = "font-family: 'Questrial'; padding-bottom: 15px; width: 1200px; margin: auto; font-size: 18pt;
                letter-spacing: .2rem; color: #466378",
          align = "center"
        ),
        p(
          HTML("As you can see, there is a large variance of the death rate, 
      PM10 levels, PM2.5 levels, and NO2 levels between all of the states, 
      with some states having a respiratory disease death rate of 20 deaths 
      per 100,000 people, and some having a rate of 60. Because of this large 
      variance, it is interesting to see how the states change over time. Going 
      through 2014 to 2021, you can see how the death rates and pollution levels 
      becomes better overtime. But, the data does not tell whether or not the 
      death rate is decreasing because pollution is decreasing, it just shows the 
      contrast between the states in a period of time.<br/><br/>This is because 
      there are many factors that go into a state's respiratory death rate, with 
      health care access and quality being a major factor. It is also interesting 
      to see how some regions of the U.S. have a worse death rate or quality of 
      air compared to other places. For example southeast U.S. primarily has the 
      worst death rates compared to other regions.<br/><br/>There are also some 
      N/A values in the datasets that we used, so there isn't always a PM10, 
      PM2.5, or NO2 value for each state.<br/><br/>One other thing to notice is 
      how some places have seemingly contradicting data, for example, Wyoming has 
      a relatively high respiratory death rate, but very low PM10, PM2.5, and NO2 
      levels. Like I mentioned previously, this is likely due to other factors, 
      like access to healthcare and whatnot. This also may be caused by the air 
      quality stations (which collected the data) in Wyoming may also be in rural 
      locations, which would not provide the most accurate data that represents 
      Wyoming as a whole."),
          style = "text-align:left, center; font-family: 'Noto Sans'; color: #3d3d3d,"),
        style = "margin-right: 50px; margin-left: 50px; margin-top: 100px; text-align: left"
      )
    )
  ),
  tabPanel(
    "Scatter Plot", 
    value = "scatter_page", 
    fluidPage(
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
            column(3,
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
            column(9,
                   fluidRow(
                     p(
                       strong("Select air pollution type:"), 
                       style = "font-size:15px; padding-bottom: 1px; padding-left: 15px",
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
                       tags$head(tags$style("#scatter_title{font-size:23px; padding-left:15px;}"))
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
          strong(HTML("Analysis<br/><br/>Data Story: Change Over Time (Trends)")),
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
  )
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
