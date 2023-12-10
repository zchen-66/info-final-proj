# Import needed libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggpubr)

# ----------------- DATA WRANGLING ------------------------ #

# Create dataframes
air_df <- read.csv("AirQuality.csv")
air_df <- filter(air_df, country_name == "United States of America")
deaths_df <- read.csv("DeathCounts.csv") 

# Create state name columns for merging
air_df <- filter(air_df, !is.na(city))
air_df$state_code <- str_sub(air_df$"city", str_locate(air_df[, "city"], ",")[,1]+2, 
                             str_locate(air_df[, "city"], ",")[,1]+3)
air_df$state_name <- state.name[match(air_df$"state_code", state.abb)]

deaths_df$state_name <- state.name[match(deaths_df$"STATE", state.abb)]

# Aggregate air dataframe by finding the mean air quality concentration for 
# each year for each state
air_df <- group_by(air_df, year, state_name)
new_air_df <- summarize(
  air_df,
  avg_pm10 = mean(pm10_concentration, na.rm=TRUE),
  avg_pm25 = mean(pm25_concentration, na.rm=TRUE),
  avg_no2 = mean(no2_concentration, na.rm=TRUE)
)

# Merge dataframe
df <- merge(x=deaths_df, y=new_air_df, by.x=c("state_name", "YEAR"), by.y=c("state_name", "year"), all.x=TRUE)
df <- subset(df, select=c(-URL))
df <- filter(df, YEAR != 2005)
df <- df[-64,]

# Get rid of comma in numbers
df$"DEATHS" <- ifelse(str_detect(df$"DEATHS", ","), str_remove(df$"DEATHS", ","),df$"DEATHS")
df$"DEATHS" <- as.numeric(df$"DEATHS")

# for U.S. States Map
df_mainland <- filter(df, state_name != "Alaska" & state_name != "Hawaii")

# U.S. State map data
state_names <- state.name[state.name != "Alaska" & state.name != "Hawaii"]
state_names[19] <- "massachusetts:main"
state_names[20] <- "michigan:south"
state_names[30] <- "new york:main"
state_names[31] <- "north carolina:main"
state_names[44] <- "virginia:main"
state_names[45] <- "washington:main"

# ----------------- Over time Scatterplot Page ------------------------ #
# (andrew's stuff no touchy)

# Create a dataset of the average air quality levels from 2014 - 2021 of each state
# This will be used later to "rank" the air quality of each state
avg_air_quality_df <- group_by(df, state_name)
avg_air_quality_df <- summarize(
  avg_air_quality_df,
  avg_pm10 = mean(avg_pm10, na.rm=TRUE),
  avg_pm25 = mean(avg_pm25, na.rm=TRUE),
  avg_no2 = mean(avg_no2, na.rm=TRUE)
)

# Create a new column in the avg_air_quality_df named `pm10_rank` that ranks the states
# from 1 (highest level of pm25) to 50 (lowest level of pm25)
avg_air_quality_df <- avg_air_quality_df[order(avg_air_quality_df$avg_pm10, decreasing = TRUE), ]
avg_air_quality_df$pm10_rank <- 1:nrow(avg_air_quality_df)


# Repeat the above task for `pm25_rank` and `no2_rank`
avg_air_quality_df <- avg_air_quality_df[order(avg_air_quality_df$avg_pm25, decreasing = TRUE), ]
avg_air_quality_df$pm25_rank <- 1:nrow(avg_air_quality_df)
avg_air_quality_df <- avg_air_quality_df[order(avg_air_quality_df$avg_no2, decreasing = TRUE), ]
avg_air_quality_df$no2_rank <- 1:nrow(avg_air_quality_df)

# Create a column `urgency_index` that averages the ranks of all other categories
avg_air_quality_df$urgency_index <- round((avg_air_quality_df$pm25_rank + 
                                             avg_air_quality_df$pm10_rank + 
                                             avg_air_quality_df$no2_rank) /3, 0)

# Write a function that given a state as a string and a dataframe containing 
# the dataset, returns the state air quality information an HTML string. 
get_state_info <- function(df, state){
  df <- filter(df, state_name == state)
  if(df$"urgency_index" < 15){
    quality <- "very terrible"
  } else if (df$"urgency_index" < 23) {
    quality <- "not good"
  } else if (df$"urgency_index" < 28) {
    quality <- "mediocre"
  } else if (df$"urgency_index" < 35) {
    quality <- "good"
  } else {
    quality <- "very excellent"
  }
  
  pm10_rank <- df$pm10_rank
  pm25_rank <- df$pm25_rank
  no2_rank <- df$no2_rank
  avg_pm10 <- round(df$avg_pm10, 2)
  avg_pm25 <- round(df$avg_pm25, 2)
  avg_no2 <- round(df$avg_no2, 2)
    
  
  info <- sprintf("Overall, %s's air pollutant level from 2014 to 2021 is %s when 
  compared to other states, as %s ranks %s in PM10 Levels, %s in PM2.5 
  Levels, and %s in NO2 Levels.\n\n\n\n
  Below are some stats regarding the average air pollutant levels in %s:\n\n
  Average PM10 Level: %s\n\n
  Average PM2.5 level: %s\n\n
  Average NO2 Level: %s", state, quality, state, pm10_rank, pm25_rank, no2_rank, state,
                  avg_pm10, avg_pm25, avg_no2)
  
  info <- HTML(
    str_replace_all(
      HTML(
        info
      ),
      "\n\n",
      "<br/>"
    )
  )
  
  
  return(info)
}

# Write a function that given a pollutant as a string, returns 
# the air pollutant information an HTML string. 
get_pollutant_info <- function(pollutant){
  if (pollutant == "avg_pm10"){
    info <- "PM10, or particulate matter with a diameter of 10 micrometers or less,
    consists of airborne particles originating from various sources, including dust, 
    wildfires, and industrial activities. While larger than PM2.5, PM10 particles can 
    still affect respiratory health. Regulations and monitoring efforts are implemented 
    to manage PM10 levels, promoting air quality and protecting public health by controlling
    the concentration of these particles in the atmosphere."
  } else if (pollutant == "avg_pm25"){
    info <- "PM2.5 refers to ultra-fine particles in the air originating from sources
    like wildfires and vehicle emissions. Inhaling these particles can lead to health
    issues, particularly impacting the respiratory and cardiovascular systems. To ensure
    air quality and public well-being, regulations and monitoring are in place to control
    the levels of PM2.5 in the atmosphere."
  } else if (pollutant == "avg_no2"){
    info <- "NO2, or nitrogen dioxide, is a gaseous air pollutant primarily generated from 
    combustion processes in vehicles, industrial operations, and power plants. Inhaling NO2 
    can pose health risks, particularly impacting the respiratory system. To safeguard air 
    quality and public health, regulatory measures and monitoring systems are in place to 
    control NO2 levels in the atmosphere, aiming to mitigate adverse health effects and 
    environmental consequences associated with this pollutant."
  } else if (pollutant == "RATE"){
    info <- "The death rate refers to the Chronic Lower Respiratory Disease Death Rate. The rate is the number 
    of deaths per 100,000 total population. CDC defines chronic lower respiratory diseases as: chronic obstructive 
    pulmonary disease (COPD), chronic bronchitis, emphysema, and asthma."
  }
  
  info <- HTML(
    str_replace_all(
      HTML(
        info
      ),
      "\n\n",
      "<br/>"
    )
  )
  
  
  return(info)
}

# Fill out the following function called `states_over_time` that given a 
# String state and a String type (representing the type of air pollution),
# returns a line graph The line graph your function creates should have 
# the following characteristics: 
#   x axis & Label: Year 
#   left y-axis: Respiratory Death Rate
#   right y-axis: <air quality type> Level
#   line color / point shape should correspond to either respiratory death rate
#   or the air quality level
#   Title of the plot should read "Respiratory Death Rate and <Air Quality> Level over time 
#     in <STATE>" where <STATE> is the state specified by the user. 
# your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 

state_over_time_linechart <- function(state, type){
  state_df <- filter(df, state_name == state)
  coeff <- mean(state_df[,type]) / mean(state_df[, "RATE"])
  
  NA_PLOT <- ggplot() +
    theme_classic() +
    theme(plot.title = element_text(size=22, hjust = 0.5),
        plot.background = element_rect(fill= '#ebf6fa'),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        # panel.border = element_rect(color='black', fill=NA),
        panel.background = element_rect(fill = 'white')) +
    annotate(geom="text", x = 1, y = 1,label="NO DATA AVAILABLE", color="red")
  
  if(type == "avg_pm10"){
    if (is.na(state_df[1,type])){
      return(NA_PLOT)
    }
    name <- "PM10"
  }
  if(type == "avg_pm25"){
    if (is.na(state_df[1,type])){
      return(NA_PLOT)
    }
    name <- "PM2.5"
  }
  if(type == "avg_no2"){
    if (is.na(state_df[1,type])){
      return(NA_PLOT)
    }
    name <- "NO2"
  }

  linechart <- ggplot(data = state_df, aes(x=YEAR), ) +
    geom_line( aes(y=RATE, color = "Respiratory Death Rate"), size = 1, na.rm=TRUE) +
    geom_point(aes(y=RATE, text=RATE), color = "blue", shape = 17, size = 2, na.rm=TRUE) +
    geom_line( aes(y=eval(parse(text=type)) / coeff, text=round(eval(parse(text=type)), 2), color = sprintf("%s Level", name)), size = 1, na.rm=TRUE) +
    geom_point(
      aes(y=eval(parse(text=type)) / coeff),
      color = "red", shape = 20, size = 2.5, na.rm=TRUE) +
    labs(x="Year", color = "Legend")+
         #title=sprintf(("Respiratory Death Rate and %s Level over time in %s"), name, state)) +
    theme_classic() +
    theme(plot.title = element_text(size=22, hjust = 0.5),
          plot.background = element_rect(fill= '#ebf6fa'),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color='black', size=0.25),
          # panel.border = element_rect(color='black', fill=NA),
          panel.background = element_rect(fill = 'white'),
          legend.box.background = element_rect(fill = "#ebf6fa")) +
    # element_text(family = NULL, face = NULL, colour = NULL, size = NULL,
    #              hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
    #              color = NULL)
    scale_color_manual(values=c('Red','Blue')) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=8)) +
    scale_y_continuous(

      # Features of the first axis  
      name = "Respiratory Death Rate",

      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*coeff, name=sprintf("%s Level", name))
    )

  return(linechart)
}
#plot(state_over_time_linechart("Washington", "avg_pm25"))


# -----------------------------------------------------------------------------------------

##### for u.s. state maps page :)
percent_map <- function(choice, year, min=0, max=100) {
  var <- filter(df_mainland, YEAR == year)$RATE
  color <- "red3"
  legend.title <- "Death Rate"
  
  if (choice != "RATE") {
    if(choice == "avg_pm10") {
      var <- filter(df_mainland, YEAR == year)$avg_pm10
      color <- "blue3"
      legend.title <- "Average PM10"
    } else if(choice == "avg_pm25") {
      var <- filter(df_mainland, YEAR == year)$avg_pm25
      color <- "green3"
      legend.title <- "Average PM2.5"
    } else {
      var <- filter(df_mainland, YEAR == year)$avg_no2
      color <- "yellow2"
      legend.title <- "Average NO2"
    }
  }
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  fills <- replace(fills, is.na(fills), "#A9A9A9")
  legend_fill <- shades[c(1, 25, 50, 75, 100)]
  legend_fill[6] <- "#A9A9A9"
  # overlay state borders
  map("state", region = state_names, fill = TRUE, col = fills, 
      resolution = 0, lty = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  # add a legend
  inc <- round((max(var, na.rm=TRUE) - min(var, na.rm=TRUE)) / 4, 1)
  legend.text <- c(paste0(round(min(var, na.rm=TRUE), 1), " or less"),
                   paste0(round(min(var, na.rm=TRUE), 1) + inc, ""),
                   paste0(round(min(var, na.rm=TRUE), 1) + 2 * inc, ""),
                   paste0(round(min(var, na.rm=TRUE), 1) + 3 * inc, ""),
                   paste0(round(max(var, na.rm=TRUE), 1), " or more"),
                   paste0("NA Values"))
  legend("bottomleft", 
         legend = legend.text, 
         fill = legend_fill,
         title = legend.title)
}

get_US_map_info <- function(choice, year) {
  df_us <- filter(df_mainland, YEAR == year)
  us_specific <- df_us$RATE
  type <- "Death Rate"
  state_max <- filter(df_mainland, RATE == max(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
  state_min <- filter(df_mainland, RATE == min(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
  
  if (choice != "RATE") {
    if(choice == "avg_pm10") {
      us_specific <- df_us$avg_pm10
      type <- "PM10 Level"
      state_max <- filter(df_mainland, avg_pm10 == max(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
      state_min <- filter(df_mainland, avg_pm10 == min(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
    } else if(choice == "avg_pm25") {
      us_specific <- df_us$avg_pm25
      type <- "PM2.5 Level"
      state_max <- filter(df_mainland, avg_pm25 == max(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
      state_min <- filter(df_mainland, avg_pm25 == min(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
    } else {
      us_specific <- df_us$avg_no2
      type <- "NO2 Level"
      state_max <- filter(df_mainland, avg_no2 == max(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
      state_min <- filter(df_mainland, avg_no2 == min(us_specific, na.rm=TRUE), na.rm=TRUE)$state_name[1]
    }
  }
  
  choice_max <- max(us_specific, na.rm=TRUE)
  choice_min <- min(us_specific, na.rm=TRUE)
  choice_avg <- round(mean(us_specific, na.rm=TRUE), 2)
  
  info <- sprintf(
    "Highest %s: %s with %s\n\n
    Lowest %s: %s with %s\n\n
    Average %s: %s", 
    type, state_max, round(choice_max, 2), type, state_min, round(choice_min, 2), type, choice_avg
  )
  info <- HTML(
    str_replace_all(
      HTML(
        info
      ),
      "\n\n",
      "<br/>"
    )
  )
  return(info)
}

# -------------------------------------------------------------------------------------


make_scatter <- function(choice, year) {
  df_scatter <- filter(df, YEAR == year)
  
  p <- ggplot(data=df_scatter, aes(x=avg_pm10, y=RATE)) + 
    xlab("Average PM10 Level") +
    ylab("Respiratory Death Rate") + 
    stat_cor(label.x = (max(df_scatter$avg_pm10, na.rm=TRUE)-(max(df_scatter$avg_pm10, na.rm=TRUE)-min(df_scatter$avg_pm10, na.rm=TRUE))/2)-2.5,
             label.y =73, p.accuracy = 0.001, r.accuracy = 0.01, na.rm=TRUE)
  
  if (choice != "avg_pm10") {
    if (choice == "avg_pm25") {
      p <- ggplot(data=df_scatter, aes(x=avg_pm25, y=RATE)) + 
        xlab("Average PM2.5 Level") +
        ylab("Respiratory Death Rate") +
        stat_cor(label.x = (max(df_scatter$avg_pm25, na.rm=TRUE)-(max(df_scatter$avg_pm25, na.rm=TRUE)-min(df_scatter$avg_pm25, na.rm=TRUE))/2)-0.25, 
                 label.y = 73, p.accuracy = 0.001, r.accuracy = 0.01, na.rm=TRUE)
    } else {
      p <- ggplot(data=df_scatter, aes(x=avg_no2, y=RATE)) + 
        xlab("Average NO2 Level") +
        ylab("Respiratory Death Rate") + 
        stat_cor(label.x = (max(df_scatter$avg_no2, na.rm=TRUE)-(max(df_scatter$avg_no2, na.rm=TRUE)-min(df_scatter$avg_no2, na.rm=TRUE))/2)-2.5, 
                 label.y = 73, p.accuracy = 0.001, r.accuracy = 0.01, na.rm=TRUE)
    }
  }
  
  p <- p + geom_smooth(method='lm', se= FALSE, na.rm=TRUE) +
    geom_point(aes(color = state_name), na.rm=TRUE) +
    geom_text_repel(aes(label = state_name), size = 3, na.rm=TRUE) +
    theme_classic() +
    theme(
          plot.background = element_rect(fill= '#ebf6fa'),
          panel.grid.minor.x = element_line(color='black', size=0.05),
          panel.grid.major.x = element_line(color='black', size=0.25),
          panel.grid.minor.y = element_line(color='black', size=0.05),
          panel.grid.major.y = element_line(color='black', size=0.25),
          panel.background = element_rect(fill = 'white'),
          legend.position = "none")
    
  
  return(p)
}


# -------------------------------------------------------------------------------------

# Exports final dataframe (DO NOT CHANGE!!!!!!!!!!!!)
write.csv(df, sprintf("%s/df.csv", getwd()), row.names=FALSE)
write.csv(avg_air_quality_df, sprintf("%s/avg_air_quality_df.csv", getwd()), row.names=FALSE)


# Description
  # top 5 highest death rate states
  # top 5 pm level states
    # see if they match
  # state with the most change is
# interactive map page
  # compare between states (contrast)
# scatter plot of deaths/pm over years page
  # compares over years (over time)