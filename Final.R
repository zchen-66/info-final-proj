# Import needed libraries
library(dplyr)
library(stringr)
library(ggplot2)

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

# Get rid of comma in numbers
df$"DEATHS" <- ifelse(str_detect(df$"DEATHS", ","), str_remove(df$"DEATHS", ","),df$"DEATHS")
df$"DEATHS" <- as.numeric(df$"DEATHS")



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
#   left y-axis: Respiratory Deaths
#   right y-axis: <air quality type> Level
#   line color / point shape should correspond to either respiratory deaths
#   or the air quality level
#   Title of the plot should read "Respiratory Deaths and <Air Quality> Level over time 
#     in <STATE>" where <STATE> is the state specified by the user. 
# your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 

state_over_time_linechart <- function(state, type){
  state_df <- filter(df, state_name == state)
  coeff <- mean(state_df[,type]) / mean(state_df[, "DEATHS"])
  
  if(type == "avg_pm10"){
    name <- "PM10"
  }
  if(type == "avg_pm25"){
    name <- "PM2.5"
  }
  if(type == "avg_no2"){
    name <- "NO2"
  }

  linechart <- ggplot(data = state_df, aes(x=YEAR)) +
    geom_line( aes(y=DEATHS, color = "Respiratory Deaths"), size = 1) +
    geom_point(aes(y=DEATHS, text=DEATHS), color = "blue", shape = 17, size = 2) +
    geom_line( aes(y=eval(parse(text=type)) / coeff, text=round(eval(parse(text=type)), 2), color = sprintf("%s Level", name)), size = 1) +
    geom_point(
      aes(y=eval(parse(text=type)) / coeff),
      color = "red", shape = 20, size = 2.5) +
    labs(x="Year", color = "Legend",
         title=sprintf(("Respiratory Deaths and %s Level over time in %s"), name, state)) +
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
      name = "Respiratory Deaths",

      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*coeff, name=sprintf("%s Level", name))
    )

  return(linechart)
}
#plot(state_over_time_linechart("Washington", "avg_pm25"))










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