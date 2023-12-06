# Import needed libraries
library(dplyr)
library(stringr)
library(ggplot2)

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




# Description
  # top 5 highest death rate states
  # top 5 pm level states
    # see if they match
  # state with the most change is
# interactive map page
  # compare between states
# scatter plot of deaths/pm over years page
  # compares over years
# 