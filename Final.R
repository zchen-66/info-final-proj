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

state_df <- filter(df, state_name == "Washington")
DEATHS_linechart <- ggplot(data = state_df, aes(x=YEAR, y=DEATHS)) +
  geom_line() +
  labs(x="Year", y = "Respiratory Deaths",
       title="Respiratory Deaths over time in Washington")
PM25_linechart <- ggplot(data = state_df, aes(x=YEAR, y=avg_pm25)) +
  geom_line() +
  labs(x="Year", y = "Respiratory Deaths",
       title="Respiratory Deaths over time in Washington")














# Description
  # top 5 highest death rate states
  # top 5 pm level states
    # see if they match
  # state with the most change is
# interactive map page
  # compare between states (contrast)
# scatter plot of deaths/pm over years page
  # compares over years (over time)