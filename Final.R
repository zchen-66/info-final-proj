library(dplyr)
library(stringr)
library(ggplot2)

air_df <- read.csv("AirQuality.csv")
air_df <- filter(air_df, country_name == "United States of America")
deaths_df <- read.csv("CountsOfDeaths.csv")