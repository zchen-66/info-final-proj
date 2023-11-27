library(dplyr)
library(stringr)
library(ggplot2)

df <- read.csv("AirQuality.csv")
air_df <- filter(df, country_name == "United States of America")
deaths_df <- read.csv("CountsOfDeaths.csv")