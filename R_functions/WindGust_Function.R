#Function to Calculate Gust values based on Crosby and Chandler 1966 (2004 reprint)
#Takes input in mph or kmhr and calulates average or maximum gust based on hourly or daily average
library(ggplot2)
library(dplyr)

#Setup

#Create Curves

wind_kmh <- seq(1.6, 48.3, by = 1.6)

#Table Values (Table1)
one_min_kmh <- c(4.8, 8.0, 9.7, 12.9, 14.5, 16.1, 17.7, 19.3, 20.9, 22.5,
                 24.1, 27.4, 30.6, 32.2, 33.8, 35.4, 37.0, 38.6, 40.2,
                 41.8, 43.4, 45.1, 46.7, 48.3, 49.9, 51.5, 53.1, 54.7, 56.3, 56.3)

gust_avg_kmh <- c(9.7, 12.9, 17.7, 20.9, 24.1, 25.7, 27.4, 30.6, 32.2, 35.4,
                  37.0, 40.2, 46.7, 46.7, 48.3, 51.5, 51.5, 53.1, 54.7,
                  56.3, 58.0, 59.6, 61.2, 62.8, 64.4, 66.0, 69.2, 70.8, 72.4, 74.0)

gust_max_kmh <- c(14.5, 19.3, 24.1, 27.4, 29.0, 32.2, 35.4, 37.0, 38.6, 41.8,
                  43.5, 46.7, 51.5, 53.1, 56.3, 56.3, 58.0, 59.6, 62.8,
                  64.4, 67.6, 70.8, 75.6, 77.2, 80.5, 82.1, 84.1, 85.7, 86.9, 86.9)

#Fit Curves
one_min_spline <- splinefun(wind_kmh, one_min_kmh)
gust_avg_spline <- splinefun(wind_kmh, gust_avg_kmh)
gust_max_spline <- splinefun(wind_kmh, gust_max_kmh)

#Function to apply curves to input windspeed average
Calculate_Gust<- function(speed, units = c("kmh", "mph")) {
  units <- match.arg(units)
  
  if (units == "mph") {
    speed <- speed * 1.60934  # convert to km/h
  }
  
  # Apply splines
  one_min <- one_min_spline(speed)
  gust_avg <- gust_avg_spline(speed)
  gust_max <- gust_max_spline(speed)
  
  # Output in desired units
  if (units == "mph") {
    one_min <- one_min / 1.60934
    gust_avg <- gust_avg / 1.60934
    gust_max <- gust_max / 1.60934
  }
  
  # Return as data frame
  data.frame(
    InputSpeed = speed * ifelse(units == "mph", 1/1.60934, 1),
    Units = units,
    OneMinuteMax = one_min,
    GustAverage = gust_avg,
    GustMaximum = gust_max
  )
}

