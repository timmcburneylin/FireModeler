#Function to Find nearest cell center based on a vector of latitude and longitutdes
library(geosphere)
find_nearest <- function(station_lats, station_lons, weather_df) {
  # Initialize a data frame to store the results
  results_df <- data.frame(station_lat = station_lats, 
                           station_lon = station_lons, 
                           closest_id = numeric(length(station_lats)),
                           stringsAsFactors = FALSE)
  
  # Iterate over each station coordinate
  for (i in seq_along(station_lats)) {
    # Calculate distances from the current station to all weather data points
    distances <- geosphere::distm(x = matrix(c(station_lons[i], station_lats[i]), ncol = 2, byrow = TRUE),
                       y = as.matrix(weather_df[, c("long", "lat")]),
                       fun = distHaversine)
    
    # Find the index of the closest weather data point
    closest_index <- which.min(distances)
    
    # Store the ID of the closest weather data point in the results dataframe
    results_df$closest_id[i] <- closest_index # Assuming 'station_id' is the identifier in weather_df
  }
  
  return(results_df)
}