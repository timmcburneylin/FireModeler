# Function to determine the season based on month and day
get_season <- function(date_str) {
  # Extract month and day from the date string
  parts <- unlist(strsplit(date_str, "-"))
  month <- as.integer(parts[2])
  day <- as.integer(parts[3])
  
  # Determine the season based on the month and day
  if ((month == 3 && day >= 1) || (month == 4 || month == 5) || (month == 5 && day < 15)) {
    return("Spring")
  } else if ((month == 5 && day >= 15) || (month == 6 || month == 7 || month == 8) || (month == 9 && day < 15)) {
    return("Summer")
  } else if ((month == 9 && day >= 15) || (month == 10 || month == 11) || (month == 11 && day < 15)) {
    return("Fall")
  } else {
    return("Winter")
  }
}