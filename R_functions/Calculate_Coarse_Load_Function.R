#Function to calculate KF.m^2 for CWD and LWD
#length in meters
#width in meters
#decay class

#produces pieces, and load per piece

#function
Calculate_coarse_load <- function(length, width) {
  
  # Process
  length_m <- length
  diameter_cm <- width
  
  # Coefficients from log-log model
  a <- exp(-5.20558604)   # intercept after exp transformation
  b <- 2.22000037         # length exponent
  c <- 0.88533566         # diameter exponent (CORRECTED)
  d <- 0.01574038         # interaction term (CORRECTED)
  
  fuel_load <- a * length_m^b * diameter_cm^c * exp(d * log(length_m) * log(diameter_cm))
  
  return(pmax(fuel_load, 0))
}

  