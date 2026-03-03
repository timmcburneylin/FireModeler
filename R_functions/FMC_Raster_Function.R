FMC_Rast_calc<- function(LAT,LON,ELEV,DATE){
  
  #equation to calculate foliar moisture content; primed for BC forests from the ST-X-3 Canadian Fire Danger Group revision to CFDBS
  #all rasters
  #elevation in meters
  #latitude in degrees
  #longitude in degrees
  #date as "Year-Month-Day"
  
  
  #calculate latitude constant
  Lat.n<- 43 +33.7*exp(-0.0351*(150-LON))
  
  #calculate date of minimum foliar moisture content
  D_0<- 142.1*(LAT/Lat.n)+0.0172*ELEV
  #julian date
  #Date input formated as "Year-Month-Day" ie "2024-01-01"
  # Convert to POSIXlt object
  date_components <- as.POSIXlt(DATE)
  # Extract the day of the year (Julian date)
  D_J <- date_components$yday + 1
  #calculate difference between julian date and date of minimum foliar moisture content
  if(class(D_0) == "SpatRaster"){
    ND_rast<-D_J-D_0
  }else{  ND_rast<-rast(D_J-D_0)
}
  
  #calculate current foliar moisture content
  FMC_rast <- ifel(
    ND_rast < 30, 85 + 0.0189 * (ND_rast^2),
    ifel(
      ND_rast >= 30 & ND_rast < 50, 32.9 + 3.17 * ND_rast - 0.0288 * (ND_rast^2),
      120  # Else condition
    )
  )
  
  # Round the FMC values
  FMC_rast <- round(FMC_rast, 2)
  
  return(FMC_rast)
}
