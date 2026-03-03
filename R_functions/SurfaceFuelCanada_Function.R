#Code to predict Fine woody fuel, medium, woody fuel, coarse woody fuel, or All woody fuel in Canadian forests.

#From Hanes et al 2021:Dead and down woody debris fuel loads in Canadian forests
#Function varies by species, genera, and ecozone and can be changed depending on type of data you have.

#Load Packagaes
if (requireNamespace("raster", quietly = TRUE)) library(raster)
library(dplyr)
if (requireNamespace("terra", quietly = TRUE)) library(terra)
if (requireNamespace("firebehavioR", quietly = TRUE)) library(firebehavioR)

#Load Functions
source_fun("Crosswalk CAN to US Fuel Model Function.R")


#Test data
FuelType="C4"
Latitude=54
Longitude=-120
Lat=Latitude
Long=Longitude
Canadian_Fuel_Model=FuelType
Preset="Yes"

#Start:
fuel_prediction<- function(
    FuelType,
    LeadSpecies,
    SpeciesList,
    SpeciesPercent,
    Ecozone,
    Latitude,
    Longitude,
    Slope,
    Aspect,
){
  
  #Modify Fuel Type to fit------------------------
  #Remove Hyphen from model
  hyphen_cad_models<-c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7",
                       "D-1", "D-2", "M-1", "M-2", "M-3", "M-4", "S-1", "S-2", "S-3", "O-1b", "O-1a","NF", "WA")
  
  if(FuelType %in% hyphen_cad_models){
    FuelType<-gsub("-", "",FuelType)
  }
  
  # Define valid Canadian fuel models
  valid_canadian_models <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7",
                             "D1", "D2", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1b", "O1a","NF", "WA")
  
  # Validate FuelType input
  if (!FuelType %in% valid_canadian_models) {
    stop("Invalid Canadian Fuel Model:",FuelType ," Please provide one of the following: ", 
         paste(valid_canadian_models, collapse = ", "))
  }
  
  #Get best US Model:
  #My internal function comparing rate of spread curves
  BestUS_Model<- Best_US_Model(Canadian_Fuel_Model = FuelType, Preset = "Yes", Plot = "Yes", 
                           Lat = Latitude, Long = Longitude, Original13 = "No", Aspect=180,Slope=30)
  BestUS_Model$WeatherConditions
  #Rothermel function comparing ROS Curves
  
  #Create input fuel moisture data
  m_df<-data.frame(
    Hr1=rep(BestUS_Model$WeatherConditions$hr1,nrow(BestUS_Model$Results$ROS_Data)),
    Hr10=rep(BestUS_Model$WeatherConditions$hr10,nrow(BestUS_Model$Results$ROS_Data)),
    Hr100=rep(BestUS_Model$WeatherConditions$hr100,nrow(BestUS_Model$Results$ROS_Data)),
    Herb=rep(BestUS_Model$WeatherConditions$herblive,nrow(BestUS_Model$Results$ROS_Data)),
    Woody=rep(BestUS_Model$WeatherConditions$woodylive,nrow(BestUS_Model$Results$ROS_Data))
  )
  
  #Calculate MidFlame WindSpeeds from WAF
  
  u_df<-(BestUS_Model$Results$ROS_Data$WS*BestUS_Model$WeatherConditions$waf)
  
  #Get US Model from bestFM function
    US_Model<- bestFM(
      obs=BestUS_Model$Results$ROS_Data[,2],
      m=m_df,
      u=u_df,
      slope=rep(Slope,nrow(BestUS_Model$Results$ROS_Data))
    )
    
  
  
  
  
  
}

