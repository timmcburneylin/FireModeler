#Function to calculate tree volume per the National Volume Estimator Library
#Load libraries
library(dplyr)
if (requireNamespace("sf", quietly = TRUE)) library(sf)
if (requireNamespace("terra", quietly = TRUE)) library(terra)

#load volume library
dyn.load("C:/NVEL/64/vollib.dll")
source_fun("BCTreeCode_USFIANumber_Function.R")

# Function
calculate_tree_volume <- function(
    Species,
    DBH,
    Height,
    Region = 6,
    Forest = "01",
    District = "01",
    errflg = 0) {
  
  # Change DBH and Height to US increments
  DBHin <- DBH * 0.393701
  Heightft <- Height * 3.28084
  
  # Get FIA Species code
  SpeciesCode <- bc_to_nvel_species(Species)$nvel_species
  
  # Setup voleq
  voleq <- "          "  # 10 spaces instead of empty string
  
  # Get Vol EQs
  VOLEQ_list <- list()
  
  for(i in 1:length(SpeciesCode)) {
    # Get volume equation for given species
    VOLEQ <- .Fortran("getvoleq_r",
                      as.integer(Region),
                      as.character(Forest),
                      as.character(District),
                      as.integer(SpeciesCode[[i]]),
                      as.character(voleq),
                      as.integer(errflg))[[5]]
    
    VOLEQ_list[[i]] <- VOLEQ
  }
  
  # Calculate Volumes
  Volumes_List <- list()
  for(i in 1:length(Species)) {
    Volres <- .Fortran("vollib_r",
                       as.character(VOLEQ_list[[i]]),
                       as.integer(Region),
                       as.character(Forest),
                       as.character(District),
                       as.integer(SpeciesCode[[i]]),
                       as.double(DBHin[[i]]),
                       as.double(Heightft[[i]]),
                       as.double(0),      # Merch top primary
                       as.double(0),      # Merch top secondary
                       as.double(0),      # Height 1st product
                       as.double(0),      # Height 2nd product
                       as.double(0),      # Upper stem height 1
                       as.double(0),      # Upper stem diameter 1
                       as.double(1),      # Stump height (1 ft default)
                       as.integer(0),     # Form class
                       as.double(0),      # DBT/BH ratio
                       as.double(0),      # Bark thickness ratio
                       as.double(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
                       as.integer(0))[[18]]
    
    Volumes_List[[i]] <- Volres
  }
  
  # Volume export - FIXED THIS PART
  # Extract volumes from each element of the list
  VolOut_Gross <- sapply(Volumes_List, function(x) round(x[1] * 0.0283168, 3))
  VolOut_Net <- sapply(Volumes_List, function(x) round(x[4] * 0.0283168, 3))
  
  # Export
  result <- data.frame(
    Species = Species,
    VolumeGross_m3 = VolOut_Gross,
    VolumeNet_m3 = VolOut_Net,
    stringsAsFactors = FALSE
  )
  return(result)
}
