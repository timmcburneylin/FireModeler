# AUTO-GENERATED from R/generated/firemodel_purl.R
# Step 1: Clean SNAP
source('R/common.R')
cat('
Step 1: Clean SNAP
 starting...\n')

## ----firemodel_setup, include=FALSE-------------------------------------------
root <- normalizePath(getwd())

#' 
#' ---
#' title: "Fire Modeling Hat Creek RX"
#' output: html_document
#' date: "2024-03-06"
#' chunk_collapse: true
#' editor_options: 
#'   chunk_output_type: inline
#' ---
#' #Load Necessary packages
#' 
## -----------------------------------------------------------------------------
#install.packages(c("rspatial","DBI","RSQLite"))
#install.packages("rspatial")
library(plyr)
#library(rspatial)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(cffdrs)
library(dplyr)
library(magrittr)
library(XLConnect)
library(openair)
library(plyr)
library(ggplot2)
library(grid)
library(tidyr)
library(reshape2)
library(readxl)
library(testthat)
library(GA)
library(ftsa)
#install.packages("Rothermel")
library(Rothermel)
library(xlsx)
library(devtools)
library(firebehavioR)
library(stars)
library(geostats)
library(plantecophys)
library(progress)
library(ggstatsplot)
library(ggridges)
#install.packages("hrbrthemes")
#library(hrbrthemes)
library(ggdist)
library(MetBrewer)
library(progress)
library(DBI)
library(RSQLite)
library(glue)
library(rootSolve)
library(reshape2)
library(progress)
library(rlang)
library(ggstatsplot)
library(glue)
library(spatialEco)


#' 
#' Go to Snap and EXPORT the Overstory, Understory, Fuels, and Extra; then RESAVE as a csv:
#'   [Project_Name]_OS.csv 
#'   [Project_Name]_US.csv
#'   [Project_Name]_EXTRA.csv
#'   [Project_Name]_FUELS.csv 
#'   into the SNAP folder. 
#'   
#'   Get Cathro and EXPORT and then RESAVE it in your selected folder (/Dropoff and /Cut specs for run) as Cathro.xlsx (2007+)
#'   
#' #Set up Paths
## -----------------------------------------------------------------------------
path = file.path(root, "data", "raw")
out = path
name = "TR_LionsBurn"
project=name

# Prefix for SNAP summary file names
snap_prefix = "/SNAP/"
FWI_prefix = "/Weather/"
Fuel_prefix = "/FireBehavior/Inputs/"
cath_prefix = "/Cut Specs for run/"
Fire_out="/FireBehavior/Outputs/"
s_s_prefix = "/Stand_StockTables/"
out_slash = paste0(path, "/FuelCalcBC/Outputs/Slash/")
out_fuelcalc = paste0(path,"/FuelCalcBC/Outputs/")
fuelcalc = paste0(path,"/FuelCalcBC/")
out_weather = paste0("/Weather/")
out_residuals =paste0(path, "/FuelCalcBC/Outputs/Slash/Residuals/")
results = "/Outputs/"


# Load in SNAP summaries and cathro
Snap_OS = read.csv(paste0(path,snap_prefix,name,"_OS.csv"))
Snap_US = read.csv(paste0(path,snap_prefix,name,"_US.csv"))
Snap_EX= read.csv(paste0(path,snap_prefix,name,"_EXTRA.csv"))
Snap_fuels= read.csv(paste0(path,snap_prefix,name,"_FUELS.csv"))

# Specify the path to the Excel file
excel_file <- paste0(path, cath_prefix, "")

# set file path directory where your fcp files are located for all treatments
path_fcp <- paste0(path,"/FuelCalcBC/")

#' 
#' #Modifications:
## -----------------------------------------------------------------------------
#Load data
OSplots<-unique(Snap_OS$Plot..)
USplots<-unique(Snap_US$Plot..)
missing_plots <- setdiff(unique(Snap_OS$Plot..), unique(Snap_US$Plot..))


#Process Understory------------------------------------------------------------------
fake_US_data <- list()

# Add fake understory data for missing plots
for(pl in missing_plots){
  # Get overstory data for this plot
  OSdata <- Snap_OS %>% dplyr::filter(Plot.. == pl)
  
  # Find closest US plot by plot number
  plot_distances <- abs(USplots - pl)
  closest_US_plot <- USplots[which.min(plot_distances)]
  
  # Get understory data from closest plot
  USdata_template <- Snap_US %>% dplyr::filter(Plot.. == closest_US_plot)
  
  # Create fake understory data by copying template and changing plot number
  fake_USdata <- USdata_template %>%
    mutate(Plot.. = pl)
  
  OSSPP<-unique(OSdata$Spp)
  n_sppUS<-length(fake_USdata$SPP)
  
   if(n_sppUS <= length(OSSPP)){ #
    new_species <- sample(OSSPP, n_sppUS, replace = FALSE)
  } else { #
    new_species <- c(
      sample(OSSPP, length(OSSPP), replace = FALSE),  # All species once
      sample(OSSPP, n_sppUS - length(OSSPP), replace = TRUE)  # Random for remainder
    )
  }
  
  # Assign new species to fake data
  fake_USdata$Spp <- new_species
  
  
  # Store in list
  fake_US_data[[as.character(pl)]] <- fake_USdata
}

fake_US_combined <- bind_rows(fake_US_data)
Snap_US_complete <- bind_rows(Snap_US, fake_US_combined)

#CHECK
missing_plots <- setdiff(unique(Snap_OS$Plot..), unique(Snap_US_complete$Plot..))

#EXPORT
write.csv(Snap_US_complete,paste0(path,snap_prefix,name,"_US.csv"),row.names=FALSE)


#Process Overstory------------------------------------------------------------------
Snap_OS = read.csv(paste0(path,snap_prefix,name,"_OS.csv"))

missingcbh<-which(is.na(Snap_OS$CBH..0.1m.))
nonConiferList <- c("Act","Acb","Ac","At","Ep","DP", "DU","Dead", "Dr","Lt","Lw")


for(row in missingcbh){
  #pull row wiht missing cbh
  d<-Snap_OS[row,]
  
  #
  if(d$Spp %in% nonConiferList){
    Snap_OS[row,]$CBH..0.1m.<-Snap_OS[row,]$Total.Height..m.-1
  next
    }
  
  #find another tree with cloest values
  Species_Only<-Snap_OS[-row,] %>% dplyr::filter(Spp == d$Spp, !is.na(CBH..0.1m.))
  
  dbh_diff<-abs(Species_Only$DBH-d$DBH)
  height_diff<-abs(Species_Only$Total.Height..m.-d$Total.Height..m.)
  
  euclidean_dist <- sqrt(dbh_diff^2 + height_diff^2)
  closest_idx <- which.min(euclidean_dist)
  closest_tree <- Species_Only[closest_idx, ]  
  
  #change cbh
  newCBH<-closest_tree$CBH..0.1m.
  Snap_OS[row,]$CBH..0.1m.<-newCBH
  }

#EXPORT
write.csv(Snap_OS,paste0(path,snap_prefix,name,"_OS.csv"),row.names=FALSE)


#' 
#' #NOTES:
#' 
#' 
#' 
#' #Load Functions:
#'  Functions
## -----------------------------------------------------------------------------
source(file.path(root, "R_functions", "Find_Nearest_Cell_BC.R"))
source(file.path(root, "R_functions", "Improved_CFIS_perrakis_function.R"))
source(file.path(root, "R_functions", "Prob_Crown_Function_Perrakis2023.R"))
source(file.path(root, "R_functions", "fueltypes_crosswalkFBP_function.R"))
source(file.path(root, "R_functions", "fueltypes_crosswalk_function.R"))
source(file.path(root, "R_functions", "fueltypes_crosswalkFBP_Raster_function.R"))
source(file.path(root, "R_functions", "Crown_Area_Function.R"))
source(file.path(root, "R_functions", "Get_Season_Function.R"))
source(file.path(root, "R_functions", "FMC_Function.R"))
source(file.path(root, "R_functions", "SFC_Function.R"))
source(file.path(root, "R_functions", "Fine_Fuel_MC_SA_function.R"))
source(file.path(root, "R_functions", "initial_spread_index.R"))
source(file.path(root, "R_functions", "fire_behavior_prediction_function.R"))
source(file.path(root, "R_functions", "Flaming_Ign_Probability_Function.R"))
source(file.path(root, "R_functions", "Ignition_Probability_Function.R"))
source(file.path(root, "R_functions", "CanFuel_Raster_Function.R"))
source(file.path(root, "R_functions", "FMC_Raster_Function.R"))
source(file.path(root, "R_functions", "ISI_Raster_Function.R"))
source(file.path(root, "R_functions", "Flaming_Ign_Probability_Raster_Function.R"))
source(file.path(root, "R_functions", "FFMC_sa_Raster_Function.R"))
source(file.path(root, "R_functions", "CFIS_Modified_Raster_Function.R"))
source(file.path(root, "R_functions", "Fire_Behavior_Pred_Rast_cffdrs_function.R"))
source(file.path(root, "R_functions", "Residence_Time_Function_Nelson2003b.R"))
source(file.path(root, "R_functions", "surface_fuel_consumption_CANFBP_function.R"))
source(file.path(root, "R_functions", "WindGust_Function.R"))
source(file.path(root, "R_functions", "rothermel_function_mod.R"))
source(file.path(root, "R_functions", "Select_Best_Fuel_Model_Rothermel.R"))
source(file.path(root, "R_functions", "BC_TREECODES_USCodes_function.R"))
source(file.path(root, "R_functions", "CFC_Groot_Function.R"))

#' 
#'   Custom:
## -----------------------------------------------------------------------------
#functions
# Function to extract metric value
    extract_metric <- function(model, metric_name){
      if(metric_name == "CFC"){
        return(model$CFC)
      } else if(metric_name == "CFB"){
        return(model$CFB)
      } else if(metric_name == "Intensity_Crown"){
        return(model$Intensity_Crown)
      } else if(metric_name == "Intensity_Surface"){
        return(model$Intensity_Surface)
      } else if(metric_name == "ROS"){
        return(model$ROS)
      } else if(metric_name == "Flame_Length"){
        return(model$Flame_Length)
      } else if(metric_name == "pCrown"){
        return(model$pCrown)
      } else if(metric_name == "SFC"){
        return(model$SFC)
      } else if(metric_name == "FireFlag"){
        return(model$FLAG)}
      else if(metric_name == "CritFlameLength"){
        return(model$CritFlameLength)
        } else if(metric_name == "FuelConsumed_kg_m2"){
        return(model$FuelConsumed_kg_m2)
      } else {
        # Default to CFC if metric not recognized
        warning(paste("TargetMetric", metric_name, "not recognized. Defaulting to CFC."))
        return(model$CFC)
      }
    }

TargetMetricUpper = "pCrown" # Options: "CFC" (Crown Fraction Consumed), "CFB" (Crown Fraction Burned),"Intensity_Crown", "Intensity_Surface", "ROS", "Flame_Length", "pCrown", "SFC", "FireFlag", "CritFlameLength", "FuelConsumed_kg_m2

TargetMetricLower = "FuelConsumed_kg_m2" # Options: "CFC" (Crown Fraction Consumed), "CFB" (Crown Fraction Burned), "Intensity_Crown", "Intensity_Surface", "ROS", "Flame_Length", "pCrown", "SFC", "FireFlag", "CritFlameLength", "FuelConsumed_kg_m2



#rounding cutting specs to nearest multiple of 10 and setting it into fuelcalc format
round_to_nearest_10_custom <- function(x) {
  if (x == 95) {
    return(90)  # Special case for 95
  } else {
    return(ceiling(x / 10) * 10)  # Round up to nearest multiple of 10
  }
}

# Custom function to rename and merge columns for species
rename_and_merge_species <- function(cuts) {
  # Get the original species names (excluding DBH.Class)
  spp_original <- cuts %>% dplyr::select(-DBH.Class) %>% colnames()
  
  # Rename species names according to the provided pattern-replacement rules
  species <- spp_original
  species <- gsub("Ep", "Eb", species)
  species <- gsub("Act", "At", species)
  species <- gsub("Acb", "At", species)
  species <- gsub("Dead", "Eb", species)
  species <- gsub("Ac", "At", species)
  species <- gsub("DU", "Eb", species)
  species <- gsub("DP", "Eb", species)
  species <- gsub("Lt", "Lw", species) #change larch to western larch
  species <- gsub("Atb", "At", species)
  species <- gsub("Fdi", "Fd", species) #Fdi to Fd
  
  # Rename columns in the cuts data frame
  colnames(cuts)[2:ncol(cuts)] <- species  # The first column is DBH.Class, so skip it
  
# Remove duplicate columns based on their names (keep only the first occurrence)
  cuts <- cuts[, !duplicated(colnames(cuts))]  
  return(cuts)
}

is_within_range <- function(value, min_val, max_val) {
  value >= min_val & value <= max_val
}

#' 
#' #FuelType File AOI Slope and Aspect
#' 
## -----------------------------------------------------------------------------
AOI<-st_read(paste0(path,"/TR__LionsBurn_BOX.shp"))
ft_raw<-st_read(paste0(path,Fuel_prefix,"ft_raw.shp"))
dem<-rast(paste0(path,Fuel_prefix,"dem.tif"))
#RASTERIZE:
dem_disag<-disagg(dem,fact=100)
plot(dem_disag)
FT_RAST<-rasterize(vect(ft_raw), dem_disag, field="FT_PROME_1")
writeRaster(FT_RAST,paste0(path,Fuel_prefix,"FT_PROM.tif"))

#change fuels to FBP
ft_fbp<-fueltypes_crosswalkfbp_raster(FT_RAST)
plot(ft_fbp)

writeRaster(ft_fbp,paste0(path,Fuel_prefix,"FT_FBP.tif"))

#Calc slope and aspect
slope <- terrain(dem, v = "slope", unit = "degrees")
slope_percent <- tan(slope * pi/180) * 100

aspect <- terrain(dem, v = "aspect", unit = "degrees")
writeRaster(slope,paste0(path,Fuel_prefix,"Slope.tif"),overwrite=TRUE)
writeRaster(slope_percent,paste0(path,Fuel_prefix,"SlopePercent.tif"),overwrite=TRUE)

writeRaster(aspect,paste0(path,Fuel_prefix,"Aspect.tif"),overwrite=TRUE)
plot(dem_disag)
plot(slope)

unique(AOI)
#calc average slope
AOI$Stratum<-"A"

Units<-unique(AOI$Stratum)
for(U in Units){

  Unit<-AOI %>% dplyr::filter(Stratum==U)
slope_vals<-terra::extract(slope_percent,vect(Unit))
dem_vals<-terra::extract(dem_disag,vect(Unit))
dem_nona<-dem_vals[dem_vals>1]
slope_nona<-slope_vals[slope_vals > 1]
print(paste0("Mean Percent Slope Unit ",U," = ",round(mean(slope_nona,na.rm=TRUE),2),"%"))
print(paste0("Mean Elevation Unit ",U," = ",round(mean(dem_nona,na.rm=TRUE),2)," Meters"))
}


#' 
#' 
#' 
#' #1.Export Files from Snap: Overstory, Understory, and Extra
#'     
#'     #Label them as "Name"_OS.csv, "Name"_US.csv,"Name"_FUELS.csv and "Name"_EXTRA.csv in correct folder: ~/SNAP/
#'     #Go To Stand and Stock Tables Markdown document and follow instructions to run all
#' 
#'     

cat('Step 1: Clean SNAP complete.\n')
