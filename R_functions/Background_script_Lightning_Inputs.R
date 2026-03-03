#Generateing lightining model inputs background script:
#load packages
if (requireNamespace("raster", quietly = TRUE)) library(raster)
if (requireNamespace("terra", quietly = TRUE)) library(terra)
library(solrad)
library(dplyr)
library(geostats)
library(BurnP3.HelpR)
library(solrad)
library(parallel)
library(doParallel)
library(lubridate)

source("Z:/Scripts/FronteraCodez/Helpful_little_scripts/SolRad_Raster_Function.R")

#------------------
# set file path directory
path = ""
out = path
name = ""
project= ""

#Prefixes 
spatial = "/Inputs/Raw/"
spatial_new="/Inputs/Processed/"
spatial_out = "/Outputs/"
in_weather = "/Weather/"
out_weather = "/Weather/Out/"
results="/Results/"
process= "/Tabular/"
configuration="Configuration/"
WindGrids="/WindGrids/"
spatialInputs<-paste0(path, "/Landscape/")
tabularInputs=paste0(path,"/Tabular/")
outputs="/Outputs/"


#------------------
DEM<-rast(paste0(spatialInputs,"DEM_WN_Final.tif"))
crs(DEM)<-"EPSG:2954"

#get aspect and slope
aspect<-terra::terrain(DEM,"aspect",unit="degrees")
slope<-terra::terrain(DEM,"slope",unit="degrees")
DEM<-raster(DEM)
#MASK
aspect<-mask(raster(aspect),DEM)
slope<-mask(raster(slope),DEM)
slope <- terra::resample(slope, DEM, method = "bilinear")

#Get Terrain Info: TPI, TRI, Flow_Dir
terrain_info<-tpi.bp3(rast(DEM),window_size = 5)
tpi<-terrain_info[[1]]
tri<-terrain_info[[2]]
flow_dir<-terrain_info[[3]]

#Solar Irradiation:iterate over weather list and calculate solar irradiation
FWL_dates<-read.csv(paste0(path,process,"ERA5Stations_FWList_dates.csv"))

#Run SolRad Raster function over all summer days you are using: Generally May 15th to Sept 15th
start_date <- as.POSIXct("2021-05-15 12:00:00", format="%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-09-15 12:00:00", format="%Y-%m-%d %H:%M:%S")

# Generate a sequence of dates
date_vector <- seq.POSIXt(from = start_date, 
                          to = end_date, 
                          by = "day")

# Format the dates as strings in the requested format
dates <- format(date_vector, "%Y-%m-%d %H:%M:%S")

Radiation<-SolRad_rast(DEM,dates,parrallel = FALSE, CRS = "2954")
plot(Radiation)

#Stack them all
Lightning_Stack<-stack(DEM,aspect,slope,raster(Radiation),raster(tpi),raster(tri))
plot(Lightning_Stack)

#Export
writeRaster(tpi,paste0(path,spatial_new,"TPI.tif"),overwrite=TRUE)
writeRaster(tri,paste0(path,spatial_new,"TRI.tif"),overwrite=TRUE)
writeRaster(flow_dir,paste0(path,spatial_new,"FlowDir.tif"),overwrite=TRUE)
writeRaster(slope,paste0(path,spatial_new,"Slope.tif"),overwrite=TRUE)
writeRaster(aspect,paste0(path,spatial_new,"Aspect.tif"),overwrite=TRUE)
writeRaster(Radiation,paste0(path,spatial_new,"SolarRad.tif"),overwrite=TRUE)
writeRaster(Lightning_Stack,filename=paste0(path,spatial_new,"Lightning_Ignition_Inputs.tif"))
