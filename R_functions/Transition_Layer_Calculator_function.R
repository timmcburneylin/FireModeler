#Function to calculate transition layers:

  #Load Libraries:
library(gdistance)
if (requireNamespace("raster", quietly = TRUE)) library(raster)
if (requireNamespace("sf", quietly = TRUE)) library(sf)
if (requireNamespace("terra", quietly = TRUE)) library(terra)
#Load Functions:
source_fun("Transition_Function_Modified.R")


#setup function:
Transition_Cost_Calculator<-function(
  Input_Path,
  Output_Path,
  Box,
  AOI,
  Focus_Points,
  Fireshed,
  Ignitions,
  DEM,
  ROS,
  BP,
  n_cores= NULL,
  Preset_CRS_String,
  TransitionType
){
  #Set up CRS
  if(is.null(Preset_CRS_String)){
  Preset_CRS_String<-"+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
  }
  
  crs(DEM)<- CRS(Preset_CRS_String)
  crs(BP)<- CRS(Preset_CRS_String)
  crs(ROS)<-CRS(Preset_CRS_String)
  template<-DEM
  #Process focus points
  targets<-Focus_Points
  targets$target<-1
  targets<- targets %>% dplyr::select(target,geometry)
  targets_sp <- as_Spatial(targets)
  
  #Process and create seeded ignitions layer
  ignitions_within_AOI <- st_intersects(Ignitions, AOI, sparse = FALSE)
  Ignitions_excluded <- Ignitions[!apply(ignitions_within_AOI, 1, any), ]
  Ignitions_Historical<- st_as_sf(Ignitions_excluded)
  
  #create ignitions modeled for every cell in fireshed
  Fireshed_rast<-rasterize(Fireshed,template, method="first")
  Fireshed_points <- rasterToPoints(Fireshed_rast, spatial = TRUE)
  Ignitions_modeled <- st_as_sf(Fireshed_points)
  Ignitions_modeled_within_AOI <- st_intersects(Ignitions_modeled, AOI, sparse = FALSE)
  Ignitions_Seeded <- Ignitions_modeled[!apply(Ignitions_modeled_within_AOI, 1, any), ]
  Ignitions_Historical_sp<-as_Spatial(Ignitions_Historical)
  Ignitions_Seeded_sp<-as_Spatial(Ignitions_Seeded)
  st_write(Ignitions_Seeded,paste0(Output_Path,"Seeded_Ignitions_",name,".shp"),append = FALSE)
  
  #Process DEM, BP, and ROS
  DEM<-mask(DEM,Fireshed)
  
  #Create rasters with no NAs for ROS, BP, and Firshed
  fireshed_pts<-as.data.frame(rasterToPoints(Fireshed_rast))
  #ROS
  pts_ROS<-as.data.frame(rasterToPoints(ROS))
  colnames(pts_ROS)[3]<-"ROS"
  #BP
  pts_BP<-as.data.frame(rasterToPoints(BP))
  colnames(pts_BP)[3]<-"BP"
  #ROS
  ROS<-rasterFromXYZ(pts_ROS %>% dplyr::select(x,y,"ROS"), res=res(Fireshed_rast), crs=crs(Fireshed_rast))
  crs(ROS) <- CRS(Preset_CRS_String)
  #BP
  BP<-rasterFromXYZ(pts_BP %>% dplyr::select(x,y,"BP"), res=res(Fireshed_rast), crs=crs(Fireshed_rast))
  crs(BP) <- CRS(Preset_CRS_String)
  
  #DEM
  pts_dem<-as.data.frame(rasterToPoints(DEM))
  colnames(pts_dem)[3]<-"DEM"
  DEM_new<-rasterFromXYZ(pts_dem %>% dplyr::select(x,y,"DEM"), res=res(ROS), crs=crs(ROS))
  crs(DEM_new) <- CRS(Preset_CRS_String)
  DEM_final<-raster::resample(DEM_new,ROS,method="bilinear")
  
  #Generate Transition surfaces from BP or ROS
  ROS[is.na(ROS)]<-0
  BP[is.na(BP)]<-0
  
  
  if(!is.null(n_cores)){
    # Set up parallel backend
    numCores <- n_cores
    cl <- makeCluster(numCores)
    registerDoParallel(cl)
    
    if(TransitionType== "ROS"){
      TR<- transition_mod(x=ROS_90th,
                               transitionFunction = function(x){mean(x,na.rm=T)},
                               directions = 8,
                               dem=DEM_final,
                               Use.Slope=TRUE,
                               symm = FALSE)
      Type="ROS"
    }
    if(TransitionType== "BP"){
      TR <- transition_mod(x=BP,
                              transitionFunction = function(x){mean(x,na.rm=T)},
                              directions = 8,
                              dem=DEM_final,
                              Use.Slope=TRUE,
                              symm = FALSE)
      Type="BP"
    }
    stopCluster(cl)
    
  }else{
    
    if(TransitionType== "ROS"){
      TR<- transition_mod(x=ROS_90th,
                          transitionFunction = function(x){mean(x,na.rm=T)},
                          directions = 8,
                          dem=DEM_final,
                          Use.Slope=TRUE,
                          symm = FALSE)
      Type="ROS"
    }
    if(TransitionType== "BP"){
      TR <- transition_mod(x=BP,
                           transitionFunction = function(x){mean(x,na.rm=T)},
                           directions = 8,
                           dem=DEM_final,
                           Use.Slope=TRUE,
                           symm = FALSE)
      Type="BP"
    }
    
    
  }
  
  #Geocorrect
  TR_gc<-geoCorrection(TR, type="c")
  
  #Export Transition
  saveRDS(TR_gc,paste0(Output_Path,Type,"_TransitionOBJ_",name,".rds"))
  TR_gc_r<-gdistance::raster(TR_gc)
  crs(TR_gc_r) <- CRS(Preset_CRS_String)
  writeRaster(TR_gc_r,paste0(Output_Path,Type,"_TransitionRaster_",name,".tif"),overwrite=TRUE)
  
  
  #Calculate Accumulated surface and export
  AccSurf<-accCost(TR_gc,targets_sp)
  crs(AccSurf) <- CRS(Preset_CRS_String)
  writeRaster(AccSurf,paste0(Output_Path,Type,"_CostSurf_",name,".tif"),overwrite=TRUE)
  
  #Calculate Accumulated Path for mutiple focus points
  ACC_layers<-list()
  for(x in 1:nrow(targets_sp)){
    pnt<-targets_sp[x,]
    #pnt_reprojected <- spTransform(pnt, crs(tr_90th_gc))
    crs(TR_gc)<-CRS(Preset_CRS_String)
    ACC<-accCost(TR_gc,pnt)
    crs(ACC) <- CRS(Preset_CRS_String)
    ACC_layers[[x]]<-ACC
    #Export costs
    writeRaster(ACC,paste0(Output_Path,Type,"_AccCost_Point",x,"_",name,".tif"),overwrite=TRUE)
  }
  
  return(list(
    TransitionLayer=TR_gc,
    AccumulatedCostLayers=ACC_layers
  ))
  
}
