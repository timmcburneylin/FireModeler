#CanFuel Function from FireBehavioR adapted to Raster version

#Regression equations from Cruz,Alexander, and Wakimoto 2003

# Define the function to operate directly on raster objects
canFuel_raster <- function(ba_raster, ht_raster, tph_raster, type_raster) {
  #
  if(class(ba_raster)[1] != "SpatRaster"){
    ba_raster<-rast(ba_raster)
  }
  if(class(ht_raster)[1] != "SpatRaster"){
    ht_raster<-rast(ht_raster)
  }
  if(class(tph_raster)[1] != "SpatRaster"){
    tph_raster<-rast(tph_raster)
  }
  if(class(type_raster)[1] != "SpatRaster"){
    type_raster<-rast(type_raster)
  }
  
  
  # Calculate CFL using numeric values directly
  cfl_rast <- exp(
    ifel(type_raster == 1, -3.959 + 0.826 * log(ba_raster) + 0.175 * log(tph_raster),  # df
         ifel(type_raster == 2, -4.066 + 0.91 * log(ba_raster) + 0.13 * log(tph_raster),   # lp
              ifel(type_raster == 3, -4.824 + 0.804 * log(ba_raster) + 0.333 * log(tph_raster), # mc
                   ifel(type_raster == 4, -3.592 + 0.864 * log(ba_raster) + 0.11 * log(tph_raster), NA)))))  # pp
  
  # Calculate CBD
  cbd_rast <- exp(
    ifel(type_raster == 1, -7.38 + 0.479 * log(ba_raster) + 0.625 * log(tph_raster),   # df
         ifel(type_raster == 2, -7.852 + 0.349 * log(ba_raster) + 0.711 * log(tph_raster),  # lp
              ifel(type_raster == 3, -8.445 + 0.319 * log(ba_raster) + 0.859 * log(tph_raster),  # mc
                   ifel(type_raster == 4, -6.649 + 0.435 * log(ba_raster) + 0.579 * log(tph_raster), NA)))))  # pp
  
  # Calculate CBH
  cbh_rast <- ifel(type_raster == 1, -1.771 + 0.554 * ht_raster + 0.045 * ba_raster,  # df
                   ifel(type_raster == 2, -1.475 + 0.613 * ht_raster + 0.043 * ba_raster,  # lp
                        ifel(type_raster == 3, -1.463 + 0.578 * ht_raster + 0.026 * ba_raster,  # mc
                             ifel(type_raster == 4, 0.134 + 0.393 * ht_raster + 0.049 * ba_raster, NA))))  # pp
  
  #make cbh zero
  cbh_rast[cbh_rast<0]<-0
  
  # Round to 4 decimal places
  cfl_rast <- round(cfl_rast, 4)
  cbd_rast <- round(cbd_rast, 4)
  cbh_rast <- round(cbh_rast, 4)
  
  return(list(CFL = cfl_rast, CBD = cbd_rast, CBH = cbh_rast))
}
