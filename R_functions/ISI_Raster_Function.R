if (requireNamespace("terra", quietly = TRUE)) library(terra)
#equation from:https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf
# Function to calculate ISI directly from raster inputs
initial_spread_index_raster <- function(ffmc_raster, ws_raster, fbpMod = FALSE) {
  
  if(class(ffmc_raster) !="SpatRaster"){
    ffmc_raster<-rast(ffmc_raster)
  }
  if(class(ws_raster) !="SpatRaster"){
    ws_raster<-rast(ws_raster)
  }
  
  # Eq. 10 - Moisture content
  fm_raster <- 147.2 * (101 - ffmc_raster) / (59.5 + ffmc_raster)
  
  # Eq. 24 - Wind Effect (vectorized using terra's ifel)
  fW_raster <- ifel(
    (ws_raster >= 40) & (fbpMod == TRUE),
    12 * (1 - exp(-0.0818 * (ws_raster - 28))),
    exp(0.05039 * ws_raster)
  )
  
  # Eq. 25 - Fine Fuel Moisture
  fF_raster <- 91.9 * exp(-0.1386 * fm_raster) * (1 + (fm_raster^5.31) / 49300000)
  
  # Eq. 26 - Spread Index Equation
  isi_raster <- 0.208 * fW_raster * fF_raster
  
  return(isi_raster)
}
