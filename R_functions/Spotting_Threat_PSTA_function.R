#Load libraries
if (requireNamespace("terra", quietly = TRUE)) library(terra)
library(progress)

#Function to calculate the spotting values for threat calculation under the 2017 Provincial Strategtic Threat Analysis framework:
#https://www2.gov.bc.ca/assets/gov/public-safety-and-emergency-services/wildfire-status/prevention/fire-fuel-management/fuels-management/provincial_strategic_threat_analysis_2017_update.pdf

spotting_threat <- function(input_FT_surface) {
  
  # Check if input is spatraster from terra
  if(!inherits(input_FT_surface, "SpatRaster")) {
    ft <- rast(input_FT_surface)
    ft_raster <- raster(input_FT_surface)
  } else {
    ft <- input_FT_surface
    ft_raster <- raster(input_FT_surface)
  }
  
  
  
  # Set up fuel type categories based on prometheus code
  cat_1 <- c(1, 2, 4, 480, 485, 490, 495, 580, 585, 590, 595, 680, 685, 690, 695, 780, 785, 790, 795, 880, 885, 890, 895, 980, 985, 990, 995)
  cat_2 <- c(3, 7, 450, 455, 460, 465, 470, 475, 550, 555, 560, 565, 570, 575, 650, 655, 660, 665, 670, 675, 750, 755, 760, 765, 770, 775, 850, 855, 860, 865, 870, 875, 950, 955, 960, 965, 970, 975, 771, 555, 560, 565, 570, 575)
  cat_3 <- c(5, 6, 12, 32, 9, 10, 11, 430, 435, 440, 445, 530, 535, 540, 545, 630, 635, 640, 645, 730, 735, 740, 745, 830, 835, 840, 845, 930, 935, 940, 945, 444, 333, 222, 666)
  cat_4 <- c(8, 979, 969, 40, 405, 410, 415, 420, 425, 50, 505, 510, 515, 520, 525, 60, 605, 610, 615, 620, 625, 70, 705, 710, 715, 720, 725, 80, 805, 810, 815, 820, 825, 90, 905, 910, 915, 920, 925, 777, 888, 999)
  cat_5 <- c(99, 102, NaN)
  
  # Create distance band rasters
  # 2000m
  vals_2000m <- c(7, 5, 3, 0.1, 0)
  dist_2000m_band <- rbind(
    cbind(cat_1, vals_2000m[1]),
    cbind(cat_2, vals_2000m[2]),
    cbind(cat_3, vals_2000m[3]),
    cbind(cat_4, vals_2000m[4]),
    cbind(cat_5, vals_2000m[5])
  )
  ft_raster <- raster(ft)
  rast_2000m <- reclassify(ft_raster, dist_2000m_band)
  
  # 1000m
  vals_1000m <- c(40, 30, 25, 1, 0)
  dist_1000m_band <- rbind(
    cbind(cat_1, vals_1000m[1]),
    cbind(cat_2, vals_1000m[2]),
    cbind(cat_3, vals_1000m[3]),
    cbind(cat_4, vals_1000m[4]),
    cbind(cat_5, vals_1000m[5])
  )
  rast_1000m <- reclassify(ft_raster, dist_1000m_band)
  
  # 500m
  vals_500m <- c(400, 300, 250, 10, 0)
  dist_500m_band <- rbind(
    cbind(cat_1, vals_500m[1]),
    cbind(cat_2, vals_500m[2]),
    cbind(cat_3, vals_500m[3]),
    cbind(cat_4, vals_500m[4]),
    cbind(cat_5, vals_500m[5])
  )
  rast_500m <- reclassify(ft_raster, dist_500m_band)
  
  # Convert rasters to terra format for consistency
  rast_2000m_terra <- rast(rast_2000m)
  rast_1000m_terra <- rast(rast_1000m)
  rast_500m_terra <- rast(rast_500m)
  
  # Create empty result raster
  result_raster <- ft * 0
  
  # Process cell by cell (this is slower but more reliable than focal)
  cell_count <- ncell(ft)
  
  # Get non-NA cells to process
  valid_cells <- which(!is.na(values(ft)))
  
  #Setup progress bar
  total_iters <- cell_count
  pb <- progress_bar$new(
    format = "  running [:bar] :percent eta: :eta",
    total  = total_iters,
    clear  = FALSE,   
    width  = 60
  )
  #initialize
  iter <- 0
  
  # Calculate distance once, outside the loop
  for (i in valid_cells) {
    # Get focal cell coordinates
    focal_coords <- xyFromCell(ft, i)
    focal_point <- vect(focal_coords, crs=crs(ft))
    
    # Create a buffer around the focal point
    buffer_2000m <- terra::buffer(focal_point, width=2000)
    
    # Extract cells within buffer
    cells_in_buffer <- terra::extract(ft, buffer_2000m, cells=TRUE)
    
    if (nrow(cells_in_buffer) == 0) {
      # No cells in buffer
      result_raster[i] <- NA
      next
    }
    
    # Calculate distances from focal cell to each cell in buffer
    buffer_cells <- cells_in_buffer[,3]
    buffer_coords <- xyFromCell(ft, buffer_cells)
    distances <- sqrt((buffer_coords[,1] - focal_coords[1])^2 + 
                        (buffer_coords[,2] - focal_coords[2])^2)
    
    # Get values from reclassified rasters
    vals_2000m <- terra::extract(rast_2000m_terra, buffer_cells)[,1]
    vals_1000m <- terra::extract(rast_1000m_terra, buffer_cells)[,1]
    vals_500m <- terra::extract(rast_500m_terra, buffer_cells)[,1]
    
    # Assign weights based on distance
    weights <- ifelse(
      distances > 1000, vals_2000m,
      ifelse(
        distances > 500, vals_1000m,
        vals_500m
      )
    )
    
    # Calculate mean of weights
    result_raster[i] <- mean(weights, na.rm=TRUE)
    
    # Tick the progress bar:
    iter <- iter+1
    pb$tick()
    # Progress indicator (every 1000 cells)
    #if (i %% 1000 == 0) {
     # cat("Processed", i, "of", length(valid_cells), "cells\n")
    #}
    
  }
  
  return(result_raster)
}

