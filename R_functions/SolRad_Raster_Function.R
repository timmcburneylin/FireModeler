#SolarRadiation Function from a raster stack

#Raster version fo the solarradiation function
#load packages
if (requireNamespace("terra", quietly = TRUE)) library(terra)
library(lubridate)


#' Calculate Solar Radiation Raster
#' @param Elevation DEM raster (SpatRaster or path to raster file)
#' @param Dates Vector of dates to calculate solar radiation for
#' @param sLon Standard longitude for time zone calculations (default -120 for PST)
#' @return Mean solar radiation raster
SolRad_rast <- function(Elevation, Dates, sLon=-120, CRS, parrallel=TRUE, n_cores=NULL, print_progress=TRUE) {
  
  # Load required libraries if not already loaded
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required")

  # Check for parallel libraries if parallel=TRUE
  if (parrallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) stop("Package 'parallel' is required for parallel processing")
    if (!requireNamespace("foreach", quietly = TRUE)) stop("Package 'foreach' is required for parallel processing")
    if (!requireNamespace("doParallel", quietly = TRUE)) stop("Package 'doParallel' is required for parallel processing")
    
    # Determine number of cores to use
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 3)
    }
    
    message(paste("Using parallel processing with", n_cores, "cores"))
  }
  
  DEM <- if(class(Elevation)[1] != "SpatRaster") terra::rast(Elevation) else Elevation
  
  #calculate slope and aspect
  SLOPE<-terra::terrain(DEM,"slope",unit="degrees")
  ASPECT<-terra::terrain(DEM,"aspect",unit="degrees")
  
  #Create latitude and longitude raster
  LAT <- terra::rast(DEM)
  LON <- terra::rast(DEM)
  
  Input_crs <- st_crs(CRS)
  # Define WGS84 CRS.
  wgs84_crs <- st_crs(4326)  # EPSG:4326
  

  coords <- terra::xyFromCell(DEM, 1:terra::ncell(DEM))
  pts <- terra::vect(coords, crs = Input_crs$wkt)
  pts_wgs84 <- terra::project(pts, wgs84_crs$wkt)
  
  coords_wgs84 <- terra::crds(pts_wgs84, df = TRUE)
  # Create new raster layers for LON and LAT by copying the DEM structure.
  LON <- DEM
  LAT <- DEM
  
  # Populate these rasters with the new coordinate values.
  terra::values(LON) <- coords_wgs84$x
  terra::values(LAT) <- coords_wgs84$y
  
  
  #Generate DOY values
  Dates <- format(as.Date(Dates), "%Y-%m-%d")
  DOY_vals <- as.numeric(format(as.Date(Dates), "%j")) + 0.5
  
  get_ds_vals <- function(dates) {
    sapply(dates, function(date) {
      is_dst <- dst(as.POSIXct(date, tz = "America/Vancouver"))
      ifelse(is_dst, 60, 0)  # 60 minutes if DST, 0 otherwise
    })
  }
  
  #DS Values
  DS_vals <- get_ds_vals(Dates)
  
  
  #Start Running Functions------------------------------------------------------
  # EOT (Equation of Time) function
  EOT <- function(DOY) {
    B <- (DOY - 81) * 360 / 365
    ET <- 9.87 * sin(pi/180 * 2 * B) - 7.53 * cos(pi/180 * B) - 1.5 * sin(pi/180 * B)
    return(ET)
  }
  
  # LST (Local Solar Time) function
  LST <- function(DOY) {
    lst <- (DOY * 24 * 60) %% (24 * 60)
    return(lst)
  }
  
  # AST (Apparent Solar Time) function
  AST <- function(DOY, Lon, SLon, DS) {
    eot <- EOT(DOY)
    lst <- LST(DOY)
    ast <- lst + eot + 4 * (SLon - Lon) - DS
    return(ast)
  }
  
  # Hour Angle function
  HourAngle <- function(DOY, Lon, SLon, DS) {
    ast <- AST(DOY, Lon, SLon, DS)
    H <- (ast - 12 * 60) / 4
    return(H)
  }
  
  # Declination function
  Declination <- function(DOY) {
    Delta <- 23.45 * sin(pi/180 * 360/365 * (284 + DOY))
    return(Delta)
  }
  
  # Solar Altitude function
  Altitude <- function(DOY, Lat, Lon, SLon, DS) {
    Delta <- Declination(DOY)
    H <- HourAngle(DOY, Lon, SLon, DS)
    A <- 180/pi * asin(sin(pi/180 * Lat) * sin(pi/180 * Delta) + 
                         cos(pi/180 * Lat) * cos(pi/180 * Delta) * cos(pi/180 * H))
    return(A)
  }
  
  # Transmittance function
  Transmittance <- function(DOY, Lat, Lon, SLon, DS, Elevation) {
    a0 <- 0.4237 - 0.00821 * (6 - Elevation/1000.0)^2
    a1 <- 0.5055 + 0.00595 * (6.5 - Elevation/1000.0)^2
    k <- 0.2711 + 0.01858 * (2.5 - Elevation/1000.0)^2
    Alpha <- Altitude(DOY, Lat, Lon, SLon, DS)
    
    tb <- (a0 + a1 * exp(-k/sin(pi/180 * Alpha))) * (Alpha > 0)
    return(tb)
  }
  
  # Extraterrestrial radiation function
  SolarConstant <- 1366.1
  Extraterrestrial <- function(DOY) {
    Sextr <- SolarConstant * (1 + 0.033 * cos(pi/180 * 360 * DOY/365))
    return(Sextr)
  }
  
  # Open radiation function
  OpenRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation) {
    tb <- Transmittance(DOY, Lat, Lon, SLon, DS, Elevation)
    Sextr <- Extraterrestrial(DOY)
    
    Sopen <- tb * Sextr
    return(Sopen)
  }
  
  # Incidence function
  Incidence <- function(DOY, Lat, Lon, SLon, DS, Slope, Aspect) {
    Delta <- Declination(DOY)
    H <- HourAngle(DOY, Lon, SLon, DS)
    
    theta <- 180/pi * acos(sin(pi/180 * Lat) * sin(pi/180 * Delta) * cos(pi/180 * Slope) -
                             cos(pi/180 * Lat) * sin(pi/180 * Delta) * sin(pi/180 * Slope) * cos(pi/180 * Aspect) +
                             cos(pi/180 * Lat) * cos(pi/180 * Delta) * cos(pi/180 * H) * cos(pi/180 * Slope) +
                             sin(pi/180 * Lat) * cos(pi/180 * Delta) * cos(pi/180 * H) * sin(pi/180 * Slope) * cos(pi/180 * Aspect) +
                             cos(pi/180 * Delta) * sin(pi/180 * H) * sin(pi/180 * Slope) * sin(pi/180 * Aspect))
    return(theta)
  }
  
  # Direct radiation function
  DirectRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation, Slope, Aspect) {
    Theta <- Incidence(DOY, Lat, Lon, SLon, DS, Slope, Aspect)
    Sopen <- OpenRadiation(DOY, Lat, Lon, SLon, DS, Elevation)
    Sdiropen <- Sopen * cos(pi/180 * Theta)
    # Set negative values to zero (no radiation when sun is below horizon)
    Sdiropen[Sdiropen < 0] <- 0
    return(Sdiropen)
  }
  
  # Total number of dates to process
  total_dates <- length(Dates)
  
  # Process dates in parallel or sequentially
  if (parrallel) {
    # Set up parallel backend
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    
    # Export required objects to the cluster
    parallel::clusterExport(cl, varlist = c("LAT", "LON", "DEM", "SLOPE", "ASPECT", "sLon", 
                                            "DOY_vals", "DS_vals", "DirectRadiation", "Incidence", 
                                            "OpenRadiation", "Transmittance", "Altitude", 
                                            "Declination", "HourAngle", "AST", "LST", "EOT", 
                                            "Extraterrestrial", "SolarConstant", "print_progress"), 
                            envir = environment())
    
    # Create a shared progress tracker
    progress_counter <- 0
    
    # Process each date in parallel with progress tracking
    radiation_rasters <- parallel::parLapply(cl, 1:length(Dates), function(i) {
      # Calculate direct radiation for current date
      DR_rast <- DirectRadiation(
        DOY = DOY_vals[i],
        Lat = LAT,
        Lon = LON,
        SLon = sLon,
        DS = DS_vals[i],
        Elevation = DEM,
        Slope = SLOPE,
        Aspect = ASPECT
      )
      
      # Note: In parallel mode, we can't reliably track progress this way
      # Progress will be handled after parallel processing completes
      
      return(DR_rast)
    })
    
    # Clean up parallel backend
    parallel::stopCluster(cl)
    
    # Print completion message for parallel processing
    if (print_progress) {
      message(paste("Processed all", total_dates, "dates with parallel processing"))
    }
    
  } else {
    # Sequential processing with progress tracking
    radiation_rasters <- list()
    
    for (i in 1:length(Dates)) {
      # Calculate direct radiation for current date
      DR_rast <- DirectRadiation(
        DOY = DOY_vals[i],
        Lat = LAT,
        Lon = LON,
        SLon = sLon,
        DS = DS_vals[i],
        Elevation = DEM,
        Slope = SLOPE,
        Aspect = ASPECT
      )
      
      radiation_rasters[[i]] <- DR_rast
      
      # Print progress message every 100 dates if requested
      if (print_progress && i %% 10 == 0) {
        message(paste(i, "dates processed"))
      }
    }
    
    # Print final progress message
    if (print_progress && total_dates > 0) {
      message(paste("All", total_dates, "dates processed"))
    }
  }
  
  # Calculate mean solar radiation across all dates
  if (length(radiation_rasters) > 1) {
    mean_radiation <- terra::app(terra::rast(radiation_rasters), fun = mean)
  } else {
    mean_radiation <- radiation_rasters[[1]]
  }
  
  # Set proper name
  names(mean_radiation) <- "mean_solar_radiation"
  
  return(mean_radiation)
}

