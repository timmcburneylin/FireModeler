#Modified FWI raster function from CFFRDS with adjusted sections
fwiRaster_mod <- function(
    input,
    init = c(ffmc = 85, dmc = 6, dc = 15),
    mon = 7,
    out = "all",
    lat.adjust = TRUE,
    uppercase = TRUE) {
  #Load required functions-------------------------
  {
  #BUI
  buildup_index <- function(dmc, dc) {
    # Eq. 27a
    bui1 <- ifelse(dmc == 0 & dc == 0, 0, 0.8 * dc * dmc / (dmc + 0.4 * dc))
    # Eq. 27b - next 3 lines
    p <- ifelse(dmc == 0, 0, (dmc - bui1) / dmc)
    cc <- 0.92 + ((0.0114 * dmc)^1.7)
    bui0 <- dmc - cc * p
    # Constraints
    bui0 <- ifelse(bui0 < 0, 0, bui0)
    bui1 <- ifelse(bui1 < dmc, bui0, bui1)
    return(bui1)
  }
  
  .buiCalc <- function(...) {
    .Deprecated("buildup_index")
    return(buildup_index(...))
  }
  #FFMC:
  FFMC_COEFFICIENT<-147.2
  fine_fuel_moisture_code <- function(ffmc_yda, temp, rh, ws, prec) {
    # Eq. 1
    wmo <- FFMC_COEFFICIENT * (101 - ffmc_yda) / (59.5 + ffmc_yda)
    # Eq. 2 Rain reduction to allow for loss in
    #  overhead canopy
    ra <- ifelse(prec > 0.5, prec - 0.5, prec)
    # Eqs. 3a & 3b
    wmo <- ifelse(
      prec > 0.5,
      ifelse(
        wmo > 150,
        (wmo + 0.0015 * (wmo - 150) * (wmo - 150) * sqrt(ra)
         + 42.5 * ra * exp(-100 / (251 - wmo)) * (1 - exp(-6.93 / ra))),
        wmo + 42.5 * ra * exp(-100 / (251 - wmo)) * (1 - exp(-6.93 / ra))
      ),
      wmo
    )
    # The real moisture content of pine litter ranges up to about 250 percent,
    # so we cap it at 250
    wmo <- ifelse(wmo > 250, 250, wmo)
    # Eq. 4 Equilibrium moisture content from drying
    ed <- (0.942 * (rh^0.679) + (11 * exp((rh - 100) / 10))
           + 0.18 * (21.1 - temp) * (1 - 1 / exp(rh * 0.115)))
    # Eq. 5 Equilibrium moisture content from wetting
    ew <- (0.618 * (rh^0.753) + (10 * exp((rh - 100) / 10))
           + 0.18 * (21.1 - temp) * (1 - 1 / exp(rh * 0.115)))
    # Eq. 6a (ko) Log drying rate at the normal temperature of 21.1 C
    z <- ifelse(
      wmo < ed & wmo < ew,
      (0.424 * (1 - (((100 - rh) / 100)^1.7))
       + 0.0694 * sqrt(ws) * (1 - ((100 - rh) / 100)^8)),
      0
    )
    # Eq. 6b Affect of temperature on  drying rate
    x <- z * 0.581 * exp(0.0365 * temp)
    # Eq. 8
    wm <- ifelse(wmo < ed & wmo < ew, ew - (ew - wmo) / (10^x), wmo)
    # Eq. 7a (ko) Log wetting rate at the normal temperature of 21.1 C
    z <- ifelse(
      wmo > ed,
      (0.424 * (1 - (rh / 100)^1.7)
       + 0.0694 * sqrt(ws) * (1 - (rh / 100)^8)),
      z
    )
    # Eq. 7b Affect of temperature on  wetting rate
    x <- z * 0.581 * exp(0.0365 * temp)
    # Eq. 9
    wm <- ifelse(wmo > ed, ed + (wmo - ed) / (10^x), wm)
    # Eq. 10 Final ffmc calculation
    ffmc1 <- (59.5 * (250 - wm)) / (FFMC_COEFFICIENT + wm)
    # Constraints
    ffmc1 <- ifelse(ffmc1 > 101, 101, ffmc1)
    ffmc1 <- ifelse(ffmc1 < 0, 0, ffmc1)
    return(ffmc1)
  }
  
  .ffmcCalc <- function(...) {
    .Deprecated("fine_fuel_moisture_code")
    return(fine_fuel_moisture_code(...))
  }
  #DC
  drought_code <- function(
    dc_yda, temp, rh, prec, lat, mon,
    lat.adjust = TRUE) {
    # Day length factor for DC Calculations
    # 20N: North of 20 degrees N
    fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
    # 20S: South of 20 degrees S
    fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
    # Near the equator, we just use 1.4 for all months.
    # Constrain temperature
    temp <- ifelse(temp < (-2.8), -2.8, temp)
    
    # Eq. 22 - Potential Evapotranspiration
    pe <- (0.36 * (temp + 2.8) + fl01[mon]) / 2
    # Daylength factor adjustment by latitude for Potential Evapotranspiration
    if (lat.adjust) {
      pe <- ifelse(lat <= -20, (0.36 * (temp + 2.8) + fl02[mon]) / 2, pe)
      pe <- ifelse(lat > -20 & lat <= 20, (0.36 * (temp + 2.8) + 1.4) / 2, pe)
    }
    # Cap potential evapotranspiration at 0 for negative winter DC values
    pe <- ifelse(pe < 0, 0, pe)
    ra <- prec
    # Eq. 18 - Effective Rainfall
    rw <- 0.83 * ra - 1.27
    # Eq. 19
    smi <- 800 * exp(-1 * dc_yda / 400)
    # Alteration to Eq. 21
    dr0 <- dc_yda - 400 * log(1 + 3.937 * rw / smi)
    dr0 <- ifelse(dr0 < 0, 0, dr0)
    # if precip is less than 2.8 then use yesterday's DC
    dr <- ifelse(prec <= 2.8, dc_yda, dr0)
    # Alteration to Eq. 23
    dc1 <- dr + pe
    dc1 <- ifelse(dc1 < 0, 0, dc1)
    return(dc1)
  }
  
  .dcCalc <- function(...) {
    .Deprecated("drought_code")
    return(drought_code(...))
  }
  
  #DMC
  duff_moisture_code <- function(
    dmc_yda, temp, rh, prec, lat, mon,
    lat.adjust = TRUE) {
    # Reference latitude for DMC day length adjustment
    # 46N: Canadian standard, latitude >= 30N   (Van Wagner 1987)
    ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
    # 20N: For 30 > latitude >= 10
    ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1, 8.6, 8.1, 7.8)
    # 20S: For -10 > latitude >= -30
    ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
    # 40S: For -30 > latitude
    ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
    # For latitude near the equator, we simple use a factor of 9 for all months
    # constrain low end of temperature
    temp <- ifelse(temp < (-1.1), -1.1, temp)
    # Eq. 16 - The log drying rate
    rk <- 1.894 * (temp + 1.1) * (100 - rh) * ell01[mon] * 1e-04
    # Adjust the day length and thus the drying r, based on latitude and month
    if (lat.adjust) {
      rk <- ifelse(
        lat <= 30 & lat > 10,
        1.894 * (temp + 1.1) * (100 - rh) * ell02[mon] * 1e-04,
        rk
      )
      rk <- ifelse(
        lat <= -10 & lat > -30,
        1.894 * (temp + 1.1) * (100 - rh) * ell03[mon] * 1e-04,
        rk
      )
      rk <- ifelse(
        lat <= -30 & lat >= -90,
        1.894 * (temp + 1.1) * (100 - rh) * ell04[mon] * 1e-04,
        rk
      )
      rk <- ifelse(
        lat <= 10 & lat > -10,
        1.894 * (temp + 1.1) * (100 - rh) * 9 * 1e-04,
        rk
      )
    }
    # Constrain P
    pr <- ifelse(
      prec <= 1.5,
      dmc_yda,
      {
        ra <- prec
        # Eq. 11 - Net rain amount
        rw <- 0.92 * ra - 1.27
        # Alteration to Eq. 12 to calculate more accurately
        wmi <- 20 + 280 / exp(0.023 * dmc_yda)
        # Eqs. 13a, 13b, 13c
        b <- ifelse(
          dmc_yda <= 33,
          100 / (0.5 + 0.3 * dmc_yda),
          ifelse(
            dmc_yda <= 65,
            14 - 1.3 * log(dmc_yda),
            6.2 * log(dmc_yda) - 17.2
          )
        )
        # Eq. 14 - Moisture content after rain
        wmr <- wmi + 1000 * rw / (48.77 + b * rw)
        # Alteration to Eq. 15 to calculate more accurately
        43.43 * (5.6348 - log(wmr - 20))
      }
    )
    pr <- ifelse(pr < 0, 0, pr)
    # Calculate final P (DMC)
    dmc1 <- pr + rk
    dmc1 <- ifelse(dmc1 < 0, 0, dmc1)
    return(dmc1)
  }
  
  .dmcCalc <- function(...) {
    .Deprecated("duff_moisture_code")
    return(duff_moisture_code(...))
  }
  #ISI
  FFMC_COEFFICIENT<-147.2
  initial_spread_index <- function(
    ffmc,
    ws,
    fbpMod = FALSE) {
    # Eq. 10 - Moisture content
    fm <- FFMC_COEFFICIENT * (101 - ffmc) / (59.5 + ffmc)
    # Eq. 24 - Wind Effect
    # the ifelse, also takes care of the ISI modification for the fbp functions
    # This modification is Equation 53a in FCFDG (1992)
    fW <- ifelse(
      ws >= 40 & fbpMod == TRUE,
      12 * (1 - exp(-0.0818 * (ws - 28))),
      exp(0.05039 * ws)
    )
    # Eq. 25 - Fine Fuel Moisture
    fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31) / 49300000)
    # Eq. 26 - Spread Index Equation
    isi <- 0.208 * fW * fF
    return(isi)
  }
  
  .ISIcalc <- function(...) {
    .Deprecated("initial_spread_index")
    return(initial_spread_index(...))
  }
  #Fire weather index
  fire_weather_index <- function(isi, bui) {
    # Eqs. 28b, 28a, 29
    bb <- ifelse(
      bui > 80,
      0.1 * isi * (1000 / (25 + 108.64 / exp(0.023 * bui))),
      0.1 * isi * (0.626 * (bui^0.809) + 2)
    )
    # Eqs. 30b, 30a
    fwi <- ifelse(bb <= 1, bb, exp(2.72 * ((0.434 * log(bb))^0.647)))
    return(fwi)
  }
  
  .fwiCalc <- function(...) {
    .Deprecated("fire_weather_index")
    return(fire_weather_index(...))
  }
  }
  
  #Function Starts----------------------------------------------------------------------------
  # due to NSE notes in R CMD check
  short = full = NULL
  # Reference latitude for DMC day length adjustment
  # 46N: Canadian standard, latitude >= 30N   (Van Wagner 1987)
  ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  # 20N: For 30 > latitude >= 10
  ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1, 8.6, 8.1, 7.8)
  # 20S: For -10 > latitude >= -30
  ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
  # 40S: For -30 > latitude
  ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
  # For latitude near the equator, we simple use a factor of 9 for all months
  
  # Day length factor for DC Calculations
  # 20N: North of 20 degrees N
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  # 20S: South of 20 degrees S
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
  # Near the equator, we just use 1.4 for all months.
  
  # Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  if (!is(input,"SpatRaster")) {
    input <- terra::rast(input)
  }
  names(input) <- tolower(names(input))
  had_latitude <- "lat" %in% names(input)
  if (!had_latitude) {
    # FIX: use actual latitude from raster
    # input[["lat"]] <- terra::init(input[["temp"]], "y")
    # use old default value
    input[["lat"]] <- terra::init(input[["temp"]], 55)
  }
  
  
  required_cols <- data.table(
    full = c("temperature", "precipitation", "wind speed", "relative humidity"),
    short = c("temp", "prec", "ws", "rh")
  )
  #Changed section---------------------------------
  missing_vars <- required_cols[!short %in% names(input), full]
  if (length(missing_vars) > 0) {
    stop(paste(missing_vars, collapse = " , "), " is missing!")
  }
  
  #if (nrow(required_cols[-which(names(input) %in% short)]) > 0) {
  #  stop(
   #   paste(
  #      required_cols[-which(names(input) %in% short), full],
  #      collapse = " , "
  #    ),
  #    " is missing!"
  #  )
  #}
  #-----------------------------------------------
  
  if (!length(input[["prec"]][input[["prec"]] < 0]) == 0) {
    stop("precipiation (prec) cannot be negative!")
  }
  if (!length(input[["ws"]][input[["ws"]] < 0]) == 0) {
    stop("wind speed (ws) cannot be negative!")
  }
  if (!length(input[["rh"]][input[["rh"]] < 0]) == 0) {
    stop("relative humidity (rh) cannot be negative!")
  }
  
  names(init) <- tolower(names(init))
  
  # Assign values for initializing variables
  if (is.numeric(init)) {
    if (is.null(names(init))) {
      names(init) <- c("ffmc", "dmc", "dc")
    }
    ffmc_yda <- dmc_yda <- dc_yda <- input[["temp"]]
    terra::values(ffmc_yda) <- init[["ffmc"]]
    names(ffmc_yda) <- "ffmc_yda"
    terra::values(dmc_yda) <- init[["dmc"]]
    terra::values(dc_yda) <- init[["dc"]]
  } else {
    ffmc_yda <- init$ffmc
    dmc_yda <- init$dmc
    dc_yda <- init$dc
  }
  # constrain relative humidity
  was_rh_100 <- input[["rh"]] >= 100
  input[["rh"]][was_rh_100] <- 99.9999
  ###########################################################################
  #                    Fine Fuel Moisture Code (FFMC)
  ###########################################################################
  
  ffmc <- lapp(
    x = c(ffmc_yda, input[[c("temp", "rh", "ws", "prec")]]),
    fun = Vectorize(fine_fuel_moisture_code)
  )
  
  ###########################################################################
  #                        Duff Moisture Code (DMC)
  ###########################################################################
  
  dmc <- lapp(
    x = c(
      dmc_yda,
      input[[c("temp", "rh", "prec")]],
      input[["lat"]],
      setValues(input[["temp"]], mon)
    ),
    fun = Vectorize(duff_moisture_code),
    lat.adjust = lat.adjust
  )
  
  ###########################################################################
  #                             Drought Code (DC)
  ###########################################################################
  
  dc <- lapp(
    x = c(
      dc_yda,
      input[[c("temp", "rh", "prec")]],
      input[["lat"]],
      setValues(input[["temp"]], mon)
    ),
    fun = Vectorize(drought_code),
    lat.adjust = lat.adjust
  )
  
  ###########################################################################
  #                    Initial Spread Index (ISI)
  ###########################################################################
  
  isi <- lapp(
    x = c(ffmc, input[["ws"]]),
    fun = Vectorize(initial_spread_index),
    fbpMod = FALSE
  )
  
  ###########################################################################
  #                       Buildup Index (BUI)
  ###########################################################################
  
  bui <- lapp(x = c(dmc, dc), fun = Vectorize(buildup_index))
  
  ###########################################################################
  #                     Fire Weather Index (FWI)
  ###########################################################################
  
  fwi <- lapp(x = c(isi, bui), fun = Vectorize(fire_weather_index))
  
  ###########################################################################
  #                   Daily Severity Rating (DSR)
  ###########################################################################
  # Eq. 31
  dsr <- 0.0272 * (fwi^1.77)
  if (!had_latitude) {
    input <- input[[setdiff(names(input), c("lat"))]]
  }
  input[["rh"]][was_rh_100] <- 100.0
  # If output specified is "fwi", then return only the FWI variables
  if (out == "fwi") {
    # Creating a raster stack of FWI variables to return
    new_FWI <- c(ffmc, dmc, dc, isi, bui, fwi, dsr)
    names(new_FWI) <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr")
    if (uppercase) {
      names(new_FWI) <- toupper(names(new_FWI))
    }
    # If output specified is "all", then return both FWI and input weather vars
  } else {
    if (out == "all") {
      # Create a raster stack of input and FWI variables
      new_FWI <- c(input, ffmc, dmc, dc, isi, bui, fwi, dsr)
      names(new_FWI) <- c(
        names(input),
        "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr"
      )
      if (uppercase) {
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(new_FWI)
}