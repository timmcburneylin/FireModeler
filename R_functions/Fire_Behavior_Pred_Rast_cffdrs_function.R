#modified FBP Raster function  
source(file.path(root, "R_functions", "fire_behavior_prediction_function.R"))
source(file.path(root, "R_functions", "Initial_Spread_Index.R"))
library(data.table)
library(raster)
library(terra)

#
fbpRaster_mod <- function(
    input,
    output = "Primary",
    select = NULL,
    m = NULL,
    cores = 1) {
  
  output_orig <- output
  output <- toupper(output)
  # due to NSE notes in R CMD check
  x = y = FUELTYPE = FUELTYPE0 = NULL
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case. This is also dont in FBPcalc, but we require use
  #  of this variable here before it gets to FBPCalc
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  if (!is(input,"SpatRaster")) {
    input <- terra::rast(input)
  }
  # split up large rasters to allow calculation. This will be used in the
  #  parallel methods
  if (is.null(m)) {
    m <- ifelse(ncell(input) > 500000, 3000, 1000)
  }
  # Setup correct output names
  allNames <- c(
    "CFB", "CFC", "FD", "HFI", "RAZ", "ROS", "SFC", "TFC", "BE", "SF", "ISI",
    "FFMC", "FMC", "D0", "RSO", "CSI", "FROS", "BROS", "HROSt", "FROSt",
    "BROSt", "FCFB", "BCFB", "FFI", "BFI", "FTFC", "BTFC", "TI", "FTI",
    "BTI", "LB", "LBt", "WSV", "DH", "DB", "DF", "TROS", "TROSt", "TCFB",
    "TFI", "TTFC", "TTI"
  )
  primaryNames <- allNames[1:8]
  secondaryNames <- allNames[9:length(allNames)]
  # If outputs are specified, then check if they exist and stop with an error
  #  if not.
  if (!is.null(select)) {
    select <- toupper(select)
    select <- select[!duplicated(select)]
    if (output == "SECONDARY" | output == "S") {
      if (!sort(select %in% secondaryNames)[1]) {
        stop("Selected variables are not in the outputs")
      }
    }
    if (output == "PRIMARY" | output == "P") {
      if (!sort(select %in% primaryNames)[1]) {
        stop("Selected variables are not in the outputs")
      }
    }
    if (output == "ALL" | output == "A") {
      if (!sort(select %in% allNames)[1]) {
        stop("Selected variables are not in the outputs")
      }
    }
  }
  # If caller specifies select outputs, then create a raster stack that contains
  # only those outputs
  if (is.null(select)) {
    if (output %in% c("PRIMARY", "P")) {
      select <- primaryNames
    } else if (output %in% c("SECONDARY", "S")) {
      select <- secondaryNames
    } else if (output %in% c("ALL", "A")) {
      select <- allNames
    } else {
      stop("Invalid output selected. ",output_orig, " cannot be returned.")
    }
  }
  
  names(input) <- toupper(names(input))
  if (!"LAT" %in% names(input)) {
    r <- as.data.table(input, xy = TRUE)
    coords <- st_sfc(
      st_multipoint(matrix(ncol = 2, r[, c(x, y)]), dim = "XY"),
      crs = terra::crs(input)
    )
    coords <- st_coordinates(st_transform(coords, 4326))
    r[, `:=`(x = coords[, "X"], y = coords[, "Y"])]
    data.table::setnames(r, c("x", "y"), c("LON", "LAT"))
    # Check for valid latitude
    if (max(r$LAT) > 90 | min(r$LAT) < -90) {
      warning(paste0(
        "Input projection is not in lat/long, consider",
        " re-projection or include LAT as input"
      ))
    }
  } else {
    r <- as.data.table(input)
  }
  # Unique IDs
  r$ID <- 1:nrow(r)
  # merge fuel codes with integer values
  fuelCross <- data.table(
    FUELTYPE0 = sort(c(
      paste("C", 1:7, sep = "-"),
      "D-1",
      paste("M", 1:4, sep = "-"),
      paste("S", 1:3, sep = "-"),
      "O-1a", "O-1b", "WA", "NF"
    )),
    code = 1:19
  )
  r[, FUELTYPE := fuelCross[match(r$FUEL, fuelCross$code), FUELTYPE0]]
  # Calculate FBP through the fbp() function
  FBP <- fire_behavior_prediction_function(r, output = output)
  # FBP <- FBP[r, on = c("ID")]
  # If secondary output selected then we need to reassign character
  # representation of Fire Type S/I/C to a numeric value 1/2/3
  if (!(output == "SECONDARY" | output == "S")) {
    FBP$FD <- as.integer(chartr("SIC", "123", FBP$FD))
  }
  if ("FD" %in% select) {
    message(paste0(
      "FD = 1,2,3 representing Surface (S),",
      " Intermittent (I), and Crown (C) fire"
    ))
  }
  out <- c(rep(input[[1]], length(select)))
  names(out) <- select
  # FBP <- FBP[, ..select]
  for (i in select) {
    out[[i]] <- FBP[[i]]
  }
  return(out)
}
