# Modified crown_area function to use BC species codes and calculate crown area
crown_area <- function(species_code, dbh) {
  #
  # Define the coefficients for each species
  species_coefficients <- list(
    "SF" = c(A = 3.9723, B = 0.5177),
    "WF" = c(A = 3.8166, B = 0.5229),
    "GF" = c(A = 4.187, B = 0.5341),
    "AF" = c(A = 3.2348, B = 0.5179),
    "RF" = c(A = 3.1146, B = 0.578),
    "NF" = c(A = 3.0614, B = 0.6276),
    "YC" = c(A = 3.5341, B = 0.5374),
    "C" = c(A = 4.092, B = 0.4912),
    "S" = c(A = 3.6802, B = 0.494),
    "LP" = c(A = 2.4132, B = 0.6403),
    "JP" = c(A = 3.2367, B = 0.6247),
    "SP" = c(A = 3.061, B = 0.6201),
    "WP" = c(A = 3.4447, B = 0.5185),
    "PP" = c(A = 2.8541, B = 0.64),
    "DF" = c(A = 4.4215, B = 0.5329),
    "RW" = c(A = 4.4215, B = 0.5329), 
    "RC" = c(A = 6.2318, B = 0.4259),
    "WH" = c(A = 5.4864, B = 0.5144),
    "MH" = c(A = 2.9372, B = 0.5878),
    "BM" = c(A = 7.5183, B = 0.4461),
    "RA" = c(A = 7.0806, B = 0.4771),
    "WA" = c(A = 7.0806, B = 0.4771), 
    "PB" = c(A = 5.898, B = 0.4841),
    "GC" = c(A = 2.4922, B = 0.8544),
    "AS" = c(A = 4.091, B = 0.5907),
    "CW" = c(A = 7.5183, B = 0.4461), 
    "WO" = c(A = 2.4922, B = 0.8544), 
    "J" = c(A = 4.5859, B = 0.4841),
    "LL" = c(A = 2.1039, B = 0.6758),
    "WB" = c(A = 2.1606, B = 0.6897),
    "KP" = c(A = 2.1451, B = 0.7132),
    "PY" = c(A = 4.5859, B = 0.4841), 
    "DG" = c(A = 2.4922, B = 0.8544), 
    "HT" = c(A = 4.5859, B = 0.4841), 
    "CH" = c(A = 4.5859, B = 0.4841), 
    "WI" = c(A = 4.5859, B = 0.4841),
    "Any" = c(A=4.4215, B= 0.5329)
  )
  
  # Mapping from BC species codes to U.S. species codes used in the function
  bc_to_us_species_map <- list(
    "Ac" = "PB",   # Poplar to Paper Birch
    "Act" = "PB",  # Black Cottonwood to Paper Birche
    "At" = "AS",   # Trembling Aspen to Aspen
    "Ax" = "PB",   # Poplar Hybrid to Paper Birch
    "Ba" = "AF",   # Amabilis Fir to Amabilis Fir
    "Bg" = "GF",   # Grand Fir to Grand Fir
    "Bl" = "SF",   # Sub Alpine Fir to Subalpine Fir
    "Bp" = "NF",   # Noble Fir to Noble Fir
    "Cw" = "C",    # Western Red Cedar to Cedar
    "Dg" = "DG",   # Green/Sitka Alder to Sitka Alder
    "Dr" = "RA",   # Red Alder to Red Alder
    "Ea" = "PB",   # Alaska Paper Birch to Paper Birch
    "Ep" = "PB",   # Paper Birch to Paper Birch
    "Fd" = "DF",   # Douglas Fir to Douglas-Fir
    "Hm" = "MH",   # Mountain Hemlock to Mountain Hemlock
    "Hw" = "WH",   # Western Hemlock to Western Hemlock
    "Lt" = "LP",   # Tamarack to Lodgepole Pine or Jack Pine
    "Lw" = "LP",   # Western Larch to Lodgepole Pine or Jack Pine
    "Mb" = "BM",   # Bigleaf Maple to Bigleaf Maple
    "Pa" = "WB",   # Whitebark Pine to Whitebark Pine
    "Pf" = "WP",   # Limber Pine to Western White Pine
    "Pj" = "LP",   # Jack Pine to Lodgepole Pine or Jack Pine
    "Pl" = "LP",   # Lodgepole Pine to Lodgepole Pine or Jack Pine
    "Pw" = "WP",   # Western White Pine to Western White Pine
    "Py" = "PP",   # Yellow Pine to Ponderosa Pine
    "Sb" = "S",    # Black Spruce to Spruce
    "Sn" = "S",    # Norway Spruce to Spruce
    "Ss" = "S",    # Sitka Spruce to Spruce
    "Sx" = "S",    # Spruce Hybrid to Spruce
    "Sxs" = "S",   # Sitka X to Spruce
    "Yc" = "YC",# Yellow Cypress to Yellow Cypress
    "Any" = "Any"
  )
  
  # Convert BC code to U.S. code using the mapping
  if (!is.null(bc_to_us_species_map[[species_code]])) {
    species_code <- bc_to_us_species_map[[species_code]]
  } else {
    species_code<-"Any"
  }
  
  # dbh in cm, converted to inches
  dbh_in <- dbh / 2.54
  
  # Check if species code exists in the coefficients list
  if (!species_code %in% names(species_coefficients)) {
    stop("U.S. species code derived from BC code not recognized in coefficients list")
  }
  
  # Extract coefficients for the species
  coefficients <- species_coefficients[[species_code]]
  A <- coefficients["A"]
  B <- coefficients["B"]
  
  # Calculate crown width based on species equations
  CW <- A * (dbh_in^B)
  
  # Calculate crown area in square feet and convert to square meters
  CA_ft <- ((CW / 2)^2) * pi
  CrownArea_m2 <- CA_ft * 0.09290304
  
  return(CrownArea_m2)
}
