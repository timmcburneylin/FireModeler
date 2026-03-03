#surface fuel consumption form canadian fbp system

surface_fuel_consumption <- function(FUELTYPE, FFMC, BUI, PC, GFL) {
  SFC <- rep(-999, length(FFMC))
  
  # Eqs. 9a, 9b (Wotton et. al. 2009) - Solving the lower bound of FFMC value
  # for the C1 fuel type SFC calculation
  SFC <- ifelse(
    FUELTYPE %in% c("C1","C-1"),
    ifelse(
      FFMC > 84,
      0.75 + 0.75 * (1 - exp(-0.23 * (FFMC - 84)))**0.5,
      0.75 - 0.75 * (1 - exp(-0.23 * (84 - FFMC)))**0.5
    ),
    SFC
  )
  
  # Eq. 10 (FCFDG 1992) - C2, M3, and M4 Fuel Types
  SFC <- ifelse(
    FUELTYPE %in% c("C2","C-2") | FUELTYPE %in% c("M3","M-1")| FUELTYPE %in% c("M4","M-4"),
    5.0 * (1 - exp(-0.0115 * BUI)),
    SFC
  )
  
  # Eq. 11 (FCFDG 1992) - C3, C4 Fuel Types
  SFC <- ifelse(
    FUELTYPE %in% c("C3","C-3") | FUELTYPE %in% c("C4","C-4"),
    5.0 * (1 - exp(-0.0164 * BUI))**2.24,
    SFC
  )
  
  # Eq. 12 (FCFDG 1992) - C5, C6 Fuel Types
  SFC <- ifelse(
    FUELTYPE %in% c("C5","C-5") | FUELTYPE %in% c("C6","C-6"),
    5.0 * (1 - exp(-0.0149 * BUI))**2.48,
    SFC
  )
  # Eqs. 13, 14, 15 (FCFDG 1992) - C7 Fuel Types
  SFC <- ifelse(
    FUELTYPE %in% c("C7","C-7"),
    ifelse(
      FFMC > 70,
      2 * (1 - exp(-0.104 * (FFMC - 70))),
      0
    ) + 1.5 * (1 - exp(-0.0201 * BUI)),
    SFC
  )
  # Eq. 16 (FCFDG 1992) - D1 Fuel Type
  SFC <- ifelse(FUELTYPE %in% c("D1","D-1"), 1.5 * (1 - exp(-0.0183 * BUI)), SFC)
  # Eq. 17 (FCFDG 1992) - M1 and M2 Fuel Types
  SFC <- ifelse(
    FUELTYPE %in% c("M1","M-1") | FUELTYPE %in% c("M2","M-2"),
    (PC / 100 * (5.0 * (1 - exp(-0.0115 * BUI)))
     + ((100 - PC) / 100 * (1.5 * (1 - exp(-0.0183 * BUI))))),
    SFC
  )
  # Eq. 18 (FCFDG 1992) - Grass Fuel Types
  SFC <- ifelse(FUELTYPE %in% c("O1A","O-1A") | FUELTYPE %in% c("O1B","O-1B"), GFL, SFC)
  # Eq. 19, 20, 25 (FCFDG 1992) - S1 Fuel Type
  SFC <- ifelse(
    FUELTYPE %in% c("S1","S-1"),
    4.0 * (1 - exp(-0.025 * BUI)) + 4.0 * (1 - exp(-0.034 * BUI)),
    SFC
  )
  # Eq. 21, 22, 25 (FCFDG 1992) - S2 Fuel Type
  SFC <- ifelse(
    FUELTYPE %in% c("S2","S-2"),
    10.0 * (1 - exp(-0.013 * BUI)) + 6.0 * (1 - exp(-0.060 * BUI)),
    SFC
  )
  # Eq. 23, 24, 25 (FCFDG 1992) - S3 Fuel Type
  SFC <- ifelse(
    FUELTYPE %in% c("S3","S-3"),
    12.0 * (1 - exp(-0.0166 * BUI)) + 20.0 * (1 - exp(-0.0210 * BUI)),
    SFC
  )
  # Constrain SFC value
  SFC <- ifelse(SFC <= 0, 0.000001, SFC)
  return(SFC)
}

.SFCcalc <- function(...) {
  .Deprecated("surface_fuel_consumption")
  return(surface_fuel_consumption(...))
}