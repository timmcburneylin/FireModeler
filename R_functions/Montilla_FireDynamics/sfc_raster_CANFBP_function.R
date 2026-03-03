#surface fuel consumption form canadian fbp system
library(terra)
sfc_raster <- function(FUELTYPE, FFMC, BUI, PC, GFL) {
  SFC <- rep(-999, length(FFMC))
  # Eqs. 9a, 9b (Wotton et. al. 2009) - Solving the lower bound of FFMC value
  # for the C1 fuel type SFC calculation
  SFC <- ifel(
    FUELTYPE == 1,
    ifel(
      FFMC > 84,
      0.75 + 0.75 * (1 - exp(-0.23 * (FFMC - 84)))**0.5,
      0.75 - 0.75 * (1 - exp(-0.23 * (84 - FFMC)))**0.5
    ),
    SFC
  )
  
  # Eq. 10 (FCFDG 1992) - C2, M3, and M4 Fuel Types
  SFC <- ifel(
    FUELTYPE == 2 | FUELTYPE == 11 | FUELTYPE == 12,
    5.0 * (1 - exp(-0.0115 * BUI)),
    SFC
  )
  # Eq. 11 (FCFDG 1992) - C3, C4 Fuel Types
  SFC <- ifel(
    FUELTYPE == 3 | FUELTYPE == 4,
    5.0 * (1 - exp(-0.0164 * BUI))**2.24,
    SFC
  )
  # Eq. 12 (FCFDG 1992) - C5, C6 Fuel Types
  SFC <- ifel(
    FUELTYPE == 5 | FUELTYPE == 6,
    5.0 * (1 - exp(-0.0149 * BUI))**2.48,
    SFC
  )
  # Eqs. 13, 14, 15 (FCFDG 1992) - C7 Fuel Types
  SFC <- ifel(
    FUELTYPE == 7,
    ifel(
      FFMC > 70,
      2 * (1 - exp(-0.104 * (FFMC - 70))),
      0
    ) + 1.5 * (1 - exp(-0.0201 * BUI)),
    SFC
  )
  # Eq. 16 (FCFDG 1992) - D1 Fuel Type
  SFC <- ifel(FUELTYPE == 8, 1.5 * (1 - exp(-0.0183 * BUI)), SFC)
  # Eq. 17 (FCFDG 1992) - M1 and M2 Fuel Types
  SFC <- ifel(
    FUELTYPE == 9 | FUELTYPE == 10,
    (PC / 100 * (5.0 * (1 - exp(-0.0115 * BUI)))
     + ((100 - PC) / 100 * (1.5 * (1 - exp(-0.0183 * BUI))))),
    SFC
  )
  # Eq. 18 (FCFDG 1992) - Grass Fuel Types
  SFC <- ifel(FUELTYPE == 14 | FUELTYPE == 15, GFL, SFC)
  # Eq. 19, 20, 25 (FCFDG 1992) - S1 Fuel Type
  SFC <- ifel(
    FUELTYPE == 16,
    4.0 * (1 - exp(-0.025 * BUI)) + 4.0 * (1 - exp(-0.034 * BUI)),
    SFC
  )
  # Eq. 21, 22, 25 (FCFDG 1992) - S2 Fuel Type
  SFC <- ifel(
    FUELTYPE == 17,
    10.0 * (1 - exp(-0.013 * BUI)) + 6.0 * (1 - exp(-0.060 * BUI)),
    SFC
  )
  # Eq. 23, 24, 25 (FCFDG 1992) - S3 Fuel Type
  SFC <- ifel(
    FUELTYPE == 18,
    12.0 * (1 - exp(-0.0166 * BUI)) + 20.0 * (1 - exp(-0.0210 * BUI)),
    SFC
  )
  # Constrain SFC value
  SFC <- ifel(SFC <= 0, 0.000001, SFC)
  return(SFC)
}

.SFCcalc <- function(...) {
  .Deprecated("surface_fuel_consumption")
  return(surface_fuel_consumption(...))
}