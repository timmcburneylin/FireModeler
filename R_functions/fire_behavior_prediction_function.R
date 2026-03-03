source_fun("Initial_Spread_Index.R")
#manual fire behavior prediction function
fire_behavior_prediction_function <- function(
    input = NULL,
    output = "Primary") {
  library(cffdrs)
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  # print(input)
  # print(input$ID)
  output <- toupper(output)
  # if input does not exist, then set defaults
  if (is.null(input)) {
    input <- data.frame(
      FUELTYPE = "C2", ACCEL = 0, DJ = 180, D0 = 0, ELV = 0, BUIEFF = 1,
      HR = 1, FFMC = 90, ISI = 0, BUI = 60, WS = 10, WD = 0, GS = 0,
      ASPECT = 0, PC = 50, PDF = 35, CC = 80, GFL = 0.35, CBH = 3, CFL = 1,
      LAT = 55, LONG = -120, FMC = 0, THETA = 0
    )
    input[, "FUELTYPE"] <- as.character(input[, "FUELTYPE"])
  }
  # set local scope variables from the parameters for simpler to referencing
  names(input) <- toupper(names(input))
  ID <- input$ID
  FUELTYPE <- input$FUELTYPE
  FFMC <- input$FFMC
  BUI <- input$BUI
  WS <- input$WS
  WD <- input$WD
  FMC <- input$FMC
  GS <- input$GS
  LAT <- input$LAT
  LONG <- input$LONG
  ELV <- input$ELV
  DJ <- input$DJ
  D0 <- input$D0
  SD <- input$SD
  SH <- input$SH
  HR <- input$HR
  PC <- input$PC
  PDF <- input$PDF
  GFL <- input$GFL
  CC <- input$CC
  THETA <- input$THETA
  ACCEL <- input$ACCEL
  ASPECT <- input$ASPECT
  BUIEFF <- input$BUIEFF
  CBH <- input$CBH
  CFL <- input$CFL
  ISI <- input$ISI
  n0 <- nrow(input)
  ########################################################################
  #                       Functions
  #CBH
  crown_base_height <- function(FUELTYPE, CBH, SD, SH) {
    # logic originally in fbp() pulled into its own function
    CBHs <- c(
      2, 3, 8, 4, 18, 7, 10,
      0, 6, 6, 6, 6, 0, 0, 0, 0, 0
    )
    names(CBHs) <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )
    CBH <- ifelse(
      CBH <= 0 | CBH > 50 | is.na(CBH),
      ifelse(
        FUELTYPE %in% c("C6") & SD > 0 & SH > 0,
        -11.2 + 1.06 * SH + 0.0017 * SD,
        CBHs[FUELTYPE]
      ),
      CBH
    )
    CBH <- ifelse(CBH < 0, 1e-07, CBH)
    return(CBH)
  }
  #CFL
  crown_fuel_load <- function(FUELTYPE, CFL) {
    # logic originally in fbp() pulled into its own function
    CFLs <- c(
      0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5,
      0, 0.8, 0.8, 0.8, 0.8, 0, 0, 0, 0, 0
    )
    names(CFLs) <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )
    CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), CFLs[FUELTYPE], CFL)
    return(CFL)
  }
  #FMC
  foliar_moisture_content <- function(LAT, LONG, ELV, DJ, D0) {
    # Initialize vectors
    FMC <- rep(-1, length(LAT))
    LATN <- rep(0, length(LAT))
    # Calculate Normalized Latitude
    # Eqs. 1 & 3 (FCFDG 1992)
    LATN <- ifelse(
      D0 <= 0,
      ifelse(
        ELV <= 0,
        46 + 23.4 * exp(-0.0360 * (150 - LONG)),
        43 + 33.7 * exp(-0.0351 * (150 - LONG))
      ),
      LATN
    )
    # Calculate Date of minimum foliar moisture content
    # Eqs. 2 & 4 (FCFDG 1992)
    D0 <- ifelse(
      D0 <= 0,
      ifelse(
        ELV <= 0,
        151 * (LAT / LATN),
        142.1 * (LAT / LATN) + 0.0172 * ELV
      ),
      D0
    )
    # Round D0 to the nearest integer because it is a date
    D0 <- round(D0, 0)
    # Number of days between day of year and date of min FMC
    # Eq. 5 (FCFDG 1992)
    ND <- abs(DJ - D0)
    # Calculate final FMC
    # Eqs. 6, 7, & 8 (FCFDG 1992)
    FMC <- ifelse(
      ND < 30,
      85 + 0.0189 * ND^2,
      ifelse(
        ND >= 30 & ND < 50,
        32.9 + 3.17 * ND - 0.0288 * ND^2,
        120
      )
    )
    return(FMC)
  }
  
  .FMCcalc <- function(...) {
    .Deprecated("foliar_moisture_content")
    return(foliar_moisture_content(...))
  }
  #SFC
  surface_fuel_consumption <- function(FUELTYPE, FFMC, BUI, PC, GFL) {
    SFC <- rep(-999, length(FFMC))
    # Eqs. 9a, 9b (Wotton et. al. 2009) - Solving the lower bound of FFMC value
    # for the C1 fuel type SFC calculation
    SFC <- ifelse(
      FUELTYPE == "C1",
      ifelse(
        FFMC > 84,
        0.75 + 0.75 * (1 - exp(-0.23 * (FFMC - 84)))**0.5,
        0.75 - 0.75 * (1 - exp(-0.23 * (84 - FFMC)))**0.5
      ),
      SFC
    )
    # Eq. 10 (FCFDG 1992) - C2, M3, and M4 Fuel Types
    SFC <- ifelse(
      FUELTYPE == "C2" | FUELTYPE == "M3" | FUELTYPE == "M4",
      5.0 * (1 - exp(-0.0115 * BUI)),
      SFC
    )
    # Eq. 11 (FCFDG 1992) - C3, C4 Fuel Types
    SFC <- ifelse(
      FUELTYPE == "C3" | FUELTYPE == "C4",
      5.0 * (1 - exp(-0.0164 * BUI))**2.24,
      SFC
    )
    # Eq. 12 (FCFDG 1992) - C5, C6 Fuel Types
    SFC <- ifelse(
      FUELTYPE == "C5" | FUELTYPE == "C6",
      5.0 * (1 - exp(-0.0149 * BUI))**2.48,
      SFC
    )
    # Eqs. 13, 14, 15 (FCFDG 1992) - C7 Fuel Types
    SFC <- ifelse(
      FUELTYPE == "C7",
      ifelse(
        FFMC > 70,
        2 * (1 - exp(-0.104 * (FFMC - 70))),
        0
      ) + 1.5 * (1 - exp(-0.0201 * BUI)),
      SFC
    )
    # Eq. 16 (FCFDG 1992) - D1 Fuel Type
    SFC <- ifelse(FUELTYPE == "D1", 1.5 * (1 - exp(-0.0183 * BUI)), SFC)
    # Eq. 17 (FCFDG 1992) - M1 and M2 Fuel Types
    SFC <- ifelse(
      FUELTYPE == "M1" | FUELTYPE == "M2",
      (PC / 100 * (5.0 * (1 - exp(-0.0115 * BUI)))
       + ((100 - PC) / 100 * (1.5 * (1 - exp(-0.0183 * BUI))))),
      SFC
    )
    # Eq. 18 (FCFDG 1992) - Grass Fuel Types
    SFC <- ifelse(FUELTYPE == "O1A" | FUELTYPE == "O1B", GFL, SFC)
    # Eq. 19, 20, 25 (FCFDG 1992) - S1 Fuel Type
    SFC <- ifelse(
      FUELTYPE == "S1",
      4.0 * (1 - exp(-0.025 * BUI)) + 4.0 * (1 - exp(-0.034 * BUI)),
      SFC
    )
    # Eq. 21, 22, 25 (FCFDG 1992) - S2 Fuel Type
    SFC <- ifelse(
      FUELTYPE == "S2",
      10.0 * (1 - exp(-0.013 * BUI)) + 6.0 * (1 - exp(-0.060 * BUI)),
      SFC
    )
    # Eq. 23, 24, 25 (FCFDG 1992) - S3 Fuel Type
    SFC <- ifelse(
      FUELTYPE == "S3",
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
  #slope
  slope_adjustment <- function(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI) {
    NoBUI <- rep(-1, length(FFMC))
    # Eq. 39 (FCFDG 1992) - Calculate Spread Factor
    SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
    # ISI with 0 wind on level grounds
    ISZ <- initial_spread_index(FFMC, 0)
    # Surface spread rate with 0 wind on level ground
    RSZ <- rate_of_spread(FUELTYPE, ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, CC, CBH)
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
    RSF <- RSZ * SF
    # setup some reference vectors
    d <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )
    a <- c(
      90, 110, 110, 110, 30, 30, 45,
      30, 0, 0, 120, 100, 75, 40, 55, 190, 250
    )
    b <- c(
      0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800,
      0.0305, 0.0232, 0, 0, 0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350
    )
    c0 <- c(
      4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0,
      1.6, 0, 0, 1.4, 1.48, 1.3, 1.7, 3.2, 1.4, 1.7
    )
    names(a) <- names(b) <- names(c0) <- d
    # initialize some local vars
    RSZ <- rep(-99, length(FFMC))
    RSF_C2 <- rep(-99, length(FFMC))
    RSF_D1 <- rep(-99, length(FFMC))
    RSF_M3 <- rep(-99, length(FFMC))
    RSF_M4 <- rep(-99, length(FFMC))
    CF <- rep(-99, length(FFMC))
    ISF <- rep(-99, length(FFMC))
    ISF_C2 <- rep(-99, length(FFMC))
    ISF_D1 <- rep(-99, length(FFMC))
    ISF_M3 <- rep(-99, length(FFMC))
    ISF_M4 <- rep(-99, length(FFMC))
    
    # Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalent ISI
    
    is_basic <- Vectorize(function(fuel) { fuel %in% c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "S1", "S2", "S3"
    ) })(FUELTYPE)
    basic_isf <- Vectorize(function(rsf, fuel) {
      ifelse(
        (1 - (rsf / a[fuel])**(1 / c0[fuel])) >= 0.01,
        log(1 - (rsf / a[fuel])**(1 / c0[fuel])) / (-b[fuel]),
        log(0.01) / (-b[fuel])
      )
    })
    ISF <- ifelse(
      is_basic,
      basic_isf(RSF, FUELTYPE),
      ISF
    )
    
    # When calculating the M1/M2 types, we are going to calculate for both C2
    # and D1 types, and combine
    # Surface spread rate with 0 wind on level ground
    RSZ <- ifelse(
      FUELTYPE %in% c("M1", "M2"),
      rate_of_spread(
        rep("C2", length(ISZ)),
        ISZ, NoBUI, FMC, SFC, PC, PDF, CC, CBH
      ),
      RSZ
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for C2
    RSF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_C2)
    RSZ <- ifelse(
      FUELTYPE %in% c("M1", "M2"),
      rate_of_spread(
        rep("D1", length(ISZ)),
        ISZ, NoBUI, FMC, SFC, PC, PDF, CC, CBH
      ),
      RSZ
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
    RSF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_D1)
    RSF0 <- 1 - (RSF_C2 / a[["C2"]])^(1 / c0[["C2"]])
    # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_C2 <- ifelse(
      FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
      log(1 - (RSF_C2 / a[["C2"]])**(1 / c0[["C2"]])) / (-b[["C2"]]),
      ISF_C2
    )
    # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_C2 <- ifelse(
      FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
      log(0.01) / (-b[["C2"]]),
      ISF_C2
    )
    RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
    # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_D1 <- ifelse(
      FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
      log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
      ISF_D1
    )
    # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_D1 <- ifelse(
      FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
      log(0.01) / (-b[["D1"]]),
      ISF_D1
    )
    # Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
    ISF <- ifelse(
      FUELTYPE %in% c("M1", "M2"),
      PC / 100 * ISF_C2 + (1 - PC / 100) * ISF_D1,
      ISF
    )
    
    # Set % Dead Balsam Fir to 100%
    PDF100 <- rep(100, length(ISI))
    # Surface spread rate with 0 wind on level ground
    RSZ <- ifelse(
      FUELTYPE %in% c("M3"),
      rate_of_spread(
        rep("M3", length(FMC)),
        ISZ, NoBUI, FMC, SFC, PC, PDF100, CC, CBH
      ),
      RSZ
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
    RSF_M3 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_M3)
    # Surface spread rate with 0 wind on level ground, using D1
    RSZ <- ifelse(
      FUELTYPE %in% c("M3"),
      rate_of_spread(
        rep("D1", length(ISZ)),
        ISZ, NoBUI, FMC, SFC, PC, PDF100, CC, CBH
      ),
      RSZ
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
    RSF_D1 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_D1)
    RSF0 <- 1 - (RSF_M3 / a[["M3"]])^(1 / c0[["M3"]])
    # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_M3 <- ifelse(
      FUELTYPE %in% c("M3") & RSF0 >= 0.01,
      log(1 - (RSF_M3 / a[["M3"]])**(1 / c0[["M3"]])) / (-b[["M3"]]),
      ISF_M3
    )
    # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_M3 <- ifelse(
      FUELTYPE %in% c("M3") & RSF0 < 0.01,
      log(0.01) / (-b[["M3"]]),
      ISF_M3
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
    RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
    # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_D1 <- ifelse(
      FUELTYPE %in% c("M3") & RSF0 >= 0.01,
      log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
      ISF_D1
    )
    # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_D1 <- ifelse(
      FUELTYPE %in% c("M3") & RSF0 < 0.01,
      log(0.01) / (-b[["D1"]]),
      ISF_D1
    )
    # Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
    ISF <- ifelse(
      FUELTYPE %in% c("M3"),
      PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1,
      ISF
    )
    # Surface spread rate with 0 wind on level ground, using M4
    RSZ <- ifelse(
      FUELTYPE %in% c("M4"),
      rate_of_spread(
        rep("M4", length(FMC)),
        ISZ, NoBUI, FMC, SFC, PC, PDF100, CC, CBH
      ),
      RSZ
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M4
    RSF_M4 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_M4)
    # Surface spread rate with 0 wind on level ground, using M4
    RSZ <- ifelse(
      FUELTYPE %in% c("M4"),
      rate_of_spread(
        rep("D1", length(ISZ)),
        ISZ, NoBUI, FMC, SFC, PC, PDF100, CC, CBH
      ),
      RSZ
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
    RSF_D1 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_D1)
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
    RSF0 <- 1 - (RSF_M4 / a[["M4"]])^(1 / c0[["M4"]])
    # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_M4 <- ifelse(
      FUELTYPE %in% c("M4") & RSF0 >= 0.01,
      log(1 - (RSF_M4 / a[["M4"]])**(1 / c0[["M4"]])) / (-b[["M4"]]),
      ISF_M4
    )
    # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
    ISF_M4 <- ifelse(
      FUELTYPE %in% c("M4") & RSF0 < 0.01,
      log(0.01) / (-b[["M4"]]),
      ISF_M4
    )
    # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
    RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
    # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI (D1)
    ISF_D1 <- ifelse(
      FUELTYPE %in% c("M4") & RSF0 >= 0.01,
      log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
      ISF_D1
    )
    # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI (D1)
    ISF_D1 <- ifelse(
      FUELTYPE %in% c("M4") & RSF0 < 0.01,
      log(0.01) / (-b[["D1"]]),
      ISF_D1
    )
    # Eq. 42c (Wotton 2009) - Calculate weighted average for the M4 type
    ISF <- ifelse(
      FUELTYPE %in% c("M4"),
      PDF / 100 * ISF_M4 + (1 - PDF / 100.) * ISF_D1,
      ISF
    )
    # Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
    CF <- ifelse(
      FUELTYPE %in% c("O1A", "O1B"),
      ifelse(CC < 58.8,
             0.005 * (exp(0.061 * CC) - 1),
             0.176 + 0.02 * (CC - 58.8)
      ),
      CF
    )
    # Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
    ISF <- ifelse(
      FUELTYPE %in% c("O1A", "O1B"),
      ifelse(
        (1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) >= 0.01,
        log(1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) / (-b[FUELTYPE]),
        log(0.01) / (-b[FUELTYPE])
      ),
      ISF
    )
    # Initialize RAZ and WSV
    RAZ <- WSV <- rep(-99, length(FFMC))
    # Eq. 46 (FCFDG 1992)
    m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
    # Eq. 45 (FCFDG 1992) - FFMC function from the ISI equation
    fF <- 91.9 * exp(-.1386 * m) * (1 + (m**5.31) / 4.93e7)
    # Eqs. 44a, 44d (Wotton 2009) - Slope equivalent wind speed
    WSE <- 1 / 0.05039 * log(ISF / (0.208 * fF))
    # Eqs. 44b, 44e (Wotton 2009) - Slope equivalent wind speed
    WSE <- ifelse(
      WSE > 40 & ISF < (0.999 * 2.496 * fF),
      28 - (1 / 0.0818 * log(1 - ISF / (2.496 * fF))),
      WSE
    )
    # Eqs. 44c (Wotton 2009) - Slope equivalent wind speed
    WSE <- ifelse(WSE > 40 & ISF >= (0.999 * 2.496 * fF), 112.45, WSE)
    # Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
    WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
    # Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
    WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
    # Eq. 49 (FCFDG 1992) - the net effective wind speed
    WSV <- sqrt(WSX * WSX + WSY * WSY)
    WSV <- ifelse(FUELTYPE %in% c("NF", "WA"), NA, WSV)
    # Eq. 50 (FCFDG 1992) - the net effective wind direction (radians)
    RAZ <- acos(WSY / WSV)
    # Eq. 51 (FCFDG 1992) - convert possible negative RAZ into more understandable
    # directions
    RAZ <- ifelse(WSX < 0, 2 * pi - RAZ, RAZ)
    RAZ <- ifelse(FUELTYPE %in% c("NF", "WA"), NA, RAZ)
    return(list(WSV = WSV,
                RAZ = RAZ))
  }
  
  .Slopecalc <- function(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI,
    output = "RAZ") {
    .Deprecated("slope_adjustment")
    # output options include: RAZ and WSV
    
    # check for valid output types
    validOutTypes <- c("RAZ", "WAZ", "WSV")
    if (!(output %in% validOutTypes)) {
      stop(paste0(
        "In 'slopecalc()', '", output, "' is an invalid 'output' type."
      ))
    }
    
    values <- slope_adjustment(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
    return(values[[output]])
  }
  #ROS
  rate_of_spread_extended <- function(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) {
    # Set up some data vectors
    NoBUI <- rep(-1, length(ISI))
    d <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )
    a <- c(
      90, 110, 110, 110, 30, 30, 45,
      30, 0, 0, 120, 100, 75, 40, 55, 190, 250
    )
    b <- c(
      0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305,
      0.0232, 0, 0, 0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350
    )
    c0 <- c(
      4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0,
      1.6, 0, 0, 1.4, 1.48, 1.3, 1.7, 3.2, 1.4, 1.7
    )
    names(a) <- names(b) <- names(c0) <- d
    
    # Calculate RSI (set up data vectors first)
    # Eq. 26 (FCFDG 1992) - Initial Rate of Spread for Conifer and Slash types
    RSI <- rep(-1, length(ISI))
    RSI <- ifelse(
      FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C7", "D1", "S1", "S2", "S3"),
      as.numeric(a[FUELTYPE] * (1 - exp(-b[FUELTYPE] * ISI))**c0[FUELTYPE]),
      RSI
    )
    # Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M1 Mixedwood type
    RSI <- ifelse(
      FUELTYPE %in% c("M1"),
      (PC / 100
       * rate_of_spread(
         rep("C2", length(ISI)),
         ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH
       )
       + (100 - PC) / 100
       * rate_of_spread(
         rep("D1", length(ISI)),
         ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH
       )),
      RSI
    )
    # Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M2 Mixedwood type
    RSI <- ifelse(
      FUELTYPE %in% c("M2"),
      (PC / 100 *
         rate_of_spread(
           rep("C2", length(ISI)),
           ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH
         )
       + 0.2 * (100 - PC) / 100 *
         rate_of_spread(
           rep("D1", length(ISI)),
           ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH
         )),
      RSI
    )
    # Initial Rate of Spread for M3 Mixedwood
    RSI_m3 <- rep(-99, length(ISI))
    # Eq. 30 (Wotton et. al 2009)
    RSI_m3 <- ifelse(
      FUELTYPE %in% c("M3"),
      as.numeric(a[["M3"]] * ((1 - exp(-b[["M3"]] * ISI))**c0[["M3"]])),
      RSI_m3
    )
    # Eq. 29 (Wotton et. al 2009)
    RSI <- ifelse(
      FUELTYPE %in% c("M3"),
      (PDF / 100 * RSI_m3
       + (1 - PDF / 100) *
         rate_of_spread(
           rep("D1", length(ISI)),
           ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH
         )),
      RSI
    )
    # Initial Rate of Spread for M4 Mixedwood
    RSI_m4 <- rep(-99, length(ISI))
    # Eq. 30 (Wotton et. al 2009)
    RSI_m4 <- ifelse(
      FUELTYPE %in% c("M4"),
      as.numeric(a[["M4"]] * ((1 - exp(-b[["M4"]] * ISI))**c0[["M4"]])), RSI_m4
    )
    # Eq. 33 (Wotton et. al 2009)
    RSI <- ifelse(
      FUELTYPE %in% c("M4"),
      (PDF / 100 * RSI_m4
       + 0.2 * (1 - PDF / 100) *
         rate_of_spread(
           rep("D1", length(ISI)),
           ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH
         )),
      RSI
    )
    # Eq. 35b (Wotton et. al. 2009) - Calculate Curing function for grass
    CF <- rep(-99, length(ISI))
    CF <- ifelse(
      FUELTYPE %in% c("O1A", "O1B"),
      ifelse(
        CC < 58.8,
        0.005 * (exp(0.061 * CC) - 1),
        0.176 + 0.02 * (CC - 58.8)
      ),
      CF
    )
    # Eq. 36 (FCFDG 1992) - Calculate Initial Rate of Spread for Grass
    RSI <- ifelse(
      FUELTYPE %in% c("O1A", "O1B"),
      a[FUELTYPE] * ((1 - exp(-b[FUELTYPE] * ISI))**c0[FUELTYPE]) * CF,
      RSI
    )
    # used to be called like this and return ROS here
    # # Calculate the Rate of Spread (ROS)
    # ROS <- rate_of_spread(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
    # Calculate Critical Surface Intensity
    CSI <- critical_surface_intensity(FMC, CBH)
    # Calculate Surface fire rate of spread (m/min)
    RSO <- surface_fire_rate_of_spread(CSI, SFC)
    # use ifelse for C6 because ROS depends on CFB (opposite of other fuels)
    RSI <- ifelse(
      FUELTYPE %in% c("C6"),
      intermediate_surface_rate_of_spread_c6(ISI),
      RSI)
    RSC <- ifelse(
      FUELTYPE %in% c("C6"),
      crown_rate_of_spread_c6(ISI, FMC),
      NA)
    # HACK: need ROS first for non-C6
    # this is RSS for C6 and ROS otherwise
    RSS <- ifelse(
      FUELTYPE %in% c("C6"),
      surface_rate_of_spread_c6(RSI, BUI),
      buildup_effect(FUELTYPE, BUI) * RSI)
    # Calculate Crown Fraction Burned (CFB), C6 has different calculations
    CFB <- ifelse(
      FUELTYPE %in% c("C6"),
      crown_fraction_burned_c6(RSC, RSS, RSO),
      crown_fraction_burned(RSS, RSO))
    ROS <- ifelse(
      FUELTYPE %in% c("C6"),
      rate_of_spread_c6(RSC, RSS, CFB),
      RSS)
    # add a constraint
    ROS <- ifelse(ROS <= 0, 0.000001, ROS)
    # CFB <- ros_and_cfb$CFB
    return(list(ROS=ROS,
                CFB=CFB,
                CSI=CSI,
                RSO=RSO))
  }
  
  rate_of_spread <- function(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) {
    # HACK: C6 ROS depends on CFB so do this to not repeat calculations
    ros_vars <- rate_of_spread_extended(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
    return(ros_vars$ROS)
  }
  
  .ROScalc <- function(...) {
    .Deprecated("rate_of_spread")
    return(rate_of_spread(...))
  }
  #flank fire ROS
  flank_rate_of_spread <- function(ROS, BROS, LB) {
    # Eq. 89 (FCFDG 1992)
    FROS <- (ROS + BROS) / LB / 2
    return(FROS)
  }
  
  .FROScalc <- function(...) {
    .Deprecated("flank_rate_of_spread")
    return(flank_rate_of_spread(...))
  }
  #back spread rate
  back_rate_of_spread <- function(
    FUELTYPE, FFMC, BUI, WSV,
    FMC, SFC, PC, PDF, CC, CBH) {
    # Eq. 46 (FCFDG 1992)
    # Calculate the FFMC function from the ISI equation
    m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
    # Eq. 45 (FCFDG 1992)
    fF <- 91.9 * exp(-0.1386 * m) * (1.0 + (m**5.31) / 4.93e7)
    # Eq. 75 (FCFDG 1992)
    # Calculate the Back fire wind function
    BfW <- exp(-0.05039 * WSV)
    # Calculate the ISI associated with the back fire spread rate
    # Eq. 76 (FCFDG 1992)
    BISI <- 0.208 * BfW * fF
    # Eq. 77 (FCFDG 1992)
    # Calculate final Back fire spread rate
    BROS <- rate_of_spread(FUELTYPE, BISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
    return(BROS)
  }
  
  .BROScalc <- function(...) {
    .Deprecated("back_rate_of_spread")
    return(back_rate_of_spread(...))
  }
  #length to breadth
  length_to_breadth <- function(FUELTYPE, WSV) {
    # calculation is depending on if fuel type is grass (O1) or other fueltype
    LB <- ifelse(
      FUELTYPE %in% c("O1A", "O1B"),
      # Correction to orginal Equation 80 is made here
      # Eq. 80a / 80b from Wotton 2009
      ifelse(WSV >= 1.0, 1.1 * (WSV**0.464), 1.0), # Eq. 80/81
      1.0 + 8.729 * (1 - exp(-0.030 * WSV))**(2.155)
    ) # Eq. 79
    return(LB)
  }
  
  .LBcalc <- function(...) {
    .Deprecated("length_to_breadth")
    return(length_to_breadth(...))
  }
  #CSI
  critical_surface_intensity <- function(FMC, CBH)
  {
    # FIX: .FuelNF returns non-NA values from this
    #Eq. 56 (FCFDG 1992) Critical surface intensity
    CSI <- 0.001 * (CBH**1.5) * (460 + 25.9 * FMC)**1.5
    return (CSI)
  }
  
  surface_fire_rate_of_spread <- function(CSI, SFC) {
    # Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
    # # no fuel consumption means no spread
     RSO <- ifelse(0 == SFC,
                   0,
                   CSI / (300 * SFC))
    #RSO <- CSI / (300 * SFC)
    return(RSO)
  }
  
  crown_fraction_burned <- function(ROS, RSO) {
    # Eq. 58 (FCFDG 1992) Crown fraction burned
    CFB <- ifelse(ROS > RSO, 1 - exp(-0.23 * (ROS - RSO)), 0)
    return(CFB)
  }
  
  .CFBcalc <- function(
    FUELTYPE, FMC, SFC, ROS, CBH,
    option = "CFB") {
    CSI <- critical_surface_intensity(FMC, CBH)
    # Return at this point, if specified by caller
    if (option == "CSI") {
      .Deprecated("critical_surface_intensity")
      return(CSI)
    }
    RSO <- surface_fire_rate_of_spread(CSI, SFC)
    # Return at this point, if specified by caller
    if (option == "RSO") {
      .Deprecated("surface_fire_rate_of_spread")
      return(RSO)
    }
    CFB <- crown_fraction_burned(ROS, RSO)
    .Deprecated("crown_fraction_burned")
    return(CFB)
  }
  #BUI Effect
  buildup_effect <- function(FUELTYPE, BUI) {
    # Fuel Type String represenations
    d <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )
    # The average BUI for the fuel type - as referenced by the "d" list above
    BUIo <- c(
      72, 64, 62, 66, 56, 62, 106,
      32, 50, 50, 50, 50, 38, 63, 31, 01, 01
    )
    # Proportion of maximum possible spread rate that is reached at a standard BUI
    Q <- c(
      0.9, 0.7, 0.75, 0.8, 0.8, 0.8, 0.85,
      0.9, 0.8, 0.8, 0.8, 0.8, 0.75, 0.75, 0.75, 1.0, 1.0
    )
    names(BUIo) <- names(Q) <- d
    # Eq. 54 (FCFDG 1992) The Buildup Effect
    BE <- ifelse(
      BUI > 0 & BUIo[FUELTYPE] > 0,
      exp(50 * log(Q[FUELTYPE]) * (1 / BUI - 1 / BUIo[FUELTYPE])),
      1
    )
    return(as.numeric(BE))
  }
  
  .BEcalc <- function(...) {
    .Deprecated("buildup_effect")
    return(buildup_effect(...))
  }
  intermediate_surface_rate_of_spread_c6 <- function(ISI) {
    # Eq. 62 (FCFDG 1992) Intermediate surface fire spread rate
    RSI <- 30 * (1 - exp(-0.08 * ISI))**3.0
    return(RSI)
  }
  
  surface_rate_of_spread_c6 <- function(RSI, BUI) {
    # Eq. 63 (FCFDG 1992) Surface fire spread rate (m/min)
    RSS <- RSI * buildup_effect("C6", BUI)
    return (RSS)
  }
  
  crown_rate_of_spread_c6 <- function(ISI, FMC) {
    #Average foliar moisture effect
    FMEavg <- 0.778
    #Eq. 59 (FCFDG 1992) Crown flame temperature (degrees K)
    tt <- 1500 - 2.75 * FMC
    #Eq. 60 (FCFDG 1992) Head of ignition (kJ/kg)
    H <- 460 + 25.9 * FMC
    #Eq. 61 (FCFDG 1992) Average foliar moisture effect
    FME <- ((1.5 - 0.00275 * FMC)**4.)/(460 + 25.9 * FMC) * 1000
    #Eq. 64 (FCFDG 1992) Crown fire spread rate (m/min)
    RSC <- 60 * (1 - exp(-0.0497 * ISI)) * FME / FMEavg
    return (RSC)
  }
  
  crown_fraction_burned_c6 <- function(RSC, RSS, RSO) {
    CFB <- ifelse((RSC > RSS) & (RSS > RSO), crown_fraction_burned(RSS, RSO), 0)
    return(CFB)
  }
  
  rate_of_spread_c6 <- function(RSC, RSS, CFB) {
    # Eq. 65 (FCFDG 1992) Calculate Rate of spread (m/min)
    ROS <- ifelse(RSC > RSS, RSS + (CFB) * (RSC - RSS), RSS)
    return(ROS)
  }
  
  .C6calc <- function(
    FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC,
    option = "CFB") {
    # feels like this should make sense, but fails when called from fbp() and not all C6
    # stopifnot("C6" == FUELTYPE)
    RSI <- intermediate_surface_rate_of_spread_c6(ISI)
    # Return at this point, if specified by caller
    if (option == "RSI") {
      .Deprecated("intermediate_surface_rate_of_spread_c6")
      return(RSI)
    }
    RSC <- crown_rate_of_spread_c6(ISI, FMC)
    # Return at this point, if specified by caller
    if (option == "RSC") {
      .Deprecated("crown_rate_of_spread_c6")
      return(RSC)
    }
    RSS <- surface_rate_of_spread_c6(RSI, BUI)
    CSI <- critical_surface_intensity(FMC, CBH)
    RSO <- surface_fire_rate_of_spread(CSI, SFC)
    # Crown Fraction Burned
    CFB <- crown_fraction_burned_c6(RSC, RSS, RSO)
    # Return at this point, if specified by caller
    if (option == "CFB") {
      .Deprecated("crown_fraction_burned_c6")
      return(CFB)
    }
    .Deprecated("rate_of_spread_c6")
    ROS <- rate_of_spread_c6(RSC, RSS, CFB)
    return(ROS)
  }
  #ffmc
  fine_fuel_moisture_code <- function(ffmc_yda, temp, rh, ws, prec) {
    # Eq. 1
    wmo <- 147.2 * (101 - ffmc_yda) / (59.5 + ffmc_yda)
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
    ffmc1 <- (59.5 * (250 - wm)) / (147.2 + wm)
    # Constraints
    ffmc1 <- ifelse(ffmc1 > 101, 101, ffmc1)
    ffmc1 <- ifelse(ffmc1 < 0, 0, ffmc1)
    return(ffmc1)
  }
  
  .ffmcCalc <- function(...) {
    .Deprecated("fine_fuel_moisture_code")
    return(fine_fuel_moisture_code(...))
  }
  
  crown_fuel_consumption <- function(FUELTYPE, CFL, CFB, PC, PDF) {
    # Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
    CFC <- CFL * CFB
    CFC <- ifelse(
      FUELTYPE %in% c("M1", "M2"),
      # Eq. 66b (Wotton 2009) - CFC for M1/M2 types
      PC / 100 * CFC,
      ifelse(
        FUELTYPE %in% c("M3", "M4"),
        # Eq. 66c (Wotton 2009) - CFC for M3/M4 types
        PDF / 100 * CFC,
        CFC)
    )
    return(CFC)
  }
  
  total_fuel_consumption <- function(
    FUELTYPE, CFL, CFB, SFC, PC, PDF,
    option = "TFC") {
    CFC <- crown_fuel_consumption(FUELTYPE, CFL, CFB, PC, PDF)
    # Return CFC if requested
    if (option == "CFC") {
      return(CFC)
    }
    # Eq. 67 (FCFDG 1992) - Total Fuel Consumption
    TFC <- SFC + CFC
    return(TFC)
  }
  
  .TFCcalc <- function(...) {
    .Deprecated("total_fuel_consumption")
    return(total_fuel_consumption(...))
  }
  fire_intensity <- function(FC, ROS) {
    # Eq. 69 (FCFDG 1992) Fire Intensity (kW/m)
    FI <- 300 * FC * ROS
    return(FI)
  }
  
  .FIcalc <- function(...) {
    .Deprecated("fire_intensity")
    return(fire_intensity(...))
  }
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  if (!exists("FUELTYPE") | is.null(FUELTYPE)) {
    warning(
      paste0(
        "FuelType is a required input,",
        " default FuelType = C2 is used in the calculation"
      )
    )
    FUELTYPE <- rep("C2", n0)
  }
  FUELTYPE <- toupper(FUELTYPE)
  if (!exists("FFMC") | is.null(FFMC)) {
    warning(
      paste0(
        "FFMC is a required input, default FFMC = 90 is used in the",
        " calculation"
      )
    )
    FFMC <- rep(90, n0)
  }
  if (!exists("BUI") | is.null(BUI)) {
    warning(
      "BUI is a required input, default BUI = 60 is used in the calculation"
    )
    BUI <- rep(60, n0)
  }
  if (!exists("WS") | is.null(WS)) {
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    WS <- rep(10, n0)
  }
  if (!exists("GS") | is.null(GS)) {
    warning("GS is a required input,GS = 0 is used in the calculation")
    GS <- rep(0, n0)
  }
  if (!exists("LAT") | is.null(LAT)) {
    warning(
      "LAT is a required input, default LAT=55 is used in the calculation"
    )
    LAT <- rep(55, n0)
  }
  if (!exists("LONG") | is.null(LONG)) {
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    LONG <- rep(-120, n0)
  }
  if (!exists("DJ") | is.null(DJ)) {
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    DJ <- rep(180, n0)
  }
  if (!exists("ASPECT") | is.null(ASPECT)) {
    warning(
      "Aspect is a required input, Aspect = 0 is used in the calculation"
    )
    ASPECT <- rep(0, n0)
  }
  if (!exists("WD") | is.null(WD)) {
    WD <- rep(0, n0)
  }
  if (!exists("FMC") | is.null(FMC)) {
    FMC <- rep(0, n0)
  }
  if (!exists("ELV") | is.null(ELV)) {
    ELV <- rep(0, n0)
  }
  if (!exists("SD") | is.null(SD)) {
    SD <- rep(0, n0)
  }
  if (!exists("SH") | is.null(SH)) {
    SH <- rep(0, n0)
  }
  if (!exists("D0") | is.null(D0)) {
    D0 <- rep(0, n0)
  }
  if (!exists("HR") | is.null(HR)) {
    HR <- rep(1, n0)
  }
  if (!exists("PC") | is.null(PC)) {
    PC <- rep(50, n0)
  }
  if (!exists("PDF") | is.null(PDF)) {
    PDF <- rep(35, n0)
  }
  if (!exists("GFL") | is.null(GFL)) {
    GFL <- rep(0.35, n0)
  }
  if (!exists("CC") | is.null(CC)) {
    CC <- rep(80, n0)
  }
  if (!exists("THETA") | is.null(THETA)) {
    THETA <- rep(0, n0)
  }
  if (!exists("ACCEL") | is.null(ACCEL)) {
    ACCEL <- rep(0, n0)
  }
  if (!exists("BUIEFF") | is.null(BUIEFF)) {
    BUIEFF <- rep(1, n0)
  }
  if (!exists("CBH") | is.null(CBH)) {
    CBH <- rep(0, n0)
  }
  if (!exists("CFL") | is.null(CFL)) {
    CFL <- rep(0, n0)
  }
  if (!exists("ISI") | is.null(ISI)) {
    ISI <- rep(0, n0)
  }
  # Convert Wind Direction from degress to radians
  WD <- WD * pi / 180
  # Convert Theta from degress to radians
  THETA <- THETA * pi / 180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi / 180
  ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) {
    warning("Input variable Accel is out of range, will be assigned to 1")
  }
  ACCEL <- ifelse(!ACCEL %in% c(0, 1), 1, ACCEL)
  DJ <- ifelse(DJ < 0 | DJ > 366, 0, DJ)
  DJ <- ifelse(is.na(DJ), 180, DJ)
  D0 <- ifelse(is.na(D0) | D0 < 0 | D0 > 366, 0, D0)
  ELV <- ifelse(ELV < 0 | ELV > 10000, 0, ELV)
  ELV <- ifelse(is.na(ELV), 0, ELV)
  BUIEFF <- ifelse(BUIEFF <= 0, 0, 1)
  BUIEFF <- ifelse(is.na(BUIEFF), 1, BUIEFF)
  HR <- ifelse(HR < 0, -HR, HR)
  HR <- ifelse(HR > 366 * 24, 24, HR)
  HR <- ifelse(is.na(HR), 0, HR)
  FFMC <- ifelse(FFMC < 0 | FFMC > 101, 0, FFMC)
  FFMC <- ifelse(is.na(FFMC), 90, FFMC)
  ISI <- ifelse(is.na(ISI) | ISI < 0 | ISI > 300, 0, ISI)
  BUI <- ifelse(BUI < 0 | BUI > 1000, 0, BUI)
  BUI <- ifelse(is.na(BUI), 60, BUI)
  WS <- ifelse(WS < 0 | WS > 300, 0, WS)
  WS <- ifelse(is.na(WS), 10, WS)
  WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi, 0, WD)
  GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  CC <- ifelse(is.na(CC), 80, CC)
  GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35, GFL)
  LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  LAT <- ifelse(is.na(LAT), 55, LAT)
  LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  LONG <- ifelse(is.na(LONG), -120, LONG)
  THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA > 2 * pi, 0, THETA)
  SD <- ifelse(SD < 0 | SD > 1e+05, -999, SD)
  SD <- ifelse(is.na(SD), 0, SD)
  SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  SH <- ifelse(is.na(SH), 0, SH)
  
  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  if (length(FUELTYPE[is.na(FUELTYPE)]) > 0) {
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE <- ifelse(is.na(FUELTYPE), "C2", FUELTYPE)
  }
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  # Convert hours to minutes
  HR <- HR * 60
  # Corrections to reorient Wind Azimuth(WAZ) and Uphill slode azimuth(SAZ)
  WAZ <- WD + pi
  WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)
  SAZ <- ASPECT + pi
  SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  # Any negative longitudes (western hemisphere) are translated to positive
  #  longitudes
  LONG <- ifelse(LONG < 0, -LONG, LONG)
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Initializing variables
  ############################################################################
  SFC <- TFC <- HFI <- CFB <- ROS <- rep(0, length(LONG))
  RAZ <- rep(-999, length(LONG))
  validOutTypes <- c("SECONDARY", "ALL", "S", "A")
  # HACK: add options to ensure tests work for now
  validOutTypes <- c(validOutTypes, c("RAZ0", "WSV0"))
  if (output %in% validOutTypes) {
    FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <-
      BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- rep(
        0,
        length(LONG)
      )
    TI <- FTI <- BTI <- TTI <- LB <- WSV <- rep(-999, length(LONG))
  }
  #Modify input CBH based on accuracy of your modeled CBH, ie if you have CBH the equation will use it, if not it uses defaults
  #CBH<- ifelse(is.na(input$CBH),crown_base_height(FUELTYPE, CBH, SD, SH),input$CBH)
  CBH <- crown_base_height(FUELTYPE, CBH, SD, SH)

  #Modify input CFL based on accuracy of your modeled CFL, ie if you have CFL the equation will use it, if not it uses defaults
  CFL <- crown_fuel_load(FUELTYPE, CFL)
  #CFL<- ifelse(is.na(input$CFL),crown_fuel_load(FUELTYPE, CFL),input$CFL)
  FMC <- ifelse(
    FMC <= 0 | FMC > 120 | is.na(FMC),
    foliar_moisture_content(LAT, LONG, ELV, DJ, D0),
    FMC
  )
  FMC <- ifelse(FUELTYPE %in% c("D1", "S1", "S2", "S3", "O1A", "O1B"), 0, FMC)
  ############################################################################
  #                         END
  ############################################################################
  
  # Calculate Surface fuel consumption (SFC)
  SFC <- surface_fuel_consumption(FUELTYPE, FFMC, BUI, PC, GFL)
  # Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  slope_values <- slope_adjustment(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
                                   FMC, SFC, PC, PDF, CC, CBH, ISI)
  # Calculate the net effective windspeed (WSV)
  WSV0 <- slope_values[["WSV"]]
  if ("WSV0" == output) {
    return(WSV0)
  }
  WSV <- ifelse(GS > 0 & FFMC > 0, WSV0, WS)
  # Calculate the net effective wind direction (RAZ)
  RAZ0 <- slope_values[["RAZ"]]
  if ("RAZ0" == output) {
    return(RAZ0)
  }
  RAZ <- ifelse(GS > 0 & FFMC > 0, RAZ0, WAZ)
  # Calculate or keep Initial Spread Index (ISI)
  ISI <- ifelse(ISI > 0, ISI, initial_spread_index(FFMC, WSV, TRUE))
  # HACK: C6 ROS depends on CFB so do this to not repeat calculations
  ros_vars <- rate_of_spread_extended(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  ROS <- ros_vars$ROS
  CFB <- ifelse(CFL > 0, ros_vars$CFB, 0)
  CSI <- ros_vars$CSI
  RSO <- ros_vars$RSO
  # Calculate Total Fuel Consumption (TFC)
  TFC <- total_fuel_consumption(FUELTYPE, CFL, CFB, SFC, PC, PDF)
  # Calculate Head Fire Intensity(HFI)
  HFI <- fire_intensity(TFC, ROS)
  # Adjust Crown Fraction Burned
  CFB <- ifelse(HR < 0, -CFB, CFB)
  # Adjust RAZ
  RAZ <- RAZ * 180 / pi
  RAZ <- ifelse(RAZ == 360, 0, RAZ)
  # Calculate Fire Type (S = Surface, C = Crowning, I = Intermittent Crowning)
  FD <- rep("I", length(CFB))
  FD <- ifelse(CFB < 0.1, "S", FD)
  FD <- ifelse(CFB >= 0.9, "C", FD)
  # Calculate Crown Fuel Consumption(CFC)
  CFC <- total_fuel_consumption(
    FUELTYPE, CFL, CFB, SFC, PC, PDF,
    option = "CFC"
  )
  # Calculate the Secondary Outputs
  if (output %in% c("SECONDARY", "ALL", "S", "A")) {
    # Eq. 39 (FCFDG 1992) Calculate Spread Factor (GS is group slope)
    SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
    # Calculate The Buildup Effect
    BE <- buildup_effect(FUELTYPE, BUI)
    # Calculate length to breadth ratio
    LB <- length_to_breadth(FUELTYPE, WSV)
    LBt <- ifelse(
      ACCEL == 0,
      LB,
      length_to_breadth_at_time(FUELTYPE, LB, HR, CFB)
    )
    # Calculate Back fire rate of spread (BROS)
    BROS <- back_rate_of_spread(
      FUELTYPE, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH
    )
    # Calculate Flank fire rate of spread (FROS)
    FROS <- flank_rate_of_spread(ROS, BROS, LB)
    # Calculate the eccentricity
    E <- sqrt(1 - 1 / LB / LB)
    # Calculate the rate of spread towards angle theta (TROS)
    TROS <- ROS * (1 - E) / (1 - E * cos(THETA - RAZ))
    # Calculate rate of spread at time t for Flank, Back of fire and at angle
    # theta.
    ROSt <- ifelse(
      ACCEL == 0,
      ROS,
      rate_of_spread_at_time(FUELTYPE, ROS, HR, CFB)
    )
    BROSt <- ifelse(
      ACCEL == 0,
      BROS,
      rate_of_spread_at_time(FUELTYPE, BROS, HR, CFB)
    )
    FROSt <- ifelse(ACCEL == 0, FROS, flank_rate_of_spread(ROSt, BROSt, LBt))
    # Calculate rate of spread towards angle theta at time t (TROSt)
    TROSt <- ifelse(
      ACCEL == 0,
      TROS,
      (ROSt * (1 - sqrt(1 - 1 / LBt / LBt))
       / (1 - sqrt(1 - 1 / LBt / LBt) * cos(THETA - RAZ)))
    )
    # Calculate Crown Fraction Burned for Flank, Back of fire and angle theta.
    FCFB <- ifelse(
      CFL == 0,
      0,
      ifelse(FUELTYPE %in% c("C6"), 0, crown_fraction_burned(FROS, RSO))
    )
    BCFB <- ifelse(
      CFL == 0,
      0,
      ifelse(FUELTYPE %in% c("C6"), 0, crown_fraction_burned(BROS, RSO))
    )
    TCFB <- ifelse(
      CFL == 0,
      0,
      ifelse(FUELTYPE %in% c("C6"), 0, crown_fraction_burned(TROS, RSO))
    )
    # Calculate Total fuel consumption for the Flank fire, Back fire and at
    #  angle theta
    FTFC <- total_fuel_consumption(FUELTYPE, CFL, FCFB, SFC, PC, PDF)
    BTFC <- total_fuel_consumption(FUELTYPE, CFL, BCFB, SFC, PC, PDF)
    TTFC <- total_fuel_consumption(FUELTYPE, CFL, TCFB, SFC, PC, PDF)
    # Calculate the Fire Intensity at the Flank, Back and at angle theta fire
    FFI <- fire_intensity(FTFC, FROS)
    BFI <- fire_intensity(BTFC, BROS)
    TFI <- fire_intensity(TTFC, TROS)
    # Calculate Rate of spread at time t for the Head, Flank, Back of fire and
    #  at angle theta.
    HROSt <- ifelse(HR < 0, -ROSt, ROSt)
    FROSt <- ifelse(HR < 0, -FROSt, FROSt)
    BROSt <- ifelse(HR < 0, -BROSt, BROSt)
    TROSt <- ifelse(HR < 0, -TROSt, TROSt)
    
    # Calculate the elapsed time to crown fire initiation for Head, Flank, Back
    # fire and at angle theta. The (a# variable is a constant for Head, Flank,
    # Back and at angle theta used in the *TI equations)
    a1 <- 0.115 - (18.8 * CFB^2.5 * exp(-8 * CFB))
    TI <- log(ifelse(1 - RSO / ROS > 0, 1 - RSO / ROS, 1)) / (-a1)
    a2 <- 0.115 - (18.8 * FCFB^2.5 * exp(-8 * FCFB))
    FTI <- log(ifelse(1 - RSO / FROS > 0, 1 - RSO / FROS, 1)) / (-a2)
    a3 <- 0.115 - (18.8 * BCFB^2.5 * exp(-8 * BCFB))
    BTI <- log(ifelse(1 - RSO / BROS > 0, 1 - RSO / BROS, 1)) / (-a3)
    a4 <- 0.115 - (18.8 * TCFB^2.5 * exp(-8 * TCFB))
    TTI <- log(ifelse(1 - RSO / TROS > 0, 1 - RSO / TROS, 1)) / (-a4)
    
    # Fire spread distance for Head, Back, and Flank of fire
    DH <- ifelse(
      ACCEL == 1,
      distance_at_time(FUELTYPE, ROS, HR, CFB),
      ROS * HR
    )
    DB <- ifelse(
      ACCEL == 1,
      distance_at_time(FUELTYPE, BROS, HR, CFB),
      BROS * HR
    )
    DF <- ifelse(ACCEL == 1, (DH + DB) / (LBt * 2), (DH + DB) / (LB * 2))
  }
  # Create an id field if it does not exist
  if (!exists("ID") || is.null(ID)) {
    ID <- row.names(input)
  }
  # if Primary is selected, wrap the primary outputs into a data frame and
  #  return them
  if (output %in% c("PRIMARY", "P")) {
    FBP <- data.frame(ID, CFB, CFC, FD, HFI, RAZ, ROS, SFC, TFC)
    FBP[, c(2:3, 5:ncol(FBP))] <- apply(
      FBP[, c(2:3, 5:ncol(FBP))],
      2,
      function(.x) {
        ifelse(FUELTYPE %in% c("WA", "NF"), 0, .x)
      }
    )
    FBP[, "FD"] <- as.character(FBP[, "FD"])
    FBP[, "FD"] <- ifelse(FUELTYPE %in% c("WA", "NF"), "NA", FBP[, "FD"])
  } else if (output %in% c("SECONDARY", "S")) {
    # If Secondary is selected, wrap the secondary outputs into a data frame
    #  and return them.
    FBP <- data.frame(
      ID, BE, SF, ISI, FFMC, FMC, D0, RSO,
      CSI, FROS, BROS, HROSt, FROSt, BROSt, FCFB, BCFB,
      FFI, BFI, FTFC, BTFC, TI, FTI, BTI, LB, LBt, WSV,
      DH, DB, DF, TROS, TROSt, TCFB, TFI, TTFC, TTI
    )
    FBP[, 2:ncol(FBP)] <- apply(
      FBP[, 2:ncol(FBP)],
      2,
      function(.x) {
        ifelse(FUELTYPE %in% c("WA", "NF"), 0, .x)
      }
    )
  } else if (output %in% c("ALL", "A")) {
    # If all outputs are selected, then wrap all outputs into a data frame and
    # return it.
    FBP <- data.frame(
      ID, CFB, CFC, FD, HFI, RAZ, ROS, SFC,
      TFC, BE, SF, ISI, FFMC, FMC, D0, RSO, CSI, FROS,
      BROS, HROSt, FROSt, BROSt, FCFB, BCFB, FFI, BFI,
      FTFC, BTFC, TI, FTI, BTI, LB, LBt, WSV, DH, DB, DF,
      TROS, TROSt, TCFB, TFI, TTFC, TTI
    )
    FBP[, c(2:3, 5:ncol(FBP))] <- apply(
      FBP[, c(2:3, 5:ncol(FBP))],
      2,
      function(.x) {
        ifelse(FUELTYPE %in% c("WA", "NF"), 0, .x)
      }
    )
    FBP[, "FD"] <- as.character(FBP[, "FD"])
    FBP[, "FD"] <- ifelse(FUELTYPE %in% c("WA", "NF"), "NA", FBP[, "FD"])
  }
  return(FBP)
}

.FBPcalc <- function(...) {
  .Deprecated("fire_behaviour_prediction")
  return(fire_behaviour_prediction(...))
}
