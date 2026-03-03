#load packages
if (requireNamespace("Rothermel", quietly = TRUE)) library(Rothermel)
if (requireNamespace("plantecophys", quietly = TRUE)) library(plantecophys)
if (requireNamespace("firebehavioR", quietly = TRUE)) library(firebehavioR)
if (requireNamespace("geosphere", quietly = TRUE)) library(geosphere)
library(dplyr)
library(purrr)
library(ggplot2)
source_fun("Fine_Fuel_MC_SA_function.R")
source_fun("rothermel_function_mod.R")


#Canadian_Fuel_Model <- "C-4"   # Example required input
#Lat <- 54.0                    # Example required input
#Long <- -125.0                 # Example required input

#CBH <- 3                       # Canopy Base Height
#CFL <- 1.2                     # Canopy Fuel Load
#Preset <- "Yes"               # Use preset parameters
#Original13 <- "No"            # Use original 13 US models
#Plot <- "No"                  # Disable plotting
#BUI <- 60                     # Buildup Index
#FFMC <- 85                    # Fine Fuel Moisture Code
#FMC <- 98                      # Fuel Moisture Content
#Slope <- 15                   # Slope in degrees
#Aspect <- 180                 # Aspect in degrees
#Season <- "summer"           # Season
#Temp <- 25                    # Temperature in °C
#RH <- 30                      # Relative Humidity
#T_min <- 15      
#DMC <- 40                     # Duff Moisture Code
#CBD <- 0.1                    # Canopy Bulk Density
#litter <- 0.5                 # Litter load
#hr1 <- 0.3                    # 1-hr fuels
#hr10 <- 0.5                   # 10-hr fuels
#hr100 <- 0.7                  # 100-hr fuels
#FBDepth <- 0.4    

# Function to Generate the best fitting U.S Fuel Model Based on an input Canadian fuel model, CBH, and CFL
 Best_US_Model <- function(
    Canadian_Fuel_Model,        # Required: Canadian Fuel Model code (e.g., "C-7")
    Lat,                        # Required: Latitude 
    Long,                       # Required: Longitude 
    Preset,
    CBH = NULL,                 # Canopy Base Height 
    CFL = NULL,                 # Canopy Fuel Load 
    Original13 = NULL,          # Use original 13 fuel models? 
    Plot = NULL,                # Generate plots? 
    BUI = NULL,                 # Buildup Index
    FFMC = NULL,                # Fine Fuel Moisture Code 
    FMC = NULL,                 # Fuel Moisture Content 
    Slope = NULL,               # Slope in degrees 
    Aspect = NULL,              # Aspect in degrees 
    Season = NULL,              # Season 
    Temp = NULL,                # Temperature in °C
    RH = NULL,                  # Relative Humidity in % 
    T_min = NULL,               # Minimum daily temperature 
    DMC = NULL,                 # Duff Moisture Code
    CBD = 0.1,                  # Canopy Bulk Density 
    litter = NULL,              # Litter load 
    hr1 = NULL,                 # 1-hr fuel load 
    hr10 = NULL,                # 10-hr fuel load 
    hr100 = NULL,               # 100-hr fuel load 
    FBDepth = NULL              # Fuel bed depth 
) {

  #key to adjust for NF's
  if(Canadian_Fuel_Model == "NF" | Canadian_Fuel_Model == "WA"){
    return(list(
      Best_Model = "NF"))
  }
   #Remove Hyphen from model
   hyphen_cad_models<-c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7",
                        "D-1", "D-2", "M-1", "M-2", "M-3", "M-4", "S-1", "S-2", "S-3", "O-1b", "O-1a","NF", "WA")
   
   if(Canadian_Fuel_Model %in% hyphen_cad_models){
     Canadian_Fuel_Model<-gsub("-", "",Canadian_Fuel_Model)
   }
   
   # Define valid Canadian fuel models
   valid_canadian_models <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7",
                              "D1", "D2", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1b", "O1a","NF", "WA")
   
   # Validate Canadian_Fuel_Model input
   if (!Canadian_Fuel_Model %in% valid_canadian_models) {
     stop("Invalid Canadian Fuel Model:",Canadian_Fuel_Model ," Please provide one of the following: ", 
          paste(valid_canadian_models, collapse = ", "))
   }
   
  # Preset CBH and CFL data
  CAN_Canopy_Data <- data.frame(
    Fuel_Type = c("C1", "C2", "C3", "C4", "C5", "C6", "C7",
                  "D1", "D2", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1a", "O1b"),
    CBH = c(2, 3, 8, 4, 18, 6, 10,
            0, 0,6, 6, 6, 6, 0, 0, 0, 0, 0),
    CFL = c(0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5,
            0, 0,0.8, 0.8, 0.8, 0.8, 0, 0, 0, 0, 0)
  )
  
  # If CBH is not provided, retrieve preset CBH
  if (is.null(CBH)) {
    CBH <- CAN_Canopy_Data$CBH[CAN_Canopy_Data$Fuel_Type == Canadian_Fuel_Model]
    print(paste("Using preset Canopy Base Height for", Canadian_Fuel_Model))
  }
  
  # If CFL is not provided, retrieve preset CFL
  if (is.null(CFL)) {
    CFL <- CAN_Canopy_Data$CFL[CAN_Canopy_Data$Fuel_Type == Canadian_Fuel_Model]
    print(paste("Using preset Canopy Fuel Load for", Canadian_Fuel_Model))
  }
  
  #key to adjust for errors with CBH and CFL
  if(CFL == 0){
    CFL=NULL
  }
  
  if(is.null(CBD) | CBD == 0){
    CBD=0.1
  }
  
  if(is.null(FMC)){
    FMC=90
  }
  if(is.null(Temp)){
    Temp=25
  }
  if(is.null(RH)){
    RH=30
  }
  if(is.null(T_min)){
    T_min=10
  }
  if(is.null(BUI)){
    BUI=120
  }
  if(is.null(FFMC)){
    FFMC=90
  }
  if(is.null(DMC)){
    DMC=90
  }

  
  # Validate CBH and CFL inputs
  if (!is.numeric(CBH) || CBH < 0) {
    stop("CBH must be a non-negative numeric value.")
  }
  if (!is.numeric(CFL) || CFL < 0) {
    stop("CFL must be a non-negative numeric value.")
  }
  
  #change slopes to maximize model fit: Certain American Models fit better then using static 45
    #applies to C5 and C7 stands
  slope_changed_models<-c("C5","C7")
  
  if(Canadian_Fuel_Model %in% slope_changed_models){
    Slope = 25
    Aspect=135
    
  }else{
    Slope=45
    Aspect=135
  }
  
#Pull Fire Behavior for preset conditions--------------------------------------------
  #pull in static ROS @ preset conditions: FFMC=90, BUI=120, DMC=100, FMC=90, Temp =25, RH =30
  static_data<-readRDS("Z:/General-GIS/Holden's Data/Fuel Typing Crosswalk/Canada_ROS_dataset.rds")

  #extract specific conditions
    CAN_ROS<- cbind(static_data[1],static_data[[Canadian_Fuel_Model]])
    colnames(CAN_ROS)<-c("WS",Canadian_Fuel_Model)

      #Run Rothermel Rate of Spread at specific conditions to test US fuel model
    
    #Setup Presets:
  
        
    #FOREST TYPE AND FUEL TYPE CROSSOVERS
    Pine<-c("GR1", "GR2", "GR3" ,"GR4", "GR5",
            "GR6", "GR7", "GR8" ,"GR9", "GS1", "GS2", "GS3", "GS4", "SH1"
            ,"SH2", "SH3" ,"SH4" ,"SH5" ,"SH6" ,"SH7" ,"SH8", "SH9","TL1","TL4","TL5","TL8","SB1","SB2",
            "SB3","SB4","A1","A2","A3","A4","A5","A6","A10","A11","A12","A13","TU3","S1", "S2", "S3","O1a", "O1b","C6","C3", "C4")
    Spruce<-c("TU4","A7","A8","C1","C2")
    Mixed<-c("TU2","TU1","M1", "M2", "M3", "M4","C5")
    Douglas_fir<-c("TU5","TL3","TL7","C7")
    Deciduous<-c("TL2","TL6","TL9","A9","D1", "D2")
    
    #preset conditions: non-changeable since we want to test fire behavior under these conditions
    WS=seq(0:60)
    photo_per <- geosphere::daylength(Lat, "2024-07-15")
    VPD <- (RHtoVPD(RH, Temp, Pa = 101)) * 1000
    i_pp <- ifelse(photo_per > 11, 1, ifelse(photo_per < 10, 0, photo_per - 10))
    i_VPD <- ifelse(VPD > 4100, 0, ifelse(VPD < 900, 1, (VPD * (-1 / 3200) + 1.2813)))
    i_Tmin <- ifelse(T_min > 5, 1, ifelse(T_min < -2, 0, (T_min * (1 / 6) + 0.3333)))
    GSI <- i_Tmin * i_pp * i_VPD
    herb_live <- ifelse(GSI < 0.5, 30, ((440 * GSI) - 190))
    woody_live <- ifelse(GSI < 0.5, 60, ((280 * GSI) - 30))
    
    # Compute necessary values
    ForestType <- case_when(
      Canadian_Fuel_Model %in% Pine ~ "Pine",
      Canadian_Fuel_Model %in% Douglas_fir ~ "Douglas-fir",
      Canadian_Fuel_Model %in% Spruce ~ "Spruce",
      Canadian_Fuel_Model %in% Deciduous ~ "Deciduous",
      Canadian_Fuel_Model %in% Mixed ~ "Mixed",
      TRUE ~ "Non-Fuel"
    )
    
    #Get Exportable weather data
    effm <- FFMC_sa(ForestType, "Summer", ffmc = FFMC, dmc = DMC, density = "Dense")
    #Assume 0.5 meter FB Depth, 20 m trees, 80% crown ratios, and 75% density
    wind_adj <- firebehavioR::waf(0.5, 20, 80, 75)
    Weather_data<-data.frame(hr1=effm,hr10=effm+1,hr100=effm+2,herblive=herb_live,woodylive=woody_live,waf=wind_adj)
    
    
    
    #get U.S Fuel models
    if(Original13 =="Yes"){
    U.S_Fuel_Models<-unique(row.names(fuelModels))
    }else{
      U.S_Fuel_Models<-unique(row.names(fuelModels))[-(1:13)]
    }
    
    #US_Model<-U.S_Fuel_Models[1]
    # Function to process US Fuel Model
    process_fuel_model <- function(US_Model) {
      # Base data for the U.S. fuel model
      Fuels_data <- fuelModels[US_Model, 1:16]
    
      #if fuels data exists this pulls it in: and convert to tons/ha
      if(!is.null(hr1)){
        Fuels_data <- fuelModels[US_Model, 1:16]
        Fuels_data$loadLitter<-litter*10
        Fuels_data$load1hr<-hr1*10
        Fuels_data$load10hr<-hr10*10
        Fuels_data$load100hr<-hr100*10
      }
      
      FT <- case_when(
        US_Model %in% Pine ~ "Pine",
        US_Model %in% Douglas_fir ~ "Douglas-fir",
        US_Model %in% Spruce ~ "Spruce",
        US_Model %in% Deciduous ~ "Deciduous",
        US_Model %in% Mixed ~ "Mixed",
        TRUE ~ "Non-Fuel"
      )
      
      if (FT == "Non-Fuel"){return(NULL)}  # Skip non-fuel models
      
      # Assign density based on the fuel model
      Density <- case_when(
        US_Model %in% Pine ~ "Light",
        US_Model %in% Douglas_fir ~ "Moderate",
        US_Model %in% Spruce ~ "Dense",
        TRUE ~ "Dense"
      )
      
      # Compute necessary values
      #fine fuel moisture
      #1:
      #effm <- FFMC_sa(FT, Season, ffmc = FFMC, dmc = DMC, density = Density)
      #2:
      effm<- firebehavioR::ffm(method = "anderson",
                              rh=RH,
                              temp = Temp,
                              month = 7,
                              hour= 15,
                              asp = Aspect,
                              slp= Slope,
                              bla= "b", 
                              shade= "y")
      
      #wind adj
      wind_adj <- firebehavioR::waf(Fuels_data$fuelBedDepth, 20, 80, 75)
      
      # Combine moisture data
      Moisture_data <- data.frame(
        litter = effm$fmLitter,
        hr1 = effm$fm1hr,
        hr10 = effm$fm10hr,
        hr100 = effm$fm100hr,
        live_herb = herb_live,
        live_woody = woody_live
      )
      
      # Create Canopy, Wind, and Environmental Data
      Canopy_Wind_Env <- data.frame(
        US_Model = rep(US_Model,length(WS)),
        CAN_Model = rep(Canadian_Fuel_Model,length(WS)),
        cbd = rep(CBD,length(WS)),
        fmc =rep(FMC,length(WS)),
        cbh = rep(CBH,length(WS)),
        cfl = rep(CFL,length(WS)),
        slope = rep(Slope,length(WS)),
        ws = WS,
        wind_d = rep(Aspect,length(WS)),
        wind_adj = rep(wind_adj,length(WS))
      )
      
      # Combine fuel and moisture data
      Moisture_fuels_data <- cbind(Moisture_data, Fuels_data)
      
      # Replicate the first row for each wind speed
      Moisture_fuels_extended <- Moisture_fuels_data[rep(1, length(WS)), ]
      
      # Combine the final data
      Combined_data <- cbind(Canopy_Wind_Env, Moisture_fuels_extended)
  
      return(Combined_data)
    }
    
    # Apply the function to all fuel models and store in a list
    Master_Fuel_List <- purrr::map(U.S_Fuel_Models, process_fuel_model)
    
    # Remove NULLs if any fuel model was skipped
    Master_Fuel_List <- Filter(Negate(is.null), Master_Fuel_List)
    
    # function to calculate Rothermel Rate of Spread
    calculate_ros <- function(data) {
      data_raw<-data
      fuel_model<-data_raw[1,1]
      # Run Rothermel function
      Fire_Behavior <- rothermel_mod(
        surfFuel = data_raw[, 17:32],
        moisture = data_raw[, 11:16],
        crownFuel = data_raw[, 3:6],
        enviro = data_raw[, 7:10],
        rosMult = 1.7,
        cfbForm = "w",
        folMoist = "n"
      )
      
      # Create exportable data frame
      Output_Data <- data.frame(
        WS = data_raw[, 7:10]$ws,
        ROS = Fire_Behavior$fireBehavior$`Rate of Spread [m/min]`
      )
      colnames(Output_Data)<-c("WS",fuel_model)
      return(Output_Data)
    }
    
    Master_ROS_Results <- purrr::map(Master_Fuel_List, calculate_ros)  
    
    Merged_US_ROS_Data<-Reduce(function(x, y) merge(x, y, by = "WS"), Master_ROS_Results)

    
#Run Fire Behavior Model for User Input conditions------------------------------------------------------
if(Preset == "No"){
  input_fuel_model  <- gsub("([A-Z])([0-9])", "\\1-\\2", Canadian_Fuel_Model)
  input_data<-data.frame(
    FuelType=input_fuel_model,
    LAT=Lat,
    LONG=Long,
    FFMC=FFMC,
    BUI=BUI,
    WS=seq(0:60),
    GS=Slope,
    Dj=200, #Julain Date preset to July 15th, 2024 for the middle of summer
    Aspect=Aspect,
    FMC=FMC
  )
  
  fbp_results<-fbp(input = input_data, output = "Primary")
  fbp_results$ID <- as.numeric(fbp_results$ID)
  fbp_results<- fbp_results %>% arrange(ID)
  
  FBP_results<-data.frame(WS=seq(0:60),ROS=fbp_results$ROS)
  colnames(FBP_results)<- c("WS",Canadian_Fuel_Model)
  CAN_ROS<-FBP_results
} else{
  CAN_ROS<- cbind(static_data[1],static_data[[Canadian_Fuel_Model]])
  colnames(CAN_ROS)<-c("WS",Canadian_Fuel_Model)
}
    
#Calculate Best Fitting U.S Model-----------------------------    
    

    #Merge FBP and US Roth data frame to compare best models
    All_ROS_Data<-merge(CAN_ROS,Merged_US_ROS_Data)
    
    #Function to calculate best fit with RMSE and difference
    calculate_best_fit <- function(canadian_model, data) {
      # Filter data for the given Canadian model
      ROS_data_canadian <- data[, c("WS", canadian_model)]
      
      # Initialize variables to track the best fit
      min_rmse <- Inf
      min_avg_percentage_diff <- Inf
      min_actual_diff <- Inf
      Best_US_Model <- NA
      
      # Iterate over each U.S. model to find the best fit
      for (US_Model in setdiff(colnames(data), c("WS", canadian_model))) {  # Exclude "WS" and Canadian model columns
        # Extract ROS data for the U.S. model
        ROS_data_us <- data[, c("WS", US_Model)]
        if(is.na(ROS_data_us[1,2])){
          next
        }
        
        # Compute RMSE between Canadian and U.S. ROS values
        squared_differences <- (ROS_data_canadian[[canadian_model]] - ROS_data_us[[US_Model]])^2
        rmse <- sqrt(mean(squared_differences, na.rm = TRUE))  # Root Mean Square Error
        
        # Compute percentage difference and average it
        percentage_differences <- abs((ROS_data_canadian[[canadian_model]] - ROS_data_us[[US_Model]]) / ROS_data_canadian[[canadian_model]]) * 100
        avg_percentage_diff <- mean(percentage_differences, na.rm = TRUE)
        
        # Compute actual difference (mean absolute difference)
        actual_diff <- mean(abs(ROS_data_canadian[[canadian_model]] - ROS_data_us[[US_Model]]), na.rm = TRUE)
        
        # Check if this is the best fit so far
        if (rmse < min_rmse || 
            (rmse == min_rmse && avg_percentage_diff < min_avg_percentage_diff) ||
            (rmse == min_rmse && avg_percentage_diff == min_avg_percentage_diff && actual_diff < min_actual_diff)) {
          min_rmse <- rmse
          min_avg_percentage_diff <- avg_percentage_diff
          min_actual_diff <- actual_diff
          Best_US_Model <- US_Model
        }
      }
      
      # Create the results data frame
      Results <- data.frame(
        CAN_FBP_FT = canadian_model,
        US_Model = Best_US_Model,
        RMSE = min_rmse,
        Mean_Percentage_Diff = min_avg_percentage_diff,
        Mean_Diff = min_actual_diff
      )
      
      # Extract ROS data for the Canadian and best U.S. models
      ROS_Data <- data[, c("WS", canadian_model, Best_US_Model)]
      colnames(ROS_Data) <- c("WS", canadian_model, Best_US_Model)
      
      # Return a list with results and ROS data
      return(list(
        Results = Results,
        ROS_Data = ROS_Data
      ))
    }
    
    # Apply the function for each Canadian model
    Best_Model_Results <- calculate_best_fit(Canadian_Fuel_Model,All_ROS_Data)
  
    
#Plot results and export
  if(Plot=="Yes"){
    
    # Dynamically retrieve column names for Canadian and U.S. models
    canadian_model_name <- colnames(Best_Model_Results$ROS_Data)[2]
    us_model_name <- colnames(Best_Model_Results$ROS_Data)[3]
    
    # Create the plot
    p <- ggplot(Best_Model_Results$ROS_Data, aes(x = WS)) +
      geom_line(aes(y = !!sym(canadian_model_name), color = canadian_model_name), size = 1) +
      geom_line(aes(y = !!sym(us_model_name), color = us_model_name), size = 1, linetype = "dashed") +
      labs(
        title = paste("Rate of Spread Curve:", canadian_model_name, "vs", us_model_name),
        subtitle = "Best Fit US Model Compared to Candian FBP Fuel Type",
        x = "Wind Speed (km/hr)",
        y = "Rate of Spread (m/min)",
        color = "Model"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Display the plot
    print(p)
    
    #export the resupts
    return(list(
      Results = Best_Model_Results,
      Best_Model = Best_Model_Results$Results$US_Model,
      Plot = p,
      WeatherConditions=Weather_data
    ))
    
  }else{
    #return just the results
    return(list(
      Results = Best_Model_Results,
      Best_Model = Best_Model_Results$Results$US_Model,
      WeatherConditions=Weather_data
    ))
  }
}

