step1 <- function(cfg, root) {
cat("Running Step 1: Process To FuelCalc\n")

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
  a
}

process_cfg <- cfg$process_to_fuelcalc %||% list()
target_env <- environment()

as_char_vec <- function(x, default) as.character(unlist(x %||% default, use.names = FALSE))
as_num_vec <- function(x, default) as.numeric(unlist(x %||% default, use.names = FALSE))
as_logical_vec <- function(x, default) {
  vals <- unlist(x %||% default, use.names = FALSE)
  if (is.logical(vals)) return(as.logical(vals))
  tolower(as.character(vals)) %in% c("true", "t", "1", "yes", "y")
}
find_template <- function(candidates) {
  template_root <- cfg$runtime$templates_dir %||% file.path(root, "templates")
  for (nm in candidates) {
    p <- file.path(template_root, nm)
    if (file.exists(p)) return(p)
  }
  stop("Missing template file under ", template_root, ". Tried: ", paste(candidates, collapse = ", "))
}
source_local_function <- function(fname) {
  f <- file.path(root, "R_functions", fname)
  if (!file.exists(f)) stop("Missing R_functions file: ", f)
  source(f, local = target_env)
}
source_optional_function <- function(fname) {
  f <- file.path(root, "R_functions", fname)
  if (!file.exists(f)) {
    warning("Optional R_functions file missing: ", f)
    return(invisible(FALSE))
  }
  tryCatch(
    {
      source(f, local = target_env)
      TRUE
    },
    error = function(e) {
      warning("Optional function source failed for ", fname, ": ", conditionMessage(e))
      FALSE
    }
  )
}

#CALL CONFIG
project_name <- cfg$project_name %||% ""
if (!nzchar(project_name)) stop("config project_name is required for Process To FuelCalc")

name <- project_name
project <- name

#Libraries Loading
library(dplyr)

#UI Inputs(Modifiable):-----------------------------------

#Are you cutting: TRUE or FALSE
cutting <- isTRUE(process_cfg$cutting %||% TRUE)

#What are your treatment names?
tr_names <- as_char_vec(process_cfg$tr_names, c("A","B","C"))

#What is the density you'd like to thin to in TPH? 1 input per stratum
thinning_target_order <- as_num_vec(process_cfg$thinning_target_order, c(593,647,683))
#What height are you pruning to in meters? 1 input per stratum
prune <- as_num_vec(process_cfg$prune, c(2,2,2))
#Are you burning: TRUE or FALSE 1 input per stratum
BurningFlags <- as_logical_vec(process_cfg$burning_flags, c(FALSE,FALSE,FALSE))
#Are you thinning: TRUE or FALSE 1 input per stratum
ThinFlags <- as_logical_vec(process_cfg$thin_flags, c(TRUE,TRUE,TRUE))
#Are you pruning: TRUE or FALSE 1 input per stratum
PruneFlags <- as_logical_vec(process_cfg$prune_flags, c(TRUE,TRUE,TRUE))

#Emssion factor:
#either 4:WesternForestWF, 3:WesternForestRX, 2:BorealForest, 6:Grassland, 5:Shrubland
#Notes if you are doing a burn use 3, if you are planning for fire use 4, if you are in boreal use 2
EmissionFactor <- as_num_vec(process_cfg$emission_factor, c(4,4,4))

#Type of surface fuel load:
#Either (Grass)=grass-dominated,(Chapparral) = chapparral and shrubfeilds,
#(TimberLitter) = timber litter, or (Slash)= logging slash
surf_fuel <- as_char_vec(process_cfg$surf_fuel, c("TimberLitter","TimberLitter","TimberLitter"))

#Bole Char Height:
#Notes:if you are burning or planning for fire what flame lengths are you okay with in meters
#2 meters based on  3m pruning
BoleCharHeight <- as_num_vec(process_cfg$bole_char_height, c(2,2,2))

#What Moisture level are your 10hr fuels?: Either VeryDry (6%), Dry (10%), Moderate (16%), or Wet(22%)
moist <- as.character(process_cfg$moist %||% "VeryDry")

#What Climate Type is this?: Either  Humid or Arid
climate <- as_char_vec(process_cfg$climate, c("Arid","Arid","Arid"))

#If you are Burning:-----------------

#What Moisture level are your 10hr fuels?: Either VeryDry (6%), Dry (10%), Moderate (16%), or Wet(22%)
moistRegimes <- as_char_vec(process_cfg$moist_regimes, c("VeryDry","VeryDry","VeryDry"))

#What season are you burning?: Either Spring, Summer, Fall, Winter
Season <- as_char_vec(process_cfg$season, c("Summer","Summer","Summer"))

#-------------------------------------------------------------------

#Templates and Functions:
FUELCALC_BC_TEMPLATE <- readLines(find_template(c("FuelCalcBC_Template_FCP.fcp", "FuelCalcBC_Template_FCP.FCP")), warn = FALSE)
ffi_temp <- readLines(find_template(c("FuelCalc_FFI_Template.ffi", "FuelCalc_FFI_Template.FFI")), warn = FALSE)
treatment_temp <- read.csv(find_template(c("Treatment_Template_FuelCalc.csv")))
tre_temp <- readLines(find_template(c("FuelCalc_TRE_Template.tre")), warn = FALSE)
FUELCALC_US_TEMPLATE <- readLines(find_template(c("FuelCalc_Template_FCP.fcp", "FuelCalc_Template_FCP.FCP")), warn = FALSE)

source_local_function("fueltypes_crosswalk_function.R")
source_optional_function("Crown_Area_Function.R")
source_optional_function("BC_TREECODES_USCodes_function.R")
source_optional_function("TreeVolume_Calculator_Function_NVEL.R")


#-------------------

#Load Project Name
name <- project_name

#Manual Functions-------------------------------------------
#Function to extract metric value
extract_metric <- function(model, metric_name){
  if(metric_name == "CFC"){
    return(model$CFC)
  } else if(metric_name == "CFB"){
    return(model$CFB)
  } else if(metric_name == "Intensity_Crown"){
    return(model$Intensity_Crown)
  } else if(metric_name == "Intensity_Surface"){
    return(model$Intensity_Surface)
  } else if(metric_name == "ROS"){
    return(model$ROS)
  } else if(metric_name == "Flame_Length"){
    return(model$Flame_Length)
  } else if(metric_name == "pCrown"){
    return(model$pCrown)
  } else if(metric_name == "SFC"){
    return(model$SFC)
  } else if(metric_name == "FireFlag"){
    return(model$FLAG)}
  else if(metric_name == "CritFlameLength"){
    return(model$CritFlameLength)
  } else if(metric_name == "FuelConsumed_kg_m2"){
    return(model$FuelConsumed_kg_m2)
  } else {
    # Default to CFC if metric not recognized
    warning(paste("TargetMetric", metric_name, "not recognized. Defaulting to CFC."))
    return(model$CFC)
  }
}



#rounding cutting specs to nearest multiple of 10 and setting it into fuelcalc format
round_to_nearest_10_custom <- function(x) {
  if (x == 95) {
    return(90)  # Special case for 95
  } else {
    return(ceiling(x / 10) * 10)  # Round up to nearest multiple of 10
  }
}

# Custom function to rename and merge columns for species
rename_and_merge_species <- function(cuts) {
  # Get the original species names (excluding DBH.Class)
  spp_original <- cuts %>% dplyr::select(-DBH.Class) %>% colnames()
  
  # Rename species names according to the provided pattern-replacement rules
  species <- spp_original
  species <- gsub("Ep", "Eb", species)
  species <- gsub("Act", "At", species)
  species <- gsub("Acb", "At", species)
  species <- gsub("Dead", "Eb", species)
  species <- gsub("Ac", "At", species)
  species <- gsub("DU", "Eb", species)
  species <- gsub("DP", "Eb", species)
  species <- gsub("Lt", "Lw", species) #change larch to western larch
  species <- gsub("Atb", "At", species)
  species <- gsub("Fdi", "Fd", species) #Fdi to Fd
  
  # Rename columns in the cuts data frame
  colnames(cuts)[2:ncol(cuts)] <- species  # The first column is DBH.Class, so skip it
  
  # Remove duplicate columns based on their names (keep only the first occurrence)
  cuts <- cuts[, !duplicated(colnames(cuts))]  
  return(cuts)
}

is_within_range <- function(value, min_val, max_val) {
  value >= min_val & value <= max_val
}
#--------------------------------------------------



#File Paths:
#root<-normalizePath(getwd())
path = file.path(root,"projects",name,"data","raw")

# Prefix for file paths
snap_prefix = "/SNAP/"
FWI_prefix = "/Weather/"
Fuel_prefix = "/FireBehavior/Inputs/"
cath_prefix = "/Cut Specs for run/"
Fire_out="/FireBehavior/Outputs/"
s_s_prefix = "/Stand_StockTables/"
out_slash = paste0(path, "/FuelCalcBC/Outputs/Slash/")
out_fuelcalc = paste0(path,"/FuelCalcBC/Outputs/")
fuelcalc = paste0(path,"/FuelCalcBC/")
out_weather = paste0("/Weather/")
out_residuals =paste0(path, "/FuelCalcBC/Outputs/Slash/Residuals/")
results = "/Outputs/"
path_fcp <- paste0(path,"/FuelCalcBC/")

# Load in SNAP summary files
Snap_OS = read.csv(paste0(path,snap_prefix,project,"_OS.csv"))
Snap_US = read.csv(paste0(path,snap_prefix,project,"_US.csv"))
Snap_EX= read.csv(paste0(path,snap_prefix,project,"_EXTRA.csv"))
Snap_fuels= read.csv(paste0(path,snap_prefix,project,"_FUELS.csv"))

#US and BC FuelCalc Setup Script:

# Step 1.1  - Remove treatments that won't be modeled
##############################################################################################

# Note: Remove strata (treatment units) if either 1. No treatment, or 2. No associated cutting table

# Note: Stratum name to identify treatment units that won't be modeled is variable, loaded data
# should be checked to ensure only relevant strata are present and that others are removed!

# REMOVE XCLUDED
if (NROW(which(Snap_OS$Stratum == "Excluded"))>0){
  Snap_OS = Snap_OS[-which(Snap_OS$Stratum == "Excluded"),]
  Snap_US = Snap_US[-which(Snap_US$Stratum == "Excluded"),]
}
# REMOVE "all plots"
if (NROW(which(Snap_OS$Stratum == "all plots"))>0){
  Snap_OS = Snap_OS[-which(Snap_OS$Stratum == "all plots"),]
  Snap_US = Snap_US[-which(Snap_US$Stratum == "all plots"),]
}
# REMOVE "RESERVE"
if (NROW(which(Snap_OS$Stratum == "RESERVE"))>0){
  Snap_OS = Snap_OS[-which(Snap_OS$Stratum == "RESERVE"),]
  Snap_US = Snap_US[-which(Snap_US$Stratum == "RESERVE"),]
}
# REMOVE "unused"
if (NROW(which(Snap_OS$Stratum == "unused"))>0){
  Snap_OS = Snap_OS[-which(Snap_OS$Stratum == "unused"),]
  Snap_US = Snap_US[-which(Snap_US$Stratum == "unused"),]
}

# Remove forward slashes from strata names
Snap_OS$Stratum = gsub("\\\\", "-", Snap_OS$Stratum)
Snap_OS$Stratum = gsub("/", "-", Snap_OS$Stratum)
Snap_US$Stratum = gsub("\\\\", "-", Snap_US$Stratum)
Snap_US$Stratum = gsub("/", "-", Snap_US$Stratum)

##############################################################################################
# Step 1.2  - Fill in missing BAF values
##############################################################################################

# Note: Fill in any missing BAF values (overstorey and understorey data frames)

# Overstorey
# For each strata...
for (x in 1:length(unique(Snap_OS$Stratum))){
  
  # Subset all rows within a treatment unit
  tmp = Snap_OS[which(Snap_OS$Stratum==unique(Snap_OS$Stratum)[x]),]
  
  if(all(is.na(tmp$BAF))){
    tmp$BAF<-"NO BAF"
  } else{
    # Populate the empty BAF values with existing ones filled in at other plots
    tmp$BAF[is.na(tmp$BAF)] <- tmp$BAF[which(any(!is.na(tmp$BAF)))]
  }
  # Place the subset back into the original data frame
  Snap_OS[which(Snap_OS$Stratum==unique(Snap_OS$Stratum)[x]),] = tmp
}

# Understorey
# For each strata...
for (x in 1:length(unique(Snap_US$Stratum))){
  
  # Subset all rows within a treatment unit
  tmp = Snap_US[which(Snap_US$Stratum==unique(Snap_US$Stratum)[x]),]
  
  # If: BAF value is empty in all rows then assign 0 value
  if (length(tmp$BAF[which(is.na(tmp$BAF))]) == NROW(tmp)){
    tmp$BAF = 0
    
    # Else: Populate the empty BAF values with existing ones filled in at other plots
  } else {
    tmp$BAF[is.na(tmp$BAF)] <- tmp$BAF[which(any(!is.na(tmp$BAF)))]
    
  }
  
  # Place the subset back into the original data frame
  Snap_US[which(Snap_US$Stratum==unique(Snap_US$Stratum)[x]),] = tmp
}

##############################################################################################
# Step 1.3  - Fill in missing fuel load values
##############################################################################################

# Note: Fill in any missing fuel load values (overstorey and understorey data frames)

# Overstorey
# For each strata...

for (x in 1:length(unique(Snap_fuels$Stratum))){
  
  # Subset all rows within a treatment unit
  tmp = Snap_fuels[which(Snap_fuels$Stratum==unique(Snap_fuels$Stratum)[x]),]
  
  #get average fuels
  hr1_mean<-mean(tmp$Avg.1.hr.fuels...0.6.cm...kg.m2.,na.rm=T)
  hr10_mean<-mean(tmp$Avg.10.hr.fuels..0.6.2.5cm...kg.m2.,na.rm=T)
  hr100_mean<-mean(tmp$Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.,na.rm=T)
  hr1000_mean<-mean(tmp$X1000.hr.fuels...7.6.cm.kg.m2.,na.rm=T)
  
  # Default all missing values for 1/10/100/1000 hr fuels to averages kg/m2
  tmp$Avg.1.hr.fuels...0.6.cm...kg.m2.[is.na(tmp$Avg.1.hr.fuels...0.6.cm...kg.m2.)] <- hr1_mean
  tmp$Avg.10.hr.fuels..0.6.2.5cm...kg.m2.[is.na(tmp$Avg.10.hr.fuels..0.6.2.5cm...kg.m2.)] <- hr10_mean
  tmp$Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.[is.na(tmp$Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.)] <- hr100_mean
  tmp$X1000.hr.fuels...7.6.cm.kg.m2.[is.na(tmp$X1000.hr.fuels...7.6.cm.kg.m2.)] <- hr1000_mean
  
  # Place the subset back into the original data frame
  Snap_fuels[which(Snap_fuels$Stratum==unique(Snap_fuels$Stratum)[x]),] = tmp
}


#Fix Snap File
for (i in 1:length(Snap_US$Layer)){
  
  if (Snap_US$Layer[i] == "Layer 1 (12.5-17.5)"){
    Snap_US$DBH[i] = as.numeric(15)
    if(is.na(Snap_US$CBH[i])){
      Snap_US$CBH[i] = as.numeric(4)
    }
  }
  else if (Snap_US$Layer[i] == "Layer 2 (7.5-12.49)"){
    Snap_US$DBH[i] = as.numeric(10)
    if(is.na(Snap_US$CBH[i])){
      Snap_US$CBH[i] = as.numeric(3)
    }
  }
  else if (Snap_US$Layer[i] == "Layer 3 (2.5-7.49)"){
    Snap_US$DBH[i] = as.numeric(5)
    if(is.na(Snap_US$CBH[i])){
      Snap_US$CBH[i] = as.numeric(1)
    }
  }
  else if (Snap_US$Layer[i] == "Layer 4 (<1.3m)" ){
    Snap_US$DBH[i] = as.numeric(1.5)
    if(is.na(Snap_US$CBH[i])){
      Snap_US$CBH[i] = as.numeric(0)
    }
  }
}


#Fix Species
validSpecies  <- c("Ba","Bl", "Bg","Bb","Cw", "Fd", "Hw","T","Pl", "Sx","Sb","Sw", "Lw", "Lt", "Pw","Yc", "Fdi", "Fdc", "Py","Pli","Act","Acb","Ac","At","Ep","DP", "DU","Dead", "Dr","D","Mb","Hm","Ax","Bp","Dg","La","Pa","Pf","Pj","Plc","Pli","Pw","Sn","Ss")

# Update Tree Species for Understory
for (i in 1:nrow(Snap_US)) {
  current_species <- Snap_US$SPP[i]
  # Handle NA or empty values
  if (is.na(current_species) || current_species == "") {
    Snap_US$SPP[i] <- "D"
    next
  }
  # Trim whitespace and standardize case
  current_species <- trimws(current_species)
  # Check if species is valid
  if (!current_species %in% validSpecies) {
    # Try case-insensitive match
    match_idx <- which(tolower(validSpecies) == tolower(current_species))
    
    if (length(match_idx) > 0) {
      # Fix the case
      Snap_US$SPP[i] <- validSpecies[match_idx[1]]
    } else {
      # Mark as unknown or invalid
      Snap_US$SPP[i] <- "D"
      warning(paste("Invalid species code at row", i, ":", current_species))
    }
  }
}



#Fix CBH vs height
Snap_OS$CBH..0.1m. <- ifelse(
  !is.na(Snap_OS$CBH..0.1m.) & 
    !is.na(Snap_OS$Total.Height..m.) & 
    Snap_OS$CBH..0.1m. >= Snap_OS$Total.Height..m.,
  Snap_OS$Total.Height..m. - 1,
  Snap_OS$CBH..0.1m.
)
Snap_US$CBH..0.1m. <- ifelse(
  !is.na(Snap_US$CBH..0.1m.) & 
    !is.na(Snap_US$Height..0.1m.) & 
    Snap_US$CBH..0.1m. >= Snap_US$Height..0.1m.,
  Snap_US$Height..0.1m. - 1,
  Snap_US$CBH..0.1m.
)

#export fuels and understory and overstory
write.csv(Snap_OS,paste0(path,snap_prefix,project,"_OS.csv"))
write.csv(Snap_US,paste0(path,snap_prefix,project,"_US.csv"))
write.csv(Snap_fuels,paste0(path,snap_prefix,project,"_FUELS.csv"))


##############################################################################################
# Step 2.1  - Initialize data frame
##############################################################################################

# Initialize data frames

# Data frame legend (as required by FuelCalcBC as an input):
#
# Note: Units for all density-measure fields are in t/ha
#
# Strat = Stratum (initialized from SNAP) 
# PlotId = Plot number (initialized from SNAP)
# DufMos = Duff Depth (initialized from SNAP)
# ShrHer = Shrub tonnage
# LitLic = Litter tonnage
# F1 = Fuel load < 0.5 cm (initialized from SNAP 1 hr fuel load)
# F2 = Fuel load 0.5 - 1 cm (initialized from SNAP 10 hr fuel load)
# F3 = Fuel load 1 - 3 cm
# F4 = Fuel load 3 - 5 cm (initialized from SNAP 100 hr fuel load)
# F5 = Fuel load 5 - 7 cm (initialized from SNAP 1000 hr fuel load)
# CS/CR6 = Coarse sound/rotten fuel load 7.6 - 15 cm
# CS/CR7 = Coarse sound/rotten fuel load 15 - 25 cm
# CS/CR8 = Coarse sound/rotten fuel load 25 - 40 cm
# CS/CR9 = Coarse sound/rotten fuel load 40 - 60 cm
# CS/CR10 = Coarse sound/rotten fuel load > 60 cm
# Pil = Pile
#
# Note: FuelCalcBC uses the Canadian standard of fuel load size classes. This includes 5 fine
# fuel classes and 5 coarse fuel classes. We have to convert our American classes accordingly.

#Clean 1000 HOURS
Snap_fuels <- Snap_fuels %>%
  mutate(X1000.hr.fuels...7.6.cm.kg.m2. = ifelse(
    (is.na(X1000.hr.fuels...7.6.cm.kg.m2.) | X1000.hr.fuels...7.6.cm.kg.m2. == 0) &
      (!is.na(LWD.fuels..7.0.20.0cm...kg.m2.) | !is.na(CWD.fuels...20.0cm...kg.m2.)) &
      (LWD.fuels..7.0.20.0cm...kg.m2. > 0 | CWD.fuels...20.0cm...kg.m2. > 0),
    LWD.fuels..7.0.20.0cm...kg.m2. + CWD.fuels...20.0cm...kg.m2.,
    X1000.hr.fuels...7.6.cm.kg.m2.
  ))
Snap_fuels[is.na(Snap_fuels)]<-0

# Overstorey
bcd <- data.frame(Strat = Snap_fuels$Stratum,
                  PlotId = Snap_fuels$Plot..,
                  DufMos = Snap_fuels$Duff.Depth..cm.,
                  ShrHer = Snap_fuels$Avg.Shrub.Loading..kg.m2.,
                  LitLic = Snap_fuels$Avg.1.hr.fuels...0.6.cm...kg.m2./2, #equal to half 1 hour load
                  F1 = Snap_fuels$Avg.1.hr.fuels...0.6.cm...kg.m2.,
                  F2=Snap_fuels$Avg.10.hr.fuels..0.6.2.5cm...kg.m2.,
                  F3=numeric(length(Snap_fuels$Plot..)),
                  F4=Snap_fuels$Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.,
                  F5=Snap_fuels$X1000.hr.fuels...7.6.cm.kg.m2.,
                  CS6=Snap_fuels$LWD.fuels..7.0.20.0cm...kg.m2.,
                  CS7=Snap_fuels$CWD.fuels...20.0cm...kg.m2.,
                  CS8=numeric(length(Snap_fuels$Plot..)),
                  CS9=numeric(length(Snap_fuels$Plot..)),
                  CS10=numeric(length(Snap_fuels$Plot..)),
                  CR6=numeric(length(Snap_fuels$Plot..)),
                  CR7=numeric(length(Snap_fuels$Plot..)),
                  CR8=numeric(length(Snap_fuels$Plot..)),
                  CR9=numeric(length(Snap_fuels$Plot..)),
                  CR10=numeric(length(Snap_fuels$Plot..)),
                  Pil=numeric(length(Snap_fuels$Plot..)),
                  stringsAsFactors=FALSE)

# Understorey
bcd2 = data.frame(Strat = Snap_fuels$Stratum,
                  PlotId = Snap_fuels$Plot..,
                  DufMos = Snap_fuels$Duff.Depth..cm.,
                  ShrHer = Snap_fuels$Avg.Shrub.Loading..kg.m2.,
                  LitLic =  Snap_fuels$Avg.1.hr.fuels...0.6.cm...kg.m2./2,#equal to half 1 hour load
                  F1 = Snap_fuels$Avg.1.hr.fuels...0.6.cm...kg.m2.,
                  F2=Snap_fuels$Avg.10.hr.fuels..0.6.2.5cm...kg.m2.,
                  F3=numeric(length(Snap_fuels$Plot..)),
                  F4=Snap_fuels$Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.,
                  F5=Snap_fuels$X1000.hr.fuels...7.6.cm.kg.m2.,
                  CS6=Snap_fuels$LWD.fuels..7.0.20.0cm...kg.m2.,
                  CS7=Snap_fuels$CWD.fuels...20.0cm...kg.m2.,
                  CS8=numeric(length(Snap_fuels$Plot..)),
                  CS9=numeric(length(Snap_fuels$Plot..)),
                  CS10=numeric(length(Snap_fuels$Plot..)),
                  CR6=numeric(length(Snap_fuels$Plot..)),
                  CR7=numeric(length(Snap_fuels$Plot..)),
                  CR8=numeric(length(Snap_fuels$Plot..)),
                  CR9=numeric(length(Snap_fuels$Plot..)),
                  CR10=numeric(length(Snap_fuels$Plot..)),
                  Pil=numeric(length(Snap_fuels$Plot..)),
                  stringsAsFactors=FALSE)

# Combine overstorey and understorey into 1 data frame
finalbcd = as.data.frame(rbind(bcd,bcd2))

# Remove repeating rows for the .BCD file, since the same information is repeated for each plot

# Note: Only one row needed per plot

# Note: Merging overstorey and understorey data and then paring down ensures inclusion of plots
# where either overstorey trees or understorey trees are absent

finalbcd = finalbcd %>% distinct()

finalbcd2 = finalbcd

##############################################################################################
# Step 2.2  - Generate mean 1/10/100/1000 hr fuel loading values per stratum
##############################################################################################

# Replace missing values for 1/10/100/1000 hr fuels (previously assigned value of 99) with NA
finalbcd2[finalbcd == 99] = NA


# Calculate mean values for 1/10/100/1000 hr fuels for each stratum. Put values into their own
# respective data frames.
ave_duff      <- finalbcd2  |> dplyr::group_by(Strat)   |> dplyr::summarize(duff   = mean(DufMos,                              na.rm = TRUE))
ave_grass     <- Snap_fuels |> dplyr::group_by(Stratum) |> dplyr::summarize(grass  = mean(Avg.Grass.Loading..kg.m2.,           na.rm = TRUE))
ave_shrub     <- Snap_fuels |> dplyr::group_by(Stratum) |> dplyr::summarize(shrub  = mean(Avg.Shrub.Loading..kg.m2.,           na.rm = TRUE))
ave_lit       <- finalbcd2  |> dplyr::group_by(Strat)   |> dplyr::summarize(lit    = mean(LitLic,                              na.rm = TRUE))
ave_1hr       <- finalbcd2  |> dplyr::group_by(Strat)   |> dplyr::summarize(hr1    = mean(F1,                                  na.rm = TRUE))
ave_10hr      <- finalbcd2  |> dplyr::group_by(Strat)   |> dplyr::summarize(hr10   = mean(F2,                                  na.rm = TRUE))
ave_100hr     <- finalbcd2  |> dplyr::group_by(Strat)   |> dplyr::summarize(hr100  = mean(F4,                                  na.rm = TRUE))
ave_1000hr    <- finalbcd2  |> dplyr::group_by(Strat)   |> dplyr::summarize(hr1000 = mean(F5,                                  na.rm = TRUE))
ave_CWD       <- Snap_fuels |> dplyr::group_by(Stratum) |> dplyr::summarize(CWD    = mean(CWD.fuels...20.0cm...kg.m2.,         na.rm = TRUE))
ave_LWD       <- Snap_fuels |> dplyr::group_by(Stratum) |> dplyr::summarize(LWD    = mean(LWD.fuels..7.0.20.0cm...kg.m2.,     na.rm = TRUE))
ave_LWDPieces <- Snap_fuels |> dplyr::group_by(Stratum) |> dplyr::summarize(LWD    = mean(LWD.pieces,                         na.rm = TRUE))

# Bind together 1/10/100/1000 hr fuels for each stratum and write out a csv
d = cbind(ave_duff,ave_grass$grass,ave_shrub$shrub,ave_lit$lit,ave_1hr$hr1,ave_10hr$hr10,ave_100hr$hr100,ave_1000hr$hr1000,ave_LWD$LWD,ave_CWD$CWD,ave_LWDPieces$LWD)

names(d)<- c("Stratum","DuffDepth.cm","Grass.kg.m2","Shrub.kg.m2","Litter.kg.m2","Hr1.kg.m2","Hr10.kg.m2","Hr100.kg.m2","Hr1000.kg.m2","LWD.kg.m2","CWD.kg.m2","LWDPieces.ha")
d[is.na(d)] <- 0

write.csv(d,paste0(path,snap_prefix,"FuelAverages_Stratum_PreTreatment.csv"),row.names = FALSE)

##############################################################################################
# Step 2.3  - Adapt distribution of SNAP fuel loads to Canadian standard
##############################################################################################

# Note: Fuels in SNAP are measured in kg/m2, must convert to t/ha for FuelCalcBC (multiply by 10)

# Note: See Step 2.1 for a legend of what these size classes are

finalbcd$F1 = finalbcd$F1*10        # All of 1 hr fuel load
finalbcd$F2 = (finalbcd$F2/2)*10    # Half of 10 hr fuel load
finalbcd$F3 = finalbcd$F2           # Half of 10 hr fuel load
finalbcd$F4 = (finalbcd$F4/2)*10    # Half of 100 hr fuel load
finalbcd$F5 = finalbcd$F4           # Half of 100 hr fuel load
finalbcd$CS6 = (finalbcd$F5/2)*10   # Half of 1000 hr fuel load
finalbcd$CS7 = finalbcd$CS6         # Half of 1000 hr fuel load

#finalbcd[,c(6:12)][finalbcd[,c(6:12)]>45] = 99

#############################################################################################
# Step 2.4  - Export .bcd files
#############################################################################################

# Create empty list with number of elements equal to number of strata
bcdlist = unique(finalbcd$Strat)
ddat <- as.list(rep("", length(bcdlist)))

# For each list element...
for(i in 1:length(bcdlist)) {
  
  # Populate each list element with a data frame. Empty fields have names and data types 
  # matching those in the bcd data frame
  ddat[[i]] <- data.frame(Strat = character(),
                          PlotId = numeric(),
                          DufMos = numeric(),
                          ShrHer = numeric(),
                          LitLic = numeric(),
                          F1 = numeric(),
                          F2=numeric(),
                          F3=numeric(),
                          F4=numeric(),
                          F5=numeric(),
                          CS6=numeric(),
                          CS7=numeric(),
                          CS8=numeric(),
                          CS9=numeric(),
                          CS10=numeric(),
                          CR6=numeric(),
                          CR7=numeric(),
                          CR8=numeric(),
                          CR9=numeric(),
                          CR10=numeric(),
                          Pil=numeric(),
                          stringsAsFactors=FALSE)
}

# Note: See Step 2.1 for a legend of what these fields are

# For each list element...
for (i in 1:length(ddat)){
  
  # Make a subset containing all plots associated with a common stratum in the bcd data 
  # frame. Place this subset into its own list element. 
  strt = as.character(bcdlist[i])
  bind = finalbcd[which(finalbcd$Strat == strt),]
  ddat[[i]] = bind
}

# For each list element...
for (i in 1:length(ddat)){
  
  # Write each subset out into its own file
  df = ddat[[i]]
  name = as.character(df$Strat[1])
  df = df[,-1]
  write.csv(df,file = paste0(fuelcalc,name,".bcd"),row.names = FALSE)
  #write.csv(df,file = paste0(out,"/FuelCalcBC/",name,"_NO_US",".bcd"),row.names = FALSE)
  
}


##############################################################################################
# Step 3.1  - Generate overstorey component
##############################################################################################

# Initialize data frames

# Data frame legend (as required by FuelCalcBC as an input):
#
# Strat = Stratum (initialized from SNAP) 
# Plt = Plot number (initialized from SNAP)
# No. = Tree number (initialized from SNAP)
# spe = Species (initialized from SNAP)
# TPH = Trees per hectare?? 
# DBH = Diameter at breast-height (intialized from SNAP)
# TH = Total height (initialized from SNAP)
# CBH = Crown base height
# CrCl = Crown class
# TS = Tree status (same as decay class, initialized from SNAP)
# RP = Retention priority??
# sp1 = Species 1 (initialized from SNAP)
# cbh1 = Crown base height of species 1 (initialized from SNAP)
# etc. etc.
#
# Note: .tre files must contain an entry for each individual tree (applies to understorey
# as well).
#

# Overstorey
tre = data.frame(Strat=Snap_OS$Stratum,
                 Plt=Snap_OS$Plot..,
                 No.=Snap_OS$No.,
                 spe=Snap_OS$Spp,
                 TPH=numeric(length(Snap_OS$Plot..)),
                 DBH=Snap_OS$DBH,
                 TH=Snap_OS$Total.Height..m.,
                 CBH=Snap_OS$CBH..0.1m.,
                 CrCl=character(length(Snap_OS$Plot..)),
                 TS=Snap_OS$Decay.Class,
                 RP=numeric(length(Snap_OS$Plot..)),
                 BAF=Snap_OS$BAF,
                 stringsAsFactors=FALSE)


# Extrapolate trees per hectare from number of trees found at plot (given known plot radius)
if(tre$BAF[1] == "NO BAF"){
  radius=mean(Snap_US$Plot.Radius)
  tre$TPH<-mean(Snap_US$Plot.Multiplier)
  
} else{
  prf = (1/(sqrt(tre$BAF)*2))*1
  pr = (prf*tre$DBH)*1
  area = ((pr^2)*3.14)/10000
  tre$TPH = 1/area
}

# If there is no understorey in any of the plots, then this is the final .tre file
if (NROW(Snap_US) == 0){
  finaltre = tre
  
  # Otherwise...
  
} else {
  
  ##############################################################################################
  # Step 3.2  - Generate understorey component
  ##############################################################################################    
  
  # Understorey
  
  # Initialize data frame with fields matching to the SNAP understorey data frame
  temptre = data.frame(C=character(),
                       S=character(),
                       P=numeric(),
                       BAF=numeric(),
                       DD=numeric(),
                       x1=numeric(),
                       x10=numeric(),
                       x100=numeric(),
                       x1000=numeric(),
                       lyr=character(),
                       spp=character(),
                       xoftree=numeric(),
                       CBH=numeric(),
                       H=numeric(),stringsAsFactors=FALSE)
  
  # For each species in a given layer in a given plot in the SNAP understorey data frame...
  # (ex: Layer 3 in Plot 17 could have 3 Fd trees)
  for (i in 1:NROW(Snap_US)){
    
    # if (Snap_US$Layer[i] != "Layer 4 (<1.3m)"){
    
    # Isolate the trees of a given species/layer/plot
    d = Snap_US[i,]
    
    # Identify number of rows that need to be added to the data frame (temptre). This 
    # will be one less than the number of trees in a layer (for a given species/plot). The original
    # data frame will be appended to this one to complete it.
    rpt = (d$X..of.Trees)-1
    if (is.na(rpt) | rpt == -1 ){rpt=0}
    
    # Generate a row for each tree in the plot/layer/species
    rptrws = do.call("rbind", replicate(rpt, d, simplify = FALSE))
    
    # Bind all trees from each species/layer/plot (one at a time) into one data frame
    if (i == 1){
      temptre = rptrws
    } else {
      temptre = rbind(temptre,rptrws)
    }
  }
  # }
  
  # Bind together all the two dataframes (each understorey tree now has its own row)
  temptre2 = rbind(Snap_US,temptre)
  # understorey trees
  temptre2$TPH = (temptre2$X..of.Trees*mean(Snap_US$Plot.Multiplier))/temptre2$X..of.Trees 
  
  temptre2 = temptre2[!is.na(temptre2$TPH),]
  BAF_DF<-tre %>% dplyr::select(Strat,Plt,BAF) %>% dplyr::rename(Stratum=Strat,Plot..=Plt)
  temptre2$BAF <- BAF_DF$BAF[match(
    paste(temptre2$Stratum, temptre2$Plot..), 
    paste(BAF_DF$Stratum, BAF_DF$Plot..)
  )]
  
  # temptre2 = temptre2[-which(is.na(temptre2$TPH)),]
  
  # Initialize data frame with fields matching to "tre", and populated with values from the 
  # manipulated SNAP understorey data frame (temptre[2]). "tre" is for overstorey trees while
  # tre2 (below) is for understorey trees.
  #
  # Note: See Step 3.1 for a legend of what these fields are
  #
  tre2 = data.frame(Strat=temptre2$Stratum,
                    Plt=temptre2$Plot..,
                    No.=numeric(length(temptre2$Plot..)),
                    spe=as.character(temptre2$SPP),
                    TPH=temptre2$TPH,
                    DBH=as.character(temptre2$Layer),
                    TH=temptre2$Height..0.1m.,
                    CBH=temptre2$CBH..0.1m.,
                    CrCl=rep("N",length(temptre2$Plot..)),
                    TS=rep("H",length(temptre2$Plot..)),
                    RP=numeric(length(temptre2$Plot..)),
                    BAF=temptre2$BAF, stringsAsFactors=FALSE)
  
  # Generate DBH values for understorey trees (based off of layers, Frontera doesn't
  # measure DBH for understorey trees). Now adjusted since we do sometimes so if NA this fixes it and CBH
  for (i in 1:length(tre2$DBH)){
    if (tre2$DBH[i] == "Layer 1 (12.5-17.5)"){
      tre2$DBH[i] = as.numeric(15)
      if(is.na(tre2$CBH[i])){
        tre2$CBH[i] = as.numeric(4)
      }
    }
    else if (tre2$DBH[i] == "Layer 2 (7.5-12.49)"){
      tre2$DBH[i] = as.numeric(10)
      if(is.na(tre2$CBH[i])){
        tre2$CBH[i] = as.numeric(3)
      }
    }
    else if (tre2$DBH[i] == "Layer 3 (2.5-7.49)"){
      tre2$DBH[i] = as.numeric(5)
      if(is.na(tre2$CBH[i])){
        tre2$CBH[i] = as.numeric(1)
      }
    }
    else if (tre2$DBH[i] == "Layer 4 (<1.3m)" ){
      tre2$DBH[i] = as.numeric(1.5)
      if(is.na(tre2$CBH[i])){
        tre2$CBH[i] = as.numeric(0)
      }
    }
  }
  
  
  # Update Tree status to appropriate code if dead.
  for (i in 1:length(tre2$spe)){
    if (tre2$spe[i] =="Dead" ){
      tre2$TS[i] = "D"
      # } else if (tre2$TH[i] <= 1 & tre2$CrCl[i] == "N"){
      #   tre2$TS[i] = "D"
    }
  }
  
  
  #Fix Species
  validSpecies  <- c("Ba","Bl", "Bg","Bb","Cw", "Fd", "Hw","T","Pl", "Sx","Sb","Sw", "Lw", "Lt", "Pw","Yc", "Fdi", "Fdc", "Py","Pli","Act","Acb","Ac","At","Ep","DP", "DU","Dead", "Dr","D","Mb","Hm","Ax","Bp","Dg","La","Pa","Pf","Pj","Plc","Pli","Pw","Sn","Ss")
  
  # Update Tree Species for Understory
  for (i in 1:nrow(tre2)) {
    current_species <- tre2$spe[i]
    # Handle NA or empty values
    if (is.na(current_species) || current_species == "") {
      tre2$spe[i] <- "D"
      next
    }
    # Trim whitespace and standardize case
    current_species <- trimws(current_species)
    # Check if species is valid
    if (!current_species %in% validSpecies) {
      # Try case-insensitive match
      match_idx <- which(tolower(validSpecies) == tolower(current_species))
      
      if (length(match_idx) > 0) {
        # Fix the case
        tre2$spe[i] <- validSpecies[match_idx[1]]
      } else {
        # Mark as unknown or invalid
        tre2$spe[i] <- "D"
        warning(paste("Invalid species code at row", i, ":", current_species))
      }
    }
  }
  
  # Update Tree Species for Overstory
  for (i in 1:nrow(tre)) {
    current_species <- tre$spe[i]
    # Handle NA or empty values
    if (is.na(current_species) || current_species == "") {
      tre$spe[i] <- "D"
      next
    }
    # Trim whitespace and standardize case
    current_species <- trimws(current_species)
    # Check if species is valid
    if (!current_species %in% validSpecies) {
      # Try case-insensitive match
      match_idx <- which(tolower(validSpecies) == tolower(current_species))
      
      if (length(match_idx) > 0) {
        # Fix the case
        tre$spe[i] <- validSpecies[match_idx[1]]
      } else {
        # Mark as unknown or invalid
        tre$spe[i] <- "D"
        warning(paste("Invalid species code at row", i, ":", current_species))
      }
    }
  }
  
  # Bind together overstorey and understorey trees into single data frame.
  finaltre = as.data.frame(rbind(tre,tre2))
  finaltre$DBH = as.numeric(finaltre$DBH)
}


##############################################################################################
# Step 3.3  - Reassigning values to meet FuelCalcBC input requirements
##############################################################################################

# Make sure row order of Snap_OS file matches new .tre file:
finaltre = finaltre[order(finaltre$Strat,finaltre$Plt,finaltre$No.),]

# Note: FuelCalcBC expects certain tree codes. The changes below address needed changes to
# adhere to input requirements

pattern = "Ep"
replacement = "Eb"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Act"
replacement = "At"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Dead"
replacement = "Eb"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Ac"
replacement = "At"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Acb"
replacement = "At"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Eb/Down"
replacement = "Eb"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Lt"
replacement = "Lw"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Atb"
replacement = "At"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Fdi"
replacement = "Fd"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Fdc"
replacement = "Fd"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Pli"
replacement = "Pl"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

pattern = "Plc"
replacement = "Pl"
finaltre$spe = gsub(pattern,replacement,finaltre$spe)

# Updating Tree Status (TS):

# Note: Decay class entered into SNAP has classes 1 - 9. FuelCalcBC accepts the following
# statuses: H = healthy, S = sick, D = dead.

# For each tree (overstorey and understorey)...
for (i in 1:length(finaltre$Plt)){
  
  if (finaltre$Plt[i] == 0){
    finaltre$TS[i] = finaltre$TS[i]
  }
  
  else {
    # if status is absent, then NA or H for healthy
    if (is.na(finaltre$TS[i])){
      #finaltre$TS[i] = "NA"
      finaltre$TS[i] = "H"
      
    }
    
    # if decay class 1 then healthy
    else if (finaltre$TS[i] == 1){
      finaltre$TS[i] = "H"
    }
    
    # if decay class 2 then sick
    else if (finaltre$TS[i] == 2){
      finaltre$TS[i] = "S"
    }
    
    # if decay class is 3 or lower then dead
    else if (finaltre$TS[i] >= 3 & finaltre$TS[i] <= 9){
      finaltre$TS[i] = "D"
    }
  }
  
}

# Fixing CBH (THIS STILL NEEDS WORK)

# Categorize trees by diameter class and by species
#
# Note: Diameter classes have been simplified to include all overstorey trees in a single
# category
finaltre$diameterClass <- cut(finaltre$DBH,
                              breaks = c(-1, 1.5, 7.5, 12.5, 17.5, 300),
                              labels = c(1, 2, 3, 4, 5))

# Determine the average CBH per species and diameter class
meanCBHbyClass <- finaltre %>%
  dplyr::group_by(spe, diameterClass) %>%
  dplyr::summarise(CBH2 = mean(as.numeric(CBH), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::filter(!is.nan(CBH2))  # Remove NaN values

# Apply column  names to match finaltre for merge below
colnames(meanCBHbyClass) <- c("spe", "diameterClass", "CBH2")

# Join average CBH values from above to finaltre
finalTemp <- merge(finaltre, meanCBHbyClass, by = c("spe", "diameterClass"))

# Place average CBH values into appropriate column where value was previously missing (NA)
finalTemp$CBH[which(is.na(finalTemp$CBH))] <- finalTemp$CBH2[which(is.na(finalTemp$CBH))]

# If any CBH values are still missing, make the CBH 1/2 of the tree height
finalTemp$CBH[which(is.nan(finalTemp$CBH))] <- finalTemp$TH[which(is.nan(finalTemp$CBH))]/2

# If any assigned CBH values are greater than the listed tree height, make it barely less than the tree height
finalTemp$CBH[which(finalTemp$CBH >= finalTemp$TH)] <- finalTemp$TH[which(finalTemp$CBH > finalTemp$TH)] - 1

# Reorder dataframe to how finaltre was before
finaltre <- data.frame(Strat = finalTemp$Strat,
                       Plt = finalTemp$Plt,
                       No. = finalTemp$No.,
                       spe = finalTemp$spe,
                       TPH = finalTemp$TPH,
                       DBH = finalTemp$DBH,
                       TH = finalTemp$TH,
                       CBH = finalTemp$CBH,
                       CrCl = finalTemp$CrCl,
                       TS = finalTemp$TS,
                       RP = finalTemp$RP,
                       BAF = finalTemp$BAF)



# Create empty list with number of elements equal to number of strata
ddat2 <- as.list(rep("", length(bcdlist)))

for(i in 1:length(bcdlist)) {
  
  # Populate each list element with a data frame. Empty fields have names and data types 
  # matching those in the tre data frame 
  ddat2[[i]] <- data.frame(Strat=character(),
                           Plt=numeric(),
                           No.=numeric(),
                           spe=character(),
                           TPH=numeric(),
                           DBH=numeric(),
                           TH=numeric(),
                           CBH=numeric(),
                           CrCl=character(),
                           TS=character(),
                           RP=numeric(),stringsAsFactors=FALSE)
}

# Parse trees in the tre data frame by stratum

# For each list element...
for (l in 1:length(ddat2)){
  
  # Make a subset containing all plots associated with a common stratum in the bcd data 
  # frame. Place this subset into its own list element. 
  strt = as.character(bcdlist[l])
  bind = finaltre[which(finaltre$Strat == strt),]
  
  ddat2[[l]] = bind
}

# For each stratum, add the average CBH data for the top 3 species, and remove unnecessary column:
# NOTE: For species beyond the top 3, CBH is assumed to be 10 m, since extra species are likely deciduous 

# !ERROR! Loop may not loop properly through all treatment units/strata

# For each list element...
for (i in 1:length(ddat2)){
  
  # Note: Only overstorey trees are given a number in SNAP. Any auto-populated with 0 are therefore
  # understorey trees.
  
  # Split trees into overstorey and understorey
  df2 = ddat2[[i]]
  df2os = df2[which(df2$No.>0),]
  df2us = df2[which(df2$No.==0),]
  
  # If there are no overstorey trees then the understorey trees make up the final data frame
  if (NROW(df2os$Plt)==0){
    finaldf = df2us
    
    # Otherwise, if there are overstorey trees...
  } else if (NROW(df2os$Plt)>0){
    
    ################
    
    # Crown Class
    #
    # Note: The crown class assignment of overstorey trees varies according to the variability of tree heights.
    # 
    # Note: The crown class assignment of understorey trees varies according to the variability of tree heights.
    
    # Calculate the coefficient of variation of tree heights within all plots
    var = sd(df2os$TH,na.rm = T)/mean(df2os$TH,na.rm = T)
    
    # Note: Circumstances when the coefficient of variation might be NA include 1. Very few plots were done,
    # 2. Plots were within very open forests or grass ecosystems.
    
    # If the coefficient of variation is NA, use overstorey tree height quantile break points of 0.95, 0.6,
    # 0.3, 0.2 to categorize crown class of overstorey trees.
    if (is.na(var)){
      
      # For each tree in the overstorey trees data frame...
      for (k in 1:length(df2os$Plt)){
        
        if (is.na(df2os$TH[k])){next}     #---------------->>       IS THIS NEEDED???
        
        # Note: Crown class of overstorey trees is determined based on the distribution of trees in all plots
        
        if (df2os$TH[k] <= quantile(df2os$TH,0.95,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.6,na.rm = T)){
          df2os$CrCl[k] = "D"
        }
        else if (df2os$TH[k] > quantile(df2os$TH,0.95,na.rm = T)){
          df2os$CrCl[k] = "E"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.6,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.3,na.rm = T)){
          df2os$CrCl[k] = "C"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.3,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.2,na.rm = T)){
          df2os$CrCl[k] = "I"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.2,na.rm = T)){
          df2os$CrCl[k] = "S"
        }
      }
      
      # Then, if there aren't any understorey, then then we can skip classifying understorey trees.
      if (NROW(df2us$Plt)==0){
        finaldf = df2os
        
        #Otherwise...
      } else if (NROW(df2us$Plt)>0){
        
        # If the coefficient of variation is NA, use overstorey tree height quantile break points of 0.6,
        # 0.3, 0.1 to categorize crown class of understorey trees.
        
        for (k in 1:length(df2us$Plt)){
          
          if (is.na(df2us$TH[k])){next}
          
          if (df2us$TH[k]< quantile(df2os$TH,0.6,na.rm = T) & df2us$TH[k] >= quantile(df2os$TH,0.3,na.rm = T)){
            df2us$CrCl[k] = "C"
          }
          else if (df2us$TH[k] < quantile(df2os$TH,0.3,na.rm = T) & df2us$TH[k] >= quantile(df2os$TH,0.1,na.rm = T)){
            df2us$CrCl[k] = "I"
          }
          else if (df2us$TH[k] < quantile(df2os$TH,0.1,na.rm = T)){
            df2us$CrCl[k] = "N"
          }
        }
      }
      
      
      # If the coefficient of variation is greater than 0.75, use overstorey tree height quantile break points of 
      # 0.95, 0.8, 0.7, 0.5 to categorize crown class of overstorey trees.
    } else if (var > .75){
      for (k in 1:length(df2os$Plt)){
        
        if (is.na(df2os$TH[k])){next}
        
        if (df2os$TH[k] <= quantile(df2os$TH,0.95,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.8,na.rm = T)){
          df2os$CrCl[k] = "D"
        }
        else if (df2os$TH[k] > quantile(df2os$TH,0.95,na.rm = T)){
          df2os$CrCl[k] = "E"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.8,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.7,na.rm = T)){
          df2os$CrCl[k] = "C"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.7,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.5,na.rm = T)){
          df2os$CrCl[k] = "I"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.5,na.rm = T)){
          df2os$CrCl[k] = "S"
        }
      }
      
      if (NROW(df2us$Plt)==0){
        finaldf = df2os
      } else if (NROW(df2us$Plt)>0){
        
        # If the coefficient of variation is greater than 0.75, use overstorey tree height quantile break points of 
        # 0.95, 0.8, 0.7, 0.5 to categorize crown class of understorey trees.
        
        for (k in 1:length(df2us$Plt)){
          
          if (is.na(df2us$TH[k])){next}
          
          if (df2us$TH[k]< quantile(df2os$TH,0.8,na.rm = T) & df2us$TH[k] >= quantile(df2os$TH,0.5,na.rm = T)){
            df2us$CrCl[k] = "C"
          }
          else if (df2us$TH[k] < quantile(df2os$TH,0.5,na.rm = T) & df2us$TH[k] >= quantile(df2os$TH,0.1,na.rm = T)){
            df2us$CrCl[k] = "I"
          }
          else if (df2us$TH[k] < quantile(df2os$TH,0.1,na.rm = T)){
            df2us$CrCl[k] = "N"
          }
        }
      }
    }
    
    # If the coefficient of variation is less than 0.75, use overstorey tree height quantile break points of 
    # 0.95, 0.6, 0.3, 0.2 to categorize crown class of overstorey trees.
    else {
      for (k in 1:length(df2os$Plt)){
        
        if (is.na(df2os$TH[k])){next}
        
        if (df2os$TH[k] <= quantile(df2os$TH,0.95,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.6,na.rm = T)){
          df2os$CrCl[k] = "D"
        }
        else if (df2os$TH[k] > quantile(df2os$TH,0.95,na.rm = T)){
          df2os$CrCl[k] = "E"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.6,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.3,na.rm = T)){
          df2os$CrCl[k] = "C"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.3,na.rm = T) & df2os$TH[k] >= quantile(df2os$TH,0.2,na.rm = T)){
          df2os$CrCl[k] = "I"
        }
        else if (df2os$TH[k] < quantile(df2os$TH,0.2,na.rm = T)){
          df2os$CrCl[k] = "S"
        }
      }
      
      if (NROW(df2us$Plt)==0){
        finaldf = df2os
        
        # If the coefficient of variation is less than 0.75, use overstorey tree height quantile break points of 
        # 0.95, 0.6, 0.3, 0.2 to categorize crown class of understorey trees.
      } else if (NROW(df2us$Plt)>0){
        
        for (k in 1:length(df2us$Plt)){
          
          if (is.na(df2us$TH[k])){next}
          
          if (df2us$TH[k]< quantile(df2os$TH,0.6,na.rm = T) & df2us$TH[k] >= quantile(df2os$TH,0.3,na.rm = T)){
            df2us$CrCl[k] = "C"
          }
          else if (df2us$TH[k] < quantile(df2os$TH,0.3,na.rm = T) & df2us$TH[k] >= quantile(df2os$TH,0.1,na.rm = T)){
            df2us$CrCl[k] = "I"
          }
          else if (df2us$TH[k] < quantile(df2os$TH,0.1,na.rm = T)){
            df2us$CrCl[k] = "N"
          }
        }
      }
    }
    
    #############################################################################################
    
  }
  
  if (NROW(df2os$Plt)>0 & NROW(df2us$Plt)>0){
    finaldf = as.data.frame(rbind(df2os,df2us))
  }
  
  #out to us tre file
  USTRE<-finaldf
  #
  finaldf = finaldf[order(finaldf$Strat,finaldf$Plt),]
  name2 = as.character(finaldf$Strat[1])
  finaldf = finaldf[,-c(1,3,12:18)]
  
  #Clean
  # Clean tree height - only apply to rows where TH is NA
  finaldf$TH <- case_when(
    !is.na(finaldf$TH) ~ finaldf$TH,  # Keep existing values
    finaldf$DBH <= 2.5 ~ 1,
    finaldf$DBH <= 7.5 ~ 3,
    finaldf$DBH <= 12.5 ~ 5,
    finaldf$DBH <= 17.5 ~ 7,
    finaldf$DBH <= 22.5 ~ 11,
    finaldf$DBH <= 27.5 ~ 15,
    finaldf$DBH <= 35 ~ 20,
    finaldf$DBH <= 45 ~ 25,
    TRUE ~ 30
  )
  
  # CBH - only apply to rows where CBH is NA
  finaldf$CBH <- case_when(
    !is.na(as.numeric(finaldf$CBH)) ~ as.numeric(finaldf$CBH),
    finaldf$DBH <= 2.5 ~ 0,
    finaldf$DBH <= 7.5 ~ 1,
    finaldf$DBH <= 12.5 ~ 2,
    finaldf$DBH <= 17.5 ~ 3,
    finaldf$DBH <= 22.5 ~ 3,
    finaldf$DBH <= 27.5 ~ 5,
    finaldf$DBH <= 35 ~ 6,
    finaldf$DBH <= 45 ~ 8,
    TRUE ~ 9
  )
  
  write.csv(finaldf,file = paste0(fuelcalc,name2,".tre"),row.names = FALSE) # AND FINALLY WRITE EACH FILE OUT!
  
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  finaldf_NOUS = finaldf[-which(finaldf$DBH==1.5),] 
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #Make US FuelCalc TRE Files:
  USTRE$PlotId<-paste0("TU.",USTRE$Strat,"_","PL.",USTRE$Plt)
  USTRE$MonStatus<-"Treatment"
  USTRE$MonOrder<-1
  USTRE$TreeSpecies<-species_crosswalk(USTRE$spe)
  USTRE$TreeExpansionFactor<-round(USTRE$TPH/2.47105,0)
  USTRE$TreeHeight<-round(USTRE$TH*3.28084,2)
  USTRE$Diameter<-round(USTRE$DBH*0.393701,2)
  USTRE$CrownBaseHeight<-round(as.numeric(USTRE$CBH)*3.28084,2)
  USTRE$TreeStatus<-USTRE$TS
  USTRE$CrownClass<-USTRE$CrCl
  USTRE$CrownRatio<-round((1-as.numeric(USTRE$CBH)/USTRE$TH)*100,0)
  USTRE$CharHeight<-1
  USTRE$CrownScorchPercent<-""
  USTRE$CrownScorchHeight<-""
  USTRE$CKR<-""
  USTRE$BeetleDamage<-"N"
  USTRE$EquationType<-"CRNSCH"
  USTRE$FlHt_ScHt<-"4"
  USTRE$FS<-"F"
  USTRE$Severity<-"L"
  
  USTRE_OUT<- USTRE %>% dplyr::select(PlotId,                            MonStatus,MonOrder,TreeSpecies,TreeExpansionFactor,Diameter,TreeHeight,CrownBaseHeight,TreeStatus,CrownClass,CrownRatio,CharHeight,CrownScorchPercent,CrownScorchHeight,CKR,BeetleDamage,EquationType,FlHt_ScHt,FS,Severity)
  
  #clean treestatus
  USTRE_OUT$TreeStatus <- ifelse(
    is.na(USTRE_OUT$TreeStatus),
    ifelse(
      USTRE$TreeHeight > USTRE$CrownBaseHeight & USTRE$CrownBaseHeight > 0,
      "H",
      "D"
    ),
    USTRE_OUT$TreeStatus
  )
  for(i in 1:nrow(USTRE_OUT)){
    if(!USTRE_OUT$TreeStatus[i] %in% c("H","D","U","S")){
      USTRE_OUT$TreeStatus[i] <-"H"
    }
  }
  #
  
  #PlotId,MonStatus,MonOrder,TreeSpecies,TreeExpansionFactor,Diameter,TreeHeight,CrownBaseHeight,TreeStatus,CrownClass,CrownRatio,CharHeight,CrownScorchPercent,CrownScorchHeight,CKR,BeetleDamage,EquationType,FlHt/ScHt,FS,Severity
  
  #Export
  USTRE_OUT = USTRE_OUT[order(USTRE_OUT$PlotId),]
  STname = as.character(USTRE$Strat[1])
    write.table(
    USTRE_OUT,
    file = paste0(path,"/FuelCalc/","TU_",STname,"_FuelCalc_FFI.tre"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,      # remove quotes around text fields
    na = "",            # blank instead of NA
    fileEncoding = "UTF-8"
  )
}



#export cleaned snap data
write.csv(Snap_OS,paste0(path,snap_prefix,project,"_OS.csv"),row.names = FALSE)
write.csv(Snap_US,paste0(path,snap_prefix,project,"_US.csv"),row.names = FALSE)
write.csv(Snap_EX,paste0(path,snap_prefix,project,"_EXTRA.csv"),row.names = FALSE)
write.csv(Snap_fuels,paste0(path,snap_prefix,project,"_FUELS.csv"),row.names = FALSE)


#Setting up FuelCalc and FuelCalcBC Files:-------------------------------------------------------------------------------------

#Creates fake cutting specs with all zeros if you are not cutting
if(!cutting){
  for(i in 1:length(unique(Snap_EX$Stratum))){
    tr<-unique(Snap_EX$Stratum)[i]
    OS<-Snap_OS[Snap_OS$Stratum == tr,]
    US<-Snap_US[Snap_US$Stratum == tr,]
    all_spp<-c(OS$Spp,US$SPP)
    spp<-unique(all_spp)
    spp<- ifelse(spp == "Dead","DU",spp)
    #create cutting specs
    cut_spec<-data.frame(
      Stand.Layer=c("L4","L3","L2","L1","L1","L1","L1","L1","L1"),
      DBH.Class=c("0 - 1.5","1.5 - 7.5","7.5 - 12.5","12.5 - 17.5","17.5 - 22.5" ,"22.5 - 27.5","27.5 - 35","35 - 45", "45+")
    )
    for (species in spp) {
      # Add a new column named after the species
      cut_spec[[species]] <- 0  # Fill with NA or default values
    }
    dropoff_dir <- file.path(path, "Dropoff")
    dir.create(dropoff_dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(cut_spec,paste0(path, "/Dropoff/cuttingSpecs_",tr,".csv"),row.names = FALSE)
    #
    if(!dir.exists(paste0(path, s_s_prefix,tr,"_tables/"))){
      dir.create(paste0(path, s_s_prefix,tr,"_tables/"))
    }
    write.csv(cut_spec,paste0(path, s_s_prefix,tr,"_tables/","cuttingSpecs_",tr,".csv"),row.names = FALSE)
  }
}

#FuelCalcBC Input Files:------------------------

# Loop through each cutting spec file
for (i in 1:length(unique(Snap_EX$Stratum))) {
  TR<- tr_names[i]
  inputfolder<-paste0(TR,"_tables")
  cutting_specs<-paste0(path,s_s_prefix,inputfolder,"/cuttingSpecs_",TR,".csv")
  cuts <- read.csv(cutting_specs)
  desc_cols <- c("Stand.Layer", "DBH.Class")
  pct_cols  <- names(cuts)[grepl("\\..%$", names(cuts))]
  cuts <- cuts[, c(desc_cols, pct_cols)]
  names(cuts) <- gsub("\\..%$", "", names(cuts))
  
  #decide if thinning
  Thin<-ifelse(ThinFlags[i] ==TRUE,"Yes","No")
  
  #prune
  PruneYesNo<-ifelse(PruneFlags[i] ==TRUE,"Yes","No")
  
  # Remove unwanted columns
  cuts <- cuts %>% dplyr::select(-Stand.Layer)
  cuts <- cuts %>% dplyr::select(-starts_with("X"))
  thin_target <- thinning_target_order[i]
  fcp_file <- FUELCALC_BC_TEMPLATE
  
  # Set the other species line (which does not change)
  fcp_file[2] <- paste("#Species-0", TR, "_Other", "0 100 1.0")
  #change column names to reselect species
  cuts<-rename_and_merge_species(cuts)
  
  # Get the species and DBH size classes
  species <- cuts %>% dplyr::select(-DBH.Class) %>% colnames()
  
  #remove duplicate species now that they bare cleaned
  species<-unique(species)
  
  #EXTRACT DBH CLASSES
  dbh_classes<- cuts$DBH.Class
  dbh_classes[9]<-"45"
  # Initialize line counter starting from 3
  line_counter <- 3
  
  # Loop through each species
  for (j in 1:length(species)) {
    # Get the original row indices where the cutting percentage is greater than 0
    valid_indices <- which(cuts[[species[j]]] > 0)
    
    # Loop through each valid row for the species
    for (k in valid_indices) {
      lower_bound <- strsplit(dbh_classes[k], "-")[[1]][1] # Get the lower bound of DBH class
      upper_bound <- strsplit(dbh_classes[k], "-")[[1]][2] # Get the upper bound of DBH class
      
      # Handle cases where the upper bound is missing (e.g., "45+")
      if (is.na(upper_bound)) {
        upper_bound <- "100" # Set upper bound to 100 for "45+" class
      }
      
      # Get the cutting percentage and round it
      cutting_percentage <- cuts[[species[j]]][k]
      rounded_percentage <- (100 - round_to_nearest_10_custom(cutting_percentage)) / 100
      
      if(rounded_percentage == 1){
        rounded_percentage = paste0(rounded_percentage,".0")
      }else if(rounded_percentage == 0){
        rounded_percentage = paste0("0.",rounded_percentage)
      } else(
        rounded_percentage = rounded_percentage
      )
      
      # Format the line with species number, treatment name, species, lower DBH, upper DBH, and rounded percentage
      fcp_file <- append(fcp_file, paste0("#Species-", line_counter-2, " ", tr_names[i], " ", species[j], " ", 
                                          lower_bound, " ", upper_bound, " ", rounded_percentage))
      line_counter <- line_counter +1
    }
  }
  
  #SELECTING SPECIES GROUP AND RETENTION PRIORTIY
  fcp_file[line_counter]<-paste0(" ")
  fcp_file[line_counter+1]<-paste0("#SpeciesGroup-0"," ", tr_names[i], " ", 1)
  fcp_file[line_counter+2]<-paste0(" ")
  fcp_file[line_counter+3]<-paste0("#RetPri-0 Standard Standard 1.0 1.0 1.0 0 1 1 1 1 1 1 1 0 0 0.5"," ",tr_names[i])
  fcp_file[line_counter+4]<-paste("#RetPri-1 Low Defualt$Low$Thinning 1.0 0.7 0.2 0.0 1.0 0.9 0.6 1.0 0.1 1.0 1.0 0.5 0.6 0.8 _None")
  fcp_file[line_counter+5]<-paste("#RetPri-2 High Default$High$Thinning 1.0 0.7 0.2 0.0 0.4 0.5 1.0 0.1 1.0 1.0 1.0 0.5 0.6 0.6 _None")
  fcp_file[line_counter+6]<-paste("#RetPri-3 Diam Defualt$diameter$limit$harvest 1.0 0.1 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.5 1.0 0.1 _None")
  fcp_file[line_counter+7]<-paste0("#RetPri Standard")
  fcp_file[line_counter+8]<-paste0("#Treatment-0"," ", tr_names[i], " ", "TreeDensity"," ",thinning_target_order[i]," ","100 0  0 0 0 100"," ",prune[i], " "," 60 0 0 0 0  0 0 0 InteriorWest Summer", " ")
  
  #THINNING/TREATMENT SETTINGS
  fcp_file[line_counter+9]<-paste0(" ")
  fcp_file[line_counter+10]<-paste0("#Treatment"," ", tr_names[i])
  fcp_file[line_counter+11]<-paste0("# Thinning  ")
  fcp_file[line_counter+12]<-paste0("#Thin ",Thin)
  fcp_file[line_counter+13]<-paste0("#ThinType TreeDensity")
  fcp_file[line_counter+14]<-paste0("#ThinCriteria"," ", thinning_target_order[i],".0000")
  fcp_file[line_counter+15]<-paste0("#ThinCutEff 100.0000")
  fcp_file[line_counter+16]<-paste0("#ThinMinCut   0.0000")
  fcp_file[line_counter+17]<-paste0("#ThinMaxCut 100.0000")
  fcp_file[line_counter+18]<-paste0("#ThinSubMer   0.0000")
  fcp_file[line_counter+19]<-paste0("#ThinLogTop   0.0000") #remove all tops
  fcp_file[line_counter+20]<-paste0("#ThinYarLos  100.0000") #assume 100% loss from yarding
  
  #PRUNE SETTINGS
  fcp_file[line_counter+21]<-paste0(" ")
  fcp_file[line_counter+22]<-paste0("------------------------------------------")
  fcp_file[line_counter+23]<-paste0("# Prunning  ")
  fcp_file[line_counter+24]<-paste0("#Prune ",PruneYesNo)
  fcp_file[line_counter+25]<-paste0("#PruneHgt   ",prune[i],".0000")
  fcp_file[line_counter+26]<-paste0("#PruneMRCR  60.0000")
  
  #PILING INFO, NOT APPLICABLE
  fcp_file[line_counter+27]<-paste0(" ")
  fcp_file[line_counter+28]<-paste0(" ----------------------------------------------")
  fcp_file[line_counter+29]<-paste0("# Piling  ")
  fcp_file[line_counter+30]<-paste0("#Pile No")
  fcp_file[line_counter+31]<-paste0("#PileGround   0.0000")
  fcp_file[line_counter+32]<-paste0("#PileSlash   0.0000")
  fcp_file[line_counter+33]<-paste0("#PileBurn   0.0000")
  
  #PLOT FILE, AND MISCELLANEOUS
  fcp_file[line_counter+34]<-paste0(" ")
  fcp_file[line_counter+35]<-paste0("#DecimalPrecision 2")
  fcp_file[line_counter+36]<-paste0("#PlotFileName ",path,"/FuelCalcBC/",tr_names[i],".bcd")
  fcp_file[line_counter+37]<-paste0("#PlotFileType bcd ")
  fcp_file[line_counter+38]<-paste0("#PlotName ")
  fcp_file[line_counter+39]<-paste0("#Color1 ")
  fcp_file[line_counter+40]<-paste0("#FuelModelShow Yes")
  
  # Save the modified file (optional)
  writeLines(fcp_file, paste0(path,"/FuelCalcBC/", TR, ".fcp"))
}


#FuelCalc USA Input Files:----------------------

#HardCoded-----------
herbload<-NULL
shrubload<-NULL
soil<- "Fine"
EmissionFlag <- case_when(
  EmissionFactor == 1 ~ "1$-$Southeastern$Forest",
  EmissionFactor == 2 ~ "2$-$Boreal$Forest",
  EmissionFactor == 3 ~ "3$-$Western$Forest$-$RX",
  EmissionFactor == 4 ~ "4$-$Western$Forest$-$WF",
  EmissionFactor == 5 ~ "5$-$Shrubland",
  TRUE ~ "6$-$Grassland")
ScorchHeight=BoleCharHeight*2*3.28084 #turn scorch height to feet double bole char height
duffmoist<-ifelse(moist =="VeryDry",20,ifelse(moist == "Dry", 40, ifelse(moist == "Moderate",75,130)))
k1moist<- ifelse(moist =="VeryDry",10,ifelse(moist == "Dry", 15, ifelse(moist == "Moderate",30,40)))
soilmoist<-ifelse(moist =="VeryDry",5,ifelse(moist == "Dry", 10, ifelse(moist == "Moderate",15,25)))


#---------

  #Load files
  stratas<- unique(Snap_EX$Stratum)
  Fuels<-Snap_fuels
  US<-Snap_US
  OS<-Snap_OS
  General<-Snap_EX
  
  #Creating Input FFI Files
  for(st in stratas){
    i <- which(stratas == st)
    
    #new blank template
    FFI_Out<-ffi_temp
    
    #load data for just this strata
    strata<-st
    strata_df<-Fuels %>% filter(Stratum == strata)
    
    #get number of plots
    plots<- unique(strata_df$Plot..)
    n_plots<-length(plots)
    
    # preallocate: clone structure, expand to n_plots rows of NA
    treat_out <- treatment_temp[rep(1, n_plots), ]
    treat_out[] <- NA
    
    
    #pull burning flags and others
    BurnFlag<-BurningFlags[i]
    ThinFlag<-ThinFlags[i]
    PruneFlag<-PruneFlags[i]
    
    #pull prune value
    prune_val<-prune[i]
    
    #pull emission factor
    EmissionKey<-EmissionFactor[i]
    
    #add a row for each plot data
    for(pl in plots){
      
      rownumber <- match(pl, plots)
      plot_df<-strata_df %>% filter(Plot..==pl)
      #create new row
      
      #process herb and shrub
      if(is.null(herbload)){
        herbload<-(plot_df$Avg.Herb.Loading..kg.m2.+plot_df$Avg.Grass.Loading..kg.m2.)*4.05
        if(is.na(herbload)){
          herbload<-(plot_df$Avg.Grass.Loading..kg.m2.)*4.0
        }
      }
      if(is.null(shrubload)){
        shrubload<-(plot_df$Avg.Shrub.Loading..kg.m2.)*4.05
        if(is.na(shrubload)){shrubload=0}
      }
      
      #divide 1000hrs into 5 classes for fuels assume spread evenly
      x1000hr_5<-((plot_df$LWD.fuels..7.0.20.0cm...kg.m2.+plot_df$CWD.fuels...20.0cm...kg.m2.)*4.05)/5
      
      #order of inputs
      #TRandPlotId,Status,Order,TotalDuffLoad,DuffDuffDepth,LitterLoad,LichenLoad,LiveMossLoad,DeadMossLoad,OneHour,TenHour,HundredHour,ThousandHourDc1Sz1,ThousandHourDc1Sz2,ThousandHourDc1Sz3,ThousandHourDc1Sz4,ThousandHourDc1Sz5,ThousandHourDc2Sz1,ThousandHourDc2Sz2,ThousandHourDc2Sz3,ThousandHourDc2Sz4,ThousandHourDc2Sz5,ThousandHourDc3Sz1,ThousandHourDc3Sz2,ThousandHourDc3Sz3,ThousandHourDc3Sz4,ThousandHourDc3Sz5,ThousandHourDc4Sz1,ThousandHourDc4Sz2,ThousandHourDc4Sz3,ThousandHourDc4Sz4,ThousandHourDc4Sz5,ThousandHourDc5Sz1,ThousandHourDc5Sz2,ThousandHourDc5Sz3,ThousandHourDc5Sz4,ThousandHourDc5Sz5,HerbLoadDead,HerbLoadLive,ShrubLoadDead,ShrubLoadLive,ShrubLiveSAV,HerbSAV,WoodySAVOneHour,FractionGroundAreaPileCovered,PileLoadWholePlot,EmisSTFS,EmisDuffRSC,EmisCWDRSC
      
      row<-c(paste0(paste0("TU.",st,"_","PL.",pl),",","Treatment",",",1,",",plot_df$Duff.Depth..cm.*0.393*10,",",plot_df$Duff.Depth..cm.*0.393,",",round(plot_df$Avg.1.hr.fuels...0.6.cm...kg.m2.*10/2.47,2),",",0,",",0,",",0,",",round(plot_df$Avg.1.hr.fuels...0.6.cm...kg.m2.*10/2.47,2),",",round(plot_df$Avg.10.hr.fuels..0.6.2.5cm...kg.m2.*10/2.47,2),",",round(plot_df$Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.*10/2.47,2),",",x1000hr_5,",",x1000hr_5,",",x1000hr_5,",",x1000hr_5,",",x1000hr_5,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",0,",",",",0,",",x1000hr_5,",",herbload/2,",",herbload,",",shrubload/2,",",shrubload,",",1500,",",1500,",",2500,",",10,",",11,",",EmissionKey,",",8,",",7))
      
      #add to template
      FFI_Out[1+rownumber]<- row
      
      #create treatment file
      treat_out$Plot[rownumber]<-paste0("TU.",st,"_","PL.",pl)
      treat_out$MonSta[rownumber]<-"Treatment"
      treat_out$RetPri[rownumber]<- "Standard"
      treat_out$Treatment[rownumber]<-st
      treat_out$Thin[rownumber]<-ifelse(ThinFlag== TRUE,"Yes","No")
      treat_out$Prune[rownumber]<-ifelse(PruneFlag== TRUE,"Yes","No")
      treat_out$Pile[rownumber]<-"No"
      treat_out$Burn[rownumber]<-ifelse(BurnFlag== TRUE,"Yes","No")
      
      #reset herbload
      herbload=NULL
      shrubload=NULL
      
    }
    
    #Export
    writeLines(FFI_Out, paste0(path,"/FuelCalc/","TU_",st,"_FuelCalc_FFI.ffi"))
    write.csv(treat_out, paste0(path,"/FuelCalc/","TU_",st,"_TreatmentFile.csv"),row.names = FALSE)
  }
  
  #Creating Input FCP Files
  for (i in 1:length(unique(Snap_EX$Stratum))) {
    TR<- tr_names[i]
    strata_df<-Fuels %>% filter(Stratum == TR)
    
    inputfolder<-paste0(TR,"_tables")
    cutting_specs<-paste0(path,s_s_prefix,inputfolder,"/cuttingSpecs_",TR,".csv")
    cuts <- read.csv(cutting_specs)
    desc_cols <- c("Stand.Layer", "DBH.Class")
    pct_cols  <- names(cuts)[grepl("\\..%$", names(cuts))]
    cuts <- cuts[, c(desc_cols, pct_cols)]
    names(cuts) <- gsub("\\..%$", "", names(cuts))
    
    # Remove unwanted columns
    cuts <- cuts %>% dplyr::select(-Stand.Layer)
    cuts <- cuts %>% dplyr::select(-starts_with("X"))
    
    #get thin target in per acre value
    thin_target <- round(thinning_target_order[i]/2.47105,0)
    fcp_file <- FUELCALC_US_TEMPLATE
    
    # Set the other species line (which does not change)
    fcp_file[2] <- paste("#Species-0", TR, "_Other", "0 100 1.0")
    #change column names to reselect species
    cuts<-rename_and_merge_species(cuts)
    
    # Get the species and DBH size classes
    species <- cuts %>% dplyr::select(-DBH.Class) %>% colnames()
    
    #remove duplicate species now that they bare cleaned and crosswalk to US
    species<-unique(species)
    #species<- species_crosswalk(species)
    
    #EXTRACT DBH CLASSES
    dbh_classes<- cuts$DBH.Class
    dbh_classes[9]<-"45"
    
    #get emission key
    EmissionKey<-EmissionFlag[i]
    
    #get moisture
    moist<-moistRegimes[i]
    
    #pull burning flags and others
    BurnFlag<-BurningFlags[i]
    ThinFlag<-ThinFlags[i]
    PruneFlag<-PruneFlags[i]
    
    #pull prune value
    prune_val<-prune[i]
    
    #get scorchheight
    scorch_ht<-ScorchHeight[i]
    
    #get season
    szn<-Season[i]
    
    # Initialize line counter starting from 3
    line_counter <- 3
    
    # Loop through each species
    for (j in 1:length(species)) {
      # Get the original row indices where the cutting percentage is greater than 0
      valid_indices <- which(cuts[[species[j]]] > 0)
      
      # Loop through each valid row for the species
      for (k in valid_indices) {
        lower_bound <- strsplit(dbh_classes[k], "-")[[1]][1] # Get the lower bound of DBH class
        upper_bound <- strsplit(dbh_classes[k], "-")[[1]][2] # Get the upper bound of DBH class
        
        # Handle cases where the upper bound is missing (e.g., "45+")
        if (is.na(upper_bound)) {
          upper_bound <- "100" # Set upper bound to 100 for "45+" class
        }
        
        # Get the cutting percentage and round it
        cutting_percentage <- cuts[[species[j]]][k]
        rounded_percentage <- (100 - round_to_nearest_10_custom(cutting_percentage)) / 100
        
        if(rounded_percentage == 1){
          rounded_percentage = paste0(rounded_percentage,".0")
        }else if(rounded_percentage == 0){
          rounded_percentage = paste0("0.",rounded_percentage)
        } else(
          rounded_percentage = rounded_percentage
        )
        
        #get US Species
        US_spp<-species_crosswalk(species[j])
        
        # Format the line with species number, treatment name, species, lower DBH, upper DBH, and rounded percentage
        fcp_file <- append(fcp_file, paste0("#Species-", line_counter-2, " ", tr_names[i], " ", US_spp, " ", 
                                            lower_bound, " ", upper_bound, " ", rounded_percentage))
        line_counter <- line_counter +1
      }
    }
    #
    if(moist=="VeryDry"){
      moist_lvl<-c(6,10,20)
    }else if(moist == "Dry"){
      moist_lvl<-c(10,12,30)
    }else if(moist == "Moderate"){
      moist_lvl<-c(16,30,75)
    }else{
      moist_lvl<-c(22,40,130)
    }
    scorchflag<-ifelse(is.null(scorch_ht),round((prune_val*3.28084)-1,4),scorch_ht)
    #SELECTING SPECIES GROUP AND RETENTION PRIORTIY
    fcp_file[line_counter]<-paste0(" ")
    fcp_file[line_counter+1]<-paste0("#SpeciesGroup-0"," ", tr_names[i], " ", 1)
    fcp_file[line_counter+2]<-paste0(" ")
    fcp_file[line_counter+3]<-paste0("#RetPri-0 Standard Standard 1.0 1.0 1.0 0 1 1 1 1 1 1 1 0 0 0.5"," ",tr_names[i])
    fcp_file[line_counter+4]<-paste("#RetPri-1 Low Defualt$Low$Thinning 1.0 0.7 0.2 0.0 1.0 0.9 0.6 1.0 0.1 1.0 1.0 0.5 0.6 0.8 _None")
    fcp_file[line_counter+5]<-paste("#RetPri-2 High Default$High$Thinning 1.0 0.7 0.2 0.0 0.4 0.5 1.0 0.1 1.0 1.0 1.0 0.5 0.6 0.6 _None")
    fcp_file[line_counter+6]<-paste("#RetPri-3 Diam Defualt$diameter$limit$harvest 1.0 0.1 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.5 1.0 0.1 _None")
    fcp_file[line_counter+7]<-paste0("#RetPri Standard")
    fcp_file[line_counter+8]<-paste0("#Treatment-0"," ", tr_names[i], " ", "TreeDensity"," ",thin_target," ","100 0  0 0 0 100"," ",round((prune_val*3.28084),4), " "," 60 0 0 0 ",scorchflag," ",moist_lvl[1]," ",moist_lvl[2]," ",moist_lvl[3]," PacificWest ",szn, " ") 
    
    #THINNING/TREATMENT SETTINGS
    fcp_file[line_counter+9]<-paste0(" ")
    fcp_file[line_counter+10]<-paste0("#Treatment"," ", tr_names[i])
    fcp_file[line_counter+11]<-paste0("# Thinning  ")
    fcp_file[line_counter+12]<-paste0("#Thin ",ifelse(ThinFlag== TRUE,"Yes","No"))
    fcp_file[line_counter+13]<-paste0("#ThinType TreeDensity")
    fcp_file[line_counter+14]<-paste0("#ThinCriteria"," ", thin_target)
    fcp_file[line_counter+15]<-paste0("#ThinCutEff 100.0000")
    fcp_file[line_counter+16]<-paste0("#ThinMinCut   0.0000")
    fcp_file[line_counter+17]<-paste0("#ThinMaxCut 100.0000")
    fcp_file[line_counter+18]<-paste0("#ThinSubMer   0.0000")
    fcp_file[line_counter+19]<-paste0("#ThinLogTop   0.0000") #remove all tops
    fcp_file[line_counter+20]<-paste0("#ThinYarLos  100.0000") #assume 100% loss from yarding
    
    #PRUNE SETTINGS
    fcp_file[line_counter+21]<-paste0(" ")
    fcp_file[line_counter+22]<-paste0("------------------------------------------")
    fcp_file[line_counter+23]<-paste0("# Prunning  ")
    fcp_file[line_counter+24]<-paste0("#Prune ",ifelse(PruneFlag== TRUE,"Yes","No"))
    fcp_file[line_counter+25]<-paste0("#PruneHgt   ",round((prune_val*3.28084),4))
    fcp_file[line_counter+26]<-paste0("#PruneMRCR  60.0000")
    
    #Burning: If applicable
    duffdepth<-round(mean(strata_df$Duff.Depth..cm.)*0.393701,4)
    
    fcp_file[line_counter+27]<-paste0(" ")
    fcp_file[line_counter+28]<-paste0("------------------------------------------")
    fcp_file[line_counter+29]<-paste0("# Broadcast burning  ")
    fcp_file[line_counter+30]<-ifelse(BurningFlags[i] == TRUE,paste0("#BCB Yes"),paste0("#BCB No"))  
    
    fcp_file[line_counter+31]<-paste0("#BCBScoHgt   ",scorchflag)
    fcp_file[line_counter+32]<-paste0("#BCBDuDep  ",duffdepth)
    fcp_file[line_counter+33]<-paste0("#BCBMosLicBur  90.0000")
    fcp_file[line_counter+34]<-paste0("#BCBMoist ",moist)
    
    
    
    fcp_file[line_counter+35]<-paste0("#BCBMoi10Hr  ",moist_lvl[1])
    fcp_file[line_counter+36]<-paste0("#BCBMoi1kHr  ",moist_lvl[2])
    fcp_file[line_counter+37]<-paste0("#BCBMoiDuf  ",moist_lvl[3])
    fcp_file[line_counter+38]<-paste0("#BCBRegion PacificWest")
    fcp_file[line_counter+39]<-paste0("#BCBSeason ", szn)
    fcp_file[line_counter+40]<-paste0(" ")
    fcp_file[line_counter+41]<-paste0(" ")
    fcp_file[line_counter+42]<-paste0("#EmisType  Expanded")
    fcp_file[line_counter+43]<-paste0("#EmisFlame  ",EmissionKey)
    fcp_file[line_counter+44]<-paste0("#EmisDuff  8$-$Duff$RSC")
    fcp_file[line_counter+45]<-paste0("#EmisCoarse  7$-$Woody$RSC")
    
    #PILING INFO, NOT APPLICABLE
    fcp_file[line_counter+46]<-paste0(" ")
    fcp_file[line_counter+47]<-paste0(" ----------------------------------------------")
    fcp_file[line_counter+48]<-paste0("# Piling  ")
    fcp_file[line_counter+49]<-paste0("#Pile No")
    fcp_file[line_counter+50]<-paste0("#PileGround   0.0000")
    fcp_file[line_counter+51]<-paste0("#PileSlash   0.0000")
    fcp_file[line_counter+52]<-paste0("#PileBurn   0.0000")
    
    #Fuel Model selection:
    BedDepth<-round(mean(strata_df$Average.Fuel.Bed.Depth..0.1cm.)*3.28084,4)
    if(surf_fuel[i] == "Grass" ){
      SAV_vals<-c(1500,1500,3500)
    }else if(surf_fuel[i] == "Chaparral"){
      SAV_vals<-c(1500,1500,2000)
    }else if(surf_fuel[i] == "TimberLitter"){
      SAV_vals<-c(1500,1500,2500)
    }else{
      SAV_vals<-c(1500,1500,1500)
    }
    
    fcp_file[line_counter+53]<-paste0(" ")
    fcp_file[line_counter+54]<-paste0(" ----------------------------------------------")
    fcp_file[line_counter+55]<-paste0("# Fuel Model Selector  ")
    fcp_file[line_counter+56]<-paste0("#FMSModel All")
    fcp_file[line_counter+57]<-paste0("#FMSClimate ",climate[i])
    fcp_file[line_counter+58]<-paste0("#FMSBedDep ",BedDepth)
    fcp_file[line_counter+59]<-paste0("#FMSSAVGra ",SAV_vals[1])
    fcp_file[line_counter+60]<-paste0("#FMSSAVShu ",SAV_vals[2])
    fcp_file[line_counter+61]<-paste0("#FMSSAV1Hr ",SAV_vals[3])
    
    #PLOT FILE, AND MISCELLANEOUS
    fcp_file[line_counter+62]<-paste0(" ")
    fcp_file[line_counter+63]<-paste0("#DecimalPrecision 2")
    fcp_file[line_counter+64]<-paste0("#PlotFileType FFI")
    fcp_file[line_counter+65]<-paste0("#PlotName ","TU_",tr_names[i])
    fcp_file[line_counter+66]<-paste0("#Color1 ")
    fcp_file[line_counter+67]<-paste0("#FuelModelShow Yes")
    
    # Save the modified file (optional)
    writeLines(fcp_file, paste0(path,"/FuelCalc/TU_",tr_names[i],"_FuelCalc_FFI.fcp"))
  }
  
  #Create Plot folders for fuelcalc
  for(i in 1:length(unique(Snap_EX$Stratum))){
    tr<-unique(Snap_EX$Stratum)[i]
    treatment_folder <- paste0(path,"/FuelCalc/","Plot Files/",tr,"_Plots")
    
    # Create the directory if it doesn't already exist
    if (!dir.exists(treatment_folder)) {
      dir.create(treatment_folder, recursive = TRUE)
    }
  }
  
  #Create Batch Files:
  #make batch files
  outputs<-paste0(path, "/FuelCalc/Outputs/")
  dir.create(outputs)
  
  # Make batch files
  for (i in 1:length(unique(Snap_EX$Stratum))) {
    TR <- tr_names[i]
    strata_df <- Fuels %>% filter(Stratum == TR)
    
    # Setup names and folders
    input_folder <- paste0(path, "/FuelCalc/")
    basename <- paste0("TU_", tr_names[i], "_FuelCalc_FFI")
    
    #Create output folder
    out_folder<-paste0(input_folder,"Outputs/TU_", tr_names[i])
    dir.create(out_folder)
    
    # Define the 6 files for FuelCalc command
    ffi_file <- paste0(basename, ".ffi")
    tre_file <- paste0(basename, ".tre")
    treatment_file <- paste0("TU_", tr_names[i], "_TreatmentFile.csv")
    project_file <- paste0(basename, ".fcp")
    output_file <- paste0(basename, "_Outputs.csv")
    error_file <- paste0(basename, "_Errors.txt")
    
    # Move files
    file.rename(from = paste0(input_folder, ffi_file),
                to = paste0(out_folder, "/", ffi_file))
    file.rename(from = paste0(input_folder, tre_file),
                to = paste0(out_folder, "/", tre_file))
    file.rename(from = paste0(input_folder, treatment_file),
                to = paste0(out_folder, "/", treatment_file))
    file.rename(from = paste0(input_folder, project_file),
                to = paste0(out_folder, "/", project_file))

  #FuelCalc must be here!!!!!!:
    #C:\\Program Files (x86)\\FuelCalc1.7\
#--------------------------------------------------------
    
    # Create batch file content
    batch_content <- paste0(
      "@echo off\n",
      "set path=%path%;C:\\Program Files (x86)\\FuelCalc1.7\n",
      "cd /d ", gsub("/", "\\\\", out_folder), "\n",
      "fc_gui ", ffi_file, " ", tre_file, " ", treatment_file, " ", 
      project_file, " ", output_file, " ", error_file, "\n",
      "exit /b %errorlevel%"
    )
    
    # Write batch file
    batch_filename <- paste0(out_folder,"/Run_FuelCalc_TU_", tr_names[i], ".bat")
    writeLines(batch_content, batch_filename)
    
    cat("Created batch file:", batch_filename, "\n")
  }
  
#----------------------------------------------------
  #End of Code
  
#Run FuelCalc!!!!
  step_dir <- file.path(cfg$runtime$intermediate_dir, "step1_process_to_fuelcalc")
  dir.create(step_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- list(
    project_name = project_name,
    project_root = cfg$runtime$project_root,
    raw_dir = cfg$runtime$raw_dir,
    cutting = cutting,
    strata = as.character(unique(Snap_EX$Stratum)),
    strata_count = length(unique(Snap_EX$Stratum)),
    treatment_names = as.character(tr_names),
    fuelcalc_bc_bcd_files = list.files(file.path(path, "FuelCalcBC"), pattern = "\\.bcd$", full.names = TRUE),
    fuelcalc_bc_fcp_files = list.files(file.path(path, "FuelCalcBC"), pattern = "\\.fcp$", full.names = TRUE),
    fuelcalc_bc_tre_files = list.files(file.path(path, "FuelCalcBC"), pattern = "\\.tre$", full.names = TRUE),
    fuelcalc_ffi_files = list.files(file.path(path, "FuelCalc", "Outputs"), pattern = "\\.ffi$", recursive = TRUE, full.names = TRUE),
    fuelcalc_treatment_files = list.files(file.path(path, "FuelCalc", "Outputs"), pattern = "_TreatmentFile\\.csv$", recursive = TRUE, full.names = TRUE),
    fuelcalc_batch_files = list.files(file.path(path, "FuelCalc", "Outputs"), pattern = "^Run_FuelCalc_.*\\.bat$", recursive = TRUE, full.names = TRUE)
  )
  saveRDS(summary, file.path(step_dir, "process_to_fuelcalc_outputs.rds"))
  jsonlite::write_json(summary, file.path(step_dir, "process_to_fuelcalc_summary.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
}
