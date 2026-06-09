step3 <- function(cfg, root) {
cat("Running Step 3: FireModel Results\n")

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
  a
}

step_cfg <- cfg$firemodel_results %||% list()
target_env <- environment()

as_char_vec <- function(x, default) as.character(unlist(x %||% default, use.names = FALSE))
as_num_vec <- function(x, default) as.numeric(unlist(x %||% default, use.names = FALSE))
as_logical_vec <- function(x, default) {
  vals <- unlist(x %||% default, use.names = FALSE)
  if (is.logical(vals)) return(as.logical(vals))
  tolower(as.character(vals)) %in% c("true", "t", "1", "yes", "y")
}
RHtoVPD <- function(RH, Temp, Pa = 101) {
  rh_frac <- pmax(pmin(as.numeric(RH), 100), 0) / 100
  temp_c <- as.numeric(Temp)
  es_kpa <- 0.6108 * exp((17.27 * temp_c) / (temp_c + 237.3))
  es_kpa * (1 - rh_frac)
}
source_local_function <- function(fname) {
  f <- file.path(root, "R_functions", fname)
  if (!file.exists(f)) stop("Missing R_functions file: ", f)
  source(f, local = target_env)
}

#Libraries Loading:
library(dplyr)
library(stringr)
library(cffdrs)
library(ggplot2)
library(grid)
library(firebehavioR)
library(progress)
library(flextable)

project_name <- cfg$project_name %||% ""
if (!nzchar(project_name)) stop("config project_name is required for FireModel Results")
name <- project_name
project <- name

#File Paths:
path <- cfg$runtime$raw_dir
step_dir <- file.path(cfg$runtime$outputs_dir, "step3_fire_model")
dir.create(step_dir, recursive = TRUE, showWarnings = FALSE)
progress_path <- file.path(step_dir, "step3_progress.json")

write_progress <- function(status, period = NULL, weather_index = NULL, weather_total = NULL,
                           stratum = NULL, iter = NULL, total_iters = NULL, message = NULL,
                           force_console = FALSE) {
  payload <- list(
    project_name = project_name,
    status = status,
    timestamp = as.character(Sys.time()),
    period = period,
    weather_index = weather_index,
    weather_total = weather_total,
    stratum = stratum,
    iter = iter,
    total_iters = total_iters,
    message = message
  )
  jsonlite::write_json(payload, progress_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  if (force_console || !is.null(message)) {
    cat(sprintf(
      "[Step3] %s | status=%s | period=%s | weather=%s/%s | stratum=%s | iter=%s/%s%s\n",
      payload$timestamp,
      status,
      ifelse(is.null(period), "-", as.character(period)),
      ifelse(is.null(weather_index), "-", as.character(weather_index)),
      ifelse(is.null(weather_total), "-", as.character(weather_total)),
      ifelse(is.null(stratum), "-", as.character(stratum)),
      ifelse(is.null(iter), "-", as.character(iter)),
      ifelse(is.null(total_iters), "-", as.character(total_iters)),
      ifelse(is.null(message), "", paste0(" | ", message))
    ))
  }
}

write_progress(status = "starting", message = "Step 3 initialized", force_console = TRUE)

# Prefix for file paths
snap_prefix <- "/SNAP/"
FWI_prefix <- "/Weather/"
Fuel_prefix <- "/FireBehavior/Inputs/"
cath_prefix <- "/Cut Specs for run/"
Fire_out <- "/FireBehavior/Outputs/"
s_s_prefix <- "/Stand_StockTables/"
out_slash <- paste0(path, "/FuelCalcBC/Outputs/Slash/")
out_fuelcalc <- paste0(path,"/FuelCalcBC/Outputs/")
fuelcalc <- paste0(path,"/FuelCalcBC/")
in_weather <- paste0("/Weather/")
out_weather <- paste0("/Weather/")
out_residuals <- paste0(path, "/FuelCalcBC/Outputs/Slash/Residuals/")
results <- "/Outputs/"
path_fcp <- paste0(path,"/FuelCalcBC/")

dir.create(file.path(path, "FireBehavior", "Outputs"), recursive = TRUE, showWarnings = FALSE)

# Load in SNAP summary files
Snap_OS = read.csv(paste0(path,snap_prefix,project,"_OS.csv"))
Snap_US = read.csv(paste0(path,snap_prefix,project,"_US.csv"))
Snap_EX= read.csv(paste0(path,snap_prefix,project,"_EXTRA.csv"))
Snap_fuels= read.csv(paste0(path,snap_prefix,project,"_FUELS.csv"))


#Functions:
mode <- function(x) {
  unique_vals <- unique(x)
  unique_vals[which.max(tabulate(match(x, unique_vals)))]
}

read_cutting_specs_for_firemodel <- function(cutting_specs) {
  cuts <- read.csv(cutting_specs, check.names = FALSE, stringsAsFactors = FALSE)
  names(cuts) <- trimws(names(cuts))

  desc_cols <- c("Stand.Layer", "DBH.Class")
  missing_desc <- setdiff(desc_cols, names(cuts))
  if (length(missing_desc)) {
    stop(
      "Cutting spec file is missing required columns in ",
      cutting_specs,
      ": ",
      paste(missing_desc, collapse = ", ")
    )
  }

  pct_cols <- grep("(\\.\\%|\\.\\.)$", names(cuts), value = TRUE)
  pct_cols <- setdiff(pct_cols, desc_cols)
  if (!length(pct_cols)) {
    stop(
      "Cutting spec file has no species percent columns in ",
      cutting_specs,
      ". Expected headers like Fd.% or Fd.."
    )
  }

  cuts <- cuts[, c(desc_cols, pct_cols), drop = FALSE]
  names(cuts) <- sub("(\\.\\%|\\.\\.)$", "", names(cuts))
  cuts
}

dbh_class_midpoint <- function(dbh_class) {
  normalized <- gsub("\\s+", "", as.character(dbh_class))
  dplyr::case_when(
    normalized == "0-1.5" ~ 1,
    normalized == "1.5-7.5" ~ 5,
    normalized == "7.5-12.5" ~ 10,
    normalized == "12.5-17.5" ~ 15,
    normalized == "17.5-22.5" ~ 20,
    normalized == "22.5-27.5" ~ 25,
    normalized == "27.5-35" ~ 31.25,
    normalized == "35-45" ~ 40,
    normalized == "45+" ~ 55,
    TRUE ~ NA_real_
  )
}

valid_number <- function(x) {
  length(x) == 1 && !is.na(x) && is.finite(x)
}

source_local_function("Residence_Time_Function_Nelson2003b.R")
source_local_function("Improved_CFIS_perrakis_function.R")
source_local_function("Crosswalk CAN to US Fuel Model Function.R")
source_local_function("Select_Best_Fuel_Model_Rothermel.R")
source_local_function("Find_Nearest_Cell_BC.R")
source_local_function("Prob_Crown_Function_Perrakis2023.R")
source_local_function("fueltypes_crosswalkFBP_function.R")
source_local_function("fueltypes_crosswalk_function.R")
source_local_function("fueltypes_crosswalkFBP_Raster_function.R")
source_local_function("Crown_Area_Function.R")
source_local_function("Get_Season_Function.R")
source_local_function("FMC_Function.R")
source_local_function("SFC_Function.R")
source_local_function("Fine_Fuel_MC_SA_function.R")
source_local_function("initial_spread_index.R")
source_local_function("fire_behavior_prediction_function.R")
source_local_function("surface_fuel_consumption_CANFBP_function.R")
source_local_function("WindGust_Function.R")
source_local_function("rothermel_function_mod.R")
source_local_function("BC_TREECODES_USCodes_function.R")
source_local_function("CFC_Groot_Function.R")
source_local_function("Calculate_Coarse_Load_Function.R")

#UI Inputs(Modifiable):---------------------------------------------------------------------------------------------------

#Set Elevation in Meters
Elevation <- as.numeric(step_cfg$elevation %||% 483) #userinput #What is the elevation of your FTUs?

#Use Custom Fuels or a Model? TRUE or False
CustomFuels <- as_logical_vec(step_cfg$custom_fuels, c(TRUE,TRUE,TRUE)) #dropdown #Do you want to use custom(field measured fuels) or fuel models? one for each treatment #options TRUE or FALSE

#Fuel Structure and Fuel Types:
pruneVECT <- as_num_vec(step_cfg$prune_vector, c(2,2,2))
fuels <- as_num_vec(step_cfg$fuels, c(0.75,0.75,0.75))
hr1000s <- as_num_vec(step_cfg$hr1000s, c(1,1,1))
ftcad_vector <- as_char_vec(step_cfg$ftcad_vector, c("C-7","C-7","C-7"))
Forest_Type <- as_char_vec(step_cfg$forest_type, c("Pine","Pine","Pine","Pine","Pine","Pine"))
SurfFuel <- as_char_vec(step_cfg$surf_fuel, c("grass","grass","grass","grass","grass","grass"))


#Fuel Strata Gap Considerations
FSG.Mod.Flag <- as_logical_vec(step_cfg$fsg_mod_flag, c(FALSE,FALSE,FALSE))
FSG.Field.Flag <- as_logical_vec(step_cfg$fsg_field_flag, c(TRUE,TRUE,TRUE))


#Fire Modeling
IntensityFlag <- as.character(step_cfg$intensity_flag %||% "Byram")
fuelmoisturetype <- as.character(step_cfg$fuel_moisture_type %||% "Model")
HFlag <- as.character(step_cfg$heat_flag %||% "Manual")
WindGustMod <- isTRUE(step_cfg$wind_gust_mod %||% FALSE)
CrownFireType <- as_char_vec(step_cfg$crown_fire_type, c("Wagner","Wagner","Wagner"))
CrownFireModel <- as_char_vec(step_cfg$crown_fire_model, c("Perrakis","Perrakis","Perrakis"))

#userinput #What is your grass curing percentage? 
  #Recommended 
  #0-10:end winter/spring fully live
  #10-30:late spring, early summer
  #30-70:mid summer
  #70+:late summer, drought conditions, or fall
  GrassCuring <- as.numeric(step_cfg$grass_curing %||% 75)
  
#Weather System:
NumWeathers <- as.numeric(step_cfg$num_weathers %||% 150)
WeatherName <- as.character(step_cfg$weather_name %||% "MERRITT 2 HUB")
Season <- as.character(step_cfg$season %||% "Summer")
process_cfg <- cfg$process_to_fuelcalc %||% list()
treatment_names <- as_char_vec(process_cfg$tr_names, unique(Snap_EX$Stratum))
treatment_names <- treatment_names[nzchar(treatment_names)]
if (!length(treatment_names)) {
  stop("No treatments are configured for Step 3.")
}

#Plotting
AdvancedModels <- isTRUE(step_cfg$advanced_models %||% TRUE)

#PLOT DISPLAY:!!!!!
#-Treatment Summary:paste0(path,Fire_out,"TreatmentSummaryTable.png")
#-Crown Fire Probability BoxPlots: paste0(path,Fire_out,"ProbabilityCrownFireBoxPlot.png")
#-Crowning Index at Windspeed: paste0(path,Fire_out,"CrownProbWindSpeed.png")
#-Crowning Index at Fuelmoist: paste0(path,Fire_out,"CrownProbFuelMoist.png")
#-Head Fire Intensity: paste0(path, Fire_out, "MedianHFIBarPlot.png")
#-Rate of Spread: paste0(path, Fire_out, "MedianROSBarPlot.png")
#-FBP 90th CSI Stand:paste0(path,Fire_out,"FBP_CSISummaryTable.png")



#Stationary Inputs:-------------------------------------------------
coniferList <- c("Ba","Bl", "Bg","Bb","Cw", "Fd", "Hw","T","Pl", "Sx","Sb","Sw", "Lw", "Lt", "Pw","Yc", "Fdi", "Fdc", "Py","Pli")
nonConiferList <- c("Act","Acb","Ac","At","Ep","DP", "DU","Dead", "Dr")
academic_theme <- theme_bw(base_size = 12, base_family = "serif") +
  theme(
    panel.grid.minor   = element_line(color = "grey94", linetype = "dotted", linewidth = 0.3),
    panel.grid.major   = element_line(color = "grey85", linetype = "dashed", linewidth = 0.4),
    panel.border       = element_rect(color = "black", linewidth = 0.8, fill = NA),
    strip.background   = element_rect(fill = "grey92", color = "black", linewidth = 0.8),
    strip.text         = element_text(face = "bold", size = 9, family = "serif"),
    axis.text.x        = element_text(size = 9, family = "serif"),
    axis.text.y        = element_text(size = 9, family = "serif"),
    axis.title         = element_text(face = "bold", size = 11, family = "serif"),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    plot.title         = element_text(face = "bold", size = 15, hjust = 0, family = "serif"),
    plot.subtitle      = element_text(size = 10, hjust = 0, color = "grey30", family = "serif"),
    plot.caption       = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", family = "serif"),
    legend.text        = element_text(family = "serif"),
    legend.background  = element_rect(color = "grey70", linewidth = 0.4),
    legend.key.size    = unit(0.6, "cm"),
    plot.margin        = margin(10, 15, 10, 10)
  )

#------------------------------------------------------------------
#Create input datasets:------------------------------------------------------
#Load outputs created from fuelcalc and set up cleanly:
  #SET NUMBER OF TREATMENTS
  treatments<- treatment_names

str_lists<-list()
fuel_lists<-list()

for(x in 1:length(treatments)){
  tr<-treatments[x]
  treatmentfolder<-paste0("TU_",tr)
  treatmentfilename<-paste0("TU_",tr,"_FuelCalc_FFI_Outputs.csv")
  strfile = read.csv(file.path(root,"projects",project,"data","raw","FuelCalc","Outputs",treatmentfolder,treatmentfilename))
  strfile<-strfile[-1,]
  
  #fuel file #REMOVED UNTILL FUELCALC ERROR FIXED!
  #fuelfilename<-"FuelModel-Post.fmd"
  #fuelfile = readLines(file.path(root,"projects",project,"data","raw","FuelCalc","Outputs",treatmentfolder,fuelfilename))
  #col_names <- c("fuel_model_number", "fuel_model_code", "hr1_fuelload", 
  #              "hr10_fuelload", "hr100_fuelload", "live_herb_load", 
  #             "live_wood_load", "fuel_model_type", "1hr_dead_SAV", 
  #            "herb_SAV", "live_woody_SAV", "fuel_bed_depth", "dead_fuel_Mx", 
  #           "dead_heat_cont", "live_heat_cont", "fuel_model_name")
  #fueldata <- fuelfile[8:length(fuelfile)]
  #fueldf <- read.table(text = fueldata, 
  #                    sep = "",
  #                     fill = TRUE,
  #                    stringsAsFactors = FALSE)
  #colnames(fueldf) <- col_names[1:ncol(fueldf)]
  #change to kg/m^2
  #fueldf$hr1_fuelload<-round(as.numeric(fueldf$hr1_fuelload)*2.47105/10,2)
  #fueldf$hr10_fuelload<-round(as.numeric(fueldf$hr10_fuelload)*2.47105/10,2)
  #fueldf$hr100_fuelload<-round(as.numeric(fueldf$hr100_fuelload)*2.47105/10,2)
  #fueldf$live_herb_load<-round(as.numeric(fueldf$live_herb_load)*2.47105/10,2)
  #fueldf$live_wood_load<-round(as.numeric(fueldf$live_wood_load)*2.47105/10,2)
  #fueldf$fuel_bed_depth<-round(as.numeric(fueldf$fuel_bed_depth)*2.54,2)
  
  
  #get structure
  df_tr <- data.frame(Treatment = str_extract(strfile$PlotID, paste0(tr))
                      ,
                      Plot=as.numeric(gsub(paste0("TU\\.", tr, "_PL\\."), "", strfile$PlotID)),
                      Pre_CBH = round(as.numeric(strfile$preCBH)/3.28084,2),#change to meters
                      Post_CBH =round(ifelse(as.numeric(strfile$postCBH)/3.28084 < pruneVECT[x],pruneVECT[x],as.numeric(strfile$postCBH)/3.28084),2),#change to meters
                      
                      Pre_CFL = round(as.numeric(strfile$preCFL)*2.47105/10,2),#change to tons per ha then to kg/m^2
                      Post_CFL= round(as.numeric(strfile$postCFL)*2.47105/10,2), #change to tons per ha
                      Pre_CBD = round(as.numeric(strfile$preCBD),2),
                      Post_CBD = round(as.numeric(strfile$postCBD),2),
                      Pre_CC=round(as.numeric(strfile$preCC),2),
                      Post_CC=round(as.numeric(strfile$postCC),2),
                      Pre_TPH=round(as.numeric(strfile$preTD)*2.47105,2),#change to tph
                      Post_TPH=round(as.numeric(strfile$postTD)*2.47105,2),#change to tph
                      Pre_HT=round(as.numeric(strfile$preSH)/3.28084,2),#change to meters
                      Post_HT=round(as.numeric(strfile$postSH)/3.28084,2),#change to meters
                      Pre_BA=round(as.numeric(strfile$preBA)/4.359,2),#change to m^2 per ha
                      Post_BA=round(as.numeric(strfile$postBA)/4.359,2),#change to m^2 per ha
                      Pre_VOL=round(as.numeric(strfile$stdCubicFt)/14.3,2),#change to m^3
                      Post_VOL=round((as.numeric(strfile$stdCubicFt)-as.numeric(strfile$HarCubicFt))/14.3,2),#subtract pre standing vol from harvested vol to get post volume and change to m^3
                      Pre_FT=as.numeric(strfile$preFueMod),
                      Post_FT=as.numeric(strfile$postFueMod),
                      stringsAsFactors=FALSE)
  
  #average em
  last_row<-c(tr,"Mean All",mean(df_tr$Pre_CBH),mean(df_tr$Post_CBH),mean(df_tr$Pre_CFL),mean(df_tr$Post_CFL),mean(df_tr$Pre_CBD),mean(df_tr$Post_CBD), mean(df_tr$Pre_CC),mean(df_tr$Post_CC),mean(df_tr$Pre_TPH),mean(df_tr$Post_TPH),mean(df_tr$Pre_HT),mean(df_tr$Post_HT),mean(df_tr$Pre_BA),mean(df_tr$Post_BA),mean(df_tr$Pre_VOL),mean(df_tr$Post_VOL),mode(df_tr$Pre_FT),mode(df_tr$Post_FT))
  last_row[3:length(last_row)] <- round(as.numeric(last_row[3:length(last_row)]), 2)
  df_out<-rbind(df_tr,last_row)
  
  write.csv(df_out,paste0(path,"/FuelCalc/Outputs/TU_",tr,"/TU_",tr,"_StandStructure.csv"),row.names = FALSE)
  #write.csv(fueldf,paste0(path,"/FuelCalc/Outputs/TU_",tr,"/",tr,"_FuelLoadsPost.csv"),row.names = FALSE)
  
  str_lists[[as.character(tr)]]<-df_tr
  #fuel_lists[[as.character(tr)]]<-fueldf
  
}

#Make Large average Data Frames:
STR_all<-do.call(rbind,str_lists)
STR_ALL_out <- STR_all %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarize(
    Pre_CBH  = mean(Pre_CBH[Pre_CBH != 0],   na.rm = TRUE),
    Post_CBH = mean(Post_CBH[Post_CBH != 0], na.rm = TRUE),
    Pre_CFL  = mean(Pre_CFL[Pre_CFL != 0],   na.rm = TRUE),
    Post_CFL = mean(Post_CFL[Post_CFL != 0], na.rm = TRUE),
    Pre_CBD  = mean(Pre_CBD[Pre_CBD != 0],   na.rm = TRUE),
    Post_CBD = mean(Post_CBD[Post_CBD != 0], na.rm = TRUE),
    Pre_CC   = mean(Pre_CC[Pre_CC != 0],     na.rm = TRUE),
    Post_CC  = mean(Post_CC[Post_CC != 0],   na.rm = TRUE),
    Pre_TPH  = mean(Pre_TPH[Pre_TPH != 0],   na.rm = TRUE),
    Post_TPH = mean(Post_TPH[Post_TPH != 0], na.rm = TRUE),
    Pre_HT  = mean(Pre_HT[Pre_HT != 0],   na.rm = TRUE),
    Post_HT = mean(Post_HT[Post_HT != 0], na.rm = TRUE),
    Pre_BA  = mean(Pre_BA[Pre_BA != 0],   na.rm = TRUE),
    Post_BA = mean(Post_BA[Post_BA != 0], na.rm = TRUE),
    Pre_VOL  = mean(Pre_VOL[Pre_VOL != 0],   na.rm = TRUE),
    Post_VOL = mean(Post_VOL[Post_VOL != 0], na.rm = TRUE),
    Pre_FT  = mode(Pre_FT[!is.na(Pre_FT)]),
    Post_FT = mode(Post_FT[!is.na(Post_FT)])
  )

#FUEL_all<-do.call(rbind,fuel_lists)
#FUEL_all$Treatment<-rownames(FUEL_all)
#FUEL_all_out<- FUEL_all %>% dplyr::select(Treatment,fuel_model_code,hr1_fuelload,hr10_fuelload,hr100_fuelload,live_herb_load,live_wood_load,fuel_bed_depth)
write.csv(STR_ALL_out,paste0(path,"/FuelCalc/Outputs/All_Treatments_StandStructure.csv"),row.names = FALSE)
#write.csv(FUEL_all_out,paste0(path,"/FuelCalc/Outputs/All_Treatments_FuelLoadPost.csv"),row.names = FALSE)

#-------------------------------------------------------------------

#Load cleaned structure and create pre and post dataframe input
#generate weighted mean height of all layers by plot for US
#write project shorthand
#Load Pruning
prune_vals<-pruneVECT
strata<-treatment_names


#SET UP DATAFRAMES
PreTreat_df<-data.frame(
  Stratum=NA,
  Height_US=NA,
  US_CBH=NA,
  Fuelcalc_CBH=NA,
  Height=NA,
  DBH=NA,
  TPH=NA,
  BA=NA,
  Vol=NA,
  Duff_depth=NA,
  FB_depth=NA,
  Lit_kg=NA,
  hr_1_kg=NA,
  hr_10_kg=NA,
  hr_100_kg=NA,
  hr_1000_kg=NA,
  LWD_kg=NA,
  CWD_kg=NA,
  CWD_Pieces_5m_ha=NA,
  CFL=NA,
  CBD=NA,
  CC=NA,
  Slope=NA,
  Aspect=NA,
  OS_CBH=NA,
  FSG_Field=NA,
  Modified_CBH=NA,
  US_Centroid=NA,
  Grass_Loading=NA,
  Shrub_Loading=NA,
  Herb_Loading=NA,
  Fueltype_US_FC=NA,
  Fueltype_CAD=NA
)

PostTreat_df<-data.frame(
  Stratum=NA,
  Height_US=NA,
  US_CBH=NA,
  Fuelcalc_CBH=NA,
  Height=NA,
  DBH=NA,
  TPH=NA,
  BA=NA,
  Vol=NA,
  Duff_depth=NA,
  FB_depth=NA,
  Lit_kg=NA,
  hr_1_kg=NA,
  hr_10_kg=NA,
  hr_100_kg=NA,
  hr_1000_kg=NA,
  LWD_kg=NA,
  CWD_kg=NA,
  CWD_Pieces_5m_ha=NA,
  CFL=NA,
  CBD=NA,
  CC=NA,
  Slope=NA,
  Aspect=NA,
  OS_CBH=NA,
  FSG_Field=NA,
  Modified_CBH=NA,
  US_Centroid=NA,
  Grass_Loading=NA,
  Shrub_Loading=NA,
  Herb_Loading=NA,
  Fueltype_US_FC=NA,
  Fueltype_CAD=NA
)

Stand_Structure<-read.csv(paste0(path,"/FuelCalc/Outputs/All_Treatments_StandStructure.csv"))

for(j in 1:length(strata)){
  #read in strata
  st<-strata[j]
  #select for first treatment
  US_df<- Snap_US %>%
    filter(Stratum == st)
  OS_df<- Snap_OS %>%
    filter(Stratum == st)
  EX_df<- Snap_EX %>%
    filter(Stratum == st)
  fuels_df<-Snap_fuels %>%
    filter(Stratum == st)
  structure_data<- Stand_Structure %>%
    filter(Treatment == st)
  
  #Load CAD Fueltype from data and user
  FTCAD_pt<-mode(EX_df$Fuel.Type)
  FTCAD_USER<-ftcad_vector[j]
  
  #read in post treatment slash and stand data
  #post_treat<- read.csv(paste0(out_slash,st,".csv"))
  PT_stand<- read.csv(paste0(path,"/FuelCalc/Outputs/TU_",st,"/TU_",st,"_FuelCalc_FFI_Outputs.csv"))
  PT_stand<-PT_stand[-1,]
  #decide on prune
  prune<-suppressWarnings(as.numeric(prune_vals[j]))
  if(valid_number(prune) && prune > 0){
    pruneflag<-TRUE
  }else{
    pruneflag<-FALSE
    prune<-0
  }
  
  #1.Modify CBH 
  #weighted by the proportionate coverage of under story trees and the 
  #proportionate coverage of over story trees both weighting the CBH's of overstory and undertory. 
  #Also modify overstory CBH by the proportionate decrease in CBD of post treatment
  #or calculate the change in canopy coverage from the proportionate decrease in CBD
  cbd_pre<- as.numeric(structure_data$Pre_CBD)
  cbd_post<- as.numeric(structure_data$Post_CBD)
  CBD_ratio<-cbd_pre/cbd_post
  
  #can also use CBH as post treatment cbh generated by fuelcalc based on canopy bulk density availability
  mean_cbh_PT_FC<- as.numeric(structure_data$Pre_CBH)
  mean_cbh_Post_FC<- as.numeric(structure_data$Post_CBH)
  
  #check if post treatment CBH from fuelcalc is higher than pruning height, if not then reset it to prune height
  if(pruneflag && valid_number(mean_cbh_Post_FC) && prune > mean_cbh_Post_FC){
    mean_cbh_Post_FC<-prune
  }else{
    mean_cbh_Post_FC<-mean_cbh_Post_FC
  }
  
  #calculate TPH for overstory
  prf = (1/(sqrt(mean(OS_df$BAF,na.rm=TRUE))*2))*1
  pr = (prf*OS_df$DBH)*1
  area = ((pr^2)*3.14)/10000
  OS_df$TPH<- 1/area
  
  #weighted mean height of all layers in understory
  weighted_mean_H_US<- US_df %>%
    summarise(weighted_mean_height = weighted.mean(Height..0.1m., w = `X..of.Trees`, na.rm=TRUE)) %>%
    pull(weighted_mean_height)
  
  #IF overstory does not exist this code adjusts for that:---------------------------------------------------------
  if(is.na(OS_df$Stratum[1])){
    
    #set mean height as weighted mean of understory height by number of trees 
    mean_Height<- US_df %>%
      summarise(weighted_mean_height = weighted.mean(Height..0.1m., w = `X..of.Trees`, na.rm=TRUE)) %>%
      pull(weighted_mean_height)
    
    #if no overstory exists then use cbh from under story modified by bulk density change from thinning, use cbh for understory as pruning height
    CBH_post_modified<-(2*CBD_ratio)
    
    #modify understory CBH if it doesnt exist
    US_df$CBH..0.1m.[is.na(US_df$CBH..0.1m.)]<-0
    
    if (sum(US_df$CBH..0.1m. == 0) > 0) {
      US_df$CBH..0.1m. <- ifelse(US_df$CBH..0.1m. == "Layer 4 (<1.3m)", 0,
                                 ifelse(US_df$CBH..0.1m. == "Layer 3 (2.5-7.49)", 0.5,
                                        ifelse(US_df$CBH..0.1m. == "Layer 2 (7.5-12.49)", 1, 1.5)))
    }
    
    #mean CBH per plot for understory pre-treatment  
    mean_cbh<- US_df %>%
      filter(Layer != "Layer 4 (<1.3m)")  %>%
      summarise(mean_CBH = weighted.mean(CBH..0.1m., w = `X..of.Trees`, na.rm=TRUE)) %>%
      pull(mean_CBH)
    
    mean_cbh_us<- US_df %>%
      filter(Layer != "Layer 4 (<1.3m)")  %>%
      summarise(mean_CBH = weighted.mean(CBH..0.1m., w = `X..of.Trees`, na.rm=TRUE)) %>%
      pull(mean_CBH)
    
    #mean dbh per plot since oversory is non-existant
    DBH<-0
    #centroid calculation for stands without overstory
    table_folder<- paste0(st,"_tables")
    cuttingSpecs <- read_cutting_specs_for_firemodel(paste0(path,s_s_prefix,table_folder,"/cuttingSpecs_",st,".csv"))
    OS_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/OS_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
    US_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
    US_Ht<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_Ht_CBH.csv"), row.names = NULL, stringsAsFactors = FALSE)
    if (!("Layer" %in% names(US_SPH)) && "DBH.Class" %in% names(US_SPH)) {
      US_SPH <- US_SPH |> dplyr::rename(Layer = DBH.Class)
    }
    if (!("Layer" %in% names(US_Ht)) && "DBH.Class" %in% names(US_Ht)) {
      US_Ht <- US_Ht |> dplyr::rename(Layer = DBH.Class)
    }
    #filter out total and Dead column, and remove total layer and layer 4 for crown area calculation
    OS_SPH<-OS_SPH %>%
      filter(DBH.Class != "Total") 
    # Check if 'Total' column exists and remove it
    
    if("Total" %in% names(OS_SPH)) {
      OS_SPH <- OS_SPH %>% dplyr::select(-Total)
    }
    
    US_SPH <- US_SPH %>%
      filter(Layer != "Layer 4 (<1.3m)")
    
    US_Ht<- US_Ht %>%
      filter(Layer != "Layer 4 (<1.3m)")
    
    # Check if 'Dead' column exists and remove it
    if("Dead" %in% names(US_SPH)) {
      US_SPH <- US_SPH %>% dplyr::select(-Dead)
    }
    
    # Check if 'Total' column exists and remove it
    if("Total" %in% names(US_SPH)) {
      US_SPH <- US_SPH %>% dplyr::select(-Total)
    }
    ## Identify columns that start with "NA"
    na_columns <- grep("^NA", names(US_SPH), value = TRUE)
    
    # Check if there are any such columns and remove them
    if(length(na_columns) > 0) {
      US_SPH <- US_SPH %>% dplyr::select(-all_of(na_columns))
    }
    
    
    #add a dbh column based on midpoint of layers
    OS_SPH$DBH<- ifelse(OS_SPH$DBH.Class == "12.5 - 17.5",15,
                        ifelse(OS_SPH$DBH.Class == "17.5 - 22.5", 20,
                               ifelse(OS_SPH$DBH.Class == "22.5 - 27.5", 25,
                                      ifelse(OS_SPH$DBH.Class == "27.5 - 35",31.25,
                                             ifelse(OS_SPH$DBH.Class == "35 - 45",40,
                                                    55)))))
    US_SPH$DBH<- ifelse(US_SPH$Layer == "Layer 3 (2.5-7.49)",5,
                        ifelse(US_SPH$Layer == "Layer 2 (7.5-12.49)", 10,15))
    
    US_Ht$DBH<- ifelse(US_Ht$Layer == "Layer 3 (2.5-7.49)",5,
                       ifelse(US_Ht$Layer == "Layer 2 (7.5-12.49)", 10,15))                                  
    dbh_vec<-as.vector(US_Ht$DBH)
    #remove old dbh class column
    US_SPH<-US_SPH %>%
      dplyr::select(-Layer)
    OS_SPH<-OS_SPH %>%
      dplyr::select(-DBH.Class)
    US_Ht<- US_Ht %>%
      dplyr::select(-Layer)
    
    #merge ht and cbh columns to get crown length
    # Extracting unique species codes from the column names
    species_codes <- unique(sub("\\..*", "", names(US_Ht)[!grepl("DBH", names(US_Ht))]))
    
    # Loop over each species code to calculate canopy centroids
    for (species in species_codes) {
      ht_col <- paste(species, "Ht", sep = ".")
      cbh_col <- paste(species, "CBH", sep = ".")
      
      # Check if both necessary columns exist in the dataframe
      if (ht_col %in% names(US_Ht) && cbh_col %in% names(US_Ht)) {
        # Calculate the crown length and add it as a new column
        US_Ht[[paste(species, "CL", sep = ".")]] <- US_Ht[[ht_col]] - US_Ht[[cbh_col]]
        # Calculate the crown length and add it as a new column
        US_Ht[[paste(species, "Cen", sep = ".")]] <- US_Ht[[cbh_col]] + ((US_Ht[[ht_col]] - US_Ht[[cbh_col]])/2)
      }
    }
    #extract centroid columns
    cent_cols <- grep("Cen$", names(US_Ht), value = TRUE)
    # Check if there are any such columns and create a new data frame
    if(length(cent_cols) > 0) {
      # Select columns ending with 'Cen'
      US_Cent <- US_Ht %>% dplyr::select(all_of(cent_cols))
      
      # Remove the '.Cen' suffix from column names
      names(US_Cent) <- sub("\\.Cen$", "", names(US_Cent))
    }
    US_Cent$DBH<-dbh_vec
    
    #Merge data frames
    US_SPH_melt <- reshape2::melt(US_SPH, id = "DBH")
    names(US_SPH_melt)[names(US_SPH_melt) == "variable"] <- "Species"
    names(US_SPH_melt)[names(US_SPH_melt) == "value"] <- "SPH"
    SPH_merge<- US_SPH_melt
    
    US_Cent_melt<- reshape2::melt(US_Cent, id = "DBH")
    names(US_Cent_melt)[names(US_Cent_melt) == "variable"] <- "Species"
    names(US_Cent_melt)[names(US_Cent_melt) == "value"] <- "Cen"
    #remove columns where centroid is zero because no trees
    US_Cent_melt<-US_Cent_melt %>% filter(Cen > 0)
    #modify cutting specs
    cuttingSpecs <- subset(cuttingSpecs, select = -c(Stand.Layer))
    cuttingSpecs$DBH <- dbh_class_midpoint(cuttingSpecs$DBH.Class)
    #remove old dbh class column
    cuttingSpecs<-cuttingSpecs %>%
      dplyr::select(-DBH.Class)
    
    cuttingSpecs_melt <- reshape2::melt(cuttingSpecs, id = "DBH")
    names(cuttingSpecs_melt)[names(cuttingSpecs_melt) == "variable"] <- "Species"
    names(cuttingSpecs_melt)[names(cuttingSpecs_melt) == "value"] <- "Cutting_Spec"
    ##merge with cutting spec
    SPH_CUT <- merge(SPH_merge, cuttingSpecs_melt,  by =  c("Species", "DBH"), all.x = TRUE)
    
    #CALCULATE CENTROID CUT
    SPH_centroid_cut<- merge(SPH_CUT, US_Cent_melt,  by =  c("Species", "DBH"), all.x = TRUE)
    SPH_centroid_cut_US <- na.omit(SPH_centroid_cut)
    
    ## Calculating cut and leave
    Centroid_Results_US<-SPH_centroid_cut_US %>% mutate(SPH_remain = round(SPH*(100-Cutting_Spec)/100, digits = 0))
    
    #calculate average centroid height remaining
    #remove SPH remain with zero trees
    Centroid_Results_US<- Centroid_Results_US %>% filter(SPH_remain > 0)
    Centroid_US_post<- Centroid_Results_US %>%
      summarise(mean_Cent= weighted.mean(Cen, SPH_remain)) %>%
      pull(mean_Cent)
    
    Centroid_US_pre<- Centroid_Results_US %>%
      summarise(mean_Cent= weighted.mean(Cen, SPH)) %>%
      pull(mean_Cent)
    
    #
    Centroid_US_modified_post<-Centroid_US_post
    Centroid_US_modified_pre<-Centroid_US_pre
    
  } else{
    
    #mean height of all overstory trees weighted by number of trees
    mean_Height<- OS_df %>%
      filter(is.na(Dead.)) %>%
      summarise(mean_weighted_height = mean(Total.Height..m., na.rm=TRUE)) %>%
      pull(mean_weighted_height)
    
    #mean cbh of all overstory trees weighted by number of trees for pre or post treatment
    mean_cbh<- OS_df %>%
      filter(is.na(Dead.)) %>%
      summarise(mean_CBH = mean(CBH..0.1m.,na.rm=TRUE)) %>%
      pull(mean_CBH)
    
    
    #mean CBH of understory trees for pre-treatment
    mean_cbh_us<- US_df %>%
      filter(Layer != "Layer 4 (<1.3m)")  %>%
      summarise(mean_CBH = mean(CBH..0.1m., na.rm=TRUE)) %>%
      pull(mean_CBH)
    
    #mean dbh per plot  
    DBH<- OS_df %>%
      filter(is.na(Dead.)) %>%
      summarise(dbh = mean(DBH, na.rm=TRUE)) %>%
      pull(dbh)
    
    #calculate modified CBH to account for crown area change and CBD change post treatment
    table_folder<- paste0(st,"_tables")
    cuttingSpecs <- read_cutting_specs_for_firemodel(paste0(path,s_s_prefix,table_folder,"/cuttingSpecs_",st,".csv"))
    OS_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/OS_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
    US_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
    US_Ht<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_Ht_CBH.csv"), row.names = NULL, stringsAsFactors = FALSE)
    if (!("Layer" %in% names(US_SPH)) && "DBH.Class" %in% names(US_SPH)) {
      US_SPH <- US_SPH |> dplyr::rename(Layer = DBH.Class)
    }
    if (!("Layer" %in% names(US_Ht)) && "DBH.Class" %in% names(US_Ht)) {
      US_Ht <- US_Ht |> dplyr::rename(Layer = DBH.Class)
    }
    #filter out total and Dead column, and remove total layer and layer 4 for crown area calculation
    OS_SPH<-OS_SPH %>%
      filter(DBH.Class != "Total") 
    # Check if 'Total' column exists and remove it
    if("Total" %in% names(OS_SPH)) {
      OS_SPH <- OS_SPH %>% dplyr::select(-Total)
    }
    
    # Check if 'Dead' column exists and remove it
    if("Dead" %in% names(US_SPH)) {
      US_SPH <- US_SPH %>% dplyr::select(-Dead)
    }
    
    # Check if 'Total' column exists and remove it
    if("Total" %in% names(US_SPH)) {
      US_SPH <- US_SPH %>% dplyr::select(-Total)
    }
    ## Identify columns that start with "NA"
    na_columns <- grep("^NA", names(US_SPH), value = TRUE)
    
    # Check if there are any such columns and remove them
    if(length(na_columns) > 0) {
      US_SPH <- US_SPH %>% dplyr::select(-all_of(na_columns))
    }
    
    #add a dbh column based on midpoint of layers
    OS_SPH$DBH<- ifelse(OS_SPH$DBH.Class == "12.5 - 17.5",15,
                        ifelse(OS_SPH$DBH.Class == "17.5 - 22.5", 20,
                               ifelse(OS_SPH$DBH.Class == "22.5 - 27.5", 25,
                                      ifelse(OS_SPH$DBH.Class == "27.5 - 35",31.25,
                                             ifelse(OS_SPH$DBH.Class == "35 - 45",40,
                                                    55)))))
    US_SPH$DBH<- ifelse(US_SPH$Layer == "Layer 4 (<1.3m)",1,
                        ifelse(US_SPH$Layer == "Layer 3 (2.5-7.49)",5,
                               ifelse(US_SPH$Layer == "Layer 2 (7.5-12.49)", 10,15)))
    
    US_Ht$DBH<- ifelse(US_Ht$Layer == "Layer 4 (<1.3m)",1,
                       ifelse(US_Ht$Layer == "Layer 3 (2.5-7.49)",5,
                              ifelse(US_Ht$Layer == "Layer 2 (7.5-12.49)", 10,15)))                                  
    dbh_vec<-as.vector(US_Ht$DBH)
    
    #remove old dbh class column
    US_SPH<-US_SPH %>%
      dplyr::select(-Layer)
    OS_SPH<-OS_SPH %>%
      dplyr::select(-DBH.Class)
    US_Ht<- US_Ht %>%
      dplyr::select(-Layer)
    
    #Calculate Centroid for each individual tree:
    plot_expansion<-US_df$Plot.Multiplier[1]
    US_df$Centroid<-US_df$CBH..0.1m.+ ((US_df$Height..0.1m.- US_df$CBH..0.1m.)/2)
    US_df$TPH<- US_df$X..of.Trees*plot_expansion
    
    #Summarise by dbh layer
    US_Centroid<- US_df %>% dplyr::select(Layer,SPP,Height..0.1m.,CBH..0.1m.,X..of.Trees,TPH,Centroid) %>%
      dplyr::group_by(Layer,SPP) %>%
      dplyr::summarise(
        Height=weighted.mean(Height..0.1m.,w=X..of.Trees),
        CBH=weighted.mean(CBH..0.1m.,w=X..of.Trees),
        Centroid=weighted.mean(Centroid,w=X..of.Trees),
        TPH=sum(TPH))
    US_Centroid$DBH<- ifelse(US_Centroid$Layer == "Layer 4 (<1.3m)",1,
                             ifelse(US_Centroid$Layer == "Layer 3 (2.5-7.49)",5,
                                    ifelse(US_Centroid$Layer == "Layer 2 (7.5-12.49)", 10,15)))  
    
    #Merge data frames
    US_SPH_melt <- reshape2::melt(US_SPH, id = "DBH")
    names(US_SPH_melt)[names(US_SPH_melt) == "variable"] <- "Species"
    names(US_SPH_melt)[names(US_SPH_melt) == "value"] <- "SPH"
    OS_SPH_melt <- reshape2::melt(OS_SPH, id = "DBH")
    names(OS_SPH_melt)[names(OS_SPH_melt) == "variable"] <- "Species"
    names(OS_SPH_melt)[names(OS_SPH_melt) == "value"] <- "SPH"
    SPH_merge<- rbind(OS_SPH_melt,US_SPH_melt)
    
    #remove columns where centroid is zero because no trees and remove columns where CBH is too far from over fuelbed to be considered part of surface fire activity: then use that to calculate understory centroid
    FBDepth_cm<-fuels_df %>%
      summarise(mean_FB = mean(Average.Fuel.Bed.Depth..0.1cm., na.rm=TRUE)) %>%
      pull(mean_FB)
    FBDepth_M<-FBDepth_cm/100
    MaxSurfFireHeight<-FBDepth_M*4 #multiplied by 4 to include flame height 2 times fuel height + 2 times for heated air
    
    US_Cent_melt<-US_Centroid %>% filter(Centroid > 0,CBH <= MaxSurfFireHeight) %>% dplyr::rename(Species=SPP)
    
    #modify cutting specs
    cuttingSpecs <- subset(cuttingSpecs, select = -c(Stand.Layer))
    cuttingSpecs$DBH <- dbh_class_midpoint(cuttingSpecs$DBH.Class)
    #remove old dbh class column
    cuttingSpecs<-cuttingSpecs %>%
      dplyr::select(-DBH.Class)
    
    cuttingSpecs_melt <- reshape2::melt(cuttingSpecs, id = "DBH")
    names(cuttingSpecs_melt)[names(cuttingSpecs_melt) == "variable"] <- "Species"
    names(cuttingSpecs_melt)[names(cuttingSpecs_melt) == "value"] <- "Cutting_Spec"
    
    ##merge with cutting spec
    SPH_CUT <- merge(SPH_merge, cuttingSpecs_melt,  by =  c("Species", "DBH"), all.x = TRUE)
    SPH_CUT$Cutting_Spec[is.na(SPH_CUT$Cutting_Spec)]<-0
    
    #CALCULATE CENTROID CUT
    SPH_centroid_cut<- merge(SPH_CUT, US_Cent_melt,  by =  c("Species", "DBH"), all.x = TRUE)
    SPH_centroid_cut_US <- na.omit(SPH_centroid_cut)
    
    ## Calculating cut and leave
    SPH_Results_PT <- SPH_CUT %>% mutate(SPH_remove = round(SPH*Cutting_Spec/100, digits = 0)) %>%
      mutate(SPH_leave = round(SPH*(100-Cutting_Spec)/100, digits = 0))
    
    Centroid_Results_US<-SPH_centroid_cut_US %>% mutate(SPH_remain = round(TPH*(100-Cutting_Spec)/100, digits = 0)) %>% filter(Species %in% coniferList)
    
    #Calculate Average Centroid height, pre and post treatment
    Centroid_US_pre<- Centroid_Results_US %>%
      summarise(mean_Cent= weighted.mean(Centroid, TPH)) %>%
      pull(mean_Cent)
    if(is.na(Centroid_US_pre)){
      Centroid_US_pre<-FBDepth_cm/100
    }
    
    Centroid_US_post<- Centroid_Results_US %>%
      filter(SPH_remain > 0) %>%
      summarise(mean_Cent= weighted.mean(Centroid, SPH_remain)) %>%
      pull(mean_Cent)
    
    if(is.na(Centroid_US_post)){
      Centroid_US_post<-FBDepth_cm/100
    }
    
    #------------------------------------------------------------------------------------------------
    
    #remove Dead potential since no crown area
    SPH_Results_PT <- SPH_Results_PT %>%
      filter(Species != "DP" | Species != "DU")
    
    ##calculate crown area for each species per row
    for(x in 1:nrow(SPH_Results_PT)){
      sp<-as.character(SPH_Results_PT$Species[x])
      SPH_Results_PT$Crown_Area_Tree[x]<-crown_area(sp,SPH_Results_PT$DBH[x])                 
    }
    SPH_Results_PT<-SPH_Results_PT %>%
      filter(SPH !=0)
    
    #calculate crown area in m^2 per hectare by species and DBH after cutting
    SPH_Results_PT$Crown_Area_Ha<- SPH_Results_PT$Crown_Area_Tree*SPH_Results_PT$SPH_leave
    
    #crown area of trees total pre-treatment
    SPH_Results_PT$Crown_Area_Pre_Treatment<- SPH_Results_PT$Crown_Area_Tree*SPH_Results_PT$SPH
    
    #calculate total crown area coverage for pre and post treatment
    Total_CA<- sum(SPH_Results_PT$Crown_Area_Ha, na.rm = TRUE)
    Total_CA_pre<-sum(SPH_Results_PT$Crown_Area_Pre_Treatment, na.rm = TRUE)
    
    #proportion of remaining crown coverage of understory trees
    #calculate crown area occupied by understory trees after cutting
    US_CA <- SPH_Results_PT %>%
      filter(DBH <= 15) %>%
      summarise(US_CROWNAREA = sum(Crown_Area_Ha, na.rm = TRUE)) %>%
      pull(US_CROWNAREA)
    #crown area ratio of understory to total crown after cutting
    US_CA_ratio<- ifelse(Total_CA > 0, US_CA/Total_CA, NA_real_)
    
    #calculate crown area occupied by understory trees before cutting
    US_CA_pre <- SPH_Results_PT %>%
      filter(DBH <= 15) %>%
      summarise(US_CrownA_pre = sum(Crown_Area_Pre_Treatment, na.rm = TRUE)) %>%
      pull(US_CrownA_pre)
    US_CA_ratio_pre<- ifelse(Total_CA_pre > 0, US_CA_pre/Total_CA_pre, NA_real_)
    
    #modify understory centroid by the proportion of area occupied by understory crowns for pre and post treatment
    Centroid_US_modified_pre<-Centroid_US_pre*US_CA_ratio_pre
    Centroid_US_modified_post<-Centroid_US_post*US_CA_ratio
    
    
    #proportion of remaining crown coverage of overstory trees
    OS_CA<- SPH_Results_PT %>%
      filter(DBH > 15) %>%
      summarise(US_CROWNAREA = sum(Crown_Area_Ha, na.rm = TRUE)) %>%
      pull(US_CROWNAREA)
    OS_CA_ratio<-ifelse(Total_CA > 0, OS_CA/Total_CA, NA_real_)
    
    #calculate proportional change to canopy base height as a function of the decline in canopy coverage from the removal of overstory trees
    CA_OS_original<- SPH_Results_PT %>%
      filter(DBH > 15) %>%
      summarise(OS_CA_O = sum(Crown_Area_Pre_Treatment, na.rm = TRUE)) %>%
      pull(OS_CA_O)
    OS_CA_pre<-ifelse(Total_CA_pre > 0, CA_OS_original/Total_CA_pre, NA_real_)
    
    #calculate the change in canopy coverage from pre treatment over story to post-treatment over story, ie what reduction in canopy coverage did we induce for overstory alone
    OS_CA_change_ratio<- ifelse(CA_OS_original > 0, OS_CA/CA_OS_original, NA_real_)
    
    #Use these ratios to modify canopy base height with the contribution of the remaining understory and overstory trees
    #post treatment CBH take average of over story CBH + average weighted pruned CBH
    
    #Modified CBH: set post treatment understory CBH based on pruning; either 2 or 3
    #Equation
    
    #Modified Post-Treatment CBH= CA Weight Understory*CBH Understory Pruned + (CA Weight Overstory*(CBH Overstory*(CBD-pre/CBD-post)))
    
    #Check if cutting is happening, because if not then set ratio to  1 since there should be no change to CBD
    cut_specs_vec <- cuttingSpecs %>%
      dplyr::select(-DBH) %>%  # Remove the DBH column
      unlist() %>%      # Convert to a vector
      as.numeric()   
    if(sum(cut_specs_vec) <= 0){
      CBD_ratio<-1
      cbd_post<-cbd_pre
    }else{
      CBD_ratio<-CBD_ratio
    }
    
    CBH_post_modified<- US_CA_ratio*(prune) + (OS_CA_ratio*(mean_cbh_Post_FC*CBD_ratio))
  }
  
  #
  Duff<-fuels_df %>%
    summarise(mean_duff = mean(Duff.Depth..cm., na.rm=TRUE)) %>%
    pull(mean_duff)
  
  FB_D<-fuels_df %>%
    summarise(mean_FB = mean(Average.Fuel.Bed.Depth..0.1cm., na.rm=TRUE)) %>%
    pull(mean_FB)
  
  slope<-fuels_df %>%
    summarise(mean_slope = mean(Slope..)) %>%
    pull(mean_slope)
  #
  hr1<-fuels_df %>%
    summarise(fuel = mean(Avg.1.hr.fuels...0.6.cm...kg.m2., na.rm=TRUE)) %>%
    pull(fuel)
  # 
  hr10<-fuels_df %>%
    summarise(fuel = mean(Avg.10.hr.fuels..0.6.2.5cm...kg.m2., na.rm=TRUE)) %>%
    pull(fuel)
  #
  hr100<-fuels_df %>%
    summarise(fuel = mean(Avg.100.hr.fuels..2.6.7.5.cm...kg.m2., na.rm=TRUE)) %>%
    pull(fuel)
  
  #1000 hour fuels  
  if ("X1000.hr.fuels...7.6.cm.kg.m2." %in% names(fuels_df)) {
    hr1000<-fuels_df %>%
      summarise(fuel = mean(X1000.hr.fuels...7.6.cm.kg.m2., na.rm=TRUE)) %>%
      pull(fuel)
  }else{
    hr1000<-0
  }
  
  
  #LWD Kg per meter loads or 100 hours
  if ("LWD.fuels..7.0.20.0cm...kg.m2." %in% names(fuels_df)) {
    LWD <- fuels_df %>%
      summarise(fuel = mean(LWD.fuels..7.0.20.0cm...kg.m2., na.rm = TRUE)) %>%
      pull(fuel)
  } else {
    LWD <- 0
  }
  #Coarse Woody Debris pieces (we call LWD) keep it as 5m pieces!
  CWD<-fuels_df %>%
    summarise(lwd = mean(LWD.pieces, na.rm=TRUE)) %>%
    pull(lwd)
  #
  #Coarse Woody Debris kg> 20cm
  CWD_kg<-fuels_df %>%
    summarise(cwd = mean(CWD.fuels...20.0cm...kg.m2., na.rm=TRUE)) %>%
    pull(cwd)
  #
  
  hr1000<-LWD+CWD_kg
  
  aspect<- EX_df %>%
    summarise(mean_aspect= mean(Avg.Azimuth,na.rm=TRUE)) %>%
    pull(mean_aspect)
  
  #Post treatment fuel loads: SET BY USER ABOVE!
  #Load Fuels and calculate ratio of total fuels to each class to change new fuels using the same ratio
  new_tot_fuel<-fuels[j]
  tot_fuel_pretreat<-hr1+hr10+hr100
  hr1_ratio<-hr1/tot_fuel_pretreat
  hr10_ratio<-hr10/tot_fuel_pretreat
  hr100_ratio<-hr100/tot_fuel_pretreat
  
  #set new fuels after treatment
  hr1_PT<-new_tot_fuel*hr1_ratio
  hr10_PT<-new_tot_fuel*hr10_ratio
  hr100_PT<-new_tot_fuel*hr100_ratio
  hr1000_PT<-hr1000s[j]
  lit_PT<-hr1_PT
  
  
  #CFL CBD
  cfl_pre<- as.numeric(structure_data$Pre_CFL)
  cfl_post<-as.numeric(structure_data$Post_CFL)
  
  #BA, VOl
  BA_pre<- as.numeric(structure_data$Pre_BA)
  BA_post<- as.numeric(structure_data$Post_BA)
  Vol_pre<- as.numeric(structure_data$Pre_VOL)
  Vol_post<-as.numeric(structure_data$Post_VOL)
  
  #crown closure
  CC_fuels_df <- fuels_df %>%
    summarise(CC = mean(Crown.Closure.., na.rm=TRUE)) %>%
    pull(CC)
  pre_CC<-as.numeric(structure_data$Pre_CC)
  pre_CC<- ifelse(is.na(CC_fuels_df),pre_CC,mean(pre_CC,CC_fuels_df))
  post_CC<-as.numeric(structure_data$Post_CC)
  
  #grass,shubs,herb loads
  if ("Avg.Grass.Loading..kg.m2." %in% names(fuels_df)) {
    grass<- fuels_df %>%
      summarise(grass = mean(Avg.Grass.Loading..kg.m2., na.rm=TRUE)) %>%
      pull(grass)
  } else {
    grass <- 0
  }
  
  if ("Avg.Shrub.Loading..kg.m2." %in% names(fuels_df)) {
    shrub<- fuels_df %>%
      summarise(shrub = mean(Avg.Shrub.Loading..kg.m2., na.rm=TRUE)) %>%
      pull(shrub)
  } else {
    shrub <- 0
  }
  if(is.na(shrub)){shrub<-0}
  
  
  if ("Avg.Herb.Loading..kg.m2." %in% names(fuels_df)) {
    herb<- fuels_df %>%
      summarise(herb = mean(Avg.Herb.Loading..kg.m2., na.rm=TRUE)) %>%
      pull(herb)}else{
        herb <- 0
      }
  if(is.na(herb)){herb<-0}
  
  #if CBH is lower then pruning height for post treatment then set it to pruned cbh 
  if(pruneflag && valid_number(mean_cbh) && prune > mean_cbh){
    mean_cbh_prune<-prune
  }else{
    mean_cbh_prune<-mean_cbh
  }
  if(!valid_number(mean_cbh_prune) && pruneflag){
    mean_cbh_prune<-prune
  }
  #
  if(!valid_number(CBH_post_modified)){
    CBH_post_modified<-mean_cbh_Post_FC
  }
  if(pruneflag && (!valid_number(CBH_post_modified) || prune > CBH_post_modified)){
    CBH_post_modified<-prune
  }else{
    CBH_post_modified<-CBH_post_modified
  }
  
  #trees per ha
  TPH_pre<- as.numeric(structure_data$Pre_TPH)
  TPH_post<- as.numeric(structure_data$Post_TPH)
  
  #fuel strata gap from field
  FSG_field<-fuels_df %>%
    summarise(fsg = mean(Average.Fuel.Strata.Gap, na.rm=TRUE)) %>%
    pull(fsg)
  
  #generate DFs for pre and post treatment
  Prestrata_df<- data.frame(
    Stratum=st,
    Height_US=weighted_mean_H_US,
    US_CBH=mean_cbh_us,
    #cbh here adjusts for fuelcalcs bulk density ratio: output directly from fuelcalc, cbh of lower ladders
    Fuelcalc_CBH=mean_cbh_PT_FC,
    Height=mean_Height,
    DBH=DBH,
    TPH=TPH_pre,
    BA=BA_pre,
    Vol=Vol_pre,
    Duff_depth=Duff,
    FB_depth=FB_D,
    Lit_kg=hr1,
    hr_1_kg=hr1,
    hr_10_kg=hr10,
    hr_100_kg=hr100,
    hr_1000_kg=hr1000,
    LWD_kg=LWD,
    CWD_kg=CWD_kg,
    CWD_Pieces_5m_ha=CWD,
    CFL=cfl_pre,
    CBD=cbd_pre,
    CC=pre_CC,
    Slope=slope,
    Aspect=aspect,
    #cbh here adjusts for fuelcalcs bulk density ratio when using the Sando-Wick method
    OS_CBH=mean_cbh,
    FSG_Field=FSG_field,
    Modified_CBH=NA,
    US_Centroid=Centroid_US_modified_pre,
    Grass_Loading=grass,
    Shrub_Loading=shrub,
    Herb_Loading=herb,
    Fueltype_US_FC=structure_data$Pre_FT,
    Fueltype_CAD=FTCAD_pt
  )
  colnames(Prestrata_df)<- colnames(PreTreat_df)
  
  Poststrata_df<- data.frame(
    Stratum=st,
    #modify this based on tree removal, ie if all under story is gone you can leave height at zero otherwise can use the mean height remaining
    Height_US=0,
    US_CBH=NA,
    #cbh here adjusts for fuelcalcs bulk density ratio
    Fuelcalc_CBH=mean_cbh_Post_FC,
    Height=mean_Height,
    DBH=DBH,
    TPH=TPH_post,
    BA=BA_post,
    Vol=Vol_post,
    Duff_depth=Duff,
    FB_depth=FB_D,
    Lit_kg=lit_PT,
    hr_1_kg=hr1_PT,
    hr_10_kg=hr10_PT,
    hr_100_kg=hr100_PT,
    hr_1000_kg=hr1000_PT,
    LWD_kg=hr1000_PT,
    CWD_kg=NA,
    CWD_Pieces_5m_ha=CWD,
    CFL=cfl_post,
    CBD=cbd_post,
    CC=post_CC,
    Slope=slope,
    Aspect=aspect,
    #cbh here is the mean measured CBH from the field for just over story: depending on whether overstory exists or not
    OS_CBH=mean_cbh_prune,
    FSG_Field=NA,
    #cbh here adjusts for fuelcalcs bulk density ratio when using the Sando-Wick method and the crown area reductions
    Modified_CBH=CBH_post_modified,
    US_Centroid=Centroid_US_modified_post,
    Grass_Loading=grass,
    Shrub_Loading=shrub,
    Herb_Loading=herb,
    Fueltype_US_FC=structure_data$Post_FT,
    Fueltype_CAD=FTCAD_USER
  )
  colnames(Poststrata_df)<- colnames(PostTreat_df)
  #merge
  PreTreat_df<-rbind(PreTreat_df,Prestrata_df)
  PostTreat_df<-rbind(PostTreat_df,Poststrata_df)
}

#remove NA row
PreTreat_df<-PreTreat_df[-1,]
PostTreat_df<-PostTreat_df[-1,]

#calculate FSG as CBH from fuelcalc - understory centroid
PreTreat_df$FSG<-PreTreat_df$Fuelcalc_CBH-PreTreat_df$US_Centroid
PreTreat_df$FSG_Mod<-rep(NA,nrow(PreTreat_df))

#calculate FSG as CBH from fuelcalc - fb depth
#PreTreat_df$FSG<-PreTreat_df$Fuelcalc_CBH-PreTreat_df$FB_depth

# Setting values in the 'FSG' column that are less than zero to zero
PreTreat_df$FSG[PreTreat_df$FSG < 0] <- 0
PreTreat_df$Fine_Fuel_kg<- PreTreat_df$hr_1_kg+PreTreat_df$hr_10_kg+PreTreat_df$hr_100_kg


#FSG as CBH - centroid method
PostTreat_df$FSG<-PostTreat_df$Fuelcalc_CBH-PostTreat_df$US_Centroid
#FSG as Modified CBH (by crown area)- understory centroid
PostTreat_df$FSG_Mod<-PostTreat_df$Modified_CBH-PostTreat_df$US_Centroid

#PostTreat_df$FSG<-PostTreat_df$Fuelcalc_CBH-PostTreat_df$FB_depth
PostTreat_df$FSG[PostTreat_df$FSG < 0] <- 0
PostTreat_df$Fine_Fuel_kg<- PostTreat_df$hr_1_kg+PostTreat_df$hr_10_kg+PostTreat_df$hr_100_kg

#Calculate Duff Load: 10 tons for every 0.1 inches
PreTreat_df$Duff_Depth_in<- PreTreat_df$Duff_depth*0.3937
PreTreat_df$Duff_Load_t_a<-PreTreat_df$Duff_Depth_in*10*1
PostTreat_df$Duff_Depth_in<- PostTreat_df$Duff_depth*0.3937
PostTreat_df$Duff_Load_t_a<-PostTreat_df$Duff_Depth_in*10*1

#Add prune column
PreTreat_df$Prune<- prune_vals
PostTreat_df$Prune<- prune_vals

#Add treatment status column
PreTreat_df$Period<- rep("Pre",nrow(PreTreat_df))
PostTreat_df$Period<-rep("Post",nrow(PreTreat_df))

#Merge
AllTreatmentStr<-rbind(PreTreat_df,PostTreat_df)

#Export
write.csv(PreTreat_df, paste0(path,Fuel_prefix,"Pre_Treatment_Structure_Data.csv"),row.names = FALSE)
write.csv(PostTreat_df, paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"),row.names = FALSE)
write.csv(AllTreatmentStr, paste0(path,Fuel_prefix,"AllPeriods_StructureData.csv"),row.names = FALSE)

#---------------------------------------------------------------------------
#Read in Input Data and process:----------------------------------------------
strata_data<-read.csv(file.path(root,"projects",project,"data","raw",Fuel_prefix,"AllPeriods_StructureData.csv"))
daily_weather<- read.csv(file.path(root,"projects",project,"data","raw",in_weather,"WeatherLists","allstations_90th_FWList_dates_summer.csv"))
wthrfilename<-paste0(WeatherName,"_Hourly_Weather.csv")
hourly_weather<-read.csv(file.path(root,"projects",project,"data","raw",in_weather,"raw",wthrfilename))

#Process
strata_data$Forest_Type<-Forest_Type
strata_data$SurfFuel<-SurfFuel

#----------------------------------------------------------------

#Set Up iterative code:------------------------------------------

#Weather data: if you want you can sample
weather_list <- daily_weather
weather_list$Date <- as.Date(weather_list$DATE, format = "%Y-%m-%d")
set.seed(123)
if(NumWeathers < nrow(daily_weather)){
weather_list <- daily_weather %>% slice_sample(n = NumWeathers)
}else{
 weather_list <- daily_weather
}

# Initialize results
results_list <- list()

# Define the number of treatments and weather conditions
strata <- unique(strata_data$Stratum)

#Setup progress bar
total_iters <- nrow(weather_list) * length(strata)*2
pb <- progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total  = total_iters,
  clear  = FALSE,   # keep the bar on screen after completion
  width  = 60
)

#initialize
iter <- 0
write_progress(
  status = "running",
  weather_total = nrow(weather_list),
  total_iters = total_iters,
  message = "Entering main Step 3 model loop",
  force_console = TRUE
)

#Run Fire Behaviour Models----------------------------------------------------------------------------------
suppressWarnings({

    #Iterate over period: Pre and Post
for(P in unique(strata_data$Period)){
  input_data<- strata_data %>% filter(Period == P)
  
  # Iterate over weather conditions
  for (i in 1:nrow(weather_list)) {
    
    # Extract weather data for this iteration
    date <- as.Date(weather_list$DATE[i], format="%Y-%m-%d")
    BUI <- weather_list$BUI[i]
    FFMC <- weather_list$FFMC[i]
    DMC <- weather_list$DMC[i]
    DC <- weather_list$DC[i]
    ISI <- weather_list$ISI[i]
    WS <- weather_list$WS[i]
    WD <- weather_list$WD[i]
    TEMP <- weather_list$TEMP[i]
    RH <- weather_list$RH[i]
    Lat <- weather_list$LAT[i]
    Lon <- weather_list$LONG[i]
    J_date <- as.numeric(strftime(date, format = "%j"))
    
    #Adjust windspeed by Gusting if need be
    if(WindGustMod){
      WS_Gust<-Calculate_Gust(WS, units="kmh")
      WS<-WS_Gust$OneMinuteMax
    }
    
    #Iterate over stratas
    for(j in 1:length(strata)){
      STRATA<-strata[j]
      strata_input <- input_data %>% filter(Stratum == strata[j])
      
      #Get Elevation and Foliar Moisture Content
      Elev <- Elevation
      FMC <- FMC_calc(Lat, Lon, ELEV = Elev, DATE = date)
      
      # Extract fuel information and change to Megagrams per hectare
      #check change in fuel loads by ratio and proportionately change fuel load classes for post treatment
      ratio=(strata_input$hr_1_kg + strata_input$hr_10_kg + strata_input$hr_100_kg)/strata_input$Fine_Fuel_kg
      hr1_Mg <- (strata_input$hr_1_kg)/ratio * 10
      hr10_Mg <- (strata_input$hr_10_kg)/ratio * 10
      hr100_Mg <- (strata_input$hr_100_kg)/ratio * 10
      litter_Mg <- (strata_input$hr_1_kg)/ratio * 10
      FB_depth_cm <- strata_input$FB_depth
      
      ffl_kg <- (hr1_Mg + hr10_Mg + hr100_Mg + litter_Mg)/10 + strata_input$Grass_Loading 
      #+ #strata_input$Herb_Loading #+ strata_input$Shrub_Loading
      
      # Calculate Surface Fuel Consumption (SFC) from 2 ways:
      #Wotton 2007
      sfc_n <- sfc(fueltype = strata_input$Fueltype_CAD, dc = DC, ffmc = FFMC, ffl = ffl_kg, bui = BUI, depth = FB_depth_cm, dmc = DMC, bd = 0)
      
      
      #Canadian fbp
      SFC_FBP<-surface_fuel_consumption(FUELTYPE = strata_input$Fueltype_CAD,
                                        FFMC=FFMC,
                                        BUI=BUI,
                                        PC= GrassCuring, GFL=strata_input$Grass_Loading)
      
      # Calculate Effective Fine Fuel Moisture (EFFM)
      Density<- Density<- ifelse(strata_input$CC< 45,"Light",
                                 ifelse(strata_input$CC> 45| strata_input$CC< 60 ,"Moderate",
                                        ifelse(strata_input$CC> 60 ,"Dense")))

      ffm1_Wotton<-FFMC_sa(strata_input$Forest_Type,Season, ffmc = FFMC,dmc = DMC, density = Density)
      
      
      
      #or
      effm<- ffm(method="anderson",
                 rh=RH,
                 temp=TEMP,
                 month=as.numeric(substr(date,6,7)),
                 hour=16,
                 asp=strata_input$Aspect,
                 slp=strata_input$Slope,
                 bla="b",
                 shade="yes")
      
      #FSG: adjust for pre or post treatment
      if(P == "Pre"){
        FSG<-strata_input$FSG
      }else{
      if(FSG.Mod.Flag[j]){
        FSG<-strata_input$FSG_Mod
      }else{  
        FSG<-strata_input$FSG
      }
      }
      
      # Calculate Growing Season Index (GSI) and Live Fuel Moisture from full dataset
      #calculate indicator value from those 3 values: equations from: Jolly et al.2005 GSI https://www.youtube.com/watch?v=w8Ukio93BMU
      date2<-date
      T_min <- as.numeric(min(hourly_weather %>% filter(date == date2) %>% pull(temperature)))
      photo_per <- daylength(Lat, as.Date(date))
      VPD <- RHtoVPD(RH, TEMP, Pa = 101) * 1000
      i_pp <- ifelse(photo_per > 11, 1, ifelse(photo_per < 10, 0, photo_per - 10))
      i_VPD <- ifelse(VPD > 4100, 0, ifelse(VPD < 900, 1, (VPD * (-1 / 3200) + 1.2813)))
      i_Tmin <- ifelse(T_min > 5, 1, ifelse(T_min < -2, 0, (T_min * (1 / 6) + 0.3333)))
      GSI <- i_Tmin * i_pp * i_VPD
      herb_live <- ifelse(GSI < 0.5, 30, ((440 * GSI) - 190))
      woody_live <- ifelse(GSI < 0.5, 60, ((280 * GSI) - 30))
      
      # Prepare inputs for fire behavior models
      Plot_fuels <- fuelModels[36, 1:16]
      
      # Modify fuel loads D for dynamic then 1hr, 10hr, 100hr, 1000hr, and depth post treatment
      Plot_fuels$fuelModelType <- "D"
      Plot_fuels$loadLitter <- litter_Mg
      Plot_fuels$load1hr <- hr1_Mg
      Plot_fuels$load10hr <- hr10_Mg
      Plot_fuels$load100hr <- hr100_Mg
      Plot_fuels$fuelBedDepth <- FB_depth_cm
      
      if(!is.na(strata_input$Fueltype_US_FC)){
        RothModel<-strata_input$Fueltype_US_FC  
      }else{
        BestUSModel<-find_best_model(Plot_fuels,Density=Density, SurfFuelType=strata_input$SurfFuel, ForestType=strata_input$Forest_Type)
        RothModel<-BestUSModel$Fueltype_US_FC
      }
      
      #account for OG 13
      if(RothModel %in% c(1,2,3,4,5,6,7,8,9,10,11,13)){
        RothModel<-paste0("A",RothModel)
      }
      
      model_roth<-RothModel
      model<-model_roth
      
      #select model back from dataset: USE PRESET MODEL FUELS
      if(model_roth %in% rownames(fuelModels)){
        Plot_fuels<-fuelModels[model_roth,1:16]
      }else{
        print("Checking Custom Models: Model not in original 13 and 40+")
        Plot_fuels<-CustomModels[model_roth,1:17]
        Plot_fuels<-Plot_fuels %>% dplyr::select(-Model)
      }
      
      #Select model back from dataset: USE PRESET MODEL FUELS or dont reload your fuels
      if(CustomFuels[j]){
        Plot_fuels$fuelModelType<-"D"
        Plot_fuels$loadLitter<-litter_Mg
        Plot_fuels$load1hr<-hr1_Mg
        Plot_fuels$load10hr<-hr10_Mg
        Plot_fuels$load100hr<-hr100_Mg
        Plot_fuels$fuelBedDepth<-FB_depth_cm
        Plot_fuels$loadLiveHerb<-(strata_input$Herb_Loading+strata_input$Grass_Loading)* 10
        Plot_fuels$loadLiveWoody<-strata_input$Shrub_Loading* 10
      }
      
      Plot_moisture <- data.frame(litter = effm$fmLitter, hr1 = effm$fm1hr, hr10 = effm$fm10hr, hr100 = effm$fm100hr, live_herb = herb_live, live_woody = woody_live)
      Plot_crown <- data.frame(cbd = strata_input$CBD, fmc = FMC, cbh = strata_input$Fuelcalc_CBH, cfl = strata_input$CFL)
      Crown_Ratio<- (strata_input$Height-strata_input$Fuelcalc_CBH)/(strata_input$Height)
      Plot_stand <- data.frame(slope = strata_input$Slope, ws = WS, wind_d = WD, wind_adj = firebehavioR::waf(FB_depth_cm, strata_input$Height, Crown_Ratio, strata_input$CC))
      
      wind_adj<-firebehavioR::waf(FB_depth_cm, strata_input$Height, Crown_Ratio, strata_input$CC)
      WS_mid<-firebehavioR::waf(FB_depth_cm, strata_input$Height, Crown_Ratio, strata_input$CC)*WS
      #Run Nelson's reaction time model to get surface area burning rate as subsitute for surface fuel consumption
      
      #create input data frames
      fuel<-data.frame(litter=Plot_fuels$loadLitter,
                       hr1=Plot_fuels$load1hr,
                       hr10=Plot_fuels$load10hr,
                       hr100=Plot_fuels$load100hr,
                       depth=FB_depth_cm,CAN_model=strata_input$Fueltype_CAD)
      
      #Get Aspect
      AspectDegrees <- strata_input$Aspect %% 360
      
      breaks <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
      labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")  
      
      AspectCardinal <- cut(AspectDegrees,
                            breaks = breaks,
                            labels = labels,
                            include.lowest = TRUE,
                            right = FALSE)
      
      topography<-data.frame(lat=Lat,lon=Lon,elev=Elev, asp=AspectCardinal,slp=strata_input$Slope)
      
      structure<-data.frame(ht=strata_input$Height,cc=strata_input$CC,cr=Crown_Ratio,dbh=strata_input$DBH,cbh=strata_input$FSG,cbd=strata_input$CBD,cfl=strata_input$CFL,ba= strata_input$BA,sd=strata_input$TPH)
      
      weather<-data.frame(temp=TEMP, rh=RH,t_min=T_min,dc=DC,dmc=DMC,bui=BUI,ffm=effm$fm1hr,ws=WS,wd=WD)
      
      Nelson<-residence_time(
        model,
        fuel,
        topography,
        structure,
        weather,
        date, 
        IntensityType = "Byram", 
        UseModel =FALSE,
        ModelEFFM = FALSE)
      
      
      #get rate of spread from Nelson
      ROS_Nelson<-Nelson$RateOfSpread.m.s*60
      
      #Calculate effective heat of combustion from: Babrauskkas 2006
      H_B<- (16.52-0.057*FMC)*1000
      #or $from Nelson
      H_N<-Nelson$Mean.Heat.Combust
      #or preset standard value
      H_M<-18000
      
      H<-ifelse(HFlag == "Nelson",H_N,
                ifelse(HFlag == "Manual",H_M,H_B))
      
      Plot_sfc <- data.frame(SFC = sfc_n, SFC_FBP=SFC_FBP, BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s)
      Plot_sfc$SFC_Nelson<- Plot_sfc$BurningRate*(Nelson$Residence.Time.s)
      if(Plot_sfc$SFC_Nelson>ffl_kg){
        Plot_sfc$SFC_Nelson<-ffl_kg
      }
      
      # Run the fire behavior models (e.g., Rothermel, CFIS, CFIS 2.0, FBP, CFIM)
      #check if Nelson predicts a fire
      if(Plot_sfc$SFC > ffl_kg | Plot_sfc$SFC_FBP > ffl_kg){
        Plot_sfc<-data.frame(SFC = ffl_kg, SFC_FBP=ffl_kg, BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s, SFC_Nelson=Plot_sfc$SFC_Nelson)
      }
      
      
      #Scott and Reinhardt
      #Calculates Fire Behaviour as Reaction Intensity
      #I=residence time*rate of spread*Reaction Intensity
      #ScottReinhardt<- firebehavioR::rothermel(surfFuel = Plot_fuels, moisture = Plot_moisture, crownFuel = Plot_crown, enviro = Plot_stand, rosMult = 1.7, cfbForm = "w", folMoist = "y")
      CrownType<-ifelse(CrownFireType[j] == "Finney","f",ifelse(CrownFireType[j] == "Wagner","w","sr"))
      #Change CBD for Finney Method (assuming you use Scott and ReinHardt's CBD Calc)
      if(CrownType == "f"){
        Plot_crown$cbd<-Plot_crown$cbd*2
      }
      ScottReinhardt<- rothermel_mod(surfFuel = Plot_fuels, moisture = Plot_moisture, crownFuel = Plot_crown, enviro = Plot_stand, rosMult = 1.7, cfbForm = CrownType, folMoist = "n")
      ScottReinhardt$fireBehavior$Model_type<- "Rothermel"
      ScottReinhardt$fireBehavior$FuelType<-model_roth
      ScottReinhardt$fireBehavior$SFC<-Plot_sfc$SFC
      ScottReinhardt$FuelConsumed_kg_m2<-ScottReinhardt$fireBehavior$`Heat per Unit Area [kJ/m2]`/H
      
      
      # Use the local rothermel_mod output instead of the external Rothermel
      # package. The detailed surface ROS is the closest replacement for the
      # former ros(modeltype = "D", ...) call.
      rothermel_surface_ros <- ScottReinhardt$detailSurface$`Potential ROS [m/min]`
      
      #Check which rate of spread is greater and use that or use mean:
      ROS<-max(c(rothermel_surface_ros, 
                 ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`))
      
      #Extract Final SFC
      sfc_values <- unlist(Plot_sfc[, c("SFC", "SFC_FBP", "SFC_Nelson")])
      sfc_values<-sfc_values[sfc_values !=0]
      less_than_ffl <- sfc_values[sfc_values < ffl_kg]
      
      # Use the max of those values, or ffl_kg if none are less
      SFC <- ifelse(length(less_than_ffl) > 0, max(less_than_ffl), ffl_kg)
      
      #Get Intensity: using manual equations with actual existing fuels-------------------------
      #particle density kg/m^3: Constant from Cruz et al 2006: CFIM
      p_f<-398
      #Char Fraction: averaged from Nelson values
      CharFract<-0.1925
      
      #Packing Ratio from Rothermel or from cfim code
      #Code from CFIM Cruz 2006: change fuel load from Mg/ha to kg/m^2
      PackingRatio<- (SFC)/(strata_input$FB_depth * p_f* (1-CharFract))
      
      #Nelson's equation:
      I_B_NELSON<-0.85*H*(1 - CharFract)*p_f*strata_input$FB_depth*PackingRatio*ROS/60   
      
      #Byram's Intensity Equation      
      I_B_BYRAM<-H*(SFC)*ROS/60
      
      #Get Intensity
      Intensity<-ifelse(IntensityFlag == "Nelson",I_B_NELSON,
                        ifelse(IntensityFlag == "Byram",I_B_BYRAM,ScottReinhardt$fireBehavior$`Fireline Intensity [kW/m]`))
      
      
      #FBP Function:
      FBP_df<-data.frame(
        FuelType=ifelse(strata_input$Fueltype_CAD == "D-1/2","D-1",
                        ifelse(strata_input$Fueltype_CAD == "M-1/2","M-1",
                               ifelse(strata_input$Fueltype_CAD == "M-3/4","M-3",
                                      ifelse(strata_input$Fueltype_CAD == "O-1a/b","O-1b",strata_input$Fueltype_CAD)))),
        LAT=Lat,
        LONG= Lon,
        FFMC= FFMC,
        BUI=BUI,
        WS= WS,
        WD=WD,
        GS= strata_input$Slope,
        Dj= J_date,
        Aspect= strata_input$Aspect,
        CBH= strata_input$FSG,
        ELV= Elev,
        ISI= ISI,
        SD=strata_input$TPH,
        SH=strata_input$Height,
        CFL= strata_input$CFL,
        FMC= FMC
      )
      FBP_df<-cbind(FBP_df,data.frame(cc=GrassCuring))
      
      FBP<-cffdrs::fbp(FBP_df,output = "ALL")
      FBP$FuelType<-strata_input$Fueltype_CAD
      FBP$Model_type<-"FBP"
      FBP$Type<- ifelse(FBP$FD == "C","active",
                        ifelse(FBP$FD == "S", "surface", "passive"))
      
      #Decide on FineFuelMoisture
      if(fuelmoisturetype=="Wotton"){
        EFFM<-ffm1_Wotton
      }else{
        EFFM<-effm$fm1hr
      }
      
      #Probability of Crown Fire
      if(CrownFireModel[j] == "Perrakis"){
        #CFIS 2.0: Perrakis 2023 Logistic Crown + Cruz 2005 Spread
        CrownFire<- cfis_modified(fsg = FSG, sfc = SFC, effm = EFFM, u10 = WS, cbd = Plot_crown$cbd, id = 5, adjusted = FALSE)
        CrownFire$Model_type<-"Perrakis"
        #calculate surface rate of spread with the Rothermel equation 
        CrownFire$Surface_ROS<-ROS
      }else{
        #CFIS Original
        CrownFire<- firebehavioR::cfis(fsg = FSG, sfc = SFC*10, effm = EFFM, u10 = WS, cbd = Plot_crown$cbd, id = 1)
        CrownFire$Model_type<- "CFIS"
        #Calculate surface rate of spread with the Rothermel equation
        CrownFire$Surface_ROS<-ROS
      }
      
    
      
      #CFIM: Cruz 2005: Not working yet
      fuel<-data.frame(litter=litter_Mg/10,hr1=hr10_Mg/10,hr10=hr10_Mg/10,hr100=hr100_Mg/10,depth=strata_input$FB_depth,CAN_model=strata_input$Fueltype_CAD)
      topography<-data.frame(lat=Lat,lon=Lon,elev=Elev, asp=AspectCardinal,slp=strata_input$Slope)
      structure<-data.frame(ht=strata_input$Height,cc=strata_input$CC,cr=Crown_Ratio,dbh=strata_input$DBH,cbh=strata_input$FSG,cbd=strata_input$CBD,cfl=strata_input$CFL,ba= strata_input$BA,sd=strata_input$TPH)
      weather<-data.frame(temp=TEMP, rh=RH,t_min=T_min,dc=DC,dmc=DMC,bui=BUI,ffm=Plot_moisture$hr1,ws=WS,wd=WD)
      
      #cfim<-CFIM(model=plot_input$US.FuelType,
      #              fuel = fuel,
      #             structure = structure,
      #            topography = topography,
      #           weather = weather,
      #          date=date,
      #         IntensityType = "Nelson",
      #        SurfaceModel = "Rothermel",
      #       CalcRadAdsorb = TRUE,
      #      UseModel = FALSE,
      #     ModelEFFM = FALSE)
      #cfim$Model_type="CFIM"
      
      
      #Calculte Intensity, Critical Intensity, WITH Byrams and Van Wagner's Equations
      
      #calculate Crown Fraction Consumed from Groot et al 2022 using surface or crown ros
      FireType<-ifelse(CrownFire$type == "active" | CrownFire$type == "passive","crown","surface")
      
      #Calculte Intensity, Critical Intensity, WITH Byrams and Van Wagner's Equations:decide on Nelson or Byrams intensity
      CrownFire$Intensity_Surface<-I_B_BYRAM
      CrownFire$Critical_Int<-0.001*(FSG^1.5)*(460+25.9*FMC)^1.5
      #CrownFire$SFC<-ScottReinhardt$FuelConsumed_kg_m2  # Surface fuel consumption
      CrownFire$SFC<-SFC
      R_0<-3/Plot_crown$cbd
      CAC<-CrownFire$cROS/R_0
      CrownFire$ROS <-ifelse(is.na(CrownFire$cROS),CrownFire$Surface_ROS, CrownFire$cROS)
      CFC<-CFC_Calc(CrownFire$ROS,strata_input$CFL)$CFB
      CrownFire$CFC<-CFC
      CrownFire$Intensity_Crown<-(CrownFire$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CrownFire$cROS/60
      CrownFire$Intensity<-ifelse(is.na(CrownFire$Intensity_Crown),CrownFire$Intensity_Surface,(CrownFire$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CrownFire$ROS/60)
      CrownFire$Flame_Length<-0.0775*(CrownFire$Intensity^0.46)
      CrownFire$CrownScorch<-(4.4713*CrownFire$Intensity^0.667)/(60-TEMP)
      
      #Create Export Data
      surfacefire<-data.frame(
        Model=model_roth,
        ROS=ROS,
        Intensity=CrownFire$Intensity_Surface,
        SurfFuelConsumed=SFC,
        BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s,
        ResidenceTime=Nelson$Residence.Time.s,
        SpreadDir=ScottReinhardt$fireBehavior$`Direction of max spread [deg]`,
        FlameLength=0.0775*(CrownFire$Intensity_Surface^0.46)
      )
      
      transition<-data.frame(
        CritCBH=ScottReinhardt$critInit$`Canopy base height [m]`,
        CritIntensity=CrownFire$Critical_Int,
        CritFlameLength=ScottReinhardt$critInit$`Flame length (m)`,
        CritROS=ScottReinhardt$critInit$`Surface ROS [m/min]`,
        CritActiveCBD=ScottReinhardt$critActive$`Canopy bulk density [kg/m3]`,
        CritActiveROS=ScottReinhardt$critActive$`ROS, crown (R'active) [m/min]`
      )
      
      
      
      # Store the results in a list
      results_list[[paste(P,"Strata", STRATA, "Weather", i, sep = "_")]] <- list(
        Period=P,
        Stratum= STRATA,
        CANModel=strata_input$Fueltype_CAD,
        USModel=model_roth,
        SurfaceFire = surfacefire,
        Transition = transition,
        CrownFire = CrownFire,
        #CFIM=cfim,
        FBP=FBP,
        Plot_sfc = Plot_sfc,
        Plot_fuels = Plot_fuels,
        Plot_moisture = Plot_moisture,
        Plot_crown = Plot_crown,
        Plot_stand = Plot_stand)
      
      # Tick the progress bar:
      iter <- iter + 1
      pb$tick()
      write_progress(
        status = "running",
        period = P,
        weather_index = i,
        weather_total = nrow(weather_list),
        stratum = STRATA,
        iter = iter,
        total_iters = total_iters,
        message = "Completed loop iteration",
        force_console = (iter == 1 || iter %% 10 == 0 || iter == total_iters)
      )
      
      }
    }
  }
})
saveRDS(results_list,paste0(path,Fire_out,"FireModelingResults.rds"))


#Extract Results:--------------------------------------------------------
Results<-readRDS(paste0(path,Fire_out,"FireModelingResults.rds"))

Res <- dplyr::bind_rows(
  lapply(names(Results), function(name) {
    res <- Results[[name]]
    
    # Extract WeatherList names
    strata_match <- str_match(name, "Strata_?(.*?)_")[,2]
    weather_match <- str_match(name, "Weather[_=](\\d+)")[,2]
    period_match  <- str_match(name, "^(.*?)_+Strata")[,2]
    
    # Return as a plain data.frame row
    data.frame(
      Period = period_match,
      Strata = strata_match,
      Weather = weather_match,
      
      #Surface Fire
      ModelNFFDRS=res$SurfaceFire$Model,
      SpreadDirection=res$SurfaceFire$SpreadDir,
      SFC=res$SurfaceFire$SurfFuelConsumed,
      ResidenceTime.s=res$SurfaceFire$ResidenceTime,
      BurningRate=res$SurfaceFire$BurningRate,
      FireType=ifelse(res$CrownFire$type=="active","Active Crown Fire",
                      ifelse(res$CrownFire$type=="passive","Passive(Torching) Crown Fire","Surface Fire")),
      SurfaceROS=res$SurfaceFire$ROS,
      SurfaceIntensity=res$SurfaceFire$Intensity,
      SurfaceFlameLength=res$SurfaceFire$FlameLength,
      CritROS=res$Transition$CritROS,
      CritROSActive=res$Transition$CritActiveROS,
      CritIntensity=res$Transition$CritIntensity,
      CritFlameLength=res$Transition$CritFlameLength,
      CritCBH=res$Transition$CritCBH,
      CrownProbability=res$CrownFire$pCrown,
      CrownFireProbModel=res$CrownFire$Model_type,
      CrownROS=res$CrownFire$cROS,
      CrownIntensity=res$CrownFire$Intensity_Crown,
      CrownFractConsumed=res$CrownFire$CFC,
      CrownFlameLength=res$CrownFire$Flame_Length,
      CrownScorch=res$CrownFire$CrownScorch,
      
      #FBP Metrics:
      FBP.Type = ifelse(res$FBP$Type=="active","Active Crown Fire",
                        ifelse(res$FBP$Type=="passive","Passive(Torching) Crown Fire","Surface Fire")),
      FBP.CFB = res$FBP$CFB,
      FBP.CFC = res$FBP$CFC,
      FBP.ROS = res$FBP$ROS,
      FBP.Intensity = res$FBP$HFI,
      FBP.FlameLength = 0.0775*(res$FBP$HFI^0.46),
      FBP.CritIntensity = res$FBP$CSI,
      
      # Inputs metrics
      USModel=res$USModel,
      CANModel=res$CANModel,
      FuelMoist_1hr=res$Plot_moisture$hr1,
      FMC=res$Plot_crown$fmc,
      WS=res$Plot_stand$ws,
      WD=res$Plot_stand$wind_d,
      ISI=res$FBP$ISI,
      FFMC=res$FBP$FFMC,
      stringsAsFactors = FALSE
    )
  })
)

write.csv(Res,paste0(path,Fire_out,"FireModelingResults.csv"),row.names=FALSE)

#------------------------------------------------------------------------
#Plot Fire Behavior Results:-------------------------------------------
PlotData<-read.csv(paste0(path,Fire_out,"FireModelingResults.csv"))
PlotData <- PlotData %>%
  mutate(Period = factor(Period, levels = c("Pre", "Post")))
#Treatment Overview:----------------------------------------------------------------
Structure<-read.csv(file.path(root,"projects",project,"data","raw",Fuel_prefix,"AllPeriods_StructureData.csv"))
Structure$FSG_Mod <- ifelse(is.na(Structure$FSG_Mod), Structure$FSG, Structure$FSG_Mod)
structure_table <- Structure %>%
  mutate(Period = factor(Period, levels = c("Pre", "Post"))) %>%
  group_by(Period, Stratum) %>%
  summarise(
    TPH       = round(mean(TPH, na.rm = TRUE), 1),
    Fine_Fuel = round(mean(Fine_Fuel_kg, na.rm = TRUE), 3),
    CBH       = round(mean(FSG, na.rm = TRUE), 2),
    CFL       = round(mean(CFL, na.rm = TRUE), 3),
    CBD       = round(mean(CBD, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(Period, Stratum)
table<-flextable(structure_table) %>%
  set_header_labels(
    Period    = "Treatment Period",
    Stratum   = "Stratum",
    TPH       = "Stems per\nHectare",
    Fine_Fuel = "Fine Fuel Load\n(1-100hr)(kg/m²)",
    CBH       = "Canopy Base\nHeight (m)",
    CFL       = "Canopy Fuel Load\n(ton/ha)",
    CBD       = "Canopy Bulk\nDensity (kg/m³)"
  ) %>%
  add_header_row(
    values    = c("", "", "", "Surface Fuels", "Canopy Characteristics"),
    colwidths = c(1,  1,  1,  1,              3)
  ) %>%
  merge_v(j = "Period") %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bold(j = "Period") %>%
  bg(part = "header", bg = "#2C3E50") %>%
  color(part = "header", color = "white") %>%
  bg(i = ~ Period == "Pre",  bg = "#EBF5FB") %>%
  bg(i = ~ Period == "Post", bg = "#EAFAF1") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  set_caption("Stand Structure Summary — Pre and Post Treatment by Stratum") %>%
  autofit()
save_as_image(table, paste0(path,Fire_out,"TreatmentSummaryTable.png"))
#FBP at 90th:-------------------------------------------------------------
Wthr90<-read.csv(paste0(path,out_weather,"processed/",WeatherName,"_Percentiles.csv"))
Inputwthr<-Wthr90 %>% filter(percentile_values == "90th")
#RUN FBP @90
LAT<-first(daily_weather$LAT)
LONG<-first(daily_weather$LONG)
FMC<-FMC_calc(LAT,LONG,Elev,"2025-07-15")

FBP_df<-data.frame(
  Period=Structure$Period,
  FuelType=ifelse(Structure$Fueltype_CAD == "D-1/2","D-1",ifelse(Structure$Fueltype_CAD == "M-1/2","M-1",ifelse(Structure$Fueltype_CAD == "M-3/4","M-3",Structure$Fueltype_CAD))),
  LAT=rep(LAT,nrow(Structure)),
  LONG= rep(LONG,nrow(Structure)),
  FFMC= rep(Inputwthr$FFMC,nrow(Structure)),
  BUI=rep(Inputwthr$BUI,nrow(Structure)),
  WS= rep(Inputwthr$WS,nrow(Structure)),
  WD=rep(Inputwthr$WD,nrow(Structure)),
  GS=Structure$Slope,
  Dj= rep(200,nrow(Structure)),
  Aspect= Structure$Aspect,
  ELV=rep(Elev,nrow(Structure)),
  ISI=rep(ISI,nrow(Structure)),
  CFL= Structure$CFL,
  cc=rep(GrassCuring,nrow(Structure)),
  GFL=Structure$Grass_Loading
)
FBP.res<-cffdrs::fbp(FBP_df,output = "ALL") %>% 
  mutate(Period=Structure$Period,Strata=Structure$Stratum,FireType=ifelse(FD == "C","Active Crown Fire",
                                                                          ifelse(FD == "S", "Surface Fire", "Passive(Torching) Crown Fire")),CFB=round(CFB*100,2),ROS=round(ROS,2),
         FT=Structure$Fueltype_CAD, 
         FI=round(ROS*300*Structure$Fine_Fuel_kg,2),
         CSI=round(0.001*(Structure$FSG^1.5)*(460+25.9*FMC)^1.5,2),
         CSI_Met=FI<CSI,
         CSI_Fuels=signif(CSI/(300*ROS),2),
         Fuel=signif(Structure$Fine_Fuel_kg,2)) %>% 
  dplyr::select(Strata,Period,FT,Fuel,FireType,ROS,FI,CSI,CSI_Met,CSI_Fuels)

FBPOUT <- flextable(FBP.res) %>%
  set_header_labels(
    Period    = "Treatment Period",
    FT        = "Fuel Type",
    Stratum   = "Stratum",
    Fuel      = "Fine Fuel\nLoad\n(<7cm kg/m^2)",
    FireType  = "Fire Type",
    ROS       = "Rate of\nSpread\n(meters/min)",
    FI        = "Head Fire\nIntensity\n(kW/m^2)",
    CSI       = "Critical Surface\nFire Initiation\n(kW/m^2)",
    CSI_Met   = "CSI Standard\nMet?",
    CSI_Fuels = "Fine Fuels\nRequired for\nCSI(kg/m^2)"
  ) %>%
  # Add BUI/ISI first (will become row 2)
  add_header_row(
    values    = c(paste0("BUI: ", Inputwthr$BUI),
                  paste0("ISI: ", Inputwthr$ISI)),
    colwidths = c(5, 5)
  ) %>%
  # Add title second (will become row 1)
  add_header_row(
    values    = "Fire Behaviour Summary at 90th Percentile Conditions",
    colwidths = 10
  ) %>%
  merge_v(j = "Period") %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bold(j = "Period") %>%
  # Row 1 = title (dark green)
  bg(part = "header", i = 1, bg = "#1E5631") %>%
  color(part = "header", i = 1, color = "white") %>%
  fontsize(part = "header", i = 1, size = 14) %>%
  # Row 2 = BUI/ISI (sage green)
  bg(part = "header", i = 2, bg = "#A8D5B5") %>%
  color(part = "header", i = 2, color = "black") %>%
  # Row 3 = column labels (dark green)
  bg(part = "header", i = 3, bg = "#1E5631") %>%
  color(part = "header", i = 3, color = "white") %>%
  bg(i = ~ Period == "Pre",  bg = "#EBF5FB") %>%
  bg(i = ~ Period == "Post", bg = "#EAFAF1") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  autofit()
save_as_image(FBPOUT, paste0(path,Fire_out,"FBP_CSISummaryTable.png"))


if(AdvancedModels){
CrownPlotData <- PlotData %>%
  filter(is.finite(CrownProbability))

if(nrow(CrownPlotData) > 0){
#Probability of Crown Fire BoxPlot:--------------------------------------------------
pCrown_box <- CrownPlotData %>%
  ggplot(aes(x = Period, y = CrownProbability, fill = Period, color = Period)) +
  geom_jitter(width = 0.15, alpha = 0.05, size = 0.3) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.4) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 1.5,
               fill = "white", color = "black", stroke = 0.5) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0(round(after_stat(y), 1),"%")),
    hjust = -0.3,
    vjust= 1,
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  # Dummy layer for median legend entry
  geom_point(aes(shape = "Median"), x = Inf, y = Inf,
             fill = "white", color = "black", size = 1.5, stroke = 0.5,
             inherit.aes = FALSE) +
  scale_shape_manual(name = NULL, values = c("Median" = 23),
                     guide = guide_legend(
                       override.aes = list(fill = "white", color = "black")
                     )) +
  scale_fill_manual(values  = c("Pre" = "#D4A843", "Post" = "#4A7C59")) +
  scale_color_manual(values = c("Pre" = "#B8892A", "Post" = "#2E5E3E")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  facet_wrap(~Strata, scales = "free_x", nrow = 1) +
  academic_theme +
  labs(
    title    = "Probability of Crown Fire by Treatment Period\n and Strata: Pre or Post Treatment",
    subtitle = "Range of predicted crown fire probability under extreme weather conditions",
    y        = "Predicted Probability of Crown Fire (%)",
    x        = "Strata and Treatment Period"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
ggsave(paste0(path,Fire_out,"ProbabilityCrownFireBoxPlot.png"),
       pCrown_box,
       width = 7,    # increase this for wider
       height = 5,    # adjust height as needed
       units = "in",
       dpi = 300) 

#Probability of Crown by Wind Speed:---------------------------------------------
pCrown_Point <- CrownPlotData %>%
  ggplot(aes(x = WS, y = CrownProbability, fill = Period, color = Period)) +
  geom_vline(xintercept = seq(0, max(CrownPlotData$WS, na.rm = TRUE), 5), linetype = "dashed", 
             color = "grey60", linewidth = 0.2) +
  geom_hline(yintercept = seq(0, 100, 25), linetype = "dashed", 
             color = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "red3", linewidth = 0.6) +
  geom_text(data = data.frame(Strata = unique(CrownPlotData$Strata)),
            aes(x = Inf, y = 52, label = "Crown Fire Predicted"),
            hjust = 1.05, vjust = 0, size = 4, color = "red3",
            fontface = "italic", inherit.aes = FALSE) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.75, linewidth = 1) +
  scale_fill_manual(values  = c("Pre" = "#D4A843", "Post" = "#4A7C59")) +
  scale_color_manual(values = c("Pre" = "#B8892A", "Post" = "#2E5E3E")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  facet_wrap(~Strata, scales = "fixed", nrow = 3,
             labeller = labeller(Strata = function(x) paste0("FTU ", x))) +
  academic_theme +
  labs(
    title    = "Probability of Crown Fire over Wind Speed by Strata and Treatment Period",
    subtitle = "Points show predicted values; shaded band shows fit ± 95% CI",
    y        = "Predicted Probability of Crown Fire (%)",
    x        = "Wind Speed (km/hr)"
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y  = element_text(size = 14),
    strip.text       = element_text(size = 14, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#2C3E50", color = "#2C3E50")
  )
ggsave(paste0(path, Fire_out, "CrownProbWindSpeed.png"),
       pCrown_Point,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

#ProbCrown over fine fuel moisture:--------------------------------------------
pCrown_FM <- CrownPlotData %>%
  filter(FuelMoist_1hr + 1 <= quantile(FuelMoist_1hr + 1, 0.95, na.rm = TRUE)) %>%
  ggplot(aes(x = FuelMoist_1hr+1, y = CrownProbability, fill = Period, color = Period)) +
  geom_vline(xintercept = seq(0, max(CrownPlotData$FuelMoist_1hr+1, na.rm = TRUE), 2), linetype = "dashed", 
             color = "grey60", linewidth = 0.2) +
  geom_hline(yintercept = seq(0, 100, 25), linetype = "dashed", 
             color = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "red3", linewidth = 0.6) +
  geom_text(data = data.frame(Strata = unique(CrownPlotData$Strata)),
            aes(x = Inf, y = 52, label = "Crown Fire Predicted"),
            hjust = 1.05, vjust = 0, size = 4, color = "red3",
            fontface = "italic", inherit.aes = FALSE) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.75, linewidth = 2, span=0.85) +
  scale_fill_manual(values  = c("Pre" = "#D4A843", "Post" = "#4A7C59")) +
  scale_color_manual(values = c("Pre" = "#B8892A", "Post" = "#2E5E3E")) +
  scale_x_continuous(limits = c(0, quantile(CrownPlotData$FuelMoist_1hr + 1, 0.95, na.rm = TRUE)),
                     breaks = seq(0, quantile(CrownPlotData$FuelMoist_1hr + 1, 0.95, na.rm = TRUE), 2)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  facet_wrap(~Strata, scales = "fixed", nrow = 1,
             labeller = labeller(Strata = function(x) paste0("FTU ", x))) +
  academic_theme +
  labs(
    title    = "Probability of Crown Fire over 10-Hour Fine Fuel Moisture\n by Strata and Treatment Period",
    subtitle = "Points show predicted values; line shows best fit to data",
    y        = "Predicted Probability of Crown Fire (%)",
    x        = "Fine Fuel Moisture (10-Hour %)"
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y  = element_text(size = 14),
    strip.text       = element_text(size = 14, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#2C3E50", color = "#2C3E50")
  )
ggsave(paste0(path,Fire_out,"CrownProbFuelMoist.png"),
       pCrown_FM,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)
}
#Head Fire Intensity:---------------------------------------------
pHFI_bar <- PlotData %>%
  mutate(HeadFireIntensity = ifelse(!is.na(CrownProbability) & CrownProbability >= 50 & !is.na(CrownIntensity), CrownIntensity, SurfaceIntensity)) %>%
  group_by(Strata, Period) %>%
  summarise(MedianHFI = median(HeadFireIntensity, na.rm = TRUE),
            Q40 = quantile(HeadFireIntensity, 0.40, na.rm = TRUE),
            Q51 = quantile(HeadFireIntensity, 0.51, na.rm = TRUE),
            Q60 = quantile(HeadFireIntensity, 0.60, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Strata, y = MedianHFI, fill = Period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           color = "black", linewidth = 0.3) +
  #geom_errorbar(aes(ymin = Q40, ymax = Q60),
   #             position = position_dodge(width = 0.7),
    #            width = 0.2, linewidth = 0.4, color = "black") +
  geom_text(aes(label = paste0(round(MedianHFI, 0),"(kW/m^2)"),
                y = Q51),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("Pre" = "#D4A843", "Post" = "#4A7C59")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  academic_theme +
  labs(
    title    = "Predicted Median Head Fire Intensity change\n by Treatment",
    subtitle = "Predicted over a range of extreme weather conditions",
    y        = "Median Head Fire Intensity (kW/m)",
    x        = "Strata or FTU",
    fill     = "Period"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
ggsave(paste0(path, Fire_out, "MedianHFIBarPlot.png"),
       pHFI_bar,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

#Rate Of Spread:--------------------------------------------------------------
pROS_bar <- PlotData %>%
  mutate(ROS = ifelse(!is.na(CrownProbability) & CrownProbability >= 50 & !is.na(CrownROS), CrownROS, SurfaceROS)) %>%
  group_by(Strata, Period) %>%
  summarise(MedianROS = median(ROS, na.rm = TRUE),
            Q40 = quantile(ROS, 0.40, na.rm = TRUE),
            Q51 = quantile(ROS, 0.51, na.rm = TRUE),
            Q60 = quantile(ROS, 0.60, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Strata, y = MedianROS, fill = Period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           color = "black", linewidth = 0.3) +
  #geom_errorbar(aes(ymin = Q40, ymax = Q60),
  #             position = position_dodge(width = 0.7),
  #            width = 0.2, linewidth = 0.4, color = "black") +
  geom_text(aes(label = paste0(round(MedianROS, 0),"(m/min)"),
                y = Q51),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("Pre" = "#D4A843", "Post" = "#4A7C59")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  academic_theme +
  labs(
    title    = "Predicted Median Wildfire Rate of Spread change\n by Treatment",
    subtitle = "Predicted over a range of extreme weather conditions",
    y        = "Median Rate of Spread (meters/min))",
    x        = "Strata or FTU",
    fill     = "Period"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
ggsave(paste0(path, Fire_out, "MedianROSBarPlot.png"),
       pROS_bar,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)
}
  summary <- list(
    project_name = project_name,
    raw_dir = cfg$runtime$raw_dir,
    firebehavior_output_dir = file.path(path, "FireBehavior", "Outputs"),
    plots = list(
      treatment_summary = file.path(path, "FireBehavior", "Outputs", "TreatmentSummaryTable.png"),
      probability_crown_fire_boxplot = file.path(path, "FireBehavior", "Outputs", "ProbabilityCrownFireBoxPlot.png"),
      crown_prob_windspeed = file.path(path, "FireBehavior", "Outputs", "CrownProbWindSpeed.png"),
      crown_prob_fuel_moist = file.path(path, "FireBehavior", "Outputs", "CrownProbFuelMoist.png"),
      median_hfi = file.path(path, "FireBehavior", "Outputs", "MedianHFIBarPlot.png"),
      median_ros = file.path(path, "FireBehavior", "Outputs", "MedianROSBarPlot.png"),
      fbp_90th_csi_stand = file.path(path, "FireBehavior", "Outputs", "FBP_CSISummaryTable.png")
    )
  )
  saveRDS(summary, file.path(step_dir, "firemodel_results_outputs.rds"))
  jsonlite::write_json(summary, file.path(step_dir, "firemodel_results_summary.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  write_progress(status = "completed", iter = total_iters, total_iters = total_iters, message = "Step 3 completed", force_console = TRUE)
}
#---------------------------------------------------------------------
