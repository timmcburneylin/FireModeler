step2 <- function(cfg, root) {
cat("Running Step 2: FuelCalc To FireModel\n")

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
  a
}

step_cfg <- cfg$fuelcalc_to_firemodel %||% list()
template_path <- function(name) file.path(cfg$runtime$templates_dir %||% file.path(root, "templates"), name)

#Libraries Loading:
library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(cffdrs)
library(reshape2)
library(tidyr)
library(glue)
library(ggdist)

has_openair <- requireNamespace("openair", quietly = TRUE)

safe_plot_save <- function(label, expr) {
  tryCatch(
    {
      force(expr)
      TRUE
    },
    error = function(e) {
      warning(paste0(label, " plot generation failed: ", conditionMessage(e)))
      FALSE
    }
  )
}

project_name <- cfg$project_name %||% ""
if (!nzchar(project_name)) stop("config project_name is required for FuelCalc To FireModel")
name <- project_name
project <- name

#File Paths:
path <- cfg$runtime$raw_dir

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
in_weather <- "/Weather/"
out_weather <- "/Weather/"
out_residuals <- paste0(path, "/FuelCalcBC/Outputs/Slash/Residuals/")
results <- "/Outputs/"
path_fcp <- paste0(path,"/FuelCalcBC/")

dir.create(file.path(path, "Weather", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "Weather", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "Weather", "WindRoses"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "Weather", "DangerDays"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "Weather", "WeatherConditions"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "Weather", "WeatherLists"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "FireBehavior", "Inputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "FireBehavior", "Outputs"), recursive = TRUE, showWarnings = FALSE)

# Load in SNAP summary files
Snap_OS <- read.csv(paste0(path,snap_prefix,project,"_OS.csv"))
Snap_US <- read.csv(paste0(path,snap_prefix,project,"_US.csv"))
Snap_EX <- read.csv(paste0(path,snap_prefix,project,"_EXTRA.csv"))
Snap_fuels <- read.csv(paste0(path,snap_prefix,project,"_FUELS.csv"))


#Functions:
mode <- function(x) {
  unique_vals <- unique(x)
  unique_vals[which.max(tabulate(match(x, unique_vals)))]
}
pct_extreme <- function(x, var_name) {
  if (var_name == "RelativeHumidity (%)") {
    quantile(x, 0.10)
  } else {
    quantile(x, 0.90)
  }
}

#UI Inputs(Modifiable):------------------------------------------
#What weather station data are you uploading: Either EC (Environment Canada) or MOF (Ministry of Forests)
#Go to this link: https://services.pacificclimate.org/met-data-portal-pcds/app/ and choose the closest
#Environment Canada Raw (EC_Raw) and Ministry of Forestry (FLNRO-WMB) Station
WthrType <- as.character(step_cfg$weather_type %||% "MOF") #dropdown #What weather station type is this? "MOF" or "EC"
WthrName <- as.character(step_cfg$weather_name %||% "MERRITT 2 HUB") #userinput #What do you want your station name to be?
WthrCode <- as.numeric(step_cfg$weather_code %||% 1399) #userinput #What is the Station Code (Native ID) from PCDS?
WthrLat <- as.numeric(step_cfg$weather_lat %||% 50.121389) #userinput #What is the station Latitude?
WthrLong <- as.numeric(step_cfg$weather_long %||% -120.744167) #userinput What is the station Longitude?
DangerRegion <- as.numeric(step_cfg$danger_region %||% 3) #dropdown #What is the Wildfire Danger Region? options 1 2 or 3

#PLOT DISPLAY:
#-Wind Rose: paste0(path,in_weather,"WindRoses/", WthrName, "_WindRoses.jpg")
#-Danger Days: paste0(path,in_weather,"DangerDays/", WthrName, "_DangerDays.jpg")
#-Weather Conditions: paste0(path, in_weather, "WeatherConditions/", WthrName, "_WeatherDistributions.jpg")


#-------------------------------------------------------------------
#Stationary Inputs:-------------------------------------------------
coniferList <- c("Ba","Bl", "Bg","Bb","Cw", "Fd", "Hw","T","Pl", "Sx","Sb","Sw", "Lw", "Lt", "Pw","Yc", "Fdi", "Fdc", "Py","Pli")
nonConiferList <- c("Act","Acb","Ac","At","Ep","DP", "DU","Dead", "Dr")

#------------------------------------------------------------------
#Process Weather:
  if(WthrType == "MOF"){
    #Load Raw:
    data<-read.csv(paste0(path,in_weather,"raw/",WthrCode,".csv"),header=FALSE)
    names<-as.vector(data[2,1:ncol(data)])
    colnames(data)<-names
    data<-data[-1,]
    final_data<-data[-1,]
    write.csv(final_data,paste0(path,in_weather,"/raw/",WthrName,".csv"),row.names = FALSE)
    
    #Extract ecodivision
    ECODIV<-st_read(template_path("ERC_ECODIV_polygon.shp"))
    stn_pnt <- st_sfc(st_point(c(WthrLong, WthrLat)), crs = 4326)
    stn_pnt <- st_transform(stn_pnt, crs = 3005)  
    result <- st_intersection(ECODIV, stn_pnt)
    Ecodivision <- result$CDVSNNM
    
    #Extract Weather zone and ndt zone
    BECOLD<-st_read(template_path("BEC_Zones_OLD.shp"))
    stn_pnt <- st_sfc(st_point(c(WthrLong, WthrLat)), crs = 4326)
    stn_pnt <- st_transform(stn_pnt, crs = 3005)  
    result <- st_intersection(BECOLD, stn_pnt)
    wthr_zone<-result$wx_zone
    ndt<-result$ndt
    
    #Import CSV Data
    weather<- read.csv(paste0(path,in_weather,"raw/",WthrName,".csv"),skip =  0,header = TRUE,check.names = F)
    df = weather
  #select out columns you need
  colnames(df) <- trimws(colnames(df))
  names<-colnames(df)
  df2 = df %>%
    dplyr::select(wind_direction, precipitation, wind_speed, temperature, relative_humidity, time)
  colnames(df2)<- c("wind_direction", "precipitation","wind_speed","temperature","relative_humidity","time")
  df2$Month <- as.numeric(substr(df2$time, 7, 8))
  df2$Day = as.numeric(substr(df2$time, 10,11))
  df2$Year = as.numeric(substr(df2$time,1,5))
  df2$Hour = as.numeric(substr(df2$time,13,14))
  #filter years
  df2 = df2[df2$Year > 2009,]
  df2$date<-format(as.Date(df2$time),"%Y-%m-%d")
  
  }else{
    #Load Raw:
    data<-read.csv(paste0(path,in_weather,"raw/",WthrCode,".csv"),header=FALSE)
    names<-as.vector(data[2,1:ncol(data)])
    colnames(data)<-names
    data<-data[-1,]
    final_data<-data[-1,]
    write.csv(final_data,paste0(path,in_weather,"/raw/",WthrName,".csv"),row.names = FALSE)
    
    #Extract ecodivision
    ECODIV<-st_read(template_path("ERC_ECODIV_polygon.shp"))
    stn_pnt <- st_sfc(st_point(c(WthrLong, WthrLat)), crs = 4326)
    stn_pnt <- st_transform(stn_pnt, crs = 3005)  
    result <- st_intersection(ECODIV, stn_pnt)
    Ecodivision <- result$CDVSNNM
    
    #Extract Weather zone and ndt zone
    BECOLD<-st_read(template_path("BEC_Zones_OLD.shp"))
    stn_pnt <- st_sfc(st_point(c(WthrLong, WthrLat)), crs = 4326)
    stn_pnt <- st_transform(stn_pnt, crs = 3005)  
    result <- st_intersection(BECOLD, stn_pnt)
    wthr_zone<-result$wx_zone
    ndt<-result$ndt
    
    #Import CSV Data
    weather<- read.csv(paste0(path,in_weather,"raw/",WthrName,".csv"),skip =  0,header = TRUE,check.names = F)
    df = weather
    colnames(df) <- trimws(colnames(df))
    names<-colnames(df)
    df2 = df %>%
      dplyr::select(wind_direction, total_precipitation, wind_speed, air_temperature, relative_humidity,time)
    colnames(df2)<- c("wind_direction", "precipitation","wind_speed","temperature","relative_humidity",time)
    df2$Month <- as.numeric(substr(df2$time, 7, 8))
    df2$Day = as.numeric(substr(df2$time, 10,11))
    df2$Year = as.numeric(substr(df2$time,1,5))
    df2$Hour = as.numeric(substr(df2$time,13,14))
    #filter years
    df2 = df2[df2$Year > 2000,]
    df2$date<-format(as.Date(df2$time),"%Y-%m-%d")
  }

  #create year,month, day columns
  write.csv(df2,paste0(path,in_weather,"raw/",WthrName,"_Hourly_Weather.csv"),row.names = FALSE)
  
  
#Calculate Daily FWI:
  csv2 <- df2 %>% dplyr::select(-date)
  ## TO CALCULATE DAILY
  csv2$lat = WthrLat
  csv2$long = WthrLong
  csv2$weather_date = as.character(rep("0",NROW(csv2)))
  
  ## CALCULATE DAILY
  for (j in 1:length(csv2$weather_date)){
    csv2$weather_date[j] = paste0(csv2$Year[j],"/",csv2$Month[j],"/",csv2$Day[j])
  }
  csv2$weather_date = as.POSIXct(csv2$weather_date, format = "%Y/%m/%d", tz = "MST")
  csv2=csv2[order(csv2$weather_date),]
  
  # FOR EC Stations extract total daily precip at 23:00 hours or just sum all values which will sum to total
  if(WthrType == "EC"){
    csv2$precipitation <- csv2$precipitation %>% 
    replace(. == "None" | is.na(.), 0)  
    csv2$precipitation<- as.numeric(csv2$precipitation)
    prec = csv2 |> dplyr::group_by(weather_date) |> dplyr::summarize(prec = sum(precipitation, na.rm = TRUE))
    #Pull only Noon Data: IMPORTANT!
    csv2 = csv2[csv2$Hour == 23,]
    
  }else{
    csv2$precipitation = csv2$precipitation %>% replace(is.na(.),0)
    csv2$precipitation<- as.numeric(csv2$precipitation)
    prec = csv2 |> dplyr::group_by(weather_date) |> dplyr::summarize(prec = sum(precipitation, na.rm = TRUE))
    #Pull only Noon Data: IMPORTANT!
    csv2 = csv2[csv2$Hour == 12,]
    
  }
  ## DAILY
  colnames(csv2) = c("wd","prec","ws","temp","rh","time","mon","day","yr","hr","lat","long","Date")
  csv2$temp=as.numeric(csv2$temp)
  csv2$rh=as.numeric(csv2$rh)
  csv2$ws=as.numeric(csv2$ws)
  csv2$wd=as.numeric(csv2$wd)
  csv2<- csv2[!is.na(csv2$temp),]
  csv2<- csv2[!is.na(csv2$rh),]
  csv2<- csv2[!is.na(csv2$ws),]
  csv2<- csv2[!is.na(csv2$wd),]
  
  #Adds precip:
  for (k in 1:NROW(csv2)){
    tempo = prec$prec[which(prec$weather_date == csv2$Date[k])]
    csv2$prec[k] = tempo
  }
  
  #RAW Outfile
  raw_out<-csv2
  
  ## BASED ON: https://wps-prod.apps.silver.devops.gov.bc.ca/static/media/90th_percentile_calculator_rationale.d02b2d44.pdf
  #Calculate wildfire season based on ecodivisions:
  if(Ecodivision == "SUB-ARCTIC HIGHLANDS"){
    all = (6:8)
    lastmnth <- as.character(c(16:31))
    
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 8)&(day %in% lastmnth)))
    
  }else if(Ecodivision == "SUB-ARCTIC"){
    all = (6:8)
    lastmnth <- as.character(c(16:31))
    
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 8)&(day %in% lastmnth)))
    
  }else if(Ecodivision == "BOREAL"){
    
    all = (5:8)
    startmonth <- as.character(c(1:14))
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 5)&(day %in% startmonth)))  
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  }else if(Ecodivision == "HUMID CONTINENTAL HIGHLANDS"){
    
    all = (5:8)
    startmonth <- as.character(c(1:14))
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 5)&(day %in% startmonth)))       
    
  }else if(Ecodivision == "NORTHEASTERN SUB-ARCTIC PACIFIC"){
    
    all = (6:8)
    lastmnth <- as.character(c(16:31))
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 8)&(day %in% lastmnth)))
    
  }else if(Ecodivision == "COOL HYPERMARITIME AND HIGHLANDS"){
    
    all = (5:8)
    startmonth <- as.character(c(1:14))
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 5)&(day %in% startmonth)))    
    
  }else{
    all = (5:9)
    lastmnth <- as.character(c(16:31))
    
    csv2 = csv2[csv2$mon %in% all,]
    csv2 = filter(csv2,!((mon == 9)&(day %in% lastmnth))) 
    
  }
  
  ## DAILY
  csv2$timeDif =c(as.numeric(difftime(csv2$Date[2:length(csv2$Date)],
                                      csv2$Date[1:(length(csv2$Date)-1)],
                                      units="days")),NA)
  csv2=csv2[order(csv2$Date),]
  
  #for non-summer data
  raw_out$timedif=c(as.numeric(difftime(raw_out$Date[2:length(raw_out$Date)],
                                        raw_out$Date[1:(length(raw_out$Date)-1)],
                                        units="days")),NA)
  raw_out=raw_out[order(raw_out$Date),]
  
  ##################################################################################################################
  #change Windspeed from none to zero
  for(i in 1:nrow(csv2)) {
    if(csv2$ws[i] == "None" | is.na(csv2$ws[i])) {
      csv2$ws[i] = 0
    }
  }
  
  for(i in 1:nrow(raw_out)) {
    if(raw_out$ws[i] == "None" | is.na(csv2$ws[i])) {
      raw_out$ws[i] = 0
    }
  }
  
  ## DAILY
  index2 = which(csv2$timeDif>=3)    
  index2_x=which(raw_out$timedif>=3)
  
  #  GAP THRESHOLD FORRESETTING FWI CALCULATIONS, HOW MANY DAYS ARE ACCEPTABLE???
  if (length(index2)< 1){index2=NROW(csv2)}
  if (length(index2_x)< 1){index2_x=NROW(raw_out)}
  
  ## DAILY
  mylyst2=list()
  for (i in 1:length(index2)){
    if (i == 1){mylyst2[[paste0("df",as.character(index2[i]))]] = csv2[1:index2[i],]}
    else {
      idx12 = index2[i-1]+1
      mylyst2[[paste0("df",as.character(index2[i]))]] = csv2[idx12:index2[i],]
    }
    if (i == length(index2)){
      idx22 = index2[length(index2)] + 1
      mylyst2[["final"]] = csv2[idx22:(NROW(csv2)),]
    }
  }
  
  mylyst2_raw=list()
  for (i in 1:length(index2_x)){
    if (i == 1){mylyst2_raw[[paste0("df",as.character(index2_x[i]))]] = raw_out[1:index2_x[i],]}
    else {
      idx12 = index2_x[i-1]+1
      mylyst2_raw[[paste0("df",as.character(index2_x[i]))]] = raw_out[idx12:index2_x[i],]
    }
    if (i == length(index2_x)){
      idx22 = index2_x[length(index2_x)] + 1
      mylyst2_raw[["final"]] = raw_out[idx22:(NROW(raw_out)),]
    }
  }
  
  ## DAILY
  #DEFAULT = ffmc=74,dmc=40,dc=300)
  for (i in 1:length(mylyst2)){
    tmp = mylyst2[[i]] 
    tmp<- tmp[!is.na(tmp$prec),]
    fwi = fwi(tmp,init=data.frame(ffmc=74,dmc=40,dc=300,lat=WthrLat,long=WthrLong), batch=TRUE,out= "all",
              lat.adjust=TRUE,uppercase=TRUE)
    fwi[,c(14:length(fwi))]=round(fwi[,c(14:length(fwi))],2)
    mylyst2[[i]] = fwi
  }
  
  final2 = bind_rows(mylyst2, .id = "column_label")
  final2 = final2[-nrow(final2),]
  
  write.csv(final2,paste0(path,out_weather,"processed/",WthrName,"_Daily_FWI.csv"),row.names = F)
  
  #Calculate 90th:
  # Weather variables to summarize
  weather_vars <- c("WD", "PREC", "WS", "TEMP", "RH", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "DSR")
  
  # Percentile probabilities
  probs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1.00)
  pct_names <- c("0", "10th", "25th", "50th", "75th", "90th", "100th")
  
  # Variables where low values = extreme (percentiles flipped)
  flip_vars <- c("RH", "PREC")
  
  # Calculate percentiles for each variable
  results <- lapply(weather_vars, function(var) {
    x <- final2[[var]]
    
    if (var == "WD") {
      # Mode only, repeated for all percentile rows
      rep(mode(x), length(probs))
      
    } else if (var %in% flip_vars) {
      # Flip: use reversed probs so low values appear at extreme ends
      quantile(x, probs = rev(probs), na.rm = TRUE)
      
    } else {
      quantile(x, probs = probs, na.rm = TRUE)
    }
  })
  
  # Build dataframe
  names(results) <- weather_vars
  df_percentiles <- as.data.frame(results)
  df_percentiles <- cbind(percentile_values = pct_names, df_percentiles)
  rownames(df_percentiles) <- NULL
  write.csv(df_percentiles,paste0(path,out_weather,"processed/",WthrName,"_Percentiles.csv"),row.names = F)
  
  #DEFAULT = ffmc=74,dmc=40,dc=300)
  for (i in 1:length(mylyst2_raw)){
    tmp = mylyst2_raw[[i]] 
    tmp<- tmp[!is.na(tmp$prec),]
    fwi = fwi(tmp,init=data.frame(ffmc=74,dmc=40,dc=300,lat=WthrLat,long=WthrLong), batch=TRUE,out= "all",
              lat.adjust=TRUE,uppercase=TRUE)
    fwi[,c(14:length(fwi))]=round(fwi[,c(14:length(fwi))],2)
    mylyst2_raw[[i]] = fwi
  }
  
  final2_raw = bind_rows(mylyst2_raw, .id = "column_label")
  final2_raw = final2_raw[-nrow(final2_raw),]
  
  write.csv(final2_raw,paste0(path,out_weather,"processed/",WthrName,"_Daily_FWI_AllYear.csv"),row.names = F)
  
  #Calculate Danger Days and Wind Roses:
  Station_label<-paste0(WthrName,"(",WthrType,")")
  Station_name<-WthrName
  Months<-all

    ##################################################
  station<-read.csv(paste0(path,out_weather,"processed/",WthrName,"_Daily_FWI_AllYear.csv"))
  name<-Station_name
  final<-station
  colnames(final) = c("id", "wd","prec","ws","temp","rh","time","mon","day","yr","hr",
                      "lat","long","date","timedif","ffmc","dmc","dc","isi","bui","fwi","dsr")
  
  final$date = as.POSIXct(final$date)
  final$mon = as.integer(final$mon)
  
  mymonths <- c("Jan","Feb","Mar",
                "Apr","May","Jun",
                "Jul","Aug","Sep",
                "Oct","Nov","Dec")
  final$MonthAbb <- mymonths[ final$mon ]
  final$MonthAbb = factor(final$MonthAbb,levels = month.abb)
  
  # Define the title text
  title_text <- paste(Station_label, "Station Wind Roses")
  
  # Specify the file path for saving the JPEG image
  output_file <- paste0(path,in_weather,"WindRoses/", Station_name, "_WindRoses.jpg")

  if (has_openair) {
    safe_plot_save("Wind Rose", {
      jpeg(output_file, width = 1000, height = 750)
      on.exit(dev.off(), add = TRUE)
      plot.new()
      openair::pollutionRose(final[which(final$mon %in% Months), ],
                    pollutant = "ws", type = "MonthAbb",
                    breaks = c(0, 1, 2, 5, 10, 15, 20, 25, 30),
                    key.footer = "(km per hr)", key.position = "right", annotate=TRUE, par.settings = list(fontsize = list(text = 24)), 
                    border = "black",
                    cols = "jet")
      title(main = title_text, line = 2.5, cex.main = 2.0)
    })
  } else {
    warning("Package 'openair' is not installed; skipping Wind Rose plot generation.")
  }
#-----------------------------------------------------------------------------
  #Danger Days
  Station_label<-paste0(WthrName,"(",WthrType,")")
  Station_name<-WthrName
  Months<-all
  region<- DangerRegion
  
  # Define a vector with month names
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ###################################################
  
  csv<-read.csv(paste0(path,out_weather,"processed/",WthrName,"_Daily_FWI_AllYear.csv"))
  name<-Station_name
  colnames(csv) = c("id", "wd","prec","ws","temp","rh","time","MON","DAY","YR","hr",
                    "lat","long","date","timedif","ffmc","dmc","dc","isi","BUI","FWI","DSR")
  
  months = Months
  if(WthrType =="EC"){
  years = (2000:2026)
  }else{
  years = (2010:2026)
  }
  
  csv = csv[csv$YR %in% years,]
  csv = csv[csv$MON %in% months,]
  csv$DGR = rep_len(numeric(0),nrow(csv))
  
  
  # # BREAKS
  if(region == 1){ 
    bui1 = 20
    bui2 = 43
    bui3 = 70
    bui4 = 119
    
    fwi1 = 1
    fwi2 = 8
    fwi3 = 17
    fwi4 = 31
  }else if(region== 2){
    bui1 = 49
    bui2 = 86
    bui3 = 119
    bui4 = 159
    
    fwi1 = 5
    fwi2 = 17
    fwi3 = 26
    fwi4 = 38
  }else{
    bui1 = 51
    bui2 = 91
    bui3 = 141
    bui4 = 201
    
    fwi1 = 5
    fwi2 = 17
    fwi3 = 28
    fwi4 = 47
  }
  
  for (x in 1:nrow(csv)){
    
    if (csv$FWI[x] < fwi1 & csv$BUI[x] < bui1){csv$DGR[x] = 1}
    else if (csv$FWI[x] < fwi1 & csv$BUI[x] >= bui1 & csv$BUI[x] < bui4){csv$DGR[x] = 2}
    else if (csv$FWI[x] < fwi1 & csv$BUI[x] >= bui4 ){csv$DGR[x] = 3}
    else if (csv$FWI[x] >= fwi1 & csv$FWI[x] < fwi2 & csv$BUI[x] < bui2){csv$DGR[x] = 2}
    else if (csv$FWI[x] >= fwi1 & csv$FWI[x] < fwi2 & csv$BUI[x] >= bui2){csv$DGR[x] = 3}
    else if (csv$FWI[x] >= fwi2 & csv$FWI[x] < fwi3 & csv$BUI[x] < bui1){csv$DGR[x] = 2}
    else if (csv$FWI[x] >= fwi2 & csv$FWI[x] < fwi3 & csv$BUI[x] >= bui1 & csv$BUI[x] < bui3){csv$DGR[x] = 3}
    else if (csv$FWI[x] >= fwi2 & csv$FWI[x] < fwi3 & csv$BUI[x] >= bui3){csv$DGR[x] = 4}
    else if (csv$FWI[x] >= fwi3 & csv$FWI[x] < fwi4 & csv$BUI[x] < bui3){csv$DGR[x] = 3}
    else if (csv$FWI[x] >= fwi3 & csv$FWI[x] < fwi4 & csv$BUI[x] >= bui3 & csv$BUI[x] < bui4){csv$DGR[x] = 4}
    else if (csv$FWI[x] >= fwi3 & csv$FWI[x] < fwi4 & csv$BUI[x] >= bui4){csv$DGR[x] = 5}
    else if (csv$FWI[x] >= fwi4 & csv$BUI[x] < bui1 ){csv$DGR[x] = 3}
    else if (csv$FWI[x] >= fwi4 & csv$BUI[x] >= bui1 & csv$BUI[x] < bui3){csv$DGR[x] = 4}
    else if (csv$FWI[x] >= fwi4 & csv$BUI[x] >= bui3){csv$DGR[x] = 5}
    
  }
  
  csv1 = csv[!(csv$DGR <= 4 | is.na(csv$DGR)),]
  csv2 = csv[csv$DGR == 4 & !(is.na(csv$DGR)),] 
  
  # Only create OBJECTID if data exists
  if(nrow(csv1) > 0) {
    csv1$OBJECTID = seq(1, nrow(csv1), by = 1)
  }
  
  if(nrow(csv2) > 0) {
    csv2$OBJECTID = seq(1, nrow(csv2), by = 1)
  }
  
  
  # ========== START OF FIXED SECTION ==========
  blank = data.frame("MON"=numeric(),"freq"=numeric(),"YR"=numeric())
  blank2 = data.frame("MON"=numeric(),"freq"=numeric(),"YR"=numeric())
  
  # Process csv1 (Extreme danger days) - only if data exists
  if(nrow(csv1) > 0) {
    for (i in 1:length(unique(csv1$YR))){
      year = unique(csv1$YR)[i]
      temp = csv1[which(csv1$YR == unique(csv1$YR)[i]),]
      
      count_mnth <- temp |> dplyr::group_by(MON) |> dplyr::summarize(freq = n_distinct(OBJECTID))
      
      count_mnth$YR = rep(year,NROW(count_mnth))
      
      if (i == 1){
        blank = count_mnth
      } else {
        blank = rbind(blank,count_mnth)
      }
    }
  }
  
  # Process csv2 (High danger days) - only if data exists  
  if(nrow(csv2) > 0) {
    for(i in 1:length(unique(csv2$YR))){
      year = unique(csv2$YR)[i]
      temp = csv2[which(csv2$YR == unique(csv2$YR)[i]),]
      
      count_mnth <- temp |> dplyr::group_by(MON) |> dplyr::summarize(freq = n_distinct(OBJECTID))
      
      count_mnth$YR = rep(year,NROW(count_mnth))
      
      if (i == 1){
        blank2 = count_mnth
      } else {
        blank2 = rbind(blank2,count_mnth)
      }
    }
  }
  
  # Calculate statistics - only if data exists
  if(nrow(blank) > 0) {
    aveM    <- blank |> dplyr::group_by(MON) |> dplyr::summarize(avecnt   = mean(freq,              na.rm = TRUE))
    sdM     <- blank |> dplyr::group_by(MON) |> dplyr::summarize(sdcnt    = sd(freq,                na.rm = TRUE))
    pcntl_M <- blank |> dplyr::group_by(MON) |> dplyr::summarize(pcntlcnt = quantile(freq, 0.9,    na.rm = TRUE))
    freq_m  <- blank |> dplyr::group_by(MON) |> dplyr::summarize(freq     = sum(freq,               na.rm = TRUE))
    NN = 30*length(unique(csv1$YR))
    xtreme_mnth_data = cbind(freq_m,aveM$avecnt,sdM$sdcnt,pcntl_M$pcntlcnt,NN)
    xtreme_mnth_data$lower = xtreme_mnth_data$`aveM$avecnt` - xtreme_mnth_data$`sdM$sdcnt` /sqrt(xtreme_mnth_data$NN)
    xtreme_mnth_data$upper = xtreme_mnth_data$`aveM$avecnt` + xtreme_mnth_data$`sdM$sdcnt` /sqrt(xtreme_mnth_data$NN)
  } else {
    # Create empty dataframe with proper structure if no extreme days
    xtreme_mnth_data = data.frame(MON=numeric(), freq=numeric(), 
                                  'aveM$avecnt'=numeric(), 'sdM$sdcnt'=numeric(),
                                  'pcntl_M$pcntlcnt'=numeric(), NN=numeric(),
                                  lower=numeric(), upper=numeric())
  }
  
  if(nrow(blank2) > 0) {
    aveM2    <- blank2 |> dplyr::group_by(MON) |> dplyr::summarize(avecnt   = mean(freq,         na.rm = TRUE))
    sdM2     <- blank2 |> dplyr::group_by(MON) |> dplyr::summarize(sdcnt    = sd(freq,           na.rm = TRUE))
    pcntl_M2 <- blank2 |> dplyr::group_by(MON) |> dplyr::summarize(pcntlcnt = quantile(freq, 0.9, na.rm = TRUE))
    freq_m2  <- blank2 |> dplyr::group_by(MON) |> dplyr::summarize(freq     = sum(freq,          na.rm = TRUE))
    NN2 = 30*length(unique(csv2$YR))
    high_mnth_data = cbind(freq_m2,aveM2$avecnt,sdM2$sdcnt,pcntl_M2$pcntlcnt,NN2)
    high_mnth_data$lower = high_mnth_data$`aveM2$avecnt` - high_mnth_data$`sdM2$sdcnt` /sqrt(high_mnth_data$NN2)
    high_mnth_data$upper = high_mnth_data$`aveM2$avecnt` + high_mnth_data$`sdM2$sdcnt` /sqrt(high_mnth_data$NN2)
  } else {
    # Create empty dataframe with proper structure if no high danger days
    high_mnth_data = data.frame(MON=numeric(), freq=numeric(), 
                                'aveM2$avecnt'=numeric(), 'sdM2$sdcnt'=numeric(),
                                'pcntl_M2$pcntlcnt'=numeric(), NN2=numeric(),
                                lower=numeric(), upper=numeric())
  }
  # ========== END OF FIXED SECTION ==========
  
  # Extract unique month indices and create month names only if data exists
  if(nrow(xtreme_mnth_data) > 0) {
    unique_xtreme_mnths <- unique(xtreme_mnth_data$MON)
    xtreme_mnths <- month_names[unique_xtreme_mnths]
    xtreme_mnth_data$MonthAbb <- xtreme_mnths
    xtreme_mnth_data$MonthAbb = factor(xtreme_mnth_data$MonthAbb, levels = xtreme_mnths)
  } else {
    xtreme_mnths <- character(0)
  }
  
  if(nrow(high_mnth_data) > 0) {
    unique_high_mnths <- unique(high_mnth_data$MON)
    high_mnths <- month_names[unique_high_mnths]
    high_mnth_data$MonthAbb <- high_mnths
    high_mnth_data$MonthAbb = factor(high_mnth_data$MonthAbb, levels = high_mnths)
  } else {
    high_mnths <- character(0)
  }
  
  ##########################################################################################################
  ##########################################################################################################
  
  # Only melt and combine if data exists
  if(nrow(xtreme_mnth_data) > 0 && nrow(high_mnth_data) > 0) {
    # Both datasets have data
    xtreme_mnth_datamlt = melt(xtreme_mnth_data, id.vars=c("MonthAbb"),
                               measure.vars=c("aveM$avecnt"))
    xtreme_mnth_datamlt2 = melt(high_mnth_data, id.vars=c("MonthAbb"),
                                measure.vars=c("aveM2$avecnt"))
    err1 = melt(xtreme_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("upper"))
    err2 = melt(xtreme_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("lower"))
    err3 = melt(high_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("upper"))
    err4 = melt(high_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("lower"))
    
    lt = cbind(xtreme_mnth_datamlt[,-2],err1$value,err2$value)
    lt$Danger = "Extreme"
    st = cbind(xtreme_mnth_datamlt2[,-2],err3$value,err4$value)
    st$Danger = "High"
    colnames(st)=colnames(lt)
    
    D = rbind(lt,st)
    colnames(D) = c("Month","value","upper","lower","Rating")
    
  } else if(nrow(xtreme_mnth_data) > 0) {
    # Only extreme data exists
    xtreme_mnth_datamlt = melt(xtreme_mnth_data, id.vars=c("MonthAbb"),
                               measure.vars=c("aveM$avecnt"))
    err1 = melt(xtreme_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("upper"))
    err2 = melt(xtreme_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("lower"))
    
    lt = cbind(xtreme_mnth_datamlt[,-2],err1$value,err2$value)
    lt$Danger = "Extreme"
    
    D = lt
    colnames(D) = c("Month","value","upper","lower","Rating")
    
  } else if(nrow(high_mnth_data) > 0) {
    # Only high data exists
    xtreme_mnth_datamlt2 = melt(high_mnth_data, id.vars=c("MonthAbb"),
                                measure.vars=c("aveM2$avecnt"))
    err3 = melt(high_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("upper"))
    err4 = melt(high_mnth_data, id.vars=c("MonthAbb"),
                measure.vars=c("lower"))
    
    st = cbind(xtreme_mnth_datamlt2[,-2],err3$value,err4$value)
    st$Danger = "High"
    
    D = st
    colnames(D) = c("Month","value","upper","lower","Rating")
    
  } else {
    # No data at all - create empty dataframe and skip plotting
    warning("No extreme or high danger days found in the dataset")
    D = data.frame(Month=character(), value=numeric(), upper=numeric(), 
                   lower=numeric(), Rating=character())
  }
  
  dta_final = D
  
  ##########################################################################################################
  # Set x based on which months have data
  x <- month_names[Months]
  ##########################################################################################################
  
  # Only create plots if there's data
  # ---- ensure all selected months appear on x-axis even if zero days ----
  all_months_df <- data.frame(
    Month  = factor(month_names[Months], levels = month_names[Months]),
    Rating = rep(c("Extreme", "High"), each = length(Months))
  )
  
  # If dta_final has data, join and fill zeros; otherwise build all-zero frame
  if (nrow(dta_final) > 0) {
    dta_final <- dta_final %>%
      mutate(Month = factor(Month, levels = month_names[Months]))
    
    dta_final <- all_months_df %>%
      left_join(dta_final, by = c("Month", "Rating")) %>%
      mutate(
        value = ifelse(is.na(value), 0, value),
        upper = ifelse(is.na(upper), 0, upper),
        lower = ifelse(is.na(lower), 0, lower)
      )
  } else {
    dta_final <- all_months_df %>%
      mutate(value = 0, upper = 0, lower = 0)
  }
  
  dta_final <- dta_final %>% arrange(Month)
  
  if(any(dta_final$value > 0)) { 
    #name files
    output<-paste0(in_weather,"DangerDays/",Station_name,"_DangerDays.jpg")
    
    gg = ggplot(dta_final, aes(x = Month, y = value, fill = Rating, label = Rating)) +
      geom_bar(stat = "identity",position=position_dodge2(preserve = "single")) +
      geom_text(aes(label = round(value,1)),position = position_dodge(1), 
                check_overlap = TRUE, size = 8, vjust = -1.0) +
      geom_errorbar(data=dta_final,aes(x= Month,ymin = lower, ymax = upper),colour="black",
                    width = 0.2, show.legend = F,position = position_dodge(width = 0.9) ) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "Month", y = "Average Number of Days") +
      coord_cartesian(ylim=c(0,18)) +
      theme(axis.title=element_text(size=20),axis.text.x = element_text(size = 20), 
            axis.text.y = element_text(size = 20)) + 
      theme(legend.title=element_blank()) +
      theme(legend.position = c(0.15,0.9)) +
      theme(legend.text=element_text(size = 20))
    
    final_gg<-gg + ggtitle(paste0(Station_label," Weather Station")) +
      theme(plot.title=element_text(hjust=0.5,size = 20, vjust=1.0, face='bold')) +
      theme(axis.text.x = element_text(angle = 45,size = 20, hjust = 1))
    
    gg_save<-paste0(path,in_weather,"DangerDays/",Station_name,"_DangerDays.jpg")
    safe_plot_save("Danger Days", {
      ggsave(gg_save, plot = final_gg)
    })
  } else {
    cat("No danger days to plot for", Station_name, "\n")
  }

#---------------------------------------------------------------  
#Create Fire Weather List:----------------------------------------------
  weather_stations<-list.files(paste0(path,out_weather,"processed/"),pattern = "*_Daily_FWI.csv", full.names = TRUE)
  Station_names <- weather_stations %>%
    basename() %>%               # Extract just the file names
    sub("_Daily_FWI\\.csv$", "", .)
  
  fire_weather_lists<-list()
  dated_data<-list()
  percentile_levels <- seq(0.9, 1, 0.01)
  
  #STATION MATRIX: mapping to Weather Zones: add zone based on where station is
  dated_data<-list()
  fire_weather_lists<-list()
  
  for(i in 1:length(weather_stations)){
    name<-Station_names[i]
    Weather_Zone<- wthr_zone
    NDT_Zone<-ndt
    
    fwi<-read.csv(weather_stations[i])
    final2<-fwi
    final2$wxzone<-Weather_Zone
    final2$ndt<-NDT_Zone
    
    #summer
    summer_data<- final2
    TEMP = quantile(summer_data$TEMP,percentile_levels)
    WS = quantile(summer_data$WS,percentile_levels)
    WD = mean(summer_data$WD,na.rm=T)
    # Invert the scale of RH (so higher values become lower and vice versa)
    inverted_RH <- 100 - summer_data$RH
    percentiles_inverted_RH <- quantile(inverted_RH, c(0.90, 0.91, 0.92,0.93,0.94,0.95,0.96, 0.97, 0.98, 0.99, 1))
    RH <- 100 - percentiles_inverted_RH
    PREC = quantile(summer_data$PREC,percentile_levels)
    FFMC = quantile(summer_data$FFMC,percentile_levels)
    DMC = quantile(summer_data$DMC,percentile_levels)
    DC= quantile(summer_data$DC,percentile_levels)
    ISI=quantile(summer_data$ISI,percentile_levels)
    BUI = quantile(summer_data$BUI,percentile_levels)
    FWI = quantile(summer_data$FWI,percentile_levels)  
    summer_extreme = cbind(percentile_levels,TEMP,WS,WD,RH,PREC,FFMC,DMC,DC,ISI,BUI,FWI)
    summer_extreme<- as.data.frame(summer_extreme)
    
    
    
    #make cutoff values per index per season
    indices<- c("FWI","ISI","BUI","FFMC")
    Extreme_cutoff_df<-data.frame(
      Index=NA,
      Summer=NA
    )
    
    for(index in indices){
      summer_val<-summer_extreme %>% filter(percentile_levels == 0.90) %>% pull(index)
      #
      df<-data.frame(
        Index=index,
        Summer=summer_val
      )
      #
      Extreme_cutoff_df<-rbind(Extreme_cutoff_df,df)
    }
    Extreme_cutoff_df<-Extreme_cutoff_df[-1,]
    
    #filter data
    summer_data <- final2 %>% 
      filter(
        FWI > Extreme_cutoff_df$Summer[1] |
          BUI > Extreme_cutoff_df$Summer[3] |
          ISI > Extreme_cutoff_df$Summer[2] |
          FFMC > Extreme_cutoff_df$Summer[4])
    
    summer_data$YR<-rep(2,nrow(summer_data))
    
    all_data<-summer_data
    #
    csv<-all_data
    all_data$SZN<-ifelse(all_data$MON<6, "Spring",
                         ifelse(all_data$MON>=6 & all_data$MON <=7,"Summer","Fall"))
    csv<- csv %>% dplyr::select(DATE,DAY,YR,TEMP,RH,WS,WD,PREC,FFMC,DMC,DC,ISI,BUI,FWI,MON)
    colnames(csv) = c("Date","wx_zone","season", "temp",	"rh",	"ws",
                      "wd",	"prec",	"ffmc",	"dmc",	"dc",	"isi",	"bui",	"fwi","mon")
    csv$season[which(csv$mon < 7 )] = 1
    csv$season[which(csv$mon == 7 | csv$mon == 8 )] = 2
    csv$season[which(csv$mon > 8 )] = 3
    csv=csv[order(csv$Date),]
    csv = csv[,-c(1,15)]
    csv$ffmc[csv$ffmc==0] = 0.1
    
    csv$wx_zone <- Weather_Zone
    #can add NDT if need be
    csv$ndt <- NDT_Zone
    
    # Convert all columns to numeric
    csv <- csv %>%
      mutate_all(as.numeric)
    
    #add to list
    dated_data[[i]]<-all_data
    fire_weather_lists[[i]]<-csv
  }
  
  #Combine all data frames in the list into one data frame
  master_FWL <- bind_rows(fire_weather_lists)
  master_dated_data<-bind_rows(dated_data)
  #export
  write.csv(master_dated_data,paste0(path,out_weather,"WeatherLists/allstations_90th_FWList_dates_summer.csv"),row.names = F)
  write.csv(master_FWL,paste0(path,out_weather,"WeatherLists/allstations_90th_FWList_summer.csv"),row.names = F)
#-------------------------------------------------------------------

#Weather Conditions plot:---------------------------------------------
  station<-paste0(WthrName,"(",WthrType,")")
  AOI<-project
  station_name<- WthrName
  #type="Regular" #"Extreme" or "Regular"
  
  #load data
  #daily_weather<-read.csv(paste0(path,in_weather,"Out/", station_name,"_Daily_FWI.csv"))
  #daily_weather<- read.csv(paste0(path,in_weather,"Out/",station_name,"_Daily_FWI_AllYear.csv"))
  daily_weather<- read.csv(paste0(path,in_weather,"WeatherLists/","allstations_90th_FWList_dates_summer.csv"))
  
  #get pertinent info
  min_yr <- as.integer(format(as.Date(min(daily_weather$TIME)), "%Y"))
  max_yr <- as.integer(format(as.Date(max(daily_weather$TIME)), "%Y"))
  LAT<-max(daily_weather$LAT)
  LONG<-max(daily_weather$LONG)
  
  weather_data<-daily_weather
  #Plot Weather Data Values
  #if(type == "Extreme"){
  #  weather_data<-daily_weather %>% filter(ISI > 7.5)
  #}else{
  #  weather_data<-daily_weather
  #}
  
  #Plot range of extreme data
  weather_to_plot<-c("TEMP","RH","WS", "FFMC","ISI","BUI","FWI","DMC", "DC")
  weather_long<-pivot_longer(weather_data,cols=weather_to_plot,names_to = "Variable",values_to = "Value")
  data<-weather_long %>% dplyr::select(Variable, Value)
  
  
  #Ridgeline plot with annotations
  data<-data %>% dplyr::group_by(Variable) %>% mutate(Range=paste0(range(Value)[1],"-",range(Value)[2]))
  data <- data %>%
    mutate(
      Variable_Names = case_when(
        Variable == "WS"   ~ "WindSpeed (km/hr)",
        Variable == "TEMP" ~ "Temperature (C)",
        Variable == "RH"   ~ "RelativeHumidity (%)",
        Variable == "ISI"  ~ "InitialSpreadIndex",
        Variable == "FWI"  ~ "FireWeatherIndex",
        Variable == "FFMC" ~ "FineFuelMoistureCode",
        Variable == "BUI"  ~ "BuildupIndex",
        Variable == "DMC"  ~ "DuffMoistureCode",
        Variable == "DC"  ~ "DroughtCode",
        TRUE                ~ Variable   
      )
    )
  
  means <- data %>% group_by(Variable) %>% summarise(mean(Value))
  medians <- data %>% group_by(Variable) %>% summarise(median(Value))
  
  bg_color <- "grey97"
  font_family <- "Fira Sans"
  
  #plot_subtitle = glue("Selected distributions of pertinent weather indices for",AOI,", BC based on crucial fire weather indices. Weather indices were selected for their role in driving fire behaviour and were clipped to represent the extreme ends of their full distribution from",min_yr,"to",max_yr,"during peak fire season months.")
  #if(type == "Extreme"){
  #  plot_subtitle = glue(paste0("Selected distributions of pertinent weather indices for ",AOI,",BC based on crucial fire weather indices. Weather indices were selected for their role in driving fire behaviour and were clipped to represent the extreme ends of their full distribution from ",min_yr," to ",max_yr," during peak fire season months."))
  #}else{
  #  plot_subtitle = glue(paste0("Selected distributions of pertinent weather indices for ",AOI,",BC based on crucial fire weather indices. Weather indices were selected for their role in driving fire behaviour and were analyzed for their full distribution from ",min_yr," to ",max_yr," during peak fire season months."))
  #}
  plot_subtitle = glue(paste0("Selected distributions of pertinent weather indices for ",AOI,",BC based on crucial fire weather indices. Weather indices were selected for their role in driving fire behaviour and were clipped to represent the extreme ends of their full distribution from ",min_yr," to ",max_yr," during peak fire season months."))
  
  
  
  #data<-data %>% filter(Variable != "BUI")
  n_vars <- data %>% distinct(Variable) %>% nrow()
  
  # create the dataframe for the legend (inside plot)
  df_for_legend <- data %>% 
    filter(Variable == "TEMP")
  extreme_pct <- data %>%
    dplyr::group_by(Variable_Names) %>%
    dplyr::summarise(
      extreme_val = ifelse(
        first(Variable_Names) == "RelativeHumidity (%)",
        quantile(Value, 0.10),
        quantile(Value, 0.90)
      )
    )
  
  data <- data %>% left_join(extreme_pct, by = "Variable_Names")
  
  p<-
    data %>%
    ggplot(aes(x = Variable_Names, y = Value)) +
    stat_halfeye(fill_type = "segments", alpha = 0.6, width=2.25) +
    stat_interval() +
    stat_summary(
      fun.min   = median,
      fun.max   = median,
      geom      = "crossbar",
      width     = 0.1,
      linetype  = "dashed",
      color     = "grey20"
    ) +
    stat_summary(
      geom      = "point",
      fun       = median,
      size      = 1.5,
      color     = "darkblue",
      shape     = 21,
      fill      = "darkblue"
    ) +
    stat_summary(
      aes(label = scales::number(..y.., accuracy = 0.1)),
      fun         = median,
      geom        = "text",
      angle  = 35,
      vjust       = -1,   # move text slightly above the point
      hjust      = 1,
      size        = 2.75,
      color       = "black"
    ) +
    stat_summary(
      aes(y = extreme_val),
      fun.min  = function(x) unique(x),
      fun.max  = function(x) unique(x),
      geom     = "crossbar",
      width    = 0.1,
      linetype = "dotted",
      color    = "darkred"
    ) +
    stat_summary(
      aes(y = extreme_val),
      geom  = "point",
      fun   = function(x) unique(x),
      size  = 1.5,
      color = "darkred",
      shape = 21,
      fill  = "darkred"
    ) +
    stat_summary(
      aes(y = extreme_val,
          label = scales::number(after_stat(y), accuracy = 0.1)),
      fun   = function(x) unique(x),
      geom  = "text",
      angle = 35,
      vjust = -0.45,
      hjust = -0.5,
      size  = 2.75,
      color = "darkred"
    ) +
    scale_color_manual(values = MetBrewer::met.brewer("Homer2")) +
    #scale_x_discrete(labels = toupper) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    coord_flip(clip = "off") +
    labs(
      title    = toupper("Distribution of Key Weather Indices for Fire Behavior"),
      subtitle = plot_subtitle,
      caption  = glue(
        "Data clipped to extreme values.<br>",
        #ifelse(type=="Extreme","Data clipped to extreme values.<br>","Data over full summer range.<br>"),
        paste0("Data: ",station," Weather Station, ", min_yr,"-",max_yr,".<br>"),
        paste0("Coords: ",LAT,",",LONG)
      ),
      x = NULL,
      y = "Value"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = NA, fill = bg_color),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
      panel.grid.minor.x = element_line(linewidth = 0.08, color = "grey75"),
      #plot.title = element_text(family = "Fira Sans SemiBold"),
      plot.title.position = "plot",
      plot.title = element_text(
        #margin = margin(t = 4, b = 16), 
        size = 12),
      plot.subtitle = element_text(
        #margin = margin(t = 4, b = 16), 
        size = 7),
      plot.caption = element_text(
        #margin = margin(t = 12), 
        size = 7
      ),
      plot.caption.position = "plot",
      axis.text.y = element_text(hjust = 0 
                                 #margin = margin(r = -10),
      )
      #,plot.margin = margin(4, 4, 4, 4)
    )+ theme(legend.position = "none")
  
  
  #edited legend
  p_legend <- ggplot() +
    
    # Blue dot — Median
    annotate("point", x = 0.10, y = 0.75, color = "darkblue", size = 2.5) +
    annotate("text",  x = 0.14, y = 0.75, label = "Median Value",
             hjust = 0, size = 2) +
    
    # Red dot — 90th Percentile
    annotate("point", x = 0.32, y = 0.75, color = "darkred", size = 2.5) +
    annotate("text",  x = 0.36, y = 0.75, label = "90th Percentile",
             hjust = 0, size = 2) +
    
    # Gold rect — 50%
    annotate("rect", xmin = 0.530, xmax = 0.570,
             ymin = 0.60, ymax = 0.90, fill = "gold", color = "black") +
    annotate("text", x = 0.58, y = 0.75, label = "50%",
             hjust = 0, size = 2) +
    
    # Orange + gold — 80%
    annotate("rect", xmin = 0.720, xmax = 0.790,
             ymin = 0.60, ymax = 0.90, fill = "orange", color = "black") +
    annotate("rect", xmin = 0.742, xmax = 0.768,
             ymin = 0.60, ymax = 0.90, fill = "gold",   color = "black") +
    annotate("text", x = 0.80, y = 0.75, label = "80%",
             hjust = 0, size = 2) +
    
    # Red + orange + gold — 95%
    annotate("rect", xmin = 0.920, xmax = 1.010,
             ymin = 0.60, ymax = 0.90, fill = "red",    color = "black") +
    annotate("rect", xmin = 0.942, xmax = 0.988,
             ymin = 0.60, ymax = 0.90, fill = "orange", color = "black") +
    annotate("rect", xmin = 0.958, xmax = 0.972,
             ymin = 0.60, ymax = 0.90, fill = "gold",   color = "black") +
    annotate("text", x = 1.02, y = 0.75, label = "95% of Values",
             hjust = 0, size = 2) +
    
    coord_cartesian(xlim = c(0, 1.25), ylim = c(0.5, 1.0), expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(color = "grey30", linewidth = 0.2, fill = "white")
    )
  
  
  
  #decide on legend 1 ior 2
  #for 1
  #Full_p<-p + inset_element(p_legend1, l = 0.6, r = 1,  t = 1, b = 0.5, clip = FALSE)
  #
  # The original script used patchwork::inset_element() to place a custom
  # legend beneath the figure. We avoid that dependency here and save the core
  # weather-distribution plot directly so Step 2 can run without patchwork.
  p <- p + theme(plot.margin = margin(6, 6, 20, 6))
  Full_p <- p
  Full_p
  
  safe_plot_save("Weather Conditions", {
    ggsave(paste0(path, in_weather, "WeatherConditions/", station_name, "_WeatherDistributions.jpg"),
           Full_p,
           width = 9,    # increase this for wider
           height = 5,    # adjust height as needed
           units = "in",
           dpi = 300)
  })  
  
  
#--------------------------------------------------------------------  
  step_dir <- file.path(cfg$runtime$intermediate_dir, "step2_fuelcalc")
  dir.create(step_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- list(
    project_name = project_name,
    raw_dir = cfg$runtime$raw_dir,
    weather_type = WthrType,
    weather_name = WthrName,
    weather_code = WthrCode,
    weather_lat = WthrLat,
    weather_long = WthrLong,
    danger_region = DangerRegion,
    wind_rose = file.path(path, "Weather", "WindRoses", paste0(WthrName, "_WindRoses.jpg")),
    danger_days = file.path(path, "Weather", "DangerDays", paste0(WthrName, "_DangerDays.jpg")),
    weather_conditions = file.path(path, "Weather", "WeatherConditions", paste0(WthrName, "_WeatherDistributions.jpg"))
  )
  saveRDS(summary, file.path(step_dir, "fuelcalc_to_firemodel_outputs.rds"))
  jsonlite::write_json(summary, file.path(step_dir, "fuelcalc_to_firemodel_summary.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
}
