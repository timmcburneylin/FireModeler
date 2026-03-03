# AUTO-GENERATED from R/generated/firemodel_purl.R
# Step 3: Fire modeling
source('R/common.R')
cat('
Step 3: Fire modeling
 starting...\n')

  
  # Write batch file
  batch_filename <- paste0(out_folder,"/Run_FuelCalc_TU_", tr_names[i], ".bat")
  writeLines(batch_content, batch_filename)
  
  cat("Created batch file:", batch_filename, "\n")
}



#' 
#'     #Click on Batch Files to Run them
#'     #in ~FuelCalc/Outputs/TU_[Strata Name] folders
#'   
#' #8.Run FuelCalc/BC
#'   
#'   Creates Plot File Folders
## -----------------------------------------------------------------------------
for(i in 1:length(unique(Snap_EX$Stratum))){
  tr<-unique(Snap_EX$Stratum)[i]
  treatment_folder <- paste0(out_fuelcalc,"Plot Files/",tr,"_Plots")
  
  # Create the directory if it doesn't already exist
  if (!dir.exists(treatment_folder)) {
    dir.create(treatment_folder, recursive = TRUE)
  }
}



#'   
#'   -Go to FuelCalc and use Files automatically created.
#'   
#' #8.Exporting Plot Data from FuelCalc
#' 
#'    -As you go through FuelCalc for each plot go to Report-> Plot CSV File -> Data Only and save the files as "Plot"{plot number}.csv in appropriate folder ~/FuelCalcBC/Outputs/Plot Files/TU{tu name}_Plots 
#'    -Run Batch Summary
#'    -Run slash code to generate slash per treatment
#'   
#'  8.1 Generating average Slash Loads by Stratum per meter squared
#' 
## -----------------------------------------------------------------------------
#SET NUMBER OF TREATMENTS
treatments<- unique(Snap_EX$Stratum)


for(x in 1:length(treatments)){
  tr<-treatments[x]
files = dir(paste0(out_fuelcalc,"Plot Files/",tr,"_Plots"), pattern = "*.csv",full.names = TRUE)

#   DOES NOT INCLUDE DUFF
df <- data.frame(Treatment = character(),ShrHer = numeric(),
                    LitLic = numeric(),HR1 = numeric(),
                    HR10=numeric(),HR100=numeric(),
                    HR1000=numeric(),
                    stringsAsFactors=FALSE)

for (i in 1:length(files)){
  csv = read.csv(files[i],header = F)
  
  csv[nrow(csv) + 1,]=csv[1,]
  csv$V1 <- as.character(csv$V1)
  csv$V1[5] = "Total Slash"
  csv$V1[4] = "Total Fuel"
  csv[2:3,2:14]=csv[2:3,5:17]
  csv= csv[,-c(2,20:33)]
  
  for (j in 2:18){
    csv[5,j] = sum(csv[2,j],csv[3,j],na.rm = T)
  }
  csv=csv[c(1,2,3,5,4),]
  csv$HR1 = numeric(5)
  csv$HR10 = numeric(5)
  csv$HR100 = numeric(5)
  csv$HR1000 = numeric(5)

  csv$V4 = (csv$V4)*.1
  csv$HR1 = (csv$V5)*.1
  #Change values to kg per m^3
  csv$HR10[1] = sum(csv$V6[1],csv$V7[1],na.rm = T) *.1
  csv$HR10[2] = sum(csv$V6[2],csv$V7[2],na.rm = T) *.1
  csv$HR10[3] = sum(csv$V6[3],csv$V7[3],na.rm = T) *.1
  csv$HR10[4] = sum(csv$V6[4],csv$V7[4],na.rm = T) *.1
  csv$HR10[5] = sum(csv$V6[5],csv$V7[5],na.rm = T) *.1
  
  csv$HR100[1] = sum(csv$V8[1],csv$V9[1],na.rm = T) *.1
  csv$HR100[2] = sum(csv$V8[2],csv$V9[2],na.rm = T) *.1
  csv$HR100[3] = sum(csv$V8[3],csv$V9[3],na.rm = T) *.1
  csv$HR100[4] = sum(csv$V8[4],csv$V9[4],na.rm = T) *.1
  csv$HR100[5] = sum(csv$V8[5],csv$V9[5],na.rm = T) *.1
  
  csv$HR1000[1] = sum(csv$V10[1],csv$V11[1],na.rm = T) *.1
  csv$HR1000[2] = sum(csv$V10[2],csv$V11[2],na.rm = T) *.1
  csv$HR1000[3] = sum(csv$V10[3],csv$V11[3],na.rm = T) *.1
  csv$HR1000[4] = sum(csv$V10[4],csv$V11[4],na.rm = T) *.1
  csv$HR1000[5] = sum(csv$V10[5],csv$V11[5],na.rm = T) *.1
  
  csv=csv[,-c(4:18)]
  colnames(csv) = c("Treatment","ShrHer","LitLic","HR1","HR10","HR100","HR1000")
  
  if (i == 1){
    df = csv
  }
  else if (i > 1 & i < length(files)){
    df = rbind(df,csv)
  } else {
    df = rbind(df,csv)
    
    shrhr_pre = mean(df$ShrHer[which(df$Treatment=="Pre-Treatment")],na.rm = T)
    shrhr_th = mean(df$ShrHer[which(df$Treatment=="Thinned")],na.rm = T)
    shrhr_prn = mean(df$ShrHer[which(df$Treatment=="Pruned")],na.rm = T)
    shrhr_slsh = mean(df$ShrHer[which(df$Treatment=="Total Slash")],na.rm = T)
    shrhr_tot = mean(df$ShrHer[which(df$Treatment=="Total Fuel")],na.rm = T)

    litlic_pre = mean(df$LitLic[which(df$Treatment=="Pre-Treatment")],na.rm = T)
    litlic_th = mean(df$LitLic[which(df$Treatment=="Thinned")],na.rm = T)
    litlic_prn = mean(df$LitLic[which(df$Treatment=="Pruned")],na.rm = T)
    litlic_slsh = mean(df$LitLic[which(df$Treatment=="Total Slash")],na.rm = T)
    litlic_tot = mean(df$LitLic[which(df$Treatment=="Total Fuel")],na.rm = T)

    hr1_pre = mean(df$HR1[which(df$Treatment=="Pre-Treatment")],na.rm = T)
    hr1_th = mean(df$HR1[which(df$Treatment=="Thinned")],na.rm = T)
    hr1_prn = mean(df$HR1[which(df$Treatment=="Pruned")],na.rm = T)
    hr1_slsh = mean(df$HR1[which(df$Treatment=="Total Slash")],na.rm = T)
    hr1_tot = mean(df$HR1[which(df$Treatment=="Total Fuel")],na.rm = T)

    hr10_pre = mean(df$HR10[which(df$Treatment=="Pre-Treatment")],na.rm = T)
    hr10_th = mean(df$HR10[which(df$Treatment=="Thinned")],na.rm = T)
    hr10_prn = mean(df$HR10[which(df$Treatment=="Pruned")],na.rm = T)
    hr10_slsh = mean(df$HR10[which(df$Treatment=="Total Slash")],na.rm = T)
    hr10_tot = mean(df$HR10[which(df$Treatment=="Total Fuel")],na.rm = T)

    hr100_pre = mean(df$HR100[which(df$Treatment=="Pre-Treatment")],na.rm = T)
    hr100_th = mean(df$HR100[which(df$Treatment=="Thinned")],na.rm = T)
    hr100_prn = mean(df$HR100[which(df$Treatment=="Pruned")],na.rm = T)
    hr100_slsh = mean(df$HR100[which(df$Treatment=="Total Slash")],na.rm = T)
    hr100_tot = mean(df$HR100[which(df$Treatment=="Total Fuel")],na.rm = T)

    hr1000_pre = mean(df$HR1000[which(df$Treatment=="Pre-Treatment")],na.rm = T)
    hr1000_th = mean(df$HR1000[which(df$Treatment=="Thinned")],na.rm = T)
    hr1000_prn = mean(df$HR1000[which(df$Treatment=="Pruned")],na.rm = T)
    hr1000_slsh = mean(df$HR1000[which(df$Treatment=="Total Slash")],na.rm = T)
    hr1000_tot = mean(df$HR1000[which(df$Treatment=="Total Fuel")],na.rm = T)

    df=df[1:5,1:7]

    df[1:5,2:7] = c(shrhr_pre,shrhr_th,shrhr_prn,shrhr_slsh,shrhr_tot,litlic_pre,litlic_th,litlic_prn,litlic_slsh,litlic_tot,hr1_pre,hr1_th,hr1_prn,
                    hr1_slsh,hr1_tot,hr10_pre,hr10_th,hr10_prn,hr10_slsh,hr10_tot,hr100_pre,hr100_th,hr100_prn,hr100_slsh,hr100_tot,hr1000_pre,
                    hr1000_th,hr1000_prn,hr1000_slsh,hr1000_tot)
  }
}
write.csv(df,paste0(out_slash,"Residuals/",tr,".csv"),row.names = FALSE)
}

#' 
#'   8. 2 Select each plot file and export as a csv called "Plot"#".csv", then run stand code to generate structure per treatment
#'     @set prune height
## -----------------------------------------------------------------------------
prune = c(3,3)        #<---------------------Set Pruning height

#SET NUMBER OF TREATMENTS
treatments<- unique(Snap_EX$Stratum)
#
df <- data.frame(Treatment = character(),Plot=numeric(),Pre_CBH = numeric(),Post_CBH = numeric(),
                    Pre_CFL = numeric(),Post_CFL=numeric(),Pre_CBD = numeric(),Post_CBD = numeric(),Pre_CC=numeric(),Post_CC=numeric(),
                    stringsAsFactors=FALSE)
df_lists<-list()

for(x in 1:length(treatments)){
  tr<-treatments[x]
files = dir(paste0(out_fuelcalc,"Plot Files/",tr,"_Plots"), pattern = "*.csv",full.names = TRUE)

#   DOES NOT INCLUDE DUFF
df_tr <- data.frame(Treatment = character(),Plot=numeric(),Pre_CBH = numeric(),Post_CBH = numeric(),
                    Pre_CFL = numeric(),Post_CFL=numeric(),Pre_CBD = numeric(),Post_CBD = numeric(),Pre_CC=numeric(),Post_CC=numeric(),
                    stringsAsFactors=FALSE)

for (i in 1:length(files)){
  csv = read.csv(files[i],header = F)
  plot<-substr(basename(files[i]),5,7)
  #extract cfl
  cfl_pre<-ifelse(csv$V22[1] < 0, csv$V22[1]*-1,csv$V22[1])
  cfl_post<-ifelse(csv$V22[4] < 0, csv$V22[4]*-1,csv$V22[4])
  
  #extract cbd
  cbd_pre<-ifelse(csv$V23[1] < 0, csv$V23[1]*-1,csv$V23[1])
  cbd_post<-ifelse(csv$V23[4] < 0, csv$V23[4]*-1,csv$V23[4])
  
  #extract cbh
  cbh_pre<-ifelse(csv$V24[1] < 0, csv$V24[1]*-1,csv$V24[1])
  cbh_post<-ifelse(csv$V24[4] < 0, csv$V24[4]*-1,csv$V24[4])
  
  #extract cc
  cc_pre<-ifelse(csv$V28[1] < 0, csv$V28[1]*-1,csv$V28[1])
  cc_post<-ifelse(csv$V28[4] < 0, csv$V28[4]*-1,csv$V28[4])
  
  #Extract TPH
  TPH_pre<-ifelse(csv$V26[1] < 0, csv$V26[1]*-1,csv$V26[1])
  TPH_post<-ifelse(csv$V26[4] < 0, csv$V26[4]*-1,csv$V26[4])
  
  #average them
  df_plot<-data.frame(
    Treatment=tr,
  Plot=plot,
  Pre_CBH=cbh_pre,
  Post_CBH=ifelse(cbh_post== 0,prune[x],cbh_post),
  Pre_CFL=cfl_pre/10, #change from tons per ha to kg per m^2
  Post_CFL=cfl_post/10, #change from tons per ha to kg per m^2
  Pre_CBD=cbd_pre,
  Post_CBD=cbd_post,
  Pre_CC=cc_pre,
  Post_CC=cc_post,
  Pre_TPH=TPH_pre,
  Post_TPH=TPH_post
  )
  #add to data for one treatment level frame
  df_tr<-rbind(df_tr,df_plot)
  
  #add to data for all plots one frame
  df<-rbind(df,df_plot)
  
}
#
last_row<-c(tr,"Mean All",mean(df_tr$Pre_CBH),mean(df_tr$Post_CBH),mean(df_tr$Pre_CFL),mean(df_tr$Post_CFL),mean(df_tr$Pre_CBD),mean(df_tr$Post_CBD), mean(df_tr$Pre_CC),mean(df_tr$Post_CC),mean(df_tr$Pre_TPH),mean(df_tr$Post_TPH))
df_tr<-rbind(df_tr,last_row)

write.csv(df_tr,paste0(out_fuelcalc,tr,"_StandStructure.csv"),row.names = FALSE)

df_lists[[as.character(tr)]]<-df_tr

}

#make large average data frame
df_averaged<- df %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarize(
    Pre_CBH=mean(Pre_CBH),
    Post_CBH=mean(Post_CBH),
    Pre_CFL=mean(Pre_CFL),
    Post_CFL=mean(Post_CFL),
    Pre_CBD=mean(Pre_CBD),
    Post_CBD=mean(Post_CBD),
    Pre_CC=mean(Pre_CC),
    Post_CC=mean(Post_CC),
    Pre_TPH=mean(Pre_TPH),
    Post_TPH=mean(Post_TPH)
  )


write.csv(df_averaged,paste0(out_fuelcalc,"All_Treatments_StandStructure.csv"),row.names = FALSE)

#' 
#' 
#' #9. Process Weather Data
#' 
#'   Pre-Processing: set name and station code
#'     Navigate to: https://services.pacificclimate.org/met-data-portal-pcds/app/
#'     Find your FLNRO Station and click on it: read the name, lat, lon, and code
#'     Change them below
#'     or go here
#'     https://www.arcgis.com/apps/webappviewer/index.html?id=c36baf74b74a46978cf517579a9ee332
#'     or here
#'     https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/2024/
#'     
## -----------------------------------------------------------------------------
#CHECK WEATHER STATION DATA FROM CLIMATE IMPACTS CONSORTIUM (CLICK ON POINT AND INFO WILL BE GIVEN)
lat = 55.0274
long = -120.9341
station<- "TUMBLER(DENISON)"
code<- 127
name<-"TR_LionsBurn"

#Set it:
Ecodivision= "BOREAL"

# Specify the URL and download the specific station
  #url <- paste0('https://data.pacificclimate.org/data/pcds/lister/raw/FLNRO-WWB/',code,'.rsql.csv')
  data<-read.csv(paste0(path,out_weather,"/FLNRO-WMB/",code,".csv"),header=FALSE)

  # Download and read the CSV file into R
#data <- read.csv(url,header=FALSE)
names<-as.vector(data[2,1:ncol(data)])
colnames(data)<-names
data<-data[-1,]
final_data<-data[-1,]
write.csv(final_data,paste0(path,out_weather,"/FLNRO-WMB/",station,".csv"),row.names = FALSE)
#

#Import CSV Data
weather<- read.csv(paste0(path,out_weather,"/FLNRO-WMB/",station,".csv"),skip =  0,header = TRUE)
df = weather

#   USE THESE TWO FUNCTIONS TO CHECK OUT THE DATA AND FIND OUT WHAT YOU NEED AND DON'T NEED
nms = colnames(df) 
nms

#select out columns you need
df2 = df %>%
  dplyr::select(wind_direction, X.precipitation, X.wind_speed, X.temperature, X.relative_humidity, X.time)
colnames(df2)<- c("wind_direction", "precipitation","wind_speed","temperature","rel_hum","time")
#create year,month, day columns
df2$Month <- as.numeric(substr(df2$time, 7, 8))
df2$Day = as.numeric(substr(df2$time, 10,11))
df2$Year = as.numeric(substr(df2$time,1,5))
df2$Hour = as.numeric(substr(df2$time,13,14))

#filter years
df2 = df2[df2$Year > 2009,]
df2$date<-format(as.Date(df2$time),"%Y-%m-%d")
write.csv(df2,paste0(path,out_weather,"Hourly_Weather_",station,".csv"),row.names = FALSE)


#' 
#'    Calculate Daily FWI:
## -----------------------------------------------------------------------------
csv2 <- df2 %>% dplyr::select(-date)
## TO CALCULATE DAILY
  csv2$lat = lat
  csv2$long = long
  csv2$weather_date = as.character(rep("0",NROW(csv2)))

  ## CALCULATE DAILY
  for (j in 1:length(csv2$weather_date)){
    csv2$weather_date[j] = paste0(csv2$Year[j],"/",csv2$Month[j],"/",csv2$Day[j])
  }
  csv2$weather_date = as.POSIXct(csv2$weather_date, format = "%Y/%m/%d", tz = "MST")
  
  csv2=csv2[order(csv2$weather_date),]

  # FOR FLNRO-WMB STATIONS TO MAKE SURE THERES AN OBSERVATION FOR PRECIP:
  csv2$precipitation = csv2$precipitation %>% replace(is.na(.),0)
  csv2$precipitation<- as.numeric(csv2$precipitation)

  prec = ddply(csv2, .(weather_date), summarize, prec = sum(precipitation,na.rm = T))
  
  csv2 = csv2[csv2$Hour == 12,]

  ## DAILY
  ## FLNRO-WMB STATION:
  colnames(csv2) = c("wd","prec","ws","temp","rh","time","mon","day","yr","hr","lat","long","Date")
  ## ENVIRONMENT CANADA STATION:
  # colnames(csv2) = c("ws","wd","rh","temp","time","prec","mon","day","yr","hr","lat","long","Date")
  
  csv2$temp=as.numeric(csv2$temp)
  csv2$rh=as.numeric(csv2$rh)
  csv2$ws=as.numeric(csv2$ws)
  csv2$wd=as.numeric(csv2$wd)
  
  csv2<- csv2[!is.na(csv2$temp),]
  csv2<- csv2[!is.na(csv2$rh),]
  csv2<- csv2[!is.na(csv2$ws),]
   csv2<- csv2[!is.na(csv2$wd),]

  # DONT KNOW IF NECCESARY FOR FLNRO-WMB?
  for (k in 1:NROW(csv2)){
    tempo = prec$prec[which(prec$weather_date == csv2$Date[k])]
    csv2$prec[k] = tempo
  }
   
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
    fwi = fwi(tmp,init=data.frame(ffmc=74,dmc=40,dc=300,lat=lat,long=long), batch=TRUE,out= "all",
              lat.adjust=TRUE,uppercase=TRUE)
    fwi[,c(14:length(fwi))]=round(fwi[,c(14:length(fwi))],2)
    mylyst2[[i]] = fwi
  }
 
  final2 = bind_rows(mylyst2, .id = "column_label")
  final2 = final2[-nrow(final2),]
  
  write.csv(final2,paste0(path,out_weather,name,"_Daily_FWI.csv"),row.names = F)
  
  
  #DEFAULT = ffmc=74,dmc=40,dc=300)
  for (i in 1:length(mylyst2_raw)){
    tmp = mylyst2_raw[[i]] 
    tmp<- tmp[!is.na(tmp$prec),]
    fwi = fwi(tmp,init=data.frame(ffmc=74,dmc=40,dc=300,lat=lat,long=long), batch=TRUE,out= "all",
              lat.adjust=TRUE,uppercase=TRUE)
    fwi[,c(14:length(fwi))]=round(fwi[,c(14:length(fwi))],2)
    mylyst2_raw[[i]] = fwi
  }
 
  final2_raw = bind_rows(mylyst2_raw, .id = "column_label")
  final2_raw = final2_raw[-nrow(final2_raw),]
  
  write.csv(final2_raw,paste0(path,out_weather,name,"_Daily_FWI_AllYear.csv"),row.names = F)
  
  
  
  
  ## DAILY
  p_temp2 = quantile(final2$TEMP,c(.9,.8,.7,.3,.2,.1))
  ave_temp2 = mean(final2$TEMP,na.rm=T)
  p_ws2 = quantile(final2$WS,c(.9,.8,.7,.3,.2,.1))
  ave_ws2 = mean(final2$WS,na.rm=T)
  ave_wd2 = mean(final2$WD,na.rm=T)
  p_wd2 = quantile(final2$WD,c(.9,.8,.7,.3,.2,.1),na.rm=T)
  p_rh2 = quantile(final2$RH,c(.1,.2,.3,.7,.8,.9))
  ave_rh2 = mean(final2$RH,na.rm=T)
  p_prec2 = quantile(final2$PREC,c(.9,.8,.7,.3,.2,.1))
  p_ffmc2 = quantile(final2$FFMC,c(.9,.8,.7,.3,.2,.1))
  ave_ffmc2 = mean(final2$FFMC,na.rm=T)
  p_dmc = quantile(final2$DMC,c(.9,.8,.7,.3,.2,.1))
  ave_dmc = mean(final2$DMC,na.rm=T)
  p_dc = quantile(final2$DC,c(.9,.8,.7,.3,.2,.1))
  ave_dc = mean(final2$DC,na.rm=T)
  p_isi2 = quantile(final2$ISI,c(.9,.8,.7,.3,.2,.1))
  ave_isi2 = mean(final2$ISI,na.rm=T)
  p_bui2 = quantile(final2$BUI,c(.9,.8,.7,.3,.2,.1))
  ave_bui2 = mean(final2$BUI,na.rm=T)
  p_fwi2 = quantile(final2$FWI,c(.9,.8,.7,.3,.2,.1))
  p_dsr2 = quantile(final2$DSR,c(.9,.8,.7,.3,.2,.1))
  ave_dsr2 = mean(final2$DSR,na.rm=T)
  
  percentiles2 = cbind(ave_temp2,p_temp2,ave_ws2,p_ws2,ave_wd2,p_wd2,ave_rh2,p_rh2,p_prec2,ave_ffmc2,p_ffmc2,
                      ave_dmc,p_dmc,ave_dc,p_dc,ave_isi2,p_isi2,ave_bui2,p_bui2,p_fwi2,p_dsr2,ave_dsr2)
  
  write.csv(percentiles2,paste0(path,out_weather,name,"_Daily_summary.csv"),row.names = T)
  
   ##Daily 90th 10 100th percentile
  percentile_levels <- seq(0.9, 1, 0.01)
 
  TEMP = quantile(final2$TEMP,percentile_levels)
  WS = quantile(final2$WS,percentile_levels)
  WD = mean(final2$WD,na.rm=T)
  # Invert the scale of RH (so higher values become lower and vice versa)
  inverted_RH <- 100 - final2$RH
  percentiles_inverted_RH <- quantile(inverted_RH, c(0.90, 0.91, 0.92,0.93,0.94,0.95,0.96, 0.97, 0.98, 0.99, 1))
  RH <- 100 - percentiles_inverted_RH
  PREC = quantile(final2$PREC,percentile_levels)
  FFMC = quantile(final2$FFMC,percentile_levels)
  DMC = quantile(final2$DMC,percentile_levels)
  DC= quantile(final2$DC,percentile_levels)
  ISI=quantile(final2$ISI,percentile_levels)
  BUI = quantile(final2$BUI,percentile_levels)
  FWI = quantile(final2$FWI,percentile_levels)
  DSR = quantile(final2$DSR, percentile_levels)
  
  upper_extreme = cbind(TEMP,WS,WD,RH,PREC,FFMC,DMC,DC,ISI,BUI,FWI,DSR)
  upper_extreme
  
  write.csv(upper_extreme,paste0(path,out_weather,name,"_90_100.csv"),row.names = T)

#'   
#'    Hourly FWI:
## -----------------------------------------------------------------------------
csv = df2 %>% dplyr::select(-date)
#TO CALCULATE HOURLY 
  csv$lat = lat
  csv$long = long
   csv$weather_date = as.character(rep("0",NROW(csv)))
  csv$weather_date2 = as.character(rep("0",NROW(csv)))

   for (j in 1:length(csv$weather_date)){
     csv$weather_date[j] = paste0(as.character(csv$Year[j]),"/",
                                  as.character(csv$Month[j]),"/",
                                  as.character(csv$Day[j]),"/",
                                  as.character(csv$Hour[j]))
     csv$weather_date2[j] = paste0(as.character(csv$Year[j]),"/",
                                  as.character(csv$Month[j]),"/",
                                  as.character(csv$Day[j]))
   }
   csv$weather_date = as.POSIXct(csv$weather_date, format = "%Y/%m/%d/%H", tz = "MST")
   csv$weather_date2 = as.POSIXct(csv$weather_date2, format = "%Y/%m/%d", tz = "MST")
  
 csv=csv[order(csv$weather_date),]
 
  
  ## HOURLY
   colnames(csv) = c("wd","prec","ws","temp","rh","time","mon","day","yr","hr","lat","long","Date","Date2")
   csv$temp=as.numeric(csv$temp)
   csv$rh=as.numeric(csv$rh)
   csv$ws=as.numeric(csv$ws)
   csv$prec=as.numeric(csv$prec)
  
   csv<- csv[!is.na(csv$temp),]
   csv<- csv[!is.na(csv$rh),]
   csv<- csv[!is.na(csv$ws),]
   csv<- csv[!is.na(csv$prec),]
  
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ##################################################################################  
  
  ## HORULY
  csv$timeDif = c(as.numeric(difftime(csv$Date[2:length(csv$Date)],
                                       csv$Date[1:(length(csv$Date)-1)],
                                       units="hours")),NA)
   csv$timeDif2 = c(as.numeric(difftime(csv$Date2[2:length(csv$Date2)],
                                       csv$Date2[1:(length(csv$Date2)-1)],
                                       units="days")),NA)
   csv=csv[order(csv$Date),]
  
  

  ##################################################################################################################
  ## HOURLY
   index = which(csv$timeDif>=3)    #  GAP THRESHOLD FOR RESETTING FWI CALCULATIONS, HOW MANY DAYS ARE ACCEPTABLE???
   if (length(index)< 1){index=NROW(csv)}

  
  
  # HOURLY
   mylyst=list()
   for (i in 1:length(index)){
     if (i == 1){mylyst[[paste0("df",as.character(index[i]))]] = csv[1:index[i],]}
     else {
      idx1 = index[i-1]+1
       mylyst[[paste0("df",as.character(index[i]))]] = csv[idx1:index[i],]
     }
    if (i == length(index)){
       idx2 = index[length(index)] + 1
       mylyst[["final"]] = csv[idx2:NROW(csv),]
     }
   }


  
  ##################################################################################
# HOURLY
  #DEFAULT = ffmc=74,dmc=40,dc=300)
   
   # HOURLY
  #DEFAULT = ffmc=74,dmc=40,dc=300)
   
   for (i in 1:length(mylyst)) {
  tmp = mylyst[[i]] 
  tmp$bui = rep(0, nrow(tmp))
  
  # Check if dates in tmp$Date2 exist in final2$DATE and filter tmp
  if (any(tmp$Date2 %in% final2$DATE)) {
    tmp = tmp[tmp$Date2 %in% final2$DATE,]
    
    for (j in 1:nrow(tmp)) {
      if (tmp$Date2[j] %in% final2$DATE) {
        tmp$bui[j] = final2$BUI[which(final2$DATE == tmp$Date2[j])]
      }
    }
    
    hourly = hffmc(tmp, ffmc_old = 74, time.step = 1, calc.step = TRUE, batch = TRUE, hourlyFWI = TRUE)
    hourly[, c(15:length(hourly))] = round(hourly[, c(15:length(hourly))], 2)
    mylyst[[i]] = hourly
  } else {
    next  # Skip to the next iteration if no dates match
  }
}

   
   final = bind_rows(mylyst, .id = "column_label")
   write.csv(final,paste0(path,out_weather,name,"_Hourly_FWI.csv"),row.names = F)
   #
   
    ## HOURLY
   p_temp = quantile(as.numeric(final$temp,c(.9,.8,.7,.3,.2,.1)))
   ave_temp = mean(as.numeric(final$temp),na.rm=T)
   p_ws = quantile(as.numeric(final$ws,c(.9,.8,.7,.3,.2,.1)))
   ave_ws = mean(as.numeric(final$ws),na.rm=T)
   p_wd = quantile(as.numeric(final$wd,c(.9,.8,.7,.3,.2,.1)))
   ave_wd = mean(as.numeric(final$wd),na.rm=T)
   p_rh = quantile(as.numeric(final$rh,c(.9,.8,.7,.3,.2,.1)))
   ave_rh = mean(as.numeric(final$rh),na.rm=T)
   p_prec = quantile(as.numeric(final$prec,c(.9,.8,.7,.3,.2,.1)))
   p_ffmc = quantile(as.numeric(final$ffmc,c(.9,.8,.7,.3,.2,.1)),na.rm=TRUE)
   ave_ffmc = mean(as.numeric(final$ffmc),na.rm=T)
   p_isi = quantile(as.numeric(final$isi,c(.9,.8,.7,.3,.2,.1)),na.rm=TRUE)
   ave_isi = mean(as.numeric(final$isi),na.rm=T)
   p_bui = quantile(as.numeric(final$bui,c(.9,.8,.7,.3,.2,.1)),na.rm=TRUE)
   ave_bui = mean(as.numeric(final$bui),na.rm=T)
   p_fwi = quantile(as.numeric(final$fwi,c(.9,.8,.7,.3,.2,.1)),na.rm=TRUE)
   p_dsr = quantile(as.numeric(final$dsr,c(.9,.8,.7,.3,.2,.1)),na.rm=TRUE)
   ave_dsr = mean(as.numeric(final$dsr),na.rm=T)
   
   percentiles = cbind(ave_temp,p_temp,ave_ws,p_ws,ave_wd,p_wd,ave_rh,p_rh,p_prec,ave_ffmc,p_ffmc,
                       ave_isi,p_isi,ave_bui,p_bui,p_fwi,p_dsr,ave_dsr)
   
   write.csv(percentiles,paste0(path,out_weather,name,"_Hourly_summary.csv"),row.names = T)
  

#'   
#'   Wind Roses and Domain Winds:
#'   @Must @Change @Station @Names + @Project
## -----------------------------------------------------------------------------
#read in multiple stations
Station_name<-"TUMBLER(Denison)"
project<- "TR_LionsBurn"

startmon<-2
endmon<-11
Months<-c(3,4,5,8,9,10)
##################################################
  station<-read.csv(paste0(path,out_weather,project,"_Daily_FWI_AllYear.csv"))
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

#jpeg(paste0(out_weather,name,"_WindRoses.jpg"),width = 1250,height = 950)
#pollutionRose(final[which(final$mon > 4 & final$mon < 10),],pollutant = "ws",type="MonthAbb",breaks = c(0,1,2,5,10,15,20,25,30),
 #                       key.footer = "(km per hr)",key.position = "right",par.settings=list(fontsize=list(text=14)))

# Define the title text
title_text <- paste(name, "Station Wind Roses")

# Specify the file path for saving the JPEG image
output_file <- paste0(path,out_weather, name, "_WindRoses_",startmon,"-",endmon,".jpg")

# Start plotting
jpeg(output_file, width = 1000, height = 750)
# Call plot.new() to start a new plot
plot.new()
# Plot wind rose without title
#pollutionRose(final[which(final$mon > startmon & final$mon < endmon), ],
 #             pollutant = "ws", type = "MonthAbb",
  #            breaks = c(0, 1, 2, 5, 10, 15, 20, 25, 30),
   #           key.footer = "(km per hr)", key.position = "right",
    #          par.settings = list(fontsize = list(text = 14)), border="black")
pollutionRose(final[which(final$mon %in% Months), ],
              pollutant = "ws", type = "MonthAbb",
              breaks = c(0, 1, 2, 5, 10, 15, 20, 25, 30),
              key.footer = "(km per hr)", key.position = "right", annotate=TRUE, par.settings = list(fontsize = list(text = 24)), 
              border = "black",
              cols = "jet")  # Add this line
# Add title
title(main = title_text, line = 2.5, cex.main = 2.0)

# End plotting
dev.off()
#
ave_s_N = mean(final$ws[which(final$wd >= 337 | final$wd <= 23)])
ave_s_NE = mean(final$ws[which(final$wd >= 24 & final$wd <= 68)])
ave_s_E = mean(final$ws[which(final$wd >= 69 & final$wd <= 113)])
ave_s_SE = mean(final$ws[which(final$wd >= 114 & final$wd <= 158)])
ave_s_S = mean(final$ws[which(final$wd >= 159 & final$wd <= 203)])
ave_s_SW = mean(final$ws[which(final$wd >= 204 & final$wd <= 248)])
ave_s_W = mean(final$ws[which(final$wd >= 249 & final$wd <= 293)])
ave_s_NW = mean(final$ws[which(final$wd >= 294 & final$wd <= 336)])
  
df = rbind(ave_s_N,ave_s_NE,ave_s_E,ave_s_SE,ave_s_S,ave_s_SW,ave_s_W,ave_s_NW)

write.csv(df,paste0(path,out_weather,name,project,"_domain_winds.csv"),row.names = T)


#' 
#'  Danger Days
#'   @Must @Change @Station @Names + @Project + @region
#'   check region:https://www.bclaws.gov.bc.ca/civix/document/id/complete/statreg/11_38_2005
## -----------------------------------------------------------------------------
#read in multiple stations
Station_name<-"TUMBLER(DENISON)"
project<- "TR_LionsBurn"
region<- 1

months = (4:9)
years = (2010:2025)
###################################################

  station<-read.csv(paste0(path,out_weather,project,"_Daily_FWI.csv"))
  name<-Station_name
  csv<-station
colnames(csv) = c("id", "wd","prec","ws","temp","rh","time","MON","DAY","YR","hr",
                     "lat","long","date","timedif","ffmc","dmc","dc","isi","BUI","FWI","DSR")


# 
csv = csv[csv$YR %in% years,]
csv = csv[csv$MON %in% months,]
csv$DGR = rep_len(numeric(0),nrow(csv))


# # BREAKS
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

csv1$OBJECTID = seq(1,nrow(csv1),by = 1)
csv2$OBJECTID = seq(1,nrow(csv2),1)

blank = data.frame("MON"=numeric(),"freq"=numeric(),"YR"=numeric())
blank2 = data.frame("MON"=numeric(),"freq"=numeric(),"YR"=numeric())


for (i in 1:length(unique(csv1$YR))){
  year = unique(csv1$YR)[i]
  temp = csv1[which(csv1$YR == unique(csv1$YR)[i]),]
  
  count_mnth = ddply(temp, .(MON), summarize, freq = length(unique((OBJECTID))))
  
  count_mnth$YR = rep(year,NROW(count_mnth))
  
  
  if (i == 1){
    blank = count_mnth
  } else {
    blank = rbind(blank,count_mnth)
  }
}
 
for(i in 1:length(unique(csv2$YR))){
  year = unique(csv2$YR)[i]
  temp = csv2[which(csv2$YR == unique(csv2$YR)[i]),]
  
  count_mnth = ddply(temp, .(MON), summarize, freq = length(unique((OBJECTID))))
  
  count_mnth$YR = rep(year,NROW(count_mnth))
  
  if (i == 1){
    blank2 = count_mnth
  } else {
    blank2 = rbind(blank2,count_mnth)
  }
}

aveM = ddply(blank, .(MON), summarize, avecnt = mean(freq))
aveM2 = ddply(blank2, .(MON), summarize, avecnt = mean(freq))

sdM = ddply(blank, .(MON), summarize, sdcnt = sd(freq))
sdM2 = ddply(blank2, .(MON), summarize, sdcnt = sd(freq))

pcntl_M = ddply(blank, .(MON), summarize, pcntlcnt = quantile(freq,c(.9)))
pcntl_M2 = ddply(blank2, .(MON), summarize, pcntlcnt = quantile(freq,c(.9)))

freq_m = ddply(blank, .(MON), summarize, freq = sum(freq))
freq_m2 = ddply(blank2, .(MON), summarize, freq = sum(freq))


NN = 30*length(unique(csv1$YR))
xtreme_mnth_data = cbind(freq_m,aveM$avecnt,sdM$sdcnt,pcntl_M$pcntlcnt,NN)
NN2 = 30*length(unique(csv2$YR))
high_mnth_data = cbind(freq_m2,aveM2$avecnt,sdM2$sdcnt,pcntl_M2$pcntlcnt,NN2)

  xtreme_mnth_data$lower = xtreme_mnth_data$`aveM$avecnt` - xtreme_mnth_data$`sdM$sdcnt` /sqrt(xtreme_mnth_data$NN)
  xtreme_mnth_data$upper = xtreme_mnth_data$`aveM$avecnt` + xtreme_mnth_data$`sdM$sdcnt` /sqrt(xtreme_mnth_data$NN)
  high_mnth_data$lower = high_mnth_data$`aveM2$avecnt` - high_mnth_data$`sdM2$sdcnt` /sqrt(high_mnth_data$NN2)
  high_mnth_data$upper = high_mnth_data$`aveM2$avecnt` + high_mnth_data$`sdM2$sdcnt` /sqrt(high_mnth_data$NN2)
  
  # Extract unique month indices
unique_xtreme_mnths <- unique(xtreme_mnth_data$MON)
unique_high_mnths <- unique(high_mnth_data$MON)

# Define a vector with month names
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Map month indices to month names
xtreme_mnths <- month_names[unique_xtreme_mnths]
high_mnths <- month_names[unique_high_mnths]

  ##########################################################################################################
  ##########################################################################################################
  xtreme_mnth_data$MonthAbb <- xtreme_mnths
  xtreme_mnth_data$MonthAbb = factor(xtreme_mnth_data$MonthAbb,levels = xtreme_mnths)
  high_mnth_data$MonthAbb <- high_mnths
  high_mnth_data$MonthAbb = factor(high_mnth_data$MonthAbb,levels = high_mnths)
  
  
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

  
  dta_final = D   #rbind(dta,dta2)
  
  ##########################################################################################################
  x <- high_mnths
  ##########################################################################################################
  
  dta_final = dta_final %>%
    mutate(Month =  factor(Month, levels = x)) %>%
    arrange(Month)
  
  #name files
  output<-paste0(out_weather,Station_name,"_",project,"_DangerDays.jpg")
    
  gg = ggplot(dta_final, aes(x = Month, y = value, fill = Rating, label = Rating)) +   #, fill = Record, label = Record
    geom_bar(stat = "identity",position=position_dodge2(preserve = "single")) +
    ##################   CAREFUL ABOUT ROUNDING     #############################
    geom_text(aes(label = round(value,1)),position = position_dodge(1), 
              check_overlap = TRUE, size = 8, vjust = -1.0) +
    geom_errorbar(data=dta_final,aes(x= Month,ymin = lower, ymax = upper),colour="black",
                  width = 0.2, show.legend = F,position = position_dodge(width = 0.9) ) +
    
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Month", y = "Average Number of Days") +        # "Average of Total Monthly Precipitation (mm)"     "Temperature (?C)"
    #coord_cartesian(ylim=c(min(dta_final$value)*0,12)) +
    coord_cartesian(ylim=c(0,18)) +
    theme(axis.title=element_text(size=20),axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 20)) + 
    theme(legend.title=element_blank()) +
    theme(legend.position = c(0.15,0.9))   +
    theme(legend.text=element_text(size = 20))
  
  final_gg<-gg + ggtitle(paste0(Station_name," Weather Station")) +
    theme(plot.title=element_text( hjust=0.5,size = 20, vjust=1.0, face='bold')) +
    theme(axis.text.x = element_text(angle = 45,size = 20, hjust = 1))
  
  #dev.off()
  
gg_save<-paste0(path,out_weather,Station_name,"_",project,"_DangerDays.jpg")
  ggsave(gg_save,plot=final_gg)


#' 
#' #10.Fire Weather list: 
#' 
#'   -create regular list and create 90th percentile list based on ISI, BUI, FWI, FFMC, DC, DMC, by season. 90TH Percentile index selection based on Wang et al. 2023
#' 
#'   -Creating 90th percentile fire weather list (seasonal mean) from multiple stations and merging with weather zones by season
## -----------------------------------------------------------------------------
wthr_zone<-3
ndt<-3

weather_stations<-list.files(paste0(path,out_weather),pattern = "*_Daily_FWI.csv", full.names = TRUE)
Station_names <- weather_stations %>%
  basename() %>%               # Extract just the file names
  str_remove("_Daily_FWI.csv")

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

#generate quantile ranges
  spring_data<- final2 %>% filter(MON < 7)
  TEMP = quantile(spring_data$TEMP,percentile_levels)
  WS = quantile(spring_data$WS,percentile_levels)
  WD = mean(spring_data$WD,na.rm=T)
  # Invert the scale of RH (so higher values become lower and vice versa)
  inverted_RH <- 100 - spring_data$RH
  percentiles_inverted_RH <- quantile(inverted_RH, c(0.90, 0.91, 0.92,0.93,0.94,0.95,0.96, 0.97, 0.98, 0.99, 1))
  RH <- 100 - percentiles_inverted_RH
  PREC = quantile(spring_data$PREC,percentile_levels)
  FFMC = quantile(spring_data$FFMC,percentile_levels)
  DMC = quantile(spring_data$DMC,percentile_levels)
  DC= quantile(spring_data$DC,percentile_levels)
  ISI=quantile(spring_data$ISI,percentile_levels)
  BUI = quantile(spring_data$BUI,percentile_levels)
  FWI = quantile(spring_data$FWI,percentile_levels)  
  spring_extreme = cbind(percentile_levels,TEMP,WS,WD,RH,PREC,FFMC,DMC,DC,ISI,BUI,FWI)
  spring_extreme<- as.data.frame(spring_extreme)

#summer
summer_data<- final2 %>% filter(MON == 7 | MON == 8)
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
  
#fall
fall_data<- final2 %>% filter(MON > 8)
  TEMP = quantile(fall_data$TEMP,percentile_levels)
  WS = quantile(fall_data$WS,percentile_levels)
  WD = mean(fall_data$WD,na.rm=T)
  # Invert the scale of RH (so higher values become lower and vice versa)
  inverted_RH <- 100 - fall_data$RH
  percentiles_inverted_RH <- quantile(inverted_RH, c(0.90, 0.91, 0.92,0.93,0.94,0.95,0.96, 0.97, 0.98, 0.99, 1))
  RH <- 100 - percentiles_inverted_RH
  PREC = quantile(fall_data$PREC,percentile_levels)
  FFMC = quantile(fall_data$FFMC,percentile_levels)
  DMC = quantile(fall_data$DMC,percentile_levels)
  DC= quantile(fall_data$DC,percentile_levels)
  ISI=quantile(fall_data$ISI,percentile_levels)
  BUI = quantile(fall_data$BUI,percentile_levels)
  FWI = quantile(fall_data$FWI,percentile_levels)  
  fall_extreme = cbind(percentile_levels,TEMP,WS,WD,RH,PREC,FFMC,DMC,DC,ISI,BUI,FWI)
  fall_extreme<- as.data.frame(fall_extreme)

#make cutoff values per index per season
indices<- c("FWI","ISI","BUI","FFMC")
Extreme_cutoff_df<-data.frame(
  Index=NA,
    Spring=NA,
    Summer=NA,
    Fall=NA
)

for(index in indices){
spring_val<-spring_extreme %>% filter(percentile_levels == 0.90) %>% pull(index)
summer_val<-summer_extreme %>% filter(percentile_levels == 0.90) %>% pull(index)
fall_val<-fall_extreme %>% filter(percentile_levels == 0.90) %>% pull(index)
#
df<-data.frame(
  Index=index,
    Spring=spring_val,
    Summer=summer_val,
    Fall=fall_val
)
#
Extreme_cutoff_df<-rbind(Extreme_cutoff_df,df)
}
Extreme_cutoff_df<-Extreme_cutoff_df[-1,]

#filter data
spring_data <- final2 %>%
  filter(MON < 7) %>%  # Filter for Spring (MON < 7)
  filter(
    FWI > Extreme_cutoff_df$Spring[1] |
    BUI > Extreme_cutoff_df$Spring[3] |
    ISI > Extreme_cutoff_df$Spring[2] |
    FFMC > Extreme_cutoff_df$Spring[4])
summer_data <- final2 %>%
  filter(MON == 7 | MON==8) %>%  # Filter for Summer (MON = 7 or 8)
  filter(
    FWI > Extreme_cutoff_df$Summer[1] |
    BUI > Extreme_cutoff_df$Summer[3] |
    ISI > Extreme_cutoff_df$Summer[2] |
    FFMC > Extreme_cutoff_df$Summer[4])
fall_data <- final2 %>%
  filter(MON > 8) %>%  # Filter for Fall (MON > 8)
  filter(
    FWI > Extreme_cutoff_df$Fall[1] |
    BUI > Extreme_cutoff_df$Fall[3] |
    ISI > Extreme_cutoff_df$Fall[2] |
    FFMC > Extreme_cutoff_df$Fall[4])
#
spring_data$YR<-rep(1,nrow(spring_data))
summer_data$YR<-rep(2,nrow(summer_data))
fall_data$YR<-rep(3,nrow(fall_data))

all_data<-rbind(spring_data,summer_data,fall_data)

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

# Determine the number of rows in the data frame
spring_rows<- csv %>% filter(season == 1)
summer_rows<- csv %>% filter(season == 2)
fall_rows<- csv %>% filter(season == 3)

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
write.csv(master_dated_data,paste0(path,out_weather,"allstations_90th_FWList_dates.csv"),row.names = F)
write.csv(master_FWL,paste0(path,out_weather,"allstations_90th_FWList.csv"),row.names = F)

#check distirbution
master_FWL


#' 
#'    Creating 90th percentile fire weather list (summer mean) from multiple stations and merging with weather zones by season
## -----------------------------------------------------------------------------
wthr_zone<-3
ndt<-3

weather_stations<-list.files(paste0(path,out_weather),pattern = "*_Daily_FWI.csv", full.names = TRUE)
Station_names <- weather_stations %>%
  basename() %>%               # Extract just the file names
  str_remove("_Daily_FWI.csv")

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
write.csv(master_dated_data,paste0(path,out_weather,"allstations_90th_FWList_dates_summer.csv"),row.names = F)
write.csv(master_FWL,paste0(path,out_weather,"allstations_90th_FWList_summer.csv"),row.names = F)

#check distirbution
master_FWL


#' 
#' 
#' # 11.Grass Curing: percent cured for grass fuels and daily weather
#' -https://cwfis.cfs.nrcan.gc.ca/downloads/pcuring/
#' 
## -----------------------------------------------------------------------------
# Read data
station<-"TUMBLER(DENISON)"
AOI <- st_read(paste0(path,"/TR__LionsBurn_BOX.shp"))
FT <- raster::raster(paste0(path, Fuel_prefix, "FT_FBP.tif"))
#FT <- crop(FT, AOI)
weather_list <- read.csv(paste0(path, out_weather, "Hourly_Weather_",station,".csv"))
#weather_list <- read.csv(paste0(path,out_weather,"allstations_90th_FWList_dates_summer.csv"))
weather_list<- read.csv(paste0(path,out_weather,project,"_Daily_FWI_AllYear.csv"))

#run this if using all year fwi weather list
weather_list<- weather_list %>% dplyr::select(-column_label,-TIMEDIF)
colnames(weather_list)<-c("wind_direction","precipitation","wind_speed","temperature","rel_hum","time","mon","day","yr","hr","lat","long","date","FFMC","DMC","DC","ISI","BUI","FWI","DSR")


# Ensure date column is properly formatted
weather_list$date <- as.POSIXct(weather_list$date)
weather_list$Hour <- hour(weather_list$date)

# Step 1: Summarize daily weather (10am-4pm average)

daily_weather <- weather_list %>%
  #filter(Hour >= 10 & Hour <= 16) %>%  # Filter for 10am-4pm if using hourly data
  mutate(date_only = as.Date(date)) %>%
  dplyr::group_by(date_only) %>%
  dplyr::summarise(
    temp = mean(as.numeric(temperature), na.rm = TRUE),
    rh = mean(as.numeric(rel_hum), na.rm = TRUE),
    ws = mean(as.numeric(wind_speed), na.rm = TRUE),
    precip = sum(as.numeric(precipitation), na.rm = TRUE),
    wd=mean(as.numeric(wind_direction), na.rm = TRUE),
    .groups = 'drop'
  )


# Step 2: Create grass mask and include C7
FT_grass <- FT
FT_grass[!(FT_grass %in% c(2,3,7, 14, 15))] <- NA
FT_perc_grass <- FT_grass
FT_perc_grass[!is.na(FT_perc_grass)] <- 1
plot(FT)
# Aggregate to calculate grass percentage
FT_grass_aggregate <- terra::aggregate(rast(FT_perc_grass), fact = 20, fun = "sum")
FT_grass_percentage <- FT_grass_aggregate / 400

# Create grass mask from areas with more than 50% coverage
Grass_mask <- FT_grass_percentage
Grass_mask[Grass_mask < 0.5] <- NA
points_non_na <- as.points(Grass_mask, na.rm = TRUE)


# Initialize grass curing column
daily_weather$grass_curing <- NA

# Track progress by year
current_year <- NULL
year_start_time <- Sys.time()

for(i in 1:nrow(daily_weather)) {
  
  day <- daily_weather[i, ]
  
  # Format date for file lookup
  year <- format(day$date_only, "%Y")
  month <- format(day$date_only, "%m")
  day_num <- format(day$date_only, "%d")
  
  # Report progress when year changes
  if (is.null(current_year) || year != current_year) {
    if (!is.null(current_year)) {
      year_time <- difftime(Sys.time(), year_start_time, units = "secs")
      cat("Completed year", current_year, "in", round(year_time, 1), "seconds\n")
    }
    current_year <- year
    year_start_time <- Sys.time()
    cat("Processing year", year, "...\n")
  }
  
  # Adjust year for 2024 and 2010 (missing data years)
  year_adj <- ifelse(year == "2024", "23",
                     ifelse(year == "2010", "11",
                            substr(year, 3, 4)))
  
  date_string <- paste0(year_adj, month, day_num)
  
  # Construct file path
  file_path <- paste0("cfg$external$grass_curing_raster_dir %||% ""pc", 
                      date_string, ".tif")
  
  # Check if file exists
  if (!file.exists(file_path)) {
    daily_weather$grass_curing[i] <- NA
    next
  }
  
  # Try to read grass curing raster
  gc_raw <- tryCatch({
    rast(file_path)
  }, error = function(e) {
    return(NULL)
  })
  
  # If reading failed, set to NA and continue
  if (is.null(gc_raw)) {
    daily_weather$grass_curing[i] <- NA
    next
  }
  
  # Project grass points to gc_raw coordinate system
  grass_points_proj <- tryCatch({
    terra::project(points_non_na, gc_raw)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(grass_points_proj)) {
    daily_weather$grass_curing[i] <- NA
    next
  }
  
  # Extract grass curing values
  PC <- terra::extract(gc_raw, grass_points_proj)
  
  # Check if extraction was successful
  if (nrow(PC) == 0 || all(is.na(PC[, 2]))) {
    daily_weather$grass_curing[i] <- NA
    next
  }
  
  # Calculate mean grass curing
  PC_mean <- mean(PC[, 2], na.rm = TRUE)
  daily_weather$grass_curing[i] <- PC_mean
}


# Report final year
if (!is.null(current_year)) {
  year_time <- difftime(Sys.time(), year_start_time, units = "secs")
  cat("Completed year", current_year, "in", round(year_time, 1), "seconds\n")
}

daily_weather$wd<-daily_weather$wd/7
daily_weather$Date<-as.Date(daily_weather$date_only)
#daily_weather$Date<-as.POSIXct(daily_weather$date_only)

# Export results
write.csv(daily_weather, paste0(path, out_weather, "Daily_Weather_AllYear.csv"), 
          row.names = FALSE)

#merge with fwi
FWI<- read.csv(paste0(path,out_weather,project,"_Daily_FWI_AllYear.csv"))

FWI<-read.csv(paste0(path,out_weather,"allstations_90th_FWList_dates_summer.csv"))
FWI$Date<-as.Date(FWI$DATE)

#
FWI <- left_join(FWI, 
                 daily_weather %>% dplyr::select(Date, grass_curing), 
                 by = c("Date"))

write.csv(FWI, paste0(path, out_weather,"allstations_90th_FWList_dates_summer.csv"), 
          row.names = FALSE)
write.csv(FWI, paste0(path,out_weather,project,"_Daily_FWI_AllYear.csv"), 
          row.names = FALSE)

#' 
#' #12.Fuel/Forest Structure Datasets
#' 
#'   12.1 #Generate Strata Level Data sets, mean fuel loads, Heights, CBH, for use in  Preliminary CFIS, Rothermel, CFFRDS, for Pre and Post Treated Stands
#'   @Set: Pruning and Project Name
#'   
## -----------------------------------------------------------------------------
#generate weighted mean height of all layers by plot for US
#write project shorthand
project<- "TR_LionsBurn"
#set pruning
prune_vals<-c(3,3)

#extract species
strata<-unique(Snap_EX$Stratum)

coniferList <- c("Ba","Bl", "Bg","Bb","Cw", "Fd", "Hw","T","Pl", "Sx","Sb","Sw", "Lw", "Lt", "Pw","Yc", "Fdi", "Fdc", "Py")
nonConiferList <- c("Act","Acb","Ac","At","Ep","DP", "DU","Dead", "Dr")

#set up df
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
    CWD_Pieces_5m_ha=NA,
    CFL=NA,
    CBD=NA,
    CC=NA,
    Slope=NA,
    Aspect=NA,
    OS_CBH=NA,
    US_Centroid=NA,
    Grass_Loading=NA,
    Shrub_Loading=NA,
    Herb_Loading=NA
      )

PostTreat_df<-data.frame(
    Stratum=NA,
      Height_US=NA,
      OS_CBH=NA,
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
    CWD_Pieces_5m_ha=NA,
    CFL=NA,
    CBD=NA,
    CC=NA,
    Slope=NA,
    Aspect=NA,
    Modified_CBH=NA,
    US_Centroid=NA,
    Grass_Loading=NA,
    Shrub_Loading=NA,
    Herb_Loading=NA
      )

Stand_Structure<-read.csv(paste0(out_fuelcalc,"All_Treatments_StandStructure.csv"))

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
 
  #read in post treatment slash and stand data
  post_treat<- read.csv(paste0(out_residuals,st,".csv"))
  PT_stand<- read.csv(paste0(fuelcalc,st,"-Out.csv"))

  #decide on prune
  if(prune_vals[j]>0){
    pruneflag<-TRUE
    prune<-prune_vals[j]
  }else{
    pruneflag<-FALSE
  }
  prune<-prune_vals[j]
  
    #Modify CBH weighted by the proportionate coverage of under story trees and the proportionate coverage of over story trees both weighting the CBH's of overstory and undertory. Also modify overstory CBH by the proportionate decrease in CBD of post treatment
    #or calculate the change in canopy coverage from the proportionate decrease in CBD
    cbd_pre<- as.numeric(structure_data$Pre_CBD)
    cbd_post<- as.numeric(structure_data$Post_CBD)
    CBD_ratio<-cbd_pre/cbd_post
 #can also use CBH as post treatment cbh generated by fuelcalc based on canopy bulk density availability
    mean_cbh_PT_FC<- as.numeric(structure_data$Pre_CBH)
    mean_cbh_Post_FC<- as.numeric(structure_data$Post_CBH)
    
#check if post treatment CBH from fuelcalc is higher than pruning height, if not then reset it to prune height
 if(prune > mean_cbh_Post_FC){
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
  
#If overstory does not exist this code adjusts for that
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
  cuttingSpecs <- read.csv(file = paste0(path,s_s_prefix,table_folder,"/cuttingSpecs_",st,".csv"), row.names = NULL, stringsAsFactors = FALSE)
  OS_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/OS_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
  US_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
  US_Ht<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_Ht_CBH.csv"), row.names = NULL, stringsAsFactors = FALSE)

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
  setnames(US_SPH_melt, old = c("variable", "value"), new = c("Species", "SPH"))
  SPH_merge<- US_SPH_melt
  
  US_Cent_melt<- reshape2::melt(US_Cent, id = "DBH")
  setnames(US_Cent_melt, old = c("variable", "value"), new = c("Species", "Cen"))
  #remove columns where centroid is zero because no trees
  US_Cent_melt<-US_Cent_melt %>% filter(Cen > 0)
  #modify cutting specs
  cuttingSpecs <- subset(cuttingSpecs, select = -c(Stand.Layer))
  cuttingSpecs$DBH<- ifelse(cuttingSpecs$DBH.Class == "0-1.5",1,
    ifelse(cuttingSpecs$DBH.Class == "1.5-7.5",5,
                ifelse(cuttingSpecs$DBH.Class== "7.5-12.5",10,
                    ifelse(cuttingSpecs$DBH.Class == "12.5-17.5",15,
                      ifelse(cuttingSpecs$DBH.Class == "17.5 - 22.5", 20,
                             ifelse(cuttingSpecs$DBH.Class == "22.5 - 27.5", 25,
                                    ifelse(cuttingSpecs$DBH.Class == "27.5 - 35",31.25,
                                           ifelse(cuttingSpecs$DBH.Class == "35 - 45",40,
                                                  55))))))))
  #remove old dbh class column
  cuttingSpecs<-cuttingSpecs %>%
    dplyr::select(-DBH.Class)
  
  cuttingSpecs_melt <- reshape2::melt(cuttingSpecs, id = "DBH")
  setnames(cuttingSpecs_melt, old = c("DBH", "variable", "value"), new = c("DBH", "Species", "Cutting_Spec"))
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
  
  #mean height of all overstory trees weigthed by number of trees
   mean_Height<- OS_df %>%
     filter(is.na(Dead.)) %>%
    summarise(mean_weighted_height = weighted.mean(Total.Height..m., w = `TPH`, na.rm=TRUE)) %>%
      pull(mean_weighted_height)

   #mean cbh of all overstory trees weighted by number of trees for pre or post treatment
   mean_cbh<- OS_df %>%
       filter(is.na(Dead.)) %>%
    summarise(mean_CBH = weighted.mean(CBH..0.1m., w = `TPH`, na.rm=TRUE)) %>%
      pull(mean_CBH)


   #mean CBH of understory trees for pre-treatment
    mean_cbh_us<- US_df %>%
      filter(Layer != "Layer 4 (<1.3m)")  %>%
    summarise(mean_CBH = mean(CBH..0.1m., na.rm=TRUE)) %>%
      pull(mean_CBH)
    
    #mean dbh per plot  
    DBH<- OS_df %>%
      filter(is.na(Dead.)) %>%
    summarise(dbh = weighted.mean(DBH, w = `TPH`, na.rm=TRUE)) %>%
      pull(dbh)
    
  #calculate modified CBH to account for crown area change and CBD change post treatment
  table_folder<- paste0(st,"_tables")
  cuttingSpecs <- read.csv(file = paste0(path,s_s_prefix,table_folder,"/cuttingSpecs_",st,".csv"), row.names = NULL, stringsAsFactors = FALSE)
  OS_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/OS_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
  US_SPH<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_SPH.csv"), row.names = NULL, stringsAsFactors = FALSE)
  US_Ht<- read.csv(paste0(path,s_s_prefix,table_folder,"/US_Ht_CBH.csv"), row.names = NULL, stringsAsFactors = FALSE)

  
  #filter out total and Dead column, and remove total layer and layer 4 for crown area calculation
 OS_SPH<-OS_SPH %>%
    filter(DBH.Class != "Total") 
  # Check if 'Total' column exists and remove it
  if("Total" %in% names(OS_SPH)) {
  OS_SPH <- OS_SPH %>% dplyr::select(-Total)
  }
 
  #US_SPH <- US_SPH %>%
  #filter(Layer != "Layer 4 (<1.3m)")
  
  ##US_Ht<- US_Ht %>%
  #  filter(Layer != "Layer 4 (<1.3m)")
  
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
  US_df$Centroid<-US_df$CBH..0.1m.+ ((US_df$Height..0.1m.- US_df$CBH..0.1m.)/2)
  US_df$TPH<- US_df$X..of.Trees*200
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
  setnames(US_SPH_melt, old = c("variable", "value"), new = c("Species", "SPH"))
  OS_SPH_melt <- reshape2::melt(OS_SPH, id = "DBH")
  setnames(OS_SPH_melt, old = c("variable", "value"), new = c("Species", "SPH"))
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
  cuttingSpecs$DBH<- ifelse(cuttingSpecs$DBH.Class == "0-1.5",1,
    ifelse(cuttingSpecs$DBH.Class == "1.5-7.5",5,
                ifelse(cuttingSpecs$DBH.Class== "7.5-12.5",10,
                    ifelse(cuttingSpecs$DBH.Class == "12.5-17.5",15,
                      ifelse(cuttingSpecs$DBH.Class == "17.5 - 22.5", 20,
                             ifelse(cuttingSpecs$DBH.Class == "22.5 - 27.5", 25,
                                    ifelse(cuttingSpecs$DBH.Class == "27.5 - 35",31.25,
                                           ifelse(cuttingSpecs$DBH.Class == "35 - 45",40,
                                                  55))))))))
  #remove old dbh class column
  cuttingSpecs<-cuttingSpecs %>%
    dplyr::select(-DBH.Class)

  cuttingSpecs_melt <- reshape2::melt(cuttingSpecs, id = "DBH")
  setnames(cuttingSpecs_melt, old = c("DBH", "variable", "value"), new = c("DBH", "Species", "Cutting_Spec"))

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
  Total_CA<- sum(SPH_Results_PT$Crown_Area_Ha)
  Total_CA_pre<-sum(SPH_Results_PT$Crown_Area_Pre_Treatment)

  #proportion of remaining crown coverage of understory trees
  #calculate crown area occupied by understory trees after cutting
  US_CA <- SPH_Results_PT %>%
  filter(DBH <= 15) %>%
  summarise(US_CROWNAREA = sum(Crown_Area_Ha)) %>%
  pull(US_CROWNAREA)
  #crown area ratio of understory to total crown after cutting
  US_CA_ratio<- US_CA/Total_CA
    
  #calculate crown area occupied by understory trees before cutting
  US_CA_pre <- SPH_Results_PT %>%
  filter(DBH <= 15) %>%
  summarise(US_CrownA_pre = sum(Crown_Area_Pre_Treatment)) %>%
  pull(US_CrownA_pre)
  US_CA_ratio_pre<- US_CA_pre/Total_CA_pre
  
  #modify understory centroid by the proportion of area occupied by understory crowns for pre and post treatment
  Centroid_US_modified_pre<-Centroid_US_pre*US_CA_ratio_pre
  Centroid_US_modified_post<-Centroid_US_post*US_CA_ratio

  
  #proportion of remaining crown coverage of overstory trees
  OS_CA<- SPH_Results_PT %>%
  filter(DBH > 15) %>%
  summarise(US_CROWNAREA = sum(Crown_Area_Ha)) %>%
  pull(US_CROWNAREA)
  OS_CA_ratio<-OS_CA/Total_CA
  
  #calculate proportional change to canopy base height as a function of the decline in canopy coverage from the removal of overstory trees
  CA_OS_original<- SPH_Results_PT %>%
    filter(DBH > 15) %>%
  summarise(OS_CA_O = sum(Crown_Area_Pre_Treatment)) %>%
  pull(OS_CA_O)
  OS_CA_pre<-CA_OS_original/Total_CA_pre

  #calculate the change in canopy coverage from pre treatment over story to post-treatment over story, ie what reduction in canopy coverage did we induce for overstory alone
  OS_CA_change_ratio<- OS_CA/CA_OS_original

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
    
     
     aspect<- EX_df %>%
       summarise(mean_aspect= mean(Avg.Azimuth,na.rm=TRUE)) %>%
       pull(mean_aspect)
     
  #post treatment fuel loads:
    hr1_PT<-post_treat$HR1[5]
    hr10_PT<-post_treat$HR10[5]
    hr100_PT<-post_treat$HR100[5]
    hr1000_PT<-post_treat$HR1000[5]
    lit_PT<-post_treat$LitLic[5]
    
  #CFL CBD
    cfl_pre<- as.numeric(structure_data$Pre_CFL)
    cfl_post<-as.numeric(structure_data$Post_CFL)

  #BA, VOl
    BA_pre<- as.numeric(PT_stand$preBA[PT_stand$PlotID == "Average"])
    BA_post<- as.numeric(PT_stand$postBA[PT_stand$PlotID == "Average"])
    Vol_pre<- as.numeric(PT_stand$stdCubicM[PT_stand$PlotID == "Average"])+as.numeric(PT_stand$HarCubicM[PT_stand$PlotID == "Average"])
    Vol_post<-as.numeric(PT_stand$stdCubicM[PT_stand$PlotID == "Average"])
  
    #crown closure
  CC_fuels_df <- fuels_df %>%
    summarise(CC = mean(Crown.Closure.., na.rm=TRUE)) %>%
      pull(CC)
  pre_CC<-as.numeric(structure_data$Pre_CC)
  pre_cc<- mean(pre_CC,CC_fuels_df)
  post_CC<-as.numeric(structure_data$Post_CC)
  ##
  
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
  if(pruneflag == TRUE & prune > mean_cbh){
    mean_cbh_prune<-prune
  }else{
    mean_cbh_prune<-mean_cbh
  }
#
  if(pruneflag == TRUE & prune > CBH_post_modified){
    CBH_post_modified<-prune
  }else{
    CBH_post_modified<-CBH_post_modified
  }
  
  #trees per ha
  TPH_pre<- as.numeric(structure_data$Pre_TPH)
  TPH_post<- as.numeric(structure_data$Post_TPH)
  
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
    CWD_Pieces_5m_ha=CWD,
    CFL=cfl_pre,
    CBD=cbd_pre,
    CC=CC_fuels_df[1],
    Slope=slope,
    Aspect=aspect,
     #cbh here adjusts for fuelcalcs bulk density ratio when using the Sando-Wick method
     OS_CBH=mean_cbh,
    US_Centroid=Centroid_US_modified_pre,
    Grass_Loading=grass,
      Shrub_Loading=shrub,
      Herb_Loading=herb
    )
  colnames(Prestrata_df)<- colnames(PreTreat_df)
  
   Poststrata_df<- data.frame(
    Stratum=st,
    #modify this based on tree removal, ie if all under story is gone you can leave height at zero otherwise can use the mean height remaining
      Height_US=0,
      #cbh here is the mean measured CBH from the field for just over story: depending on whether overstory exists or not
      OS_CBH=mean_cbh_prune,
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
    CWD_Pieces_5m_ha=CWD,
      CFL=cfl_post,
      CBD=cbd_post,
      CC=CC_fuels_df[1],
      Slope=slope,
     Aspect=aspect,
    #cbh here adjusts for fuelcalcs bulk density ratio when using the Sando-Wick method and the crown area reductions
    Modified_CBH=CBH_post_modified,
    US_Centroid=Centroid_US_modified_post,
     Grass_Loading=grass,
      Shrub_Loading=shrub,
      Herb_Loading=herb
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
  #
  PostTreat_df$Duff_Depth_in<- PostTreat_df$Duff_depth*0.3937
  PostTreat_df$Duff_Load_t_a<-PostTreat_df$Duff_Depth_in*10*1

  
#Add prune column
PreTreat_df$Prune<- prune_vals
PostTreat_df$Prune<- prune_vals


#Export
write.csv(PreTreat_df, paste0(path,Fuel_prefix,"Pre_Treatment_Structure_Data.csv"),row.names = FALSE)
write.csv(PostTreat_df, paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"),row.names = FALSE)




#' 
#' 
#' #13.Fire Behavior Prediction
#'  ##This section reads in weather data conditions and predicts fire behavior outputs over a range of weather conditions to generate assessments of where tipping points may be rather then just selecting 90th percentile
#' 
#'   13.1 #Read in strata structure assessment data sets and weather data:
#'   Decide on:
#'   Fuel Types
#'   Forest Types
#'   Surface Fuel that would Carry Fire
#'   @must @change @files
#' 
## -----------------------------------------------------------------------------
#Bring in fuel structural/ forest attributes
name<-"TR_LionsBurn"
stationname<-"TUMBLER(DENISON)"
PreTreat_df<- read.csv(paste0(path,Fuel_prefix,"Pre_Treatment_Structure_Data.csv"))
PostTreat_df<-read.csv(paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"))

#bring in weather data: pick your weather list
  #daily_weather<- read.csv(paste0(path,out_weather,"MUSKWA_Daily_FWI_AllYear.csv"))
  #daily_weather<- read.csv(paste0(path,out_weather,"allstations_90th_FWList_summer.csv"))
  daily_weather<- read.csv(paste0(path,out_weather,"allstations_90th_FWList_dates_summer.csv"))
  hourly_weather<-read.csv(paste0(path,out_weather,"Hourly_Weather_",stationname,".csv"))

  #Date cleaning if need be
#daily_weather$Date <- as.Date(daily_weather$DATE, format = "%m/%d/%Y")
#daily_weather$Date <- format(daily_weather$Date, "%m-%d-%Y")
#write.csv(daily_weather,paste0(path,out_weather,"allstations_90th_FWList_dates_summer.csv"))


#strata order for dfs
order<-c("FTU-A","FTU-B")

PreTreat_df <- PreTreat_df %>%
  mutate(Stratum = factor(Stratum, levels = order)) %>%
  arrange(Stratum)

PostTreat_df <- PostTreat_df %>%
  mutate(Stratum = factor(Stratum, levels = order)) %>%
  arrange(Stratum)

#Add fuel types and forest types
  #Pre-Treat
  PreTreat_df$FT_Can<-c("M-1/2","M-1/2")

  #set forest types: "Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce", "Grass", "Shrub", "Slash"
  PreTreat_df$Forest_Type<- c("Mixed","Mixed")

  #Post Treat
  PostTreat_df$FT_Can<-c("M-1/2","M-1/2")

  #set forest types: "Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce", "Grass", "Shrub", "Slash"
  PostTreat_df$Forest_Type<- c("Mixed","Mixed")

#What type of fuel will carry a surface fire?: litter, b_litter (broadleaf-litter), shrub, grass, slash, moss
  #Pre-Treat
  PreTreat_df$SurfFuel<-c("grass","grass")

  #Post Treat
  PostTreat_df$SurfFuel<-c("grass","grass")
  
  
#Choose American NFFDRS Model
  #Load Custom Models: includes sage brush and PIPOJuniper Model
  CustomModels <- read.csv(file.path(root, "templates", "Modeling", "CustomFuelModels.csv"))
  rownames(CustomModels)<-CustomModels$Model
  
  #Pre-Treat:
  #Alaska Model:Coastal Boreal Transition Open White Spruce:
    #grass primary carrier of fire
  #TU1: if fern dominated
  #TU3 if grass dominated
  #GR3 
  #Grass and herbaceous fuelbed with aspen overstory and spruce, open sturcture
  #GR4: moderate load dry climate grass

  #Pre-Treat
  PreTreat_df$USModel<-c("GR4","GR4")

  
  #Post Treat: Closed Paper Birch Forest and Closed Quaking Aspen Forest
  #leaf litter and grass understory fire behavior
  #TU1:low load dry climate timber grass shrub
  #8:
  
  #Post Treat
  PostTreat_df$USModel<-c("TU1","TU1")

  
write.csv(PreTreat_df, paste0(path,Fuel_prefix,"Pre_Treatment_Structure_Data.csv"),row.names = FALSE)
write.csv(PostTreat_df, paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"),row.names = FALSE)



#' 
#'  
#'   13.2 Fire Behavior Over a range of conditions prior to Treatment:
#'   #Pre Treatment
## -----------------------------------------------------------------------------
source(file.path(root, "R_functions", "Residence_Time_Function_Nelson2003b.R"))
source(file.path(root, "R_functions", "Improved_CFIS_perrakis_function.R"))
source(file.path(root, "R_functions", "Crosswalk CAN to US Fuel Model Function.R"))

#Modifiables: Elevation, Intensity Value to use (Nelson, Byram, Rothermel), Low heat of combustion (Nelson or manual)
Elevation<-750
CustomFuels<-c(FALSE,FALSE)
IntensityFlag<-"Nelson"
HFlag<-"Manual"
WindGustMod<-TRUE
#Crown Fire Calculation Type: Wagner for Van Wagner 1993, SR for Scott and Reinhardt 2001, and Finney for Finney 1998
CrownFireType<-c("SR","SR") 
FSG.Mod.Flag<-c(FALSE,FALSE) #always false for pretreatment
#Change Model again if need be
#PreTreat_df$USModel<-"TU3"

#bring in fuel data
strata_data<-PreTreat_df

#Weather data: if you want you can sample
weather_list <- daily_weather
weather_list$Date <- ymd(weather_list$DATE)
#set.seed(123)

#weather_list <- daily_weather %>% slice_sample(n = 150)
# Initialize results
results_list <- list()

# Define the number of treatments and weather conditions
strata <- unique(strata_data$Stratum)

#Setup progress bar
total_iters <- nrow(weather_list) * length(strata)
pb <- progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total  = total_iters,
  clear  = FALSE,   # keep the bar on screen after completion
  width  = 60
)
#initialize
iter <- 0


suppressWarnings({

# Iterate over weather conditions
for (i in 1:nrow(weather_list)) {
  
  # Extract weather data for this iteration
  date <- as.Date(weather_list$Date[i], format="%m-%d-%Y")
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
    strata_input <- strata_data %>% filter(Stratum == strata[j])
    
    #Get Elevation and Foliar Moisture Content
    Elev <- Elevation
    FMC <- FMC_calc(Lat, Lon, ELEV = Elev, DATE = date)
    
    # Extract fuel information and change to Megagrams per hectare
    litter_Mg <- strata_input$Lit_kg * 10
    hr1_Mg <- strata_input$hr_1_kg * 10
    hr10_Mg <- strata_input$hr_10_kg * 10
    hr100_Mg <- strata_input$hr_100_kg * 10
    FB_depth_cm <- strata_input$FB_depth
    ffl_kg <- strata_input$Lit_kg + strata_input$hr_1_kg + strata_input$hr_10_kg + strata_input$hr_100_kg + strata_input$Grass_Loading + strata_input$Herb_Loading + strata_input$Shrub_Loading

    # Calculate Surface Fuel Consumption (SFC) from 2 ways:
    #Wotton 2007
    sfc_n <- sfc(fueltype = strata_input$FT_Can, dc = DC, ffmc = FFMC, ffl = ffl_kg, bui = BUI, depth = FB_depth_cm, dmc = DMC, bd = 0)
    
    GrassCuring<-weather_list$grass_curing[i]
    if(is.na(GrassCuring)){GrassCuring<-50}
    
    #Canadian fbp
    SFC_FBP<-surface_fuel_consumption(FUELTYPE = strata_input$FT_Can,
                                      FFMC=FFMC,
                                      BUI=BUI,
                                      PC= GrassCuring, GFL=strata_input$Grass_Loading)
    
    # Calculate Effective Fine Fuel Moisture (EFFM)
    Density<- Density<- ifelse(strata_input$CC< 45,"Light",
                   ifelse(strata_input$CC> 45| strata_input$CC< 60 ,"Moderate",
                          ifelse(strata_input$CC> 60 ,"Dense")))
    Season<- "Summer"
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
  #PreTreat_df$EFFM[j]<-effm$fm1hr
    
  #FSG:
  if(FSG.Mod.Flag[j]){
  FSG<-strata_input$FSG_Mod
  }else{  
  FSG<-strata_input$FSG
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
    
    
    #Find Best American Model select by surface fuel and forest type:
    #ForestType: "Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce", "Grass", "Shrub", "Slash"
    #SurfFuel: litter, b_litter (broadleaf-litter), shrub, grass, slash, moss
    #Density: < 45% = "Light", 45- 60% = "Moderate", 60% > = "Dense"
    
    
    # Prepare inputs for fire behavior models
    Plot_fuels <- fuelModels[36, 1:16]
    
    # Modify fuel loads D for dynamic then 1hr, 10hr, 100hr, 1000hr, and depth post treatment
    Plot_fuels$fuelModelType <- "D"
    Plot_fuels$loadLitter <- litter_Mg
    Plot_fuels$load1hr <- hr1_Mg
    Plot_fuels$load10hr <- hr10_Mg
    Plot_fuels$load100hr <- hr100_Mg
    Plot_fuels$fuelBedDepth <- FB_depth_cm
    
    if(!is.na(strata_input$USModel)){
    RothModel<-strata_input$USModel  
    }else{
    BestUSModel<-find_best_model(Plot_fuels,Density=Density, SurfFuelType=strata_input$SurfFuel, ForestType=strata_input$Forest_Type)
    RothModel<-BestUSModel$FuelModel
    }
    
    model_roth<-RothModel
    model<-model_roth
  
  #select model back from dataset: USE PRESET MODEL FUELS
    if(model_roth %in% rownames(fuelModels)){
        Plot_fuels<-fuelModels[model_roth,1:16]
    }else{
      print("Checking Custom Models: Model not in original 40 and 13")
      Plot_fuels<-CustomModels[model_roth,1:17]
      Plot_fuels<-Plot_fuels %>% dplyr::select(-Model)
    }
    #select model back from dataset: USE PRESET MODEL FUELS or dont reload your fuels
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
    fuel<-data.frame(litter=Plot_fuels$loadLitter,hr1=Plot_fuels$load1hr,hr10=Plot_fuels$load10hr,hr100=Plot_fuels$load100hr,depth=FB_depth_cm,CAN_model=strata_input$FT_Can)
   
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
      UseModel =TRUE,
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

# Run the fire behavior models (e.g., Rothermel, CFIS, CFIS 2.0, FBP, CFIM)
 
   #check if Nelson predicts a fire
    if(Plot_sfc$SFC > ffl_kg | Plot_sfc$SFC_FBP > ffl_kg){
      Plot_sfc<-data.frame(SFC = ffl_kg, SFC_FBP=ffl_kg, BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s, SFC_Nelson=Plot_sfc$BurningRate*(Nelson$Residence.Time.s))
      }
    
    
    #Scott and Reinhardt
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
    
    
    load<-Plot_fuels %>% dplyr::select(load1hr,load10hr,load100hr,loadLiveHerb,loadLiveWoody)
    sav<-Plot_fuels %>%dplyr::select(sav1hr,sav10hr,sav100hr,savLiveHerb,savLiveWoody)
    depth<-Plot_fuels$fuelBedDepth
    mxdead<-Plot_fuels %>% dplyr::select(mxDead)
    heat<-c(rep(H,3),19500,20000)#standard Heat of combustion
    #heat<-c(heat_df$H_N[2:4],19500,20000) #or calculated values from Albini equations
    moist<- cbind(effm$fm1hr,effm$fm10hr,effm$fm100hr,herb_live,woody_live)
    u1<-WS_mid
    slp<-Plot_stand$slope
    WindSpeed<-WS
    
    Rothermel_ros<-ros(modeltype = "D",
                       w=unlist(unname(as.vector(load))),
                       s=unlist(unname(as.vector(sav))),
                       delta=unlist(unname(as.vector(depth))),
                       mx.dead = unlist(unname(as.vector(mxdead))),
                        h=heat,
                       m=as.vector(moist),
                       u=as.vector(wind_adj*WindSpeed),
                       slope=as.vector(slp))
    
    #Check which rate of spread is greater and use that or use mean
   #ROS <- pmax(Rothermel_ros$`ROS [m/min]`, 
    #        ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`)
   ROS<-max(c(Rothermel_ros$`ROS [m/min]`, 
            ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`))
   
   #Extract Final SFC
   Plot_sfc$SFC<-ScottReinhardt$FuelConsumed_kg_m2
   
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
    
      #Nelson's equation ()
      I_B_NELSON<-0.85*H*(1 - CharFract)*p_f*strata_input$FB_depth*PackingRatio*ROS/60   
      #Byram's Intensity Equation      
      I_B_BYRAM<-H*(SFC)*ROS/60
      
      #Get Intensity
      Intensity<-ifelse(IntensityFlag == "Nelson",I_B_NELSON,
                        ifelse(IntensityFlag == "Byram",I_B_BYRAM,ScottReinhardt$fireBehavior$`Fireline Intensity [kW/m]`))
                        
      
    #FBP Function:
  FBP_df<-data.frame(
    FuelType=ifelse(strata_input$FT_Can == "D-1/2","D-1",ifelse(strata_input$FT_Can == "M-1/2","M-1",strata_input$FT_Can)),
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
  if("grass_curing" %in% colnames(weather_list)){
    FBP_df<-cbind(FBP_df,data.frame(cc=weather_list$grass_curing[i]))
  }

  FBP<-cffdrs::fbp(FBP_df,output = "ALL")
  FBP$FuelType<-strata_input$FT_Can
  FBP$Model_type<-"FBP"
  FBP$Type<- ifelse(FBP$FD == "C","active",
                             ifelse(FBP$FD == "S", "surface", "passive"))
  
    #Decide on FineFuelMoisture
    EFFM<-ffm1_Wotton
    #EFFM<-effm$fm1hr
  
  
    #CFIS Original
    CFIS<- firebehavioR::cfis(fsg = FSG, sfc = SFC, effm = EFFM, u10 = WS, cbd = Plot_crown$cbd, id = 1)
    CFIS$Model_type<- "CFIS"
    
    #calculate surface rate of spread with the Rothermel equation
    #CFIS$Surface_ROS<-ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`
    CFIS$Surface_ROS<-ROS
 
    #CFIS 2.0: Perrakis 2023 Logistic Crown + Cruz 2005 Spread
    CFIS_2.0 <- cfis_modified(fsg = FSG, sfc = SFC, effm = EFFM, u10 = WS, cbd = Plot_crown$cbd, id = 5, adjusted = TRUE)
  CFIS_2.0$Model_type<-"CFIS_2.0"
  #calculate surface rate of spread with the Rothermel equation 
  #CFIS_2.0$Surface_ROS<-ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`
  CFIS_2.0$Surface_ROS<-ROS
  #CFIM: Cruz 2005
  fuel<-data.frame(litter=litter_Mg/10,hr1=hr10_Mg/10,hr10=hr10_Mg/10,hr100=hr100_Mg/10,depth=strata_input$FB_depth,CAN_model=strata_input$FT_Can)
    
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
FireType<-ifelse(CFIS$type == "active" | CFIS$type == "passive","crown","surface")
CFC<-ifelse(FireType == "crown",CFC_Calc(CFIS$cROS,strata_input$CFL)$CFC,CFC_Calc( CFIS$Surface_ROS,strata_input$CFL)$CFC)


FireType_2.0<-ifelse(CFIS_2.0$type == "active" | CFIS_2.0$type == "passive","crown","surface")
CFC_2.0<-ifelse(FireType_2.0 == "crown",CFC_Calc(CFIS_2.0$cROS,strata_input$CFL)$CFC,CFC_Calc( CFIS_2.0$Surface_ROS,strata_input$CFL)$CFC)

 
#Calculte Intensity, Critical Intensity, WITH Byrams and Van Wagner's Equations:decide on Nelson or Byrams intensity
  CFIS$Intensity_Surface<-I_B_BYRAM
    CFIS$Intensity_Crown<-(CFIS$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS$cROS/60
    CFIS$Critical_Int<-0.001*(strata_input$FSG^1.5)*(460+25.9*FMC)^1.5
    CFIS$CFC<-CFC
    CFIS$SFC<-ScottReinhardt$FuelConsumed_kg_m2  # Surface fuel consumption
    R_0<-3/Plot_crown$cbd
    CAC<-CFIS$cROS/R_0
    CFIS$ROS <-ifelse(is.na(CFIS$cROS),CFIS$Surface_ROS, CFIS$cROS * exp(-CAC_2.0))
    CFIS$Intensity<-ifelse(is.na(CFIS$Intensity_Crown),CFIS$Intensity_Surface,(CFIS$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS$ROS/60)
  CFIS$Flame_Length<-0.0775*(CFIS$Intensity^0.46)
  CFIS$CrownScorch<-(4.4713*CFIS$Intensity^0.667)/(60-TEMP)

    CFIS_2.0$Intensity_Surface<-I_B_BYRAM
    CFIS_2.0$Intensity_Crown<-(CFIS_2.0$Intensity_Surface+(CFC_2.0 * 0.20482 * H * 0.430265))*11.349*CFIS_2.0$cROS/60
    CFIS_2.0$Critical_Int<-0.001*(strata_input$FSG^1.5)*(460+25.9*FMC)^1.5
    CFIS_2.0$CFC<-CFC_2.0
    CFIS_2.0$SFC<-ScottReinhardt$FuelConsumed_kg_m2  # Surface fuel consumption
    CAC_2.0<-CFIS_2.0$cROS/R_0
    CFIS_2.0$ROS <-ifelse(is.na(CFIS_2.0$cROS),CFIS_2.0$Surface_ROS, CFIS_2.0$cROS * exp(-CAC_2.0))
    CFIS_2.0$Intensity<-ifelse(is.na(CFIS_2.0$Intensity_Crown),CFIS_2.0$Intensity_Surface,(CFIS_2.0$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS_2.0$ROS/60)
    CFIS_2.0$Flame_Length<-0.0775*(CFIS_2.0$Intensity^0.46)
     CFIS_2.0$CrownScorch<-(4.4713*CFIS_2.0$Intensity^0.667)/(60-TEMP)
    
 
  
    # Store the results in a list
    results_list[[paste("Strata_", STRATA, "Weather", i, sep = "_")]] <- list(
      Stratum= STRATA,
      CANModel=strata_input$FT_Can,
      USModel=model_roth,
      ScottReinhardt = ScottReinhardt,
      CFIS = CFIS,
      CFIS_2.0=CFIS_2.0,
      #CFIM=cfim,
      FBP=FBP,
      Plot_sfc = Plot_sfc,
      Plot_fuels = Plot_fuels,
      Plot_moisture = Plot_moisture,
      Plot_crown = Plot_crown,
      Plot_stand = Plot_stand,
      Plot_nelson = Nelson
    )
    
    # Tick the progress bar:
      iter <- iter + 1
      pb$tick()
    
  }
}
})

saveRDS(results_list,paste0(path,Fire_out,"Results_PreTreatment.rds"))




#' 
#'   13.3 Fire Behavior Over a range of conditions after Treatment:
#'   #Post Treatment
#'   FTU-A:0.75
#'   FTU-B:0.75
#' 
## -----------------------------------------------------------------------------

#Set Post Treat Fuels and change model if need be:
#Post treatment surface fuel targets:
  PostTreat_df$Fine_Fuel_kg<-c(0.75,0.75)
  PostTreat_df$USModel<-c("TU1","TU1")
  
#Modifiables: Elevation, Intensity Value to use (Nelson, Byram, Rothermel), Low heat of combustion (Nelson or manual), FSG.Mod.Flag(TRUE used modified cbh or FALSE use original)
Elevation<-750
CustomFuels<-c(TRUE,TRUE)
IntensityFlag<-"Nelson"
HFlag<-"Manual"
FSG.Mod.Flag<-c(TRUE,TRUE)
WindGustMod<-TRUE
#Crown Fire Calculation Type: Wagner for Van Wagner 1993, SR for Scott and Reinhardt 2001, and Finney for Finney 1998
CrownFireType<-c("SR","SR") 


source(file.path(root, "R_functions", "Improved_CFIS_perrakis_function.R"))
source(file.path(root, "R_functions", "Crosswalk CAN to US Fuel Model Function.R"))
source(file.path(root, "R_functions", "CFIM_Cruz2006_Function.R"))

#bring in fuel data
strata_data<-PostTreat_df
PostTreat_df$USModel

#Weather data: filter if need be or sample
#weather_list<-daily_weather %>% dplyr::filter(MON %in% #c(4,5,6,7,8,9,10))
#Weather data: if you want you can sample
weather_list <- daily_weather
weather_list$Date <- ymd(weather_list$DATE)
#set.seed(123)
#set.seed(123)
#weather_list <- daily_weather %>% slice_sample(n = 150)

# Initialize results
results_list <- list()

# Define the number of treatments and weather conditions
strata <- unique(strata_data$Stratum)

#Setup progress bar
total_iters <- nrow(weather_list) * length(strata)
pb <- progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total  = total_iters,
  clear  = FALSE,   # keep the bar on screen after completion
  width  = 60
)
#initialize
iter <- 0

suppressWarnings({

# Iterate over weather conditions
for (i in 1:nrow(weather_list)) {
  
  # Extract weather data for this iteration
  date <- as.Date(weather_list$Date[i], format="%m-%d-%Y")
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
    strata_input <- strata_data %>% filter(Stratum == strata[j])
    
    #Get Elevation and Foliar Moisture Content
    Elev <- Elevation
    FMC <- FMC_calc(Lat, Lon, ELEV = Elev, DATE = date)
    
    # Extract fuel information and change to Megagrams per hectare
    litter_Mg <- strata_input$Lit_kg * 10
    hr1_Mg <- strata_input$hr_1_kg * 10
    hr10_Mg <- strata_input$hr_10_kg * 10
    hr100_Mg <- strata_input$hr_100_kg * 10
    FB_depth_cm <- strata_input$FB_depth
    ffl_kg <- strata_input$Lit_kg + strata_input$hr_1_kg + strata_input$hr_10_kg + strata_input$hr_100_kg + strata_input$Grass_Loading + strata_input$Herb_Loading + strata_input$Shrub_Loading

    # Calculate Surface Fuel Consumption (SFC) from 2 ways:
    #Wotton 2007
    sfc_n <- sfc(fueltype = strata_input$FT_Can, dc = DC, ffmc = FFMC, ffl = ffl_kg, bui = BUI, depth = FB_depth_cm, dmc = DMC, bd = 0)
    
    GrassCuring<-weather_list$grass_curing[i]
    if(is.na(GrassCuring)){GrassCuring<-50}
    
    #Canadian fbp
    SFC_FBP<-surface_fuel_consumption(FUELTYPE = strata_input$FT_Can,
                                      FFMC=FFMC,
                                      BUI=BUI,
                                      PC= GrassCuring, GFL=strata_input$Grass_Loading)
    
    # Calculate Effective Fine Fuel Moisture (EFFM)
    Density<- Density<- ifelse(strata_input$CC< 45,"Light",
                   ifelse(strata_input$CC> 45| strata_input$CC< 60 ,"Moderate",
                          ifelse(strata_input$CC> 60 ,"Dense")))
    Season<- "Summer"
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
  #PreTreat_df$EFFM[j]<-effm$fm1hr
    
  #FSG:
  if(FSG.Mod.Flag[j]){
  FSG<-strata_input$FSG_Mod
  }else{  
  FSG<-strata_input$FSG
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
    
    
    #Find Best American Model select by surface fuel and forest type:
    #ForestType: "Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce", "Grass", "Shrub", "Slash"
    #SurfFuel: litter, b_litter (broadleaf-litter), shrub, grass, slash, moss
    #Density: < 45% = "Light", 45- 60% = "Moderate", 60% > = "Dense"
    
    
    # Prepare inputs for fire behavior models
    Plot_fuels <- fuelModels[36, 1:16]
    
    # Modify fuel loads D for dynamic then 1hr, 10hr, 100hr, 1000hr, and depth post treatment
    Plot_fuels$fuelModelType <- "D"
    Plot_fuels$loadLitter <- litter_Mg
    Plot_fuels$load1hr <- hr1_Mg
    Plot_fuels$load10hr <- hr10_Mg
    Plot_fuels$load100hr <- hr100_Mg
    Plot_fuels$fuelBedDepth <- FB_depth_cm
    
    if(!is.na(strata_input$USModel)){
    RothModel<-strata_input$USModel  
    }else{
    BestUSModel<-find_best_model(Plot_fuels,Density=Density, SurfFuelType=strata_input$SurfFuel, ForestType=strata_input$Forest_Type)
    RothModel<-BestUSModel$FuelModel
    }
    
    model_roth<-RothModel
    model<-model_roth
  
  #select model back from dataset: USE PRESET MODEL FUELS
    if(model_roth %in% rownames(fuelModels)){
        Plot_fuels<-fuelModels[model_roth,1:16]
    }else{
      print("Checking Custom Models: Model not in original 40 and 13")
      Plot_fuels<-CustomModels[model_roth,1:17]
      Plot_fuels<-Plot_fuels %>% dplyr::select(-Model)
    }
    #select model back from dataset: USE PRESET MODEL FUELS or dont reload your fuels
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
    fuel<-data.frame(litter=Plot_fuels$loadLitter,hr1=Plot_fuels$load1hr,hr10=Plot_fuels$load10hr,hr100=Plot_fuels$load100hr,depth=FB_depth_cm,CAN_model=strata_input$FT_Can)
   
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
      UseModel =TRUE,
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

# Run the fire behavior models (e.g., Rothermel, CFIS, CFIS 2.0, FBP, CFIM)
 
   #check if Nelson predicts a fire
    if(Plot_sfc$SFC > ffl_kg | Plot_sfc$SFC_FBP > ffl_kg){
      Plot_sfc<-data.frame(SFC = ffl_kg, SFC_FBP=ffl_kg, BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s, SFC_Nelson=Plot_sfc$BurningRate*(Nelson$Residence.Time.s))
      }
    
    
    #Scott and Reinhardt
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
    
    load<-Plot_fuels %>% dplyr::select(load1hr,load10hr,load100hr,loadLiveHerb,loadLiveWoody)
    sav<-Plot_fuels %>%dplyr::select(sav1hr,sav10hr,sav100hr,savLiveHerb,savLiveWoody)
    depth<-Plot_fuels$fuelBedDepth
    mxdead<-Plot_fuels %>% dplyr::select(mxDead)
    heat<-c(rep(H,3),19500,20000)#standard Heat of combustion
    #heat<-c(heat_df$H_N[2:4],19500,20000) #or calculated values from Albini equations
    moist<- cbind(effm$fm1hr,effm$fm10hr,effm$fm100hr,herb_live,woody_live)
    u1<-WS_mid
    slp<-Plot_stand$slope
    WindSpeed<-WS
    
    Rothermel_ros<-ros(modeltype = "D",
                       w=unlist(unname(as.vector(load))),
                       s=unlist(unname(as.vector(sav))),
                       delta=unlist(unname(as.vector(depth))),
                       mx.dead = unlist(unname(as.vector(mxdead))),
                        h=heat,
                       m=as.vector(moist),
                       u=as.vector(wind_adj*WindSpeed),
                       slope=as.vector(slp))
    
    #Check which rate of spread is greater and use that or use mean
   #ROS <- pmax(Rothermel_ros$`ROS [m/min]`, 
    #        ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`)
   ROS<-max(c(Rothermel_ros$`ROS [m/min]`, 
            ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`))
   
   #Extract Final SFC
   Plot_sfc$SFC<-ScottReinhardt$FuelConsumed_kg_m2
   
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
    
      #Nelson's equation ()
      I_B_NELSON<-0.85*H*(1 - CharFract)*p_f*strata_input$FB_depth*PackingRatio*ROS/60   
      #Byram's Intensity Equation      
      I_B_BYRAM<-H*(SFC)*ROS/60
      
      #Get Intensity
      Intensity<-ifelse(IntensityFlag == "Nelson",I_B_NELSON,
                        ifelse(IntensityFlag == "Byram",I_B_BYRAM,ScottReinhardt$fireBehavior$`Fireline Intensity [kW/m]`))
                        
      
    #FBP Function:
  FBP_df<-data.frame(
    FuelType=ifelse(strata_input$FT_Can == "D-1/2","D-1",ifelse(strata_input$FT_Can == "M-1/2","M-1",strata_input$FT_Can)),
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
  if("grass_curing" %in% colnames(weather_list)){
    FBP_df<-cbind(FBP_df,data.frame(cc=weather_list$grass_curing[i]))
  }

  FBP<-cffdrs::fbp(FBP_df,output = "ALL")
  FBP$FuelType<-strata_input$FT_Can
  FBP$Model_type<-"FBP"
  FBP$Type<- ifelse(FBP$FD == "C","active",
                             ifelse(FBP$FD == "S", "surface", "passive"))
  
    #Decide on FineFuelMoisture
    EFFM<-ffm1_Wotton
    #EFFM<-effm$fm1hr
  
  
    #CFIS Original
    CFIS<- firebehavioR::cfis(fsg = FSG, sfc = SFC, effm = EFFM, u10 = WS, cbd = Plot_crown$cbd, id = 1)
    CFIS$Model_type<- "CFIS"
    
    #calculate surface rate of spread with the Rothermel equation
    #CFIS$Surface_ROS<-ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`
    CFIS$Surface_ROS<-ROS
 
    #CFIS 2.0: Perrakis 2023 Logistic Crown + Cruz 2005 Spread
    CFIS_2.0 <- cfis_modified(fsg = FSG, sfc = SFC, effm = EFFM, u10 = WS, cbd = Plot_crown$cbd, id = 5, adjusted = TRUE)
  CFIS_2.0$Model_type<-"CFIS_2.0"
  #calculate surface rate of spread with the Rothermel equation 
  #CFIS_2.0$Surface_ROS<-ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`
  CFIS_2.0$Surface_ROS<-ROS
  #CFIM: Cruz 2005
  fuel<-data.frame(litter=litter_Mg/10,hr1=hr10_Mg/10,hr10=hr10_Mg/10,hr100=hr100_Mg/10,depth=strata_input$FB_depth,CAN_model=strata_input$FT_Can)
    
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
FireType<-ifelse(CFIS$type == "active" | CFIS$type == "passive","crown","surface")
CFC<-ifelse(FireType == "crown",CFC_Calc(CFIS$cROS,strata_input$CFL)$CFC,CFC_Calc( CFIS$Surface_ROS,strata_input$CFL)$CFC)


FireType_2.0<-ifelse(CFIS_2.0$type == "active" | CFIS_2.0$type == "passive","crown","surface")
CFC_2.0<-ifelse(FireType_2.0 == "crown",CFC_Calc(CFIS_2.0$cROS,strata_input$CFL)$CFC,CFC_Calc( CFIS_2.0$Surface_ROS,strata_input$CFL)$CFC)

 
#Calculte Intensity, Critical Intensity, WITH Byrams and Van Wagner's Equations:decide on Nelson or Byrams intensity
  #Calculte Intensity, Critical Intensity, WITH Byrams and Van Wagner's Equations:decide on Nelson or Byrams intensity
  CFIS$Intensity_Surface<-I_B_BYRAM
    CFIS$Intensity_Crown<-(CFIS$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS$cROS/60
    CFIS$Critical_Int<-0.001*(strata_input$FSG^1.5)*(460+25.9*FMC)^1.5
    CFIS$CFC<-CFC
    CFIS$SFC<-ScottReinhardt$FuelConsumed_kg_m2  # Surface fuel consumption
    R_0<-3/Plot_crown$cbd
    CAC<-CFIS$cROS/R_0
    CFIS$ROS <-ifelse(is.na(CFIS$cROS),CFIS$Surface_ROS, CFIS$cROS * exp(-CAC_2.0))
    CFIS$Intensity<-ifelse(is.na(CFIS$Intensity_Crown),CFIS$Intensity_Surface,(CFIS$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS$ROS/60)
  CFIS$Flame_Length<-0.0775*(CFIS$Intensity^0.46)
  CFIS$CrownScorch<-(4.4713*CFIS$Intensity^0.667)/(60-TEMP)

    CFIS_2.0$Intensity_Surface<-I_B_BYRAM
    CFIS_2.0$Intensity_Crown<-(CFIS_2.0$Intensity_Surface+(CFC_2.0 * 0.20482 * H * 0.430265))*11.349*CFIS_2.0$cROS/60
    CFIS_2.0$Critical_Int<-0.001*(strata_input$FSG^1.5)*(460+25.9*FMC)^1.5
    CFIS_2.0$CFC<-CFC_2.0
    CFIS_2.0$SFC<-ScottReinhardt$FuelConsumed_kg_m2  # Surface fuel consumption
    CAC_2.0<-CFIS_2.0$cROS/R_0
    CFIS_2.0$ROS <-ifelse(is.na(CFIS_2.0$cROS),CFIS_2.0$Surface_ROS, CFIS_2.0$cROS * exp(-CAC_2.0))
    CFIS_2.0$Intensity<-ifelse(is.na(CFIS_2.0$Intensity_Crown),CFIS_2.0$Intensity_Surface,(CFIS_2.0$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS_2.0$ROS/60)
    CFIS_2.0$Flame_Length<-0.0775*(CFIS_2.0$Intensity^0.46)
     CFIS_2.0$CrownScorch<-(4.4713*CFIS_2.0$Intensity^0.667)/(60-TEMP)
    
 
 
  
    # Store the results in a list
    results_list[[paste("Strata_", STRATA, "Weather", i, sep = "_")]] <- list(
      Stratum= STRATA,
      CANModel=strata_input$FT_Can,
      USModel=model_roth,
      ScottReinhardt = ScottReinhardt,
      CFIS = CFIS,
      CFIS_2.0=CFIS_2.0,
      #CFIM=cfim,
      FBP=FBP,
      Plot_sfc = Plot_sfc,
      Plot_fuels = Plot_fuels,
      Plot_moisture = Plot_moisture,
      Plot_crown = Plot_crown,
      Plot_stand = Plot_stand,
      Plot_nelson = Nelson
    )
    
    # Tick the progress bar:
      iter <- iter + 1
      pb$tick()
    
  }
}
})
saveRDS(results_list,paste0(path,Fire_out,"Results_PostTreatment.rds"))



#' 
#'   #Create Treatment structure summary files
## -----------------------------------------------------------------------------

#Create Pre and Post treatment file
stratas<-unique(PreTreat_df$Stratum)
workbook <- openxlsx::createWorkbook()
library(openxlsx)
result_tables<-list()

for(i in 1:length(stratas)){
st<-stratas[i]
  
#setup df
Result_df <- data.frame(
  `Forest Structural Attribute` = c("CFFDRS Fuel Type (CAN)","NFFDRS Fuel Type (USA)", "Canopy Base Height (m)", "Fuel Strata Gap (m)", "Canopy Bulk Density (kg/m^3)", "Canopy Fuel Load (kg/m^2)", "Surface Fuel Load (1-100hr) (kg/m^2)","Grass Herb Shrub Loading (kg/m^2)" ,"Large Woody Debris (kg^m2)","Coarse Woody Debris (kg/m^2" ,"1000 Hour Fuels (kg/m^2)", "Coarse Woody Fuel Pieces (ha)"),
  `Pre-Treatment` = rep(NA, 12),
  `Post-Treatment` = rep(NA, 12)
)

#add to data
PRE<- PreTreat_df %>% filter(Stratum == st)
POST<- PostTreat_df %>% filter(Stratum == st)

#pre
Result_df$Pre.Treatment[1]<-PRE$FT_Can
Result_df$Pre.Treatment[2]<-PRE$USModel
Result_df$Pre.Treatment[3]<-round(PRE$Fuelcalc_CBH,2)
Result_df$Pre.Treatment[4]<-round(PRE$FSG,2)
Result_df$Pre.Treatment[5]<-round(PRE$CBD,2)
Result_df$Pre.Treatment[6]<-round(PRE$CFL,2)
Result_df$Pre.Treatment[7]<-round(PRE$Fine_Fuel_kg,2)
Result_df$Pre.Treatment[8]<-round(PRE$Grass_Loading,2)+round(PRE$Shrub_Loading,2)+round(PRE$Herb_Loading,2)
Result_df$Pre.Treatment[9]<-round(PRE$LWD_kg,2)
Result_df$Pre.Treatment[10]<-round(PRE$CWD_kg,2)
Result_df$Pre.Treatment[11]<-round(PRE$hr_1000_kg,2)
Result_df$Pre.Treatment[12]<-round(PRE$CWD_Pieces_5m_ha)

#post
Result_df$Post.Treatment[1]<-POST$FT_Can
Result_df$Post.Treatment[2]<-POST$USModel
Result_df$Post.Treatment[3]<-round(POST$Fuelcalc_CBH,2)
Result_df$Post.Treatment[4]<-round(POST$FSG,2)
Result_df$Post.Treatment[5]<-round(POST$CBD,2)
Result_df$Post.Treatment[6]<-round(POST$CFL,2)
Result_df$Post.Treatment[7]<-round(POST$Fine_Fuel_kg,2)
Result_df$Post.Treatment[8]<-round(POST$Grass_Loading,2)+round(POST$Shrub_Loading,2)+round(POST$Herb_Loading,2)
Result_df$Post.Treatment[9]<-round(POST$LWD_kg,2)
Result_df$Post.Treatment[10]<-round(POST$CWD_kg,2)
Result_df$Post.Treatment[11]<-round(POST$hr_1000_kg,2)
Result_df$Post.Treatment[12]<-round(POST$CWD_Pieces_5m_ha)

#export  
result_tables[[i]]<-Result_df  
names(result_tables)[[i]]<- stratas[i]
#
current_table <- Result_df
treatment_name <- stratas[i]

# Add a new sheet with the treatment name
  openxlsx::addWorksheet(workbook, sheetName = paste0("FTU_",treatment_name))
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "TopBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  
  # Write the treatment name as a header in the first row
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = paste("FTU_",treatment_name,": Stand Structure"), startRow = 1, startCol = 1)
  
  # Merge the header cells (across columns A to C) and apply header style
  openxlsx::mergeCells(workbook, sheet = paste0("FTU_",treatment_name), cols = 1:3, rows = 1)
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  # Write the actual table below the header
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = current_table, startRow = 2, startCol = 1)
  
  # Apply table border style to the entire table
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = table_border_style, 
           rows = 2:(2 + nrow(current_table)), cols = 1:ncol(current_table), gridExpand = TRUE)
  

}

# Create "All" sheet with all tables stacked
openxlsx::addWorksheet(workbook, sheetName = "All")

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  border = "TopBottomLeftRight",
  borderColour = "black",
  fgFill = "lightgrey"
)

table_border_style <- openxlsx::createStyle(
  border = "TopBottomLeftRight",
  borderColour = "black"
)

current_row <- 1

for(i in 1:length(result_tables)) {
  treatment_name <- stratas[i]
  current_table <- result_tables[[i]]
  
  # Write the treatment name as a header
  openxlsx::writeData(workbook, sheet = "All", x = paste("FTU_", treatment_name, ": Stand Structure"), 
                      startRow = current_row, startCol = 1)
  
  # Merge and style header
  openxlsx::mergeCells(workbook, sheet = "All", cols = 1:3, rows = current_row)
  openxlsx::addStyle(workbook, sheet = "All", style = header_style, 
                     rows = current_row, cols = 1:3, gridExpand = TRUE)
  
  # Write the table
  openxlsx::writeData(workbook, sheet = "All", x = current_table, 
                      startRow = current_row + 1, startCol = 1)
  
  # Apply table border style
  openxlsx::addStyle(workbook, sheet = "All", style = table_border_style, 
                     rows = (current_row + 1):(current_row + 1 + nrow(current_table)), 
                     cols = 1:ncol(current_table), gridExpand = TRUE)
  
  # Update current_row for next table (add table rows + header + 1 blank row for spacing)
  current_row <- current_row + nrow(current_table) + 2
}

# Save the workbook to an Excel file
openxlsx::saveWorkbook(workbook, paste0(path,results,"Treatment_Structures.xlsx"), overwrite = TRUE)

  

#' 
#' 
#' #14.Extract Results
#' 
#'  14.1 #Creating Exportable table of fire behavior results:
## -----------------------------------------------------------------------------
#Read in Results
PreTreat_results<-readRDS(paste0(path,Fire_out,"Results_PreTreatment.rds"))
PostTreat_results<-readRDS(paste0(path,Fire_out,"Results_PostTreatment.rds"))


#Aggregate and summarize:
#Extract Results:
Pre <- rbindlist(
    lapply(names(PreTreat_results), function(name) {
      res <- PreTreat_results[[name]]
    
      # Extract WeatherList names
      strata_match <- str_match(name, "Strata__?(.*?)_")[,2]
      weather_match <- str_match(name, "Weather[_=](\\d+)")[,2]
    
    # Return as data.table row
      data.table(
        Strata = strata_match,
        Weather = weather_match,
      
      # CFIS metrics
        CFIS.Type = res$CFIS$type,
        CFIS.pCrown = res$CFIS$pCrown,
        CFIS.cROS = res$CFIS$cROS,
        CFIS.sROS = res$CFIS$Surface_ROS,
        CFIS.ROS = res$CFIS$ROS,  
        CFIS.cIntensity = res$CFIS$Intensity_Crown,
        CFIS.sIntensity = res$CFIS$Intensity_Surface,
        CFIS.Intensity = res$CFIS$Intensity,
        CFIS.CritIntensity = res$CFIS$Critical_Int,
        CFIS.FlameLength = res$CFIS$Flame_Length,
      
      # CFIS 2.0 metrics
        CFIS2.Type = res$CFIS_2.0$type,
        CFIS2.pCrown = res$CFIS_2.0$pCrown,
        CFIS2.cROS = res$CFIS_2.0$cROS,
        CFIS2.sROS = res$CFIS_2.0$Surface_ROS,
        CFIS2.ROS = res$CFIS_2.0$ROS,
        CFIS2.cIntensity = res$CFIS_2.0$Intensity_Crown,
        CFIS2.sIntensity = res$CFIS_2.0$Intensity_Surface,
        CFIS2.Intensity = res$CFIS_2.0$Surface,
        CFIS2.CritIntensity = res$CFIS_2.0$Critical_Int,
        CFIS2.FlameLength = res$CFIS_2.0$Flame_Length,
       
      #FBP Metrics:
       FBP.Type = res$FBP$FD,
         FBP.CFB = res$FBP$CFB,
         FBP.cROS = res$FBP$ROS,
         FBP.sROS = res$FBP$ROS,
         FBP.cIntensity = res$FBP$HFI,
         FBP.sIntensity = res$FBP$HFI,
        FBP.CritIntensity = res$CFIS_2.0$CSI,
         FBP.FlameLength = 0.0775*(res$FBP$HFI^0.46),
         
      
      #ScottReinhardt Metrics:
        SR.Type = res$ScottReinhardt$fireBehavior$`Type of Fire`,
         SR.CFB = res$ScottReinhardt$fireBehavior$`Crown Fraction Burned [%]`,
        SR.sROS = res$ScottReinhardt$detailSurface$`Potential ROS [m/min]`,
         SR.cROS = res$ScottReinhardt$detailCrown$`Potential ROS [m/min]`,
        SR.sIntensity = res$ScottReinhardt$detailSurface$`Reaction intensity [kW/m2]`,
         SR.cIntensity = res$ScottReinhardt$detailCrown$`Reaction intensity [kW/m2]`,
         SR.FlameLength = res$ScottReinhardt$fireBehavior$`Flame Length [m]`,
         SR.CritIntensity = res$ScottReinhardt$critInit$`Fireline Intensity [kW/m]`,
        SR.CrownIndexWS = res$ScottReinhardt$fireBehavior$`Crowning Index [km/hr]`,
        SR.CritFlameLength = res$ScottReinhardt$critInit$`Flame length (m)`,

      # Inputs metrics
        USModel=res$USModel,
        CANModel=res$CANModel,
        SFC=res$Plot_sfc$SFC,
        SFCNelson=res$Plot_sfc$SFC_Nelson,
        FuelMoist_1hr=res$Plot_moisture$hr1,
        FMC=res$Plot_crown$fmc,
        WS=res$Plot_stand$ws,
        Nelson_BurningRate=res$Plot_nelson$Surface.Area.Burning.Rate.kg..m2.s,
        Nelson_Flag=res$Plot_nelson$FLAG
        
      )
      }),
      fill = TRUE
  )

Post <- rbindlist(
    lapply(names(PostTreat_results), function(name) {
      res <- PostTreat_results[[name]]
    
      # Extract WeatherList names
      strata_match <- str_match(name, "Strata__?(.*?)_")[,2]
      weather_match <- str_match(name, "Weather[_=](\\d+)")[,2]
    
    # Return as data.table row
      data.table(
        Strata = strata_match,
        Weather = weather_match,
      
      
      # CFIS metrics
        CFIS.Type = res$CFIS$type,
        CFIS.pCrown = res$CFIS$pCrown,
        CFIS.cROS = res$CFIS$cROS,
        CFIS.sROS = res$CFIS$Surface_ROS,
        CFIS.ROS = res$CFIS$ROS,  
        CFIS.cIntensity = res$CFIS$Intensity_Crown,
        CFIS.sIntensity = res$CFIS$Intensity_Surface,
        CFIS.Intensity = res$CFIS$Intensity,
        CFIS.CritIntensity = res$CFIS$Critical_Int,
        CFIS.FlameLength = res$CFIS$Flame_Length,
      
      # CFIS 2.0 metrics
        CFIS2.Type = res$CFIS_2.0$type,
        CFIS2.pCrown = res$CFIS_2.0$pCrown,
        CFIS2.cROS = res$CFIS_2.0$cROS,
        CFIS2.sROS = res$CFIS_2.0$Surface_ROS,
        CFIS2.ROS = res$CFIS_2.0$ROS,
        CFIS2.cIntensity = res$CFIS_2.0$Intensity_Crown,
        CFIS2.sIntensity = res$CFIS_2.0$Intensity_Surface,
        CFIS2.Intensity = res$CFIS_2.0$Surface,
        CFIS2.CritIntensity = res$CFIS_2.0$Critical_Int,
        CFIS2.FlameLength = res$CFIS_2.0$Flame_Length,
      
      #FBP Metrics:
        FBP.Type = res$FBP$FD,
         FBP.CFB = res$FBP$CFB,
         FBP.cROS = res$FBP$ROS,
         FBP.sROS = res$FBP$ROS,
         FBP.cIntensity = res$FBP$HFI,
         FBP.sIntensity = res$FBP$HFI,
         FBP.FlameLength = 0.0775*(res$FBP$HFI^0.46),
         FBP.CritIntensity = res$CFIS_2.0$CSI,
      
      #ScottReinhardt Metrics:
       SR.Type = res$ScottReinhardt$fireBehavior$`Type of Fire`,
         SR.CFB = res$ScottReinhardt$fireBehavior$`Crown Fraction Burned [%]`,
        SR.sROS = res$ScottReinhardt$detailSurface$`Potential ROS [m/min]`,
         SR.cROS = res$ScottReinhardt$detailCrown$`Potential ROS [m/min]`,
        SR.sIntensity = res$ScottReinhardt$detailSurface$`Reaction intensity [kW/m2]`,
         SR.cIntensity = res$ScottReinhardt$detailCrown$`Reaction intensity [kW/m2]`,
         SR.FlameLength = res$ScottReinhardt$fireBehavior$`Flame Length [m]`,
         SR.CritIntensity = res$ScottReinhardt$critInit$`Fireline Intensity [kW/m]`,
        SR.CrownIndexWS = res$ScottReinhardt$fireBehavior$`Crowning Index [km/hr]`,
        SR.CritFlameLength = res$ScottReinhardt$critInit$`Flame length (m)`,

      
      # Inputs metrics
        USModel=res$USModel,
        CANModel=res$CANModel,
        SFC=res$Plot_sfc$SFC,
        SFCNelson=res$Plot_sfc$SFC_Nelson,
        FuelMoist_1hr=res$Plot_moisture$hr1,
        FMC=res$Plot_crown$fmc,
        WS=res$Plot_stand$ws,
        Nelson_BurningRate=res$Plot_nelson$Surface.Area.Burning.Rate.kg..m2.s,
        Nelson_Flag=res$Plot_nelson$FLAG
        
      )
      }),
      fill = TRUE
  )

    
#Check results
Pre
Post

#merge data frames to make graph
Pre$Period <- "Pre"
Post$Period <- "Post"
Results_Master <- rbind(Pre, Post)
Results_Master$Period <- factor(Results_Master$Period, levels = c("Pre", "Post"))

#Summarise:
# Create a mode function
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Means for numeric, mode for non-numeric
Results_Means <- Results_Master %>%
  group_by(Strata, Period) %>%
  dplyr::summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    across(
      where(~!is.numeric(.x)),
      ~ get_mode(.x)
    ),
    .groups = "drop"
  )

# Median for numeric, mode for non-numeric
Results_Median <- Results_Master %>%
  group_by(Strata, Period) %>%
  dplyr::summarise(
    across(
      where(is.numeric),
      ~ median(.x, na.rm = TRUE)
    ),
    across(
      where(~!is.numeric(.x)),
      ~ get_mode(.x)
    ),
    .groups = "drop"
  )

#set aggregated df for plotting
Results_Master
Results_Means
Results_Median
Results_Aggregated<-Results_Means
Results_Median$CFIS.cIntensity[1]


#' 
#'   14.2 Viewing:
## -----------------------------------------------------------------------------
SRDATA<-Results_Master %>% dplyr::select(SR.Type,SR.CFB,SR.cROS,SR.sROS,SR.cIntensity,SR.sIntensity,WS,FuelMoist_1hr,Period,Strata)
SRDATA$ROS<-ifelse(SRDATA$SR.Type %in% c("surface","conditional"),SRDATA$SR.sROS,SRDATA$SR.cROS)
SRDATA$Intensity<-ifelse(SRDATA$SR.Type %in% c("surface","conditional"),SRDATA$SR.sIntensity,SRDATA$SR.cIntensity)

SRDATA$FMGroup <- case_when(
  SRDATA$FuelMoist_1hr <= 5 ~ "<5%",
  SRDATA$FuelMoist_1hr > 5 & SRDATA$FuelMoist_1hr <= 7 ~ "5%-7%",
  SRDATA$FuelMoist_1hr > 7 & SRDATA$FuelMoist_1hr <= 9 ~ "7%-9%",
  SRDATA$FuelMoist_1hr > 9 & SRDATA$FuelMoist_1hr <= 11 ~ "9%-11%",
  SRDATA$FuelMoist_1hr > 11 ~ "11%+"
)
SRDATA$FMGroup <- factor(SRDATA$FMGroup, 
                         levels = c("<5%", "5%-7%", "7%-9%", "9%-11%", "11%+"))


SRDATA %>%
  filter(ROS>=0) %>%
  ggplot(aes(x=WS,y=ROS,group=SR.Type, color=SR.Type))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(
    title="Predicted Rate of Spread by Fire Type (Rothermel Model)",
    x="Wind Speed (km/hr)",
    y="Rate of Spread (meters/min)",
    color="Fire Type"
  )+
  facet_wrap(~Period,scales="fixed")

SRDATA %>%
  filter(ROS >= 0, SR.Type %in% c("surface","conditional")) %>%
  ggplot(aes(x = WS, y = ROS, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Surface Rate of Spread by Fire Type (Rothermel Model)",
    x = "Wind Speed (km/hr)",
    y = "Rate of Spread (meters/min)",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period + Strata,scales="fixed")

SRDATA %>%
  filter(ROS >= 0, SR.Type %in% c("surface")) %>%
  ggplot(aes(x = WS, y = ROS, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Crown Rate of Spread by Fire Type (Rothermel Model)",
    x = "Wind Speed (km/hr)",
    y = "Rate of Spread (meters/min)",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period,scales="fixed")

#CFIS MODELING
CFISDATA<-Results_Master %>% dplyr::select(CFIS.Type,CFIS.pCrown,CFIS.cROS,CFIS.sROS,CFIS.cROS,CFIS.ROS,CFIS.cIntensity,CFIS.sIntensity,CFIS.Intensity,CFIS.FlameLength,SR.CritFlameLength,WS,FuelMoist_1hr,SFC,Period,Strata)


CFISDATA$FMGroup <- case_when(
  CFISDATA$FuelMoist_1hr <= 5 ~ "<5%",
  CFISDATA$FuelMoist_1hr > 5 & CFISDATA$FuelMoist_1hr <= 7 ~ "5%-7%",
  CFISDATA$FuelMoist_1hr > 7 & CFISDATA$FuelMoist_1hr <= 9 ~ "7%-9%",
  CFISDATA$FuelMoist_1hr > 9 & CFISDATA$FuelMoist_1hr <= 11 ~ "9%-11%",
  CFISDATA$FuelMoist_1hr > 11 ~ "11%+"
)
CFISDATA$FMGroup <- factor(CFISDATA$FMGroup, 
                         levels = c("<5%", "5%-7%", "7%-9%", "9%-11%", "11%+"))


maxROS<-max(CFISDATA$CFIS.ROS,na.rm=TRUE)

CFISDATA %>%
  filter(CFIS.ROS>=0, CFIS.ROS<maxROS) %>%
  ggplot(aes(x=WS,y=CFIS.ROS,group=CFIS.Type, color=CFIS.Type))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(
    title="Predicted Rate of Spread by Fire Type (CFIS+Byram Model)",
    x="Wind Speed (km/hr)",
    y="Rate of Spread (meters/min)",
    color="Fire Type"
  )+
  facet_wrap(~Period,scales="fixed")

CFISDATA %>%
  filter(CFIS.ROS >= 0, CFIS.ROS<maxROS, CFIS.Type %in% c("surface","passive")) %>%
  ggplot(aes(x = WS, y = CFIS.ROS, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Surface Rate of Spread by Fire Type (CFIS+Byram Model)",
    x = "Wind Speed (km/hr)",
    y = "Rate of Spread (meters/min)",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period + Strata,scales="fixed")

CFISDATA %>%
  filter(CFIS.ROS >= 0, CFIS.ROS<maxROS,Strata=="FTU-A") %>%
  ggplot(aes(x = WS, y = CFIS.ROS, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Crown Rate of Spread by Fire Type (CFIS+Byram Model)",
    x = "Wind Speed (km/hr)",
    y = "Rate of Spread (meters/min)",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period,scales="fixed")

CFISDATA %>%
  filter(CFIS.ROS >= 0, CFIS.ROS<maxROS,Strata=="FTU-A") %>%
  ggplot(aes(x = WS, y = SR.CritFlameLength, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Critical Flame Length for Crowning by FuelMoisture",
    x = "Wind Speed (km/hr)",
    y = "Critical Flame Length (m)",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period,scales="fixed")


CFISDATA %>%
  filter(CFIS.ROS >= 0, CFIS.ROS<maxROS,Strata=="FTU-B") %>%
  ggplot(aes(x = WS, y = SR.CritFlameLength, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Critical Flame Length for Crowning by FuelMoisture",
    x = "Wind Speed (km/hr)",
    y = "Critical Flame Length (m)",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period,scales="fixed")


CFISDATA %>%
  filter(CFIS.ROS >= 0, CFIS.ROS<maxROS, CFIS.FlameLength <2) %>%
  ggplot(aes(x = WS, y = SFC, group = FMGroup, color = FMGroup)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("<5%" = "#D32F2F",      # Red
                                 "5%-7%" = "#FF9800",    # Orange
                                 "7%-9%" = "#FDD835",    # Yellow
                                 "9%-11%" = "#7CB342",   # Green
                                 "11%+" = "#1976D2")) +  # Blue
  labs(
    title = "Predicted Critical Flame Length for Crowning by FuelMoisture",
    x = "Wind Speed (km/hr)",
    y = "Surface Fuel Consumption",
    color = "Fuel Moisture Level"
  )+
  facet_wrap(~Period,scales="fixed")

CFISDATA %>%
  filter(CFIS.ROS >= 0, CFIS.ROS<maxROS, CFIS.FlameLength <2)

#' 
#' #15.Interpret Results:Plotting
#' 
#'   15.1 Probability of Crown Fire: CFIS
## -----------------------------------------------------------------------------
dir<-paste0(path,results,"CFIS pCrown/")
if(!dir.exists(dir)){
  dir.create(dir)
}

#Main Results Graph
mean_pcrown<-Results_Aggregated %>%
  ggplot() +
  #shading
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100,fill = "Crown Fire"),alpha = 0.06) +  # Light red shading
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50,fill = "Surface Fire"),alpha = 0.06) +  # Light green shading
  # One segment for each period (Pre and Post) with black segments, grouped by period
  geom_segment(aes(x = Strata, y = 0, yend = CFIS.pCrown, group = Period), 
               color = "black", position = position_dodge(width = 0.6), size = 0.75) +
  # Points colored by Period (Pre and Post), placed next to each other
  geom_point(aes(x = Strata, y = CFIS.pCrown, color = Period), 
             position = position_dodge(width = 0.6), size = 5.5) +
  scale_color_manual(values = c("Pre" = "#FFB10E", "Post" = "#0000FF"))+
  geom_text(aes(x = Strata, y = CFIS.pCrown, label = paste0(round(CFIS.pCrown, 0), "%"), color = Period), 
            position = position_dodge(width = 0.6), vjust = -1, size = 3.5) +
  scale_fill_manual(
    name = "Fire Type",
    values = c("Crown Fire" = "lightcoral", "Surface Fire" = "lightgreen"),
    guide = guide_legend(override.aes = list(alpha = 0.5))
  ) +
  # Add a horizontal line at y = 50
  #geom_hline(yintercept = 50, color = "red", size = 0.75, linetype="dashed") +
  #scale Y
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +  # Set y-axis limits
  # Axis labels and title
  labs(
    title = "Mean Predicted Probability of Crown Fire by Treatment",
    subtitle = "Pre vs. Post-Treatment",
    x = "Treatment Unit (Groups)",
    y = "Probability of Crown Fire Occurrence (%) {CFIS}",
    color="Treatment Period"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
mean_pcrown

ggsave(paste0(path,results,"pCrown_Mean_plot.jpeg"),mean_pcrown)


distribution_pcrown<-Results_Master %>%  
  ggplot(aes(x=Strata, y=CFIS.pCrown, fill=Period)) +
  geom_boxplot() +
    scale_fill_manual(values = c("Pre" = "#FFB14E", "Post" = "#0000FF"))+
    labs(
      title = "Distribution of Crown Fire Probability Predicted by each Unit",
      subtitle = paste(project,",BC, Range of Extreme Weather Conditions"),
      x= "Treatment Sites",
      y= "Probability of Crown Fire [CFIS] (%)"
    ) +
    facet_wrap(~Strata, scale="free_x") +
    scale_y_continuous(limits = c(0, 100))

distribution_pcrown


ggsave(paste0(path,results,"pCrown_Distribution_plot.jpeg"),distribution_pcrown)


#review:
for(i in 1:length(unique(Results_Master$Strata))){
  st<-unique(Results_Master$Strata)[i]
  data<-Results_Master %>% filter(Strata==st)
  plt <- ggstatsplot::ggbetweenstats(
  data = data,
  x = Period,
  y = CFIS.pCrown,
  type="robust",
  pairwise.display	="s")
  
  plt<-plt + 
  # Add labels and title
  labs(
    x = "Treatment Period",
    y = "Probability of Crown Fire (CFIS)",
    title = paste("Probability of Crowning by Treatment Period for Strata",st)
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Arial", size = 8, color = "black"),
    plot.title = element_text(
      family = "Arial", 
      size = 15,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Arial", 
      size = 12, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )
  plt<-plt  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
  
  ggsave(paste0(dir,st,"_pCrown_StatsDistribution.jpeg"),plt)


}



#' 
#'   Probability of Crown Fire: Perrakis 2023
## -----------------------------------------------------------------------------
dir<-paste0(path,results,"Perrakis pCrown/")
if(!dir.exists(dir)){
  dir.create(dir)
}

#Main Results Graph
mean_pcrown<-Results_Aggregated %>%
  ggplot() +
  #shading
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100,fill = "Crown Fire"),alpha = 0.06) +  # Light red shading
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50,fill = "Surface Fire"),alpha = 0.06) +  # Light green shading
  # One segment for each period (Pre and Post) with black segments, grouped by period
  geom_segment(aes(x = Strata, y = 0, yend = CFIS2.pCrown, group = Period), 
               color = "black", position = position_dodge(width = 0.6), size = 0.75) +
  # Points colored by Period (Pre and Post), placed next to each other
  geom_point(aes(x = Strata, y = CFIS2.pCrown, color = Period), 
             position = position_dodge(width = 0.6), size = 5.5) +
  scale_color_manual(values = c("Pre" = "#FFB10E", "Post" = "#0000FF"))+
  geom_text(aes(x = Strata, y = CFIS2.pCrown, label = paste0(round(CFIS2.pCrown, 0), "%"), color = Period), 
            position = position_dodge(width = 0.6), vjust = -1, size = 3.5) +
  scale_fill_manual(
    name = "Fire Type",
    values = c("Crown Fire" = "lightcoral", "Surface Fire" = "lightgreen"),
    guide = guide_legend(override.aes = list(alpha = 0.5))
  ) +
  # Add a horizontal line at y = 50
  #geom_hline(yintercept = 50, color = "red", size = 0.75, linetype="dashed") +
  #scale Y
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +  # Set y-axis limits
  # Axis labels and title
  labs(
    title = "Mean Predicted Probability of Crown Fire by Treatment",
    subtitle = "Pre vs. Post-Treatment",
    x = "Treatment Unit (Groups)",
    y = "Probability of Crown Fire Occurrence (%) {Perrakis2023}",
    color="Treatment Period"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
mean_pcrown

ggsave(paste0(path,results,"pCrown_Mean_plot_perrakis.jpeg"),mean_pcrown)

#review:
for(i in 1:length(unique(Results_Master$Strata))){
  st<-unique(Results_Master$Strata)[i]
  data<-Results_Master %>% filter(Strata==st)
  plt <- ggstatsplot::ggbetweenstats(
  data = data,
  x = Period,
  y = CFIS2.pCrown,
  type="robust",
  pairwise.display	="s")
  
  plt<-plt + 
  # Add labels and title
  labs(
    x = "Treatment Period",
    y = "Probability of Crown Fire (%) (Perrakis2023)",
    title = paste("Probability of Crowning by Treatment Period for Strata",st)
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Arial", size = 8, color = "black"),
    plot.title = element_text(
      family = "Arial", 
      size = 15,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Arial", 
      size = 12, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )
  plt<-plt  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
  
  ggsave(paste0(dir,st,"_pCrown_StatsDistribution_Perrakis.jpeg"),plt)


}



#' 
#'   15.2 Rate of Spread
## -----------------------------------------------------------------------------
Results_Aggregated$ROS<-Results_Aggregated$CFIS.ROS

Results_Master$ROS<-Results_Master$CFIS.ROS

#Rate of Spread
mean_ros<-Results_Aggregated %>%
  ggplot() +
  # One segment for each period (Pre and Post) with black segments, grouped by period
  geom_segment(aes(x = Strata, y = 0, yend = ROS, group = Period), 
               color = "black", position = position_dodge(width = 0.6), size = 0.75) +
  # Points colored by Period (Pre and Post), placed next to each other
  geom_point(aes(x = Strata, y = ROS, color = Period), 
             position = position_dodge(width = 0.6), size = 5.5) +
  scale_color_manual(values = c("Pre" = "#FFB10E", "Post" = "#0000FF"))+
  geom_text(aes(x = Strata, y = ROS, label = paste0(round(ROS, 1)), color = Period), 
            position = position_dodge(width = 0.6), hjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(0, max(Results_Aggregated$ROS)), expand = c(0, 0)) +
  # Axis labels and title
  labs(
    title = "Predicted Rate of Spread by Strata",
    subtitle = paste("Pre vs. Post-Treatment:",project,",BC"),
    x = "Strata Unit (Groups)",
    y = "Rate of Spread (m/min)",
    color="Treatment Period"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  coord_flip()+
  theme_minimal()

mean_ros
ggsave(paste0(path,results,"ROS_Mean_plot.jpeg"),mean_ros)

#histograms
xmin <- 0
xmax <- max(Results_Master$ROS, na.rm = TRUE)
bins <- 20
binwidth <- (xmax - xmin) / bins

# --- 2) Means + tallest-bar heights for y placement ---
ROSSummary <- Results_Master %>%
  group_by(Strata, Period) %>%
  dplyr::summarize(mean = mean(ROS, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(round(mean, 1), " m/min"))

# compute tallest bar per Strata × Period using same binning
ros_heights <- Results_Master %>%
  filter(!is.na(ROS), ROS >= xmin, ROS <= xmax) %>%
  mutate(.bin = pmin(floor((ROS - xmin) / binwidth), bins - 1L)) %>%
  group_by(Strata, Period, .bin) %>%
  dplyr::summarize(count = n(), .groups = "drop") %>%
  group_by(Strata, Period) %>%
  dplyr::summarize(ypos = max(count) * 1.05, .groups = "drop")  # 5% above tallest bar

ROSSummary_pos <- ROSSummary %>%
  left_join(ros_heights, by = c("Strata", "Period")) %>%
  mutate(
    ypos  = ifelse(is.na(ypos), 0, ypos),
    x_lab = mean + binwidth * 0.1   # small nudge to right of vline
  )

# --- 3) Plot ---
ROS_Hists <- Results_Master %>%
  ggplot(aes(x = ROS, fill = Period)) +
  geom_histogram(
    position = "identity",
    alpha    = 0.5,
    binwidth = binwidth,
    boundary = xmin,
    closed   = "left",
    bins     = bins
  ) +
  # mean lines (darker)
  geom_vline(
    data = ROSSummary,
    aes(xintercept = mean, color = Period),
    linetype = "dashed", size = 0.6,
    show.legend = FALSE
  ) +
  # mean labels (darker) positioned above tallest bar
  geom_text(
    data = ROSSummary_pos,
    aes(x = x_lab, y = ypos, label = label, color = Period),
    hjust = 0, vjust = 0, size = 3,
    show.legend = FALSE
  ) +
  # keep light fills; make lines/labels darker
  scale_fill_manual(values = c("Pre" = "#FFB14E", "Post" = "#0000FF")) +
  scale_color_manual(values = c("Pre" = "#CC8400", "Post" = "#000099"), guide = "none") +
  labs(
    title    = "Predicted Rate of Spread by Strata and Treatment Period",
    subtitle = glue("{project}, BC Under Extreme Conditions"),
    x        = "Rate of Spread (Ensemble Models) (m/min)",
    y        = "Count"
  ) +
  facet_wrap(~Strata, scales = "fixed") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  # ensure labels aren’t clipped above tallest bar
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(nrow = 1))

ROS_Hists

ggsave(paste0(path,results,"ROS_Histograms_plot.jpeg"),ROS_Hists)



distribution_ros<-Results_Master %>%  
  ggplot(aes(x=Strata, y=ROS, fill=Period)) + 
    geom_boxplot() +
    scale_fill_manual(values = c("Pre" = "#FFB14E", "Post" = "#0000FF"))+
    labs(
      title = "Predicted Rate of Spread by Strata and Treatment Period",
      subtitle = paste(project,", BC, 90th percentile weather distribution"),
      x= "Treatment Sites",
      y= "Rate of Spread (meters/min)"
    ) +
    facet_wrap(~Strata, scale="free_x") +
    scale_y_continuous(limits = c(0, max(Results_Master$ROS)))+
  theme_minimal()

distribution_ros

ggsave(paste0(path,results,"ROS_Distribution_plot.jpeg"),distribution_ros)

#Review:

Results_Master %>% filter(Period =="Post") %>% pull(ROS) %>% hist()


#' 
#'   15.3 Head Fire Intensity
## -----------------------------------------------------------------------------
Results_Aggregated$Intensity<-Results_Aggregated$CFIS.Intensity

Results_Master$Intensity<-Results_Master$CFIS.Intensity


mean_HFI<-Results_Aggregated %>%
  ggplot() +
  geom_bar(aes(x = Strata, y = Intensity, fill = as.factor(Period)), 
              stat = "identity", position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("Pre" = "#FFB10E", "Post" = "#0000FF"))+
  geom_text(aes(x = Strata, y = Intensity, label = paste0(round(Intensity, 1)), fill = Period), 
            position = position_dodge(width = 0.6), vjust = -0.8, size = 3.0)+
   scale_y_continuous(limits = c(0, quantile(Results_Aggregated$Intensity,0.95)), expand = c(0, 0)) +
  # Axis labels and title
  labs(
    title = "Predicted Wildfire Intensity by Strata",
    subtitle = paste("Pre vs. Post-Treatment:",project,",BC"),
    x = "Strata Unit (Groups)",
    y = "Wildfire Intensity (kW/m)",
    fill="Treatment Period"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +theme_minimal()

mean_HFI


ggsave(paste0(path,results,"FireIntensity_Mean_plot.jpeg"),mean_HFI)

distribution_intensity<-Results_Master %>%  
  ggplot(aes(x=Strata, y=Intensity, fill=Period)) + 
    geom_boxplot() +
    scale_fill_manual(values = c("Pre" = "#FFB10E", "Post" = "#0000FF"))+
    labs(
      title = "Predicted Head Fire Intensity by Strata and Treatment Period",
      subtitle = "Stellaten, BC, 90th percentile weather distribution",
      x= "Treatment Sites",
      y= "Wildfire Intensity (kW/m)"
    ) +
    facet_wrap(~Strata, scale="free") +
    scale_y_continuous(limits = c(0, quantile(Results_Aggregated$Intensity,0.95)))+
  theme_minimal()

distribution_intensity

ggsave(paste0(path,results,"HFI_Distribution_plot.jpeg"),distribution_intensity)


#histograms
xmin <- 0
xmax <- 70000
bins <- 25
binwidth <- (xmax - xmin) / bins

# --- 2) Summaries: means + per-facet max bin count for y placement ---
IntensitySummary <- Results_Master %>%
  group_by(Strata, Period) %>%
  dplyr::summarize(mean = mean(Intensity, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(round(mean, 1), " kW/m"))

# per (Strata, Period) tallest bar using same binning
hist_heights <- Results_Master %>%
  filter(Intensity >= xmin, Intensity <= xmax) %>%
  mutate(.bin = pmin(floor((Intensity - xmin) / binwidth), bins - 1L)) %>%
  dplyr::group_by(Strata, Period, .bin) %>%
  dplyr::summarize(count = n(), .groups = "drop") %>%
  group_by(Strata, Period) %>%
  dplyr::summarize(ypos = max(count) * 0.96, .groups = "drop")   # a little below the top

# join y position to the mean summary + nudge label slightly right of the line
IntensitySummary_pos <- IntensitySummary %>%
  left_join(hist_heights, by = c("Strata", "Period")) %>%
  mutate(
    ypos  = ifelse(is.na(ypos), 0, ypos),
    x_lab = mean + binwidth * 0.15  # small right nudge
  )


# --- 3) Plot ---
INT_Hists <- Results_Master %>%
  ggplot(aes(x = Intensity, fill = Period)) +
  geom_histogram(
    position = "identity",
    alpha    = 0.5,
    binwidth = binwidth,
    boundary = xmin,
    closed   = "left",
    bins     = bins
  ) +
  geom_vline(
    data = IntensitySummary,
    aes(xintercept = mean, color = Period),
    linetype = "dashed", size = 0.6,
    show.legend = FALSE
  ) +
  geom_text(
    data = IntensitySummary_pos,
    aes(x = x_lab, y = ypos, label = label, color = Period),
    hjust = 0, vjust = 0, size = 3,
    show.legend = FALSE
  ) +
  # keep histogram fills light
  scale_fill_manual(values = c("Pre" = "#FFB14E", "Post" = "#0000FF")) +
  # make lines + labels darker
  scale_color_manual(values = c("Pre" = "#CC8400", "Post" = "#000099"), guide = "none") +
  labs(
    title    = "Predicted Head Fire Intensity by Strata and Treatment Period",
    subtitle = glue("{project}, BC Under Extreme Conditions"),
    x        = "Wildfire Intensity (Ensemble Models) (kW/m)",
    y        = "Count"
  ) +
  facet_wrap(~Strata, scales = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(nrow = 1))

INT_Hists

ggsave(paste0(path,results,"Intensity_Histograms_plot.jpeg"),INT_Hists)

#' 
#'   15.4 Excel Files
#'   
#'   CFIS/Perrakis Modeling
## -----------------------------------------------------------------------------
Results_Aggregated<-Results_Means
Results_Aggregated$ROS<-ifelse(Results_Aggregated$CFIS.pCrown>=50,Results_Aggregated$CFIS.cROS,Results_Aggregated$CFIS.sROS)

#create Export Tables
Results_Aggregated$Intensity<-ifelse(Results_Aggregated$CFIS.pCrown>=50,Results_Aggregated$CFIS.cIntensity,Results_Aggregated$CFIS.sIntensity)


stratas<-unique(Results_Aggregated$Strata)
workbook <- openxlsx::createWorkbook()
library(openxlsx)
result_tables<-list()

for(i in 1:length(stratas)){

  #setup df
Result_df <- data.frame(
  Factor = c("Fire Type", "Likelihood of Crown Fire (%)", "Rate of Spread (m/min)", "Critical Surface Fire Intensity (kW/m)", "Wildfire Intensity (kW/m)", "Flame Length (m)","Crowning Index (km/hr)"),
  `Pre-Treatment` = rep(NA, 7),
  `Post-Treatment` = rep(NA, 7)
)

#select for treatment
res<-Results_Aggregated %>% filter(Strata == stratas[i])

#add to data
PRE<- res %>% filter(Period == "Pre")
POST<- res %>% filter(Period == "Post")

#pre
Result_df$Pre.Treatment[1]<-ifelse(PRE$CFIS.pCrown<=50,"Surface Fire",ifelse(PRE$CFIS.pCrown>50 & PRE$CFIS.pCrown < 75,"Passive Crown Fire","Active Crown Fire"))
Result_df$Pre.Treatment[2]<-round(PRE$CFIS.pCrown,2)
Result_df$Pre.Treatment[3]<-round(PRE$ROS,2)
Result_df$Pre.Treatment[4]<-round(PRE$CFIS.CritIntensity,2)
Result_df$Pre.Treatment[5]<-round(PRE$Intensity,2)
Result_df$Pre.Treatment[6]<-round(0.0775*(round(PRE$Intensity,2)^0.46),2)
Result_df$Pre.Treatment[7]<-round(PRE$SR.CrownIndexWS,2)

#post
Result_df$Post.Treatment[1]<-ifelse(POST$CFIS.pCrown<=50,"Surface Fire",ifelse(POST$CFIS.pCrown>50 & POST$CFIS.pCrown < 75,"Passive Crown Fire","Active Crown Fire"))
Result_df$Post.Treatment[2]<-round(POST$CFIS.pCrown,2)
Result_df$Post.Treatment[3]<-round(POST$ROS,2)
Result_df$Post.Treatment[4]<-round(POST$CFIS.CritIntensity,2)
Result_df$Post.Treatment[5]<-round(POST$Intensity,2)
Result_df$Post.Treatment[6]<-round(0.0775*(round(POST$Intensity,2)^0.46),2)
Result_df$Post.Treatment[7]<-round(POST$SR.CrownIndexWS,2)



#export  
result_tables[[i]]<-Result_df  
names(result_tables)[[i]]<- stratas[i]
#
current_table <- Result_df
treatment_name <- stratas[i]

# Add a new sheet with the treatment name
  openxlsx::addWorksheet(workbook, sheetName = paste0("FTU_",treatment_name))
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "topBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  # Style for red and green highlights
  red_highlight <- openxlsx::createStyle(
    fgFill = "red",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )
  
  green_highlight <- openxlsx::createStyle(
    fgFill = "green",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )
  # Write the treatment name as a header in the first row
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = paste("FTU_",treatment_name,": Fire Behaviour"), startRow = 1, startCol = 1)
  
  # Merge the header cells (across columns A to C) and apply header style
  openxlsx::mergeCells(workbook, sheet = paste0("FTU_",treatment_name), cols = 1:3, rows = 1)
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  # Write the actual table below the header
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = current_table, startRow = 2, startCol = 1)
  
  # Apply table border style to the entire table
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = table_border_style, 
           rows = 2:(2 + nrow(current_table)), cols = 1:ncol(current_table), gridExpand = TRUE)
  
  # RED
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = red_highlight, rows = 7, cols = 2, gridExpand = TRUE)
  
  # Green
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = green_highlight, rows = 7, cols = 3, gridExpand = TRUE)

}

# Create "All" sheet with all tables stacked
openxlsx::addWorksheet(workbook, sheetName = "All")

header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "topBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  # Style for red and green highlights
  red_highlight <- openxlsx::createStyle(
    fgFill = "red",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )
  
  green_highlight <- openxlsx::createStyle(
    fgFill = "green",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )

current_row <- 1

for(i in 1:length(result_tables)) {
  treatment_name <- stratas[i]
  current_table <- result_tables[[i]]
  
  # Write the treatment name as a header
  openxlsx::writeData(workbook, sheet = "All", x = paste("FTU_", treatment_name, ": Fire Behaviour"), 
                      startRow = current_row, startCol = 1)
  
  # Merge and style header
  openxlsx::mergeCells(workbook, sheet = "All", cols = 1:3, rows = current_row)
  openxlsx::addStyle(workbook, sheet = "All", style = header_style, 
                     rows = current_row, cols = 1:3, gridExpand = TRUE)
  
  # Write the table
  openxlsx::writeData(workbook, sheet = "All", x = current_table, 
                      startRow = current_row + 1, startCol = 1)
  
  # Apply table border style
  openxlsx::addStyle(workbook, sheet = "All", style = table_border_style, 
                     rows = (current_row + 1):(current_row + 1 + nrow(current_table)), 
                     cols = 1:ncol(current_table), gridExpand = TRUE)
  
  # Red Highlight
  openxlsx::addStyle(workbook, sheet = "All", style = red_highlight, rows = current_row + 6, cols = 2, gridExpand = TRUE)
  
  # Green Highlight
  openxlsx::addStyle(workbook, sheet = "All", style = green_highlight, rows = current_row + 6, cols = 3, gridExpand = TRUE)
 
  current_row <- current_row + nrow(current_table) + 2
}

# Save the workbook to an Excel file
openxlsx::saveWorkbook(workbook, paste0(path,results,"Treatment_Results_CFIS.xlsx"), overwrite = TRUE)



#' 
#'   FBP/Redbook Modeling
## -----------------------------------------------------------------------------
#Create Pre and Post treatment file
stratas<-unique(PreTreat_df$Stratum)
workbook <- openxlsx::createWorkbook()
library(openxlsx)

result_tables<-list()

for(i in 1:length(stratas)){

  #setup df
Result_df <- data.frame(
  Factor = c("REPRESENTATIVE FUEL TYPE", "FIRE TYPE", "CROWN FRACTION BURNT (%)", "HEAD FIRE INTENSITY (kW/m^2)", "RATE OF SPREAD (m/min)", "FLAME LENGTH (m)"),
  `Pre-Treatment` = rep(NA, 6),
  `Post-Treatment` = rep(NA, 6)
)

#select for treatment
res<-Results_Aggregated %>% filter(Strata == stratas[i])

#add to data
PRE<- res %>% filter(Period == "Pre")
POST<- res %>% filter(Period == "Post")

#pre
Result_df$Pre.Treatment[1]<-PRE$CANModel
  Result_df$Pre.Treatment[2]<-ifelse(PRE$FBP.Type == "S","Surface Fire",ifelse(PRE$FBP.Type == "I","Passive Crown Fire","Active Crown Fire"))
Result_df$Pre.Treatment[3]<-round(PRE$FBP.CFB,2)*100
Result_df$Pre.Treatment[4]<-round(PRE$FBP.sIntensity,2)
Result_df$Pre.Treatment[5]<-round(PRE$FBP.cROS,2)
Result_df$Pre.Treatment[6]<-round(PRE$FBP.FlameLength,2)

#post
Result_df$Post.Treatment[1]<-POST$CANModel
Result_df$Post.Treatment[2]<-ifelse(POST$FBP.Type == "S","Surface Fire",ifelse(POST$FBP.Type == "I","Passive Crown Fire","Active Crown Fire"))
Result_df$Post.Treatment[3]<-round(POST$FBP.CFB,2)*100
Result_df$Post.Treatment[4]<-round(POST$FBP.sIntensity,2)
Result_df$Post.Treatment[5]<-round(POST$FBP.cROS,2)
Result_df$Post.Treatment[6]<-round(POST$FBP.FlameLength,2)



#export  
result_tables[[i]]<-Result_df  
names(result_tables)[[i]]<- stratas[i]
#
current_table <- Result_df
treatment_name <- stratas[i]

# Add a new sheet with the treatment name
  openxlsx::addWorksheet(workbook, sheetName = paste0("FTU_",treatment_name))
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "topBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  # Style for red and green highlights
  red_highlight <- openxlsx::createStyle(
    fgFill = "red",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )
  
  green_highlight <- openxlsx::createStyle(
    fgFill = "green",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )
  
  # Write the treatment name as a header in the first row
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = paste("FTU_",treatment_name,": Fire Behaviour"), startRow = 1, startCol = 1)
  
  # Merge the header cells (across columns A to C) and apply header style
  openxlsx::mergeCells(workbook, sheet = paste0("FTU_",treatment_name), cols = 1:3, rows = 1)
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  # Write the actual table below the header
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = current_table, startRow = 2, startCol = 1)
  
  # Apply table border style to the entire table
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = table_border_style, 
           rows = 2:(2 + nrow(current_table)), cols = 1:ncol(current_table), gridExpand = TRUE)
  
  # RED
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = red_highlight, rows = 6, cols = 2, gridExpand = TRUE)
  
  # Green
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = green_highlight, rows = 6, cols = 3, gridExpand = TRUE)

}

# Create "All" sheet with all tables stacked
openxlsx::addWorksheet(workbook, sheetName = "All")

header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "topBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  # Style for red and green highlights
  red_highlight <- openxlsx::createStyle(
    fgFill = "red",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )
  
  green_highlight <- openxlsx::createStyle(
    fgFill = "green",
    fontColour = "white",
    border = "topBottomLeftRight",
    textDecoration = "bold"
  )

current_row <- 1

for(i in 1:length(result_tables)) {
  treatment_name <- stratas[i]
  current_table <- result_tables[[i]]
  
  # Write the treatment name as a header
  openxlsx::writeData(workbook, sheet = "All", x = paste("FTU_", treatment_name, ": Fire Behaviour"), 
                      startRow = current_row, startCol = 1)
  
  # Merge and style header
  openxlsx::mergeCells(workbook, sheet = "All", cols = 1:3, rows = current_row)
  openxlsx::addStyle(workbook, sheet = "All", style = header_style, 
                     rows = current_row, cols = 1:3, gridExpand = TRUE)
  
  # Write the table
  openxlsx::writeData(workbook, sheet = "All", x = current_table, 
                      startRow = current_row + 1, startCol = 1)
  
  # Apply table border style
  openxlsx::addStyle(workbook, sheet = "All", style = table_border_style, 
                     rows = (current_row + 1):(current_row + 1 + nrow(current_table)), 
                     cols = 1:ncol(current_table), gridExpand = TRUE)
  
  # Red Highlight
  openxlsx::addStyle(workbook, sheet = "All", style = red_highlight, rows = current_row + 5, cols = 2, gridExpand = TRUE)
  
  # Green Highlight
  openxlsx::addStyle(workbook, sheet = "All", style = green_highlight, rows = current_row + 5, cols = 3, gridExpand = TRUE)
 
  current_row <- current_row + nrow(current_table) + 2
}

# Save the workbook to an Excel file
openxlsx::saveWorkbook(workbook, paste0(path,results,"Treatment_Results_FBP.xlsx"), overwrite = TRUE)



#' 
#'   15.5 Weather Conditions Plotting:
## -----------------------------------------------------------------------------
station<-"Chetwynd (MOF)"
AOI<-"Dokie Siding"
station_name<-"CHETWYND"
library(viridis)
library(hrbrthemes)
library(ggtext)
library(patchwork)
#daily_weather<-read.csv(paste0(path,in_weather,station_name,"_Daily_FWI.csv"))
daily_weather<- read.csv(paste0(path,out_weather,"allstations_90th_FWList_dates_summer.csv"))

#get pertinent info
min_yr<-min(daily_weather$YR)
max_yr<-max(daily_weather$YR)
LAT<-max(daily_weather$LAT)
LONG<-max(daily_weather$LONG)

#Plot Weather Data Values
weather_data<-daily_weather %>% filter(ISI>7.5)
weather_data

#Plot range of extreme data
weather_to_plot<-c("TEMP","RH","WS", "FFMC","ISI","BUI","FWI")
weather_long<-pivot_longer(weather_data,cols=weather_to_plot,names_to = "Variable",values_to = "Value")
data<-weather_long %>% dplyr::select(Variable, Value)


ggplot(weather_long, aes(x = Value, y = Variable, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Value", option = "C") +
  labs(title = 'Distribution of Pertinent Weather Values Under Extreme Conditions',
       subtitle= paste(AOI,", BC. Summer ", min_yr,"-",max_yr)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
#+facet_wrap(~Variable)

#Ridgeline plot with annotations
data<-data %>% group_by(Variable) %>% mutate(Range=paste0(range(Value)[1],"-",range(Value)[2]))
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
      TRUE                ~ Variable   
    )
  )

means <- data %>% group_by(Variable) %>% summarise(mean(Value))
medians <- data %>% group_by(Variable) %>% summarise(median(Value))

bg_color <- "grey97"
font_family <- "Fira Sans"

#plot_subtitle = glue("Selected distributions of pertinent weather indices for",AOI,", BC based on crucial fire weather indices. Weather indices were selected for their role in driving fire behaviour and were clipped to represent the extreme ends of their full distribution from",min_yr,"to",max_yr,"during peak fire season months.")
plot_subtitle = glue(paste0("Selected distributions of pertinent weather indices for ",AOI,",BC based on crucial fire weather indices. Weather indices were selected for their role in driving fire behaviour and were clipped to represent the extreme ends of their full distribution from ",min_yr," to ",max_yr," during peak fire season months."))

#data<-data %>% filter(Variable != "BUI")

n_vars <- data %>% distinct(Variable) %>% nrow()
# create the dataframe for the legend (inside plot)
df_for_legend <- data %>% 
  filter(Variable == "TEMP")


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
    vjust       = -1,   # move text slightly above the point
    size        = 2.75,
    color       = "black"
  ) +
  # stat_summary(
  #  geom = "text",
  # fun.data = function(x) {
  #    rng <- range(x, na.rm = TRUE)
  #    data.frame(
  #      y     = -25,
  #      label = sprintf("(%0.1f–%0.1f)", rng[1], rng[2])
  #    )
  #  },
  #  size = 2.75
  #  ) +
scale_color_manual(values = MetBrewer::met.brewer("Homer2")) +
  #scale_x_discrete(labels = toupper) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  coord_flip(clip = "off") +
  labs(
    title    = toupper("Distribution of Key Weather Indices for Fire Behavior"),
    subtitle = plot_subtitle,
    caption  = glue(
      "Data clipped to extreme values.<br>",
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
    plot.title = element_textbox_simple(
      #margin = margin(t = 4, b = 16), 
      size = 12),
    plot.subtitle = element_textbox_simple(
      #margin = margin(t = 4, b = 16), 
      size = 7),
    plot.caption = element_textbox_simple(
      #margin = margin(t = 12), 
      size = 7
    ),
    plot.caption.position = "plot",
    axis.text.y = element_text(hjust = 0 
                               #margin = margin(r = -10),
    )
    #,plot.margin = margin(4, 4, 4, 4)
  )+ theme(legend.position = "none")



#p_legend <- 
#  df_for_legend %>% 
#  ggplot(aes(Variable_Names, Value)) +
#  stat_halfeye(fill_type = "segments", alpha = 0.6) +
#  stat_interval() +
#  stat_summary(geom = "point", fun = median, color="darkblue") +
##  annotate(
#    "richtext",
#    x = c(0.8, 1.46, 0.8, 1.35, 1.85),
##    y = c(13, 38, 30.5, 26.5, 39.6),
#  label = c("50% of values<br>fall within this range", "95% of values", 
#              "80% of values", "Median", "Distribution<br>of values"),
#    fill = NA, label.size = 0, size = 2.15, vjust = 1,
#  ) +
##  geom_curve(
#    data = data.frame(
#      x = c(0.72, 1.37, 0.77, 1.25, 1.8),
##      xend = c(0.95, 1.062, 0.95, 1.065, 1.8), 
#      y = c(20.3, 32.75, 28.5, 25.3, 34),
#      yend = c(21.5, 31, 28.5, 24, 25)),
#    aes(x = x, xend = xend, y = y, yend = yend),
#    stat = "unique", curvature = 0.2, size = 0.2, color = "black",
#    arrow = arrow(angle = 20, length = unit(1, "mm"))
#  ) +
#  scale_color_manual(values = MetBrewer::met.brewer("Homer2")) +
#  coord_flip(xlim = c(0.80, 1.35), ylim = c(0, 50), expand = TRUE) +
#  guides(color = "none") +
#  labs(title = "Legend") +
#  theme_void() +
#  theme(plot.title = element_text(size = 9,
#                                  hjust = 0.075),
#        plot.background = element_rect(color = "grey30", size = 0.2, fill = bg_color))

#edited legend
p_legend <- ggplot() +
  # Blue dot
  annotate("point", x = 0.15, y = 5, color = "darkblue", size = 2.5) +
  annotate("text",  x = 0.30, y = 5.05, label = "Mean Value", hjust = 0, size = 2.2) +
  
  # Yellow square
  annotate("rect", xmin = 0.11, xmax = 0.19, ymin = 4.5, ymax = 4.8, 
           fill = "gold", color = "black") +
  annotate("text",  x = 0.30, y = 4.70, label = "50% of Values", hjust = 0, size = 2.2) +
  
  # Yellow + orange (80%)
  annotate("rect", xmin = 0.07, xmax = 0.23, ymin = 4.1, ymax = 4.4, 
           fill = "orange", color = "black") +
  annotate("rect", xmin = 0.11, xmax = 0.19, ymin = 4.1, ymax = 4.4, 
           fill = "gold", color = "black") +
  annotate("text",  x = 0.30, y = 4.25, label = "80% of Values", hjust = 0, size = 2.2) +
  
  # Yellow + orange + red (95%)
  annotate("rect", xmin = 0.03, xmax = 0.27, ymin = 3.6, ymax = 3.9, 
           fill = "red", color = "black") +
  annotate("rect", xmin = 0.07, xmax = 0.23, ymin = 3.6, ymax = 3.9, 
           fill = "orange", color = "black") +
  annotate("rect", xmin = 0.11, xmax = 0.19, ymin = 3.6, ymax = 3.9, 
           fill = "gold", color = "black") +
  annotate("text",  x = 0.3, y = 3.75, label = "95% of Values", hjust = 0, size = 2.2) +
  
  # Gray distribution
  annotate("polygon",
           x = c(0.1, 0.13, 0.15, 0.17, 0.20),
           y = c(3.1, 3.4, 3.6, 3.4, 3.1),
           fill = "grey70", alpha = 0.7, color = NA) +
  annotate("text",  x = 0.30, y = 3.3, label = "Distribution of Values", hjust = 0, size = 2.2) +
  
  # Layout
  coord_cartesian(xlim = c(0,1), ylim = c(3,5.5), expand = FALSE) +
  theme_void() +
  labs(title = "Legend") +
  theme(
    plot.title = element_text(size = 9, hjust = 0.05),
    plot.background = element_rect(color = "grey30", size = 0.2, fill = "white")
  )


Full_p<-p + inset_element(p_legend, l = 0.6, r = 1,  t = 1, b = 0.5, clip = FALSE)
Full_p

ggsave(paste0(path,in_weather,"WeatherData_plot.jpeg"),Full_p)
ggsave(paste0(path,results,"WeatherData_plot.jpeg"),Full_p)



#' 
#' # 16.RX Burning Modeling
#'     
#'   #Set target canopy consumption for Redbook (0-100) or target crown fire probability for cfis model (0-100%)
#'   -This code will run fire behavior over all weather conditions, not just extreme, and generate upper and lower bounds of burning based on targets
#'   16.1 Run Models
## -----------------------------------------------------------------------------
#Modelling over post treatment file as burning will happen after treatment
#Load Structure
PreTreat_df<-read.csv(paste0(path,Fuel_prefix,"Pre_Treatment_Structure_Data.csv"))
PostTreat_df<-read.csv(paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"))
PostTreat_df$USModel<-c("TU2","TU2")

#Modifiables:
Elevation<-750
name<- "TR_LionsBurn"
PostTreat_df$Fine_Fuel_kg<-c(1,1) #Set Target Fuel Loads
CustomFuels<-c(FALSE,FALSE)
BurnSeason<-"Spring"
BurnHour<-10
FFM_Type="Other" #Type of calculation for fine fuel moisture either "Wotton" for calculation based on stand adjusted FFMC or "Other" using models from Anderson and more with Temp and RH

# TARGET SETTINGS
TargetFlagUpper = "CFIS" # Options: "FBP", "CFIS", "CFIS_2.0", "Nelson","SR"
TargetFlagLower = "SR" # Options: "FBP", "CFIS", "CFIS_2.0", "Nelson", "SR"


TargetMetricUpper = "Flame_Length" # Options: "CFC" (Crown Fraction Consumed), "CFB" (Crown Fraction Burned),"Intensity_Crown", "Intensity_Surface", "ROS", "Flame_Length", "pCrown", "SFC", "CritFlameLength", "FireFlag", "CritFlameLength", "FuelConsumed_kg_m2

TargetMetricLower = "FuelConsumed_kg_m2" # Options: "CFC" (Crown Fraction Consumed), "CFB" (Crown Fraction Burned), "Intensity_Crown", "Intensity_Surface", "ROS", "Flame_Length", "pCrown", "SFC", "FireFlag", "CritFlameLength", "FuelConsumed_kg_m2

UpperTarget = 2 # Upper threshold - exclude days above this
LowerTarget = 0.50  # Lower threshold - exclude days below this (set to 0 for no lower limit)

#---------------------------------------------------
#read in FWI Data
daily_weather<- read.csv(paste0(path,out_weather,project,"_Daily_FWI_AllYear.csv"))
#daily_weather<- read.csv(paste0(path,out_weather,"Weather_BU2_V2.csv"))
weather_list<-daily_weather %>% 
  dplyr::filter(MON %in% c(4, 5, 6, 7, 8, 9, 10))

#sample
#weather_list<-daily_weather %>% dplyr::filter(MON %in% c(4,5,6,7,8,9,10)) %>%
 # sample_n(300)
#sample around median
weather_list <- weather_list %>%
  filter(
    TEMP >= quantile(TEMP, 0.15, na.rm = TRUE) & 
    TEMP <= quantile(TEMP, 0.95, na.rm = TRUE),
    RH >= quantile(RH, 0.15, na.rm = TRUE) & 
    RH <= quantile(RH, 0.95, na.rm = TRUE)
  ) %>%
  sample_n(min(500, n()))  

#bring in fuel data
strata_data<-PostTreat_df

# Initialize results
results_list <- list()
filtered_days_log <- list()

# Define the number of treatments and weather conditions
strata <- unique(strata_data$Stratum)

#Setup progress bar
total_iters <- nrow(weather_list) * length(strata)
pb <- progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total  = total_iters,
  clear  = FALSE,
  width  = 60
)
#initialize
iter <- 0
i
suppressWarnings({

# Iterate over weather conditions
for (i in 375:nrow(weather_list)) {
  #Iterate over stratas
  for(j in 1:length(strata)){
    
  # Extract weather data for this iteration
  date <- as.Date(weather_list$DATE[i])
  BUI <- weather_list$BUI[i]
  FFMC <- weather_list$FFMC[i]
  DMC <- weather_list$DMC[i]
  DC <- weather_list$DC[i]
  ISI <- weather_list$ISI[i]
  FWI<-weather_list$FWI[i]
  WSOG <- weather_list$WS[i]
  #WS<-WSOG
  #apply windspeed gust modifier
  WS_Max<-Calculate_Gust(WSOG,units="kmh")$OneMinuteMax
  WS_Gust<-Calculate_Gust(WSOG,units="kmh")$GustAverage
  WS<-mean(WS_Max,WS_Gust)
  if(WS<0){WS=5}
 
  WD <- weather_list$WD[i]
  TEMP <- weather_list$TEMP[i]
  RH <- weather_list$RH[i]
  Lat <- weather_list$LAT[i]
  Lon <- weather_list$LONG[i]
  J_date <- as.numeric(strftime(date, format = "%j"))

  
    STRATA<-strata[j]
    strata_input <- strata_data %>% filter(Stratum == strata[j])
    
    #extract weather
    Weather<-data.frame(TEMP=TEMP,
                        RH=RH,
                        WS=WS,
                        WD=WD,
                        FFMC=FFMC,
                        ISI=ISI,
                        DC=DC,
                        DMC=DMC,
                        BUI=BUI,
                        FWI=FWI,
                        WSOG=WSOG)
    GrassCuring<-weather_list$grass_curing[i]
    if(is.na(GrassCuring)){GrassCuring<-50}
    
    #Get Elevation and Foliar Moisture Content
    Elev <- Elevation
    FMC <- FMC_calc(Lat, Lon, ELEV = Elev, DATE = date)
    
    # Extract fuel information and change to Megagrams per hectare
    ffl_kg <- strata_input$Fine_Fuel_kg+strata_input$Herb_Loading+strata_input$Grass_Loading+strata_input$Shrub_Loading
    
    #check change in fuel loads by ratio and proportionately change fuel load classes for post treatment
    ratio=(strata_input$hr_1_kg + strata_input$hr_10_kg + strata_input$hr_100_kg)/strata_input$Fine_Fuel_kg
    hr1_Mg <- (strata_input$hr_1_kg)/ratio * 10
    hr10_Mg <- (strata_input$hr_10_kg)/ratio * 10
    hr100_Mg <- (strata_input$hr_100_kg)/ratio * 10
    
    litter_Mg <- strata_input$Lit_kg * 10
    FB_depth_cm <- strata_input$FB_depth
    
    # Calculate Surface Fuel Consumption (SFC) from 2 ways:
    #Wotton 2007
    sfc_n <- sfc(fueltype = strata_input$FT_Can, dc = DC, ffmc = FFMC, ffl = ffl_kg, bui = BUI, depth = FB_depth_cm, dmc = DMC, bd = 0)
    
    #Canadian fbp
    #check if grass exists if not use herb + half shrub
    if(!is.na(strata_input$Grass_Loading)){
      grassload<-strata_input$Grass_Loading
    }else{
      grassload<-strata_input$Herb_Loading+strata_input$Shrub_Loading/2
    }
    
    SFC_FBP<-surface_fuel_consumption(FUELTYPE = strata_input$FT_Can,
                                      FFMC=FFMC,
                                      BUI=BUI,
                                      PC= GrassCuring, GFL= grassload)
    
    # Calculate Effective Fine Fuel Moisture (EFFM)
    Density<- ifelse(strata_input$CC< 45,"Light",
                   ifelse(strata_input$CC> 45| strata_input$CC< 60 ,"Moderate",
                          ifelse(strata_input$CC> 60 ,"Dense")))
    Season<- BurnSeason
    ffm1_Wotton<-FFMC_sa(strata_input$Forest_Type, Season, ffmc = FFMC,dmc = DMC, density = Density)
    
    #or
    effm<- ffm(method="anderson",
             rh=RH,
             temp=TEMP,
             month=as.numeric(substr(date,6,7)),
             hour=BurnHour,
             asp=strata_input$Aspect,
             slp=strata_input$Slope,
             bla="b",
             shade="yes")
    
    hr1ffm<-ifelse(FFM_Type=="Wotton", ffm1_Wotton,effm$fm1hr)
    
    #PreTreat_df$EFFM[j]<-hr1ffm
  
    #Calculate effective heat of combustion from: Babrauskkas 2006 since burning when foliar moisture content is important
    H<- (16.52-0.057*FMC)*1000
    
    #FSG
    FSG<-strata_input$FSG_Mod

    # Calculate Growing Season Index (GSI) and Live Fuel Moisture from full dataset
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
    
    if(!is.na(strata_input$USModel)){
      RothModel<-strata_input$USModel  
    }else{
      BestUSModel<-find_best_model(Plot_fuels,Density=Density, SurfFuelType=strata_input$SurfFuel, ForestType=strata_input$Forest_Type)
      RothModel<-BestUSModel$FuelModel
    }
    
    model_roth<-RothModel
    model<-model_roth
    
    #select model back from dataset: USE PRESET MODEL FUELS or dont reload your fuels
    Plot_fuels<-rbind(fuelModels[model_roth,1:16])
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

    #Run Nelson's reaction time model
    fuel<-data.frame(litter=Plot_fuels$loadLitter,hr1=Plot_fuels$load1hr,hr10=Plot_fuels$load10hr,hr100=Plot_fuels$load100hr,depth=FB_depth_cm,CAN_model=strata_input$FT_Can)
    
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
    
    structure<-data.frame(ht=strata_input$Height,cc=strata_input$CC,cr=Crown_Ratio,dbh=strata_input$DBH,cbh=strata_input$Fuelcalc_CBH,cbd=strata_input$CBD,cfl=strata_input$CFL,ba= strata_input$BA,sd=strata_input$TPH)
    
    weather<-data.frame(temp=TEMP, rh=RH,t_min=T_min,dc=DC,dmc=DMC,bui=BUI,ffm=effm$fm1hr,ws=WS,wd=WD)
    
    Nelson<-residence_time(
      model,
      fuel,
      topography,
      structure,
      weather,
      date, 
      IntensityType = "Byram", 
      UseModel = TRUE,
      ModelEFFM = FALSE)
    
    #get rate of spread from Nelson
    ROS_Nelson<-Nelson$RateOfSpread.m.s*60
  
    #Calculate effective heat of combustion or get it from nelson
    #H<-Nelson$Mean.Heat.Combust
    
    
    mc_vals <- c(effm$fmLitter, effm$fm1hr, effm$fm10hr, effm$fm100hr) / 100 
    sav_vals <- c(Plot_fuels$savLitter, Plot_fuels$sav1hr, Plot_fuels$sav10hr, Plot_fuels$sav100hr)
    load_vals<-c(Plot_fuels$loadLitter, Plot_fuels$load1hr, Plot_fuels$load10hr, Plot_fuels$load100hr)
    
    Plot_sfc <- data.frame(SFC = sfc_n, SFC_FBP=SFC_FBP, BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s)
    Plot_sfc$SFC_Nelson<- Plot_sfc$BurningRate*(Nelson$Residence.Time.s/Nelson$Depth.Reaction.Zone.m)

    # Run the fire behavior models
    if(Plot_sfc$SFC > ffl_kg | Plot_sfc$SFC_FBP > ffl_kg){
      Plot_sfc<-data.frame(SFC = ffl_kg, SFC_FBP=ffl_kg, BurningRate=Nelson$Surface.Area.Burning.Rate.kg..m2.s, SFC_Nelson=Plot_sfc$BurningRate*(Nelson$Residence.Time.s/Nelson$Depth.Reaction.Zone.m))
    }
   
    ScottReinhardt<- rothermel_mod(surfFuel = Plot_fuels, moisture = Plot_moisture, crownFuel = Plot_crown, enviro = Plot_stand, rosMult = 1.7, cfbForm = "w", folMoist = "n")

    ScottReinhardt$fireBehavior$Model_type<- "Rothermel"
    ScottReinhardt$fireBehavior$FuelType<-model_roth
    ScottReinhardt$fireBehavior$SFC<-Plot_sfc$SFC
    
    #Extract SR Results
    RothermelModel<-as.data.frame(cbind(ScottReinhardt$fireBehavior$`Type of Fire`,ScottReinhardt$fireBehavior$`Crown Fraction Burned [%]`,ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`,ScottReinhardt$fireBehavior$`Fireline Intensity [kW/m]`,ScottReinhardt$critInit$`Fireline Intensity [kW/m]`,ScottReinhardt$critInit$`Flame length (m)`,ScottReinhardt$fireBehavior$`Heat per Unit Area [kJ/m2]`))
    names(RothermelModel)<- c("FireType","CFB","ROS","ReactionIntensity","CritIntensity","CritFlameLength","HeatperUnitArea")
    RothermelModel <- RothermelModel %>%
  mutate(across(-FireType, as.numeric))
    
    
    #Recalculate Intensity with fuel and heat
    RothermelModel$FuelConsumed_kg_m2<-RothermelModel$HeatperUnitArea/H
    RothermelModel$FireLineIntensity<-H*RothermelModel$FuelConsumed_kg_m2*RothermelModel$ROS/60
    
    if(RothermelModel$FuelConsumed_kg_m2>ffl_kg){
      RothermelModel$FuelConsumed_kg_m2<-ffl_kg
    }
    
    load<-Plot_fuels %>% dplyr::select(load1hr,load10hr,load100hr,loadLiveHerb,loadLiveWoody)
    sav<-Plot_fuels %>%dplyr::select(sav1hr,sav10hr,sav100hr,savLiveHerb,savLiveWoody)
    depth<-Plot_fuels$fuelBedDepth
    mxdead<-Plot_fuels %>% dplyr::select(mxDead)
    heat<-c(rep(H,3),19500,20000)
    moist<- cbind(effm$fm1hr,effm$fm10hr,effm$fm100hr,herb_live,woody_live)
    u1<-WS_mid
    slp<-Plot_stand$slope
    WindSpeed<-WS
    
    Rothermel_ros<-ros(modeltype = "D",
                       w=unlist(unname(as.vector(load))),
                       s=unlist(unname(as.vector(sav))),
                       delta=unlist(unname(as.vector(depth))),
                       mx.dead = unlist(unname(as.vector(mxdead))),
                       h=heat,
                       m=as.vector(moist),
                       u=as.vector(wind_adj*WindSpeed),
                       slope=as.vector(slp))
    
    #Get Intensity
    p_f<-398
    CharFract<-0.1925
    
    PackingRatio<- (RothermelModel$FuelConsumed_kg_m2)/(strata_input$FB_depth * p_f* (1-CharFract))
    
    #Nelson's Intensity
    I_B_NELSON<-0.85*H*(1 - CharFract)*p_f*RothermelModel$FuelConsumed_kg_m2*PackingRatio*Rothermel_ros$`ROS [m/min]`/60    
    
    #Byram's FireLine Intensity
    I_B_BYRAM<-H*(RothermelModel$FuelConsumed_kg_m2)*Rothermel_ros$`ROS [m/min]`/60
  
    #Rothermel's Reaction Intensity
    I_B_ROTHERMEL<-RothermelModel$ReactionIntensity
    
    #Update Surface Fuel Consumed:
    
    Plot_sfc$SFC<-RothermelModel$FuelConsumed_kg_m2
    
    
    
   #FBP Function:
  FBP_df<-data.frame(
    FuelType=ifelse(strata_input$FT_Can == "D-1/2","D-1",ifelse(strata_input$FT_Can == "M-1/2","M-1",strata_input$FT_Can)),
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
  if("grass_curing" %in% colnames(weather_list)){
    FBP_df<-cbind(FBP_df,data.frame(cc=weather_list$grass_curing[i]))
  }

  FBP<-cffdrs::fbp(FBP_df,output = "ALL")
  FBP$FuelType<-strata_input$FT_Can
  FBP$Model_type<-"FBP"
  FBP$Type<- ifelse(FBP$FD == "C","active",
                             ifelse(FBP$FD == "S", "surface", "passive"))

    #CFIS Original
    CFIS<- firebehavioR::cfis(fsg = FSG, sfc = Plot_sfc$SFC, effm = Plot_moisture$hr1, u10 = WS, cbd = Plot_crown$cbd, id = 1)
    CFIS$Model_type<- "CFIS"
    CFIS$Surface_ROS<-RothermelModel$ROS
 
    #CFIS 2.0: Perrakis 2023 Logistic Crown + Cruz 2005 Spread
    CFIS_2.0 <- cfis_modified(fsg = FSG, sfc = Plot_sfc$SFC, effm = Plot_moisture$hr1, u10 = WS, cbd = Plot_crown$cbd, id = 5, adjusted = TRUE)
    CFIS_2.0$Model_type<-"CFIS_2.0"
    CFIS_2.0$Surface_ROS<-RothermelModel$ROS
  

    #Calculate Crown Fraction Consumed
    FireType<-ifelse(CFIS$type == "active" | CFIS$type == "passive","crown","surface")
    CFC<-ifelse(FireType == "crown",CFC_Calc(CFIS$cROS,strata_input$CFL)$CFC,CFC_Calc(CFIS$Surface_ROS,strata_input$CFL)$CFC)

    FireType_2.0<-ifelse(CFIS_2.0$type == "active" | CFIS_2.0$type == "passive","crown","surface")
    CFC_2.0<-ifelse(FireType_2.0 == "crown",CFC_Calc(CFIS_2.0$cROS,strata_input$CFL)$CFC,CFC_Calc(CFIS_2.0$Surface_ROS,strata_input$CFL)$CFC)

    #Calculate Intensity, Critical Intensity, Flame Length:Use Byram's Intensity Equation
    CFIS$Intensity_Surface<-I_B_BYRAM
    CFIS$Intensity_Crown<-(CFIS$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS$cROS/60
    CFIS$Critical_Int<-0.001*(strata_input$FSG^1.5)*(460+25.9*FMC)^1.5
    CFIS$CFC<-CFC
    CFIS$SFC<-RothermelModel$FuelConsumed_kg_m2  # Surface fuel consumption
    R_0<-3/Plot_crown$cbd
    CAC<-CFIS$cROS/R_0
    CFIS$ROS <-ifelse(is.na(CFIS$cROS),CFIS$Surface_ROS, CFIS$cROS * exp(-CAC_2.0))
    CFIS$Intensity<-ifelse(is.na(CFIS$Intensity_Crown),CFIS$Intensity_Surface,(CFIS$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS$ROS/60)
  CFIS$Flame_Length<-0.0775*(CFIS$Intensity^0.46)
  CFIS$CrownScorch<-(4.4713*CFIS$Intensity^0.667)/(60-TEMP)

    CFIS_2.0$Intensity_Surface<-I_B_BYRAM
    CFIS_2.0$Intensity_Crown<-(CFIS_2.0$Intensity_Surface+(CFC_2.0 * 0.20482 * H * 0.430265))*11.349*CFIS_2.0$cROS/60
    CFIS_2.0$Critical_Int<-0.001*(strata_input$FSG^1.5)*(460+25.9*FMC)^1.5
    CFIS_2.0$CFC<-CFC_2.0
    CFIS_2.0$SFC<-RothermelModel$FuelConsumed_kg_m2  # Surface fuel consumption
    CAC_2.0<-CFIS_2.0$cROS/R_0
    CFIS_2.0$ROS <-ifelse(is.na(CFIS_2.0$cROS),CFIS_2.0$Surface_ROS, CFIS_2.0$cROS * exp(-CAC_2.0))
    CFIS_2.0$Intensity<-ifelse(is.na(CFIS_2.0$Intensity_Crown),CFIS_2.0$Intensity_Surface,(CFIS_2.0$Intensity_Surface+(CFC * 0.20482 * H * 0.430265))*11.349*CFIS_2.0$ROS/60)
    CFIS_2.0$Flame_Length<-0.0775*(CFIS_2.0$Intensity^0.46)
     CFIS_2.0$CrownScorch<-(4.4713*CFIS_2.0$Intensity^0.667)/(60-TEMP)
    
    # ==========================================================================
    # TARGET FILTERING LOGIC
    # ==========================================================================
    
    # Select which model to use for filtering
    if(TargetFlagUpper == "FBP"){
      filter_modelUp <- FBP
    } else if(TargetFlagUpper == "CFIS"){
      filter_modelUp <- CFIS
    } else if(TargetFlagUpper == "CFIS_2.0"){
      filter_modelUp <- CFIS_2.0
    } else if(TargetFlagUpper == "Nelson"){
      filter_modelUp <- Nelson
    }else if(TargetFlagUpper == "SR"){
      filter_modelUp <- RothermelModel
    }
    
    if(TargetFlagLower == "FBP"){
      filter_modelLow <- FBP
    } else if(TargetFlagLower == "CFIS"){
      filter_modelLow <- CFIS
    } else if(TargetFlagLower == "CFIS_2.0"){
      filter_modelLow <- CFIS_2.0
    } else if(TargetFlagLower == "Nelson"){
      filter_modelLow <- Nelson
    }else if(TargetFlagLower == "SR"){
      filter_modelLow <- RothermelModel
    }
    
    
    # Extract the metric values for upper and lower targets
    metric_value_upper <- extract_metric(filter_modelUp, TargetMetricUpper)
    metric_value_lower <- extract_metric(filter_modelLow, TargetMetricLower)
 
    
    # Check if the day meets the target criteria
    # Upper metric must be <= UpperTarget
    # Lower metric must be >= LowerTarget
    meets_upper_target <- metric_value_upper <= UpperTarget
    if(is.character(LowerTarget)){
   meets_lower_target <- metric_value_lower == LowerTarget
    } else {
   meets_lower_target <- metric_value_lower >= LowerTarget
    }
    meets_target <- meets_upper_target & meets_lower_target
    
    # Log the filtering decision
    filter_log <- data.frame(
      Date = date,
      Stratum = STRATA,
      Model = TargetFlagUpper,
      MetricUpper = TargetMetricUpper,
      ValueUpper = metric_value_upper,
      UpperTarget = UpperTarget,
      MeetsUpperTarget = meets_upper_target,
      MetricLower = TargetMetricLower,
      ValueLower = metric_value_lower,
      LowerTarget = LowerTarget,
      MeetsLowerTarget = meets_lower_target,
      MeetsTarget = meets_target
    )
    
    filtered_days_log[[paste("Strata", STRATA, "Weather", i, sep = "_")]] <- filter_log
    
    # Only store results if the day meets the target criteria
    if(meets_target){
      results_list[[paste("Strata_", STRATA, "Weather", i, sep = "_")]] <- list(
        Stratum= STRATA,
        Date = date,
        CANModel=strata_input$FT_Can,
        USModel=model_roth,
        ScottReinhardt = ScottReinhardt,
        CFIS = CFIS,
        CFIS_2.0=CFIS_2.0,
        FBP=FBP,
        Weather=Weather,
        Plot_sfc = Plot_sfc,
        Plot_fuels = Plot_fuels,
        Plot_moisture = Plot_moisture,
        Plot_crown = Plot_crown,
        Plot_stand = Plot_stand,
        Plot_nelson = Nelson,
        FilterMetricUpper = metric_value_upper,
        FilterMetricLower = metric_value_lower
      )
    }
    
    # Tick the progress bar:
    iter <- iter + 1
    pb$tick()
  }
}
})


# Save results and filtering log
saveRDS(results_list, paste0(path, Fire_out, "Results_Burning_Filtered.rds"))
saveRDS(filtered_days_log, paste0(path, Fire_out, "Filtering_Log.rds"))

CFIS

#'   
#'   16.2 Extract Results
## -----------------------------------------------------------------------------
#Read in Results
BurnResults<-readRDS(paste0(path, Fire_out, "Results_Burning_Filtered.rds"))

#Aggregate and summarize:
#Extract Results:
BurnRes <- rbindlist(
    lapply(names(BurnResults), function(name) {
      res <- BurnResults[[name]]
    
      # Extract WeatherList names
      strata_match <- str_match(name, "Strata__?(.*?)_")[,2]
      weather_match <- str_match(name, "Weather[_=](\\d+)")[,2]
    
    # Return as data.table row
      data.table(
        Strata = strata_match,
        Weather = weather_match,
      
      # CFIS metrics
       CFIS.Type = res$CFIS$type,
        CFIS.pCrown = res$CFIS$pCrown,
        CFIS.cROS = res$CFIS$cROS,
        CFIS.sROS = res$CFIS$Surface_ROS,
        CFIS.ROS = res$CFIS$ROS,  
        CFIS.cIntensity = res$CFIS$Intensity_Crown,
        CFIS.sIntensity = res$CFIS$Intensity_Surface,
        CFIS.Intensity = res$CFIS$Intensity,
        CFIS.CritIntensity = res$CFIS$Critical_Int,
        CFIS.FlameLength = res$CFIS$Flame_Length,
        CFIS.ScorchHeight = res$CFIS$CrownScorch,
      
      # CFIS 2.0 metrics
        CFIS2.Type = res$CFIS_2.0$type,
        CFIS2.pCrown = res$CFIS_2.0$pCrown,
        CFIS2.cROS = res$CFIS_2.0$cROS,
        CFIS2.sROS = res$CFIS_2.0$Surface_ROS,
        CFIS2.ROS = res$CFIS_2.0$ROS,
        CFIS2.cIntensity = res$CFIS_2.0$Intensity_Crown,
        CFIS2.sIntensity = res$CFIS_2.0$Intensity_Surface,
        CFIS2.Intensity = res$CFIS_2.0$Surface,
        CFIS2.CritIntensity = res$CFIS_2.0$Critical_Int,
        CFIS2.FlameLength = res$CFIS_2.0$Flame_Length,
      
      #FBP Metrics:
       FBP.Type = res$FBP$FD,
         FBP.pCrown = res$FBP$CFB,
         FBP.cROS = res$FBP$ROS,
         FBP.sROS = res$FBP$ROS,
         FBP.cIntensity = res$FBP$HFI,
         FBP.sIntensity = res$FBP$HFI,
         FBP.FlameLength = 0.0775*(res$FBP$HFI^0.46),
      
      #ScottReinhardt Metrics:
        SR.Type = res$ScottReinhardt$fireBehavior$`Type of Fire`,
         SR.pCrown = res$ScottReinhardt$fireBehavior$`Crown Fraction Burned [%]`,
         SR.cROS = res$ScottReinhardt$fireBehavior$`Rate of Spread [m/min]`,
         SR.sROS = res$ScottReinhardt$detailSurface$`Potential ROS [m/min]`,
         SR.cIntensity = res$ScottReinhardt$fireBehavior$`Fireline Intensity [kW/m]`,
         SR.sIntensity = res$ScottReinhardt$fireBehavior$`Fireline Intensity [kW/m]`,
         SR.FlameLength = res$ScottReinhardt$fireBehavior$`Flame Length [m]`,
         SR.CritIntensity = res$ScottReinhardt$critInit$`Fireline Intensity [kW/m]`,
        SR.CrownIndexWS = res$ScottReinhardt$fireBehavior$`Crowning Index [km/hr]`,
        SR.CritFlameLength = res$ScottReinhardt$critInit$`Flame length (m)`,
      
      
      # Inputs metrics
        USModel=res$USModel,
        CANModel=res$CANModel,
        SFC=res$Plot_sfc$SFC,
        FuelMoist_1hr=res$Plot_moisture$hr1,
        FMC=res$Plot_crown$fmc,
        WSOG=res$Weather$WSOG,
        WS=res$Weather$WS,
        RH=res$Weather$RH,
        TEMP=res$Weather$TEMP,
        ISI=res$Weather$ISI,
        BUI=res$Weather$BUI,
        FWI=res$Weather$FWI,
        FFMC=res$Weather$FFMC,
        DMC=res$Weather$DMC,
        DC=res$Weather$DC,
        Nelson_BurningRate=res$Plot_nelson$Surface.Area.Burning.Rate.kg..m2.s,
        Nelson_Flag=res$Plot_nelson$FLAG
        
      )
      }),
      fill = TRUE
  )


#Summarise
#merge data frames to make graph
BurnRes$Period <- "Burn"


#export
write.csv(BurnRes,paste0(path,Fire_out,"FullBurnConditions_Data.csv"))

#' 
#'   16.3 Extract Burn Window
#'   FunctionViewer
## -----------------------------------------------------------------------------
plot_burn_window <- function(data, metric, metric_label = NULL) {
  
  # Auto-generate label if not provided
  if (is.null(metric_label)) {
    metric_label <- metric
  }
  
  # Calculate statistics for the selected metric
  median_val <- median(data[[metric]], na.rm = TRUE)
  mean_val <- mean(data[[metric]], na.rm = TRUE)
  q25 <- quantile(data[[metric]], 0.25, na.rm = TRUE)
  q75 <- quantile(data[[metric]], 0.75, na.rm = TRUE)
  min_val <- min(data[[metric]], na.rm = TRUE)
  max_val <- max(data[[metric]], na.rm = TRUE)
  
  # Calculate weather statistics
  weather_stats <- data.frame(
    Variable = c("Wind Speed", "ISI", "BUI", "FWI","Temperature" ,"FFMC", "1-hr Fuel Moist.", "RH","DMC","DC"),
    Min = c(
      round(min(data$WS, na.rm = TRUE), 1),
      round(min(data$ISI, na.rm = TRUE), 1),
      round(min(data$BUI, na.rm = TRUE), 1),
      round(min(data$FWI, na.rm = TRUE), 1),
      round(min(data$TEMP, na.rm = TRUE), 1),
      round(min(data$FFMC, na.rm = TRUE), 1),
      round(max(data$FuelMoist_1hr, na.rm = TRUE), 1),
      round(max(data$RH, na.rm = TRUE), 1),
      round(min(data$DMC, na.rm = TRUE), 1),
      round(min(data$DC, na.rm = TRUE), 1)
    ),
    Med = c(
      round(median(data$WS, na.rm = TRUE), 1),
      round(median(data$ISI, na.rm = TRUE), 1),
      round(median(data$BUI, na.rm = TRUE), 1),
      round(median(data$FWI, na.rm = TRUE), 1),
      round(median(data$TEMP, na.rm = TRUE), 1),
      round(median(data$FFMC, na.rm = TRUE), 1),
      round(median(data$FuelMoist_1hr, na.rm = TRUE), 1),
      round(median(data$RH, na.rm = TRUE), 1),
      round(median(data$DMC, na.rm = TRUE), 1),
      round(median(data$DC, na.rm = TRUE), 1)
    ),
    Max = c(
      round(max(data$WS, na.rm = TRUE), 1),
      round(max(data$ISI, na.rm = TRUE), 1),
      round(max(data$BUI, na.rm = TRUE), 1),
      round(max(data$FWI, na.rm = TRUE), 1),
      round(max(data$TEMP, na.rm = TRUE), 1),
      round(max(data$FFMC, na.rm = TRUE), 1),
      round(min(data$FuelMoist_1hr, na.rm = TRUE), 1),
      round(min(data$RH, na.rm = TRUE), 1),
      round(max(data$DMC, na.rm = TRUE), 1),
      round(max(data$DC, na.rm = TRUE), 1)
    )
  )
  
  # Create weather table as text with smaller, more compact format
  weather_text <- paste0(
    "Weather Conditions Range:\n",
    sprintf("%-16s %5s %5s %5s\n", "Variable", "Min", "Med", "Max"),
    paste0(sprintf("%-16s %5.1f %5.1f %5.1f", 
                   weather_stats$Variable, 
                   weather_stats$Min, 
                   weather_stats$Med, 
                   weather_stats$Max), 
           collapse = "\n")
  )
  
  # Create the main histogram
  p <- ggplot(data, aes(x = !!sym(metric))) +
  geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = median_val), 
             color = "red", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = median_val, y = Inf, 
           label = paste0("Median: ", round(median_val, 2)),
           vjust = 1.5, hjust = 0.1, color = "red", size = 2, fontface = "bold") +
  
  # Add weather stats box in top-right corner - moved right and up
  annotation_custom(
    grob = grid::textGrob(
      weather_text,
      x = 1.15, y = 1.15,
      just = c("right", "top"),
      gp = grid::gpar(fontsize = 7, fontfamily = "mono", col = "gray20")
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  
  # Add summary stats box in bottom-left instead
  annotate("text", x = -Inf, y = 0, 
           label = sprintf("Min: %.2f | Q25: %.2f | Q75: %.2f | Max: %.2f | n = %d",
                         min_val, q25, q75, max_val, nrow(data)),
           hjust = -0.02, vjust = -0.5, size = 3.5, color = "gray30") +
  
  labs(
    title = paste("Distribution of", metric_label),
    subtitle = "Burn Window Conditions",
    x = metric_label,
    y = "Count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0),
    axis.title = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(20, 80, 20, 20)  # Extra right margin for weather box
  ) +
  coord_cartesian(clip = "off") 
  # Return the ggplot object (works better with RMarkdown)
  return(list(Plot=p,Weather=weather_stats))
}


plot_weather_window <- function(data, metric, metric_label = NULL) {
  
  # Auto-generate label if not provided
  if (is.null(metric_label)) {
    metric_label <- metric
  }
  
  # Calculate statistics for the selected metric
  median_val <- median(data[[metric]], na.rm = TRUE)
  mean_val<- mean(data[[metric]], na.rm = TRUE)
  q25 <- quantile(data[[metric]], 0.25, na.rm = TRUE)
  q75 <- quantile(data[[metric]], 0.75, na.rm = TRUE)
  min_val <- min(data[[metric]], na.rm = TRUE)
  max_val <- max(data[[metric]], na.rm = TRUE)
  
  # Calculate weather statistics for all variables
  weather_stats <- data.frame(
    Variable = c("Wind Speed", "ISI", "BUI", "FWI", "Temperature", 
                 "FFMC", "1-hr Fuel Moist.", "RH", "DMC", "DC"),
    Min = c(
      round(min(data$WS, na.rm = TRUE), 1),
      round(min(data$ISI, na.rm = TRUE), 1),
      round(min(data$BUI, na.rm = TRUE), 1),
      round(min(data$FWI, na.rm = TRUE), 1),
      round(min(data$TEMP, na.rm = TRUE), 1),
      round(min(data$FFMC, na.rm = TRUE), 1),
      round(min(data$FuelMoist_1hr, na.rm = TRUE), 1),
      round(min(data$RH, na.rm = TRUE), 1),
      round(min(data$DMC, na.rm = TRUE), 1),
      round(min(data$DC, na.rm = TRUE), 1)
    ),
    Med = c(
      round(median(data$WS, na.rm = TRUE), 1),
      round(median(data$ISI, na.rm = TRUE), 1),
      round(median(data$BUI, na.rm = TRUE), 1),
      round(median(data$FWI, na.rm = TRUE), 1),
      round(median(data$TEMP, na.rm = TRUE), 1),
      round(median(data$FFMC, na.rm = TRUE), 1),
      round(median(data$FuelMoist_1hr, na.rm = TRUE), 1),
      round(median(data$RH, na.rm = TRUE), 1),
      round(median(data$DMC, na.rm = TRUE), 1),
      round(median(data$DC, na.rm = TRUE), 1)
    ),
    Max = c(
      round(max(data$WS, na.rm = TRUE), 1),
      round(max(data$ISI, na.rm = TRUE), 1),
      round(max(data$BUI, na.rm = TRUE), 1),
      round(max(data$FWI, na.rm = TRUE), 1),
      round(max(data$TEMP, na.rm = TRUE), 1),
      round(max(data$FFMC, na.rm = TRUE), 1),
      round(max(data$FuelMoist_1hr, na.rm = TRUE), 1),
      round(max(data$RH, na.rm = TRUE), 1),
      round(max(data$DMC, na.rm = TRUE), 1),
      round(max(data$DC, na.rm = TRUE), 1)
    )
  )
  
  # Create weather table as text
  weather_text <- paste0(
    "Weather Conditions Range:\n",
    sprintf("%-16s %5s %5s %5s\n", "Variable", "Min", "Med", "Max"),
    paste0(sprintf("%-16s %5.1f %5.1f %5.1f", 
                   weather_stats$Variable, 
                   weather_stats$Min, 
                   weather_stats$Med, 
                   weather_stats$Max), 
           collapse = "\n")
  )
  
  # Create the histogram plot
  p <- ggplot(data, aes(x = !!sym(metric))) +
    geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "white") +
    geom_vline(aes(xintercept = median_val), 
               color = "red", linetype = "dashed", linewidth = 1.2) +
    annotate("text", x = median_val, y = Inf, 
             label = paste0("Median: ", round(median_val, 2)),
             vjust = 1.5, hjust = -0.1, color = "red", size = 3, fontface = "bold") +
    annotate("text", x = mean_val, y = Inf, 
             label = paste0("Mean: ", round(mean_val, 2)),
             vjust = 3.5, hjust = -0.2, color = "orange", size = 3, fontface = "bold") +
    
    # Add weather stats box in top-right corner
    annotation_custom(
      grob = grid::textGrob(
        weather_text,
       x = 1.15, y = 1.15,
        just = c("right", "top"),
        gp = grid::gpar(fontsize = 7, fontfamily = "mono", col = "gray20")
      ),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    
    # Add summary stats box in bottom-left
    annotate("text", x = -Inf, y = 0, 
             label = sprintf("Min: %.2f | Q25: %.2f | Q75: %.2f | Max: %.2f | n = %d",
                           min_val, q25, q75, max_val, nrow(data)),
             hjust = -0.02, vjust = -0.5, size = 3.5, color = "gray30") +
    
    labs(
      title = paste("Distribution of Selected Weather Conditions"),
      subtitle = "Burn Window Weather Conditions",
      x = metric_label,
      y = "Count"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0),
      axis.title = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(20, 80, 20, 20)
    ) +
    coord_cartesian(clip = "off")
  
  # Return both plot and weather stats
  return(list(
    Plot = p,
    Weather = weather_stats
  ))
}



#' 
#'   -Decide on fuel loads and cutoffs again if need be
## -----------------------------------------------------------------------------
Data<-read.csv(paste0(path,Fire_out,"FullBurnConditions_Data.csv"))
Str<-read.csv(paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"))
#Get all weather data
Weather_Raw<-read.csv(paste0(path,out_weather,name,"_daily_FWI_AllYear.csv"))
  
#Conditions
UpperMetric="CFIS.FlameLength"
LowerMetric="SFC"
UpperValue=2
LowerValue=0.50

#Extract Upper Mean:
Data_filtered <- Data %>%
  filter(
    !!sym(UpperMetric) <= UpperValue,
    !!sym(LowerMetric) >= LowerValue
  )

# --- CHANGE THIS VARIABLE TO PLOT DIFFERENT METRICS ---
#Plot
plot_burn_window(Data_filtered, "CFIS.ROS", "Rate of Spread (SR) (Meters/Min)")$Plot
plot_burn_window(Data_filtered, "CFIS.FlameLength", "Flame Length (SR) (Meters)")$Plot
plot_burn_window(Data_filtered, "CFIS.pCrown", "CrownProb (CFIS) (%)")$Plot
plot_burn_window(Data_filtered, "CFIS.Intensity", "Intensity (CFIS) (kW/m)")$Plot

res<-plot_burn_window(Data_filtered, "CFIS.ROS", "Rate of Spread (SR) (Meters/Min)")


#Create Filtered Weather Data for BU2
WeatherLimits<-res$Weather
var_mapping <- c(
  "Wind Speed" = "WS",
  "ISI" = "ISI",
  "BUI" = "BUI",
  "FWI" = "FWI",
  "FFMC" = "FFMC",
  "RH" = "RH",
  "DMC" = "DMC",
  "DC" = "DC",
  "TEMP"= "TEMP")

NewWeather <- daily_weather %>% 
  dplyr::filter(MON %in% c(4, 5, 6, 7, 8, 9, 10)) %>%
  rowwise() %>%
  dplyr::mutate(
    metrics_in_range = sum(
      map_lgl(1:nrow(WeatherLimits), function(i) {
        var_name <- var_mapping[WeatherLimits$Variable[i]]
        if (!is.na(var_name) && var_name %in% names(cur_data())) {
          value <- cur_data()[[var_name]]
          !is.na(value) && value >= WeatherLimits$Min[i] && value <= WeatherLimits$Max[i]
        } else {
          FALSE
        }
      })
    )
  ) %>%
  ungroup() %>%
  dplyr::filter(metrics_in_range >= 4) %>%
  dplyr::select(-metrics_in_range)

write.csv(NewWeather,paste0(path,out_weather,"Weather_Burning.csv"))


#' 
#'   View Units:
#'   -Make sure fits within prescription
## -----------------------------------------------------------------------------
Data<-read.csv(paste0(path,Fire_out,"FullBurnConditions_Data.csv"))
Str<-read.csv(paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"))
#Get all weather data
Weather_Raw<-read.csv(paste0(path,out_weather,name,"_daily_FWI_AllYear.csv"))

  
#Conditions
UpperMetric="CFIS.FlameLength"
LowerMetric="SFC"
UpperValue=2
LowerValue=0.50

#Extract Upper Mean:
Data_filtered <- Data %>%
  filter(
    !!sym(UpperMetric) <= UpperValue,
    !!sym(LowerMetric) >= LowerValue
  )


plot_burn_window(Data_filtered, "CFIS.ROS", "Rate of Spread (SR) (Meters/Min)")$Plot
plot_burn_window(Data_filtered, "CFIS.FlameLength", "Flame Length (SR) (Meters)")$Plot
plot_burn_window(Data_filtered, "CFIS.pCrown", "CrownProb (CFIS) (%)")$Plot
plot_burn_window(Data_filtered, "CFIS.Intensity", "Intensity (CFIS) (kW/m)")$Plot


plot_weather_window(Data_filtered, "WS", "Wind Speed (km/hr)")$Plot
plot_weather_window(Data_filtered, "ISI", "InitalSpreadIndex")$Plot
plot_weather_window(Data_filtered, "DMC", "DuffMoistureCode")$Plot
plot_weather_window(Data_filtered, "DC", "DroughtCode")$Plot
plot_weather_window(Data_filtered, "FWI", "FireWeatherIndex")$Plot

plot_weather_window(Data_filtered, "BUI", "Buildup Index")$Plot
plot_weather_window(Data_filtered, "FuelMoist_1hr", "FineFuelMoisture (%)")$Plot
plot_weather_window(Data_filtered, "FFMC", "FineFuelMoistureCode")$Plot


#Decide on Ideal Fire Behaviour:
#Decide on Ideal Fire Behaviour:
BurnWindow <- Data_filtered[Data_filtered$SFC >= 0.50 & Data_filtered$SFC <= 1, ] %>% dplyr::select(-Weather,-X)

# Calculate mean, median, and range for all columns except SFC
numeric_cols <- sapply(BurnWindow, is.numeric)
numeric_cols["SFC"] <- FALSE  # Exclude SFC column

# Function to remove outliers using IQR method
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.1, na.rm = TRUE)
  Q3 <- quantile(x, 0.9, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  x[x >= lower_bound & x <= upper_bound]
}

# Calculate statistics
WindowSummary <- data.frame(
  Index = names(BurnWindow)[numeric_cols],
  Mean = round(sapply(BurnWindow[, numeric_cols], function(x) mean(remove_outliers(x), na.rm = TRUE)), 2),
  Median = round(sapply(BurnWindow[, numeric_cols], function(x) median(remove_outliers(x), na.rm = TRUE)), 2),
  Range_Min = round(sapply(BurnWindow[, numeric_cols], function(x) min(remove_outliers(x), na.rm = TRUE)), 2),
  Range_Max = round(sapply(BurnWindow[, numeric_cols], function(x) max(remove_outliers(x), na.rm = TRUE)), 2),
  Range_Span = round(sapply(BurnWindow[, numeric_cols], function(x) diff(range(remove_outliers(x), na.rm = TRUE))), 2)
)

# View the results
print(WindowSummary)

#BurnConditions DataOut
write.csv(WindowSummary,paste0(path,Fire_out,"BurnWindow_Full.csv"),row.names=FALSE)


#' 
#'   #Create Structure Pre and Post Treatment Files
## -----------------------------------------------------------------------------
PreTreat_df<-read.csv(paste0(path,Fuel_prefix,"Pre_Treatment_Structure_Data.csv"))
PostTreat_df<-read.csv(paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"))

#Modifiable:
PostTreat_df$Fine_Fuel_kg<-c(1,1)
stratas<-unique(PreTreat_df$Stratum)
TreatFlag<-c(TRUE,TRUE)


workbook <- openxlsx::createWorkbook()
library(openxlsx)
result_tables<-list()

for(i in 1:length(stratas)){
st<-stratas[i]
treatment_name<-st
#For Treatments  
if(TreatFlag[i]){
#setup df
Result_df <- data.frame(
  `Forest Structural Attribute` = c("CFFDRS Fuel Type (CAN)",
                                    "NFFDRS Fuel Type (USA)", 
                                    "Canopy Base Height (m)", 
                                    "Fuel Strata Gap (m)", 
                                    "Canopy Bulk Density (kg/m^3)", 
                                    "Canopy Fuel Load (kg/m^2)", 
                                    "Surface Fuel Load (1-100hr) (kg/m^3)",
                                    "Grass Loads (kg/m^2)",
                                    "Shrub Loads (kg/m^2)",
                                    "1000 Hour Fuels (kg/m^3)", 
                                    "Coarse Woody Fuel Pieces (ha)"),
  `Pre-Treatment` = rep(NA, 11),
  `Post-Treatment` = rep(NA, 11))

#add to data
PRE<- PreTreat_df %>% filter(Stratum == st)
POST<- PostTreat_df %>% filter(Stratum == st)

  
#pre
Result_df$Pre.Treatment[1]<-PRE$FT_Can
Result_df$Pre.Treatment[2]<-PRE$USModel
Result_df$Pre.Treatment[3]<-round(PRE$Fuelcalc_CBH,2)
Result_df$Pre.Treatment[4]<-round(PRE$FSG,2)
Result_df$Pre.Treatment[5]<-round(PRE$CBD,2)
Result_df$Pre.Treatment[6]<-round(PRE$CFL,2)
Result_df$Pre.Treatment[7]<-round(PRE$Fine_Fuel_kg,2)
Result_df$Pre.Treatment[8]<-round(PRE$Grass_Loading,2)
Result_df$Pre.Treatment[9]<-round(PRE$Shrub_Loading)
Result_df$Pre.Treatment[10]<-round(PRE$hr_1000_kg)
Result_df$Pre.Treatment[11]<-round(PRE$CWD_Pieces_5m_ha)

#post
Result_df$Post.Treatment[1]<-POST$FT_Can
Result_df$Post.Treatment[2]<-POST$USModel
Result_df$Post.Treatment[3]<-round(POST$Fuelcalc_CBH,2)
Result_df$Post.Treatment[4]<-round(POST$FSG,2)
Result_df$Post.Treatment[5]<-round(POST$CBD,2)
Result_df$Post.Treatment[6]<-round(POST$CFL,2)
Result_df$Post.Treatment[7]<-round(POST$Fine_Fuel_kg,2)
Result_df$Post.Treatment[8]<-round(POST$Grass_Loading,2)
Result_df$Post.Treatment[9]<-round(POST$Shrub_Loading)
Result_df$Post.Treatment[10]<-round(POST$hr_1000_kg)
Result_df$Post.Treatment[11]<-round(POST$CWD_Pieces_5m_ha)

#
current_table <- Result_df
cols<-3
}
else{
  #setup df
Result_df <- data.frame(
  `Forest Structural Attribute` = c("CFFDRS Fuel Type (CAN)",
                                    "NFFDRS Fuel Type (USA)", 
                                    "Canopy Base Height (m)", 
                                    "Fuel Strata Gap (m)", 
                                    "Canopy Bulk Density (kg/m^3)", 
                                    "Canopy Fuel Load (kg/m^2)", 
                                    "Surface Fuel Load (1-100hr) (kg/m^3)",
                                    "Grass Loads (kg/m^2)",
                                    "Shrub Loads (kg/m^2)",
                                    "1000 Hour Fuels (kg/m^3)", 
                                    "Coarse Woody Fuel Pieces (ha)"),
  `Pre-Burn` = rep(NA, 11))

#add to data
POST<- PostTreat_df %>% filter(Stratum == st)

#post
Result_df$Pre.Burn[1]<-POST$FT_Can
Result_df$Pre.Burn[2]<-POST$USModel
Result_df$Pre.Burn[3]<-round(POST$Fuelcalc_CBH,2)
Result_df$Pre.Burn[4]<-round(POST$FSG,2)
Result_df$Pre.Burn[5]<-round(POST$CBD,2)
Result_df$Pre.Burn[6]<-round(POST$CFL,2)
Result_df$Pre.Burn[7]<-round(POST$Fine_Fuel_kg,2)
Result_df$Pre.Burn[8]<-round(POST$Grass_Loading,2)
Result_df$Pre.Burn[9]<-round(POST$Shrub_Loading)
Result_df$Pre.Burn[10]<-round(POST$hr_1000_kg)
Result_df$Pre.Burn[11]<-round(POST$CWD_Pieces_5m_ha)

current_table <- Result_df
cols<-2
}

# Add a new sheet with the treatment name
  openxlsx::addWorksheet(workbook, sheetName = paste0("FTU_",treatment_name))
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "TopBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  
  # Write the treatment name as a header in the first row
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = paste("FTU_",treatment_name,": Stand Structure"), startRow = 1, startCol = 1)
  
  # Merge the header cells (across columns A to C) and apply header style
  openxlsx::mergeCells(workbook, sheet = paste0("FTU_",treatment_name), cols = 1:cols, rows = 1)
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = header_style, rows = 1, cols = 1:cols, gridExpand = TRUE)
  
  # Write the actual table below the header
  openxlsx::writeData(workbook, sheet = paste0("FTU_",treatment_name), x = current_table, startRow = 2, startCol = 1)
  
  # Apply table border style to the entire table
  openxlsx::addStyle(workbook, sheet = paste0("FTU_",treatment_name), style = table_border_style, 
           rows = 2:(2 + nrow(current_table)), cols = 1:ncol(current_table), gridExpand = TRUE)
}


# Save the workbook to an Excel file
openxlsx::saveWorkbook(workbook, paste0(path,results,"Treatment_Structures.xlsx"), overwrite = TRUE)

  

#'   
#'   #Create Fire Behavior Table
## -----------------------------------------------------------------------------
#Load Results
BurnWindow<-read.csv(paste0(path,Fire_out,"BurnWindow_Full.csv"))


workbook <- openxlsx::createWorkbook()
library(openxlsx)
result_tables<-list()

  #setup df
Result_df <- data.frame(
  Factor = c("Fire Type", "Rate of Spread (m/min)", "Wildfire Intensity (kW/m)", "Flame Length (m)","Scorch Height (m)"),
  `FTU-A` = rep(NA, 5),
  `FTU-B` = rep(NA, 5)
)

#select for treatment
#Res<-MasterData %>% filter(Strata == stratas[i])

#FTU-A
Result_df$FTU.A[1]<-"Prescribed"
Result_df$FTU.A[2]<-paste0(round(quantile(DataBU2$CFIS.pCrown,0.40),2),"-",round(quantile(DataBU2$CFIS.pCrown,0.90),2))
Result_df$FTU.A[3]<-paste0(round(quantile(DataBU2$SR.sROS,0.40),2),"-",round(quantile(DataBU2$SR.sROS,0.90),2))
Result_df$FTU.A[4]<-paste0(round(quantile(DataBU2$CFIS.sIntensity,0.40),2),"-",round(quantile(DataBU2$CFIS.sIntensity,0.90),2))
Result_df$FTU.A[5]<-paste0(round(quantile(DataBU2$CFIS.FlameLength,0.40),2),"-",round(quantile(DataBU2$CFIS.FlameLength,0.90),2))

#FTU-B
Result_df$FTU.B[1]<-"Prescribed"
Result_df$FTU.B[2]<-paste0(round(quantile(DataBU2X$CFIS.pCrown,0.40),2),"-",round(quantile(DataBU2X$CFIS.pCrown,0.90),2))
Result_df$FTU.B[3]<-paste0(round(quantile(DataBU2X$SR.sROS),2),"-",round(quantile(DataBU2X$SR.sROS,0.90),2))
Result_df$FTU.B[4]<-paste0(round(quantile(DataBU2X$CFIS.sIntensity,0.40),2),"-",round(quantile(DataBU2X$CFIS.sIntensity,0.90),2))
Result_df$FTU.B[5]<-paste0(round(quantile(DataBU2X$CFIS.FlameLength,0.40),2),"-",round(quantile(DataBU2X$CFIS.FlameLength,0.90),2))


#Rename Cols
colnames(Result_df)<-c("Factor","FTU-A","FTU-B")

#
current_table <- Result_df

# Add a new sheet with the treatment name
  openxlsx::addWorksheet(workbook, sheetName = paste0("All Stratas Quantile"))
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "topBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  
  # Write the treatment name as a header in the first row
  openxlsx::writeData(workbook, sheet = paste0("All Stratas Quantile"), x = paste("All Stratas: Predicted Fire Behaviour"), startRow = 1, startCol = 1)
  
  # Merge the header cells (across columns A to C) and apply header style
  openxlsx::mergeCells(workbook, sheet = paste0("All Stratas Quantile"), cols = 1:4, rows = 1)
  openxlsx::addStyle(workbook, sheet = paste0("All Stratas Quantile"), style = header_style, rows = 1, cols = 1:4, gridExpand = TRUE)
  
  # Write the actual table below the header
  openxlsx::writeData(workbook, sheet = paste0("All Stratas Quantile"), x = current_table, startRow = 2, startCol = 1)
  
  # Apply table border style to the entire table
  openxlsx::addStyle(workbook, sheet = paste0("All Stratas Quantile"), style = table_border_style, 
           rows = 2:(2 + nrow(current_table)), cols = 1:ncol(current_table), gridExpand = TRUE)
  
#Mean Page
  Result_df <- data.frame(
  Factor = c("Fire Type", "Crowning Percentage (%)", "Rate of Spread (m/min)", "Wildfire Intensity (kW/m)", "Flame Length (m)"),
  `BU-2` = rep(NA, 5),
  `BU-2(Treated)` = rep(NA, 5),
  `BU-3` = rep(NA, 5)
)

#select for treatment
#Res<-MasterData %>% filter(Strata == stratas[i])

#FTU-A
Result_df$BU.2[1]<-"Prescribed"
Result_df$BU.2[2]<-paste0(round(mean(DataBU2$CFIS.pCrown,na.rm=TRUE),2))
Result_df$BU.2[3]<-paste0(round(mean(DataBU2$SR.sROS,na.rm=TRUE),2))
Result_df$BU.2[4]<-paste0(round(mean(DataBU2$CFIS.sIntensity,na.rm=TRUE),2))
Result_df$BU.2[5]<-paste0(round(mean(DataBU2$CFIS.FlameLength,na.rm=TRUE),2))

#FTU-B
Result_df$BU.2.Treated.[1]<-"Prescribed"
Result_df$BU.2.Treated.[2]<-paste0(round(mean(DataBU2X$CFIS.pCrown,na.rm=TRUE),2))
Result_df$BU.2.Treated.[3]<-paste0(round(mean(DataBU2X$SR.sROS,na.rm=TRUE),2))
Result_df$BU.2.Treated.[4]<-paste0(round(mean(DataBU2X$CFIS.sIntensity,na.rm=TRUE),2))
Result_df$BU.2.Treated.[5]<-paste0(round(mean(DataBU2X$CFIS.FlameLength,na.rm=TRUE),2))



#Rename Cols
colnames(Result_df)<-c("Factor","FTU-A","FTU-B")

#
current_table <- Result_df

# Add a new sheet with the treatment name
  openxlsx::addWorksheet(workbook, sheetName = paste0("All Stratas Mean"))
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "topBottomLeftRight",
    borderColour = "black",
    fgFill = "lightgrey" # Optional: background color for header
  )
  
  # Style for table borders
  table_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight ",
    borderColour = "black"
  )
  
  # Write the treatment name as a header in the first row
  openxlsx::writeData(workbook, sheet = paste0("All Stratas Mean"), x = paste("All Stratas: Predicted Fire Behaviour"), startRow = 1, startCol = 1)
  
  # Merge the header cells (across columns A to C) and apply header style
  openxlsx::mergeCells(workbook, sheet =  paste0("All Stratas Mean"), cols = 1:4, rows = 1)
  openxlsx::addStyle(workbook, sheet =  paste0("All Stratas Mean"), style = header_style, rows = 1, cols = 1:4, gridExpand = TRUE)
  
  # Write the actual table below the header
  openxlsx::writeData(workbook, sheet =  paste0("All Stratas Mean"), x = current_table, startRow = 2, startCol = 1)
  
  # Apply table border style to the entire table
  openxlsx::addStyle(workbook, sheet =  paste0("All Stratas Mean"), style = table_border_style, 
           rows = 2:(2 + nrow(current_table)), cols = 1:ncol(current_table), gridExpand = TRUE)
  
# Save the workbook to an Excel file
openxlsx::saveWorkbook(workbook, paste0(path,results,"Predicted_Fire_Behavior.xlsx"), overwrite = TRUE)


#check crown scorch height


#' 
#'   #Create Burn Window and Weather Window Table
## -----------------------------------------------------------------------------
#Pull in All Weather
AllWeather<- read.csv(paste0(path,out_weather,name,"_Daily_FWI_AllYear.csv"))

burnwindow_dir<-paste0(path,results,"BurnWindowPlots/")

if(!dir.exists(burnwindow_dir)){
  dir.create(burnwindow_dir)
}

#Load Results
BurnWindow<-read.csv(paste0(path,Fire_out,"BurnWindow_Full.csv"))

#Extract Ranges
  WeatherLimits<-data.frame(
    variable=c("WS","FineFuelMoisture","TEMP","RH","FFMC"),
    Min=rep(NA,5),
    Max=rep(NA,5)
  )
  
  #Manual Changes:
  #Wind
  WeatherLimits$Min[1]<-8
  WeatherLimits$Max[1]<-20
  #FuelMoist (10hrs)
  WeatherLimits$Min[2]<-8
  WeatherLimits$Max[2]<-12
  #Temperature
  WeatherLimits$Min[3]<-5
  WeatherLimits$Max[3]<-30
  #RelativeHumdity
  WeatherLimits$Min[4]<-35
  WeatherLimits$Max[4]<-60
  #FFMC
  WeatherLimits$Min[5]<-70
  WeatherLimits$Max[5]<-90
  
  write.csv(WeatherLimits,paste0(path,out_weather,"BurnTable.csv"))
  
  #Calculate FFM
  
  AllWeather$effm_FTUA<- ffm(method="anderson",
             rh=AllWeather$RH,
             temp=AllWeather$TEMP,
             month=as.numeric(AllWeather$MON),
             hour=16,
             asp=Str$Aspect[1],
             slp=Str$Slope[1],
             bla="b",
             shade="yes")$fm10hr
  AllWeather$effm_FTUB<- ffm(method="anderson",
             rh=AllWeather$RH,
             temp=AllWeather$TEMP,
             month=as.numeric(AllWeather$MON),
             hour=16,
             asp=Str$Aspect[2],
             slp=Str$Slope[2],
             bla="b",
             shade="yes")$fm10hr
  
  AllWeather$FineFuelMoisture <- rowMeans(AllWeather[, c("effm_FTUA", "effm_FTUB")], na.rm = TRUE)

    
  
#Find Weather days within prescription
  #Choose data
    LimitData<-WeatherLimits_BU3
    Strata= "FTU-A"
    num_vars<-4
    total_vars<-5
  #Weather Window Plot
  date_starts <- c("03-01")  # Start dates (MM-DD format)
  date_ends <- c("06-01")    # End dates (MM-DD format)
  period_length <- 15
  Season="Spring"
  months<-c(3,4,5,6,7,8,9,10,11)
  
  Seasons<-c("Spring","Fall","All Season")
  Stratas<-c("FTU-A","FTU-B")
  
  for(Season in Seasons){
    
    if(Season == "All Season"){
        months<-c(3,4,5,9,10,11)
    date_starts <- c("03-01") 
     date_ends <- c("11-01")  
    }else if(Season == "Spring"){
      months<-c(3,4,5)
    date_starts <- c("03-01") 
     date_ends <- c("06-01")  
    }else{
      months<-c(9,10,11)
    date_starts <- c("09-01") 
     date_ends <- c("11-01")  
    }
    
  for(Strata in Stratas){  
    
  if(Strata == "FTU-A"){
    LimitData<-WeatherLimits

  }else{
    LimitData<-WeatherLimits
  }
    
BurnLikely <- AllWeather %>% 
  dplyr::filter(MON %in% months) %>%
  rowwise() %>%
  dplyr::mutate(
    metrics_in_range = sum(
      map_lgl(1:nrow(LimitData), function(i) {
        var_name <- LimitData$variable[i]
        if (!is.na(var_name) && var_name %in% names(cur_data())) {
          value <- cur_data()[[var_name]]
          !is.na(value) && value >= LimitData$Min[i] && value <= LimitData$Max[i]
        } else {
          FALSE
        }
      })
    )
  ) %>%
  ungroup() %>%
  dplyr::filter(metrics_in_range >= num_vars) %>%
  dplyr::select(-metrics_in_range)


  
  #Get Data
PlotDataRidges <- BurnLikely %>%
  dplyr::mutate(
    DATE = as.Date(DATE),
    Year = year(DATE),
    MonthDay = format(DATE, "%m-%d")
  ) %>%
  # Create all possible combinations and filter
  tidyr::crossing(RangeIndex = seq_along(date_starts)) %>%
  dplyr::mutate(
    RangeStart_check = date_starts[RangeIndex],
    RangeEnd_check = date_ends[RangeIndex],
    InRange = MonthDay >= RangeStart_check & MonthDay <= RangeEnd_check
  ) %>%
  dplyr::filter(InRange) %>%
  # Keep only the first matching range for each date
  dplyr::group_by(DATE, Year) %>%
  slice(1) %>%
  ungroup() %>%
  # Calculate periods within each range
  dplyr::mutate(
    RangeStart = as.Date(paste0(Year, "-", RangeStart_check)),
    RangeEnd = as.Date(paste0(Year, "-", RangeEnd_check)),
    DaysSinceRangeStart = as.numeric(DATE - RangeStart),
    Period = floor(DaysSinceRangeStart / period_length),
    PeriodStart = RangeStart + (Period * period_length),
    PeriodEnd = pmin(PeriodStart + (period_length - 1), RangeEnd),
    PeriodLabel = paste0(format(PeriodStart, "%b %d"), " - ",
                         format(PeriodEnd, "%b %d")),
    PeriodSort = as.numeric(format(PeriodStart, "%m%d"))
  ) %>%
  # Only keep periods that start within the date ranges
  dplyr::filter(PeriodStart >= RangeStart & PeriodStart <= RangeEnd) %>%
  # Count days per period per year
  dplyr::group_by(Year, PeriodLabel, PeriodSort) %>%
  dplyr::summarise(DaysInWindow = n(), .groups = "drop") %>%
  arrange(PeriodSort)

 

# Ridgeline Plot (uses year-by-year variation)
ggplot(PlotDataRidges, aes(x = DaysInWindow, y = reorder(PeriodLabel, PeriodSort), 
                           fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "plasma", name = "Days") +
  labs(
    title =  paste0("Strata ", Strata, ": Likely ",Season," Burn Window by Period (2010-2025)"),
    subtitle = paste0("Distribution of days with >",round(num_vars/total_vars*100,0) ,"% indices within prescription"),
    x = "Number of Days in Burn Window",
    y = "Two-Week Period"
  ) +
  theme_ridges() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.text.y = element_text(size = 14),
    plot.background = element_rect(fill = "#FAF0E6" , color = NA),
    panel.background = element_rect(fill = "#FAF0E6" , color = NA)
  )
ggsave(paste0(burnwindow_dir, "BurnWindowRidgeline_",Strata,"_",Season,".png"), width = 10, height = 8, dpi = 300)

# Violin Plot
ggplot(PlotDataRidges, aes(x = reorder(PeriodLabel, PeriodSort), y = DaysInWindow)) +
  geom_violin(fill = "steelblue", alpha = 0.7, trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  labs(
    title =  paste0("Strata ", Strata, ": Likely ",Season," Burn Window by Period (2010-2025)"),
    subtitle = paste0("Distribution of days with >",round(num_vars/total_vars*100,0) ,"% indices within prescription"),
    x = "Two-Week Period",
    y = "Number of Days in Burn Window"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "#FAF0E6" , color = NA),
    panel.background = element_rect(fill = "#FAF0E6", color = NA)
  )
ggsave(paste0(burnwindow_dir, "BurnWindowViolin_",Strata,"_",Season,".png"), width = 10, height = 8, dpi = 300)



#find weather norms
BurnAverages <- PlotDataRidges %>%
  dplyr::group_by(PeriodLabel, PeriodSort) %>%  
  dplyr::summarise(
    MeanDaysInWindow = mean(DaysInWindow, na.rm = T),
    SDDaysInWindow = sd(DaysInWindow, na.rm = T),
    .groups = "drop"
  )

BurnAverages %>%
  ggplot(aes(x = reorder(PeriodLabel, PeriodSort), y = MeanDaysInWindow)) +
  geom_bar(stat = "identity", fill = "steelblue") +  
  geom_errorbar(aes(ymin = MeanDaysInWindow - SDDaysInWindow, 
                    ymax = MeanDaysInWindow + SDDaysInWindow),
                width = 0.4,  # Width of the error bar caps
                linewidth = 0.5) +  # Thickness of error bars
  labs(
    title = paste0("Strata ", Strata, ": Likely ",Season," Burn Window by Period (2010-2025)"),
    subtitle = paste0("Mean Number of days with >", round(num_vars/total_vars*100, 0), "% indices within prescription"),
    x = "Two-Week Period",
    y = "Number of Days in Burn Window"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave(paste0(burnwindow_dir, "BurnWindowBar_",Strata,"_",Season,".png"), width = 10, height = 8, dpi = 300)

  }
}    

#' 
#' 
#' #17.FOFEM Input Modification and batch
#' 
## -----------------------------------------------------------------------------
#Load Treatment
st="FTU-B"

#Load Plot Data
Plot_data<-read.csv(paste0(fuelcalc,st,"-Out.csv"))
Plot_data$PlotId<-paste0("TU.",st,"_PL.",Plot_data$PlotID)
Plot_data <- Plot_data %>%
  distinct(PlotId, .keep_all = TRUE)

#Set Fire behavior Parameters from modeling
FL<-1.64 #Flame length Feet
CambiumKillRating<-1 #0-4
CSP<-20 #CrowwnScorchPercent
ScorchHeight<-4.92

#if using flame length set flag to "F" if using scorch set to "S"
Scorch_FlameLengthFlag<-"F"
RottenPercent<-10 #thousand hour rotten percentage
CrownBurnPercent<-5  

  #10hr moisture: "VeryDry"=6,"Dry"=10,"Moderate"=15,"Wet"=20
  Moist10HR<-9
  
  #Duff moisture: "VeryDry"=20,"Dry"=40,"Moderate"=75,"Wet"=130
  MoistDuff<- 40
  
  #1000hr moisture: "VeryDry"=10,"Dry"=15,"Moderate"=30,"Wet"=40
  Moist1000HR<-15
  
  #Soil moisture: "VeryDry"=5,"Dry"=10,"Moderate"=15,"Wet"=20
  SoilMoisture<-15
  
  #slash or Natural fuel  
  FuelCat<- "Natural"

  #season: Spring, Summer, Fall, Winter  
  Season<-"Spring"
  
  #Region: "PacificWest" or "InteriorWest"
  Region<-"InteriorWest"
  
  #Cover Group: "GG" = Grass; "SG" = Shrub; "SGC" = Shrub-Chaparral; "SB" = Sagebrush; "PN" = Ponderosa pine; "PC" = Pocosin; "BBS" = Balsam, Black, Red, White Spruce; "RJP" = Red; Jack Pine; "WPH" = White Pine Hemlock
  Cover<-"GG"
  
#-----------------------------------------------------
#Load TRE and FFI File
TRE <- read.table(
  file = paste0(path, "/FuelCalc/Outputs/TU_",st, "/TU_", st, "_FuelCalc_FFI.tre"))),
  sep = ",",
  header = TRUE,           # Use header = TRUE instead of col.names = TRUE
  quote = "",              # Remove quotes around text fields
  na.strings = "",         # Treat blank as NA
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE # Prevent automatic conversion to factors
)

FFI <- read.table(
  file = paste0(path, "/FuelCalc/Outputs/TU_",st, "/TU_", st, "_FuelCalc_FFI.ffi"))),
  sep = ",",
  header = TRUE,           # Use header = TRUE instead of col.names = TRUE
  quote = "",              # Remove quotes around text fields
  na.strings = "",         # Treat blank as NA
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE # Prevent automatic conversion to factors
)
FFIOG<-FFI

#Modify
  #TRE
  TRE$FlHt_ScHt<-FL
  TRE$CKR<-CambiumKillRating
  TRE$CharHeight<-FL*1.25
  TRE$CrownScorchPercent<-CSP
  TRE$FS<-Scorch_FlameLengthFlag
  TRE$Severity<-"L"
  
  #FFI
  FFI$TenHr_moist<- Moist10HR
  FFI$ThousandHr_moist<- Moist1000HR
  FFI$DuffMoist<-MoistDuff
  FFI$ThousandLoad<-as.numeric(FFI$ThousandHourDc1Sz1)*5
  FFI$ThousandLoad[is.na(FFI$ThousandLoad)]<-0
  FFI$ThousandRottenPercent<-RottenPercent
  FFI$ThousandDist<-"Even"
  FFI$SoilMoisture<-SoilMoisture
  FFI$AllHerbLoad<-FFI$HerbLoadDead+FFI$HerbLoadLive
  FFI$AllShrubLoad<-FFI$ShrubLoadDead+FFI$ShrubLoadLive
  
  #Merge canopy fuels
  FFI<-left_join(FFI,Plot_data %>% dplyr::select(PlotId,postCFL), by = "PlotId")
  mean_CFL<-mean(as.numeric(FFI$postCFL),na.rm=TRUE)
  FFI$postCFL[is.na(FFI$postCFL)]<-mean_CFL
  FFI$CFL<-round(as.numeric(FFI$postCFL)*0.404686,2)
  FFI$CBL<-FFI$CFL*0.9
  FFI$CBP<-CrownBurnPercent
  FFI$Region<-Region
  FFI$Season<-Season
  FFI$Fuel<-FuelCat
  FFI$CoverType<-Cover
  
  FFIOUT<- FFI %>% 
    dplyr::select(
    PlotId,
    LitterLoad,
    OneHour,
    TenHour,
    TenHr_moist,
    HundredHour,
    ThousandLoad,
    ThousandHr_moist,
    ThousandRottenPercent,
    ThousandDist,
    TotalDuffLoad,
    DuffMoist,
    DuffDepth,
    AllHerbLoad,
    AllShrubLoad,
    CFL,
    CBL,
    CBP,
    Region,
    CoverType,
    Season,
    Fuel)
  

#create FOFEM directory 
directory<-paste0(path,"/FOFEM/",st)

if(!dir.exists(directory)){
  dir.create(directory)
}

#export
#writes out modified TRE to use in Batch
write.table(
  TRE,
  file = paste0(directory,"/",st,"_FOFEM_TRE_BATCH_IN.tre"))),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,      # remove quotes around text fields
  na = "",            # blank instead of NA
  fileEncoding = "UTF-8"
)

#writes out MODIFIED FFI to use in batch
write.table(
  FFIOUT,
  file = paste0(directory,"/",st,"_FOFEM_FFI_BATCH_IN.ffi"))),
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,      # remove quotes around text fields
  na = "",            # blank instead of NA
  fileEncoding = "UTF-8"
)

#Writes out the original FFI to use by plot
write.table(
  FFIOG,
  file = paste0(directory,"/",st,"_FOFEM_FFI.ffi"))),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,      # remove quotes around text fields
  na = "",            # blank instead of NA
  fileEncoding = "UTF-8"
)

#Write batch files for fofem tre and ffi
  out_folder<-paste0(directory)

  #Files
  EmissionsIn <- paste0(st, "_FOFEM_FFI_BATCH_IN.ffi")))
  EmissionsOut <- paste0(st, "_FOFEM_Emissions_BATCH_OUT.txt")
  EmissionsRun <- paste0(st, "_FOFEM_Emissions_BATCH_RUN.txt")
  EmissionsError <- paste0(st, "_FOFEM_Emissions_BATCH_ERROR.txt")

  #Move files to fofem directiory
  fofem_local<- "C:\\Users\\Holden\\AppData\\Local\\FOFEM6.7"
  fofem_dir <- "C:\\Program Files (x86)\\FOFEM6.7"
  
  #move emissions file to your folder
  file.copy(from=paste0(fofem_local,"/Emission_Factors.csv"),to=paste0(out_folder,"/Emission_Factors.csv"))
  
  # Create batch file content
  batch_content <- paste0(
  "@echo off\n",
  "REM Set FOFEM path\n",
  "set path=%path%;",fofem_dir,"\n",
  "\n",
  "REM Change to working directory\n",
  "cd /d \"", out_folder, "\"\n",
  "\n",
  "REM Run FOFEM with Consumed/Emissions model\n",
  "fof_gui C ", EmissionsIn, " ", EmissionsOut, " ", EmissionsRun, " ", EmissionsError, " H\n",
  "\n",
  "echo FOFEM processing complete\n",
  "echo Check ", EmissionsError, " for any errors\n",
  "pause"
)

  # Write batch file
  batch_filename <- paste0(out_folder, "/Run_FOFEM_Emissions_", st, ".bat")
  writeLines(batch_content, batch_filename)
  
  #Now Do mortality
  MortalityIn <- paste0(st, "_FOFEM_TRE_BATCH_IN.TRE")
  MortalityOut <- paste0(st, "_FOFEM_Mortality_BATCH_OUT.txt")
  MortalityRun <- paste0(st, "_FOFEM_Mortality_BATCH_RUN.txt")
  MortalityError <- paste0(st, "_FOFEM_Mortality_BATCH_ERROR.txt")
  
  # Create batch file content
  batch_content <- paste0(
  "@echo off\n",
  "REM Set FOFEM path\n",
  "set path=%path%;",fofem_dir,"\n",
  "\n",
  "REM Change to working directory\n",
  "cd /d \"", out_folder, "\"\n",
  "\n",
  "REM Run FOFEM with Consumed/Mortality model\n",
  "fof_gui M ", MortalityIn, " ", MortalityOut, " ", MortalityRun, " ", MortalityError, " H\n",
  "\n",
  "echo FOFEM processing complete\n",
  "echo Check ", MortalityError, " for any errors\n",
  "pause")
  
  batch_filename <- paste0(out_folder, "/Run_FOFEM_Mortality_", st, ".bat")
  writeLines(batch_content, batch_filename)
  #-----------------------------------------------------------

#'         
#'         #Run Batch Files!
#'   
#'   #Load Results, Summarise, and Merge
## -----------------------------------------------------------------------------
for(st in unique(Snap_EX$Stratum)){
  #st="BU-3"
  out_folder<-paste0(path,"/FOFEM/",st)
  #Load Results
  #Emissions
  FOFEM_Emis<-read.table(
  file = paste0(out_folder,"/",st, "_FOFEM_Emissions_BATCH_OUT.txt"),
  sep = ",",
  header = TRUE,           # Use header = TRUE instead of col.names = TRUE
  quote = "",              # Remove quotes around text fields
  na.strings = "",         # Treat blank as NA
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE)
  Emiss_Units<-FOFEM_Emis %>% filter(Std == "Id")
  FOFEM_Emis<-FOFEM_Emis[-1,]
  colnames(FOFEM_Emis)[1]<-"PlotId"
  
  #Mortality  
  all_lines <- read_lines(paste0(out_folder, "/", st, "_FOFEM_Mortality_BATCH_OUT.txt"))
  header1 <- strsplit(all_lines[5], ",")[[1]]
  header2 <- strsplit(all_lines[6], ",")[[1]]
  col_names <- paste(trimws(header1), trimws(header2), sep = "_")
  col_names <- gsub("_+", "_", col_names)  # Replace multiple underscores with one
  col_names <- gsub("_$|^_", "", col_names)  # Remove leading/trailing underscores

  FOFEM_Mort <- read_csv(
    paste0(out_folder, "/", st, "_FOFEM_Mortality_BATCH_OUT.txt"),
    skip = 6,
    col_names = col_names,
    na = "")
  
  #Summarise
  resultsdirectory<-paste0(path,"/FOFEM/Results")
  if(!dir.exists(resultsdirectory)){
   dir.create(resultsdirectory)}
  
  FOFEM_Emis <- FOFEM_Emis %>%
  dplyr::mutate(across(-PlotId, as.numeric))
  Emission_Summary<- FOFEM_Emis %>%
  dplyr::summarise(
    # Using Consumed / Pre * 100
    LitterPctConsumed = round((mean(LitCon/LitPre, na.rm = TRUE)) * 100, 2),
    Hr1PctConsumed = round((mean(DW1Con/DW1Pre, na.rm = TRUE)) * 100, 2),
    Hr10PctConsumed = round((mean(DW10Con/DW10Pre, na.rm = TRUE)) * 100, 2),
    Hr100PctConsumed = round((mean(DW100Con/DW100Pre, na.rm = TRUE)) * 100, 2),
    Hr1000PctConsumed = round((mean(DW1kSndCon/DW1kSndPre, na.rm = TRUE)) * 100, 2),
    HerbPctConsumed = round((mean(HerCon/HerPre, na.rm = TRUE)) * 100, 2),
    ShrubPctConsumed = round((mean(ShrCon/ShrPre, na.rm = TRUE)) * 100, 2),
    DuffPctConsumed = round((mean(DufCon/DufPre, na.rm = TRUE)) * 100, 2),
    CFLPctConsumed = round((mean(FolCon/FolPre, na.rm = TRUE)) * 100, 2),
    MineralSoilExposed = round(mean(MSE, na.rm = TRUE), 2)
  )
    Emission_Summary$Stratum=st

  write.csv(Emission_Summary,paste0(resultsdirectory,"/",st,"_FOFEM_Consumption.csv"), row.names=FALSE)

  
  #Mortality
  FOFEM_Mort <- FOFEM_Mort %>%
  dplyr::mutate(across(-Plot_Id, as.numeric))
  Mortality_Summary<- FOFEM_Mort %>%
  dplyr::summarise(
    TreesPctKilled = round((mean(Density_Killed/Density_PrefirePre, na.rm = TRUE)) * 100, 2),
    BAPctKilled = round((mean(`BAKld_sq/ft`/`BAPre_sq/ft`, na.rm = TRUE)) * 100, 2),
    CanCovChange= round((mean(CanCov_Diff/CanCov_Prefire, na.rm = TRUE)) * 100, 2),
    MeanDBHKilled= round(mean(`DBHKldAvg_(inch)`*2.54),2),
    MortalityPercentage_10cmUP=round(mean(MortAvg4_percent),2),
    MortalityPercentageALL=round(mean(MortAvg_percent),2))
  Mortality_Summary$Stratum=st
    
  write.csv(Mortality_Summary,paste0(resultsdirectory,"/",st,"_FOFEM_Mortality.csv"), row.names=FALSE)
  }

  #Merge Results
  Emissionfiles <- list.files(resultsdirectory, pattern = "*_FOFEM_Consumption", full.names = TRUE)
  EmissionsResults_AllStratum <- Emissionfiles %>%
    lapply(read.csv) %>%
    bind_rows()

  Mortalityfiles <- list.files(resultsdirectory, pattern = "*_FOFEM_Mortality", full.names = TRUE)
  MortalityResults_AllStratum <- Mortalityfiles %>%
    lapply(read.csv) %>%
    bind_rows()

  write.csv(EmissionsResults_AllStratum, 
          paste0(resultsdirectory, "/EmissionsResults_AllStratum.csv"),
          row.names = FALSE)

  write.csv(MortalityResults_AllStratum, 
          paste0(resultsdirectory, "/MortalityResults_AllStratum.csv"),
          row.names = FALSE)



#'   
#' 
#' #18.Spatial FOFEM/Flammap
#'   Generate Landscape Files for FOFEM and Run a SpatialFOFEM run
## -----------------------------------------------------------------------------
#Load existing
Structure<-read.csv(paste0(path,Fuel_prefix,"Post_Treatment_Structure_Data.csv"))
AOI<-st_read(paste0(path,"/DokieSiding_AOI.shp"))
dem<-rast(paste0(path,Fuel_prefix,"dem.tif"))
PlotData<-read.csv(paste0(fuelcalc,"Dokie-A-Out.csv"))
Units<-PlotData %>% dplyr::filter(PlotID=="Units")
PlotData <- PlotData %>% 
  dplyr::filter(!is.na(as.numeric(PlotID)))

#create spatial plot data
PlotData_SF <- PlotData %>%
  mutate(across(c(Latitude, Longitude), as.numeric))
PlotData_SF <- st_as_sf(PlotData_SF, 
                           coords = c("Longitude", "Latitude"),
                           crs = 3005)
#Calculations
FTU_AREA<-st_area(AOI)
Area_Per_Plot<-FTU_AREA/length(unique(PlotData$PlotID))/10000
    
#Disaggregate Dem and Generate Slope,Aspect:
DEM<-disagg(dem,fact=50)
Slope<- disagg(terrain(dem, v = "slope", unit = "degrees"),fact=50)
Aspect<-disagg(terrain(dem, v = "aspect", unit = "degrees"),fact=50)
plot(Slope)

#Generate Fuels Data by Sampling Around the plot files and interpolating
  #Columns to interpolate
cols_to_interpolate <- c("postCFL", "postCBD", "postCBH", "postSH", "postTD", "postCC", "postBA")

  #Numeric
PlotData_SF <- PlotData_SF %>%
  mutate(across(all_of(cols_to_interpolate), as.numeric))

  # Create a template
  template<-rast(DEM)
  covariates <- c(DEM, Slope, Aspect)
  names(covariates) <- c("DEM", "Slope", "Aspect")
 
  PlotData_SF <- PlotData_SF %>%
  mutate(
    DEM = extract(DEM, vect(.))[, 2],
    Slope = extract(Slope, vect(.))[, 2],
    Aspect = extract(Aspect, vect(.))[, 2]
  )

# Function to perform regression-kriging style interpolation
interpolate_with_covariates <- function(points_sf, template, covariates, variable_name, power = 2) {
  
  # Prepare data
  data_df <- st_drop_geometry(points_sf) %>%
    dplyr::select(all_of(c(variable_name, "DEM", "Slope", "Aspect"))) %>%
    na.omit()
  
  points_filtered <- points_sf[complete.cases(st_drop_geometry(points_sf)[, c(variable_name, "DEM", "Slope", "Aspect")]), ]
  
  if (nrow(data_df) < 5) {
    cat("  Not enough data points, using simple IDW\n")
    return(simple_idw(points_filtered, template, variable_name, power))
  }
  
  # Try full model first
  formula_full <- as.formula(paste(variable_name, "~ DEM + Slope + Aspect"))
  model <- try(lm(formula_full, data = data_df), silent = TRUE)
  
  # If rank deficient, try simpler models
  if (inherits(model, "try-error") || summary(model)$r.squared < 0.01) {
    cat("  Using simple IDW (covariates not informative)\n")
    return(simple_idw(points_filtered, template, variable_name, power))
  }
  
  cat("  R-squared:", round(summary(model)$r.squared, 3), "\n")
  
  # Predict using covariates
  prediction <- predict(covariates, model, na.rm = TRUE)
  
  # Calculate residuals
  points_filtered$residual <- data_df[[variable_name]] - predict(model, data_df)
  
  # Interpolate residuals
  residual_interp <- simple_idw(points_filtered, template, "residual", power)
  
  # Combine
  final_prediction <- prediction + residual_interp
  
  return(final_prediction)
}

# Simple IDW function as fallback
simple_idw <- function(points_sf, template, variable_name, power = 2) {
  
  # Get valid data
  values <- st_drop_geometry(points_sf)[[variable_name]]
  valid <- !is.na(values)
  points_filtered <- points_sf[valid, ]
  
  if (sum(valid) == 0) {
    return(template * NA)
  }
  
  # Convert to SpatVector
  points_filtered$idw_value <- st_drop_geometry(points_filtered)[[variable_name]]
  points_vect <- vect(points_filtered)
  
  # Get all raster cell coordinates
  xy <- crds(template, na.rm = FALSE)
  
  # Get point coordinates and values
  pt_coords <- crds(points_vect)
  pt_values <- values(points_vect)$idw_value
  
  # Calculate IDW for all cells at once
  result_values <- apply(xy, 1, function(cell_xy) {
    dists <- sqrt((cell_xy[1] - pt_coords[, 1])^2 + 
                  (cell_xy[2] - pt_coords[, 2])^2)
    dists[dists == 0] <- 1e-10
    weights <- 1 / (dists^power)
    sum(weights * pt_values) / sum(weights)
  })
  
  # Create output raster
  result <- template
  values(result) <- result_values
  
  return(result)
}

# Interpolate all variables
raster_list <- list()

for (var in cols_to_interpolate) {
  cat("Interpolating", var, "...\n")
  
  raster_list[[var]] <- interpolate_with_covariates(PlotData_SF, template, 
                                                     covariates, var, power = 2)
  
  output_file <- paste0(var, "_interpolated_covariates.tif")
  #writeRaster(raster_list[[var]], output_file, overwrite = TRUE)
  
  cat("Saved:", output_file, "\n\n")
}

# Create multi-band raster
AllRasts <- rast(raster_list)
names(AllRasts) <- c("CFL","CBD","CBH","SH","TD","CC","BA")
plot(AllRasts,main=names(AllRasts))

#Run Smoother
gaussian_window <- focalWeight(AllRasts[[1]], d = 10, type = "Gauss")

AllRasts_Smooth <- focal(AllRasts, 
                         w = gaussian_window, 
                         fun = "sum",  # Use sum with weighted kernel
                         na.rm = TRUE)
#Check
plot(AllRasts[[3]],main=names(AllRasts)[3])
plot(AllRasts_Smooth[[3]],main=names(AllRasts_Smooth)[3])

#Create Fuel file and FCCS file and Export
FT<-rast(template)
values(FT)<-182
FCCS<-rast(template)
values(FCCS)<-88
#FT<-mask(FT,AOI)

LandscapeFile<-c(DEM,Slope,Aspect,FT,AllRasts_Smooth$CC,AllRasts_Smooth$SH,AllRasts_Smooth$CBH,AllRasts_Smooth$CBD*100,FCCS)
names(LandscapeFile)<-c("Elevation","Slope","Aspect","FuelModel","CrownClosure","StandHeight","CanopyBaseHeight","CrownBulkDensity","FCCS")
#LandscapeOut<-trim(mask(LandscapeFile,AOI))
plot(LandscapeOut,main=names(LandscapeOut))
writeRaster(LandscapeFile,paste0(path,"/LandscapeFile.tif"),overwrite=TRUE)

#Create Custom FuelModel File:


#' 
#' 

cat('Step 3: Fire modeling complete.\n')
