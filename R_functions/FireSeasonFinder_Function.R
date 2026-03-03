#Fire Season Finder by Ecodivision based on:
  #https://wps-prod.apps.silver.devops.gov.bc.ca/static/media/90th_percentile_calculator_rationale.d02b2d44.pdf


FireSeasonFinder<-function(Ecodivision){
  
if(Ecodivision == "SUB-ARCTIC HIGHLANDS"){
  months = (6:8)
  daysfirstmonth =  as.character(c(1:31))
  dayslastmonth <- as.character(c(1:15))
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else if(Ecodivision == "SUB-ARCTIC"){
  months = (6:8)
  daysfirstmonth =  as.character(c(1:31))
  dayslastmonth <- as.character(c(1:15))
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else if(Ecodivision == "BOREAL"){
  months = (5:8)
  daysfirstmonth =  as.character(c(15:31))
  dayslastmonth <- as.character(c(1:31))
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else if(Ecodivision == "HUMID CONTINENTAL HIGHLANDS"){
  months = (5:8)
  daysfirstmonth =  as.character(c(15:31))
  dayslastmonth <- as.character(c(1:31)) 
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else if(Ecodivision == "NORTHEASTERN SUB-ARCTIC PACIFIC"){
  months = (6:8)
  daysfirstmonth =  as.character(c(1:31))
  dayslastmonth <- as.character(c(1:15))
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else if(Ecodivision == "COOL HYPERMARITIME AND HIGHLANDS"){
  months = (5:8)
  daysfirstmonth =  as.character(c(15:31))
  dayslastmonth <- as.character(c(1:31))   
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else if(is.na(Ecodivision)){
  months = (6:8)
  daysfirstmonth =  as.character(c(1:31))
  dayslastmonth <- as.character(c(1:31))   
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
}else{
  months = (5:9)
  daysfirstmonth =  as.character(c(1:31))
  dayslastmonth <- as.character(c(1:15)) 
  
  season<-list(
    Months=months,
    FirstMonth=daysfirstmonth,
    LastMonth=dayslastmonth
  )
  
}

return(season)  
    
}
