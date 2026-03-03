#surface fuel consumption
sfc<- function(dc,ffl,ffmc,dmc,depth,fueltype,bd,bui){
  FFL<-ffl
  DMC<-dmc
  DC<-dc
  Depth<-depth
  BD<-bd
  FT<-fueltype
  BUI<-bui
  FFMC<-ffmc
  
  #Calculate Surface Fuel Consumption or log(SFC) from W.J. de Groot et al. 2009, uses fine fuel loads in kg/m^2 of 1-10-100 hours
  if(FT == "Any"){
    log.sfc<- -4.252 + 0.719*log(DC) + 0.671*log(FFL)
  } else if(toupper(FT) %in% c("C-1","C1")){
    log.sfc <- -71.682 - 2.732*log(DMC) + 18.347*log(FFMC)
  } else if(toupper(FT) %in% c("C-2","C2")){
    sfc<- 0.721 + 0.187*FFL + 0.030*DMC
    log.sfc<- log(sfc)
  } else if(toupper(FT) %in% c("C-3","C3")){
    log.sfc<- -3.184 + 0.746*log(DC)-0.318*log(DMC) + 0.577*log(FFL)
  } else if(toupper(FT) %in% c("C-4","C4")){
    log.sfc<- -3.477 + 0.609*log(DC)+ 1.116*log(FFL)
  } else if(toupper(FT) %in% c("D-1","D1")){
    log.sfc<- log(0.924 + 0.023*DC -0.082*BUI)
  } else if (toupper(FT) %in% c("D-2","D2")){
    log.sfc<- log(0.924 + 0.023*DC -0.082*BUI)
  } else{
    log.sfc<- 11.658 - 2.598*log(BUI)
  }
  #export log.sfc for use in final equation
  LOG.SFC<-log.sfc 
  sfc<- exp(LOG.SFC)
  return(sfc)
}