cfis_modified <- function(fsg, u10, effm, sfc, cbd, id,centroid ,adjusted=TRUE) {
  
  #all values as vectors:
  
  #u10 = 10 meter open wind speed in (km/hr)
  #fsg = fuel strata gap or canopy base height (m) use modified CBH or FSG if adjusted is true then subtracts 1 from the FSG
  #sfc = surface fuel consumption kg/ha
  #effm = effective fine fuel moisture (%) as a percentage
  #cbd = canopy bulk density kg/m3
  
  if (any(fsg > 12)) warning("Canopy base height should not exceed 12 m")
  if (any(u10 > 80)) warning("Open wind speed should not exceed 80 km/hr")
  #if (any(effm > 20)) warning("Fuel moisture should not exceed 20 percent")
  if (any(cbd > 1)) warning("Canopy bulk density should not exceed 1 kg/m3")
  
  sfc
  MC_WS<- u10*effm
  
  if(adjusted == "TRUE" & fsg > 2.0){
    FSG_mod<-(fsg-1)^1.5
  }else{
    FSG_mod<-fsg^1.5
  }
  SFC_ln<- log(sfc)
  
  #Function(11) from Perrakis et al. 2023
  g_x<- (-3.5550 + (u10*1.4407) + (-0.53211*FSG_mod) + (2.4897*SFC_ln) + (-0.07019*MC_WS))
  
  p_crown <-exp(g_x)/(1+(exp(g_x)))
  
  crosa <- (11.02 * u10^0.9) * (cbd^0.19) * exp(-0.17 * effm)
  crosp <- crosa * exp(-0.3333 * crosa * cbd)
  cac <- crosa * cbd / 3
  
  cros <- ifelse(p_crown > 0.5, ifelse(cac < 1, crosp, crosa), NA)
  sd <- cros * (30 + id) - cros * (30 + (exp(-0.115 * 30) / 0.115) -
                                     1 / (0.115))
  type <- ifelse(p_crown < 0.5, "surface", ifelse(cac < 1, "passive",
                                                  "active"
  ))
  return(data.frame(type = type, pCrown = round(
    p_crown * 100,
    2
  ), cROS = round(cros, 2), sepDist = round(sd, 2)))
}