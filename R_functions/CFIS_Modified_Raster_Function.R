cfis_mod_raster <- function(cbh, u10, effm, sfc, cbd, id, centroid ,adjusted=TRUE) {
  
  #all values as vectors:
  
  #u10 = 10 meter open wind speed in (km/hr)
  #fsg = fuel strata gap or canopy base height (m) use modified CBH or FSG if adjusted is true then subtracts 1 from the FSG
  #sfc = surface fuel consumption kg/ha
  #effm = effective fine fuel moisture (%) as a percentage
  #cbd = canopy bulk density kg/m3
  
  sfc<-sfc*10
  MC_WS<- u10*effm
  
  if(adjusted == "TRUE"){
    FSG<-ifel(cbh > 2,cbh-1,cbh)
  }
  
  FSG_mod<-FSG^1.5
  SFC_ln<- log(sfc)
  
  #Function(11) from Perrakis et al. 2023
  g_x<- (-3.5550 + (u10*1.4407) + (-0.53211*FSG_mod) + (2.4897*SFC_ln) + (-0.07019*MC_WS))
  
  p_crown <-exp(g_x)/(1+(exp(g_x)))
  
  crosa <- (11.02 * u10^0.9) * (cbd^0.19) * exp(-0.17 * effm)
  crosp <- crosa * exp(-0.3333 * crosa * cbd)
  cac <- crosa * cbd / 3
  
  cros <- ifel(p_crown > 0.5, ifel(cac < 1, crosp, crosa), NA_real_)
  sd <- cros * (30 + id) - cros * (30 + (exp(-0.115 * 30) / 0.115) -1 / (0.115))
  type <- ifel(p_crown < 0.5, 1, ifel(cac < 1, 2,3))
  
  return(list(type = type, pCrown = round(p_crown * 100,2), cROS = round(cros, 2), sepDist = round(sd, 2)))
}