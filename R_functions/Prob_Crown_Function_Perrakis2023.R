#Improved Models of Crown Fire Potential from Perrakis et al.2023
#model:

#p(CFO) = 1/(1 + e^-(B0 + B0x1 + ...Bnxn))
#Needs
#B0: -3.5550
#B1: WS10: 1.4407
#B2: FSG^1.5: -0.53211
#B3: WS10 x MC: -0.07019
#b4: ln(SFC): 2.4897


pCFO<- function(WS,CBH,MC,SFC,adjusted="TRUE"){
  #reads moisture content as a percentage
  
  #Can take a vector of values and returns a vector of crown fire probability
  #From equation 11: Perrakis et al 2023:
  
  #p(CFO) = 1/(1 + e^-(B0 + B0x1 + ...Bnxn))
  
  #Needs
  #B0: -3.5550
  #B1: WS10: 1.4407 km/hr
  #B2: FSG^1.5: -0.53211 m use modified CBH or FSG
  #B3: WS10 x MC: -0.07019
  #B4: ln(SFC): 2.4897
  
  #Setup values
  if(adjusted == "TRUE" & cbh > 2.0){
    FSG_mod<-(FSG-1)^1.5
  }else{
    FSG_mod<-FSG^1.5
  }
  MC_WS<-WS*MC
  SFC_ln<- log(SFC)
  
  #calculate lower equation part
  g_x<- (-3.5550 + (WS*1.4407) + (-0.53211*FSG_mod) + (2.4897*SFC_ln) + (-0.07019*MC_WS))
  
  #calculate probability of crowning
  prob_crown<- exp(g_x)/(1+(exp(g_x)))
  #returns probability out of 1
  return(prob_crown)
}