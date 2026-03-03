# Function to calculate equilibrium moisture content:
  #-The equilibrium moisture content EMC
  #is "the moisture content [%] of a fuel 
  #particle allowed sufficient time to reach equilibrium with 
  #its environment, i.e. no net moisture exchange" (Bradshaw et al. 1983).

#Simard (1968) defined the EMC as follows:

EMC<- function(RH,T){
  #change temperature from C to F for functions
  T_f<- T*1.8 + 32
  
  #equations based on RH catagory
  if(RH < 10){
    emc<-0.3229 + (0.281073*RH) - (0.000578*RH*T_f)
  }else if(RH >= 10 & RH <50){
    emc<-2.22749 + (0.160107*RH) - (0.01478*T_f)
  }else{
    emc<-21.0606 + (0.005565*(RH^2)) - (0.00035*RH*T_f) - (0.483199*RH)
  }
  return(emc)
  
}