#Code to predict Crown fuel consumption in Canadian forests.

#From Groot et al 2022:Crown fuel consumption in Canadian boreal forest fires 

#Used original Canadian experimental fire database of 59 fires. Function uses a new methodology to predict CFB as related to rate of
#spread not critical rate of spread. Two models were available (R^2= 0.86 and R^2=0.76). Final inputs were foilage of overstory and ROS for
#second model which just uses CFL of overstory because first model requires fuel load of dead branches (usually not available).

#Data ranges:
#CBD=0.04-0.36
#CFL=0.12-1.07
#FuelTypes = C1,C2,C3,C4

#Start:
CFC_Calc<- function(ROS,CFL,CFB=TRUE){
  #CFL in kg m^-2
  #ROS in m min^-1
  
  #create CFB output
  if(CFB == TRUE){
    cfb=1-exp(-0.23*ROS) #new CFB function based on ROS alone not RSO
  }
  #CFC output: Equation 8 from Groot et al. 2022
  cfc<- -0.193 + (1-exp(-0.23*ROS))*1.998*CFL
  
  #clean:
  cfc<- ifelse(cfc<0,0,cfc)
  cfc<- ifelse(cfc>CFL,CFL,cfc)
  
  df<-data.frame(
    CFB=cfb,
    CFC=cfc
  )
  #Export values CFB and CFC
  return(df)
}

