#Probability of Ignition model from Schroeder 1969 and adapted by 
#Andrews in the National Wildfire Cooridnating Group (NWCG) (https://www.nwcg.gov/publications/pms437/fuel-moisture/probability-of-ignition)

#Model predicts the probability of the ignition of a fuelbed from a firebrand or heat source base
#on the Dry bulb temperature (F) and 1 hr moisture content of fuels (%) for catgories of the amount of shading of fuels
#using the midpoint of the shade classes.

#load packages
library(dplyr)
library(raster)
library(firebehavioR)



#Inputs into the model are temperature(c)(ie dry bulb), relative humdity, crown closure, hour (of day),month, aspect, slope.
ignition_probability<-function(closure=75,
                               temp=25,
                               rh=40,
                               hour=15,
                               month=7,
                               aspect="S",
                               slope=20,
                               elev=1000,
                               fit="linear", #set method to linear or log for curve
                               ffm_manual=NULL,
                               method= "Schroeder"#set moisture content manually as (%)
                               ){
  #create shade logical
  shade<-ifelse(closure > 50,"y", "n")
  
  
  #Calculate effective fine fuel moiture with the Anderson Model:
  effm<- ffm(method = "anderson",
             rh=rh,
             temp=temp,
             hour=hour,
             month=month,
             asp=aspect,
             slp=slope,
             bla="y",
             shade=shade)
  
  #Adjust for manual fine fuel moisture or modeled
  if(!is.null(ffm_manual)){
   effm<-ffm_manual 
  }else{
    effm<-effm$fm1hr
  }
  
 
  #change temperture to dry bulb temp in F
  temp_F<-temp*1.8 + 32
  
  #Use andrews method or original approach
  
  if(method == "Andrews"){
  
  #Calculate shade class:
  shade_class<-ifelse(closure <= 10,5,
                      ifelse(closure > 10 & closure <= 60,35,
                             ifelse(closure > 60 & closure <= 90, 75, 95)))
  
  #calculate probability of the ignition of a fuel bed: equations generated from GLM models of prob and mc+temp
  if(shade_class == 5){
    
    if(method=="linear"){
    #linear
      p_I<-82.55493 + 0.17543*(temp_F) -5.57843*(effm)
    }else{
    #log-log
    p_I <- 72.92 * (temp_F)^0.33660 * (effm)^-1.04605
    }
  }else if(shade_class == 35){
    
    if(method=="linear"){
    #linear
    p_I <- 82.48312 + 0.17961*(temp_F) - 5.59641*(effm)
    } else{
    #log-log
    p_I <- 72.426 * (temp_F)^0.33912  * (effm)^-1.04743
    }
    
  }else if(shade_class == 75){
    
    if(method=="linear"){
    #linear
    p_I <- 82.48312 + 0.17961*(temp_F) - 5.59641*(effm)
    }else{
    #log-log
    p_I <- 72.426 * (temp_F)^0.33912  * (effm)^-1.04743
    }
    
  }else{
    if(method=="linear"){
    #linear
    p_I <- 81.87182 + 0.18376*(temp_F) -5.57190*(effm)
    }else{
    #log-log
    p_I <- 72.712 * (temp_F)^0.34306  * (effm)^-1.04437
    }
  }
  
  return(p_I)
  } else{
    
  #Air Tempature to forest fuel temp using RH + T relationship with moisture content
    
    #1) Calculate atmospheric pressure with barometric formula
    
    p<- 1013.25 * (1- ((0.0065*elev)/288.15)^((9.80665-0.0289644)/(8.3144598-0.0065)))
    
    #2) Calculate saturation vapor pressure and vapor pressure:
    
    e_s<-6.112 * exp((17.67*temp)/(temp + 243.5))
    
    e<- rh*(e_s/100)
    
    #3) Calculate mixing ratio: mass of water per mass of dry air
    
    w<- 0.622 * (e/(p-e))
    
    #4) Calculate temperature of fuel based on ratio of MC to RH by solving for T in vapor pressure equation
    
    #solving equation 2.1 and 2.2 for T;
    L <- log((100 * e) / (6.112 * rh)) 
    T <- (-243.5 * L) / (L - 17.67)
    
    #Subsituting T into Anderson equation to predict moisture content
      #this predicts MC using anderson's equation based on just RH and e
    MC <- 1.651 * (rh ^ 0.493) + 
      0.001972 * exp(0.092 * rh) + 
      0.101 * (23.9 + (243.5 * log((100 * e) / (6.112 * rh))) / (17.67 - log((100 * e) / (6.112 * rh))))
    
    #5) Assuming the relationship betweem MC(mass of water per mass dry fuel) ~ RH (mass of water per mass air) is the same as 
      #temperature of fuel ~ Temp of air; then just replace rh with T to get temp of fuel
    
    T_0 <- 1.651 * (temp ^ 0.493) + 
      0.001972 * exp(0.092 * temp) + 
      0.101 * (23.9 + (243.5 * log((100 * e) / (6.112 * temp))) / (17.67 - log((100 * e) / (6.112 * temp))))
    
    
  #calculate heat of preignition assuming an ignition temp of 320 C from Schroeder Equation 10
    Q_ig<- 144.51 - 0.266*T_0 - 0.00058*(T_0^2) - T_0*effm + 18.54(1-exp(-15.1*effm)) + 640*effm
    
    x<- (400-Q_ig)/10
    
  #probability of firebrand igniting  
    p_I<-(0.000048*(x^4.3))/50
    
  return(p_I)  
    
  }
}