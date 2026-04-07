#Raster Version of Probability of Ignition model from Schroeder 1969 and adapted by 
#Andrews in the National Wildfire Cooridnating Group (NWCG) (https://www.nwcg.gov/publications/pms437/fuel-moisture/probability-of-ignition)

#Model predicts the probability of the ignition of a fuelbed from a firebrand or heat source base
#on the Dry bulb temperature (F) and 1 hr moisture content of fuels (%) for catgories of the amount of shading of fuels
#using the midpoint of the shade classes.

  
#load packages
library(dplyr)
library(raster)
library(firebehavioR)
library(terra)

#Inputs into the model are temperature(c)(ie dry bulb), relative humdity, crown closure, hour (of day),month, aspect, slope.
ignition_probability<-function(closure_rast,
                               temp_rast,
                               rh_rast,
                               aspect_rast,
                               slope_rast,
                               elev_rast,
                               fit, #set method to linear or log for curve
                               ffm_manual=NULL
){
  if(!(fit %in% c("linear", "log"))){
    stop("❌ Invalid value for 'fit'. Please set fit to either 'linear' or 'log'.")
  }
  # Adjust for manual fine fuel moisture or modeled
  if (!is.null(ffm_manual)) {
    # If ffm_manual is a raster, use it directly
    if (class(ffm_manual)[1] == "SpatRaster") {
      effm <- ffm_manual
    } else {
      # If ffm_manual is a single value, create constant raster
      effm <- rast(temp_rast)  # Create template with same extent/resolution
      values(effm) <- ffm_manual  # Fill with constant value
    }
  } else {
    # Predict Fine Fuel Moisture (effm) using raster calculation
    effm <- 1.651 * rh_rast^0.493 + 0.001972 * exp(0.092 * rh_rast) + 0.101 * (23.9 - temp_rast)
  }
  
  #Change effm to fraction
  effm<-effm/100
  
  #change temperature to dry bulb temp in F
  temp_F<-temp_rast*1.8 + 32

# For Andrews method:-----------------------------------------------------------------------
      shade_class <- ifel(closure_rast <= 10, 5,
                          ifel(closure_rast <= 60, 35,
                               ifel(closure_rast <= 90, 75, 95)))
      # Apply different formulas based on shade class using ifel
      if (fit == "linear") {
        # Linear fit calculations for each shade class
        p_I_5 <- 82.55493 + 0.17543 * temp_F - 5.57843 * effm
        p_I_35 <- 82.48312 + 0.17961 * temp_F - 5.59641 * effm
        p_I_75 <- 82.48312 + 0.17961 * temp_F - 5.59641 * effm
        p_I_95 <- 81.87182 + 0.18376 * temp_F - 5.57190 * effm
        
        # Combine with ifel based on shade class
        p_I_A <- ifel(shade_class == 5, p_I_5,
                    ifel(shade_class == 35, p_I_35,
                         ifel(shade_class == 75, p_I_75, p_I_95)))
      } else {
        # Log-log fit calculations for each shade class
        p_I_5 <- 72.92 * (temp_F)^0.33660 * (effm)^-1.04605
        p_I_35 <- 72.426 * (temp_F)^0.33912 * (effm)^-1.04743
        p_I_75 <- 72.426 * (temp_F)^0.33912 * (effm)^-1.04743
        p_I_95 <- 72.712 * (temp_F)^0.34306 * (effm)^-1.04437
        
        # Combine with ifel based on shade class
        p_I_A <- ifel(shade_class == 5, p_I_5,
                    ifel(shade_class == 35, p_I_35,
                         ifel(shade_class == 75, p_I_75, p_I_95)))
      }
      
#Schroedor Math method:-----------------------------------------------------------------------
    #Air Temperature to forest fuel temp using RH + T relationship with moisture content
    
    #1) Calculate atmospheric pressure with barometric formula
    
    p<- 1013.25 * (1- ((0.0065*elev_rast)/288.15)^((9.80665-0.0289644)/(8.3144598-0.0065)))
    
    #2) Calculate saturation vapor pressure and vapor pressure:
    
    e_s<-6.112 * exp((17.67*temp_rast)/(temp_rast + 243.5))
    
    e<- rh_rast*(e_s/100)
    
    #3) Calculate mixing ratio: mass of water per mass of dry air
    
    w<- 0.622 * (e/(p-e))
    
    #or solve for w
    e<- w*p/(w+0.622) 
      
    #4) Calculate temperature of fuel based on ratio of MC to RH by solving for T in vapor pressure equation
    
    #solving equation 2.1 and 2.2 for T;
    L <- log((100 * e) / (6.112 * rh_rast)) 
    T <- (-243.5 * L) / (L - 17.67)
    
    #Subsituting T into Anderson equation to predict moisture content
    #this predicts MC using anderson's equation based on just RH and e
    MC <- 1.651 * (rh_rast ^ 0.493) + 
      0.001972 * exp(0.092 * rh_rast) + 
      0.101 * (23.9 + (243.5 * log((100 * e) / (6.112 * rh_rast))) / (17.67 - log((100 * e) / (6.112 * rh_rast))))
    
    #5) Assuming the relationship betweem MC(mass of water per mass dry fuel) ~ RH (mass of water per mass air) is the same as 
    #temperature of fuel ~ Temp of air; then just replace rh with T to get temp of fuel
    
    T_0 <- 1.651 * (temp_rast ^ 0.493) + 
      0.001972 * exp(0.092 * temp_rast) + 
      0.101 * (23.9 + (243.5 * log((100 * e) / (6.112 * temp_rast))) / (17.67 - log((100 * e) / (6.112 * temp_rast))))
    
    
    #calculate heat of pre-ignition assuming an ignition temp of 320 C from Schroeder Equation 10
    Q_ig<- 144.51 - 0.266*T_0 - 0.00058*(T_0^2) - T_0*effm + 18.54*(1-exp(-15.1*effm)) + 640*effm
    
    x<- (400-Q_ig)/10
    
    #probability of firebrand igniting  
    p_I_S<-(0.000048*(x^4.3))/50

  # Ensure values are within reasonable range (0-100%)
  output_stack<-c(p_I_S,p_I_A)
  names(output_stack)<- c("Probability Schroeder", "Probability Andrews")
  return(output_stack)
}
