#Model to predict maximum spotting distance of a firebreand- Albini 1979
  
#Paper: Potential spotting distance from wind-driven surface fires - ALBINI 1979
#accuracy test:https://www.fs.usda.gov/rm/pubs_journals/2019/rmrs_2019_page_w001.pdf


source("Z:/Scripts/FronteraCodez/Functions/CrownWeight_Brown77.R")

#Inputs:
  #height of torching tree
  #DBH
  #Species
  #ROS of fire there
  #HFI fire there
  #Sfc fire there
  #windspeed
  #wind direction heading

ht<-25
spp<-"Spruce"
dbh<-35
ws<-17
wg<-25
slope<-30
direction<-45
ros<-15
hfi<-10000
sfc<-2
M<-0.07
Ta<-25

maximum_spot<-function(
    Species,
    DBH,
    Heights,
    WindSpeed,
    WindGust,
    Elevation,
    Slope,
    Direction,
    ROS,
    HFI,
    SFC,
    MC,
    Ta,
    IntensityType="Byram" #either "Byram or Preset"
){
  #Load Variables:
  ht=Heights
  spp<-tolower(Species)
  dbh<-DBH
  ws<-WindSpeed
  wg<-WindGust
  slope<-Slope
  direction<-Direction
  ros<-ROS
  hfi<-HFI
  sfc<-SFC
  M<-MC
  T_a<-Ta
  W_a<-sfc
  #assume a 1 meter fuelbed depth
  FBDepth_m=1
  
  if(IntensityType == "Byram"){
  I_B<-ros*300*sfc
  else{
      I_B<-hfi
  }
  #Calculate crown weight from Brown 1977
  CW<-crown_weight(species=spp,
                   dbh_cm=dbh,
                   crown_ratio = rep(80,length(spp)),
                   h_m = ht)
  
  #calclulate wind adjustment: assume 80% crown ratios and 80% closed canopy
  wind_adj<-firebehavioR::waf(FBDepth_m,ht,80, 80)
  WS_mid<-wind_adj*ws/3.6
    
  #Constants:
  #Q_m: heat to raise temperature of water in particle from ambient to 100C ie 2570 kJ per kg
  Q_m<-2570
  #Q_f or H_D (Albini 1980: heat to raise dry fuel particle temperature from ambient to sublimination value 
  #(averaged from measured values in Nelson) in kJ/kg or J/gm for Albini 1980 equations
  Q_f<-700.75
  #H_D<-700.75
  H_D<-775 #From Cruz 2006 CFIM Code
  #H'i(H._i):heat to raise fuel from ambient (25C) to 325C (averaged from measured values in Nelson) in kJ per kg
  #or in J/gm for Albini 1980 equations
  H._i<-500.75
  #T_s: sublimination temperature ie 400C or 500c
  T_s<-500
  #C_ps:
  C_ps = 2.09 #kJ kg−1 ◦C−1 
  #T_*:
  T_star = 500 #◦C
  #T_c:
  T_c = 1000#◦C 
  #T_u
  T_u =  700 #◦C
  #Ash fraction: averaged from Nelson values
  AshFract<-0.1185
  #Char Fraction: averaged from Nelson values
  CharFract<-0.1925
  #Low heat value of combustion (ash free): averaged from Nelson values
  delta_H_O<-20775.5
  #Stefan boltzman constant
  B <- 5.67*10^-11
  #acceleration due to gravity (g)
  g<-9.81 #m/s
  #density of ambient air kg/m^3
  p_a<-1.225
  #C_p:
  C_p <- 1.05 #kJ kg−1 ◦C−1 
  #Hc Values from CFIM Code Cruz2006
  H.c<-19600
  delta_H.c<-31200
  #Calculation of delta_H_V and N_V as in Albini 1980 Appendix
  H_i<-H._i + 2570*mc
  
  #particle density kg/m^3: Constant from Cruz et al 2006: CFIM
  p_f<-398
  
  #SAV Ratio: use preset from model A8
  SAVRatio<-6561.68
  
  PackingRatio<-(sfc)/(FBDepth_m * p_f* (1-CharFract))
  
  #Rad F= radiation lost fraction (EQ 5)
  Rad_F<- 0.283 + 0.178*log(SAVRatio*PackingRatio*FBDepth_m)
  
  #H_I=H_i/(1-F)
  H_I=H_i/(Rad_F)# Modification from CFIM COde
  H_p=H_D-H._i
  #H_R=217*(1-CharFract)
  H_R=175*(1-CharFract)# Modification from CFIM COde
  H_S=836*M
  H_N=H_I + H_p + H_R + H_S
  #delta_H_V= ((1-AshFract)*delta_H_O-(CharFract-AshFract)*31200)/(1-CharFract)
  delta_H_V= ((1-0.0035)*delta_H_O-(CharFract-0.0035)*delta_H.c)/(1-CharFract) #From Cruz 2006 Code
  delta_H._V=delta_H_V + 1580
  delta_H._O=delta_H_O + 1380
  N_V=delta_H._V/3270
  N_O=delta_H._O/3270
  
  #I. Flame Structure Model
  
  
  #I.1. Flame Length
  #Calculate M from Nelson equations using ROS and HFI
  #get rate of spread in meters/sec
  R<-ros/60
  
  #Find z to solve for modified X.1 and T.m
  #Calculate Z iteratively starting at initial values until there is 0.0001s difference or smaller between r estimates:
  #set initials and constants
  
  #Constants:
  T.crit=132.5 #(K)
  p.crit=314.5 #(kg/m^3)
  P <- 101325  # Pa (standard atmospheric)
  R_u <- 8.314 # J/(mol·K)
  M_air <- 0.02897 # kg/mol
  
  # Set convergence criteria
  tolerance <- 0.0001
  max_iter <- 50
  converged <- FALSE
  iteration <- 1
  
  # Initial values
  Z_ini <- 0
  r_ini <- 75571 * FBDepth_m * PackingRatio
  
  # Initialize run log as a character vector
  runlog <- character(0)  #add inputs
  
  Z_val <- Z_ini
  r_val <- r_ini
  
  while (!converged && iteration <= max_iter) {
    
    # X_1 calculation: Volatile Fraction That burns: reformatted equation 7 for X.1, substituting H_N in for H_A1
    #X.1_test <- (H_N + (709 * Z_val) + (1045 * M)) / ((1 - CharFract) * (delta_H_V - 525 - 1024 * N_V))
    X.1 <-(H_N + 1045*M +1.05*(700-25)*Z_val)/((1-CharFract)*(delta_H_V- 525 - 1024 * N_V)) #Cruz 2006 CFIM Code
    
    #X_A1
    HA.1 = X.1 * (1 - CharFract) * (delta_H_V - 525 - 1024 * N_V) - 1045 * M - 709 * Z_val
    
    # Tm calculation: Temperature of the Mixture
    #T.m_test <- 500 + ((500 * ((2.09 * M) + (1.05 * X.1_test) * (1 - CharFract) * (1 + N_V))) /
    # ((2.09 * M) + (1.05 * Z_val) + 1.05 * (1 - CharFract) * (1 + X.1_test * N_V)))
    T.m = 500 + (500 * (2.09 * M + 1.05 * X.1 * (1 - CharFract) * (1 + N_V))) / (2.09 * M + 1.05 * Z_val + 1.05 * (1 - CharFract) * (1 + X.1 * N_V))
    
    #change to K
    T.m_K<-T.m+273.15
    p.m<- p_a*(T_a+273.15)/(T.m_K)
    
    #Flame angle: make sure A is in radians for cos() function below
    U.v <- ((2 * g * I_B) / (p_a * C_p * (T_a + 273.15)))^(1/3)
    U.h=WS_mid
    A<- atan2(U.h,U.v)
    
    
    #V_test <- (H.A1 * (1 - CharFract) * p_f * PackingRatio * FBDepth_m) /
    # (p.m_test * C_p * (T.m_test - T_star) * r_val * cos(A*pi/180))
    #must be positive: velocity of reacting mixture
    V = (H_N * PackingRatio * (1- CharFract) * FBDepth_m * p_f * (1 / cos(A))) / ((r_val * p.m * C_p * (T.m - T_star)))
    
    # Update reaction time and Z
    #using inital r from last state
    #Z: kg of air entering reacting mixture
    #Z_out <- ((p.m_test * V_test * r_val * cos(A*pi/180)) /
    #           ((1 - CharFract) * p_f * PackingRatio * FBDepth_m)) -
    # (M + (1 - CharFract) * (1 + X.1_test * N_V))
    Z_out = ((r_val * cos(A) * p.m * V) / (W_a/10)) - M - (1 + X.1 * N_V)
    
    # Particle heat transfer coefficients
    #h.r_test <- 0.5 * B * (T.m_test + T_s) * (T.m_test^2 + T_s^2)
    h.r = 0.5 * B * ((T.m+273) + (T_s+273)) * ((T.m+273)^2 + (T_s+273)^2) #Cruz 2006 CFIM
    
    #Constants set from Cruz et al 2006 cfim and calculate heat of radiation and convection: 
    #if V is negative then H.c will be NaN and the solution will fail
    k.c<- 6.63*10^-5
    v<- 1.13*10^-4
    h.c <- 0.344 * ((SAVRatio * k.c)/4) * ((4*V )/(SAVRatio*v))^0.56
    if(is.na(h.c)){h.c<-0}
    h.p <- h.r + h.c
    
    
    #r_out <- 2 * W_a * (Q_f + Q_m * M) / (h.p_test * (T.m_test - T_s))
    r_out <- 2 * (1-CharFract) * p_f * (Q_f + Q_m * M) / (SAVRatio * h.p * (T.m - T_s) )
    r_out1 = 2* (1-CharFract) * p_f * PackingRatio * FBDepth_m * (Q_f + Q_m * M) /((h.p * (T.m-T_s) * (1 - PackingRatio)))
    
    # Create a log entry for this iteration:
    log_entry <- sprintf("Iteration %d Final: r = %.5f, Z = %.5f, delta_r = %.5f, T.m = %.5f\n", 
                         iteration, r_out1, Z_out, abs(r_out1 - r_val), T.m)
    runlog <- c(runlog, log_entry)
    #cat(log_entry, "\n")
    
    # Check convergence
    if(abs(r_out1 - r_val) < tolerance){
      converged <- TRUE
      #cat("Converged!\n")
    } else {
      r_val<-r_out1
      Z_val<-Z_out
      iteration <- iteration + 1
    }
  }
  
  if (!converged) {
    warning("Iteration did not converge!")
  } else {
    Z <- Z_out
    r <- r_out1
  }
  
  #Now that model has converged, solve the equations:
  V = (H_N * PackingRatio * (1- CharFract) * FBDepth_m * p_f * (1 / cos(A))) / (r * p.m * C_p * (T.m - T_star))
  
  #Apply final value
  rxn.time<-r_out1
  
  #Depth of the reaction zone
  D<-rxn.time*R
  
  #Residence Time: not sure where these came from, but should be tan(A) equation.
  #res.time<-rxn.time*(1-(FBDepth_m/D)*tan(A*pi/180))
  #res.time<-rxn.time*(1-(FBDepth_m/D)*tan(A))
  
  #Equation: 23 
  res.time<- 2*(W_a/10)*(Q_f+Q_m*M)/(h.p*(T.m-T_s))
  
  #Fuel bed unit area burning rate: kg m^2 per s
  R.u<- (0.5*SAVRatio*PackingRatio*FBDepth_m*h.p*(T.m-T_s))/(Q_f + Q_m*M)
  
  
  #Divide amount of fuel (from crown weight) by res.time to get the burning rate ie kg per sec
  M<-CW$total_kg/res.time*0.70 #70% scaler for amount of crown included
  
  #Get the flame length:
  Z.f=10.4*(M)^(2/5)
  
  
  
  
  
}


