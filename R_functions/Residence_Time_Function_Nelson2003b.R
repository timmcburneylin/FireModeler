#Code to calculate Nelson's 2003b Fire residence time model;

#From Nelson 2003b: Reaction times and burning rates for wind tunnel headfires 

#Uses equations from Albini 1980 to solve for submodels
#model = models[x]
#fuel = fuel[x, ]
#topography = topography
#structure = structure
#weather = weather
#date = date
#IntensityType = "Byram"
#UseModel = FALSE
#ModelEFFM = FALSE

#Load functions:
#install.packages(c("rootSolve"))
library(firebehavioR)
library(dplyr)
library(stats)
library(geosphere)

source(file.path(root, "R_functions", "rothermel_function_mod.R"))
#Load Custom Fuel Models
CustomModels<-read.csv(file.path(root, "templates", "CustomFuelModels.csv"))
fuelModels<-rbind(fuelModels[,1:16],CustomModels[,2:17])


residence_time<-function(
    model,
    fuel, #in MG/ha which is kg/m^2 *10 or T/ha
    topography,
    structure,
    weather,
    date, #as "2024-01-01" or "YYYY-MM-DD"
    IntensityType, #Either Nelson, Byram or Rothermel
    UseModel,
    ModelEFFM
){
  
  
  #Constants:----------------------------------
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
    
  #--------------
  
  #Load existing values:----------------------
    #W_a: load fine fuel loads in Mg per ha
    FineFuelLoad= fuel$hr1+fuel$hr10+ fuel$hr100
    W_a<-FineFuelLoad
    #Ambient air temp in Celsius
    AirTemp=weather$temp
    T_a<-AirTemp
    #fuelbed depth in meters
    FBDepth_m<-fuel$depth/100
    #Calculate FMC for the preset date and lat, long, and elevation.
    FMC<-FMC_calc(LAT=topography$lat,
                  LON=topography$lon,
                  ELEV=topography$elev,
                  DATE=date)
    #heat of combustion
    H<-18600
    
    #midflame wind speed in meters per second
    WindSpeed=weather$ws
    wind_adj<-firebehavioR::waf(fuel$depth,structure$ht,structure$cr, structure$cc)
    WS_mid<-wind_adj*WindSpeed/3.6

      #Load in fuelmodels from the firebehavioR function which uses US fuel models
    if(UseModel == TRUE){ #extract standardized data
      MODEL<-toupper(model)
      fuels_data<-fuelModels[MODEL,]
      
    } else {#if your fuel loads exist and convert to Mg/ha for roth function
      MODEL<-toupper(model)
      fuels_data<-fuelModels[MODEL,]
      fuels_data$loadLitter<-fuel$litter
      fuels_data$load1hr<-fuel$hr1
      fuels_data$load10hr<-fuel$hr10
      fuels_data$load100hr<-fuel$hr100
      fuels_data$fuelBedDepth<-fuel$depth
      fuels_data$heat<-H
    }
    
  #Calculations:
  #Rothermel Intensity
    #model fine fuel moisture if not given
    if(ModelEFFM==FALSE){
      effm<-data.frame(
        fmLitter=weather$ffm,
        fm1hr=weather$ffm,
        fm10hr=weather$ffm+1,
        fm100hr=weather$ffm+3
      )
    }else{
    effm<- ffm(method="anderson",
               rh=weather$rh,
               temp=weather$temp,
               month=substr(date,6,7),
               hour=15, #always 3 pm
               asp=topography$asp,
               slp=topography$slp,
               bla="b",
               shade=ifelse(structure$cc>50,"yes","no"))
    }
    
    #Moisture content as a fraction
    MoistureContent=effm$fm1hr
    M<-MoistureContent/100
    
    #Herb Live and woody
    #Calculate live herb and woody fuel moisture  
    ##Calculate Growing Season Index and Use that to generate Live woody and Herb Fuel Moisture:
    #get Temp min, photo period, and Vapor Pressure Deficit
    T_min <- weather$t_min
    photo_per<-geosphere::daylength(topography$lat,date)
    VPD<-(RHtoVPD(weather$rh, weather$temp, Pa = 101))*1000
    
    #calculate indicator value from those 3 values: equations from: Jolly et al.2005 GSI https://www.youtube.com/watch?v=w8Ukio93BMU
    
    i_pp<- ifelse(photo_per> 11,1,ifelse(photo_per< 10,0,photo_per-10))
    i_VPD<- ifelse(VPD> 4100,0,ifelse(VPD< 900,1,(VPD*(-1/3200)+1.2813)))
    i_Tmin<-  ifelse(T_min> 5,1,ifelse(T_min< -2,0,(T_min*(1/6)+0.3333)))
    #
    GSI<-i_Tmin*i_pp*i_VPD
    herb_live<- ifelse(GSI<0.5,30,((440*GSI)-190))
    woody_live<- ifelse(GSI<0.5,60,((280*GSI)-30))
    
    #set up rothermel input dataframes
    moisture_df<- data.frame(
      litter=effm$fmLitter,
      hr1=effm$fm1hr,
      hr10=effm$fm10hr,
      hr100=effm$fm100hr,
      live_herb=herb_live,
      live_woody=woody_live
    )
    #
    crown_fuel<-data.frame(
      cbd=structure$cbd,
      fmc=FMC,
      cbh=structure$cbh,
      cfl=structure$cfl
    )
    #
    enviro_df<-data.frame(
      slope=topography$slp,
      ws=weather$ws,
      wind_d=weather$wd,
      wind_adj=wind_adj
    )
    
    #Inputs
    
    load<-fuels_data %>% dplyr::select(load1hr,load10hr,load100hr,loadLiveHerb,loadLiveWoody)
    sav<-fuels_data %>%dplyr::select(sav1hr,sav10hr,sav100hr,savLiveHerb,savLiveWoody)
    depth<-fuel$depth
    mxdead<-fuels_data %>% dplyr::select(mxDead)
    heat<-rep(H,5)
    moist<- cbind(effm$fm1hr,effm$fm10hr,effm$fm100hr,herb_live,woody_live)
    u1<-WS_mid
    slp<-topography$slp
    
    #Run Rothermel
    #Rothermel_firebehave<-firebehavioR::rothermel(surfFuel = fuelModels[model,],
     #                                     moisture = moisture_df,
      #                                    crownFuel =crown_fuel,enviro =enviro_df,
       #                                   rosMult = 1.7,cfbForm = "w",folMoist = "n")
    Rothermel_firebehave<-rothermel_mod(surfFuel = fuels_data,
                                                  moisture = moisture_df,
                                                  crownFuel =crown_fuel,enviro =enviro_df,
                                                  rosMult = 1.7,cfbForm = "sr",folMoist = "y")
    FuelConsumed<-Rothermel_firebehave$fireBehavior$`Heat per Unit Area [kJ/m2]`/H
    
    # Use the local rothermel_mod surface ROS instead of the external
    # Rothermel package's ros() helper.
    
    #Rate of spread into Meters per second
    R<-max(
      Rothermel_firebehave$detailSurface$`Potential ROS [m/min]`/60,
      Rothermel_firebehave$fireBehavior$`Rate of Spread [m/min]`/60
    )
    
    #particle density kg/m^3: Constant from Cruz et al 2006: CFIM
    p_f<-398
    
    #Packing Ratio from Rothermel or from cfim code
    #PackingRatio<-Rothermel_ros$`Packing ratio [dimensionless]`
    #Code from CFIM Cruz 2006: change fuel load from Mg/ha to kg/m^2
    PackingRatio<- (W_a/10)/(FBDepth_m * p_f* (1-CharFract))
    
    #SAV Ratio
    SAVRatio<-Rothermel_firebehave$detailSurface$`Characteristic SAV [m2/m3]`
    
    #F= radiation lost fraction (EQ 5)
    Rad_F<- 0.283 + 0.178*log(SAVRatio*PackingRatio*FBDepth_m) 
    
    #Calculate effective heat of combustion from: Babrauskkas 2006
    #H<-(16.52-0.057*FMC)*1000
    #or use standard from cfim
    H<-18600
    
    #or use Albini Equations to calculate it by Fuel Size and SAVratio weighting the mean heat of combust
    mc_vals <- c(effm$fmLitter, effm$fm1hr, effm$fm10hr, effm$fm100hr) / 100 
    sav_vals <- c(fuels_data$savLitter, fuels_data$sav1hr, fuels_data$sav10hr, fuels_data$sav100hr)
    load_vals<-c(fuels_data$loadLitter, fuels_data$load1hr, fuels_data$load10hr, fuels_data$load100hr)
    
    # Define the function to calculate heat of combustion
    calc_heat_combust <- function(mc, SAV,load) {
      # H._i, H_D, PackingRatio, FBDepth_m, CharFract
      L<-load
      H_i   <- H._i + 2570 * mc
      Rad_F <- 0.283 + 0.178 * log(SAV * PackingRatio * FBDepth_m)
      H_I   <- H_i / Rad_F            # Modification from CFIM Code
      H_p   <- H_D - H._i
      H_R   <- 175 * (1 - CharFract)    # Modification from CFIM Code
      H_S   <- 836 * mc
      H_N   <- (H_I + H_p + H_R + H_S) #* 10
      
      return(data.frame(mc = mc, SAV = SAV, H_N = H_N, Load=L))
    }
    
    heat_df <- calc_heat_combust(mc_vals, sav_vals,load_vals)
    #clean heat
    heat_df$H_N <- abs(heat_df$H_N)
    #H <-weighted.mean(heat_df$H_N, heat_df$Load)
    
#If no fire is predicted then exit
    if(is.na(R) | R==0 | R<=0.01){
      Results<-list(
        Residence.Time.s=0,
        Reaction.Time.s=0,
        Surface.Area.Burning.Rate.kg..m2.s=0,
        Depth.Reaction.Zone.m=0,
        RateOfSpread.m.s=0,
        Intensity.kw.m2=0,
        Plume.Velocity=0,
        Mean.Heat.Combust=H,
        run_log = NA,
        FLAG= "NoFire"
      )
      return(Results)
    }

    #Get Intensity with 2 ways
    if(IntensityType =="Nelson"){
      #Nelson's equation ()
      I_B<-0.85*H*(1 - CharFract)*p_f*FBDepth_m*PackingRatio*R     

      }else if(IntensityType=="Byram"){
      #Byram's Intensity Equation      
      #I_B<-H*(W_a/10)*R
      I_B<-H*(FuelConsumed)*R
      
      }else{
      #Native Rothermel Calculation Reaction Intensity:      
      I_B<-Rothermel_firebehave$fireBehavior$`Fireline Intensity [kW/m]`
      }
    
    if(I_B<0){
      Results<-list(
        Residence.Time.s=0,
        Reaction.Time.s=0,
        Surface.Area.Burning.Rate.kg..m2.s=0,
        Depth.Reaction.Zone.m=0,
        RateOfSpread.m.s=0,
        Intensity.kw.m2=0,
        Plume.Velocity=0,
        Mean.Heat.Combust=H,
        run_log = NA,
        FLAG= "NoFire"
      )
      return(Results)
    }
  

  
  #Calculation of delta_H_V and N_V as in Albini 1980 Appendix
  H_i<-H._i + 2570*M
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
  
  
  
  #Make export df
  output.df<-data.frame(
    Residence.Time.s=res.time,
    Reaction.Time.s=rxn.time,
    Surface.Area.Burning.Rate.kg..m2.s=R.u,
    Depth.Reaction.Zone.m=D,
    RateOfSpread.m.s=R,
    Intensity.kw.m2=I_B,
    Plume.Velocity=V
  )
  
  Results<-list(
    Residence.Time.s=res.time,
    Reaction.Time.s=rxn.time,
    Surface.Area.Burning.Rate.kg..m2.s=R.u,
    Depth.Reaction.Zone.m=D,
    RateOfSpread.m.s=R,
    Intensity.kw.m2=I_B,
    Plume.Velocity=V,
    Mean.Heat.Combust=H,
    FuelConsumed_kg.m=FuelConsumed,
    run_log = runlog,
    FLAG="Fire"
  )

    #CLEAN OUTPUT dF if no fire exists
  if(output.df$Residence.Time.s == -Inf | output.df$Intensity.kw.m2 == 0){
    Results<-list(
      Residence.Time.s=res.time,
      Reaction.Time.s=0,
      Surface.Area.Burning.Rate.kg..m2.s=0,
      Depth.Reaction.Zone.m=D,
      RateOfSpread.m.s=R,
      Intensity.kw.m2=I_B,
      Plume.Velocity=0,
      Mean.Heat.Combust=H,
      FuelConsumed_kg.m=FuelConsumed,
      run_log = runlog,
      FLAG= "NoFire"
    )
  }

    return(Results)
}
