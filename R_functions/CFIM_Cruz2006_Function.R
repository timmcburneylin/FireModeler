#Code to canopy ignition in forests: CFIM model framework

#From Cruz et al 2006: Predicting the ignition of crown fuels above a spreading surface fire. 
#Part I: model idealization

#Fully physical fire behavior model mixed with empirical approaches based on heat loss and conservation of energy and mass and momentum
#Predicts the probability of a spreading equilibrium surface fire ignition fuels in the lower layer of the canopy. Does not predict crown fire
#spread per se but can be linked to other fire behavior models

#Load functions:
#install.packages(c("rootSolve"))
if (requireNamespace("Rothermel", quietly = TRUE)) library(Rothermel)
if (requireNamespace("firebehavioR", quietly = TRUE)) library(firebehavioR)
if (requireNamespace("plantecophys", quietly = TRUE)) library(plantecophys)
library(dplyr)
library(stats)
if (requireNamespace("rootSolve", quietly = TRUE)) library(rootSolve)
library(zoo)
library(cubature)


source_fun("Residence_Time_Function_Nelson2003b.R")
source_fun("rothermel_function_mod.R")
source_fun("CFC_Groot_Function.R")
source_fun("FMC_Function.R")
source_fun("SFC_Function.R")
source_fun("CFIM_SpatialLocation_Function.R")


#Inputs are a series of data frames describing fuels topography, stand structure, and weather. 
#The column orders do not matter but the names do. 
#See below

#Test Datasets:  
#Fuel:
  #fuel<-data.frame(
  #  litter=0.25,
  #  hr1=0.75,
  #  hr10=1.0,
  #  hr100=3.0,
  #  depth=50,
  #  CAN_model="C-5")
  #Topography:
  #topography<-data.frame(
  #  lat=50.082,
  #  lon=-123.041,
  #  elev=1000, #meters
  #  asp="S", #coordinate direction
  #  slp=70 #percent
  #)
  #Structure:
  #structure<-data.frame(
    #ht=20,
    #cc=45,
    #cr=70,
    #dbh=0.30, #meters
    #cbh=2.0,
   # cbd=0.1,
  #  cfl=1.0,
 #   ba= 25,
#    sd=450 #trees per hectare
 # )
  #Weather:
#  weather<-data.frame(
    #temp=30, #celsius
    #rh=35,
    #t_min=15,#celsius
    #dc=300,
    #dmc=25,
    #bui=75,
    #ffm=6,
  #  ws=15,
   # wd=45
  #)
  #Date:
  #date <- as.Date("2020-07-15") 
  
  #Fuel Model:USA 
  #model<-"TL3"
  
  #CalcRadAdsorb=TRUE

CFIM<- function(
    model,
    fuel,
    topography,
    structure,
    weather,
    date,
    IntensityType,
    SurfaceModel,
    CalcRadAdsorb,
    UseModel,
    ModelEFFM){
  
##Setup: 
  #Calculate FMC for the preset date and lat, long, and elevation.
  FMC<-FMC_calc(LAT=topography$lat,
                LON=topography$lon,
                ELEV=topography$elev,
                DATE=date)
  
  
  #Calculate Surface fuel consumption with W.J Groot et al. 2007
    sfc<-sfc(dc=weather$dc,
             ffl=sum(fuel$litter,fuel$hr1,fuel$hr10,fuel$hr100),
            ffmc=weather$ffmc,
            dmc=weather$dmc,
            bui=weather$bui,
            depth=fuel$depth,
            fueltype=fuel$CAN_model,
            bd=1)
    
  #Heat of combustion standard
    H<-18600
  #Calculate fine fuel moisture WITH Anderson Model
    #if exists
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
    
  #Calculate live herb and woody fuel moisture  
    ##Calculate Growing Season Index and Use that to generate Live woody and Herb Fuel Moisture:
    #get Temp min, photo period, and Vapor Pressure Deficit
    T_min <- weather$t_min
    photo_per<- geosphere::daylength(topography$lat,date)
    VPD<-(RHtoVPD(weather$rh, weather$temp, Pa = 101))*1000
    
    #calculate indicator value from those 3 values: eqU_ations from: Jolly et al.2005 GSI https://www.youtube.com/watch?v=w8Ukio93BMU
    
    i_pp<- ifelse(photo_per> 11,1,ifelse(photo_per< 10,0,photo_per-10))
    i_VPD<- ifelse(VPD> 4100,0,ifelse(VPD< 900,1,(VPD*(-1/3200)+1.2813)))
    i_Tmin<-  ifelse(T_min> 5,1,ifelse(T_min< -2,0,(T_min*(1/6)+0.3333)))
    #
    GSI<-i_Tmin*i_pp*i_VPD
    herb_live<- ifelse(GSI<0.5,30,((440*GSI)-190))
    woody_live<- ifelse(GSI<0.5,60,((280*GSI)-30))
  
  #calculate wind adjustment
    wind_adj<-firebehavioR::waf(fuel$depth,structure$ht,structure$cr, structure$cc)
    
  #Load in fuelmodels from the firebehavioR function which uses US fuel models
  if(UseModel == TRUE){ #extract standardized data
    MODEL<-toupper(model)
    fuels_data<-fuelModels[MODEL,]
    
  }else{#if your fuel loads exist and convert to Mg/HA
    MODEL<-toupper(model)
    fuels_data<-fuelModels[MODEL,]
    fuels_data$loadLitter<-fuel$litter*10
    fuels_data$load1hr<-fuel$hr1*10
    fuels_data$load10hr<-fuel$hr10*10
    fuels_data$load100hr<-fuel$hr100*10
    fuels_data$fuelBedDepth<-fuel$depth
    fuels_data$heat<-H
  }
    #setup Rothermel input data frames or vectors
    load<-fuels_data %>% dplyr::select(load1hr,load10hr,load100hr,loadLiveHerb,loadLiveWoody)
    sav<-fuels_data %>%dplyr::select(sav1hr,sav10hr,sav100hr,savLiveHerb,savLiveWoody)
    depth<-fuel$depth
    mxdead<-fuels_data %>% dplyr::select(mxDead)
    heat<-rep(H,5)
    moist<- cbind(effm$fm1hr,effm$fm10hr,effm$fm100hr,herb_live,woody_live)
    U<-weather$ws
    u1<-wind_adj*weather$ws
    FBDepth_m<-fuel$depth/100
    slp<-topography$slp
    CBD<-structure$cbd
    CFL<-structure$cfl
    CBH<-structure$cbh
    FSG<-structure$cbh-FBDepth_m
    DBH<-structure$dbh
    BA<-structure$ba/10000 #Basal area in m^2
  
#Calculate Surface rate of spread with any equation-----------------------------------------------
    
##Rothermel--------------------------
    #with ros function
    #ROTH_res <-ros(modeltype = "D",
     #              w=unlist(unname(as.vector(load))),
      #             s=unlist(unname(as.vector(sav))),
       #            delta=unlist(unname(as.vector(depth))),
        #           mx.dead = unlist(unname(as.vector(mxdead))),
         #         h=as.vector(heat),
          #        m=as.vector(moist),
           #       u=as.vector(u1),
            #      slope=as.vector(slp))
    #ros_ROTH_s<-ROTH_res$`ROS [m/min]`/60
  
    #Rothermel with rothermel function
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
    cbd=CBD,
    fmc=FMC,
    cbh=CBH,
    cfl=CFL
  )
  #
  enviro_df<-data.frame(
    slope=slp,
    ws=weather$ws,
    wind_d=weather$wd,
    wind_adj=wind_adj
  )
  #Run rothermel
  #Rothermel_res<-rothermel_mod(surfFuel = fuels_data[,1:16],
   #                                     moisture = moisture_df,
    #                                    crownFuel =crown_fuel,enviro =enviro_df,
     #                                   rosMult = 1.7,cfbForm = "w",folMoist = "y")
  #Redone function
  Rothermel_res<-rothermel_mod(surfFuel = fuels_data[,1:16],
                           moisture = moisture_df,
                          crownFuel =crown_fuel,enviro =enviro_df,
                          rosMult = 1.7,cfbForm = "w",folMoist = "y")
  ros_ROTH_s<-Rothermel_res$fireBehavior$`Rate of Spread [m/min]`/60
  ros_ROTH_m<-Rothermel_res$fireBehavior$`Rate of Spread [m/min]`
  
  
  
  

##Albini 1985--------------------------
  
  
##Balbi 2020---------------------------
  
##Residence Time Nelson------------------
  
  #Flame residence time: calculated from surface area to volume ratios in Rothermel function 
  #(similiar but simpler to reaction time from Nelson 2003b)
  #rt_F<-Rothermel_res$fireBehavior$`Flame Residence Time [min]`*60
  
  #Use Nelson 2003b function to get residence time:
  Nelson_Results<-residence_time(model = model,
                                 fuel = fuel,
                                 weather=weather,
                                 structure = structure,
                                 topography = topography,
                                 date= date,
                                 IntensityType = "Nelson",
                                 UseModel = FALSE,
                                 ModelEFFM = ifelse(ModelEFFM == TRUE,TRUE,FALSE))
  rt_F<-Nelson_Results$Reaction.Time.s
  v_p<-Nelson_Results$Plume.Velocity
  cat(sprintf("Flame residence time of %.2f seconds. Evaluating model over entire residence time\n", 
              rt_F))
  NelsonLog<-Nelson_Results$run_log
  H_mean<-Nelson_Results$Mean.Heat.Combust
  
  #End model if Nelson predicts no fire
  if(Nelson_Results$Residence.Time.s == -Inf | Nelson_Results$Residence.Time.s == 0|Nelson_Results$Intensity.kw.m2 == 0){
    Results<-Nelson_Results
    Results$CFIM_pCrown="No"
    cat(sprintf("Nelson Model predicts no surface fire. Ending model run.\n"))
  return(Results)
    }

##Modular CFIM-------------------------------------------------------------------

  ##Constants and Calculations
  #Temperature Ambient and Ignition Temp
  T_a <- weather$temp + 273.15 # Convert temperature to Kelvin
  T_ig <- 600 # Ignition temperature in Kelvin
  
  #Rate of spread from chosen surface fuel model
  ROS<-ros_ROTH_s
  
  #Stefan boltzman constant
  B <- 5.67*10^-11
  #acceleration due to gravity (g)
  g<-9.81 #m/s
  #density of ambient air kg/m^3
  p_a<-1.225
  #C_p:
  C_p <- 1.006 #kJ kg−1 ◦C−1 or K
  #particle density kg/m^3: Constant from Cruz et al 2006: CFIM
  p_f<-398
  
  # w_a = fuel consumed in flaming combustion (kg/m²)
  # U_a = midflame windspeed (m/s)
  # MC = moisture content (percent)
  W_a <- ifelse(sfc > sum(fuel$litter,fuel$hr1,fuel$hr10,fuel$hr100),sum(fuel$litter,fuel$hr1,fuel$hr10,fuel$hr100),sfc)  # Assuming sfc is already in kg/m²
  U_a <- weather$ws*wind_adj/3.6  # Convert to m/s (assuming ws is in km/h)
  MC <- effm$fm1hr/100  # Moisture content in fraction
  
  #Structure
  SH<-structure$ht
  
  #Wind at stand height
  U<-weather$ws
  Ush<- U * log((SH - 0.64 * SH) / (0.13 * SH)) / 
    log((H - 0.64 * SH) / (0.13 * SH))
  
  #Get Intensity with Nelson
  I_B<-Nelson_Results$Intensity
  
  #Get Flame Length
  FL<-0.0775*I_B^0.46
  
  #Get Flame Height  
  FH<-0.002597*I_B/U_a
  
  #Uv
  Uv<-((2*g*I_B)/(p_a*C_p*(T_a)))^(1/3)
  
  #Flame Depth and Width:
  D_f<-ROS*rt_F
  W_f<- 20

  
  #Packing Ratio of the Canopy Fuels
  PackingRatio_Canopy<-CBD/510
  
  #canopy fuel specific heat J/kg/k
  Cp_f<-2100.0
  #Water specific heat J/kg/k
  Cp_w<-4187.0
  #Latent heat of evaporation of water
  L<-2254000.0
  #
  Fd <- 0.0013
  Fl <- 0.1
  #initals for plume width and velocity
  P_w=D_f/2
  Upi=((2*g*I_B)/(p_a*C_p*(T_a)))^(1/3)
  
  
  
##Functions-------------------------------
  #Functions to predict the behavior of the radiation and plume of the evolving surface fire
  #Derivative Function to solve for the derivatives of spatial locations of moving suface
  #Mercer and Weber 1994 function for line fires 
  
  ODE_func <- function(Ush, bi, wi, SH, T_a) {
    # Constants
    Ti <- 800
    Ta <- T_a
    alpha <- 0.16
    beta <- 0.5
    rho_a <- 1.2
    g <- 9.8
    WindAlpha <- 1.2
    smin <- 0
    smax <- 40
    
    # Compute the initial density (rho_i)
    rho_i <- rho_a * (Ta / Ti)
    
    # Initial boundary conditions:
    y1i <- rho_i * bi * wi
    y2i <- rho_i * bi * wi^2
    y3i <- pi/2    # π/2 radians (90°)
    y4i <- rho_i * bi * wi * (Ti - Ta)
    y5i <- 0
    y6i <- 0
    
    # Pack the initial conditions into a named vector.
    y0 <- c(y1 = y1i, y2 = y2i, y3 = y3i, y4 = y4i, y5 = y5i, y6 = y6i)
    
    # Define the ODE system function.
    # "s" is the independent variable.
    odeFunc <- function(s, state, parameters) {
      # Unpack the state variables.
      y1 <- state["y1"]
      y2 <- state["y2"]
      y3 <- state["y3"]
      y4 <- state["y4"]
      y5 <- state["y5"]
      y6 <- state["y6"]
      
      # Compute Ua according to the condition on y6/SH:
      if (y6 / SH > 0.6) {
        Ua <- Ush * exp(WindAlpha * ((y6 / SH) - 1))
      } else {
        Ua <- Ush * exp(WindAlpha * (0.6 - 1))
      }
      
      # Compute effective Nu (νₑ):
      # νₑ = α*(y2/y1 - Ua*cos(y3)) + β*Ua*sin(y3)
      nu_e <- alpha * (y2 / y1 - Ua * cos(y3)) + beta * (Ua * sin(y3))
      
      # ODE definitions:
      # dy1/ds = ρa * νₑ
      dy1 <- rho_a * nu_e
      
      # dy2/ds = ρa * νₑ * Ua*cos(y3) + (y1*y4*g*sin(y3))/(y2*Ta)
      dy2 <- rho_a * nu_e * Ua * cos(y3) + (y1 * y4 * g * sin(y3)) / (y2 * Ta)
      
      # dy3/ds = - ( ρa*νₑ*Ua*sin(y3) - (y1*y4*g*cos(y3))/(y2*Ta) )/y2
      dy3 <- - (rho_a * nu_e * Ua * sin(y3) - (y1 * y4 * g * cos(y3)) / (y2 * Ta)) / y2
      
      # dy4/ds is zero, so y4 remains constant:
      dy4 <- 0
      
      # dy5/ds = cos(y3)
      dy5 <- cos(y3)
      
      # dy6/ds = sin(y3)
      dy6 <- sin(y3)
      
      return(list(c(dy1, dy2, dy3, dy4, dy5, dy6)))
    }
    
    # Create a sequence for the independent variable s
    s_seq <- seq(smin, smax, length.out = 200)
    
    # Solve the ODE system.
    sol <- ode(y0, times = s_seq, func = odeFunc, parms = NULL)
    
    return(sol)
  }
  
  #Wind Profile function as a result of Windspeed, stand Height 
  #and Height above the fire
  WindProfile <- function(U, SH, z) {
    alpha <- 1.2
    H <- 10 + SH
    
    Ush <- U * log((SH - 0.64 * SH) / (0.13 * SH)) / 
      log((H - 0.64 * SH) / (0.13 * SH))
    
    Ucio <- Ush * exp(alpha * (z / SH - 1))
    
    if (SH < z) {
      result <- U * log((z - 0.64 * SH) / (0.13 * SH)) / 
        log((H - 0.64 * SH) / (0.13 * SH))
    } else if ((z / SH) > 0.6) {
      result <- Ucio
    } else {
      result <- Ush * exp(alpha * (0.6 - 1))
    }
    
    return(result)
  }
  
  #Fa and Fv
  Fa_calc<-function(Fd,Fl)
    { 2*pi*(Fd/2)*Fl }
  Fv_calc<-function(Fd,Fl)
    { pi*(Fd/2)^2*Fl}
  
  #Sigma
  Sigma_calc<- function(Fd,Fl){
    Fa_calc(Fd,Fl)/Fv_calc(Fa,Fl)
  }
  
  #Plume Limit Functions
  PlumeLimit_S<-function(cbh,ODE_Results){
    Spoints <- c(fire_position(cbh,type="RearS",ODE_Results = ODE_Results), fire_position(cbh,type="CenterS",ODE_Results = ODE_Results), fire_position(cbh,type="ForwardS",ODE_Results = ODE_Results))
    return(Spoints)  
    }
  PlumeLimit_X<-function(cbh,ODE_Results){
    Xpoints <- c(fire_position(cbh,type="RearPlume",ODE_Results = ODE_Results), fire_position(cbh,type="CenterPlume",ODE_Results = ODE_Results), fire_position(cbh,type="ForwardPlume",ODE_Results = ODE_Results))
    return(Xpoints)
  }
  
  #Temperature of Gas above the flame as a function of time and ambient temp + midflame wind
  Tgas <- function(t, U_a, Wa, MC, taur, T_a) {
    # Local constants
    Tamb <- T_a
    b1 <- 300.68407718
    b2 <- 136.79152366
    b3 <- 0.506059107
    b4 <- 100.44776556
    b5 <- -0.530496458
    k <- 1
    FlameBeta <- 8
    U<-U_a
    
    # EQ(5): Maximum flame temperature model (Kelvin)
    Tmax <- Tamb + b1 * Wa + b2 * U^b3 + b4 * MC^b5
    
    # EQ(6): Non-dimensional temperature associated with the arrival of the reaction zone
    Adimen600 <- (600 - Tamb) / (Tmax - Tamb)
    
    # Compute the "rise" from the logarithmic relation.
    Rise <- sqrt(-1 * FlameBeta^2 * log(Adimen600))
    ig <- -Rise
    
    # Calculate the decay constant.
    Decay <- -log(Adimen600) / (taur + ig)
    
    # Determine the coefficient based on the time shift (t + ig).
    coef <- if (t + ig < 0) {
      k * exp(-((t + ig)^2 / (FlameBeta^2)))
    } else {
      k * exp(-Decay * (t + ig))
    }
    
    # Compute the final flame temperature.
    res <- coef * (Tmax - Tamb) + Tamb
    return(res)
  }

    #Plume Gas Temperature and Velocity Module:
  PlumeGaussTandV <- function(cbh,ODE_Results, U_a, T_a){
    # Unpack the ODE interpolation functions
    y1 <- ODE_Results$y1
    y2 <- ODE_Results$y2
    y3 <- ODE_Results$y3
    y4 <- ODE_Results$y4
    y5 <- ODE_Results$y5
    y6 <- ODE_Results$y6
    
    # --- Local parameters ---
    N1 <- 1.35
    lambda <- 1
    
    # Call positional functions for plume location
    plumeforward<-fire_position(cbh,type="ForwardPlume",ODE_Results = ODE_Results)
    plumecenter<-fire_position(cbh,type="CenterPlume",ODE_Results = ODE_Results)
    plumerear<-fire_position(cbh,type="RearPlume",ODE_Results = ODE_Results)
    
    #Plume Limits
    plumlimX <- PlumeLimit_S(cbh,ODE_Results = ODE_Results) 
    plumlimS <- PlumeLimit_X(cbh,ODE_Results = ODE_Results) 
    
    #more positions
    RearS   <- fire_position(cbh, type = "RearS",   ODE_Results = ODE_Results)
    CenterS <- fire_position(cbh, type = "CenterS", ODE_Results = ODE_Results)
    
    
    #Compute forward and backward lengths from the limits.
    LengthFor <- plumlimX[[3]] - plumlimX[[2]]
    LengthBac <- plumlimX[[1]] - plumlimX[[2]]
    
    # --- Temperature and Position on the “Forward” side ---
    s_seq_for <- seq(plumlimS[[2]], plumlimS[[3]], by = 0.01)
    
      TempListFor <- sapply(s_seq_for, function(s){
      300 + (N1 / lambda^2) * (y4(s)/y1(s)) *
        exp( - (((y6(s) - cbh) / cos(pi/2 - y3(s)))^2 /
                  (lambda^2 * ( y1(s) * (y1(s) / (1.2 * (300 / (300 + (y4(s)/y1(s)))))* y2(s) ))^2)))
  })
      XListFor <- sapply(s_seq_for, function(s) {
        - y5(s) - ( cos(pi/2 - y3(s)) * ( abs(y6(s) - cbh) / sin(pi/2 - y3(s)) ) )
      })

          # --- Temperature and Position on the “Backward” side ---
      s_seq_bac <- seq(RearS, CenterS, by = 0.01)
      
      TempListBac <- sapply(s_seq_bac, function(s) {
        300 + (N1 / lambda^2) * (y4(s) / y1(s)) *
          exp( - (((cbh - y6(s)) / sin(pi/2 - y3(s)))^2 /
                    (lambda^2 * ( y1(s) * (y1(s) / (1.2 * (300 / (300 + (y4(s)/y1(s)))))* y2(s) ))^2)))
               })
        
        XListBac <- sapply(s_seq_bac, function(s) {
          - y5(s) + ( cos(pi/2 - y3(s)) * ((cbh - y6(s)) / sin(pi/2 - y3(s))))
        })
        
        # --- Combine forward and backward data ---
        Xlist <- c(XListBac, XListFor)
        TempList <- c(TempListBac, TempListFor)
        # XTatZ: a two-column matrix: x and temperature
        XTatZ <- cbind(Xlist, TempList)
        
        # --- Compute Velocity Lists, similarly for forward and backward ---
        VelListFor <- sapply(s_seq_for, function(s){
          1.01 / lambda^2 * (y2(s) / y1(s)) *
            exp( - (((y6(s) - cbh) / cos(pi/2 - y3(s)))^2 /
                      (lambda^2 * ( y1(s) * (y1(s) / (1.2 * (300 / (300 + (y4(s)/y1(s)))))* y2(s) ))^2)))
                 })
          
          VelListBac <- sapply(s_seq_bac, function(s){
            1.01 / lambda^2 * (y2(s) / y1(s)) *
              exp( - (((cbh - y6(s)) / sin(pi/2 - y3(s)))^2 /
                        (lambda^2 * ( y1(s) * (y1(s) / (1.2 * (300 / (300 + (y4(s)/y1(s)))))* y2(s) ))^2)))
                })
            
            VelList <- c(VelListBac, VelListFor)
            XVatZ <- cbind(Xlist, VelList)
            
            # --- Build “extended” x, temperature, and velocity profiles ---
            # Create additional arrays on the left-hand side of the domain.
            x_seq_a <- seq(-20, plumlimX[[1]], by = 0.02)
            aT <- rep(T_a, length(x_seq_a))
            # U_a should be defined elsewhere (e.g. the upstream wind speed)
            aV <- rep(U_a, length(x_seq_a))
            aX <- -x_seq_a
            
            # Create arrays on the right-hand side of the domain.
            x_seq_b <- seq(plumlimX[[3]], 20, by = 0.02)
            bT <- rep(T_a, length(x_seq_b))
            bV <- rep(U_a, length(x_seq_b))
            bX <- -x_seq_b
            
            # Combine the left segment, the computed plume data, and the right segment.
            temperatureXZ1 <- c(aT, TempList)
            tempatXZ <- c(temperatureXZ1, bT)
            
            velocityXZ1 <- c(aV, VelList)
            velatXZ <- c(velocityXZ1, bV)
            
            Xlocation1 <- c(aX, Xlist)
            Xlocat <- c(Xlocation1, bX)
            
            # --- Smooth the profiles using a moving average ---
            # Load the zoo package for rollmean(); if not installed, first install it via install.packages("zoo")
            
            MovAveXlocat <- rollmean(Xlocat, 10, fill = NA, align = "center")
            MovAveTempxz <- rollmean(tempatXZ, 10, fill = NA, align = "center")
            MovAveVelxz <- rollmean(velatXZ, 10, fill = NA, align = "center")
            
            # Build two matrices (or two-column data frames) of the smoothed profiles.
            XTlineatZ <- cbind(MovAveXlocat, MovAveTempxz)
            XVlineatZ <- cbind(MovAveXlocat, MovAveVelxz)
            
            # --- Create continuous interpolating functions ---
            # Here we use splinefun() to build a smooth function; you could also use approxfun() for linear interpolation.
            InterpolXTZ <- splinefun(XTlineatZ[, 1], XTlineatZ[, 2])
            InterpolXVZ <- splinefun(XVlineatZ[, 1], XVlineatZ[, 2])
            # Optionally, you might return all the calculated items in a list:
            return(list(
              XTatZ = XTatZ,         # Original (unsmoothed) x-temperature pairs
              XVatZ = XVatZ,         # Original (unsmoothed) x-velocity pairs
              MovAveXlocat = MovAveXlocat,
              MovAveTempxz = MovAveTempxz,
              MovAveVelxz = MovAveVelxz,
              InterpolXTZ = InterpolXTZ,   # Interpolating function for temperature vs. x
              InterpolXVZ = InterpolXVZ    # Interpolating function for velocity vs. x
            ))
        }
  
  #Temeperature of the gas as a function of X position:
  TxzFakeGauss <- function(cbh,ODE_Results) {
    # Constants
    N1 <- 1.35
    lambda <- 1
    
    fire_position(cbh,type="ForwardPlume",ODE_Results = ODE_Results)
    fire_position(cbh,type="CenterPlume",ODE_Results = ODE_Results)
    fire_position(cbh,type="RearPlume",ODE_Results = ODE_Results)
    
    # Assume PlumLimX and PlumLimS return numeric vectors.
    plumlimX <- PlumeLimit_X(cbh,ODE_Results = ODE_Results)  
    plumlimS <- PlumeLimit_S(cbh,ODE_Results = ODE_Results) 
    
    # Compute lengths based on plumlimX:
    LenghtFor <- plumlimX[3] - plumlimX[2]
    LenghtBac <- plumlimX[1] - plumlimX[2]
    
    # Use the second element of plumlimS as the substitution value for 's'
    s_val <- plumlimS[2]
    
    # --- Obtain solution values for y1 and y4 at s = s_val ---
    y1_val <- approx(solution[,"time"], solution[,"y1"], xout = s_val)$y
    y4_val <- approx(solution[,"time"], solution[,"y4"], xout = s_val)$y
    
    # --- Calculate the forward temperature list ---
    r_for <- seq(0, LenghtFor, by = 0.025)
    TempListFor <- 300 + (N1 / lambda^2) *
      (((300 + (y4_val / y1_val)) - 300)) *
      exp( - (r_for^2 / (lambda^2 * LenghtFor^2)) )
    
    # --- Calculate the backward temperature list ---
    r_bac <- seq(LenghtBac, 0, by = -0.025)
    TempListBac <- 300 + (N1 / lambda^2) *
      (((300 + (y4_val / y1_val)) - 300)) *
      exp( - ((r_bac * 1.1)^2 / (lambda^2 * LenghtBac^2)) )
    
    # --- Determine X–positions ---
    # It is assumed that a global variable 'CenterX' exists (set by CenterPos(cbh))
    r_for_X <- seq(0, LenghtFor, by = 0.025)
    XListFor <- -1 * (CenterX + r_for_X)
    
    r_bac_X <- seq(LenghtBac, 0, by = -0.025)
    XListBac <- -1 * (CenterX + r_bac_X)
    
    # Combine the forward and backward lists
    Xlist <- c(XListBac, XListFor)
    TempList <- c(TempListBac, TempListFor)
    
    # For additional boundary data, define arrays on the left and right:
    test_val <- plumlimX[2] + LenghtBac
    aX <- -1 * seq(-15, test_val, by = 0.05)
    aT <- rep(300, length(aX))
    
    bX <- -1 * seq(plumlimX[3], 15, by = 0.05)
    bT <- rep(300, length(bX))
    
    # Append the values:
    temperatureXZ1 <- c(aT, TempList)
    tempatXZ <- c(temperatureXZ1, bT)
    Xlocat <- c(aX, Xlist, bX)
    
    # --- Smooth the profiles using a moving average ---
    MovAveXlocat <- rollmean(Xlocat, k = 3, fill = NA, align = "center")
    MovAveTempxz  <- rollmean(tempatXZ, k = 3, fill = NA, align = "center")
    
    # --- Construct the interpolation function ---
    XTlineatZ <- cbind(MovAveXlocat, MovAveTempxz)
    # Remove any rows with NA (at boundaries) before interpolating
    XTlineatZ <- XTlineatZ[complete.cases(XTlineatZ), , drop = FALSE]
    
    InterpolXTZ <- approxfun(XTlineatZ[, 1], XTlineatZ[, 2])
    
    # Evaluate the interpolation over the desired range
    TalongX <- InterpolXTZ(seq(-10, 5, by = 0.05))
    
    return(TalongX)
  }
  
  #Velocity of the gas as a function of X position:
  VxzGauss <- function(cbh,ODE_Results) {
    # Constants
    N1 <- 1.35
    lambda <- 1
    Ua <- 3
    
    # Update positions via external functions:
    fire_position(cbh,type="ForwardPlume",ODE_Results = ODE_Results)
    fire_position(cbh,type="CenterPlume",ODE_Results = ODE_Results)
    fire_position(cbh,type="RearPlume",ODE_Results = ODE_Results)
    
    plumlimX <- PlumeLimit_X(cbh,ODE_Results=ODE_Results)
    plumlimS <- PlumeLimit_S(cbh,ODE_Results=ODE_Results)
    
    # Compute lengths:
    LenghtFor <- plumlimX[3] - plumlimX[2]
    LenghtBac <- plumlimX[1] - plumlimX[2]
    
    s_val <- plumlimS[2]
    
    # Obtain y1 and y2 values from solution at s = s_val:
    y1_val <- approx(solution[,"time"], solution[,"y1"], xout = s_val)$y
    y2_val <- approx(solution[,"time"], solution[,"y2"], xout = s_val)$y
    
    # --- Calculate forward velocity list ---
    r_for <- seq(0, LenghtFor, by = 0.02)
    VelListFor <- (N1 / lambda^2) * (y2_val / y1_val) *
      exp( - (r_for^2 / (lambda^2 * LenghtFor^2)) )
    
    # --- Calculate backward velocity list ---
    r_bac <- seq(LenghtBac, 0, by = -0.02)
    VelListBac <- (N1 / lambda^2) * (y2_val / y1_val) *
      exp( - (r_bac^2 / (lambda^2 * LenghtBac^2)) )
    
    # --- Determine X–positions ---
    # Here, 'CenterX' is assumed defined globally.
    XListFor <- CenterX + r_for
    XListBac <- CenterX + r_bac
    
    Xlist <- c(XListBac, XListFor)
    VelList <- c(VelListBac, VelListFor)
    
    # Assemble the (X, velocity) data.
    XTatZ <- cbind(Xlist, VelList)
    
    # Set constant velocity boundaries:
    aX <- seq(-5, plumlimX[1], by = 0.02)
    aV <- rep(2, length(aX))
    
    bX <- seq(plumlimX[3], 20, by = 0.02)
    bV <- rep(2, length(bX))
    
    velocityXZ1 <- c(aV, VelList)
    velatXZ <- c(velocityXZ1, bV)
    Xlocat <- c(aX, Xlist, bX)
    
    # --- Smooth with a moving average (window size 10) ---
    MovAveXlocat <- rollmean(Xlocat, k = 10, fill = NA, align = "center")
    MovAveVelxz  <- rollmean(velatXZ, k = 10, fill = NA, align = "center")
    
    XVlineatZ <- cbind(MovAveXlocat, MovAveVelxz)
    XVlineatZ <- XVlineatZ[complete.cases(XVlineatZ), , drop = FALSE]
    
    # Construct the interpolation function.
    InterpolXVZ <- approxfun(XVlineatZ[, 1], XVlineatZ[, 2])
    
    # Evaluate over the specified domain
    vatz <- InterpolXVZ(seq(-3, 6, by = 0.05))
    
    return(vatz)
  }
  
  #Heat coefficient function for convective value:
  hcmendeslopes <- function(airtemp, airveloc, Tfuel) {
    Fd <- 0.0013
    #Fl <- 0.1
    #Tfuel is set when calculating function
    Tfilm <- (airtemp + Tfuel) / 2
    
    rho_air <- 358.98 * (Tfilm)^(-1.0046)
    Aircp <- -3e-10 * (Tfilm)^3 + 7e-7 * (Tfilm)^2 - 0.0003 * (Tfilm) + 1.0486
    Visco <- 1e-14 * (Tfilm)^3 - 4e-11 * (Tfilm)^2 + 7e-8 * (Tfilm) + 8e-7
    KinemVis <- Visco / rho_air
    ThermCond <- 2e-11 * (Tfilm)^3 - 6e-8 * (Tfilm)^2 + 0.0001 * (Tfilm) - 0.0015
    ThermDif <- ThermCond / (rho_air * Aircp * 1000)
    Pr <- 0.7
    Gr <- (9.8 * (1 / Tfilm) * (airtemp - Tfuel) * Fd^3) / (KinemVis^2)
    Ra <- Gr * Pr
    Rey <- (airveloc * Fd) / KinemVis
    Nu <- 0.1417 * (Rey^0.6053)
    
    result <- Nu * ThermCond / Fd
    return(result)
  }
  
  #Temperature of Gas above the flame as a function of X:
  TgasX <- function(U_a, Wa, MC, tau_r, R, X, T_a) {
    # Constants
    Tamb <- T_a
    b1 <- 300.68407718
    b2 <- 136.79152366
    b3 <- 0.506059107
    b4 <- 100.44776556
    b5 <- -0.530496458
    k  <- 1
    FlameBeta <- 9
    U<-U_a
    
    Tmax <- Tamb + b1 * Wa + b2 * U^b3 + b4 * MC^b5
    t <- X / R
    Adimen600 <- (600 - Tamb) / (Tmax - Tamb)
    # Calculate the “rise”
    Rise <- sqrt(-1 * (FlameBeta^2) * log(Adimen600))
    ig <- -Rise
    Decay <- -log(Adimen600) / (tau_r + ig)
    
    # Choose the exponential decay form based on the sign of (t + ig)
    coef <- ifelse(t + ig < 0,
                   k * exp(-((t + ig)^2 / (FlameBeta^2))),
                   k * exp(-(Decay * (t + ig))))
    
    res <- coef * (Tmax - Tamb) + Tamb
    return(res)
  }

  #Radiation calculation as a function of ROS, depth, etc
  Radiation <- function(cbh, Dx, U_a, Wa, MC, tau_r, R, T_a, DBH, BA, CalcRadAdsorb) {
    # Local constants and settings
    Lp <- 0.001
    Wp <- 0.001
    DF <- tau_r * R
    Wf <- 20   #can change this depending on how wide you want your flame width
    Aflame <- Wf * DF  
    SB <- 5.670e-8      # Stefan-Boltzmann constant (W m^-2 K^-4)
    epsilon_f <- 1      # Emissivity factor
    Tp <- 300           # Plume temperature (K)
    if(CalcRadAdsorb==TRUE){
      sav_trunk<- 4/DBH
      B_s<- BA*cbh #Basal area per m by cbh
      RadAbsorpCoefficient <- sav_trunk*B_s/4
    }else{
      RadAbsorpCoefficient <- 0.02
    }
    
    # Construct an interpolation for FlameTemperature along x_f:
    xvals <- seq(0, DF, by = 0.05)
    Tvals <- sapply(xvals, function(x) TgasX(U_a, Wa, MC, tau_r, R, x, T_a))
    FlameTemperature <- approxfun(xvals, Tvals)
    
    # Define the integrand function.
    # Here v is a vector containing variables in order:
    f_integrand <- function(v) {
      x_f <- v[1]
      x_p <- v[2]
      y_p <- v[3]
      y_f <- v[4]
      
      # Distance measure (S) incorporating x and y differences and the vertical dimension (cbh)
      S <- sqrt((y_f - y_p)^2 + cbh^2 + (x_p - x_f)^2)
      
      # Integrand components:
      term1 <- SB * epsilon_f * (FlameTemperature(x_f)^4 - Tp^4)
      # Two factors coming from geometric spreading:
      geom_factor <- (cbh / S)^2
      # Exponential attenuation by absorption:
      exp_factor <- exp(-RadAbsorpCoefficient *S)
      
      # Division denominator (π * S^2 squared gives π*S^4)
      denom <- pi * S^4
      
      return(term1 * geom_factor * exp_factor / denom)
    }
    
    # Integration limits:
    # x_f: from 0 to DF,
    # x_p: from (Dx - Lp/2) to (Dx + Lp/2),
    # y_p: from -Wp/2 to Wp/2,
    # y_f: from -Wf/2 to Wf/2.
    lower <- c(0, Dx - Lp / 2, -Wp / 2, -Wf / 2)
    upper <- c(DF, Dx + Lp / 2, Wp / 2, Wf / 2)
    
    # Use adaptive integration in 4D. 
    int_result <- adaptIntegrate(f_integrand, lowerLimit = lower, upperLimit = upper, tol = 1e-2)
    
    # The Mathematica expression multiplies the result by 1e6.
    return(1e6 * int_result$integral)
  }

  #Vertical velocity of the plume funciton based on intensity
  UFvert <- function(I_B,g,p_a,C_p,T_a) {
    result <-((2*g*I_B)/(p_a*C_p*(T_a)))^(1/3)
    return(result)
  }
  
  #Function to calculate temperature of canopy fuel over the moving surface fire
    #combines all functions
  FireMovingQrQc <- function(Ush, R, Wa, MC, cbh, cbd, FMC, FuelBedDepth, T_a, I_B, tau_r, p_f, BA, DBH) {
    
    # ----- Define constants and initial conditions -----
    Tamb <- T_a
    Tfuel <- Tamb           # initial fuel temperature equals ambient
    G <- 0                  # not otherwise used below
    SB <- 5.670e-8          # Stefan–Boltzmann constant [W m^-2 K^-4]
    epsilon_f <- 1          # flame emissivity (ε_f)
    CBD <- cbd
    Fcp <- 2100
    rho_f <- p_f
    Fd <- 0.00074
    Fl <- 0.1
    Wcp <- 4187
    Wlhv <- 2.254e6
    WindAlpha <- 1.2
    RadAbsorpCoefficient <- 0.02
    tstep <- 1
    CFIMlog<-character(0)
    
    # ----- Compute wind–adjusted velocity, flame geometry, and reaction time -----
    Ua <- Ush * exp(WindAlpha * (0.6 - 1))
    IB <- I_B
    FLenght <- 0.0775 * IB^0.46
    PreFH <- 0.002597*IB/Ua
    FH <- ifelse(PreFH > FLenght, FLenght, PreFH)
    #tau_r <- ReacTime(wa, MC, R, Ua, FuelBedDepth)  
    
    FSG <- cbh - FuelBedDepth
    TargetFSG <- ifelse(FSG < (FH + 0.25), 0.25, FSG - FH)
    
    #Calculate interpolating functions
    ODE_res<-ODE_func(Ush = Ush, wi=Upi, bi=P_w, SH=SH, T_a = T_a)
    
    #merge resutls
    ODEFuncs <- list(
      ODE_raw = ODE_res,  # the complete data frame from deSolve
      y1 = splinefun(ODE_res[,"time"], ODE_res[,"y1"]),
      y2 = splinefun(ODE_res[,"time"], ODE_res[,"y2"]),
      y3 = splinefun(ODE_res[,"time"], ODE_res[,"y3"]),
      y4 = splinefun(ODE_res[,"time"], ODE_res[,"y4"]),
      y5 = splinefun(ODE_res[,"time"], ODE_res[,"y5"]),
      y6 = splinefun(ODE_res[,"time"], ODE_res[,"y6"])
    )
    
    # Call a function to set up plume Gauss profiles (side effect)
    PlumeGas_Behave<-PlumeGaussTandV(TargetFSG,ODE_Results = ODEFuncs, U_a = Ua, T_a=Tamb)    
    
    # ----- Create an interpolation for flame temperature vs. x -----
    t_vals <- seq(-20, 100, by = 1)
    FlameTemp <- sapply(t_vals, function(t) Tgas(t=t,U_a=Ua,Wa= Wa,MC= MC, taur=tau_r, T_a=T_a))

    # XTemp is just the t values
    XTemp <- t_vals
    InterpFlameXT <- approxfun(XTemp, FlameTemp)
    
    # Also compute a flame temperature time series
    TTdataplot <- cbind(seq(-10, tau_r + 10, by = 1),
                        sapply(seq(-10, tau_r + 10, by = 1),
                               function(t) Tgas(t=t,U_a=Ua,Wa= Wa,MC= MC, taur=tau_r, T_a=T_a)))
    
    # ----- Main loop: update the fuel temperature as the fire “moves” -----
    n_iter <- floor(14 / R)
    
    #Setup spatial location start at -10 meters before crown
    x0 <- -10
    x <- x0
    
    # Prepare variables to store the ignition details
    ignition_time <- NA
    ignition_x <- NA
    ignition_s <- NA
    
    Tfuel <- Tamb 
    
    for (i in 1:tau_r) {
      xnew <- x + R * i
      x <- xnew
      
      DFL <- R * tau_r
      ND <- tau_r
      WD <- DFL / ND  
      
      #ERRROR HERE IN AIRVOLEC----------------------------
      # These two interpolation functions come from plume behavior object.
      airtemp <- PlumeGas_Behave$InterpolXTZ(x)   # ambient temperature versus x
      airveloc <- abs(PlumeGas_Behave$InterpolXVZ(x))
      #ERORR-----------------------------------------------
      
      hconvec <- hcmendeslopes(airtemp, airveloc,Tfuel)  
      
      #First integral Eq: 30
      Qconvec <- hconvec * Fa_calc(Fd, Fl) * (airtemp - Tfuel)  
      
      #Second integral Eq:30 radiation
      RHF <- Radiation(cbh=cbh, Dx=x, U_a=Ua, Wa=Wa, MC=MC, tau_r=tau_r, R=R, T_a=T_a, DBH, BA, CalcRadAdsorb)
      Q_rad<- RHF * (Fa_calc(Fd, Fl) / 2)
      
      #Third Integral
      Qradcool <- epsilon_f * SB * Tfuel^4
      Qrad <- Q_rad - Qradcool * (Fa_calc(Fd, Fl) / 2) +
        (epsilon_f * SB * 300^4) * (Fa_calc(Fd, Fl))
      
      #Calculate Heat sink of semi moist fuel
      AveragCp <- ((Wcp * FMC) * (373 - Tamb) + Fcp * (373 - Tamb) +
                     Wlhv * FMC + Fcp * (600 - 373)) / (600 - Tamb)
      Sink <- rho_f * AveragCp * Fv_calc(Fd, Fl)  
      
      
      # Compute qFlame: mean radiative flux over indices 1 to ND.
      #ND_int <- round(ND)
      #qFlame <- mean(sapply(1:ND_int, function(idx) {
      #  SB * epsilon_f * (InterpFlameXT(idx)^4 - Tfuel^4)
      #}), na.rm = TRUE)
      
      Qtotal <- (Qconvec + Qrad) * tstep
      NewTf <- Tfuel + Qtotal / Sink
      DifTFuel <- NewTf - Tfuel
      Tfuel <- NewTf
      
      #Check: if the back of the flaming front has passed the fuel particle
      flame_rear <- x + fire_position(cbh, type = "RearPlume", ODE_Results = ODEFuncs)
      
      log_entry1 <- sprintf("Iteration %d Final: x (m) = %.5f, Distance Traveled m = %.5f,Rear Plume x = %.5f, Tfuel = %.5f, ΔTfuel = %.5f",
                            i, x,(x-(x-R*i)),flame_rear, Tfuel, DifTFuel)
      #check error and end
      #if(is.na(Tfuel)){
       # Out<-"CFIM Error"
        #return(Out)
      #}
        
      CFIMlog <- c(CFIMlog, log_entry1)
      cat(log_entry1, "\n")
      
      # --------------------- Check Termination Conditions ---------------------
      # Condition 1: Ignition condition
      if (Tfuel > 600 || DifTFuel < -4){
        ignition_time <- i
        ignition_x <- x
        ignition_s <- fire_position(cbh, type = "CenterS", ODE_Results = ODEFuncs)
        
        cat("Fire ignites at x =", ignition_x, 
            ", s =", ignition_s, 
            ", time =", ignition_time, "seconds\n")
        CFIMlog <- c(CFIMlog, sprintf("Ignition: x = %.5f, s = %.5f, time = %.5f seconds",
                                      ignition_x, ignition_s, ignition_time))
        
        out<-list(ignition_x = ignition_x,
                  ignition_s = ignition_s,
                  ignition_time = ignition_time,
                  final_fuel_temp = Tfuel,
                  Type = "CrowningPotential",
                  CFIMlog = CFIMlog)
        # Reset variables before returning:
        x0 <- -10
        x <- x0
        tstep <- 1
        ignition_time <- NA
        ignition_x <- NA
        ignition_s <- NA
        #Tfuel<-Tamb
        xnew <- x + R * i
        
        return(out)
      }
      
      # Condition 2: Check if the fuel particle is passed by the flaming front
      if (flame_rear  > 0) {
        cat("The flame front has passed the fuel particle without ignition after", i, "iterations\n")
        CFIMlog <- c(CFIMlog, sprintf("Flame passed: x = %.5f, rear flame x = %.5f, time = %.5f seconds",
                                      x, flame_rear, i))
        
        out<-list(fuel_temp_reached = Tfuel,
                  Type = "Surface",
                  CFIMlog = CFIMlog)
        # Reset variables before returning:
        x0 <- -10
        x <- x0
        tstep <- 1
        ignition_time <- NA
        ignition_x <- NA
        ignition_s <- NA
        #Tfuel<-Tamb
        xnew <- x + R * i
        
        return(out)
      }
      # --------------------------------------------------------------------------
    } 
    
    #if(Out == "CFIM Error"){
     #   return(Out)
    #  }
    
    
    cat("Loop completed without meeting a termination condition.\n")
    return(list(final_fuel_temp = Tfuel,
                final_x = x,
                CFIMlog = CFIMlog))
  }
  
  #Error Check
  #if(Out == "CFIM Error"){
   # return(Out)
  #}
  

#Running CFIM: Check where and if fire crowns
  Results<-FireMovingQrQc(Ush = Ush,
                       R=ROS,
                       Wa=W_a,
                       MC=MC,
                       cbh=CBH,
                       cbd=CBD,
                       FMC=FMC,
                       FuelBedDepth = FBDepth_m,
                       T_a=T_a,
                       I_B=I_B,
                       tau_r = rt_F,
                       p_f=p_f,
                       BA=BA,
                       DBH=DBH)
  
#Fire Behavior Results:
  if(Results$Type == "Surface"){
    Results_Object<- list(
      FireType="Surface",
      ROS.m.s=ROS,
      ROS.m.m=ros_ROTH_m,
      Residence.Time=rt_F,
      Intensity=I_B,
      Flame.Length=FL,
      Flame.Height=FH,
      Fuel.Consumption.s=Nelson_Results$Surface.Area.Burning.Rate.kg..m2.s,
      #Crowning.Probability= (Results$fuel.temp.reached-T_a)/(600-T_a),#consider some way to calculate crowning probability
      ResidenceLog=NelsonLog,
      FireSpreadLog=Results$CFIMlog
    )
    
    }else{
      
      #Bring in Crown Fuel Consumption model from Groot et al 2022:
      CFC<- CFC_Calc(ROS,CFL=CFL,CFB=TRUE)
      
      #Bring in criteria for active crowning
      crosa <- (11.02 * U^0.9) * (CBD^0.19) * exp(-0.17 * (effm$fm1hr))
      crosp <- crosa * exp(-0.3333 * crosa * CBD)
      cac <- crosa * CBD / 3
      
      #Check if active or passive crown fire
      Type<- ifelse(cac < 1, "Passive", "Active")
      R_crown.m<-ifelse(cac < 1, crosp, crosa)
      
      #Get new intensity values
      I_B_crown<-I_B + (((structure$cfl * 0.20482 * H_mean * 0.430265)*CFC$CFB)*11.349*R_crown.m/60)
      I_B_crown<-I_B + (((structure$cfl * H_mean)*CFC$CFB)*11.349*R_crown.m/60)
      
      Results_Object<- list(
        FireType=Type,
        ROS.m.s=R_crown.m/60,
        ROS.m.m=R_crown.m,
        Residence.Time=rt_F,
        Intensity=I_B_crown,
        Flame.Length=0.0775*I_B_crown^0.46,
        Flame.Height=0.002597*I_B_crown/U_a,
        Fuel.Consumption.s=Nelson_Results$Surface.Area.Burning.Rate.kg..m2.s + CFC$CFB*structure$cfl,
        #Crowning.Probability= (Results$fuel.temp.reached-T_a)/(600-T_a), #consider some way to calculate crowning probability
        ResidenceLog=NelsonLog,
        FireSpreadLog=Results$CFIMlog
        )
    }
  
    #clean space
    rm(Tfuel,x,rt_F,tstep,i,CFIMlog,NelsonLog)
    gc()
    #Export results--------------------------------------------------------------
    return(Results_Object)
  }
