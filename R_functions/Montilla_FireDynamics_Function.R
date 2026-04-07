#Code to calculate Montilla's et al 2025's Fire dynamics model;

#From Montialla et al. 2025: Modeling wildfire dynamics through a physics-based approach 
#incorporating fuel moisture and landscape heterogeneity


#Uses equations from several submodels:

#Load packages
library(raster)
library(sp)
library(sf)
library(dplyr)
library(matrixStats)
library(truncnorm)
library(cffdrs)
library(gganimate)
library(ggplot2)
library(terra)
library(gifski)

#library(gifski)
#install.packages("gifski")

#source functions
source("Z:/Scripts/FronteraCodez/Functions/FFMC_sa_Raster_Function.R")
source("Z:/Scripts/FronteraCodez/Functions/FMC_Raster_Function.R")
source("Z:/Scripts/FronteraCodez/Functions/fueltypes_crosswalkFBP_Raster_function.R")
source("Z:/Scripts/FronteraCodez/Functions/sfc_raster_CANFBP_function.R")

#Test Inputs:
path<-"Z:/Projects/BC_BP3/Modelling/Unit Directories/Unit14/FireDynamics/"

#load files
ft<-rast(paste0(path,"FT.tif"))
ft_fbp<-fueltypes_crosswalkfbp_raster(ft)
dem<-rast(paste0(path,"DEM.tif"))
windV<-rast(paste0(path,"WindVelocity.tif"))
windDir<-rast(paste0(path,"WindDirection.tif"))
ign<-rast(paste0(path,"IgnitionProb.tif"))
sc<-rast(paste0(path,"CC.tif")) #use crownclosure as proxy for now
forestype<-rast(paste0(path,"Forest_Type.tif"))

ffmc<-dem; dmc<-dem
values(ffmc)<-90
values(dmc)<-120

#create a fake Live to dead fuel ratio
random_values <- rtruncnorm(ncell(dem), 
                            a = 0.1,      # minimum
                            b = 1.0,      # maximum  
                            mean = 0.9,   # desired mean
                            sd = 0.15)
ldfr<-dem;values(ldfr)<-random_values

#create fuel loads by sfc function
bui<-dem;values(bui)<-120
pc<-dem;values(pc)<-0
pc <- ifel(ft_fbp %in% c(9,10,11,12), 50, pc)
gc<-dem;values(gc)<-0
gc <- ifel(ft_fbp %in% c(14,15), 2, gc)
plot(gc)

sfc<-sfc_raster(ft_fbp,
                FFMC=ffmc,
                BUI=bui,
                PC=pc,
                GFL=gc)

#get fmc and mc
coords_original <- crds(dem)
coords_geo <- terra::project(coords_original, from = crs(dem), to = "EPSG:4326")
Lat <- dem
Long <- dem
values(Lat) <- coords_geo[,2]   # Latitude
values(Long) <- coords_geo[,1]  # Longitude

fmc<-FMC_Rast_calc(LAT = Lat,
                   LON=Long,
                   DATE = "2024-07-15",
                   ELEV = dem)
fmc<-fmc/10

mc<-FFMC_sa_raster(forest_rast = forestype,
                   season="Summer",
                   ffmc_rast = ffmc,
                   dmc_rast = dmc,
                   crown_closure_rast = sc)

#create inputs
Landscape_c<- c(sfc/2,sc,dem)
names(Landscape_c)<- c("Fuel","SC","DEM")

Enviroment_c<-c(windV/3.6,windDir,ldfr,mc/100,fmc/100)
names(Enviroment_c)<- c("WindVel", "WindDir", "LDFuelRatio", "MC", "FMC")

Weather<-data.frame(Temp=300,rh=35)

#usable inputs
Spatial=TRUE
Landscape<-Landscape_c
Environment<-Enviroment_c
Weather
Ignitions=NULL
IgnitionProbability=ign
NumberIgnitions=1
Plot="YES"
cellsize = 10       # desired fine resolution in meters (e.g., 10)
downscale_sd_frac = 0.05
snapshot_every = 120   # seconds (1 minute)
t_end = 1200          #total sim time [s]
snapshot_dir = paste0(path,"/Snapshots/")   # folder to write GeoTIFF snapshots
perimeter_dir = paste0(path,"/Perimeters/")   # folder to write GPKG perimeters


#Inputs
#Spatial #if spatial is false model will run in 1-dimension
#Landscape: #A raster or terra stack of fuel loads (kg./m^2) or Fueltypes (US Rothermel), surface coverage (0-1); DEM, (if non-spatial a dataframe of values)
#{with column names or stack names Fuel, SC, DEM}

#Environment: #A raster or terra stack of Wind Velocity Grid (m/s), Wind Direction Grid, 
#Live to DeadFuel Ratio (0-1), FuelMoistContent (0-1), LiveFuelMoistContent (fmc, as integer ie 1.2=120%); 
#(if non-spatial a dataframe of values) {with column names or stack names WindVel, WindDir, LDFuelRatio, MC, FMC}

#Weather: #a data frame of Temperature, RH {with column names temp, rh}
#Plot: Logical flag indicating whether to plot moving flaming front
#Ignitions # a spatvector of ignition locations, only used if spatial is true, if not sample will happen
#IgnitionProbability #an optional spatraster or raster of relative ignition probability
#NumberIgnitions #integer, number of ignitions to sample from probability surface, if ignition location not given
#Plot="YES" #plot accelaeration of flame front

# --- knobs ---
#cellsize       # desired fine resolution in meters (e.g., 10)
#downscale_sd_frac #downscale standard deviation for changing cell size
#snapshot_every  # seconds (1 minute)
#t_end           # total sim time [s]
#snapshot_dir   # folder to write GeoTIFF snapshots
#perimeter_dir    # folder to write GPKG perimeters


Montilla_FireDynamics<- function(
    Spatial=TRUE,
    Landscape,
    Environment,
    Weather,
    Ignitions=NULL,
    IgnitionProbability=NULL,
    NumberIgnitions=NULL,
    Plot="YES",
    # --- knobs ---
    cellsize = NULL,
    downscale_sd_frac = 0.05,
    snapshot_every,
    t_end,
    snapshot_dir = NULL,
    perimeter_dir = NULL){
  
  # ---- clean names
  names(Landscape)   <- toupper(names(Landscape))
  names(Environment) <- toupper(names(Environment))
  names(Weather)     <- toupper(names(Weather))
  
  #Preset Constants:
  k_c<-0.0001
  optical_path<-1
  amb_cool_coe<-0.05
  beta<-0.6
  wind_cor_coe<-0.02
  gamma<-0.05
  l<-1
  T_w<-373
  L_w<-2257
  c_w<-4.19
  p_a<-1
  SB<-5.670374419e-11
  emis<-0.9
  
  #Variable Constants
  R_f0<-0.01
  rho_f<- 400
  cp_f0<-1.0
  T_pc<-600
  H<-4000
  A_c<-0.01
  A_a<-0.0173
  CFL <- 0.25  # used in dt_cfl()
  
  # ---------- helpers ----------
  downscale_normal <- function(r, template, sd_frac = 0.05, clamp_rng = NULL, is_direction = FALSE){
    # Use nearest neighbor for circular/directional data to avoid averaging across 0/360
    base <- if (is_direction) {
      terra::resample(r, template, method = "near")
    } else {
      terra::resample(r, template, method = "bilinear")
    }
    
    vals <- values(base)
    set.seed(42)  # note: this makes every call reproducible; move outside if you want different noise per layer
    
    if (is_direction){
      # add wrapped normal noise in DEGREES, then wrap to [0, 360)
      sd_deg <- max(3, 180*sd_frac)
      draw <- vals
      ok <- !is.na(vals)
      draw[ok] <- (vals[ok] + rnorm(sum(ok), mean = 0, sd = sd_deg)) %% 360
    } else {
      sd_vals <- pmax(abs(vals)*sd_frac, 1e-8)
      draw <- rnorm(length(vals), mean = vals, sd = sd_vals)
      if (!is.null(clamp_rng)) draw <- pmin(pmax(draw, clamp_rng[1]), clamp_rng[2])
    }
    
    out <- template
    values(out) <- draw
    out
  }
  
  
  build_fine_template <- function(base_rast, ig_points, cellsize, t_end, base_res){
    # radius (m) you wanted: t_end * 1.66 * cellsize
    Rmax_m <- t_end * 1.66 * cellsize
    
    # buffer around all ignition points
    buf <- terra::buffer(ig_points, width = Rmax_m)
    
    # dissolve/union polygons regardless of terra version
    if ("dissolve" %in% getNamespaceExports("terra")) {
      buf <- terra::dissolve(buf)
    } else {
      # fallback: aggregate by a dummy constant field
      buf$.__grp__ <- 1L
      buf <- terra::aggregate(buf, by=".__grp__")
    }
    
    # small extra padding to be safe
    buf <- terra::buffer(buf, width = max(base_res, cellsize))
    
    # fine template at requested cellsize
    template <- terra::rast(terra::ext(buf), resolution = cellsize, crs = terra::crs(base_rast))
    
    list(buffer = buf, template = template, Rmax_m = Rmax_m)
  }
  # ----------------------------------------------------------------------
  
  #Decide if Spatial or not:-----------------------------------
  if(Spatial == TRUE){
    if (!inherits(Landscape, "SpatRaster")) Landscape <- rast(Landscape)
    if (!inherits(Environment, "SpatRaster")) Environment <- rast(Environment)
    if (!is.null(IgnitionProbability) && !inherits(IgnitionProbability, "SpatRaster")) {
      IgnitionProbability <- rast(IgnitionProbability)
    }
    
    # check extent/crs
    Environment <- terra::project(Environment, crs(Landscape), method="bilinear")
    Environment <- terra::resample(Environment, Landscape, method="bilinear")
    if (!is.null(IgnitionProbability)) {
      IgnitionProbability <- terra::project(IgnitionProbability, crs(Landscape), method="bilinear")
      IgnitionProbability <- terra::resample(IgnitionProbability, Landscape, method="bilinear")
    }
    
    
    
    #load correct nomenclature from inputs
    SC<-Landscape$SC
    Z<-Landscape$DEM
    WindVel <- Environment$WINDVEL
    WindDir <- Environment$WINDDIR
    MCd     <- Environment$MC
    MCg     <- Environment$FMC
    r_live  <- Environment$LDFUELRATIO
    
    base_res <- res(Z)[1]
    
    if ("FUEL" %in% names(Landscape)) {
      W_f0 <- Landscape$FUEL
    } else {
      stop("Provide Landscape$FUEL as initial fuel load (kg/m^2).")
    }
    
    # ---- ignition mask -> initial T,Y
    T_0 <- Z; values(T_0) <- Weather$TEMP
    Y_0 <- W_f0; values(Y_0) <- 1
    
    #Sample ignitions or load ignition locations from spatvector
    if (!is.null(Ignitions) && nrow(Ignitions) > 0) {
      if (!inherits(Ignitions, "SpatVector")) Ignitions <- vect(Ignitions)
      ig <- Ignitions; ig <- terra::project(ig, crs(Z))
    } else if (!is.null(IgnitionProbability) && !is.null(NumberIgnitions)) {
      set.seed(1)
      pr <- IgnitionProbability
      pr_vals <- values(pr); pr_vals[is.na(pr_vals)] <- 0
      pr_vals <- pr_vals / sum(pr_vals)
      sel <- sample.int(ncell(pr), size=NumberIgnitions, prob=pr_vals)
      ig <- as.points(pr, na.rm=TRUE)[sel,]
      ig <- terra::project(ig, crs(Z))
    } else {
      stop("Provide Ignitions or (IgnitionProbability + NumberIgnitions).")
    }
    
    # -------- build cropped fine template & downscale to 'cellsize' --------
    
    if(is.null(cellsize)){ cellsize <- base_res}
    tpl <- build_fine_template(Z, ig, cellsize, t_end, base_res)
    spread_buffer <- tpl$buffer
    fine_template <- tpl$template
    
    # downscale/crop all needed layers with normal noise around parent mean
    Z    <- terra::resample(Z, fine_template, method="bilinear") |> mask(spread_buffer)
    SC   <- downscale_normal(SC,    fine_template, sd_frac = downscale_sd_frac, clamp_rng = c(0,1)) |> mask(spread_buffer)
    W_f0 <- downscale_normal(W_f0,  fine_template, sd_frac = downscale_sd_frac, clamp_rng = c(0, Inf)) |> mask(spread_buffer)
    WindVel <- downscale_normal(WindVel, fine_template, sd_frac = downscale_sd_frac, clamp_rng = c(0, Inf)) |> mask(spread_buffer)
    WindDir <- downscale_normal(WindDir, fine_template, sd_frac = downscale_sd_frac, is_direction = TRUE) |> mask(spread_buffer)
    r_live  <- downscale_normal(r_live,  fine_template, sd_frac = downscale_sd_frac, clamp_rng = c(0,1)) |> mask(spread_buffer)
    MCd     <- downscale_normal(MCd,     fine_template, sd_frac = downscale_sd_frac, clamp_rng = c(0,2)) |> mask(spread_buffer)
    MCg     <- downscale_normal(MCg,     fine_template, sd_frac = downscale_sd_frac, clamp_rng = c(0,3)) |> mask(spread_buffer)
    
    #create Temperature and RH raster
    template<-Z
    Tinf<-template;values(Tinf)<-Weather$TEMP
    RH<-template;values(RH)<-Weather$RH
    
    # re-init T_0/Y_0 on the fine template & set ignitions
    T_0 <- Z; values(T_0) <- Weather$TEMP
    Y_0 <- Z; values(Y_0) <- 1
    ig  <- terra::project(ig, crs(Z))
    ign_r <- rasterize(ig, Z, field=1, background=0)
    ign_r <- focal(ign_r, w=3, fun="max", na.policy="omit")
    T_0[ign_r==1] <- T_pc + 70
    # ---------------------------------------------------------------------------
    
    #Fuel effective heat including moisture content of live and dead fuel
    Mmix <- if (!is.null(r_live)) { r_live*MCg + (1-r_live)*MCd } else { MCd }
    cp_eff <- cp_f0 + Mmix * ( c_w*(T_w - Tinf) + L_w ) / (T_pc - Tinf)
    cp_f   <- app(cp_eff, fun=function(x) x)
    
    # ---- k(T) with Rosseland
    k_of_T <- function(Tr) { k_c + 4*SB*emis*l*(Tr^3) }
    
    # ---- reaction rate Psi(T)
    psi_of_T <- function(Tr) { ifel(Tr >= T_pc, A_c, 0) }
    
    #Wind u and v component (with slope)
    ang <- (WindDir + 180) %% 360
    ang <- (90 - ang) * pi/180 #angle in radians
    u_w <- WindVel * cos(ang)
    v_w <- WindVel * sin(ang)
    
    asp   <- terrain(Z, v="aspect", unit="radians")
    slp   <- terrain(Z, v="slope",  unit="radians")
    tanS  <- tan(slp)
    dZdx  <- tanS * sin(asp)
    dZdy  <- tanS * cos(asp)
    
    u <- beta*u_w + gamma*dZdx
    v <- beta*v_w + gamma*dZdy
    
    # cell size (use your dxdy name as alias of cellsize)
    dxdy <- cellsize
    if (is.null(dxdy)) {
      resxy <- res(Z); dx <- resxy[1]; dy <- resxy[2]
    } else { dx <- dy <- dxdy }
    
    shift_samegrid <- function(r, dx=0, dy=0, ref){
      s <- terra::shift(r, dx = dx, dy = dy)         # move in MAP units (meters)
      terra::resample(s, ref, method = "bilinear")   # snap back to ref grid
    }
    
    # one SSP-RK stage operator
    rhs <- function(Tn, Yn) {
      # heat capacity switch
      cp_here <- terra::ifel((Tn < T_pc) & (Yn > 0.999), cp_eff, cp_f0)
      rho_cp  <- rho_f * cp_here
      
      # neighbors: shift one cell (dx/dy meters) and snap to Tn grid
      T_e <- shift_samegrid(Tn, dx=+dx, ref=Tn)
      T_w <- shift_samegrid(Tn, dx=-dx, ref=Tn)
      T_n <- shift_samegrid(Tn, dy=+dy, ref=Tn)
      T_s <- shift_samegrid(Tn, dy=-dy, ref=Tn)
      
      # Neumann-like boundary: replace edge NAs with the center cell (mirror)
      T_e <- terra::cover(T_e, Tn); T_w <- terra::cover(T_w, Tn)
      T_n <- terra::cover(T_n, Tn); T_s <- terra::cover(T_s, Tn)
      
      # upwind advection
      dTdx_up <- terra::ifel(u >= 0, (Tn - T_w)/dx, (T_e - Tn)/dx)
      dTdy_up <- terra::ifel(v >= 0, (Tn - T_s)/dy, (T_n - Tn)/dy)
      adv <- u*dTdx_up + v*dTdy_up
      
      # nonlinear diffusion with interface-averaged k
      k   <- k_of_T(Tn)
      
      k_e <- 0.5*(k + terra::cover(shift_samegrid(k, dx=+dx, ref=Tn), k))
      k_w <- 0.5*(k + terra::cover(shift_samegrid(k, dx=-dx, ref=Tn), k))
      k_n <- 0.5*(k + terra::cover(shift_samegrid(k, dy=+dy, ref=Tn), k))
      k_s <- 0.5*(k + terra::cover(shift_samegrid(k, dy=-dy, ref=Tn), k))
      
      flux_e <- -k_e*(T_e - Tn)/dx
      flux_w <- -k_w*(Tn - T_w)/dx
      flux_n <- -k_n*(T_n - Tn)/dy
      flux_s <- -k_s*(Tn - T_s)/dy
      diff <- (flux_e - flux_w)/dx + (flux_n - flux_s)/dy
      
      # sources/sinks
      psi <- psi_of_T(Tn)
      S_T <- -amb_cool_coe*(Tn - Tinf) + psi * rho_f * H * Yn * SC
      S_Y <- -psi * Yn
      
      # avoid divide-by-zero
      denom <- rho_cp + 1e-9
      dTdt <- (diff - adv + S_T) / denom
      dYdt <- S_Y
      
      list(dTdt=dTdt, dYdt=dYdt, k=k)
    }
    
    
    # dynamic dt from CFL
    dt_cfl <- function(u, v, k, dx, dy) {
      umax <- tryCatch(terra::global(abs(u), "max", na.rm=TRUE)[,1], error=function(e) 0)
      vmax <- tryCatch(terra::global(abs(v), "max", na.rm=TRUE)[,1], error=function(e) 0)
      if (!is.finite(umax)) umax <- 0
      if (!is.finite(vmax)) vmax <- 0
      
      a_dt <- if (umax>0 || vmax>0) CFL / ((umax/dx) + (vmax/dy) + 1e-12) else Inf
      
      kmax <- tryCatch(terra::global(k, "max", na.rm=TRUE)[,1], error=function(e) 0)
      if (!is.finite(kmax) || kmax <= 0) kmax <- 1e-12
      rcp  <- rho_f * cp_f0
      d_dt <- CFL * min(dx,dy)^2 * rcp / (4*kmax)
      
      out <- min(a_dt, d_dt)
      # last-ditch fallback to prevent NaN
      if (!is.finite(out) || out <= 0) {
        out <- 0.25 * min(dx,dy) / max(umax, vmax, 1e-6)
      }
      out
    }
    
    
    # ---- integrate
    Tn <- T_0; Yn <- Y_0
    t  <- 0; t_next_plot <- 0
    k0 <- k_of_T(Tn)
    dt <- dt_cfl(u, v, k0, dx, dy)
    SC    <- SC[is.na(SC)]<-0        # 0 coverage where NA
    cp_eff<- SC[is.na(SC)]<-cp_f0   # if moisture inputs had NA
    
    if (toupper(Plot) == "YES") {
      plot(Tn, main=sprintf("T @ t=%.0fs", t))
    }
    
    # ---------- results recording ----------
    Arr <- Z; values(Arr) <- NA_real_
    Front <- Z; values(Front) <- NA_real_
    ROS <- Z; values(ROS) <- NA_real_
    
    # alias your names used below
    save_every <- snapshot_every
    outputfolder <- if (!is.null(snapshot_dir)) snapshot_dir else perimeter_dir
    
    next_snap_time <- 0
    snapshots <- list()
    perimeters <- list()
    
    if (!is.null(snapshot_dir))  dir.create(snapshot_dir,  showWarnings = FALSE, recursive = TRUE)
    if (!is.null(perimeter_dir)) dir.create(perimeter_dir, showWarnings = FALSE, recursive = TRUE)
    
    write_snapshot <- function(Tn, Yn, ROS, Front, perim, tsec) {
      snap_r <- c(Tn, Yn, ROS, Front); names(snap_r) <- c("T","Y","ROS","Front")
      
      # burned polygon (Arr <= tsec)
      burned_mask <- terra::ifel(!is.na(Arr) & (Arr <= tsec), 1, NA)
      burned_poly <- tryCatch({
        bp <- as.polygons(burned_mask, dissolve = TRUE, values = FALSE)
        if (!is.null(bp) && nrow(bp) > 0) { bp$time_s <- tsec; bp } else NULL
      }, error = function(e) NULL)
      
      # front points from Front (now the minute window ring)
      front_pts <- tryCatch({
        fp <- as.points(Front, values = FALSE)
        if (!is.null(fp) && nrow(fp) > 0) {
          fp$time_s <- tsec
          ros_vals <- terra::extract(ROS, fp)[,2]
          fp$ROS <- ros_vals
          fp
        } else NULL
      }, error = function(e) NULL)
      
      if (!is.null(perim)) perim$time_s <- tsec
      
      if (!is.null(snapshot_dir)) {
        tf <- file.path(snapshot_dir, sprintf("fire_snapshot_t%05ds.tif", round(tsec)))
        terra::writeRaster(snap_r, tf, overwrite = TRUE)
      }
      if (!is.null(perimeter_dir)) {
        if (!is.null(perim) && nrow(perim) > 0) {
          vf <- file.path(perimeter_dir, sprintf("fire_perimeter_t%05ds.gpkg", round(tsec)))
          terra::writeVector(perim, vf, layer = "perimeter", overwrite = TRUE)
        }
        if (!is.null(burned_poly) && nrow(burned_poly) > 0) {
          bf <- file.path(perimeter_dir, sprintf("fire_burned_t%05ds.gpkg", round(tsec)))
          terra::writeVector(burned_poly, bf, layer = "burned", overwrite = TRUE)
        }
        if (!is.null(front_pts) && nrow(front_pts) > 0) {
          pf <- file.path(perimeter_dir, sprintf("fire_frontpts_t%05ds.gpkg", round(tsec)))
          terra::writeVector(front_pts, pf, layer = "front_pts", overwrite = TRUE)
        }
      }
      
      snapshots[[length(snapshots)+1]]  <<- list(time = tsec, raster = snap_r)
      perimeters[[length(perimeters)+1]]<<- list(time = tsec, perim = perim,
                                                 burned = burned_poly, front_pts = front_pts)
    }
    
    # ------------------------------------------------
    #Growing fire loop
    
    while (t < t_end) {
      if (t + dt > t_end) dt <- t_end - t
      
      # SSP-RK3 step
      f1 <- rhs(Tn, Yn)
      T1 <- Tn + dt * f1$dTdt
      Y1 <- clamp(Yn + dt * f1$dYdt, lower = 0, upper = 1)
      
      f2 <- rhs(T1, Y1)
      T2 <- 0.75*Tn + 0.25*(T1 + dt*f2$dTdt)
      Y2 <- clamp(0.75*Yn + 0.25*(Y1 + dt*f2$dYdt), 0, 1)
      
      f3 <- rhs(T2, Y2)
      Tn <- (1/3)*Tn + (2/3)*(T2 + dt*f3$dTdt)
      Yn <- clamp((1/3)*Yn + (2/3)*(Y2 + dt*f3$dYdt), 0, 1)
      
      t  <- t + dt
      dt <- dt_cfl(u, v, f3$k, dx, dy)
      
      # --- NEW: active front, ROS, perimeter, snapshots ---
      t_win0 <- max(0, t - snapshot_every)
      Front_win <- terra::ifel(!is.na(Arr) & (Arr > t_win0) & (Arr <= t), 1, NA)
      
      # ROS on the window front (use the same grad(Arr), just mask with Front_win)
      Ae <- shift_samegrid(Arr, dx=+dx, ref=Tn); Aw <- shift_samegrid(Arr, dx=-dx, ref=Tn)
      An <- shift_samegrid(Arr, dy=+dy, ref=Tn); As <- shift_samegrid(Arr, dy=-dy, ref=Tn)
      Ae <- terra::cover(Ae, Arr); Aw <- terra::cover(Aw, Arr)
      An <- terra::cover(An, Arr); As <- terra::cover(As, Arr)
      
      dAdx  <- (Ae - Aw) / (2*dx)
      dAdy  <- (An - As) / (2*dy)
      gradA <- sqrt(dAdx^2 + dAdy^2)
      
      ROS_full <- 1 / (gradA + 1e-9)
      ROS      <- ifel(Front_win == 1, ROS_full, NA)   # <— mask with Front_win
      
      # perimeter line
      perim <- tryCatch({ as.contour(Tn, levels = T_pc) }, error = function(e) NULL)
      
      # snapshot every snapshot_every seconds
      if (t >= next_snap_time || t >= t_end) {
        write_snapshot(Tn, Yn, ROS, Front_win, perim, t)  # <— pass Front_win
        next_snap_time <- next_snap_time + snapshot_every
      }
      
      if (toupper(Plot) == "YES" && t >= t_next_plot) {
        plot(Tn, main = sprintf("T @ t=%.0fs", t))
        if (!is.null(perim)) try(lines(perim, col = "white", lwd = 1.5), silent = TRUE)
        t_next_plot <- t_next_plot + save_every
      }
    }
    
    # return results (final rasters + time series in memory)
    out <- c(Tn, Yn); names(out) <- c("T_final","Y_final")
    return(list(
      out = out,
      Arr = Arr,
      snapshots = snapshots,
      perimeters = perimeters,
      spread_buffer = spread_buffer,
      template = fine_template
    ))
  } else{ #Non-Spatial
    stop("Non-spatial mode not implemented in this block.")
  }
}


Montilla_FireDynamics(Spatial=TRUE,
                      Landscape<-Landscape_c,
                      Environment<-Enviroment_c,
                      Weather,
                      Ignitions=NULL,
                      IgnitionProbability=ign,
                      NumberIgnitions=1,
                      Plot="YES",
                      cellsize = 10,   
                      downscale_sd_frac = 0.05,
                      snapshot_every = 120,  
                      t_end = 1200,          
                      snapshot_dir = paste0(path,"/Snapshots/"),   
                      perimeter_dir = paste0(path,"/Perimeters/"))

#Check results:
ext(bui)

#extent crop y: 762000,766000, x:1104000,1106000
rasts<-list.files(paste0(path,"Snapshots/"),full.names=TRUE)
perims<-list.files(paste0(path,"Snapshots/"),full.names=TRUE)

result_rasts<-c()
for(i in 1:length(rasts)){
  r<-rast(rasts[[i]])
  ROS<-r$ROS
  T<-r$T
  Y<-r$Y
  F<-r$Front
  result_rasts[[i]]<-Y
}

result_rasts<-rast(result_rasts)

Extent<- ext(1104400,1105000,763000,763500)

crop_res<-crop(result_rasts,Extent)

#View
plot(crop_res)


