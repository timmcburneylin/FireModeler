#Raster version of stand adjusted moisture content equation from Wotton and Beverly 2007
#-----------------------------------------------------------

# Function for calculating stand-adjusted FFMC with dynamic density from crown closure
FFMC_sa_raster <- function(forest_rast, season, ffmc_rast, dmc_rast, crown_closure_rast) {
  
  # Define constants
  mc_cons <- 0.002232
  
  # Calculate moisture content adjusted FFMC and DMC
  mc_FFMC <- 147.27 * ((101 - ffmc_rast) / (59.5 + ffmc_rast))
  mc_DMC <- 20 + exp(-1 * ((dmc_rast - 244.72) / 43.43))
  
  # Classify crown closure into density categories
  density_rast <- ifel(
    crown_closure_rast >= 20 & crown_closure_rast <= 45, 1,  # Light
    ifel(crown_closure_rast >= 46 & crown_closure_rast <= 60, 2,  # Moderate
         ifel(crown_closure_rast > 60, 3, NA_real_))  # Dense
  )
  
  # Initialize raster for adjusted moisture content
  MC_SA <- forest_rast
  
  #Create Season Rast
  szn<-ifelse(season=="Spring",1,ifelse(season=="Summer",2,3))
  szn_vec<-rep(szn,ncell(forest_rast))
  szn_rast<- setValues(forest_rast,szn_vec)
  
  # Apply raster-based logic based on density and forest type
  MC_SA <- ifel(
    density_rast == 1,  # Light density
    # TRUE branch for density == 1
    ifel(forest_rast == 4,  # Douglas-fir
         ifel(szn_rast == 1, 
              exp(0.0202 + 0.6264 * log(mc_FFMC) + mc_cons * mc_DMC),
              ifel(szn_rast == 2, 
                   exp(-0.5749 + 0.9734 * log(mc_FFMC) + mc_cons * mc_DMC),
                   exp(-0.5500 + 1.0538 * log(mc_FFMC) + mc_cons * mc_DMC))),
         ifel(forest_rast == 2,  # Pine
              ifel(szn_rast == 1, 
                   exp(0.8517 + 0.3709 * log(mc_FFMC) + mc_cons * mc_DMC),
                   ifel(szn_rast == 2, 
                        exp(0.2566 + 0.7179 * log(mc_FFMC) + mc_cons * mc_DMC),
                        exp(0.2819 + 0.7983 * log(mc_FFMC) + mc_cons * mc_DMC))),
              ifel(forest_rast %in% c(1, 3),  # Spruce and Mixed
                   ifel(szn_rast == 1, 
                        exp(0.7977 + 0.5042 * log(mc_FFMC) + mc_cons * mc_DMC),
                        ifel(szn_rast == 2, 
                             exp(0.2026 + 0.8512 * log(mc_FFMC) + mc_cons * mc_DMC),
                             exp(0.2279 + 0.9316 * log(mc_FFMC) + mc_cons * mc_DMC))),
                   NA_real_))),
    # FALSE branch for density == 1
    ifel(density_rast == 2,  # Moderate density
         # TRUE branch for density == 2
         ifel(forest_rast == 4,  # Douglas-fir
              ifel(szn_rast == 1, 
                   exp(-0.2710 + 0.8176 * log(mc_FFMC) + mc_cons * mc_DMC),
                   ifel(szn_rast == 2, 
                        exp(-0.8661 + 1.1646 * log(mc_FFMC) + mc_cons * mc_DMC),
                        exp(-0.8408 + 1.2450 * log(mc_FFMC) + mc_cons * mc_DMC))),
              ifel(forest_rast == 2,  # Pine
                   ifel(szn_rast == 1, 
                        exp(0.5605 + 0.5621 * log(mc_FFMC) + mc_cons * mc_DMC),
                        ifel(szn_rast == 2, 
                             exp(-0.0346 + 0.9091 * log(mc_FFMC) + mc_cons * mc_DMC),
                             exp(-0.0093 + 0.9895 * log(mc_FFMC) + mc_cons * mc_DMC))),
                   ifel(forest_rast == 3,  # Mixed
                        ifel(szn_rast == 1, 
                             exp(0.5065 + 0.6954 * log(mc_FFMC) + mc_cons * mc_DMC),
                             ifel(szn_rast == 2, 
                                  exp(-0.0886 + 1.0424 * log(mc_FFMC) + mc_cons * mc_DMC),
                                  exp(-0.0633 + 1.1228 * log(mc_FFMC) + mc_cons * mc_DMC))),
                        ifel(forest_rast == 1,  # Spruce
                             ifel(szn_rast == 1, 
                                  exp(0.4479 + 0.6197 * log(mc_FFMC) + mc_cons * mc_DMC),
                                  ifel(szn_rast == 2, 
                                       exp(-0.1472 + 0.9667 * log(mc_FFMC) + mc_cons * mc_DMC),
                                       exp(-0.1219 + 1.0471 * log(mc_FFMC) + mc_cons * mc_DMC))),
                             NA_real_)))),
         # FALSE branch for density == 2
         ifel(density_rast == 3,  # Dense density
              ifel(forest_rast == 4,  # Douglas-fir
                   ifel(szn_rast == 1, 
                        exp(-0.2710 + 0.8176 * log(mc_FFMC) + mc_cons * mc_DMC),
                        ifel(szn_rast == 2, 
                             exp(-0.8661 + 1.1646 * log(mc_FFMC) + mc_cons * mc_DMC),
                             exp(-0.8408 + 1.2450 * log(mc_FFMC) + mc_cons * mc_DMC))),
                   ifel(forest_rast == 2,  # Pine
                        ifel(szn_rast == 1, 
                             exp(0.5605 + 0.5621 * log(mc_FFMC) + mc_cons * mc_DMC),
                             ifel(szn_rast == 2, 
                                  exp(-0.7182 + 1.2420 * log(mc_FFMC) + mc_cons * mc_DMC),
                                  exp(-0.6929 + 1.3224 * log(mc_FFMC) + mc_cons * mc_DMC))),
                        ifel(forest_rast == 1,  # Spruce
                             ifel(szn_rast == 1, 
                                  exp(0.4479 + 0.6197 * log(mc_FFMC) + mc_cons * mc_DMC),
                                  ifel(szn_rast == 2, 
                                       exp(-0.1472 + 0.9667 * log(mc_FFMC) + mc_cons * mc_DMC),
                                       exp(-0.1219 + 1.0471 * log(mc_FFMC) + mc_cons * mc_DMC))),
                             ifel(forest_rast == 3,  # Mixed
                                  ifel(szn_rast == 1, 
                                       exp(0.5065 + 0.6954 * log(mc_FFMC) + mc_cons * mc_DMC),
                                       ifel(szn_rast == 2, 
                                            exp(-0.0886 + 1.0424 * log(mc_FFMC) + mc_cons * mc_DMC),
                                            exp(-0.0633 + 1.1228 * log(mc_FFMC) + mc_cons * mc_DMC))),
                                  NA_real_)))),
              NA_real_))
  )
    
  return(MC_SA)
  
  }

