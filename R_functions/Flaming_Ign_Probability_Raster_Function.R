#Raster Version---------

#Function to predict probabability of sustained flaming ignition from Beverly and Wotton 2007:

#Modelling the probability of sustained flaming: predictive value of fire
#weather index components compared with observations of site weather and
#fuel moisture conditions


#Based on Canadian Test fire Database and predicts the probability firebrand will produce sustained flaming ignitions that 
#would continue to spread with (n>1000) test fires

#Uses either site variables or FWI index components by different fuel classes.

#Fuel Classes:---------------------------------------------

#FC1: Grass fuel spring, ie matted and non-cured
#-Open site with scattered Douglas fir (Pseudotsuga menziesii (Mirb.) Franco) on a south-west slope.

#FC2: Grass fuel summer, ie standing and partially cured
#-Open site with scattered lodgepole pine (Pinus contorta Dougl.) on an exposed south-west slope.

#FC3: Pine lichen, ie pine overstory with lichen fuelbed
#-Dense, even-aged, 85-year old jack pine (Pinus banksiana Lamb.) stand of fire origin, located 80m from 
#the weather station with a basal area of 26m2 per hectare. Average diameter at breast height (DBH) is 
#10 cm and maximum tree height is 20 m. There is a low density of
#minor vegetation. Predominant surface fuels are Hylocomium and Calliergon spp. mosses
#and Cladonia spp., with scattered needles and twigs, and clumps of Linnea, Vaccinium, Arctostaphylos
#spp. and grass present. Scattered Shepherdia and Salix spp. shrubs occur. Surface fuels overlie a thin,
#partly decomposed layer (fermentation layer [F]) and a 1.3 cm decomposed layer (humus [H]).
#The full organic layer is generally <5 cm deep over fine sand. (applies to FC4 and FC5 also)

#FC4: Pine moss, ie pine overstory with moss fuelbed
#{see FC3}

#FC5: Pine needles, ie pine overstory with needle fuelbed (C7, C6)
#{see FC3}

#FC6: Mixed wood moss, ie mixed wood fuels with moss understory
#Mature, uneven-aged white spruce (Picea glauca (Moench) Voss)-trembling aspen (Populustremuloides spp.)-jack pine 
#stand with a basal area of 34m2 per hectare. High bush cranberry
#needles–leaf (Viburnum opulus L. var. americanum (Mill.) Ait.) and Shepherdia spp. shrubs cover 20–30%
#of the understory. Dominant surface fuels include Hylocomium and Calliergon spp. mosses, leaves
#and needles. The full organic layer varies in depth from 1.3 to 3.8 cm. Researchers noted evidence of fire
#45 to 50 years before the test fires.

#FC7: Mixed wood needle/leaf summer, mixedwood fuels with needle and leaf fuelbed in summer ie greenup
#{see FC7}

#FC8: Spruce moss, ie spruce overatory with moss understory (C1, C2)
#Very dense, 85-year old, even-aged black spruce (Picea mariana (Mill.) BSP) stand with a basal area of
#∼37m2 per hectare. A large proportion of the trees are suppressed, giving the stand an uneven-aged
# appearance. Hylocomium spp. moss, 1.3–12.7 cm deep covers 100% of the ground surface.
#The organic layer reaches a depth of 18 cm in some locations, overlying very fine silty sand.

#FC9: Aspen grass summer, ie aspen overstory with grass understory
#Pure, 60-year old even-aged trembling aspen stand with a basal area of ∼22m2 per hectare. During the
#summer months there is dense minor vegetation cover consisting of clumps of Salix spp., Shepherdia
# spp. and Rose (Rosa spp.) bushes. Underneath this shrub layer is a fairly complete cover of Epilobium,
#Lathyrus, Vicia spp. and grass. Leaf cover is 0.6–1.3 cm in depth.

#FC10: Aspen leaf summer, ie aspen overstory with aspen leaf understory
#{see FC9}


#Run function--------------------------------------------------------------------------------

sf_prob_raster <- function(FC_rast = NULL, FT_rast, RH_rast, FFMC_rast, DMC_rast, ISI_rast, FWI_rast, TEMP_rast, effm_rast = NULL) {
  
  # Predict Fine Fuel Moisture (effm) if not provided
  if (is.null(effm_rast)) {
    # Assuming Anderson model; replace this with an actual raster-based calculation if needed
    effm_rast <- 1.651 * RH_rast^0.493 + 0.001972 * exp(0.092 * RH_rast) + 0.101 * (23.9 - TEMP_rast)
  }
 
  
  # Categorize FuelType if Fuel Category is not known
  if (is.null(FC_rast)) {
    FC_rast <- ifel(
      FT_rast %in% c(1, 2), 8,  # C1, C2 -> 8
      ifel(FT_rast == 3, 4,     # C3 -> 4
           ifel(FT_rast %in% c(4, 6), 5,  # C4, C6 -> 5
                ifel(FT_rast == 5, 3,     # C5 -> 3
                     ifel(FT_rast == 7, 2,     # C7 -> 2
                          ifel(FT_rast %in% c(8, 979), 10,  # D1, D2 -> 10
                               ifel(FT_rast %in% c(14, 15), 2,   # O1a, O1b -> 2
                                    ifel(FT_rast %in% c(9, 10, 11, 12), 6,  # M1-M4 -> 6
                                         ifel(FT_rast %in% 16:18, 2,  # Specific values 16, 17, 18 -> 2
                                              NA_real_  # Default case for undefined values
                                         )))))))))
    
  }
  
  # Logistic regression applied across raster layers
  p_sf_rast <- ifel(
    FC_rast == 1, 1 / (1 + exp(-(7.3703 - 0.1725 * RH_rast))),
    ifel(FC_rast == 2, 1 / (1 + exp(-(-23.3755 + 0.2895 * FFMC_rast))),
         ifel(FC_rast == 3, 1 / (1 + exp(-(-34.8731 + 0.4304 * FFMC_rast))),
              ifel(FC_rast == 4, 1 / (1 + exp(-(-4.414 + 0.3368 * FWI_rast))),
                   ifel(FC_rast == 5, 1 / (1 + exp(-(-4.8479 + 0.0258 * DMC_rast + 0.6032 * ISI_rast))),
                        ifel(FC_rast == 6, 1 / (1 + exp(-(-24.3837 + 0.2407 * FFMC_rast + 0.0638 * DMC_rast))),
                             ifel(FC_rast == 7, 1 / (1 + exp(-(-127.8 + 1.3902 * FFMC_rast))),
                                  ifel(FC_rast == 8, 1 / (1 + exp(-(-38.9 + 0.4117 * FFMC_rast + 0.0679 * DMC_rast))),
                                       ifel(FC_rast == 9, 1 / (1 + exp(-(-3.6403 + 0.1558 * FWI_rast))),
                                            ifel(FC_rast == 10, 1 / (1 + exp(-(-2.8566 - 0.2456 * effm_rast))),
                                                 NA_real_))))))))))
  
  return(p_sf_rast)
}

