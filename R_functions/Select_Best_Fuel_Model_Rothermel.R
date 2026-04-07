#Function to select best fuel model from Rothermel and Anderson fuel models for U.S fire behavior models based on available loadlitter, load1hr, load10hr, and fuelbeddepth
#Input data frame is a dataframe of fuel loads and fuel bed depths in the structure of the firebehavioR::fuelModels dataframes

#Find Best American Model select by surface fuel and forest type:
#ForestType: "Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce", "Grass", "Shrub", "Slash"

#SurfFuel: litter, b_litter (broadleaf-litter), shrub, grass, slash, moss

#Density: < 45% = "Light", 45- 60% = "Moderate", 60% > = "Dense"


find_best_model <- function(data_row, ForestType ,Density, SurfFuelType) {
  # data_row must be a single-row data.frame (or tibble)
  compare <- c("loadLitter", "load1hr", "load10hr", "load100hr", "fuelBedDepth")
  
  # pull the reference vector from the row
  v <- as.numeric(data_row[1, compare, drop = FALSE])
  
  # candidate set
  sfm <- firebehavioR::fuelModels
  sfm$Model<-rownames(sfm)
  
if(Density == "Light"){ #Open Fuels: Grasses, Shrubs, Pine/Douglas-Fire Stands, and Open Boreal Spruce
    
    if(ForestType %in% c("Pine","Spruce","Douglas-fire")){ #Conifer Fuels
      
      if(SurfFuelType == "slash"){ #Slash Fuels in post treatment ie clearcut with retention or a blowdown open stand
        
        allowed_models = c("SB1","SB2","SB3","SB4")
        
      }else if(SurfFuelType == "shrub"){ #shrub fuels like chaparrale or open stands of conifers with shrubs
        
        allowed_models = c("SH1","SH2","SH3","SH4","SH5","SH6","SH7","SH8","SH9","GS1","GS2","GS3","GS4", "TU5","TU2","TU1")
        
      }else if(SurfFuelType == "grass"){ #grass fuels in pure stands or with open dispered conifers
        
        allowed_models = c("TU1","TU2","GR1","GR2","GR3","GR4","GR5","GR6","GR7","GR8","GR9","GS1","GS2","GS3","GS4")
        
      }else if(SurfFuelType == "moss"){ #open stands of spruce with moss understory
        
        allowed_models = c("TU4")
        
      }else if(SurfFuelType == "litter"){ #open stands of conifers with litter understory ie PIPO or Longleaf stands 
        
        allowed_models = c("TU1","TL8","TL1", "TU5")
        
      }else{ #open stands of broadleafs: eg aspen stands
        allowed_models = c("TL9","TL3","TL4")
        
      }
        
  }else if(ForestType %in% c("Mixed")){#Mixed Fuels

        if(SurfFuelType == "slash"){ #Slash Fuels in post treatment ie clearcut with retention or a blowdown open stand
          
          allowed_models = c("SB1","SB2","SB3","SB4")
          
        }else if(SurfFuelType == "shrub"){ #shrub fuels like chaparrale or open stands of conifers with shrubs
          
          allowed_models = c("SH1","SH2","SH3","SH4","SH5","SH6","SH7","SH8","SH9","GS1","GS2","GS3","GS4", "TU5","TU2","TU1")
          
        }else if(SurfFuelType == "grass"){ #grass fuels in pure stands or with open dispered conifers
          
          allowed_models = c("TU1","TU2","GR1","GR2","GR3","GR4","GR5","GR6","GR7","GR8","GR9","GS1","GS2","GS3","GS4")
          
        }else if(SurfFuelType == "moss"){ #open stands of spruce with moss understory
          
          allowed_models = c("TU4")
          
        }else if(SurfFuelType == "litter"){ #open stands of conifers with litter understory ie PIPO or Longleaf stands 
          
          allowed_models = c("TU1","TL8","TL1", "TU5")
          
        }else{ #open stands of broadleafs: eg aspen stands
          allowed_models = c("TL9","TL3","TL4")
          
        }
        
  }else if(ForestType %in% c("Grass")){ #Grass Fuels maybe with Pine Overstory
      
      allowed_models = c("GR1","GR2","GR3","GR4","GR5","GR6","GR7","GR8","GR9","GS1","GS2","GS3","GS4")
      
  }else if(ForestType %in% c("Slash")){ #Slash or Blowdown
      
      allowed_models = c("SB1","SB2","SB3","SB4", "TL5")
      
  }else{ #Deciduous fuels
      allowed_models = c("TL9","TL3")
      }
    
}else if(Density == "Moderate"){ #Closed to Medium Forests:----------------------------------------------------------
    
  if(ForestType %in% c("Pine","Spruce","Douglas-fire")){ #Conifer Fuels closed
      
      if(SurfFuelType == "slash"){ #Slash Fuels in post treatment ie clearcut with retention or a blowdown open stand
        
        allowed_models = c("SB1","SB2","SB3","SB4")
        
      }else if(SurfFuelType == "shrub"){ #shrub fuels like interior denser stands or coastal open stands
        
        allowed_models = c("TU1","TU2")
        
      }else if(SurfFuelType == "grass"){ #grass fuels in moderately closed stands
        
        allowed_models = c("TU1","TU4","TU3")
        
      }else if(SurfFuelType == "moss"){ #closed stands of spruce with moss understory
        
        allowed_models = c("TU4")
        
      }else if(SurfFuelType == "litter"){ #moderately closed stands of conifers with litter understory can have some grass/shrubs
        
        allowed_models = c("TU1","TU5","TU2","TU3","TL1","TL8","TL4","TL7","TL3")
      
      }else{ #moderate closed stands of broadleafs: eg aspen stands
        
        allowed_models = c("TL6","TL2")
        
      }
      
  }else if(ForestType %in% c("Mixed")){#Mixed Fuels
      
        if(SurfFuelType == "slash"){ #Slash Fuels in  blowdown stand
          
          allowed_models = c("SB2","SB3","SB4")
          
        }else if(SurfFuelType == "shrub"){ #Shurb fuels under mixed stand
          
          allowed_models = c("TU1","TU2")
          
        }else if(SurfFuelType == "grass"){ #grass fuels under mixed stand
          
          allowed_models = c("TU1")
          
        }else if(SurfFuelType == "moss"){ #open stands of spruce with moss understory
          
          allowed_models = c("TU4")
          
        }else if(SurfFuelType == "litter"){ #open stands of conifers with litter understory ie PIPO or Longleaf stands 
          
          allowed_models = c("TU2","TU3")
          
        }else{ #open stands of broadleafs: eg aspen stands
          allowed_models = c("TL6","TL2")
          
        }
        
  }else if(ForestType %in% c("Grass")){ #Grass Fuels maybe with Pine Overstory
        
        allowed_models = c("TU1","TU4","TU3")
        
  }else if(ForestType %in% c("Slash")){ #Slash or Blowdown
        
        allowed_models = c("SB1","SB2","SB3","SB4", "TL5")
        
  }else{ #Deciduous fuels
        allowed_models = c("TL9","TL3","TL2")
      }
    
}else{ #Dense Stands: closed pines etc--------------------------------------------------------------------
    
  if(ForestType %in% c("Pine","Spruce","Douglas-fire")){ #Conifer Fuels closed
      
      if(SurfFuelType == "slash"){ #Slash or downed logs in coastal dense forests, low spread rate wet burning
        
        allowed_models = c("TL2","TL6")
        
      }else if(SurfFuelType == "shrub"){ #shrub fuels like interior denser stands or coastal open stands
        
        allowed_models = c("TU1","TL5")
        
      }else if(SurfFuelType == "grass"){ #grass fuels in moderately closed stands
        
        allowed_models = c("TU1","TU4","TU3")
        
      }else if(SurfFuelType == "moss"){ #closed stands of spruce with moss understory
        
        allowed_models = c("TU5")
        
      }else if(SurfFuelType == "litter"){ # closed stands of conifers with litter understory 
        
        allowed_models = c("TU1","TU5","TU2","TU3","TL1","TL8","TL4","TL7","TL3")
        
      }else{ # closed stands of broadleafs: eg aspen stands
        
        allowed_models = c("TL8","TL2")
        
      }
      
  }else if(ForestType %in% c("Mixed")){#Mixed Fuels
      
        if(SurfFuelType == "slash"){ #Slash Fuels in  blowdown stand
          
          allowed_models = c("SB2","SB3","SB4")
          
        }else if(SurfFuelType == "shrub"){ #Shurb fuels under mixed stand
          
          allowed_models = c("TU1","TU2","TL5")
          
        }else if(SurfFuelType == "grass"){ #grass fuels under mixed stand
          
          allowed_models = c("TU1")
          
        }else if(SurfFuelType == "moss"){ #open stands of spruce with moss understory
          
          allowed_models = c("TU4")
          
        }else if(SurfFuelType == "litter"){ #open stands of conifers with litter understory ie PIPO or Longleaf stands 
          
          allowed_models = c("TU2","TU3","TL5")
          
        }else{ #open stands of broadleafs: eg aspen stands
          allowed_models = c("TL6","TL2","TL9")
          
        }
        
  }else if(ForestType %in% c("Grass")){ #Grass Fuels maybe with Pine Overstory
        
        allowed_models = c("TU1","TU4","TU3")
        
  }else if(ForestType %in% c("Slash")){ #Slash or Blowdown
        
        allowed_models = c("SB1","SB2","SB3","SB4", "TL5")
        
  }else{ #Deciduous fuels
        allowed_models = c("TL9","TL3","TL2")
      }
    
  }
  
  
  
  
  sfm<-sfm %>% filter(Model %in% allowed_models)
  
  # keep only the comparable columns (ensure order matches)
  sfm_mat <- as.matrix(sfm[, compare])
  
  # L1 (sum of absolute differences) distance to each model
  diffs <- rowSums(abs(sweep(sfm_mat, 2, v, `-`)), na.rm = TRUE)
  
  best_idx <- which.min(diffs)
  best_name <- rownames(sfm)[best_idx]
  
  # return something easy to bind/use
  data.frame(FuelModel = best_name, diff = diffs[best_idx], stringsAsFactors = FALSE)
}
