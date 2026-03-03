#Fine fuel moisture stand adjusted
FFMC_sa<-function(forest,season,ffmc,dmc,density){
  #Density: either "Light" (20-45% canopy closure), "Moderate" (46-60% canopy closure), or "Dense" (>61% canopy closure)
  #can use canopy closure which means; the coverage of trees from a single point ie can use photo method with wide view angle
  DENSITY=density
  #Forest type: either: "Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce"
  FOR_T=forest
  FOR_T<-ifelse(is.na(FOR_T),"Mixed",FOR_T)
  #Season: either "Spring", "Summer, "Fall
  SEASON=season
  #Duff Moisture Code: numeric value
  DMC=dmc
  #Fine Fuel Moisture Code: numeric value
  FFMC=ffmc
  #Calculate stand adjusted MC or MS.SA from Wotton and Beverly 2007
  mc_cons<-0.002232
  #moisture content adjusted FFMC and DMC
  mc.FFMC <- 147.27*((101-FFMC)/(59.5+FFMC))
  mc.DMC <- 20 + exp(-1*((DMC-244.72)/43.43))
  if(DENSITY == "Light"){
    #stratify by forest type
    if(FOR_T == "Douglas-fir"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.0202 + 0.6264*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.5749 + 0.9734*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.5500 + 1.0538*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    } else if(FOR_T=="Pine"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.8517 + 0.3709*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(0.2566 + 0.7179*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(0.2819 + 0.7983*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else{
      if(SEASON == "Spring"){
        MC.SA<-exp(0.7977 + 0.5042*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(0.2026 + 0.8512*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(0.2279 + 0.9316*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }
  }else if (DENSITY == "Moderate"){
    if(FOR_T == "Douglas-fir"){
      if(SEASON == "Spring"){
        MC.SA<-exp(-0.2710 + 0.8176*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.8661 + 1.1646*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.8408 + 1.2450*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    } else if(FOR_T=="Pine"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.5605 + 0.5621*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.0346 + 0.9091*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.0093 + 0.9895*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else if(FOR_T=="Deciduous"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.4387 + 0.7133*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.1554 + 1.0603*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.1311 + 1.1407*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else if(FOR_T=="Mixed"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.5065 + 0.6954*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.0886 + 1.0424*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.0633 + 1.1228*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else{
      if(SEASON == "Spring"){
        MC.SA<-exp(0.4479 + 0.6197*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.1472 + 0.9667*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.1219 + 1.0471*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }
  } else{
    if(FOR_T == "Douglas-fir"){
      if(SEASON == "Spring"){
        MC.SA<-exp(-0.2710 + 0.8176*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.8661 + 1.1646*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.8408 + 1.2450*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    } else if(FOR_T=="Pine"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.5605 + 0.5621*log(mc.FFMC)+mc_cons*mc.DMC)
      }else if(SEASON == "Summer"){
        MC.SA<-exp(-0.7182 + 1.2420*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.6929 + 1.3224*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else if(FOR_T=="Deciduous"){
      if(SEASON == "Spring"){
        MC.SA<-exp(-0.2449 + 1.0462*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.8400 + 1.3932*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.8147 + 1.4736*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else if(FOR_T=="Mixed"){
      if(SEASON == "Spring"){
        MC.SA<-exp(0.5065 + 0.6954*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.0886 + 1.0424*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.0633 + 1.1228*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }else{
      if(SEASON == "Spring"){
        MC.SA<-exp(0.4479 + 0.6197*log(mc.FFMC)+mc_cons*mc.DMC)
      } else if(SEASON == "Summer"){
        MC.SA<-exp(-0.1472 + 0.9667*log(mc.FFMC)+mc_cons*mc.DMC)
      }else{
        MC.SA<-exp(-0.1219 + 1.0471*log(mc.FFMC)+mc_cons*mc.DMC)
      }
    }
  }
  #export as a percent
  MC_sa<-MC.SA
  
  return(MC_sa)
}