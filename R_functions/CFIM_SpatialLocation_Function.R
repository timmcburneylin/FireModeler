
#Spatial funciton to locate moving surface fire in CFIM Module


#Function to locate the fire or spatial locations of plumes in fires when predicting crowning
  #adapted from code for CFIM Cruz et al 2006

fire_position<-function(cbh, type, ODE_Results){
  
  # Unpack the ODE interpolation functions
  y1 <- ODE_Results$y1
  y2 <- ODE_Results$y2
  y3 <- ODE_Results$y3
  y4 <- ODE_Results$y4
  y5 <- ODE_Results$y5
  y6 <- ODE_Results$y6
  
  if(type == "CenterPlume"){  #Center Position of the plume in the x-axis when z=cbh

    s_seq <- seq(0, 40, by = 0.005)
    CenterS <- NA
    
    for (s in s_seq) {
      z <- y6(s)  
      if (z > cbh) {
        CenterS <- s
        break
      }
    }
    
    if (is.na(CenterS)) {
      warning("No s value found for which y6(s) > cbh")
      return(NA)
    }
    
    CenterX <- y5(CenterS)
    return(CenterX)
    
  }else if(type == "ForwardPlume"){ #Forward Position of the plume in the x-axis when z=cbh-------------------------------------

    s_seq <- seq(0, 40, by = 0.005)
    ForwardS <- NA
    
    for (s in s_seq) {
      a <- y6(s) - 
        sin(pi/2 - y3(s)) * 
        ( y1(s) * ( y1(s) / ( 1.2 * (300 / (300 + (y4(s) / y1(s))) ) * y2(s) ) ) )
      
      if (a > cbh) {  # Since {test} = a - cbh and we check if test > 0
        ForwardS <- s
        break
      }
    }
    
    if (is.na(ForwardS)) {
      warning("No s value found for ForwardPos where the condition is met")
      return(NA)
    }
    
    ForwardX <- y5(ForwardS) + 
      cos(pi/2 - y3(ForwardS)) * 
      ( y1(ForwardS) * ( y1(ForwardS) / ( 1.2 * (300 / (300 + (y4(ForwardS) / y1(ForwardS))) ) * y2(ForwardS) ) ) )
    return(ForwardX)
    
  }else if(type == "RearPlume"){ #Rear Position of the plume in the x-axis when z=cbh------------------------------
    s_seq <- seq(0, 40, by = 0.005)
    RearS <- NA
    
    for (s in s_seq) {
      c_val <- y6(s) + 
        sin(pi/2 - y3(s)) * 
        ( y1(s) * ( y1(s) / ( 1.2 * (300 / (300 + (y4(s) / y1(s))) ) * y2(s) ) ) )
      
      if (c_val > cbh) {  # checking if c - cbh > 0
        RearS <- s
        break
      }
    }
    
    if (is.na(RearS)) {
      warning("No s value found for RearPos where the condition is met")
      return(NA)
    }
    
    RearX <- y5(RearS) - 
      cos(pi/2 - y3(RearS)) * 
      ( y1(RearS) * ( y1(RearS) / ( 1.2 * (300 / (300 + (y4(RearS) / y1(RearS))) ) * y2(RearS) ) ) )
    return(RearX)
    
  }else if(type == "CenterS"){  #Position of  center s(distance along the plume) when z=cbh------------------------

    
    s_seq <- seq(0, 40, by = 0.005)
    for (s in s_seq) {
      if (y6(s) > cbh)
        return(s)
    }
    warning("No s value found for CentS")
    return(NA)
    
  }else if(type == "ForwardS"){ #Position of  forward s(distance along the plume) when z=cbh-------------------------------------
    
    s_seq <- seq(0, 40, by = 0.005)
    for (s in s_seq) {
      a <- y6(s) - sin(pi/2 - y3(s)) * ( y1(s) * ( y1(s) / (1.2*(300/(300 + (y4(s)/y1(s)))) * y2(s)) ) )
      if (a > cbh)
        return(s)
    }
    warning("No s value found for ForwS")
    return(NA)
    
  }else{ #Position of  rear s(distance along the plume) when z=cbh------------------------------------------------
    
    s_seq <- seq(0, 40, by = 0.005)
    for (s in s_seq) {
      c_val <- y6(s) + sin(pi/2 - y3(s)) * ( y1(s) * ( y1(s) / (1.2*(300/(300 + (y4(s)/y1(s)))) * y2(s)) ) )
      if (c_val > cbh)
        return(s)
    }
    warning("No s value found for ReaS")
    return(NA)
  }
}