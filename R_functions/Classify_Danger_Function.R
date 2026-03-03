#write function to calculate Danger Class based on FWI, BUI, and Danger Zone for British Columbia

#write function to calculate danger code
#write function to calculate danger code
classify_danger<- function(BUI,FWI,DZ){
  # Check that all vectors are of the same length
  if (length(BUI) != length(FWI) || length(FWI) != length(DZ)) {
    stop("All input vectors must be of the same length")
  }
  
  DC <- numeric(length(BUI))
  for(i in 1:length(BUI)){
    # Skip if any key input is NA
    if (is.na(BUI[i]) || is.na(FWI[i]) || is.na(DZ[i])){
      DC[i]<-NA
    }
    bui<-BUI[i]
    fwi<-FWI[i]
    dz=DZ[i]  
    if(dz == 1){
      if(fwi < 1){
        DC[i]<-ifelse(bui > 0 & bui<= 19, 1,
                      ifelse(bui> 19 & bui <= 42,2,
                             ifelse(bui> 42 & bui<= 69,2,
                                    ifelse(bui> 69 & bui<= 118,2, 3))))
        
      }else if(fwi >= 1 & fwi <= 7){
        DC[i]<-ifelse(bui > 0 & bui<= 19,2,
                      ifelse(bui> 19 & bui <= 42,2,
                             ifelse(bui>42 & bui<= 69, 3,
                                    ifelse(bui>69 & bui<= 118, 3, 3))))
        
      }else if(fwi > 7 & fwi <= 16){
        DC[i]<-ifelse(bui > 0 & bui<= 19, 2,
                      ifelse(bui> 19 & bui <= 42,  3,
                             ifelse(bui > 42 & bui<= 69, 3,
                                    ifelse(bui > 69 & bui<= 118, 4, 4))))
        
      }else if(fwi > 16 & fwi <= 30){
        DC[i]<-ifelse(bui > 0 & bui<= 19, 3,
                      ifelse(bui > 19 & bui <= 42,  3,
                             ifelse(bui > 42 & bui<= 69, 4,
                                    ifelse(bui > 69 & bui<= 118, 4, 4))))
      }else{
        DC[i]<-ifelse(bui > 0 & bui<= 19, 3,
                      ifelse(bui > 19 & bui <= 42,  4,
                             ifelse(bui > 42 & bui<= 69, 4,
                                    ifelse(bui > 69 & bui<= 118, 5, 5))))}
    }else if(dz == 2){
      if(fwi > 0 & fwi <= 4){
        DC[i]<-ifelse(bui > 0 & bui<= 48, 1,
                      ifelse(bui > 48 & bui <= 85,2,
                             ifelse(bui > 85 & bui<= 118,2,
                                    ifelse(bui > 118 & bui<= 158,2, 3))))
        
      }else if(fwi > 4 & fwi <= 16){
        DC[i]<-ifelse(bui > 0 & bui<= 48, 2,
                      ifelse(bui > 48 & bui <= 85,2,
                             ifelse(bui > 85 & bui<= 118,3,
                                    ifelse(bui > 118 & bui<= 158,3, 3))))
        
      }else if(fwi > 16 & fwi <= 26){
        DC[i]<-ifelse(bui > 0 & bui<= 48, 2,
                      ifelse(bui > 48 & bui <= 85,3,
                             ifelse(bui > 85 & bui<= 118,3,
                                    ifelse(bui > 118 & bui<= 158,4, 4))))
        
      }else if(fwi > 26 & fwi <= 37){
        DC[i]<-ifelse(bui > 0 & bui<= 48, 3,
                      ifelse(bui > 48 & bui <= 85, 3,
                             ifelse(bui > 88 & bui<= 118,4,
                                    ifelse(bui > 118 & bui<= 158, 4, 5))))
        
      }else{
        DC[i]<-ifelse(bui > 0 & bui<= 48, 3,
                      ifelse(bui > 48 & bui <= 85, 4,
                             ifelse(bui > 85 & bui<= 118, 4,
                                    ifelse(bui > 118 & bui<= 158, 5, 5))))}
    }else{
      if(fwi <= 4){
        DC[i]<-ifelse(bui<= 50, 1,
                      ifelse(bui > 50 & bui <= 90,2,
                             ifelse(bui > 90 & bui<= 140,2,
                                    ifelse(bui > 140 & bui<= 200,2, 3))))
        
      }else if(fwi > 4 & fwi <= 16){
        DC[i]<-ifelse(bui > 0 & bui<= 50, 2,
                      ifelse(bui >  50 & bui <= 90, 2,
                             ifelse(bui > 90 & bui<= 140,3,
                                    ifelse(bui > 140 & bui<= 200, 3, 3))))
        
      }else if(fwi > 16 & fwi <= 27){
        DC[i]<-ifelse(bui > 0 & bui<= 50, 2,
                      ifelse(bui > 50 & bui <= 90, 3,
                             ifelse(bui > 90 & bui<= 140, 3,
                                    ifelse(bui > 140 & bui<= 200, 4, 4))))
        
      }else if(fwi > 27 & fwi <= 46){
        DC[i]<-ifelse(bui > 0 & bui<= 50, 3,
                      ifelse(bui > 50 & bui <= 90, 3,
                             ifelse(bui > 90 & bui<= 140, 4,
                                    ifelse(bui > 140 & bui<= 200, 4, 4))))
        
      }else{
        DC[i]<-ifelse(bui > 0 & bui<= 50, 3,
                      ifelse(bui > 50 & bui <= 90, 4,
                             ifelse(bui > 90 & bui<= 140, 5,
                                    ifelse(bui > 140 & bui<= 200, 5, 5))))}
    }
  }
  return(DC)
}