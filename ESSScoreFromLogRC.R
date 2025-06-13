# R script to convert logRC ratios to ESS-M scores
# Raymond Nelson

# called by the ESSMScoresFn() in the ESSMScores.R script

RCToESSFn <- function(thisScore, thisSensor) {
  # R script to convert logRC ratios to ESS-M scores
  # called by the ESSMScoresFn() in the ESSMScores.R script
  #
  # thisScore is a logRC ratio
  # 
  # thisSensor is is used to select the constraint 
  # and also the weight of the integer score
  # 
  # output is the integer score
  
  
  #### constraints for ESS-M R/C ratios ####
  
  {
    
    # swsome constraints are now initialized in NCCAASCII_init.R 
    
    ### pneumos have both a lower and upper constraint ###
    
    # posPneumoLow <- log(1.33) # 0.2851789
    # posPneumoLow <- log(1.284) # 0.2499802
    #posPneumoLow <- log(1.25) # 0.2231436
    posPneumoLow <- log(1.2) # 0.1823216
    # posPneumoLow <- log(1.221402) # 0.1999994
    # posPneumoLow <- log(1.161834) # 0.1499998
    # posPneumoLow <- log(1.10517) # 0.09999917
    # posPneumoLow <- log(1.1) # 0.09531018
    # posPneumoLow <- log(1.05127) # 0.04999896
    # posPneumoLow <- log(1.05) # 0.04879016
    # posPneumoHigh <- log(1.822115) # 0.5999979
    # posPneumoHigh <- log(1.64871) # 0.4999932
    # posPneumoHigh <- log(1.5) # 0.4054651
    posPneumoHigh <- log(1.6) # 
    # posPneumoHigh <- log(1.67) # 0.5128236
    # posPneumoHigh <- log(2) # 0.0.6931472
    
    posPneumoLow <- pneumoConstraintLow
    posPneumoHigh <- pneumoConstraintHigh
    
    # negPneumoLow <- -log(1.33) # -0.2851789
    # negPneumoLow <- -log(1.284) # -0.2499802
    # negPneumoLow <- -log(1.25) # -0.2231436
    negPneumoLow <- -log(1.2) # -0.1823216
    # negPneumoLow <- -log(1.221402) # 0.1999994
    # negPneumoLow <- -log(1.161834) # -0.1499998
    # negPneumoLow <- -log(1.10517) # -0.09999917
    # negPneumoLow <- -log(1.1) # -0.09531018
    # negPneumoLow <- -log(1.05127) # -0.04999896
    # negPneumoLow <- -log(1.05) # -0.04879016
    # negPneumoHigh <- -log(1.64871) # -0.4999932
    # negPneumoHigh <- -log(1.5) # -0.4054651
    negPneumoHigh <- -log(1.6) # 
    # negPneumoHigh <- -log(1.67) # -0.5128236
    # negPneumoHigh <- -log(2) # -0.6931472
    
    negPneumoLow <- -pneumoConstraintLow
    negPneumoHigh <- -pneumoConstraintHigh
    
    # posEDAHigh <- log(1000) # 6.907755
    # posEDALow <- log(1.25) # 0.2231436
    # posEDALow <- log(1.2) # 0.1823216
    # posEDALow <- log(1.10517) # 0.09999917
    posEDALow <- log(1.1) # 0.09531018
    # posEDALow <- log(1.05127) # 0.04999896
    # posEDALow <- log(1.1) # 0.04879016
    # posEDALow <- log(1.025315) # 0.02499988
    # posEDALow <- log(1.020201) # 0.01999967
    
    # negEDALow <- -log(1.25) # -0.2231436
    # negEDALow <- -log(1.2) # -0.1823216
    # negEDALow <- -log(1.10517) # -0.09999917
    negEDALow <- -log(1.1) # -0.09531018
    # negEDALow <- -log(1.05127) # -0.04999896
    # negEDALow <- -log(1.05) # -0.048791016
    # negEDALow <- -log(1.025315) # -0.02499988
    # negEDALow <- -log(1.020201) # -0.01999967
    
    posEDALow <- EDAConstraint
    negEDALow <- -EDAConstraint
    
    # posCardioHigh <- log(1000) # 6.907755
    # posCardioLow<- log(1.395147) # 0.3329998
    # posCardioLow <- log(1.33) # 0.2851789
    # posCardioLow <- log(1.284) # 0.2499802
    # posCardioLow <- log(1.25) # 0.2231436
    # posCardioLow <- log(1.221402) # 0.1999994
    # posCardioLow <- log(1.2) # 0.1823216
    # posCardioLow <- log(1.10517) # 0.09999917
    posCardioLow <- log(1.1) # 0.09531018
    # posCardioLow <- -log(1.05127) # -0.04999896
    # posCardioLow <- log(1.05) # 0.04879016
    # posCardioLow <- log(1.020201) # 0.01999967
    
    # negCardioHigh <- -log(1000) # -6.907755
    # negCardioLow<- -log(1.395147) # -0.3329998
    # negCardioLow <- -log(1.33) # -0.2851789
    # negCardioLow <- -log(1.284) # -0.2499802
    # negCardioLow <- -log(1.25) # -0.2231436
    # negCardioLow <- -log(1.221402) # -0.1999994
    # negCardioLow <- -log(1.2) # -0.1823216
    # negCardioLow <- -log(1.10517) # -0.09999917
    negCardioLow <- -log(1.1) # -0.09531018
    # negCardioLow <- -log(1.05127) # -0.04999896
    # negCardioLow <- -log(1.05) # -0.04879016
    # negCardioLow <- -log(1.020201) # -0.01999967
    
    # constraints are set in the NCCA_ASCI_init.R script
    posCardioLow <- cardioConstraint
    negCardioLow <- -cardioConstraint
    
    # posPLEHigh <- log(22026) # 9.999979
    # posPLELow <- log(1.05127) # 0.04999896
    # posPLELow <- log(1.10517) # 0.09999917
    # posPLELow <- log(1.1) # 0.09531018
    # posPLELow <- log(1.05127) # 0.04999896 
    # posPLELow <- log(1.05) # 0.04879016
    # posPLELow <- log(1.025315) #0.02499988
    # posPLELow <- log(1.020201) #-0.01999967
    # posPLELow <- log(1.01005) # 0.009999835
    
    # negPLEHigh <- -log(22026) # -9.999979
    # negPLELow <- -log(1.05127) # -0.04999896
    # negPLELow <- -log(1.10517) # -0.09999917
    # negPLELow <- -log(1.1) # -0.09531018
    # negPLELow <- -log(1.05) # -0.04879016
    # negPLELow <- -log(1.05127) # -0.04999896
    # negPLELow <- -log(1.025315) # -0.02499988
    # negPLELow <- -log(1.020201) # -0.01999967
    # negPLELow <- -log(1.01005) # -0.009999835
    
    posPLELow <- log(1.05)
    negPLELow <- -log(1.05)
    
    posPLELow <- PLEConstraint
    negPLELow <- -PLEConstraint
    
  }
  
  #### compute the ESS scores ####
  
  # initialize the output score
  ESSScore <- NA
  
  #### pneumo ####
  
  if(any( thisSensor=="UPneumo", 
          thisSensor=="LPneumo", 
          thisSensor=="Pneumo") ) {
    
    if( thisScore < posPneumoLow && 
        thisScore > negPneumoLow ) {
      # June 21, 2023 changed from <= and >= to < and >
      # score 0 for small responses
      ESSScore <- 0
    } else if( thisScore >= posPneumoLow && 
               thisScore <= posPneumoHigh ) { 
      ESSScore <- as.character("+1")
    } else if( thisScore <= negPneumoLow && 
               thisScore >= negPneumoHigh ) {
      ESSScore <- as.character("-1")
    } else {
      # score exceeds the out constraint for - or +
      ESSScore <- 0
    }
    
    return(ESSScore)
    
  } 
  
  #### EDA ####
  
  if(any(thisSensor=="EDA", 
                thisSensor=="AutoEDA", 
                thisSensor=="ManualEDA") ) {
    if(thisScore >= posEDALow) {
      ESSScore <- as.character("+2")
    } else if(thisScore <= negEDALow) {
      ESSScore <- as.character("-2")
    } else {
      ESSScore <- 0
    }
    return(ESSScore)
  } 
  
  #### cardio ####
  
  if(any(thisSensor=="Cardio",
                thisSensor=="eCardio", 
                thisSensor=="FC") ) {
    if(thisScore >= posCardioLow) {
      ESSScore <- as.character("+1")
    } else if(thisScore <= negCardioLow) {
      ESSScore <- as.character("-1")
    } else {
      ESSScore <- 0
    }
    return(ESSScore)
  } 
  
  #### vasomotor ####
  
  else if(thisSensor=="PLE") {
    if(thisScore >= posPLELow) {
      ESSScore <- as.character("+1")
    } else if(thisScore <= negPLELow) {
      ESSScore <- as.character("-1")
    } else {
      ESSScore <- 0
    }
    return(ESSScore)
  }
  
  #### in case of other conditions ####
  
  return(NA)
  
} # end RCToESSFn()





