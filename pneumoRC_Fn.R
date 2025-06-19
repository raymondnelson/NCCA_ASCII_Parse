# R function to parse the Pneumo logRC score
# Raymond Nelson
# 9-24-2021
# 9-28-2011 improved output when values are NA
# Oct 1, 2021 experimented with adjusting values to a 1-100 scale - not implemented


# similar to the PLE_RC_Fn




pneumoRC_Fn <- function(RQValue, 
                        CQValue, 
                        # pneumoConstraint=log(1.33),
                        pneumoConstraintLow=log(1.33),
                        pneumoConstraintHigh=log(1.75)) {
  # R function to compute the logRC score for respiration data
  # called by the RCScoresFn() in the RCScores.R script
  #
  # this is similar to the PLE RC score function
  
  # but differs from the PLE because respiration scores are RLE meseasurements
  # not pre/post ratios
  #
  # pneumo response feature is the log(pre/post) ratio
  # mean respiratory excursion for 6 prestim seconds 
  # divided by the mean excursion for 15 poststim seconds
  # 
  # the log is useful because pre/post ratios are asymmetrical
  # bounded by 0 and infinity with a mean of 1
  # 
  # logRC ratios have + and - value 
  # logRC ratios > 0 are associated with a truth-telling
  # logRC rations < 0 are associated with deception
  # 
  # the R/C ratio is also asymmetrical
  # and also uses a log
  # but the logRC inputs must be transformed first
  #
  ####
  
  # RQValue / CQValue
  # log(0.01461781^.5 / -0.01408841^.5)
  # log(-0.01408841^.5 / 0.01461781^.5)
  # log(-0.01408841^.5 / -0.01461781^.5)
  # log(0.01461781^.5 / 0.01408841^.5)
  
  # same result using these formulae
  # log(((RQValue)^2/(CQValue)^2)^.25)
  # log(.08^.5 / .02^.5)
  # log(.02^.5 / .08^.5)
  # log(((.08)^2/(.02)^2)^.25)
  # log(((.02)^2/(.08)^2)^.25)
  
  # the combination of + and - values
  # along with the use of a min constraint
  # means that the RCScore will be parsed
  # from to 8 possibilities 
  
  ####
  
  {
    
    # Oct 1, 2021
    # invert the sign so that larger values are larger reactions
    # signs were inverted in the pneumoExtractFn
    
    # RQValue and CQValue are -log(pre/post) ratios
    # values further from 0 are greater changes in physiology
    
    # This function requires + log(pre/post) ratios
    
    # RQValue <- (1-exp(RQValue)) * 100
    # CQValue <- (1-exp(CQValue)) * 100
    # 
    # RQValue <- (exp(RQValue)) * 100
    # CQValue <- (exp(CQValue)) * 100
    # 
    # # respiration RQ and CQ values are now between 1 and 100
    # # larger values signify greater changes in physiology
    # sensorMeasurement[respirationRows] <- respirationMeasurements
    
    # RQValue <- RQValue * -1
    # CQValue <- CQValue * -1
    
  }
  
  # Sep 24, 2021
  # adjust missing values
  {
    replaceVal <- 0
    
    if( any(is.na(RQValue), RQValue=="", is.null(RQValue), RQValue <= 0) ) {
      RQValue <- replaceVal
    }
    
    if( any(is.na(CQValue), CQValue=="", is.null(CQValue), CQValue <= 0) ) {
      CQValue <- replaceVal
    }
    
    # exit if either RQValue or CQValue is NA or missing
    if( any(is.na(RQValue), is.na(CQValue), RQValue=="", CQValue=="", is.null(RQValue), is.null(CQValue)) ) {
      return("")
    }
  }
  
  # round the RQ values first
  {
    RQValue <- round(RQValue, 3)
    CQValue <- round(CQValue, 3)
  }
  
  #### respiration constraints ####
  
  {
    
    # # apply the lower constraint to small values
    # {
    #   RQValue <- ifelse(RQValue < pneumoConstraintLow, replaceVal, RQValue)
    #   CQValue <- ifelse(CQValue < pneumoConstraintLow, replaceVal, CQValue)
    # }
    # 
    # # apply the upper constraint to large values
    # 
    # {
    #   RQValue <- ifelse(RQValue > pneumoConstraintHigh, NA, RQValue)
    #   CQValue <- ifelse(CQValue > pneumoConstraintHigh, NA, CQValue)
    # }
    # 
    # # large values that exceed the upper constraint are NA from the pneumoExtractFn
    
  }
  
  # initialize the output 
  RCRatio <- NA
  
  # outcome 0 # Sep 28, 2021
  # NA at RQ and CQ
  if(is.na(RQValue)  && is.na(CQValue)) {
    # negative logRQ ratios indicate no response at CQ or RQ
    return(NA)
  }
  
  # outcome 0a # Sep 28, 2021
  # NA at RQ only
  if(is.na(RQValue)) {
    # negative logRQ ratios indicate no response at CQ or RQ
    return(NA)
  }
  
  # outcome 0b # Sep 28, 2021
  # NA at CQ only
  if(is.na(CQValue)) {
    # negative logRQ ratios indicate no response at CQ or RQ
    # Sep 29, 2021, change to NA, was -RQValue
    return(NA)
  }
  
  # outcome 1
  # no response at RQ or CQ
  if(RQValue <= 0 && CQValue <= 0) {
    # negative logRQ ratios indicate no response at CQ or RQ
    return(0)
  }
  
  # # outcome 2
  # # insufficient response both RQ and CQ
  # if(RQValue < pneumoConstraintLow && CQValue < pneumoConstraintLow) {
  #   # if neither RQValue nor CQValue are - 
  #   # and neither RQValue nor CQ Value exceeds the constraint
  #   return(ifelse(RQValue <= 0, 0, -RQValue)) 
  #   # modified this Sep 24, 2021 # RQValue must be - to indicate a deceptive 
  #   # was return(0)
  # }
  
  # outcome 3
  # usable response at both RQ and CQ
  # if(RQValue >= pneumoConstraintLow && CQValue >= pneumoConstraintLow) {
  if(RQValue > 0 && CQValue > 0) {
    # if both RQValue and CQValue exceed the constraint
    
    # same result using these formulae
    # -log(((RQValue)^2/(CQValue)^2)^.25)
    # -log(.08^.5 / .02^.5)
    # -log(.02^.5 / .08^.5)
    # -log(((.08)^2/(.02)^2)^.25)
    # -log(((.02)^2/(.08)^2)^.25)
    
    # for pre/post RQ and CQ values
    # invert the sign so that negative (-) values correspond to deception
    # return(-log(RQValue^.5 / CQValue^.5))
    
    # for post stim RLE values
    return( log(RQValue / CQValue) )

  }
  
  # # outcome 4
  # # insufficent or negative value at RQ
  # if(RQValue < pneumoConstraintLow && CQValue >= pneumoConstraintLow) {
  #   # if the RQValue is less than the constraint, including - values
  #   # and CQValue exceeds the constraint
  #   # use the CQValue without inverting the sign
  #   return(CQValue)
  #   # -log(.045^.5 / .05^.5) = .05268026
  # }
  
  # # outcome 5
  # # insufficent or negative value at CQ
  # if(CQValue < pneumoConstraintLow && RQValue >= pneumoConstraintLow) {
  #   # if the RQValue is less than the constraint, including - values
  #   # and CQValue exceeds the constraint
  #   # use the RQValue after inverting the sign
  #   return(-RQValue)
  #   # -log(.05^.5 / .045^.5) = .05268026
  # }
  
  ###
  
  # just in case none of the preceding conditions apply
  # when there is no CQ value because there is no CQ
  return(NA)
  
}


