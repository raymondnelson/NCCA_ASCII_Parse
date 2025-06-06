# R functions to get an integer score from RQ and CQ values
# for respiration, EDA and cardio scores

resp_RC_Fn.R <- function(RQValue, 
                         CQValue, 
                         respConstraintInner=log(1.25), 
                         respConsntraintOuter=log(1.5) ) {
  # R function to get an integer score for RQ and CQ values
  ### pneumo ###
  thisScore <- log(RQValue / CQValue)
  if(thisScore >= respConstraintInner && 
     thisScore <= respConsntraintOuter ) { 
    return(1)
  } else if(thisScore <= -respConstraintInner && 
            thisScore >= -respConsntraintOuter ) {
    return(-1)
  } 
  return(0)
}


EDA_RC_Fn.R <- function(RQValue, CQValue, EDAConstraint=log(1.05)) {
  # R function to get an integer score for RQ and CQ values
  ### EDA ###
  thisScore <- log(RQValue / CQValue)
  if(thisScore >= EDAConstraint) {
    return(2)
  } else if(thisScore <= -EDAConstraint) {
    return(-2)
  } 
  return(0)
}


cardio_RC_Fn.R <- function(RQValue, CQValue, cardioConstraint=log(1.05)) {
  # R function to get an integer score for RQ and CQ values
  ### cardio ###
  thisScore <- log(RQValue / CQValue)
  if(thisScore >= cardioConstraint) {
    return(1)
  } else if(thisScore <= -cardioConstraint) {
    return(-1)
  } 
  return(0)
}


vasomotor_RC_Fn <- function(RQValue, CQValue, PLEConstraint=log(1.05)) {
  # R function to get an integer score for RQ and CQ values
  ### PLE ###
  # PLE values are log(pre/post) ratios
  ## outcome 1 ##
  # no response at RQ or CQ
  if(RQValue <= 0 && CQValue <= 0) {
    # negative logRQ ratios indicate no response at CQ or RQ
    return(0)
  }
  ## outcome 2 ##
  # insufficient response both RQ and CQ
  if(RQValue < PLEConstraint && CQValue < PLEConstraint) {
    # if neither RQValue nor CQValue are - 
    # and neither RQValue nor CQ Value exceeds the constraint
    return(0)
  }
  ## outcome 3 ##
  # usable response at both RQ and CQ
  if(RQValue >= PLEConstraint && CQValue >= PLEConstraint) {
    # if both RQValue and CQValue exceed the constraint
    # invert the sign so the - values correspond to deception
    thisScore <- -log(RQValue^.5 / CQValue^.5)
    # same result using these formulae
    # -log(((RQValue)^2/(CQValue)^2)^.25)
    # -log(.08^.5 / .02^.5)
    # -log(.02^.5 / .08^.5)
    # -log(((.08)^2/(.02)^2)^.25)
    # -log(((.02)^2/(.08)^2)^.25)
    if(thisScore >= PLEConstraint) {
      return(1)
    } else if(thisScore <= -PLEConstraint) {
      return(-1)
    } else {
      return(0)
    }
  }
  ## outcome 4 ##
  # insufficent or negative value at RQ
  if(RQValue < PLEConstraint && CQValue >= PLEConstraint) {
    # if the RQValue is less than the constraint, including - values
    # and CQValue exceeds the constraint
    # use the CQValue without inverting the sign
    thisScore <- CQValue
    if(thisScore >= PLEConstraint) {
      return(1) 
    } else {
      return(0)
    }
  }
  ## outcome 5 ##
  # insufficent or negative value at CQ
  if(CQValue < PLEConstraint && RQValue >= PLEConstraint) {
    # if the RQValue is less than the constraint, including - values
    # and CQValue exceeds the constraint
    # use the RQValue after inverting the sign
    thisScore <- RQValue
    if(thisScore >= PLEConstraint) {
      return(-1) 
    } else {
      return(0)
    }
  }
  ## just in case none of the preceding conditions apply ##
  return(0)
}



