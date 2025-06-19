# R function to parse the PLE logRC score
# Raymond Nelson
# 12-5-2017
# 9-24-2021
# 11-20-2021
# Feb 25, 2022 return 0 if both RQ and CQ values exceed the constraint


PLE_RC_Fn <- function(RQValue, CQValue, PLEConstraint) {
  # R function to compute the logRC score for PLE data
  # called by the RCScoresFn() in the RCScores.R script
  #
  # PLE response feature is the log(pre/post) ratio
  # mean amplitude for 3 prestim seconds 
  # divided by the mean amplitude for 5 poststim seconds
  # with a 5 second latency after stimulus onset
  #
  # RQValue and CQValue inputs are log(pre/post) ratios
  # PLEConstraint is initialized inthe NCCAASCII_init.R script
  #
  # output is the log(R/C) ratio
  #
  # use of the log is important because pre/post ratios are asymmetrical
  # bounded by 0 and infinity with a mean of 1
  # 
  # logRC ratios have + and - value
  # log rations
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
  # according to 5 possibilities 
  
  # Sep 24, 2021
  # adjust missing values
  if(any(is.na(RQValue), RQValue=="", is.null(RQValue))) {
    RQValue <- 0
  }
  if(any(is.na(CQValue), CQValue=="", is.null(CQValue))) {
    CQValue <- 0
  }
  
  # exit if either RQValue or CQValue is NA or missing
  if(any(is.na(RQValue), is.na(CQValue), RQValue=="", CQValue=="", is.null(RQValue), is.null(CQValue))) {
    return(0)
  }
  
  RQValue <- round(RQValue, 3)
  CQValue <- round(CQValue, 3)
  # PLEConstraint <- round(PLEConstraint, 3)
  
  # outcome 1
  # no response at RQ or CQ
  if(RQValue <= 0 && CQValue <= 0) {
    # negative logRQ ratios indicate no response at CQ or RQ
    return(0)
  }
  
  # outcome 2
  # insufficient response both RQ and CQ
  if(RQValue < PLEConstraint && CQValue < PLEConstraint) {
    # if neither RQValue nor CQValue are - 
    # and neither RQValue nor CQ Value exceeds the constraint
    return(ifelse(RQValue <= 0, 0, -RQValue)) 
    # sep 24, 2021 changed this to -RQValue
    # was return(0)
  }
  
  # outcome 3
  # usable response at both RQ and CQ
  if(RQValue >= PLEConstraint && CQValue >= PLEConstraint) {
    # if both RQValue and CQValue exceed the constraint
    
    # same result using these formulae
    # -log(((RQValue)^2/(CQValue)^2)^.25)
    # -log(.08^.5 / .02^.5)
    # -log(.02^.5 / .08^.5)
    # -log(((.08)^2/(.02)^2)^.25)
    # -log(((.02)^2/(.08)^2)^.25)
    
    # Feb 25, 2022 return 0 if both RQ and CQ have a usable reactions
    # return(0)
    # this implements the "something vs something is nothing" concept
    # for analysis only
    
    # July 27, 2023
    # Execute the "something vs something is nothing" rule for the PLE
    if(PLESomethingVsSomethingIsNothing) {
      # use an environment parameter set in the NCCAASCII_init.R script
      return(0)
      # use the formula if this is FALSE
    }
	
    # this method compares the strength of RQ/CQ reaction to make the result
    # invert the sign so that - values correspond to deception
    return(-log(RQValue^.5 / CQValue^.5))
  }
  
  # outcome 4
  # insufficient or negative value at RQ
  if(RQValue < PLEConstraint && CQValue >= PLEConstraint) {
    # if the RQValue is less than the constraint, including - values
    # and CQValue exceeds the constraint
    # use the CQValue without inverting the sign
    return(CQValue)
	# example
	# -log(.045^.5 / .05^.5) = -.05268026
  }
  
  # outcome 5
  # insufficient or negative value at CQ
  if(CQValue < PLEConstraint && RQValue >= PLEConstraint) {
    # if the CQValue is less than the constraint, including - values
    # and QQValue exceeds the constraint
    # use the RQValue after inverting the sign
    return(-RQValue)
  }

  # just in case none of the preceding conditions apply
  # when there is no CQ value because there is no CQ
  return(NA)
}
