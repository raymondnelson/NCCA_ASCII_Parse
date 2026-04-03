# R function to select the EDA and cardio response onset and peak with the max distance
# August 18, 2023
# Raymond Nelson
#
# abstracted from the amplitudeExtractPCFn()
#
#





maxOnsetPeakDistFn <- function(tsData, 
                               xOnset, 
                               xPeak, 
                               sensorName ) {
  # R function to select the EDA and cardio response onset and peak with the max distance
  # located in the getMaxOnsetPeakDistance.R script
  # abstracted from the amplitudeExtractPCFn()
  # August 18, 2023
  # Raymond Nelson
  # called by the amplitudeExtractPCFn()
  ####
  # tsData input is the time series data for a stimulus segment
  # including the prestimulus and poststimulus segments 
  #
  # xOnset input is a vector of indices at which + slope segments begin
  # XPeak input is a vector of indices at which - slope segments begin
  #
  # output is a list consisting of the onset and peak indices and respons value
  # yChangeOnset
  # yChangeOnsetValue
  # yChangePeak 
  # yChangePeakValue 
  # yChangeValue 
  ####
  
  #### extract max distance from each xPeak to all preceding xOnset vals ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) {
    # for each xPeak value, calculate the max y distance
    # to all preceding xOnset value
    
    # check and fix if xOnset == xPeak 
    xOnset[which(xOnset %in% xPeak)] <- xOnset[which(xOnset %in% xPeak)] - 1
    
    xPeakVals <- tsData[xPeak]
    xOnsetVals <- tsData[xOnset]
    
    # some vectors to hold the xOnset indices and values
    onsetIdx <- rep(NA, length=length(xPeak))
    onsetVals <- rep(NA, length=length(xPeak))
    
	# initialize a vector for the max y distance for xOnset to xPeak
    yDistance <- rep(NA, length=length(xPeak))
    
    # iterate over the xPeaks to get select the preceding xOnset with the max distance
    n=1
    for(n in 1:length(xPeakVals)) {
      # increment the loop if there are no xOnset indices prior to xPeak[n]
      if(length(which(xOnset < xPeak[n])) == 0) next()
      # check the xOnsetVals prior to xPeak[n]
      thisMax <- which.max( xPeakVals[n] - xOnsetVals[which(xOnset < xPeak[n])] )
      # thisOnsetVal <- xOnsetVals[which(xOnset < xPeak[n])][thisMax] 
      thisOnsetVal <- xOnsetVals[thisMax] # simpler and works the same
      yDistance[n] <- xPeakVals[n] - thisOnsetVal
      # increment the loop if the distance is negative
      if(sign(yDistance[n]) == -1) {
        yDistance[n] <- 0
        # next() # commented out 8/31/2020 10:04pm
      }
      onsetIdx[n] <- xOnset[thisMax]
      onsetVals[n] <- thisOnsetVal
      
      # 2026Mar30
      # possibly include the descentRule here
      # if(isTRUE(descentRule)) {
      #   # descentRule is a boolean initialized in the NCCAASCII_init.R script
      #   # descProp is a decimal proportion (.632 = 1 time constant) initialized in the NCCASASCII_init.R script
      #   
      #   # a scalar initiated in the NCCAASCII_init.R script
      #   # descProp
      #   
      #   # a function
      #   # descentProp
      #   
      # }
      
    } # end loop n over xPeakVals
    
    # at this point yDistance is a vector of max distance vals for each xPeak 
    # and a preceeding xOnset
    # onsetIdx and onsetVals are also vectors
    
    # get the output values using the max yDistance
    yChangeOnset <- onsetIdx[which.max(yDistance)]
    yChangeOnsetValue <- tsData[yChangeOnset]
    
    yChangePeak <- xPeak[which.max(yDistance)]
    yChangePeakValue <- tsData[yChangePeak]
    
    # yChangeValue <- yDistance[which.max(yDistance)]
    # should be the same this way
    yChangeValue <- yChangePeakValue - yChangeOnsetValue
	
  } else {
    # return NA if there are no xPeaks or xOnset indices
    return(list(yChangeOnset=NA, 
                yChangeOnsetValue=NA, 
                yChangePeak=NA, 
                yChangePeakValue=NA, 
                yChangeValue=NA, 
                sensorName=sensorName ))
  }
  
  #### Nov 17, 2023 abstract the measured value from the display gain ####
  
  if(all(useGainCorrection, !is.na(yChangeValue), !is.null(yChangeValue), !is.na(yChangeValue), yChangeValue!=0, yChangeValue!="")) {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/abstractScale.R", echo=TRUE)
    
    yChangeValue <- abstractScaleFn(x=yChangeValue, sensorName=sensorName)
    
  }
  
  #### output ####
  
  # output is a list
  return(list(yChangeOnset=yChangeOnset, 
              yChangeOnsetValue=yChangeOnsetValue, 
              yChangePeak=yChangePeak, 
              yChangePeakValue=yChangePeakValue, 
              yChangeValue=yChangeValue,
              sensorName=sensorName ))
  
} # end maxOnsetPeakDistFn()



