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
  # August 18, 2023
  # Raymond Nelson
  #
  # abstracted from the amplitudeExtractPCFn()
  # called by the amplitudeExtractPCFn()
  #
  # tsData input is the time series data for a stimmulus segment
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
  #
  ####
  
  #### extract max distance from each xPeak to all preceding xOnset vals ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) {
    # for each xPeak value, calculate the max y distance
    # to all preceding xOnset value
    
    # check and fix if xOnset == xPeak 
    xOnset[which(xOnset %in% xPeak)] <- xOnset[which(xOnset %in% xPeak)] - 1
    
    xPeakVals <- tsData[xPeak]
    xOnsetVals <- tsData[xOnset]
    
    # initialize a vector for the max y distance for xOnset to xPeak
    yDistance <- rep(NA, length=length(xPeak))
    
    # some vectors to hold the xOnset indices and values
    onsetIdx <- rep(NA, length=length(xPeak))
    onsetVals <- rep(NA, length=length(xPeak))
    
    # iterate over the xPeaks to get the max distance to a preceding xOnset
    n=1
    for(n in 1:length(xPeakVals)) {
      # increment the loop if no xOnset prior to xPeak[n]
      if(length(which(xOnset < xPeak[n])) == 0) next()
      # check the xOnsetVals prior to xPeak[n]
      thisMax <- 
        which.max( xPeakVals[n] - xOnsetVals[which(xOnset < xPeak[n])] )
      thisOnsetVal <- xOnsetVals[thisMax] # simpler and works the same
      yDistance[n] <- xPeakVals[n] - thisOnsetVal
      # increment the loop if the distance is negative
      if(sign(yDistance[n]) == -1) {
        yDistance[n] <- 0
        # next() # commented out 8/31/2020 10:04pm
      }
      onsetIdx[n] <- xOnset[thisMax]
      onsetVals[n] <- thisOnsetVal
    } # end loop n over xPeakVals
    
    # at this point yDistance is a vector of max distance vals for each peak
    # onsetIdx and onsetVals are vectors
    # for the distance from each peak to each preceding onset 
    
    # get the output values using the max yDistance
    yChangeOnset <- onsetIdx[which.max(yDistance)]
    yChangeOnsetValue <- tsData[yChangeOnset]
    
    yChangePeak <- xPeak[which.max(yDistance)]
    yChangePeakValue <- tsData[yChangePeak]
    
    yChangeValue <- yChangePeakValue - yChangeOnsetValue
  } else {
    return(list(yChangeOnset=NA, 
                yChangeOnsetValue=NA, 
                yChangePeak=NA, 
                yChangePeakValue=NA, 
                yChangeValue=NA))
  }
  
  #### Nov 17, 2023 abstract the measured value from the display gain ####
  
  if(all(useGainCorrection, !is.na(yChangeValue), !is.null(yChangeValue), yChangeValue!=0, yChangeValue!="")) {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/abstractScale.R", echo=TRUE)
    
    yChangeValue <- abstractScaleFn(x=yChangeValue, sensorName=sensorName)
    
  }
  
  
  # output is a list
  return(list(yChangeOnset=yChangeOnset, 
              yChangeOnsetValue=yChangeOnsetValue, 
              yChangePeak=yChangePeak, 
              yChangePeakValue=yChangePeakValue, 
              yChangeValue=yChangeValue))
  
} # end maxOnsetPeakDistFn()



