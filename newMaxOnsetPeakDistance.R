



newMaxOnsetPeakDistFn <- function(tsData, 
                                  xOnset, 
                                  xPeak, 
                                  ROWEndRow,
                                  sensorName ) {
  # R function to select the EDA and cardio response onset and peak with the max distance
  # located in the newMaxOnsetPeakDistance.R script
  # was previously included in the amplitudeExtractFn() in the AmplitudeExtract.R script
  # April 1, 2026
  # Raymond Nelson
  # replaces the maxOnsetPeakDistFn() in the getMaxOnstDistance.R script,
  # to include the descentRule parameter that is initialized in the NCCASCII_init.R script
  # called by the amplitudeExtractPCFn()
  ####
  # tsData input is the time series data for a stimulus segment, with prestim and poststim segments
  # xOnset input is a vector of indices at which + slope segments begin
  # XPeak input is a vector of indices at which - slope segments begin
  # sensorName is passed to another funtion to normalize the scale of the measured response
  ####
  # output is a list consisting of the onset and peak indices and respons value
  # yChangeOnset
  # yChangeOnsetValue
  # yChangePeak 
  # yChangePeakValue 
  # yChangeValue 
  ####

  # xPeak rows may be excluded depending on how the data descend prior to a peak after a previous peak
  
  {
    
    xPeakVals <- tsData[xPeak]
    xOnsetVals <- tsData[xOnset]
    
    # some vectors to hold the xOnset indices and values
    onsetIdx <- rep(NA, length=length(xOnset))
    onsetVals <- rep(NA, length=length(xOnset))
    
    peakIdx <- rep(NA, length=length(xOnset))
    peakVals <- rep(NA, length=length(xOnset))
    
    # initialize a vector for the max y distance for xOnset to xPeak
    yDistance <- rep(NA, length=length(xPeak))
    
    # first make an empty vector for the loop output
    # yChange <- rep("", times=length(xOnset)) 
    
  }
  
  #### an evil loop to get the max onset to peak difference for each onset ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) { 
    
    # iterate foward over the xOnset indices
    n=1
    for (n in 1:length(xOnset)) {
      # use a loop to iteratively shorten the comparison of peak values
      # if the data descend below onset value,
      # or if the data descend by more than a proportion (descProp),
      # within a - slope segment after the peak of a + slope segment
      
      # this loop will call the descentProp() helper function
      # descentProp() is in the amplitudeExtractHelperFunctions.R script
      
      # plot.ts(tsData[(xOnset[n]):length(tsData)])
      # plot.ts(tsData)
      
      # set the stopRow to stop including xPeak values if the data descend below the xOnset[n] value
      stopRow <- which( tsData[ c( (xOnset[n]+1):length(tsData) ) ] <= xOnsetVals[n] )[1] + xOnset[n] - 1
      # there is no stop row when the data do not descend below onset, so use the last row instead
      if(is.na(stopRow)) stopRow <- length(tsData)
      
      tsData[((xOnset[n]):(xOnset[n]+1+10))] <= xOnsetVals[n]
      
      # initialize a vector of xPeak indices for each iteration of the n loop, 
      # ignore xPeak indices after the stopRow 
      # because data have descended below the value at xOnset[n]
      # use xPeak indices betweeen xOnset[n] and the stopRow
      xPeakLoop <- xPeak[ which( xPeakVals > tsData[xOnset[n]] & xPeak <= stopRow ) ]
      # for this iteration of the loop keep only xPeak indices after xOnset[n] and before stopRow
      
      if(length(xPeakLoop) == 0) next()
      
      # initialize the default stopRow2
      # stopRow2 is the default descent cutoff row after which xPeaks are excluded
      # initialize the stopRow2 to the length of the time series data
      stopRow2 <- length(tsData)
      # this stopRow2 will be used when descentRule = 0
      
      # descentRule=0 will disable the descent rule, 
      # all peaks in the EW are used until the data descend below the y-axis value at xOnset[n]
      # descentRule=1 will enable the rule for all negative slope segments after a response peak
      # descentRule=2 will enable the rule only after ROWEndRow during the EW,
      # keeping all ascending segments during the ROW
      
      # plot.ts(tsData)
      
      # # initialize the xOnsetStart to compute the descent distance/proportion
      # xOnsetStart <- length(tsData) - 1
      # # this will be adjusted next
      # 
      # # locate the min xOnset before ROWEndRow
      # # this seems only to apply to descentRule==2
      # if( any(xOnset <= ROWEndRow) ) {
      #   # xOnsetStart is the min xOnset in the ROW
      #   xOnsetStart <- xOnset[ which.min(tsData[xOnset[which(xOnset <= ROWEndRow)]]) ]
      #   # xOnsetStart <- xOnset[ max(which(xOnset <= ROWEndRow)) ]
      #   
      #   # xPeakStart <- xPeak[max(which(xPeak <= ROWEndRow))]
      #   
      # } 
      
      
      if(descentRule == 0) {
        # the descentRule parameter in the init script     
        # descentRule 0 will disable the rule
        # and will only exclude xPeakLoop indices after the data have descended below the response onset yValue
        
        # inititalize a vector of xPeak indices between xOnset[n] and the stopRow
        xPeakLoop <- xPeak[which(xPeak >= xOnset[n])]
        # xPeakLoop <- xPeakLoop[which(xPeakLoop <= stopRow)]
        
        if(length(xPeakLoop) == 0) next()
        
        # compute the peak - onset distances
        xPeakDistances <- tsData[xPeakLoop] - tsData[xOnset[n]]
        
        # save the max distance
        yDistance[n] <- xPeakDistances[which.max(xPeakDistances)]
        
        peakIdx[n] <- xPeakLoop[which.max(xPeakDistances)]
        peakVals[n] <- tsData[peakIdx[n]]
        
        onsetIdx[n] <- xOnset[n]
        onsetVals[n] <- tsData[onsetIdx[n]]
        
      } # end if descentRule == 0
      
      
      if(descentRule == 2) {
        # rule 2 will include all positive slope segments that begin in the ROW,
        # and positive slope segments beginning withing the WOE after ROWEndRow,
        # if the data have not descended more than a proportion prop from the max peak
        
        # 10-24-2016 use the xOnset[n] only if is == or after the last xOnset before ROWEndRow
        
        # to do this locate a stopRow2 only after xPeakLoop indices after the ROW
        
        if( length(which(xPeakLoop <= ROWEndRow)) != 0 ) {
          # if there are any xPeakLoop indices before the end of the ROW,
          # get the last xPeakLoop before the ROWEndRow
          xPeakStart <- xPeakLoop[max(which(xPeakLoop <= ROWEndRow))]
          
          # initialize a subset of xPeakLoop values after the xPeakStart
          xPeakLoop2 <- xPeakLoop[xPeakLoop >= xPeakStart] # do not use >= here
          # not using >= means that xPeakLoop2 does not include the xPeakStart
          
          # call the descentProp function to get the stopRow2
          # source the amplitudeExtractHelperFunctions.R script
          # xPeakLoop2 includes only the last xPeak in the ROW
          stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop2, tsData=tsData, descProp=descProp)
        } else {
          # xPeakLoop includes all xPeaks after xOnset[n]
          stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=descProp)
          # stopRow2 <- length(tsData)
        }
        
        # if(xOnset[n] > xOnsetStart) {
        #   stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=prop, ROWEnd=ROWEndRow)
        # } else {
        #   stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=prop, ROWEnd=ROWEndRow)
        # }
        
      } # end if descentRule == 2
      
      
      
      if(descentRule == 1) {
        
        # descentRule == 1 will use all xOnset indices
        # descentProp will use the ROWEndRow from the parent env
        stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=descProp)
      
      } # end if descentRule == 1
      
      
      
      
      # 2026Apr02 commented out RN
      # # locate the stopRow2 for the descentRule
      # 
      # 
      # # stopRow2 was already initialized to length(tsData)
      # 
      # # use the xOnset[n] and stopRow2, 
      # # to keep items in the xPeak vector for each xOnset[n] in the loop
      # xPeakLoop <- xPeakLoop[which((xPeakLoop > xOnset[n]) & (xPeakLoop <= stopRow2))]
      # 
      # # use the xPeakLoop vector to determine the max xPeak-xOnset for each xOnset
      # if(length(xPeakLoop) > 0) {  
      #   # use XPeakLoop to select the max distance to xOnsetVals[n]
      #   # that gives the tsData row index for the max change for each xOnset values
      #   peakIdx[n] <- xPeakLoop[which.max(tsData[xPeakLoop[xPeakLoop >= xOnset[n]]] - xOnsetVals[n])]
      #   
      #   # yDistance is a vector of NAs that was initialized before the loop
      #   yDistance[n] <- tsData[peakIdx[n]]
      #   # yDistance[n] <- xPeakLoop[which.max(tsData[xPeakLoop[xPeakLoop >= xOnset[n]]] - xOnsetVals[n])]
      #   # yDistance will remain NA if no response is extracted 
      # }
      # # loop output is a vector 'yDistance' to index the peak row in the tsData for the max response for each xOnset index
      
    } # end of evil n loop to select the max change for each xOnset row
    
    
    
    
    # print(yDistance)
    
    # at this point yDistance is a vector of max distance vals for each xPeak 
    # and a preceeding xOnset
    # onsetIdx and onsetVals are also vectors
    
    # get the output values using the max yDistance
    yChangeOnset <- onsetIdx[which.max(yDistance)]
    yChangeOnsetValue <- tsData[yChangeOnset]
    
    yChangePeak <- peakIdx[which.max(yDistance)]
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
                yChangeValue=NA))
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
  
}


