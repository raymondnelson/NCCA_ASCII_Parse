# R function to select the EDA and cardio response onset and peak with the max distance
# April 1, 2026
# replaces the maxOnsetPeakDistFn() in the getMaxOnstDistance.R script,
# to include helper functions to exclude peaks based on the degree of descent from a previous peak
# Raymond Nelson
####




newMaxOnsetPeakDistFn <- function(tsData, 
                                  xOnset, 
                                  xPeak, 
                                  onsetRow,
                                  ROWEndRow,
                                  sensorName ) {
  # R function to select the EDA and cardio response onset and peak with the max distance
  # April 1, 2026
  # replaces the maxOnsetPeakDistFn() in the getMaxOnstDistance.R script,
  # to include helper functions to exclude peaks based on the degree of descent from a previous peak
  # Raymond Nelson
  # located in the newMaxOnsetPeakDistance.R script
  # called by the amplitudeExtractFnPC() function in the amplitudeExtractPD.R script
  ####
  # tsData input is the time series data for a stimulus segment, with prestim and poststim segments
  # time series data can be Manual or Auto EDA, and can be cardio diastolic, systolic, or mid-line
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
  
  ### initialized some vectors for the onset and peak values ####

  {
    
    # check and fix if xOnset == xPeak 
    xOnset[which(xOnset %in% xPeak)] <- xOnset[which(xOnset %in% xPeak)] - 1
    
    xPeakVals <- tsData[xPeak]
    xOnsetVals <- tsData[xOnset]
    
    # some vectors to hold the xOnset indices and values
    onsetIdx <- rep(NA, length=length(xPeak))
    onsetVals <- rep(NA, length=length(xPeak))
    
    # these may not be necessary
    peakIdx <- rep(NA, length=length(xPeak))
    peakVals <- rep(NA, length=length(xPeak))
    
    # initialize a vector for the max y distance for xOnset to xPeak
    yDistance <- rep(NA, length=length(xPeak))
    
    # first make an empty vector for the loop output
    # yChange <- rep("", times=length(xOnset)) 
    
  }
  
  #### exit if no xPeak or XOnset indices ####
  
  if( (length(xPeak) == 0 || all(is.na(xPeak))) || (length(xOnset) == 0 || all(is.na(xOnset))) ) {
      
    return(list(yChangeOnset=NA, 
                yChangeOnsetValue=NA, 
                yChangePeak=NA, 
                yChangePeakValue=NA, 
                yChangeValue=NA,
                sensorName=sensorName ))
    
  }
  
  #### an evil loop to get the max onset to peak difference for each onset ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) { 
    
    # iterate over the xOnset indices to get select the xPeak with the max distance
    n=1
    #  for (n in 1:length(xOnset)) {
    for (n in 1:length(xPeak)) {
      
      # this loop will call the descentProp() helper function
      # descentProp() is in the amplitudeExtractHelperFunctions.R script
      
      # plot.ts(tsData[(xOnset[n]):length(tsData)])
      # plot.ts(tsData)
      
      # next n if there are no xOnset indices before this [n] xPeak
      if(length(which(xOnset < xPeak[n])) == 0) next()
      
      # set the stopRow to stop including xPeak values if the data descend below the xOnset[n] value
      # stopRow <- which( tsData[ c( (xOnset[n]+1):length(tsData) ) ] <= xOnsetVals[n] )[1] + xOnset[n] - 1
      # this way will include all xPeak indices befor ROWEndROW
      stopRow <- which( tsData[ c( ROWEndRow:length(tsData) ) ] <= min(xOnsetVals ))[1] + ROWEndRow - 1
      
      if(!isTRUE(descentToOriginStop)) {
        # set the stopRow to the end of the segment to disable this rule,
        # using a parameter that was initialized in the NCCAASCII_init.R script
        stopRow <- length(tsData)
        # this will force the feature extraction function to use all xPeaks in the WOE,
        # regardless of whether the data descend below the origin or low point in the ROW
      } 
      
      # there is no stop row when the data do not descend below onset, so use the last row instead
      if(is.na(stopRow)) stopRow <- length(tsData)
      
      # next n if there are no xOnset indices before the stopRow
      if(length(which(xOnset < stopRow)) == 0) next()
      
      # descentRule=0 will disable the descent rule, 
      # descentRule=1 will enable the rule for all negative slope segments after a response peak
      # descentRule=2 will keep all ascending segments and all xPeaks in the ROW, 
      # and will enable the rule only after ROWEndRow during the WOE

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
        
        # check the xOnsetVals prior to xPeak[n]
        thisMax <- which.max( xPeakVals[n] - xOnsetVals[which(xOnset < xPeak[n])] )
        thisOnsetVal <- xOnsetVals[thisMax] 
        yDistance[n] <- xPeakVals[n] - thisOnsetVal
        
        if(sign(yDistance[n]) == -1) {
          yDistance[n] <- 0
          # next() 
        }
        
        onsetIdx[n] <- xOnset[thisMax]
        onsetVals[n] <- thisOnsetVal
        # onsetVals[n] <- xOnsetVals[thisMax]
        
        
        
        # peakIdx[n] <- xPeak[n]
        # peakVals[n] <- xPeakVals[n]
        
        
        # # inititalize a vector of xPeak indices between xOnset[n] and the stopRow
        # xPeakLoop <- xPeak[which(xPeak >= xOnset[n])]
        # # xPeakLoop <- xPeakLoop[which(xPeakLoop <= stopRow)]
        #
        # if(length(xPeakLoop) == 0) next()
        #
        # # compute the peak - onset distances
        # xPeakDistances <- tsData[xPeakLoop] - tsData[xOnset[n]]
        #
        # # save the max distance
        # yDistance[n] <- xPeakDistances[which.max(xPeakDistances)]
        #
        # peakIdx[n] <- xPeakLoop[which.max(xPeakDistances)]
        # peakVals[n] <- tsData[peakIdx[n]]
        #
        # onsetIdx[n] <- xOnset[n]
        # onsetVals[n] <- tsData[onsetIdx[n]]
        
      } # end if descentRule == 0
      
      
      
      # initialize a vector of xPeak indices for each iteration of the n loop, 
      # ignore xPeak indices after the stopRow 
      # because data have descended below the value at xOnset[n]
      # use xPeak indices betweeen xOnset[n] and the stopRow
      
      
      # xPeakLoop <- xPeak[ which( xPeakVals > tsData[xOnset[n]] & xPeak <= stopRow ) ]
      # for this iteration of the loop keep only xPeak indices after xOnset[n] and before stopRow
      
      # if(length(xPeakLoop) == 0) next()
      
      # initialize the default stopRow2
      # stopRow2 is the default descent cutoff row after which xPeaks are excluded
      # initialize the stopRow2 to the length of the time series data
      # stopRow2 <- length(tsData)
      
      
      
      
      
      # if(descentRule == 2) {
      #   # rule 2 will include all positive slope segments that begin in the ROW,
      #   # and positive slope segments beginning withing the WOE after ROWEndRow,
      #   # if the data have not descended more than a proportion prop from the max peak
      #   
      #   # 10-24-2016 use the xOnset[n] only if is == or after the last xOnset before ROWEndRow
      #   
      #   # to do this locate a stopRow2 only after xPeakLoop indices after the ROW
      #   
      #   if( length(which(xPeakLoop <= ROWEndRow)) != 0 ) {
      #     # if there are any xPeakLoop indices before the end of the ROW,
      #     # get the last xPeakLoop before the ROWEndRow
      #     xPeakStart <- xPeakLoop[max(which(xPeakLoop <= ROWEndRow))]
      #     
      #     # initialize a subset of xPeakLoop values after the xPeakStart
      #     xPeakLoop2 <- xPeakLoop[xPeakLoop >= xPeakStart] # do not use >= here
      #     # not using >= means that xPeakLoop2 does not include the xPeakStart
      #     
      #     # call the descentProp function to get the stopRow2
      #     # source the amplitudeExtractHelperFunctions.R script
      #     # xPeakLoop2 includes only the last xPeak in the ROW
      #     stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop2, tsData=tsData, descProp=descProp)
      #   } else {
      #     # xPeakLoop includes all xPeaks after xOnset[n]
      #     stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=descProp)
      #     # stopRow2 <- length(tsData)
      #   }
      #   
      #   # if(xOnset[n] > xOnsetStart) {
      #   #   stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=prop, ROWEnd=ROWEndRow)
      #   # } else {
      #   #   stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=prop, ROWEnd=ROWEndRow)
      #   # }
      #   
      # } # end if descentRule == 2
      
      
      
      # if(descentRule == 1) {
      #   
      #   # descentRule == 1 will use all xOnset indices
      #   # descentProp will use the ROWEndRow from the parent env
      #   stopRow2 <- descentProp(x=xOnset[n], xPeakLoop=xPeakLoop, tsData=tsData, descProp=descProp)
      #   
      # } # end if descentRule == 1
      
      
      
      
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
      
    } # end of n loop 
    
    
    
    
    
    #### select the onset and peak with the max distance ####
    
    {
      
      # print(yDistance)
      
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
      
    }
    
  } # end if for extant xPeak and xOnset indices
  
  #### output ####
  
  # output is a list
  return(list(yChangeOnset=yChangeOnset, 
              yChangeOnsetValue=yChangeOnsetValue, 
              yChangePeak=yChangePeak, 
              yChangePeakValue=yChangePeakValue, 
              yChangeValue=yChangeValue,
              sensorName=sensorName ))
  
} # end newMaxOnsetPeakDistFn()


