# R function to select the EDA and cardio response onset and peak with the max distance
# August 18, 2023
# Raymond Nelson
# abstracted from the amplitudeExtractPCFn() 
####






maxOnsetPeakDistFn <- function(tsData, 
                               xOnset, 
                               xPeak, 
                               ROWEndRow=ROWEndRow,
                               sensorName,
                               segmentName,
                               segmentTitle) {
  # R function to select the EDA and cardio response onset and peak with the max distance
  # August 18, 2023
  # Raymond Nelson
  # modified April 27, 2026 to include the descent rule
  # located in the getMaxOnsetPeakDistance.R script
  # abstracted from the amplitudeExtractPCFn() August 18, 2023
  # was previously included in the amplitudeExtractFn() in the AmplitudeExtract.R script
  # called by the amplitudeExtractFnPC() function in the amplitudeExtractPD.R script
  ####
  # tsData input is the time series data for a stimulus segment,
  # including the prestimulus and poststimulus segments.
  # time series data can be Manual or Auto EDA, and can be cardio diastolic, systolic, or mid-line
  # xOnset input is a vector of indices at which + slope segments begin
  # XPeak input is a vector of indices at which - slope segments begin
  ####
  # output is a list consisting of the onset and peak indices and respons value
  # yChangeOnset
  # yChangeOnsetValue
  # yChangePeak 
  # yChangePeakValue 
  # yChangeValue 
  ####
  
  #### exit if no xPeak or XOnset indices ####
  
  if( (length(xPeak) == 0 || all(is.na(xPeak))) || (all(length(xOnset)) == 0 || all(is.na(xOnset))) ) {
    return(list(yChangeOnset=NA, 
                yChangeOnsetValue=NA, 
                yChangePeak=NA, 
                yChangePeakValue=NA, 
                yChangeValue=NA,
                sensorName=sensorName ))
  }
  
  #### extract max distance from each xPeak to all preceding xOnset vals ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) {
    
    # for each xPeak value, calculate the max y distance
    # to all preceding xOnset value
    
    # check and fix if any xOnset indices == xPeak 
    xOnset[which(xOnset %in% xPeak)] <- xOnset[which(xOnset %in% xPeak)] - 1
    
    xPeakVals <- tsData[xPeak]
    xOnsetVals <- tsData[xOnset]
    
    # some vectors to hold the xOnset indices and values
    onsetIdx <- rep(NA, length=length(xPeak))
    onsetVals <- rep(NA, length=length(xPeak))
    
    # need these also 
    peakIdx <- rep(NA, length=length(xPeak))
    peakVals <- rep(NA, length=length(xPeak))
    
    # initialize a vector for the max y distance for xOnset to xPeak
    yDistance <- rep(NA, length=length(xPeak))
    
    # use the descentRule only for EDA data
    useDescentRule <- ifelse(sensorName %in% c("AutoEDA", "ManualEDA"),
                             descentRule,
                             FALSE)
    
  } # end if( any(!is.na(xPeak)) && any(!is.na(xOnset)) )
  
  #### iterate over xPeak to select the xOnset with the max distance to each xPeak ####
  
  # if(all(segmentTitle == "D25N073002_1_01A_3a")) {
  # if(all(sensorName == "AutoEDA", segmentName == "C6" )) {
  # if(all(segmentTitle == "D25N073002_2_02A_C6", sensorName == "AutoEDA")) {
  #   assign("xPeak", xPeak, envir=.GlobalEnv)
  #   assign("xOnset", xOnset, envir=.GlobalEnv)
  #   assign("xPeakVals", xPeakVals, envir=.GlobalEnv)
  #   assign("xOnsetVals", xOnsetVals, envir=.GlobalEnv)
  #   assign("yDistance", yDistance, envir=.GlobalEnv)
  #   assign("onsetIdx", onsetIdx, envir=.GlobalEnv)
  #   assign("onsetVals", onsetVals, envir=.GlobalEnv)
  #   assign("peakIdx", onsetIdx, envir=.GlobalEnv)
  #   assign("peakVals", onsetVals, envir=.GlobalEnv)
  #   # assign("xOnsetLoop", xOnsetLoop, envir=.GlobalEnv)
  #   # assign("xPeakLoop", xPeakLoop, envir=.GlobalEnv)
  #   assign("useDescentRule", useDescentRule, envir=.GlobalEnv)
  #   assign("tsData", tsData, envir=.GlobalEnv)
  #   assign("ROWEndRow", ROWEndRow, envir=.GlobalEnv)
  #   assign("sensorName", sensorName, envir=.GlobalEnv)
  #   assign("segmentName", segmentName, envir=.GlobalEnv)
  #   assign("segmentTitle", segmentTitle, envir=.GlobalEnv)
  #   # assign("descentRule", descentRule, envir=.GlobalEnv)
  #   # assign("descProp", descProp, envir=.GlobalEnv)
  #   plot.ts(tsData)
  #   stop("stop for inspection - getMaxOnsetPeakDistance.R")
  # }
  
  n=1
  for(n in 1:length(xPeak)) {
    
    # print(paste("xPeak n:", n, "- getMaxOnsetPeakDistance.R"))
    # assign("n", n, envir=.GlobalEnv)
    
    #### descent rule is not used ####
    
    if( !isTRUE(useDescentRule) ) {
      
      # useDescentRule is inititalized earlier in this script,
      # based on the sensorName and the desentRule parameter (boolean),
      # that is initialized in the NCCAASCII_init.R script
      
      ## when the descentRule is not used ##
      
      # increment the loop if there are no xOnset indices prior to xPeak[n]
      if( length(which(xOnset < xPeak[n])) == 0 ) next()
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
      # selected onset
      onsetIdx[n] <- xOnset[thisMax]
      onsetVals[n] <- thisOnsetVal
      # this peak
      peakIdx[n] <- xPeak[n]
      peakVals[n] <- xPeakVals[n]
      
      # next()
      
    } # end if(useDescentRule == 0)
    
    #### descent rule is used ####
    
    if( isTRUE(useDescentRule) ) {
      
      # descentRule is initialized in the NCCAASCII_init.R script
      
      # work with the xOnset and xPeak indices <= xPeak[n] 
      xOnsetLoop <- xOnset[which(xOnset < xPeak[n])]
      xPeakLoop <- xPeak[which(xPeak <= xPeak[n])]
      
      #### call the descentRuleFn() ####
      
      # this function will be called for each n iteration over the xPeak vector
      descentList <- descentRuleFn(xOnsetLoop=xOnsetLoop,
                                   xPeakLoop=xPeakLoop, 
                                   tsData=tsData, 
                                   ROWEndRow=ROWEndRow,
                                   useDescentRule=useDescentRule,
                                   sensorName=sensorName,
                                   segmentName=segmentName,
                                   segmentTitle=segmentTitle )
      
      # these are needed after calling the descentRuleFn()
      # to select the max yDistance, 
      # along with the yChangeOnset and yChangePeak indices
      onsetIdx[n] <- descentList$thisOnsetIdx
      onsetVals[n] <- descentList$thisOnsetVal
      
      peakIdx[n] <- descentList$thisXPeak
      peakVals[n] <- descentList$thisXPeakVal
      
      yDistance[n] <- descentList$thisYDistance
      
    } # end if(descentRule == 1)
    
  } # end loop n over xPeakVals
  
  # at this point yDistance is a vector of max distance vals for each xPeak 
  # and a preceeding xOnset
  # onsetIdx and onsetVals are also vectors
  
  ####  select the largest xOnset to xPeak distance ####
  
  {
    
    # get the output values using the max yDistance
    yChangeOnset <- onsetIdx[which.max(yDistance)]
    yChangeOnsetValue <- tsData[yChangeOnset]
    
    # use the peakIdx here
    yChangePeak <- peakIdx[which.max(yDistance)]
    yChangePeakValue <- tsData[yChangePeak]
    
    # yChangeValue <- yDistance[which.max(yDistance)]
    # should be the same this way
    yChangeValue <- yChangePeakValue - yChangeOnsetValue
    
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
              sensorName=sensorName, 
              segmentTitle=segmentTitle) )
  
} # end maxOnsetPeakDistFn()



