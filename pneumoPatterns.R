# R script to extract pneumo response patterns
# Jan 31, 2022
# July 2025
# Raymond Nelson
# 


# requires the maxPeak() function and ratePerMin() function
# from the sigProcHelper.R script




slowingPatternFn <- function(dataVector, constrained=FALSE, constraintVal=.1) {
  # R function to compute a value for the respiration slowing response pattern
  # July 5, 2025
  # intended to mimick manual feature extraction
  # input data vector is the time series respiration data (upper or lower)
  # for 10 prestimulus seconds, 15 stimulus seconds, and 10 poststimulus seconds
  # output is a log ratio (will be 0 if no response or insufficient response)
  ##

  # get the peak indices 
  maxPeakIdx <- maxPeak(x=dataVector, y=60, firstLast=FALSE)
    
  # rate values indicate the mean number of seconds per resp cycle
  # 10 prestimulus seconds
  preRate <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 1:300)]), na.rm=TRUE) / cps
  # 10 poststimulus seconds
  postRate  <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 751:1050)]), na.rm=TRUE) / cps
  
  # combine prestim and poststim segement, weighted for the prestim seg
  prePostRate <- mean(c(preRate, preRate, postRate), na.rm=TRUE)
  
  # stim segment with 2.5 sec latency
  stimRate  <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 376:750)]), na.rm=TRUE) / cps
    
  # compute the rate change ratio, weighted for the prestimulus segment
  rateChangeRatio <- prePostRate / stimRate
  # values < 1 indicate a response
  # values > 1 indicate no response

  # calculate the slowing log rate change value 
  logRateChangeVal <-  -log(rateChangeRatio)
  # values > 0 indicate a slowing response
  # larger values indicate a greater response
	
  if(is.na(logRateChangeVal)) logRateChangeVal <- 0
    
  if(isTRUE(constrained)) {
    # remove log ratios for non-responses (values < 0)
    if(logRateChangeVal < 0) logRateChangeVal <- 0
    if(logRateChangeVal < constraintVal) {
	  # rateChange <- NULL
	  logRateChangeVal <- 0
    }
  }
    
  return(logRateChangeVal)

} # end slowingPatternFn()



amplitudePatternFn <- function(ampVector, constrained=FALSE, constraintVal=.1) {
  # R function to compute a value for the respiration amplitude response pattern
  # July 5, 2025
  # intended to mimick manual feature extraction
  # input ampVector is a vector of differences between the interpolated lines at inhalation and exhalation peaks
  # for 10 prestimulus seconds, 15 stimulus seconds, and 10 poststimulus seconds
  # output is a log ratio (will be 0 if no response or insufficient response)
  ##
   
  preAmp <- ampVector[1:300]
  postAmp <- ampVector[751:1050]
  stimAmp <- ampVector[376:750]
    
  # weighted for the prestimulus
  prePostAmp <- median(c(mean(preAmp), mean(preAmp), median(postAmp)), na.rm=TRUE)
    
  # calculate the amplitude decrease difference
  ampChange <- prePostAmp / median(stimAmp)
  # values > 1 indicate a decrease in amplitude
    
  # remove ratios for non-response segments (values < 1)
  # if(ampChange < 1) ampChange <- 0
  
  # transform to a log ratio 
  logAmpChange <- log(ampChange)
  # values > 0 indicate a slowing response
  # larger values indicate a greater response
  
  if(is.na(logAmpChange)) logAmpChange <- 0
  
  # if(ampChange >= 1) {
  #    
  # } else {
  #   logAmpChange <- 0
  # }
    
  if(isTRUE(constrained)) {
    if(logAmpChange <= 0) logAmpChangeUP <- 0
    if(logAmpChange < constraintVal) {
      # ampChange <- NULL
        logAmpChangeUP <- 0
    }
  }
    
  return(logAmpChange)
  
} # end amplitudePatternFn()



baselinePatternFn <- function(exhVector, ampVector, Q50, IQRange, constrained=FALSE, constraintVal=.1) {
# R function to compute a value for the respiration baseline response pattern
  # July 5, 2025
  # intended to mimick manual feature extraction
  
  # input exhVector is a vector of values interpolated across the exhalation peaks (thoracic and abdominal sensors)
  # for 10 prestimulus seconds, 15 stimulus seconds, and 10 poststimulus seconds
  # input ampVector is a vector of differences for the interpolated inhalation and exhalation lines
  # for 10 prestimulus seconds, 15 stimulus seconds, and 10 poststimulus seconds
  # Q50 is the 50th percentile  (nedian) of the time series data from X to XX
  # used to compute changes in baseline that are robust against y-axis location
  # IQRange is the interquartile range of the time series data from X to XX
  # used to compute changes in baseline that are robust against y-axix location
  # output is a log ratio (will be 0 if no response or insufficient response)
  ##
  
  preBase <- exhVector[1:300]
  postBase <- exhVector[751:1050]
  stimBase <- exhVector[376:750]
  
  # medAmp <- median(ampVector)
    
  # weighted for the prestimulus segment
  prePost <- mean(c(median(preBase), median(preBase), median(postBase)))
    
  # calculate the temporary increase in baseline
  prePostDiff <- Q50 - prePost
  stimBaseDiff <- Q50 - median(stimBase)
  
  baseChange <- prePostDiff - stimBaseDiff
  # values > 0 indicate a temp baseline increase
  
  # 2025Aug11 to protect against errors from a large drop in baseline 
  if((baseChange / IQRange) < -1)  return(0)
  
  # compute the log ratio
  logBaseChangeVal <- log( (baseChange / IQRange) + 1 )
  
  if(is.na(logBaseChangeVal)) logBaseChangeVal <- 0
  
  # if(baseChange < 0) baseChange <- 0
  # 
  # if(baseChange > 0) {
  #   
  # } else {
  #   logBaseChangeVal <- 0
  # }
  # # values > 0 indicate an increase in baseline
    
  if(isTRUE(constrained)) {
    if(logBaseChangeVal <= 0) logBaseChangeVal <- 0
    if(logBaseChangeVal < constraintVal) {
      # baseChange <- NULL
      logBaseChangeVal <- 0
    }
  }
   
  return(logBaseChangeVal)
	
} # end baselinePatternFn()



################################################################

################# main function #########################



pneumoPatternsFn <- function(segmentDF=segmentDF, 
                             extract.params=extract.params,
                             rateDiff=.1,
                             ampDiff=.1,
                             baseDiff=.1,
                             constrained=TRUE ) {
  # R function to extract respiration patterns
  # called by the pneumoExtractFn()
  
  # need to call this from feature extraction function instead
  
  # slowing of respiration rate
  # decrease in respiration amplitude
  # temporary increase in respiration baseline
  
  # dataVector is the input time series data
  # 10 prestimulus seconds, 15 sec stimulus segment, 10 post stimulus secs
  # inhVector is the interpolated line across inhalation peaks
  # exhVector is the interpolated line across exhalation low points
  # verbalAnswer is the location of the verbal answer
  # rateDiff is the change in respiration rate as a decimal percentage
  # ampDiff is the change in resp amplitude 
  # as a percentage of the standardized respiration amplitude
  # baseDiff is the change in respiration baseline as a percentage
  # of the standardized respiration amplitude 
  
  # will also call the pneumoMeasurementFn() in the pneumoMeasurement.R script
  # to obtain the respiration excursion measurment
  # Respiratory excursion is the sum of 
  # the absolute difference of all successive respiration samples.
  
  # output is list of 4 items: rateChange ampChange, baseChange and RLE
  # 3 respiration patterns, along with the RLE measurement
  # reduction of respiration amplitude
  # slowing of respiration rate
  # temporary increase in respiration baseline
  
  ####
  
  { 
    ## segment info ##
    examName <- segmentDF$examName[301]
    seriesName <- segmentDF$seriesName[301]
    chartName <- segmentDF$chartName[301]
    eventLabel <- segmentDF$eventLabel[301]
    
    segInfo <- c(examName, seriesName, chartName, eventLabel)
  }
  
  if(any(any(segmentDF$c_UPneumoMid > segmentDF$c_UPneumo_Q75),
         any(segmentDF$c_LPneumoMid > segmentDF$c_LPneumo_Q75),
         any(segmentDF$c_UPneumoMid < segmentDF$c_UPneumo_Q25),
         any(segmentDF$c_LPneumoMid < segmentDF$c_LPneumo_Q25))) {
    ## no pattern extraction if the data are unstable ##
    # respiration mid line exceeds the interquartile range
    # for either the thoracic or abdominal sensor
    outputVector <- c(segInfo, rep(0, times=8))
    headerNames <- c("examName", "seriesName", "chartName", "eventLabel")
    patternNames <- c("rateChangeUP", "rateChangeLP", "ampChangeUP", "ampChangeLP", "baseChangeUP", "baseChangeLP", "RLEUp", "RLELp")
    names(outputVector) <- c(headerNames, patternNames)
                             
    return(outputVector)
  }
  
  {
	  # ratio constraints
    if(!exists("rateDiff")) rateDiff=.1
    if(!exists("ampDiff")) ampDiff=.1
    if(!exists("baseDiff")) baseDiff=.1
    if(!exists("constrained")) constrained=TRUE
  }
  
  {
    dataVectorUP <- segmentDF$c_UPneumoSm
    inhVectorUP <- segmentDF$c_UPneumoInh
    exhVectorUP <- segmentDF$c_UPneumoExh
    
    # amplitude vector for upper respiration sensor
	  ampVectorUP <- inhVectorUP - exhVectorUP
    
    dataVectorLP <- segmentDF$c_LPneumoSm
    inhVectorLP <- segmentDF$c_LPneumoInh
    exhVectorLP <- segmentDF$c_LPneumoExh
	
    # amplitude vector for lower respiration sensor
	  ampVectorLP <- inhVectorLP - exhVectorLP
	  
	  # median for the time series from X to XX
	  Q50Up <- segmentDF$c_UPneumo_Q50[1]
	  Q50Lp <- segmentDF$c_UPneumo_Q50[1]
	  
	  # interquartile range
	  IQRangeUp <- segmentDF$c_UPneumo_Q75[1] - segmentDF$c_UPneumo_Q25[1]
	  IQRangeLp<- segmentDF$c_LPneumo_Q75[1] - segmentDF$c_LPneumo_Q25[1]
    
    answerRow <- extract.params$answer
    verbalAnswer <- verbalAnswer <- answerRow-(prestimSeg*cps)
  }
  
  {
    if(rateDiff < 1) {
      rateChangeConstraint <- log(rateDiff + 1)
    } else {
      rateChangeConstraint <- log(rateDiff)
    }
    if(ampDiff < 1) {
      ampChangeConstraint <- log(ampDiff + 1)
    } else {
      ampChangeConstraint <- log(ampDiff)
    }
    if(baseDiff < 1) {
      baseChangeConstraint <- log(baseDiff + 1)
    } else {
      baseChangeConstraint <- log(baseDiff)
    }
  }
  
  #### initialize the output ####

  {
    outputVectorUP <- NULL
    outputVectorLP <- NULL
  }
  
  #### compute the change in respiration rate - upper ####
  
  {
    rateChangeUP <- slowingPatternFn(dataVector=dataVectorUP, 
                                     constrained=constrained, 
                                     constraintVal=rateChangeConstraint)
  }
  
  #### compute the change in respiration rate - lower ####
  
  {
    rateChangeLP <- slowingPatternFn(dataVector=dataVectorLP, 
                                     constrained=constrained, 
                                     constraintVal=rateChangeConstraint)
  }
  
  #### compute the change in respiration amplitude - upper ####
  
  {
    logAmpChangeUP <- amplitudePatternFn(ampVector=ampVectorUP, 
                                         constrained=constrained, 
                                         constraintVal=ampChangeConstraint)

  }
  
  #### compute the change in respiration amplitude - lower ####
  
  {
    logAmpChangeLP <- amplitudePatternFn(ampVector=ampVectorLP, 
                                         constrained=constrained, 
                                         constraintVal=ampChangeConstraint)
  }
  
  #### compute the change in respiration baseline - upper #### 
  
  {
    logBaseChangeUP <- baselinePatternFn(exhVector=exhVectorUP, 
                                         ampVector=ampVectorUP, 
                                         Q50=Q50Up,
                                         IQRange=IQRangeUp,
                                         constrained=constrained, 
                                         constraintVal=baseChangeConstraint)
  }

  #### compute the change in respiration baseline - lower #### 
  
  {
    logBaseChangeLP <- baselinePatternFn(exhVector=exhVectorLP, 
                                         ampVector=ampVectorLP, 
                                         Q50=Q50Lp,
                                         IQRange=IQRangeLp,
                                         constrained=constrained, 
                                         constraintVal=baseChangeConstraint)
  }
  
  #### calculate the RLE measurement - upper ####
  
  {
    RLEUp <- pneumoMeasurementFn(dataVector=dataVectorUP[301:750], 
                               verbalAnswer=verbalAnswer)
    # pneumoFEFactor is initialized in the NCCAASCII_init.R script
    RLEUp <- round((RLEUp*pneumoFEFactor),2)
  }
  
  #### calculate the RLE measurement - lower ####
  
  {
    RLELp <- pneumoMeasurementFn(dataVector=dataVectorLP[301:750], 
                                 verbalAnswer=verbalAnswer)
    # pneumoFEFactor is initialized in the NCCAASCII_init.R script
    RLELp <- round((RLELp*pneumoFEFactor),2)
  }
  
  #### output ####
  
  {
    headerInfo <- c("examName", "seriesName", "chartName", "eventLabel")
    
    # outputVectorUPi <- c(segInfo, "UPneumo", outputVectorUP)
    # outputVectorLPi <- c(segInfo, "LPneumo", outputVectorLP)
    
    outputVector <- c(segInfo,
                      round(as.numeric(rateChangeUP), 3), 
                      round(as.numeric(rateChangeLP), 3), 
                      round(as.numeric(logAmpChangeUP), 3), 
                      round(as.numeric(logAmpChangeLP), 3), 
                      round(as.numeric(logBaseChangeUP), 3), 
                      round(as.numeric(logBaseChangeLP), 3), 
                      round(as.numeric(RLEUp), 3),
                      round(as.numeric(RLELp), 3) )
    
    patternNames <- c("rateChangeUP", "rateChangeLP", "ampChangeUP", "ampChangeLP", "baseChangeUP", "baseChangeLP", "RLEUp", "RLELp")
    
    names(outputVector) <- c(headerInfo, patternNames)
    
    outputVector <- rbind.data.frame(outputVector)
    
    names(outputVector) <- c(headerInfo, patternNames)
    
  }
  
  return(outputVector)
  
} # end pneumoPatternsFn() function




