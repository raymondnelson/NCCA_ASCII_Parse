# R script to extract pneumo response patterns
# Jan 31, 2022
# Raymond Nelson
# 

# maxPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
#   # function to get the cyclic peaks from the time series data
#   # will keep the index number of max peak samples
#   # x input is a time series vector
#   # y input is the number of offset samples 
#   # firstLast will keep or exclude the first and last samples
#   ###
#   # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
#   xOut <- rep(NA, times=(length(x)))
#   if(firstLast==TRUE){
#     xOut[1] <- 1 # keep the first
#     xOut[length(xOut)] <- length(xOut) # keep the last
#   }
#   # buffer will be double the offset value
#   input_buffer <- x[2:(2*y+1)]
#   for (i in 2:(length(x)-(2*y))) {
#     input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
#     # check to see if the middle value of the buffer is the max
#     ifelse(input_buffer[(y+1)]==max(input_buffer),
#            xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
#            # work on this 4/23/2016
#            # another way is to
#            # create a buffer of NA and keep only the i+y+1 set to the row index
#            # slice the buffer into the vector 
#            # the result should be the same
#            # non max samples in the buffer are NA
#            next()
#     )
#   } # end for loop
#   return(as.numeric(na.omit(xOut)))
# }




# ratePerMin <- function(x=chartDF$c_UPneumo, buffer=42, peaks="upper", lowPass=FALSE) {
#   # function to calculate the cyclic rate per minute
#   # for cardio and pneumo time series data
#   # x input is a a time series vector
#   # peaks input is a switch to choose "upper" or "lower" peaks
#   # buffer input is the number of pre and post index samples to include in the search space
#   # buffer = 40 for pneumo
#   # buffer = 9 for cardio
#   # output is the mean rate 
#   ####
#   # first smooth the input to remove artifact peaks
#   if(lowPass == TRUE) { 
#     x1 <- lowPass2hz.2nd(x) 
#   } else {
#     x1 <- x
#   }
#   # then get the max or min peak
#   ifelse(peaks=="lower",
#          Peaks <- minPeak(x=x1, y=buffer, firstLast=FALSE),
#          Peaks <- maxPeak(x=x1, y=buffer, firstLast=FALSE) 
#   )  
#   # calculate the rate
#   
#   peakDiffs <- diff(Peaks)
#   # remove peak diffs of 1 because these occur when the cardio cuff is deflated
#   # peak diffs of 1 will distort the cardio rate measurement
#   peakDiffs <- peakDiffs[which(peakDiffs != 1)]
#   
#   rateMin <- ifelse(length(peakDiffs)>1,
#                     60 / (mean(peakDiffs) / cps ),
#                     # 60/1/cps
#                     60 / peakDiffs / cps
#   )
#   return( round(rateMin , 2) )
# }







pneumoPatternsFn <- function(segmentDF=segmentDF, 
                             extract.params=extract.params,
                             # dataVector, 
                             # inhVector,
                             # exhVector,
                             # verbalAnswer, 
                             rateDiff=.05,
                             ampDiff=.05,
                             baseDiff=.05,
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
  
  # output is list of 4 items: rRate rBase rAmp and RLE
  # 3 respiration patterns, along with the RLE measurement
  # reduction of respiration amplitude
  # slowing of respiration rate
  # temporary increase in respiration baseline
  
  ####
  
  {
    integerScores <- FALSE
    
    if(!exists("rateDiff")) rateDiff=.05
    if(!exists("ampDiff")) ampDiff=.05
    if(!exists("baseDiff")) baseDiff=.05
    if(!exists("constrained")) constrained=FALSE
    
    # rateDiff=.025
    # ampDiff=.025
    # baseDiff=.025
  }
  
  {
    dataVectorUP <- segmentDF$c_UPneumoSm
    inhVectorUP <- segmentDF$c_UPneumoInh
    exhVectorUP <- segmentDF$c_UPneumoExh
    
    dataVectorLP <- segmentDF$c_LPneumoSm
    inhVectorLP <- segmentDF$c_LPneumoInh
    exhVectorLP <- segmentDF$c_LPneumoExh
    
    answerRow <- extract.params$answer
    verbalAnswer <- verbalAnswer <- answerRow-(prestimSeg*cps)
    
    examName <- segmentDF$examName[301]
    seriesName <- segmentDF$seriesName[301]
    chartName <- segmentDF$chartName[301]
    eventLabel <- segmentDF$eventLabel[301]
  }
  
  {
    # scaleVals are set in the init script 
    # Jan 31, 2022 default to 200
    rateChangeConstraint <- log(rateDiff + 1)
    # ampChangeConstraint <- scaleVals['uPneumo'] * ampDiff
    # baseChangeConstraint <- scaleVals['uPneumo'] * baseDiff
    ampChangeConstraint <- log(ampDiff + 1)
    baseChangeConstraint <- log(baseDiff + 1)
  }
  
  #### initialize the output ####

  {
    # outputList <- NULL
    # outputVector <- c(rRate="", rAmp="", rBase="", RLE="")
    outputVectorUP <- NULL
    outputVectorLP <- NULL
  }
  
  #### compute the change in respiration rate - upper ####
  
  {
    # get the peak indices 
    maxPeakIdx <- maxPeak(x=dataVectorUP, y=40, firstLast=FALSE)
    
    # rate values indicate the mean number of seconds per resp cycle
    preRate <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 1:300)]), na.rm=TRUE) / cps
    postRate  <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 751:1050)]), na.rm=TRUE) / cps
    
    # stim segment with 2.5 sec latency
    stimRate  <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 376:750)]), na.rm=TRUE) / cps
    
    # calculate the slowing log rate changen value 
    rateChangeUp <-  -log(mean(c(preRate, postRate), na.rm=TRUE) / stimRate)
    # values > 0 indicate a slowing response
    
    if(is.na(rateChangeUp)) rateChangeUp <- 0
    
    if(isTRUE(constrained)) {
      if(rateChangeUp < rateChangeConstraint) {
        # rateChange <- NULL
        rateChangeUp <- 0
      }
    }
    
    # # recode slowing response as integer
    if(integerScores && rateChangeUp >= rateChangeConstraint) {
      rateChangeUp <- 1
    }
    
    # submit the log rate change to the output vector
    if(!is.null(rateChangeUp)) {
      # outputList$rRate <- rateChange
      outputVectorUP <- c(outputVectorUP, rRate=round(rateChangeUp,3))
    }
  }
  
  #### compute the change in respiration rate - lower ####
  
  {
    # get the peak indices 
    maxPeakIdx <- maxPeak(x=dataVectorLP, y=40, firstLast=FALSE)
    
    # rate values indicate the mean number of seconds per resp cycle
    preRate <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 1:300)]), na.rm=TRUE) / cps
    postRate  <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 751:1050)]), na.rm=TRUE) / cps
    
    # stim segment with 2.5 second latency
    stimRate  <- mean(diff(maxPeakIdx[which(maxPeakIdx %in% 376:750)]), na.rm=TRUE) / cps
    
    # calculate the slowing log rate change value
    rateChangeLp <-  -log( mean(c(preRate, postRate), na.rm=TRUE) / stimRate )
    # values > 0 indicate a slowing response
    
    if(is.na(rateChangeLp)) rateChangeLp <- 0
    
    if(isTRUE(constrained)) {
      if(rateChange < rateChangeConstraint) {
        # rateChange <- NULL
        rateChangeLp <- 0
      }
    }
    
    # # recode slowing resopnse as integer
    if(integerScores && rateChangeLp >= rateChangeConstraint) {
      rateChangeLP <- 1
    } 
    
    # submit the log rate change to the output vector
    if(!is.null(rateChangeLp)) {
      # outputList$rRate <- rateChange
      outputVectorLP <- c(outputVectorLP, rRate=round(rateChangeLp,3))
    }
  }
  
  #### compute the change in respiration amplitude - upper ####
  
  {
    # use the difference between inh and exh
    ampVectorUP <- inhVectorUP - exhVectorUP
    
    preAmp <- ampVectorUP[1:300]
    postAmp <- ampVectorUP[751:1050]
    stimAmp <- ampVectorUP[376:750]
    
    prePostAmpUP <- mean(c(mean(preAmp), mean(postAmp)), na.rm=TRUE)
    
    # calculate the amplitude decrease difference
    ampChangeUP <- prePostAmpUP - mean(stimAmp)
    # values > 0 indicate a decrease in amplitude
    
    # transform to a log ratio 
    # log ratio is a proportion of the expected y axis amplitude
    if(ampChangeUP > 0) {
      logAmpChangeUP <-  abs( log( ampChangeUP / prePostAmpUP ) )
    } else {
      logAmpChangeUP <- -abs( log( abs(ampChangeUP) / prePostAmpUP ) )
    }
    # log values > 0 indicate a reaction (suppression of amplitude)
    
    if(is.na(logAmpChangeUP)) logAmpChangeUP <- 0
    
    if(isTRUE(constrained)) {
      if(logAmpChangeUP < ampChangeConstraint) {
        # ampChange <- NULL
        logAmpChangeUP <- 0
      }
    }
    
    # # recode amplitude response as integer
    if(integerScores && logAmpChangeUP >= ampChangeConstraint) {
      logAmpChangeUP <- 1
    } 
    
    if(!is.null(logAmpChangeUP)) {
      # outputList[['rAmp']] <- ampChange 
      outputVectorUP <- c(outputVectorUP, rAmp=round(logAmpChangeUP,2))
    }
  }
  
  #### compute the change in respiration amplitude - lower ####
  
  {
    # use the difference between inh and exh
    ampVectorLP <- inhVectorLP - exhVectorLP
    
    preAmp <- ampVectorLP[1:300]
    postAmp <- ampVectorLP[751:1050]
    stimAmp <- ampVectorLP[376:750]
    
    prePostAmpLP <- mean(c(mean(preAmp), mean(postAmp)), na.rm=TRUE)
    
    # calculate the amplitude decrease
    ampChangeLP <- prePostAmpLP - mean(stimAmp)
    # values > 0 indicate a decrease in amplitude
    
    # transform to a log ratio 
    if(ampChangeLP > 0) {
      logAmpChangeLP <-  abs( log( ampChangeLP / prePostAmpLP ) )
    } else {
      logAmpChangeLP <- -abs( log( abs(ampChangeLP) / prePostAmpLP ) )
    }
    # log values > 0 indicate a reaction (suppression of amplitude)
    
    if(is.na(logAmpChangeLP)) logAmpChangeLP <- 0
    
    # # recode amplitude response as integer
    if(integerScores && logAmpChangeLP >= ampChangeConstraint) {
      logAmpChangeLP <- 1
    } else {
      # logAmpChangeLP <- 0
    }
    
    if(isTRUE(constrained)) {
      if(abs(logAmpChangeLP) < ampChangeConstraint) {
        # ampChange <- NULL
        logAmpChangeLP <- 0
      }
    }
    
    if(!is.null(logAmpChangeLP)) {
      # outputList[['rAmp']] <- ampChange 
      outputVectorLP <- c(outputVectorLP, rAmp=round(logAmpChangeLP,2))
    }
  }
  
  #### compute the change in respiration baseline - upper #### 
  
  {
    # use the exhVector
    
    preBase <- exhVectorUP[1:300]
    postBase <- exhVectorUP[751:1050]
    stimBase <- exhVectorUP[376:750]
    
    prePost <- mean(c(mean(preBase), mean(postBase)))
    
    # calculate the temporary increase in baseline
    baseChangeUP <- mean(stimBase) - prePost
    # values > 0 indicate a temp baseline increase
    
    # transform to a log ratio 
    if(baseChangeUP > 0) {
      logBaseChangeUP <- abs( log( baseChangeUP / prePostAmpUP ) )
    } else {
      logBaseChangeUP <- -abs( log( abs(baseChangeUP) / prePostAmpUP ) )
    }
    # values > 0 indicate an increase in baseline
    
    if(is.na(logBaseChangeUP)) logBaseChangeUP <- 0
    
    if(isTRUE(constrained)) {
      if(logBaseChangeUP < baseChangeConstraint) {
        # baseChange <- NULL
        logBaseChangeUP <- 0
      }
    }
    
    # # recode baseline change as integer
    if(integerScores && logBaseChangeUP >= baseChangeConstraint) {
      logBaseChangeUP <- 1
    }
    
    if(!is.null(logBaseChangeUP)) {
      # outputList[['rBase']] <- baseChange
      outputVectorUP <- c(outputVectorUP, rBase=round(logBaseChangeUP,2))
    }
  }

  #### compute the change in respiration baseline - lower #### 
  
  {
    # use the exhVector
    
    preBase <- exhVectorLP[1:300]
    postBase <- exhVectorLP[751:1050]
    stimBase <- exhVectorLP[301:750]
    
    prePost <- mean(c(mean(preBase), mean(postBase)))
    
    # calculate the temporary increase in baseline
    baseChangeLP <- mean(stimBase) - prePost
    # values > 0 indicate a temp baseline increase
    
    # transform to a log ratio 
    if(baseChangeLP > 0) {
      logBaseChangeLP <- abs( log( baseChangeLP / prePostAmpLP ) )
    } else {
      logBaseChangeLP <- -abs( log( abs(baseChangeLP) / prePostAmpLP ) )
    }
    # values > 0 indicate an increase in baseline
    
    if(is.na(logBaseChangeLP)) logBaseChangeLP <- 0
    
    if(isTRUE(constrained)) {
      if(logBaseChangeLP < baseChangeConstraint) {
        # baseChange <- NULL
        logBaseChangeLP <- 0
      }
    }
    
    # # recode baseline change as integer
    if(integerScores && logBaseChangeLP >= baseChangeConstraint) {
      logBaseChangeLP <- 1
    }
    
    if(!is.null(logBaseChangeLP)) {
      # outputList[['rBase']] <- baseChange
      outputVectorLP <- c(outputVectorLP, rBase=round(logBaseChangeLP,2))
    }
  }
  
  #### calculate the RLE measurement - upper ####
  
  {
    RLEUp <- pneumoMeasurementFn(dataVector=dataVectorUP[301:750], 
                               verbalAnswer=verbalAnswer)
    
    if(!is.null(RLEUp)) {
      # outputVector['RLE'] <- RLE * pneumoFEFactor
      RLEUp <- RLEUp * pneumoFEFactor
      outputVectorUP <- c(outputVectorUP, RLE=round((RLEUp*pneumoFEFactor),2))
    }
  }
  
  #### calculate the RLE measurement - lower ####
  
  {
    RLELp <- pneumoMeasurementFn(dataVector=dataVectorLP[301:750], 
                                 verbalAnswer=verbalAnswer)
    
    if(!is.null(RLELp)) {
      # outputVector['RLE'] <- RLE * pneumoFEFactor
      RLELp <- RLELp * pneumoFEFactor
      outputVectorLP <- c(outputVectorLP, RLE=round((RLELp*pneumoFEFactor),2))
    }
  }
  
  #### output ####
  
  {
    segInfo <- c(examName, seriesName, chartName, eventLabel)
    headerInfo <- c("examName", "seriesName", "chartName", "eventLabel", "sensorName")
    
    patternNames <- c("rRate", "rAmp", "rBase", "RLE")
    
    outputVectorUPi <- c(segInfo, "UPneumo", outputVectorUP)
    outputVectorLPi <- c(segInfo, "LPneumo", outputVectorLP)
    
    outputVector <- c(segInfo,
                      round(as.numeric(rateChangeUp, 3), 3), 
                      round(as.numeric(rateChangeLp, 3), 3), 
                      round(as.numeric(ampChangeUP, 3), 3), 
                      round(as.numeric(ampChangeLP, 3), 3), 
                      round(as.numeric(baseChangeUP, 3), 3), 
                      round(as.numeric(baseChangeLP, 3), 3), 
                      round(as.numeric(RLEUp, 3), 3),
                      round(as.numeric(RLELp, 3), 3) )
    
    patternNames <- c("rRateUP", "rRateLP", "ampChangeUP", "ampChangeLP", "baseChangeUP", "baseChangeLP", "RLEUp", "RLELp")
    
    names(outputVector) <- c(headerInfo[1:4], patternNames)
    
    outputVector <- rbind.data.frame(outputVector)
    
    names(outputVector) <- c(headerInfo[1:4], patternNames)
    
    # outputDF <- rbind.data.frame(outputVectorUPi, outputVectorLPi)
    # row.names(outputDF) <- NULL
    # names(outputDF) <- c(headerInfo, patternNames)
  }
  
  # return
  # return(outputDF)
  return(outputVector)
  
} # end pneumoPatternsFn() function




