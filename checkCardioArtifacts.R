# R function to evaluate cardio artifacts prior to and after response
# Raymond Nelson
# August 24, 2023
#
####
#
# this script contains 4 functions 
# 
# 1) tukeyFenceFn() for extrastystoles and fasiculations 
# 2) tukeyFence2Fn()
# 3) cardioPeakMAFn
# 4) checkCardioArtifactsFn() # the main function 
#
# called by the amplitudeExtractPC function 
# after extracting the respons, onset, end, and response value
#
# prestimulus and pre-response and post-response activity is evaluated for artifacts
#
####



# source("~/Dropbox/R/NCCA_ASCII_Parse/cardioExtrasystoles.R", echo=FALSE)



########## tukey fence function for extrasystoles ###########



tukeyFenceCardioFn <- function(x=segmentDF$c_Cardio1, inF=2, outF=4, fences="outer", side="upper") {
  # R function to compute cardio values that exceed the tukey fence limits
  # by extracting the diastolic and systolic peaks from the cardio time series
  # Aug 28, 2023
  # Raymond Nelson
  #
  ####
  # x input is a time series vector
  # output is a vector of 0's with significant points indicated by the value 1
  ####
  # plot.ts(x)
  # call a function to get the systolic and diastolic peaks
  # minMaxPeakFn is in the sigProcHelper.R script
  minMaxPeaks <- minMaxPeakFn(x)
  # calculate the absolute difference in peak values
  DAT <- abs(diff(x[minMaxPeaks]))
  # calculate the quartiles
  q1 <- quantile(DAT, .25)
  qm <- quantile(DAT, .5)
  q3 <- quantile(DAT, .75)
  # compute the interquartile range of absolute mean differences.
  iqr <- abs(q3 - q1)
  # adjust the length
  DAT <- c(qm, qm, DAT[2:(length(DAT)-1)], qm)
  names(DAT) <- NULL
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  # outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  if(fences=="both") {
    # inner and outer fences
    ifelse(side=="upper",
           pkVc <- sort(unique(c(which(DAT>=ufI), which(DAT>=ufO)))),
           ifelse(side=="lower",
             pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT<=lfO)))),
             pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO)))) 
           )
    )
  } else {
    # outer fences only
    ifelse(side=="upper",
           pkVc <- sort(unique(c(which(DAT>=ufO)))),
           ifelse(side=="lower",
                  pkVc <- sort(unique(c(which(DAT<=lfO)))),
                  pkVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
           )
    )
  }
  ## output ##
  # ouput only the peak rows that exceed the tukey fences
  outVc <- minMaxPeaks[pkVc]
  # remove artifacts at the first or last
  outVC <- outVc[!(outVc %in% c(1, length(outVc)))]
  return(outVc)
} # end tukeyFenceCardioFn()



tukeyFence2Fn <- function(DAT, inF=1.5, outF=3, fences="both") {
  # R function to compute cardio values that exceed the tukey fence limits
  # using the distance from MA to Mid lines
  # Aug 28, 2023
  # Raymond Nelson
  ####
  # x input is a time series vector
  # requires prior calculation of the difference between the MA and mid lines
  # output is a vector of 0's with significant points indicated by the value 1
  ####
  # plot.ts(x)
  # call a function to get the systolic and diastolic peaks
  q1 <- quantile(DAT, .25)
  qm <- quantile(DAT, .5)
  q3 <- quantile(DAT, .75)
  iqr <- abs(q3 - q1)
  # adjust the length
  DAT <- c(qm, qm, DAT[2:(length(DAT)-1)], qm)
  names(DAT) <- NULL
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  # outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  # inner and outer fences
  # pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO))))
  # outer fences only
  outVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
  #output
  # remove artifacts at the first or last
  outVC <- outVc[!(outVc %in% c(1, length(outVc)))]
  return(outVc)
} # end tukeyFence2Fn()



cardioPeakMAFn <- function(DAT, MA, sepVal=0, peaks="both") {
  # R function to extract cardio artifacts by comparing the cardio peaks to a slow moving average
  # in the checkCardioArtifacts.R script
  # Dec 4, 2023
  # Raymond Nelson
  ####
  # MA is the slow moving cardio average without the pulse
  # DAT is the cardio time series data
  # sepVal is a value to adjust the sensitivity of the function
  # output is a vector of 0's with significant points indicated by the value 1
  # peaks can be "syst" "diast" or "both"
  ####
  # requires prior calculation of the MA and Mid lines
  # requires the minPeak and maxPeak functions from the signProcHelper.R script
  # requires the cardio pulse rate to extract the peaks reliably
  ####
  # DAT <- tsData
  # MA <- cardioMA
  # initialize the output vector
  # outDAT <- rep(0, times=length(DAT))
  # compute the min and max peaks for the input data
  maxPeaks <- maxPeak(DAT, 7)
  minPeaks <- minPeak(DAT, 7)
  # offset value to adjust sensitivity
  if(!exists("sepVal")) sepVal <- 0
  # maxPeaks are expected to be greater than MA
  # maxPeaks are expected to be less than MA
  # artifacts are extracted when maxPeaks < MA or minPeaks > MA
  maxResponse <- maxPeaks[which(DAT[maxPeaks] + sepVal <= MA[maxPeaks])]
  minResponse <- minPeaks[which(DAT[minPeaks] - sepVal >= MA[minPeaks])]
  # aggregate the min and max peak artifacts to a single vector
  ifelse(peaks=="syst",
         outVc <- sort(unique(c(maxResponse))),
         ifelse(peaks=="diast",
                outVc <- sort(unique(c(minResponse))),
                outVc <- sort(unique(c(maxResponse, minResponse)))
                )
         )
  # remomve artifacts from the first and last rows of the input data
  outVc <- outVc[!(outVc %in% c(1, length(DAT)))]
  # add the artifacts to the output
  # outDAT[outVc] <- 1
  # end
  # return(outDAT)
  return(outVc)
} # end cardioPeakMAFn()
  

########## main function ##############




checkCardioArtifactsFn <- function(segmentName,
                                   tsData,
                                   # artifactVector,
                                   cardioMA, 
                                   cardioMid,
                                   onsetRow, 
                                   yChangeOnset, 
                                   yChangePeak,
                                   cardioPrestim,
                                   segmentDF ) {
  # R function to evaluate cardio artifacts prior to and after response
  # Raymond Nelson
  # August 24, 2023
  #
  ####
  #
  # called by the amplitudeExtractPC function 
  # after extracting the respons, onset, end, and response value
  #
  # activity is evaluated 
  # from a few seconds prior to reponse onset 
  # to response peak
  #
  # cardio artifacts can include
  # physical movements (voluntary or involuntary)
  # extrasystoles
  # involuntary movements (fasciculation) - less sensitivity here
  #
  # this function compares the cardioMA data (used for feature extraction)
  # with the systolic and diastolic peaks
  # artifacts are identified under 2 conditions
  # 1) when the diastolic peak (normally below the cardioMA) is above the cardioMA
  # 2) when the systolic peak (normally aboe the cardioMA) is below the cardioMA
  # these conditions indicate a sudden change in the cardio data 
  #
  #### input
  # cardioDAta is the time series cardio data after scaling and offsetting
  # cardioMA is the time series cardio data for a stimulus segment (evaluted for response)
  # including prestimulus and poststimulus data
  # cardioMid is the time series data using a lighter filter
  # onsetRow is the sample index for the onset fo the stimulus question, ususall 301
  # yChangeOnset is the sample index for the onset of the physiological response
  # yChangePeak is the sample index at the peak (end) of the physiological response
  # cardioPrestim is the length of the prestimulus and pre-respnose segments in seconds
  #
  #### output
  # a vector of 0s the same length as the number of rows in the segmebnt data frame
  # with artifacted rows tagged with the value "Artifact"
  #
  ####
  
  {
    # cardioPrestim is initialized in the NCCAASCII_init.R script
    if(!exists("cardioPrestim")) cardioPrestim <- cardioPrestim
    
    if(!exists("cardioMA")) cardioMA <- segmentDF$c_CardioMA
    if(!exists("cardioMid")) cardioMid <- segmentDF$c_CardioMid
    if(!exists("tsData")) tsData <- segmentDF$c_Cardio1
  }
  
  {
    examName <- segmentDF$examName[1]
    seriesName <- segmentDF$seriesName[1]
    chartName <- segmentDF$chartName[1]
  }
  
  
  {
    # # exit if there is no extracted y-axis change in the EDA data
    # if(is.na(yChangeOnset) || is.na(yChangePeak)) return(0)
  }
  
  {
    # if(all(examName == "DOrlandoTestJoe1", chartName == "02A", segmentName == "R2")) {
    #   # save the values to the global envir for inspection
    #   assign("segmentName", segmentName, envir=.GlobalEnv)
    #   assign("tsData", tsData, envir=.GlobalEnv)
    #   assign("cardioMA", cardioMA, envir=.GlobalEnv)
    #   assign("cardioMid", cardioMid, envir=.GlobalEnv)
    #   assign("onsetRow", onsetRow, envir=.GlobalEnv)
    #   assign("yChangeOnset", yChangeOnset, envir=.GlobalEnv)
    #   assign("yChangePeak", yChangePeak, envir=.GlobalEnv)
    #   assign("cardioPrestim", cardioPrestim, envir=.GlobalEnv)
    #   stop()
    # }
  }
  
  {
    # response segment
    newResponseRow <- yChangeOnset - (cardioPrestim*cps)
    newResponseEndRow <- yChangePeak + 0
    if(is.na(yChangeOnset) || is.na(yChangePeak)) {
      # if there is no usable response during this segment
      responseRows <- c((onsetRow-(cardioPrestim*cps)):(onsetRow+(15*cps)))
      # return(0)
    } else {
      # if there is any extracted cardio response
      responseRows <- c(newResponseRow:newResponseEndRow)
    }
  }
  
  ######## SEGMENT PLOT ############
  
  {

    # # plot the segment
    # 
    # ts <- c(1:nrow(segmentDF))
    # 
    # DAT <- cbind.data.frame(tScale=ts,
    #                         mid=segmentDF$c_CardioMid[ts],
    #                         ma=segmentDF$c_CardioMA[ts],
    #                         dat=segmentDF$c_Cardio1[ts] )
    # 
    # offsetVal <- 20
    # 
    # # pPoints <- sort(unique(c(maxPeak(DAT$mid, 5), minPeak(DAT$mid, 5))))
    # 
    # sPoints <- maxPeak(DAT$mid, 5)
    # sPoints <- sPoints[which(DAT$mid[sPoints] + 30 <= DAT$ma[sPoints])]
    # 
    # dPoints <- minPeak(DAT$mid, 5)
    # dPoints <- dPoints[which(DAT$mid[dPoints] - 30 >= DAT$ma[dPoints])]
    # 
    # scPoints <- maxPeak(DAT$dat, 10)
    # scPoints <- scPoints[which(DAT$dat[scPoints] - 0 <= DAT$ma[scPoints])]
    # 
    # dcPoints <- minPeak(DAT$dat, 10)
    # dcPoints <- dcPoints[which(DAT$dat[dcPoints] + 0 >= DAT$ma[dcPoints])]
    # 
    # ggplot() +
    #   geom_path(DAT, mapping=aes(x=tScale, y=dat), color="red") +
    #   geom_path(DAT, mapping=aes(x=tScale, y=ma), color="brown") +
    #   geom_path(DAT, mapping=aes(x=tScale, y=mid), color="black") +
    #   annotate("point", x=DAT$tScale[sPoints], y=DAT$mid[sPoints], shape=8, size=4, color="blue") +
    #   annotate("point", x=DAT$tScale[dPoints], y=DAT$mid[dPoints], shape=8, size=4, color="blue") +
    #   annotate("point", x=DAT$tScale[scPoints], y=DAT$dat[scPoints], shape=8, size=4, color="blue") +
    #   annotate("point", x=DAT$tScale[dcPoints], y=DAT$dat[dcPoints], shape=8, size=4, color="blue")

  }
  
  ########  CHART PLOT #########
  
  {

    # # plot the chart
    # 
    # ts <- c(1:nrow(chartDF))
    # 
    # DAT <- cbind.data.frame(tScale=ts,
    #                         mid=chartDF$c_CardioMid[ts],
    #                         ma=chartDF$c_CardioMA[ts],
    #                         dat=chartDF$c_Cardio1[ts] )
    # 
    # # pPoints <- sort(unique(c(maxPeak(DAT$mid, 5), minPeak(DAT$mid, 5))))
    # 
    # offsetVal <- 20
    # 
    # sPoints <- maxPeak(DAT$mid, 5)
    # sPoints <- sPoints[which(DAT$mid[sPoints] + 30 <= DAT$ma[sPoints])]
    # 
    # dPoints <- minPeak(DAT$mid, 5)
    # dPoints <- dPoints[which(DAT$mid[dPoints]- 30 >= DAT$ma[dPoints])]
    # 
    # scPoints <- maxPeak(DAT$dat, 10)
    # scPoints <- scPoints[which(DAT$dat[scPoints] - 0 <= DAT$ma[scPoints])]
    # 
    # dcPoints <- minPeak(DAT$dat, 10)
    # dcPoints <- dcPoints[which(DAT$dat[dcPoints] + 0 >= DAT$ma[dcPoints])]
    # 
    # length(sort(unique(c(sPoints, dPoints, scPoints, dcPoints))))
    # 
    # ggplot() +
    #   geom_path(DAT, mapping=aes(x=tScale, y=dat), color="red", linewidth=.2, alpha=.75) +
    #   geom_path(DAT, mapping=aes(x=tScale, y=ma), color="brown", linewidth=.2, alpha=.67) +
    #   geom_path(DAT, mapping=aes(x=tScale, y=mid), color="black", linewidth=.2, alpha=.77) +
    #   annotate("point", x=DAT$tScale[sPoints], y=DAT$mid[sPoints], shape=8, size=4, color="blue", alpha=.55) +
    #   annotate("point", x=DAT$tScale[dPoints], y=DAT$mid[dPoints], shape=8, size=4, color="blue", alpha=.55) +
    #   annotate("point", x=DAT$tScale[scPoints], y=DAT$dat[scPoints], shape=8, size=4, color="blue", alpha=.55) +
    #   annotate("point", x=DAT$tScale[dcPoints], y=DAT$dat[dcPoints], shape=8, size=4, color="blue", alpha=.55)

  }
  
  #### reset the output vector ####
  
  {
    # reset the artifact columns
    artifactVector <- rep(0, times=length(tsData))
    # segmentDF$Cardio1_a <- 0
  }
  
  #### cardio artifacts via comparison of MA and mid lines ####
  
  {
    
    # call a function to compare the MA and Mid lines
    # these are two different moving averages, one faster and the other slower
    
    # compare the cardio systolic peaks to the MA line
    artifactIndices1s <- cardioPeakMAFn(tsData, cardioMA, 50, peaks="syst")
    # systolic that are below the mid line
    
    # compare the cardio diastolic peaks to the MA line
    artifactIndices1d <- cardioPeakMAFn(tsData, cardioMA, 50, peaks="diast")
    # diastolic peaks that are above the mid line
    
    # artifactIndices1 <- which(artifactIndices1 != 0)
    
    if(length(artifactIndices1s) > 0) { 
      artifactVector[artifactIndices1s] <- "Artifact1s"
    } 
    
    if(length(artifactIndices1d) > 0) { 
      artifactVector[artifactIndices1d] <- "Artifact1d"
    } 
    
    # compare the MA line to a faster moving average with pulse info
    artifactIndices2 <- cardioPeakMAFn(cardioMid, cardioMA, 50, "both")
    
    # artifactIndices2 <- which(artifactIndices2 != 0)
    
    # commented out Apr 3, 2024
    # if(length(artifactIndices2) > 0) { 
    #   artifactVector[artifactIndices2] <- "Artifact2"
    # } 
    
    # reduce and aggregate the artifacts
    
    # artifactIndices <- sort(unique(c(artifactIndices1d, artifactIndices2)))
    artifactIndices <- sort(unique(c(artifactIndices1s, artifactIndices1d, artifactIndices2)))
    
  }
  
  #### cardio extrasystole, fasciculation, and movement artifacts ####
  
  {
    
    # call a tukey function to evaluate the distance between systolic and diastolic peaks
    # using the input time series cardio data
    extArtifacts <- tukeyFenceCardioFn(x=tsData, outF=3, fences="outer", side="upper") # was outF=6

    if(length(extArtifacts) > 0) {
      artifactVector[extArtifacts] <- "Artifact3"
    }

    # combine with the other artifacts
    artifactIndices <- sort(unique(c(artifactIndices, extArtifacts)))

    # plot.ts(tsData)
    
  }
  
  #### cardio mid artifacts using a Tukey fence ####
  
  { 
    
    # call a function to compute samples that exceed the outer Tukey fences
    
    # requires prior calculation of the difference between the MA and mid lines
    
    # changed to outF=4 2024Apr05 to reduce spurious artifacts
    midArtifacts <- tukeyFence2Fn(DAT=abs(cardioMid-cardioMA), outF=4, fences="outer") # was outF=5
    
    if(length(midArtifacts) > 0) { 
      artifactVector[midArtifacts] <- "Artifact4"
    } 
    
    # combine with the other artifacts
    artifactIndices <- sort(unique(c(artifactIndices, midArtifacts)))
    
  } 
  
  #### plot the segment #### 
  
  # {
  #   DAT <- cbind.data.frame(tScale=c(1:nrow(segmentDF)),
  #                           mid=segmentDF$c_CardioMid[],
  #                           ma=segmentDF$c_CardioMA[],
  #                           dat=segmentDF$c_Cardio1[] )
  #   ggplot() +
  #     geom_path(DAT, mapping=aes(x=tScale, y=dat), color="red") +
  #     geom_path(DAT, mapping=aes(x=tScale, y=ma), color="brown") +
  #     geom_path(DAT, mapping=aes(x=tScale, y=mid), color="black") +
  #     annotate("point", x=DAT$tScale[artifactIndices], y=DAT$mid[artifactIndices], shape=8, size=4, color="blue")
  # }
  
  #### finalize the cardio artifact output vector ####
  
  # exclude the first and last 1/2 second of the segment
  artifactIndices <- artifactIndices[!(artifactIndices %in% c(1:15, (length(tsData)-14):length(tsData)))]
  
  # if(length(artifactIndices) > 0) { 
  #   artifactVector[artifactIndices] <- "Artifact"
  # } 
  
  #### output ####
  
  # output is a vector of artifact row indices 
  return(artifactVector)
  
} # end checkCardioArtifactsFn()



# checkCardioArtifactsFn(cardioData,
#                        cardioMA, 
#                        cardioMid,
#                        onsetRow, 
#                        yChangeOnset, 
#                        yChangePeak,
#                        prestim )



