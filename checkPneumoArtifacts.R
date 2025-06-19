# R Function to check for respiration artifacts during feature extraction
# August 28, 2023
# Raymond Nelson 
#
####
#
## Helper Functions
# # respRateFn - to compute the respiration rate
# # respBufferLenFn to compute the buffer length when computing the respiration rate 
# respTonicInstabilityFn - tonic instability
# respInstabilityFn2 - inhalation and exhalation instability 
# respDesyncFn - to determine badly descynchronized thoracic and abdominal data 
# # respActivityArtifactFn - to extract biting, swallowing and other strange activity
# respActivityArtifactFn2 - another method for biting, swallowing and strange activity
# # respPreStimFn - check the prestim/stim ratio 
# # ULRatioFn - ratio comparison of thoracic and abdominal data
# respApneaFn - to identify apnea during testing 
# # respDBFn - to identify deep breaths during testing
# respDBFn2 - to identify deep breaths during testing
# baselineInstablityFn2 - to identify baseline instability artifacts 
#
# Main Function
# checkPneumoArtifactsFn - to call the helper functions for different types of resp artifacts
#
####



# first source a script for the pneumoMeasurementFn function
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/pneumoMeasurement.R'), echo=FALSE)
# already sourced by the featureExtraction.R script


# source a script for helper functions
# minPeak
# maxPeak
# source(paste0(RPath, "Parse/sigProcHelper.R", echo=FALSE)
# this is already sourced by the workFlow.R script during signal processing





respTonicInstabilityFn <- function(q25, q75, mid) {
  # R function to evaluate respiration instability artifacts using the interquartile range
  # October 12, 2024
  # Raymond Nelson
  ###
  # q25 is the 25th quartile for the entire chart or from X to XX
  # q75 is the 75th quartile for the entire chart or from X to XX
  # mid is the respiration mid line - computed via a smoothing function
  # these lines are computed at the time of signal processing and scaling
  # by a function called from the in the scaleOffse.R script
  # output is a vector of row indices where the mid line exceeded the interquartile range
  ###
  q25 <- q25[1]
  q75 <- q75[1]
  iqr <- q75 - q25
  
  ifU <- q75 # + (.5 * iqr) 
  ifL <- q25 # - (.5 * iqr)
  
  # locate rows where the midline exceeds the 25th percentile
  upLimit <- which(mid >= ifU)
  # locate rows where the midline exceeds the 75th percentile
  lwLimit <- which(mid <= q25)
  # combine them
  unstableRows <- sort(unique(c(upLimit, lwLimit)))
  # output
  return(unstableRows)
} # end respTonicInstabilityFn




respInstabilityFn2 <- function(med, q25, q75, Inh, Exh) {
  # R function to evaluate respiration instability artifacts using the interquartile range
  # October 13, 2024
  # Raymond Nelson
  ###
  # med is the 50th quartile or median for the entire chart or from X to XX
  # Inh is the interpolated inhalation line
  # Exh is the interpolated exhalation line 
  # these lines are computed at the time of signal processing and scaling
  # by a function called from the in the scaleOffse.R script
  ###
  # the inhalation line is expected to remain above the median
  # the exhalation line is expected to reamin below the median
  # inhalation or exhalation values on the unexpected side of the 50th, 25th or 75th quantile may indicate instability
  ###
  # output is a vector of row indices 
  # where the inhalation or exhalation points exceeded the median
  ###
  # locate rows where the inhalation line exceeds the 25th percentile
  inhLimit <- which(Inh <= q25)
  # locate rows where the exhalation line exceeds the 75th percentile
  exhLimit <- which(Exh >= q75)
  # combine them
  unstableRows <- sort(unique(c(inhLimit, exhLimit)))
  # output
  return(unstableRows)
} # end respInstabilityFn2




respDesyncFn <- function(diffVc=diffVc,inF=1.5, outF=3, fences="inner", side="both") {
  # R function to identify badly desynchronized thoracic and abdominal respiration
  # using a Tukey fence
  ###
  # diffVc is the distance between thoracic and abdominal time series data
  # inF is the inner fence
  # inF is the outer fence
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # output is a vector of row number where the thoracic - abdominal distance exceeded the Tukey fence
  ###
  # plot.ts(diffVc)
  # plot.ts(tsUP)
  # plot.ts(tsLP)  
  
  # adjust the data vector by removing the 1st and last sample rows
  diffVc <- diffVc[2:(length(diffVc)-1)]
  # diffVc must be the same length as the time series data
  names(diffVc) <- NULL
  # compute the quantiles
  
  # compute the quantiles
  q1 <- quantile(diffVc, .25)
  qm <- quantile(diffVc, .5)
  q3 <- quantile(diffVc, .75)
  # compute the interquartile range of absolute mean differences.
  iqr <- abs(q3 - q1)
  
  # diffVc must be the same length as the time series data
  diffVc <- c(qm, diffVc, qm)
  
  # adjust the min and max IQR to stabilize sensitivity
  # the IQR will get very small, leading to increased sensitivity with stable data
  # the IQR will get large with unstable data, leading to reduced sensitivity
  minIQR <- scaleVals['uPneumo'] * .33
  if(iqr <= minIQR) iqr <- minIQR
  maxIQR <- scaleVals['uPneumo'] * .67
  if(iqr >= maxIQR) iqr <- maxIQR
  
  # set the Tukey fences
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  # outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  
  # compare the diffVc to the tukey fences 
  if(fences=="inner") {
    # inner fences
    if(side=="upper") {
      sigVc <- which(diffVc>=ufI)
    } else if(side=="lower") {
      sigVc <- which(diffVc<=lfI)
    } else {
      # both upper and lower sides
      sigVc <- sort(unique(c(which(diffVc<=lfI), which(diffVc>=ufI))))
    }
  } else if(fences=="outer") {
    # outer fences only
    if(side=="upper") {
      sigVc <- which(diffVc>=ufO)
    } else if(side=="lower") {
      sigVc <- which(diffVc<=lfO)
    } else {
      # both upper and lower sides
      sigVc <- sort(unique(c(which(diffVc<=lfO), which(diffVc>=ufO))))
    }
  } else {
    # inner and outer fences
    if(side=="upper") {
      sigVc <- sort(unique(c(which(diffVc>=ufI), which(diffVc>=ufO))))
    } else if(side=="lower") {
      sigVc <- sort(unique(c(which(diffVc<=lfI), which(diffVc<=lfO))))
    } else {
      # both upper and lower sides
      sigVc <- sort(unique(c(which(diffVc<=lfI), which(diffVc>=ufI), which(diffVc<=lfO), which(diffVc>=ufO))))
    }
  }
  ## output ##
  # remove artifacts at the first or last data row
  sigVc <- sigVc[!(sigVc %in% c(1, length(diffVc)))]
  return(sigVc)
  # end respDesyncFn()
}



# respActivityArtifactFn <- function(tsData=tsDataUp, tsMid=tsMid, offset=0) {
#   # R function to extract unstable segments of respiration data
#   # compare min and max peaks to a mid line
#   # extract biting, swallowing, coughing, and strange respiration cycles
#   ###
#   # tsData is the time series data for thoracic or abdominal respiration
#   # tsMid is the mid line for the thoracic or abdominal respiration
#   # offset is an adjustment value to be added or subtracted to a peak
#   # to increase or decrease sensitivity
#   # output is a vector of indices at which artifacts are found
#   ###
#   # plot.ts(tsData)
#   ### call two functions to obtain the inhalation and exhalation peaks
#   maxPeaksTs <- maxPeak(x=tsData, y=round(.25*cps,0))
#   minPeaksTs <- minPeak(x=tsData, y=round(.25*cps,0))
#   # exclude the first and last indices
#   maxPeaksTs <- maxPeaksTs[!(maxPeaksTs %in% c(1, length(tsData)))]
#   minPeaksTs <- minPeaksTs[!(minPeaksTs %in% c(1, length(tsData)))]
#   ### max peaks below the mid line
#   # sigMaxTs <- which((tsData[maxPeaksTs] + offset) <= medianTs)
#   sigMaxTs <- which((tsData[maxPeaksTs] - offset) <= tsMid[maxPeaksTs])
#   ### min peaks above the mid line
#   # sigMinTs <- which((tsData[minPeaksTs] - offset) >= medianTs)
#   sigMinTs <- which((tsData[minPeaksTs] + offset) >= tsMid[minPeaksTs])
#   ###  aggregate the artifact rows
#   artifactRows <-
#     unique(sort(c(maxPeaksTs[sigMaxTs], minPeaksTs[sigMinTs])))
#   ###
#   return(artifactRows)
#   # end respActivityArtifactFn()
# }



respActivityArtifactFn2 <- function(tsData=tsDataUp, tsMid=tsMid, offset=0) {
  # R function to extract unstable segments of respiration data
  # compare min and max peaks to a mid line
  # extract biting, swallowing, coughing, and strange respiration cycles
  ###
  # tsData is the time series data for thoracic or abdominal respiration
  # tsMid is the mid line for the thoracic or abdominal respiration
  # offset is an adjustment value to be added or subtracted to a peak
  # to increase or decrease sensitivity
  # output is a vector of indices at which artifacts are found
  ###
  # plot.ts(tsData)
  ### call two functions to obtain the inhalation and exhalation peaks
  maxPeaks <- maxPeak(x=tsData, y=round(.25*cps,0))
  minPeaks <- minPeak(x=tsData, y=round(.25*cps,0))
  # exclude the first and last indices
  maxPeaks <- maxPeaks[!(maxPeaks %in% c(1, length(tsData)))]
  minPeaks <- minPeaks[!(minPeaks %in% c(1, length(tsData)))]
  
  {
    # Oct 15, 2024
    # check for very small changes in slope at the inhalation or exhalation lines
    minMaxPeaks <- sort(unique(c(maxPeaks, minPeaks)))
    minMaxPeaks2 <- minMaxPeaks
    minMaxVals <- tsData[minMaxPeaks2]
    for(i in 2:(length(minMaxPeaks2)-1)) {
      # check the sum of absolute differences and ignore small changes in slope (less than 1%)
      if(sum(abs(diff(c(minMaxVals[(i-1)], minMaxVals[i], minMaxVals[(i+1)])))) <= 30) minMaxPeaks2[i] <- NA
    }
    minMaxPeaks2 <- minMaxPeaks2[!is.na(minMaxPeaks2)]
    maxPeaks <- maxPeaks[maxPeaks %in% minMaxPeaks2]
    minPeaks <- minPeaks[minPeaks %in% minMaxPeaks2]
  }
  
  ### max peaks below the mid line
  sigMax <- which((tsData[maxPeaks] - offset) <= tsMid[maxPeaks])
  ### min peaks above the mid line
  sigMin <- which((tsData[minPeaks] + offset) >= tsMid[minPeaks])
  ###  aggregate the artifact rows
  artifactRows <-
    unique(sort(c(maxPeaks[sigMax], minPeaks[sigMin])))
  ###
  return(artifactRows)
  # end respActivityArtifactFn2()
}



# respPreStimFn <- function(measurement, prestimMeasurement, limitRatio=2) {
#   # R function to compute the prestim / stim ratio
#   prePostRatio <- exp(abs(log(prestimMeasurement / measurement)))
#   prePostRatio[which(prePostRatio >= exp(abs(log(limitRatio))))] <- 0
#   ifelse(prePostRatio == 0,
#          return(as.character(c(1:301))),
#          return(NULL) )
# }



# ULRatioFn <- function(tsUp, tsLp, limitRatio=2) {
#   # R function to compute the ratio of upper and lower respiration measurements
#   ULRatio <- exp(abs((log(tsUp / tsLp))))
#   ULRatio[which(ULRatio >= exp(abs(log(limitRatio))))] <- 0
#   ifelse(ULRatio == 0,
#          return(as.character(c(301:450))),
#          return(NULL) )
# }



respApneaFn <- function(x=segmentDF$c_UPneumoSm, w=3.75, y=20, seg=4) {
  # R function to extract respiration apnea from a stimulus segment
  ###
  # x is the time series data for thoracic or abdominal respiration
  # call separately for thoracic and abdominal respiration data
  # w is the width in seconds for a moving window 
  # a 3.75 respiration rate of 16 will have 3.75 sec cycles
  # y is the threshold value
  # an artifact is marked if the abs sum y distance is less than y for each w segment
  # seg is the number of measurements to check for each 3.75 sec segment
  ##
  # initialize an output vector
  outVc <- rep(0, length(x))
  # initialize the length of an apnea segment
  winLen <- c(1:round(w * cps))
  # select the segment indices to check
  winPoints <- c(1, cumsum(rep(round(length(winLen)/seg), (seg-1))), length(winLen))
  # iterate over the time series data with a moving window
  i=1
  for(i in 1:(length(outVc)-length(winLen)+1)) {
    # mark an artifact if the y axis chance is small for the w period
    if(sum(abs(diff(x[i+winPoints-1]))) < y) {
    # if(sum(diff(x[winPoints+i-1])) < y) {
      # outVc[winLen+i-1] <- "artifact"
      # outVc[(1+i-1)+round(winLen/2)] <- "artifact"
      outVc[i+winLen-1] <- "artifact"
    }
  }
  return(which(outVc != "0"))
  # end respApneaFn()
}



# respDBFn <- function(x=tsDataUp, mid=tsMidU, inF=1.5, outF=3, fences="inner", side="upper") {
#   # R function to extract deep breaths from the respiration data 
#   # using a Tukey fence method
#   # by extracting the inhalation and exhalation peaks
#   # Dec 15, 2023
#   # Raymond Nelson
#   ###
#   # x input is a time series vector
#   # mid is the respiration mid line
#   # inF is the inner Tukey fence
#   # outF is the outer Tukey fence
#   # fences can be "inner" "outer" or "both"
#   # side can be "upper" "lower" or "both"
#   # output is a vector of 0's with significant points indicated by the value 1
#   ###
#   # plot.ts(x)
#   ###
#   # if(!exists("fences")) fences <- "outer"
#   # if(!exists("side")) side <- "both"
#   ### initialize the output vector
#   outVc <- NULL
#   ### call a function to get the inhalation peaks peaks
#   maxPeaks <- maxPeak(x)
#   # remove the first and last peaks
#   maxPeaks <- maxPeaks[!(maxPeaks %in% c(1,length(x)))]
#   # remove peaks that are below the mid line
#   maxPeaks <- maxPeaks[x[maxPeaks] >= mid[maxPeaks]]
#   ### calculate the absolute difference in peak values
#   DAT <- (x[maxPeaks] - mid[maxPeaks])
#     ### calculate the quartiles
#   q1 <- quantile(DAT, .25)
#   qm <- quantile(DAT, .5)
#   q3 <- quantile(DAT, .75)
#   # compute the interquartile range of absolute mean differences.
#   iqr <- (q3 - q1)
#   ###
#   # October 12, 2024
#   # sensitivity to DBs increases when the respiration data are very stable
#   # this occurs because the iqr becomes restricted as a result of little variation
#   # to reduce hypersensitivity a min iqr value 
#   # can be obtained from the scalVals in the NCCAASCII_init.R scripe
#   minIQR <- scaleVals['uPneumo'] * .2
#   if(iqr <= minIQR) iqr <- minIQR
#   # sensitivity to DBs decreases when the data are unstable
#   # this is because the IQR becomes larger with more variation
#   # to reduce hyposensitivyt the iqr can be replace with a max value
#   maxIQR <- scaleVals['uPneumo'] * .8
#   if(iqr >= maxIQR) iqr <- maxIQR
#   ### 
#   # inner fences
#   lfI <- q1 - (inF * iqr)
#   ufI <- q3 + (inF * iqr)
#   # outer fences
#   lfO <- q1 - (outF * iqr)
#   ufO <- q3 + (outF * iqr)
#   ### compare the data values to the tukey fences
#   if(fences=="inner") {
#     # inner fences
#     if(side=="upper") {
#       pkVc <- which(DAT>=ufI)
#     } else if(side=="lower") {
#       pkVc <- which(DAT<=lfI)
#     } else {
#       # both upper and lower sides
#       pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI))))
#     }
#   } else if(fences=="outer") {
#     # outer fences only
#     if(side=="upper") {
#       pkVc <- which(DAT>=ufO)
#     } else if(side=="lower") {
#       pkVc <- which(DAT<=lfO)
#     } else {
#       # both upper and lower sides
#       pkVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
#     }
#   } else {
#     # both inner and outer fences
#     if(side=="upper") {
#       pkVc <- sort(unique(c(which(DAT>=ufI), which(DAT>=ufO))))
#     } else if(side=="lower") {
#       pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT<=lfO))))
#     } else {
#       # both upper and lower sides
#       pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO))))
#     }
#   }
#   ### output ###
#   # ouput only the peak rows that exceed the tukey fences
#   outVc <- maxPeaks[pkVc]
#   return(outVc)
#   # end respDBFn()
# }



respDBFn2 <- function(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="inner", side="upper") {
  # R function to extract deep breaths from the respiration data 
  # using a Tukey fence method
  # by extracting the inhalation peaks and comparing them to the median and the interquartile range
  # Oct 13, 2024
  # Raymond Nelson
  ###
  # x input is a time series vector
  # q25 is the 25th quantile for the time series data
  # q50 is the 50th quantile
  # q75 is the 75th quantile 
  # inF is the inner Tukey fence
  # outF is the outer Tukey fence
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # output is a vector of 0's with significant points indicated by the value 1
  ###
  # plot.ts(x)
  ### initialize the output vector
  outVc <- NULL
  
  ### calculate the interquartile range
  q3 <- q75[1]
  q2 <- q50[1]
  q1 <- q25[1]
  iqr <- abs(q3 - q1)
  
  ### fix the iqr for very stable or very unstable data
  
  {
    # sensitivity to DBs increases when the respiration data are very stable
    # this occurs because the iqr becomes restricted as a result of little variation
    # to reduce hypersensitivity a min iqr value 
    # can be obtained from the scalVals in the NCCAASCII_init.R scripe
    minIQR <- scaleVals['uPneumo'] * .15
    if(iqr <= minIQR) iqr <- minIQR
    # sensitivity to DBs decreases when the data are unstable
    # this is because the IQR becomes larger with more variation
    # to reduce hyposensitivyt the iqr can be replace with a max value
    maxIQR <- scaleVals['uPneumo'] * .67
    if(iqr >= maxIQR) iqr <- maxIQR
  }
  
  ### call a function to get the inhalation peaks peaks
  maxPeaks <- maxPeak(x)
  # remove the first and last peaks
  maxPeaks <- maxPeaks[!(maxPeaks %in% c(1,length(x)))]
  # remove peaks that are below the median
  maxPeaks <- maxPeaks[x[maxPeaks] >= rep(q2, times=length(maxPeaks))]
  ### get the peak values
  # DAT <- (x[maxPeaks] - rep(2, times=length(maxPeaks)))
  DAT <- x[maxPeaks]
  
  ### 
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  # outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  
  ### compare the data values to the tukey fences
  if(fences=="inner") {
    # inner fences
    if(side=="upper") {
      pkVc <- which(DAT>=ufI)
    } else if(side=="lower") {
      pkVc <- which(DAT<=lfI)
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI))))
    }
  } else if(fences=="outer") {
    # outer fences only
    if(side=="upper") {
      pkVc <- which(DAT>=ufO)
    } else if(side=="lower") {
      pkVc <- which(DAT<=lfO)
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
    }
  } else {
    # both inner and outer fences
    if(side=="upper") {
      pkVc <- sort(unique(c(which(DAT>=ufI), which(DAT>=ufO))))
    } else if(side=="lower") {
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT<=lfO))))
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO))))
    }
  }
  ### output ###
  # ouput only the peak rows that exceed the tukey fences
  outVc <- maxPeaks[pkVc]
  return(outVc)
  # end respDBFn2()
}




# baselineInstablityFn <- function(x=tsDataLp, mid=tsMidL, inF=1.5, outF=3, fences="inner", side="both") {
#   # R function to extract baseline instability artifacts from respiration data
#   # using a Tukey fence method
#   # by extracting the exhalation peaks
#   # Oct 10, 2024
#   # Raymond Nelson
#   ####
#   # x input is a time series vector
#   # mid is the respiration mid line
#   # inF is the inner Tukey fence
#   # outF is the outer Tukey fence
#   # fences can be "inner" "outer" or "both"
#   # side can be "upper" "lower" or "both"
#   # output is a vector of 0's with significant points indicated by the value 1
#   ###
#   # plot.ts(x)
#   ###
#   # if(!exists("fences")) fences <- "outer"
#   # if(!exists("side")) side <- "both"
#   ### initialize the output vector
#   outVc <- NULL
#   ### call a function to get exhalation peaks
#   # minPeak function is in the sigProcHelper.R script
#   minPeaks <- minPeak(x)
#   # remove the first and last peaks
#   minPeaks <- minPeaks[!(minPeaks %in% c(1,length(x)))]
#   # remove peaks that are above the mid line
#   minPeaks <- minPeaks[x[minPeaks] <= mid[minPeaks]]
#   # calculate the difference in peak values
#   DAT <- (mid[minPeaks] - x[minPeaks])
#   # calculate the quartiles
#   q1 <- quantile(DAT, .25)
#   qm <- quantile(DAT, .5)
#   q3 <- quantile(DAT, .75)
#   # compute the interquartile range of absolute mean differences.
#   iqr <- (q3 - q1)
#   ###
#   # October 12, 2024
#   # sensitivity to baseline changes increases when the respiration data are very stable
#   # this occurs because the iqr becomes restricted as a result of little variation
#   # to reduce hypersensitivity a min iqr value 
#   # can be obtained from the scalVals in the NCCAASCII_init.R scripe
#   minIQR <- scaleVals['uPneumo'] * .2
#   if(iqr <= minIQR) iqr <- minIQR
#   # sensitivity to DBs decreases when the data are unstable
#   # this is because the IQR becomes larger with more variation
#   # to reduce hyposensitivyt the iqr can be replace with a max value
#   maxIQR <- scaleVals['uPneumo'] * .8
#   if(iqr >= maxIQR) iqr <- maxIQR
#   ### initialize the Tukey fences
#   # inner fences
#   lfI <- q1 - (inF * iqr)
#   ufI <- q3 + (inF * iqr)
#   # outer fences
#   lfO <- q1 - (outF * iqr)
#   ufO <- q3 + (outF * iqr)
#   ### compare the data to the Tukey fences
#   # if(!exists("fences")) fences <- "outer"
#   # if(!exists("side")) side <- "both"
#   if(fences=="inner") {
#     # inner fences
#     if(side=="upper") {
#       pkVc <- which(DAT>=ufI)
#     } else if(side=="lower") {
#       pkVc <- which(DAT<=lfI)
#     } else {
#       # both upper and lower sides
#       pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI))))
#     }
#   } else if(fences=="outer") {
#     # outer fences only
#     if(side=="upper") {
#       pkVc <- which(DAT>=ufO)
#     } else if(side=="lower") {
#       pkVc <- which(DAT<=lfO)
#     } else {
#       # both upper and lower sides
#       pkVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
#     }
#   } else {
#     # both inner and outer fences
#     if(side=="upper") {
#       pkVc <- sort(unique(c(which(DAT>=ufI), which(DAT>=ufO))))
#     } else if(side=="lower") {
#       pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT<=lfO))))
#     } else {
#       # both upper and lower sides
#       pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO))))
#     }
#   }
#   ### output ###
#   # ouput only the peak rows that exceed the tukey fences
#   outVc <- minPeaks[pkVc]
#   return(outVc)
#   # end baselineInstablityFn()
# }




baselineInstablityFn2 <- function(x=tsDataLp, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="inner", side="lower") {
  # R function to extract baseline instability artifacts from respiration data
  # using a Tukey fence method
  # by extracting the exhalation peaks and comparing them to the median and the interquartile range
  # Oct 13, 2024
  # Raymond Nelson
  ###
  # x input is a time series vector
  # q25 is the 25th quantile for the time series data
  # q50 is the 50th quantile
  # q75 is the 75th quantile 
  # inF is the inner Tukey fence
  # outF is the outer Tukey fence
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # output is a vector of 0's with significant points indicated by the value 1
  ###
  # plot.ts(x)
  ### initialize the output vector
  outVc <- NULL
  
  ### calculate the quartiles
  q1 <- q25[1]
  qm <- q50[1]
  q3 <- q75[1]
  # compute the interquartile range of absolute mean differences.
  iqr <- (q3 - q1)
  
  ### fix the iqr for very stable or very unstable data
  
  {
    ###
    # sensitivity to baseline changes increases when the respiration data are very stable
    # this occurs because the iqr becomes restricted as a result of little variation
    # to reduce hypersensitivity a min iqr value 
    # can be obtained from the scalVals in the NCCAASCII_init.R scripe
    minIQR <- scaleVals['uPneumo'] * .15
    if(iqr <= minIQR) iqr <- minIQR
    # sensitivity to DBs decreases when the data are unstable
    # this is because the IQR becomes larger with more variation
    # to reduce hyposensitivyt the iqr can be replace with a max value
    maxIQR <- scaleVals['uPneumo'] * .67
    if(iqr >= maxIQR) iqr <- maxIQR
  }
  
  ### call a function to get exhalation peaks
  # minPeak function is in the sigProcHelper.R script
  minPeaks <- minPeak(x)
  # remove the first and last peaks
  minPeaks <- minPeaks[!(minPeaks %in% c(1,length(x)))]
  # remove peaks that are above the mid line
  minPeaks <- minPeaks[x[minPeaks] <= rep(qm, times=length(minPeaks))]
  # calculate the difference in peak values
  DAT <- x[minPeaks]
  
  ### initialize the Tukey fences
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  # outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  ### compare the data to the Tukey fences
  # if(!exists("fences")) fences <- "outer"
  # if(!exists("side")) side <- "both"
  if(fences=="inner") {
    # inner fences
    if(side=="upper") {
      pkVc <- which(DAT>=ufI)
    } else if(side=="lower") {
      pkVc <- which(DAT<=lfI)
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI))))
    }
  } else if(fences=="outer") {
    # outer fences only
    if(side=="upper") {
      pkVc <- which(DAT>=ufO)
    } else if(side=="lower") {
      pkVc <- which(DAT<=lfO)
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
    }
  } else {
    # both inner and outer fences
    if(side=="upper") {
      pkVc <- sort(unique(c(which(DAT>=ufI), which(DAT>=ufO))))
    } else if(side=="lower") {
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT<=lfO))))
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO))))
    }
  }
  ### output ###
  # ouput only the peak rows that exceed the tukey fences
  outVc <- minPeaks[pkVc]
  return(outVc)
  # end baselineInstablityFn2()
}




########## main function ##############



checkPneumoArtifactsFn <- function(responseOnsetRow=NULL,
                                   responseEndRow=NULL,
                                   answerRow=NULL,
                                   segmentDF=segmentDF 
                                   ) { 
  # R Function to check for respiration artifacts during feature extraction
  # August 28, 2023 
  # Raymond Nelson 
  # 
  #### 
  # 
  # segmentDF is a data frame with the time series data and event markers for a stimulus segment
  # input time series data includes 10sec prestim, 15 sec stimulus segment, and 10sec poststim
  #
  # this function makes use of both thoracic and abdominal sensor data
  #
  # output is a vector of 0s the same length as the segmentDF
  # artifacted indices are are indicated with the "Artifact" message
  #
  ####
  
  {
    examName <- segmentDF$examName[1]
    seriesName <- segmentDF$seriesName[1]
    chartName <- segmentDF$chartName[1]
    sensorName <- "pneumo"
  }
  
  {
    # get the respiration data
    tsDataUp <- segmentDF$c_UPneumoSm
    tsDataLp <- segmentDF$c_LPneumoSm
    
	# compute the distance between the thoracic and abdominal data
    diffVc <- (tsDataUp - tsDataLp)
    
    # get the mid lines
    tsMidU <- segmentDF$c_UPneumoMid
    tsMidL <- segmentDF$c_LPneumoMid
    
    # get the quantile lines
    q25U <- segmentDF$c_UPneumo_Q25
    q50U <- segmentDF$c_UPneumo_Q50
    q75U <- segmentDF$c_UPneumo_Q75
    
    q25L <- segmentDF$c_LPneumo_Q25
    q50L <- segmentDF$c_LPneumo_Q50
    q75L <- segmentDF$c_LPneumo_Q75
    
    # get the inhalation and exhalation lines
    tsInhU <- segmentDF$c_UPneumoInh
    tsInhL <- segmentDF$c_LPneumoInh
    tsExhU <- segmentDF$c_UPneumoExh
    tsExhL <- segmentDF$c_LPneumoExh
    
    # plot.ts(tsInhU)
    # plot.ts(tsMidU)
  }
  
  {
    #   # save the values to the global envir for inspection
    assign("tsDataUp", tsDataUp, envir=.GlobalEnv)
    assign("tsDataLp", tsDataLp, envir=.GlobalEnv)
    assign("diffVc", diffVc, envir=.GlobalEnv)
    assign("tsMidU", tsMidU, envir=.GlobalEnv)
    assign("tsMidL", tsMidL, envir=.GlobalEnv)
    assign("q25U", q25U, envir=.GlobalEnv)
    assign("q50U", q50U, envir=.GlobalEnv)
    assign("q75U", q75U, envir=.GlobalEnv)
    assign("q25L", q25L, envir=.GlobalEnv)
    assign("q50L", q50L, envir=.GlobalEnv)
    assign("q75L", q75L, envir=.GlobalEnv)
    assign("tsInhU", tsInhU, envir=.GlobalEnv)
    assign("tsInhL", tsInhL, envir=.GlobalEnv)
    assign("tsExhU", tsExhU, envir=.GlobalEnv)
    assign("tsExhL", tsExhL, envir=.GlobalEnv)
    assign("responseOnsetRow", responseOnsetRow, envir=.GlobalEnv)
    assign("responseEndRow", responseEndRow, envir=.GlobalEnv)
    assign("answerRow", answerRow, envir=.GlobalEnv)
    assign("segmentDF", segmentDF, envir=.GlobalEnv)
    assign("chartName", chartName, envir=.GlobalEnv)
    assign("seriesName", seriesName, envir=.GlobalEnv)
    assign("examName", examName, envir=.GlobalEnv)
    assign("sensorName", sensorName, envir=.GlobalEnv)
  }
  
  if(length(tsDataUp) != length(tsDataLp)) {
    stop("problem with pneumo data")
  }
  
  #### stop for inspection ####
  
  {
    # stop()
    
    # if(all(examName=="DDaveTestMikeA0",
    #        seriesName=="1",
    #        chartName=="06A",
    #        segmentName=="C2")) {
    #   stop()
    # }
  }
  
  {
    # initialize the output vectors
    outVcU <- rep(0, times=length(tsDataUp))
    outVcL <- rep(0, times=length(tsDataLp))
    # and an output data frame
    outVcDF <- cbind.data.frame(outVcU, outVcL)
  }
  
  {
    # fix some missing parameters
    if(is.null(responseOnsetRow)) responseOnsetRow <- length(tsDataUp)
    if(is.null(responseEndRow)) responseEndRow <- length(tsDataUp)
    if(responseEndRow > length(tsDataUp)) responseEndRow <- length(tsDataUp)
    # check for missing answer
    answerRow <- answerRow[which((answerRow <= responseEndRow) & (answerRow >= responseOnsetRow))]
    if(length(answerRow) == 0) answerRow <- NULL
    # check for very late answer
    if(!is.null(answerRow) && answerRow >= length(tsDataUp)) answerRow <- NULL
  }
  
  {
    # compute some vectors for the prestim, stim, and poststim rows
    preStimRows <- c(1:(responseOnsetRow-1))
    stimSegRows <- c(responseOnsetRow:(responseOnsetRow+round(measuredSeg*cps)-1))
    postStimRows <- c((responseOnsetRow+round(measuredSeg*cps)-1):length(tsDataUp))
  }
  
  {
    # compute the answer distortion buffer
    if(!is.null(answerRow)) {
      ansBuffOn <- answerRow - round((pneumoAnsBuff * cps)) - 0
      ansBuffOff <- answerRow + round((pneumoAnsBuff * cps)) + 0
      # make sure the answer buffer does not exceed the segment data frame
      if(ansBuffOn < 1) ansBuffOn <- 1
      if(ansBuffOff > nrow(segmentDF)) ansBuffOff <- nrow(segmentDF)
      ansBuffRows <- c(ansBuffOn:ansBuffOff)
    } else {
      ansBuffRows <- NULL
    }
  }
   
  {
    # initialize some vectors for the inhalation and exhalation diff vals
    # diffUp <- c(0, abs(diff(tsDataUp)))
    # diffLp <- c(0, abs(diff(tsDataLp)))
  }
    
  {
    # initialize the segments indices
    prestimSegUp <- tsDataUp[preStimRows]
    prestimSegLp <- tsDataLp[preStimRows]
    stimSegUp <- tsDataUp[stimSegRows]
    stimSegLp <- tsDataLp[stimSegRows]
    poststimSegUp <- tsDataUp[postStimRows]
    poststimSegLp <- tsDataLp[postStimRows]
    
    # prestimDiffUp <- diffUp[preStimRows]
    # prestimDiffLp <- diffLp[preStimRows]
    # stimDiffUp <- diffUp[stimSegRows]
    # stimDiffLp <- diffLp[stimSegRows]
    # poststimDiffUp <- diffUp[postStimRows]
    # poststimDiffLp <- diffLp[postStimRows]
  } 
  
  #### get the measurements without excluding the answer distortion  ####
  
  {
    prestimMeasureUp <- pneumoMeasurementFn(prestimSegUp, verbalAnswer=NULL)
    prestimMeasureLp <- pneumoMeasurementFn(prestimSegLp, verbalAnswer=NULL)
    
    stimSegMeasureUp <- pneumoMeasurementFn(stimSegUp, verbalAnswer=NULL)
    stimSegMeasureLp <- pneumoMeasurementFn(stimSegLp, verbalAnswer=NULL)
    
    poststimMeasureUp <- pneumoMeasurementFn(poststimSegUp, verbalAnswer=NULL)
    poststimMeasureLp <- pneumoMeasurementFn(poststimSegLp, verbalAnswer=NULL)
  }
  
  #### evaluate the desynchronization and distance between the respiration data ####
  
  {
    
    # plot.ts(tsMidU)
    # plot.ts(tsMidL)

    # diffVc <- tsMidU - tsMidL
    # diffVc <- (tsDataUp - tsDataLp)
    # plot.ts(diffVc)
    
    # diffVc is the difference between the thoracic and abdominal mid-lines

    artifactRowsDesync <- respDesyncFn(diffVc=diffVc, inF=1.5, outF=3, fences="inner", side="both")

    # if(length(artifactRowsDesync) > 0) {
    #   assign("segmentDF", segmentDF, envir=.GlobalEnv)
    #   stop()
    # }

    # remove desynchronization artifacts from the answer buffer
    artifactRowsDS <- artifactRowsDesync[!(artifactRowsDesync %in% ansBuffRows)]

    # desynchronization artifacts are marked at both abdominal and thoracic sensors
    outVcDF$outVcU[artifactRowsDS] <- "ArtifactDS"
    outVcDF$outVcL[artifactRowsDS] <- "ArtifactDS"
    
  }
  
  #### evaluate the tonic stability of the respiration data using the mid line ####
  
  {
    
    tonicInstabilityRowsUP <- respTonicInstabilityFn(q25=q25U, q75=q75U, mid=tsMidU)
    tonicInstabilityRowsLP <- respTonicInstabilityFn(q25=q25L, q75=q75L, mid=tsMidL)
    
    # combine the upper and lower artifact vectors
    tonicInstabilityRowsUL <- sort(unique(c(tonicInstabilityRowsUP, tonicInstabilityRowsLP)))
    
    outVcDF$outVcU[as.numeric(tonicInstabilityRowsUP)] <- "ArtifactTonicity"
    outVcDF$outVcL[as.numeric(tonicInstabilityRowsLP)] <- "ArtifactTonicity"
    
  }
  
  #### evaluate respiration instability using the inhalation and exhalation lines ####
  
  {
    
    # artifacts are marked when the inhalation line is less than the 25th quantile
    # or when the exhalation line is greater than the 75th quantile
    
    instabilityRowsUP <- respInstabilityFn2(med=q50U, q25=q25U, q75=q75U, Inh=tsInhU, Exh=tsExhU)
    instabilityRowsLP <- respInstabilityFn2(med=q50L, q25=q25L, q75=q75L, Inh=tsInhL, Exh=tsExhL)
    
    # combine the upper and lower artifact vectors
    instabilityRowsUL <- sort(unique(c(instabilityRowsUP, instabilityRowsLP)))
    
    # remove activity instability artifacts from the answer buffer
    {
      instabilityRowsUP <- instabilityRowsUP[!(instabilityRowsUP %in% ansBuffRows)]
      instabilityRowsLP <- instabilityRowsLP[!(instabilityRowsLP %in% ansBuffRows)]
      
      instabilityRowsUL <- instabilityRowsUL[!(instabilityRowsUL %in% ansBuffRows)]
    }

    # submit the instability artifacts to the output buffer
    # outVcDF$outVcU[as.numeric(instabilityRowsUP)] <- "ArtifactInst"
    # outVcDF$outVcL[as.numeric(instabilityRowsLP)] <- "ArtifactInst"
    
  }
  
  #### compare the ratio of abdominal and thoracic measurements ####
  
  {
    # # thoracic/abdominal ratio for the stim segment
    # artifactRows4 <- ULRatioFn(stimSegMeasureUp, stimSegMeasureLp)
    # artifactRows5 <- ULRatioFn(prestimMeasureUp, prestimMeasureLp)
    # # this function will return the value 301 if the diff is significant
    # 
    # artifactRows4 <- artifactRows4[which(artifactRows4 != "")]
    # artifactRows5 <- artifactRows5[which(artifactRows5 != "")]
    # 
    # # combine them
    # artifactRows45 <- sort(unique(c(artifactRows4, artifactRows5)))
    # 
    # outVcDF$outVcU[as.numeric(artifactRows45)] <- "Artifact"
    # outVcDF$outVcL[as.numeric(artifactRows45)] <- "Artifact"
  } 
  
  #### compare the ratio of prestimulus and stimulus segments ####
   
  {
    # artifactRowsA <- respPreStimFn(measurement=stimSegMeasureUp, prestimMeasurement=prestimMeasureUp)
    # artifactRowsB <- respPreStimFn(measurement=stimSegMeasureLp, prestimMeasurement=prestimMeasureLp)
    # # this function will reuturn an artifact at sample 301 if the diff is significant
    # 
    # # combine them
    # artifactRowsAB <- sort(unique(c(artifactRowsA, artifactRowsB)))
    # 
    # outVcDF$outVcU[as.numeric(artifactRowsA)] <- "Artifact"
    # outVcDF$outVcL[as.numeric(artifactRowsB)] <- "Artifact"
  }
  
  #### check for activity artifact - upper and lower peaks with unexpected values ####
  
  {
    ## biting, swallowing, tussis, other distortions ##

    # Call a function to separately compare the upper and lower peaks to a mid line
    artifactRowsUp <- respActivityArtifactFn2(tsData=tsDataUp, tsMid=tsMidU)
    artifactRowsLp <- respActivityArtifactFn2(tsData=tsDataLp, tsMid=tsMidL)

    # plot.ts(tsDataUP)
    # plot.ts(tsDataMidU)

    # exclude artifacts around the verbal answer
    artifactRowsUp <- artifactRowsUp[!(artifactRowsUp %in% ansBuffRows)]
    artifactRowsLp <- artifactRowsLp[!(artifactRowsLp %in% ansBuffRows)]

    artifactRowsUL <- sort(unique(c(artifactRowsUp, artifactRowsLp)))

    # outVcDF[artifactRows] <- "Artifact"
    outVcDF$outVcU[artifactRowsUp] <- "ArtifactBU"
    outVcDF$outVcL[artifactRowsLp] <- "ArtifactBL"
  } 
  
  #### evaluate the data for deep breaths ####
  
  {
    # call a function to locate deep breath artifacts
    
    # deep breaths are removed from the answering buffer
    
    # artifactRows7 <- respDBFn(x=tsDataUp, mid=tsMidU)
    # artifactRows8 <- respDBFn(x=tsDataLp, mid=tsMidL)
    # # this function will return the row index where deep breath peaks are located
    
    # quantiles were initialized earlier in this function 
    
    artifactRows7 <- respDBFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="inner", side="upper")
    artifactRows8 <- respDBFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1.5, outF=3, fences="inner", side="upper")
    # this function will return the row index where deep breath peaks are located
    
    # exclude artifacts surrounding the verbal answer
    artifactRows7 <- artifactRows7[!(artifactRows7 %in% ansBuffRows)]
    artifactRows8 <- artifactRows8[!(artifactRows8 %in% ansBuffRows)]
    
    artifactRowsDB <- sort(unique(c(artifactRows7, artifactRows8)))
    
    # keep the thoracic and abdominal separate sto avoid excess data loss 
    outVcDF$outVcU[artifactRows7] <- "ArtifactDB"
    outVcDF$outVcL[artifactRows8] <- "ArtifactDB"
  } 
  
  #### evaluate the data for BIG deep breaths ####
  
  {
    # BIG deep breaths are not removed from the answering buffer
    
    # call a function to locate deep breath artifacts
    # artifactRows7 <- respDBFn(x=tsDataUp, mid=tsMidU)
    # artifactRows8 <- respDBFn(x=tsDataLp, mid=tsMidL)
    # # this function will return the row index where deep breath peaks are located
    
    # quantiles were initialized earlier in this function 
    
    artifactRows7 <- respDBFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="outer", side="upper")
    artifactRows8 <- respDBFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1.5, outF=3, fences="outer", side="upper")
    # this function will return the row index where deep breath peaks are located
    
    # exclude artifacts surrounding the verbal answer
    # artifactRows7 <- artifactRows7[!(artifactRows7 %in% ansBuffRows)]
    # artifactRows8 <- artifactRows8[!(artifactRows8 %in% ansBuffRows)]
    
    artifactRowsDB <- sort(unique(c(artifactRows7, artifactRows8)))
    
    # keep the thoracic and abdominal separate sto avoid excess data loss 
    outVcDF$outVcU[artifactRows7] <- "ArtifactBDB"
    outVcDF$outVcL[artifactRows8] <- "ArtifactBDB"
  } 
  
  #### evaluate the data for apnea ####
  
  {
    UPApn <- respApneaFn(x=tsDataUp)
    LPApn <- respApneaFn(x=tsDataLp)
    
    # exclude artifacts surrounding the verbal answer
    # UPApn <- UPApn[!(UPApn %in% ansBuffRows)]
    # LPApn <- LPApn[!(LPApn %in% ansBuffRows)]
    # 
    # combine both respiration sensors
    apneaArtifactsUL <- sort(unique(c(UPApn, LPApn)))
    
    # is not apnea if respiration acitivity is extant in one of the sensors
    # apneaArtifacts <- apneaArtifacts[which(apneaArtifacts %in% UPApn & apneaArtifacts %in% LPApn)]
    
    outVcDF$outVcU[UPApn] <- "ArtifactAP"
    outVcDF$outVcL[LPApn] <- "ArtifactAP"
  }
  
  #### evaluate the baseline for artifacts ####
  
  {
    
    # call a function to locate baseline instability artifacts artifacts
    # baselineArtifactsUp <- baselineInstablityFn(x=tsDataUp, mid=tsMidU, inF=1.5, outF=3, fences="inner", side="both")
    # baselineArtifactsLp <- baselineInstablityFn(x=tsDataLp, mid=tsMidL, inF=1.5, outF=3, fences="inner", side="both")
    
    baselineArtifactsUp <- baselineInstablityFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1, outF=2, fences="inner", side="lower")
    baselineArtifactsLp <- baselineInstablityFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1, outF=2, fences="inner", side="lower")
    
    # exclude artifacts surrounding the verbal answer
    baselineArtifactsUp <- baselineArtifactsUp[!(baselineArtifactsUp %in% ansBuffRows)]
    baselineArtifactsLp <- baselineArtifactsLp[!(baselineArtifactsLp %in% ansBuffRows)]
    
    # compbine the baseline artifacts for the thoracic and abdominal sensors
    baselineArtifactsUL <- sort(unique(c(baselineArtifactsUp, baselineArtifactsLp)))
    
    # keep the thoracic and abdominal separate to avoid excess data loss 
    outVcDF$outVcU[baselineArtifactsUp] <- "ArtifactBL"
    outVcDF$outVcL[baselineArtifactsLp] <- "ArtifactBL"
    
  }
  
  #### evaluate the baseline for BIG artifacts ####
  
  {

    # call a function to locate baseline instability artifacts artifacts
    # baselineArtifactsUp <- baselineInstablityFn(x=tsDataUp, mid=tsMidU, inF=1.5, outF=3, fences="outer", side="both")
    # baselineArtifactsLp <- baselineInstablityFn(x=tsDataLp, mid=tsMidL, inF=1.5, outF=3, fences="outer", side="both")

    baselineArtifactsUp <- baselineInstablityFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1, outF=2, fences="outer", side="lower")
    baselineArtifactsLp <- baselineInstablityFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1, outF=2, fences="outer", side="lower")

    # exclude artifacts surrounding the verbal answer
    # baselineArtifactsUp <- baselineArtifactsUp[!(baselineArtifactsUp %in% ansBuffRows)]
    # baselineArtifactsLp <- baselineArtifactsLp[!(baselineArtifactsLp %in% ansBuffRows)]

    # compbine the baseline artifacts for the thoracic and abdominal sensors
    baselineArtifactsUL <- sort(unique(c(baselineArtifactsUp, baselineArtifactsLp)))

    # keep the thoracic and abdominal separate to avoid excess data loss
    outVcDF$outVcU[baselineArtifactsUp] <- "ArtifactBL"
    outVcDF$outVcL[baselineArtifactsLp] <- "ArtifactBL"

  }
  
  
  
  #### exclude respiration artifacts in the poststimulus segment 
  
  {
    
    # outVcDF$outVcU[postStimRows] <- "0"
    # outVcDF$outVcL[postStimRows] <- "0"
    
  }
  
  #### output ####
  
  # return(outVc)
  return(outVcDF)
  
  # end checkPneumoArtifactsFn()
} 





