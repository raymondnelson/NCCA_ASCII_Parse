# R Function to check for respiration artifacts during feature extraction
# August 28, 2023
# Raymond Nelson 
#
####
#
## Helper Functions
# # respRateFn - to compute the respiration rate
# # respBufferLenFn to compute the buffer length when computing the respiration rate 
#
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





respTonicInstabilityFn <- function(lf, uf, mid) {
  # R function to evaluate respiration instability artifacts using the interquartile range
  # October 12, 2024
  # Raymond Nelson
  ###
  # lf is the 25th or 10th percentile for the entire chart or from X to XX
  # uf is the 75th or 90th percentile  for the entire chart or from X to XX
  # mid is the respiration mid line - computed via a smoothing function
  # these lines are computed at the time of signal processing and scaling
  # by a function called MASmooth() from the in the scaleOffse.R script
  # instability artifacts are identified when the midline exceeds the IQR
  # output is a vector of row indices where the mid line exceeded the interquartile range
  ###
  # q25 <- q25[1]
  # q75 <- q75[1]
  # iqr <- q75 - q25
  # # set the inner upper and lower fences
  # ifU <- q75 # + (.5 * iqr) 
  # ifL <- q25 # - (.5 * iqr)
  
  # locate rows where the midline exceeds the upper fence
  upLimit <- which(mid >= uf)
  # locate rows where the midline exceeds the lower fence
  lwLimit <- which(mid <= lf)
  # combine them
  unstableRows <- sort(unique(c(upLimit, lwLimit)))
  # output
  return(unstableRows)
} # end respTonicInstabilityFn




respInstabilityFn2 <- function(uf, lf, Inh, Exh) {
  # R function to evaluate respiration instability artifacts using the interquartile range
  # October 13, 2024
  # Raymond Nelson
  ###
  # uf is the upper fence, can be the 50th, 75th or 90th percentile
  # lf is the lower fence, such as the 50th, 25th or 10th percentile
  # Inh is the interpolated inhalation line
  # Exh is the interpolated exhalation line 
  # these lines are computed at the time of signal processing and scaling
  # by a function called interpolatePeaks() in the scaleOffse.R script
  ###
  # the inhalation line is expected to remain above the median
  # the exhalation line is expected to remain below the median
  # inhalation or exhalation values on the unexpected side of the 50th, 25th or 75th quantile may indicate instability
  ###
  # output is a vector of row indices 
  # where the inhalation or exhalation points exceeded the median
  ###
  # locate rows where the inhalation line exceeds the 25th percentile
  inhLimit <- which(Inh <= lf)
  # locate rows where the exhalation line exceeds the 75th percentile
  exhLimit <- which(Exh >= uf)
  # combine them
  unstableRows <- sort(unique(c(inhLimit, exhLimit)))
  # output
  return(unstableRows)
} # end respInstabilityFn2




respDesyncFn <- function(diffVc=diffVc, q1=q25diffVc, q2=q50diffVc, q3=q75diffVc, inF=1.5, outF=3, fences="inner", side="both") {
  # R function to identify badly desynchronized thoracic and abdominal respiration
  # using a Tukey fence
  ###
  # diffVc is the distance between thoracic and abdominal time series data
  # inF is the inner fence, in multiples of the IQR
  # inF is the outer fence, in multiples of the IQR
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # artifacts are marked when the difference exceeds the Tukey fences for the diff (upper - lower)
  # output is a vector of row number where the thoracic - abdominal distance exceeded the Tukey fence limits
  ###
  # plot.ts(diffVc)
  # plot.ts(tsUP)
  # plot.ts(tsLP)  
  names(diffVc) <- NULL
  ## adjust the data vector by removing the 1st and last sample rows to avoid extreme values
  diffVc <- diffVc[2:(length(diffVc)-1)]
  ## compute the quantiles
  # q1 <- quantile(diffVc, .25)
  # q2 <- quantile(diffVc, .5)
  # q3 <- quantile(diffVc, .75)
  # diffVc must be the same length as the time series data
  diffVc <- c(q2, diffVc, q2)
  ## compute the interquartile range of absolute mean differences.
  iqr <- abs(q3 - q1)
  ## adjust the min and max IQR to stabilize sensitivity
  # the IQR will get very small, leading to increased sensitivity with stable data
  # the IQR will get large with unstable data, leading to reduced sensitivity
  # minIQR <- scaleVals['uPneumo'] * .33
  # if(iqr <= minIQR) iqr <- minIQR
  # maxIQR <- scaleVals['uPneumo'] * .67
  # if(iqr >= maxIQR) iqr <- maxIQR
  ## set the Tukey fences
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  ## outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  ## compare the diffVc to the tukey fences 
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
  ## output
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



respActivityArtifactFn2 <- function(tsData=tsDataUp, tsMid=q50U, offset=0) {
  # R function to extract unstable segments of respiration data
  # compare min and max peaks to a mid line
  # extract biting, swallowing, coughing, and strange respiration cycles
  ###
  # tsData is the time series data for thoracic or abdominal respiration
  # tsMid is the mid line for the thoracic or abdominal respiration
  # offset is an adjustment value to be added or subtracted to a peak
  # to increase or decrease sensitivity
  # output is a vector of indices at which artifacts are found
  # artifacts are marked where upper or lower peaks are on an unexpected side of the mid line
  ###
  # plot.ts(tsData)
  ## make sure tsMid is a vector
  if(length(tsMid) < length(tsData)) tsMid <- rep(tsMid[1], times = length(tsData))
  ## call two functions to obtain the inhalation and exhalation peaks
  maxPeaks <- maxPeak(x=tsData, y=round(.25*cps,0))
  minPeaks <- minPeak(x=tsData, y=round(.25*cps,0))
  ## exclude the first and last indices
  maxPeaks <- maxPeaks[!(maxPeaks %in% c(1, length(tsData)))]
  minPeaks <- minPeaks[!(minPeaks %in% c(1, length(tsData)))]
  ## check for very small changes in slope at the inhalation or exhalation lines
  {
    # Oct 15, 2024
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
  ## max peaks below the mid line
  sigMax <- which((tsData[maxPeaks] - offset) <= tsMid[maxPeaks])
  ## min peaks above the mid line
  sigMin <- which((tsData[minPeaks] + offset) >= tsMid[minPeaks])
  ##  aggregate the artifact rows
  artifactRows <-
    unique(sort(c(maxPeaks[sigMax], minPeaks[sigMin])))
  ##
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
  # artifacts are indicated when the segmented excursion length is less than y
  ###
  # initialize an output vector
  outVc <- rep(0, length(x))
  # initialize the length of an apnea segment
  winLen <- c(1:round(w * cps))
  # select the segment indices to check
  winPoints <- c(1, cumsum(rep(round(length(winLen)/seg), (seg-1))), length(winLen))
  # iterate over the time series data with a moving window
  i=1
  for(i in 1:(length(outVc)-length(winLen)+1)) {
    # mark an artifact if the y axis change is small for the w period
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



respDBFn2 <- function(x=tsData, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="inner", side="upper") {
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
  # inF is the distance to the inner Tukey fence, in multiples of the iqr
  # outF is the distance to the outer Tukey fence, in multiples of the iqr
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # output is a vector of 0's with significant points indicated by the value 1
  ###
  # plot.ts(x)
  ## initialize the output vector
  outVc <- NULL
  ### calculate the interquartile range
  q3 <- q75[1]
  q2 <- q50[1]
  q1 <- q25[1]
  iqr <- abs(q3 - q1)
  ## fix the iqr for very stable or very unstable data
  {
    # sensitivity to DBs increases when the respiration data are very stable
    # this occurs because the iqr becomes restricted as a result of little variation
    # to reduce hypersensitivity a min iqr value 
    # can be obtained from the scalVals in the NCCAASCII_init.R scripe
    minIQR <- scaleVals['uPneumo'] * .15
    # commented out 2026Mar01
    # if(iqr <= minIQR) iqr <- minIQR
    # sensitivity to DBs decreases when the data are unstable
    # this is because the IQR becomes larger with more variation
    # to reduce hyposensitivyt the iqr can be replace with a max value
    maxIQR <- scaleVals['uPneumo'] * .67
    # commented out 2026Mar01
    # if(iqr >= maxIQR) iqr <- maxIQR
  }
  ## call a function to get the inhalation peaks peaks
  maxPeaks <- maxPeak(x)
  ## remove the first and last peaks
  maxPeaks <- maxPeaks[!(maxPeaks %in% c(1,length(x)))]
  ## remove peaks that are below the median
  maxPeaks <- maxPeaks[x[maxPeaks] >= rep(q2, times=length(maxPeaks))]
  ## get the peak values
  ## DAT <- (x[maxPeaks] - rep(2, times=length(maxPeaks)))
  DAT <- x[maxPeaks]
  ## set the Tukey fences
  # inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  # outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  ## compare the data values to the tukey fences
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
  ## ouput only the peak rows that exceed the tukey fences
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
  ## initialize the output vector
  outVc <- NULL
  ## calculate the quartiles
  q1 <- q25[1]
  qm <- q50[1]
  q3 <- q75[1]
  # compute the interquartile range of absolute mean differences.
  iqr <- (q3 - q1)
  ## fix the iqr for very stable or very unstable data
  {
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
  ## call a function to get exhalation peaks
  # minPeak function is in the sigProcHelper.R script
  minPeaks <- minPeak(x)
  ## remove the first and last peaks
  minPeaks <- minPeaks[!(minPeaks %in% c(1,length(x)))]
  ## remove peaks that are above the mid line
  minPeaks <- minPeaks[x[minPeaks] <= rep(qm, times=length(minPeaks))]
  ## calculate the difference in peak values
  DAT <- x[minPeaks]
  ## initialize the Tukey fences
  ## inner fences
  lfI <- q1 - (inF * iqr)
  ufI <- q3 + (inF * iqr)
  ## outer fences
  lfO <- q1 - (outF * iqr)
  ufO <- q3 + (outF * iqr)
  ## compare the data to the Tukey fences
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
  ## ouput only the peak rows that exceed the tukey fences
  outVc <- minPeaks[pkVc]
  return(outVc)
  # end baselineInstablityFn2()
}




########## main function ##############



# DAT=segmentDF
# DAT=chartDF



checkPneumoArtifactsFn <- function(DAT=segmentDF) { 
  # R Function to check for respiration artifacts during feature extraction
  # August 28, 2023 
  # Raymond Nelson 
  #### 
  # DAT is a data frame with the time series data and event markers
  # DAT needs to include the centered and smoothed columns
  # for thoracic and abdominal respiration data,
  # and other sensors,
  # in addition to the time scale, sample index column and events a stimulus segment
  #
  # DAT can be the segmentDF, 
  # with 10sec prestim, 15 sec stimulus segment, and 10sec poststim
  # 
  # DAT can also be the chartDF,
  # with all events, instructions, and annotations
  #
  # this function makes use of both thoracic and abdominal sensor data
  # c_UPneumoSm
  # c_LPneumoSm
  #
  # output is a vector of 0s the same length as the DAT
  # artifacted indices are are indicated with the "Artifact" message
  # 
  # this function is called by the pneumoExtractFn() in the PneumoExtract.R script,
  # during feature extraction
  #
  # November 9, 2026
  # modified to work with the chartDF,
  # so it can be called prior to feature extraction,
  # in this way, artifacts can be extracted from all events, 
  # not limited to RQs and CQs
  ####
  
  {
    examName <- DAT$examName[1]
    seriesName <- DAT$seriesName[1]
    chartName <- DAT$chartName[1]
    sensorName <- "pneumo"
  }
  
  {
    ## get the respiration data
    tsDataUp <- DAT$c_UPneumoSm
    tsDataLp <- DAT$c_LPneumoSm
    
    # get the mid lines (slow moving average)
    tsMidU <- DAT$c_UPneumoMid
    tsMidL <- DAT$c_LPneumoMid
    
    # tsMidU <- MASmooth(x=DAT$c_UPneumoSm, y=round(12.5*cps,0), times=4)
    # tsMidL <- MASmooth(x=DAT$c_LPneumoSm, y=round(12.5*cps,0), times=4)
    
    # compute the distance between the thoracic and abdominal data
    # diffVc <- (tsDataUp - tsDataLp)
    
    # use the pneumo scale value for the distance
    pnDist <- scaleVals[1] * 2
    
    # offset the upper pneumo mid line 
    tsMidUa <- tsMidU - quantile(tsMidU, .5)
    # offset the lower pneumo mid line
    tsMidLa <- tsMidL - quantile(tsMidL, .5)
    
    diffVc <- tsMidUa - tsMidLa
    diffVc <- diffVc - min(diffVc) + 1
    # max(diffVc)
  }
  
  {
    ## 2026Nov09
    ## get the answer indices
    answerRows <- which(DAT$Label %in% c("no", "yes", "No", "Yes", "NO", "YES", "Ans", "ans", "ANS"))
    
    # # response onset end and answer indices
    # # responseOnsetRow <- which(DAT$Events == "onsetRow")
    # responseOnsetRow <- 301
    # # responseEndRow <- which(DAT$Events == "offsetRow")
    # responseEndRow <- responseOnsetRow + (15*cps) - 1
    
    # answerRow <- which(DAT$Events == "answerRow")
    # do it this way so that we don't inadvertently make answer rows for annotations
    answerRow <- which(DAT$Label %in% c("no", "yes", "No", "Yes", "NO", "YES", "Ans", "ans", "ANS"))
  }
  
  {  
    ## lines/values needed for analysis
    ## these are computed during scaling and offseting
    
    # get the quantile values - upper pneumo
    q025U <- quantile(DAT$c_UPneumoSm, .025)
    q05U <- quantile(DAT$c_UPneumoSm, .05)
    q10U <- quantile(DAT$c_UPneumoSm, .10)
    q25U <- DAT$c_UPneumo_Q25[1]
    q40U <- quantile(DAT$c_UPneumoSm, .40)
    q50U <- DAT$c_UPneumo_Q50[1]
    q60U <- quantile(DAT$c_UPneumoSm, .60)
    q75U <- DAT$c_UPneumo_Q75[1]
    q90U <- quantile(DAT$c_UPneumoSm, .90)
    q95U <- quantile(DAT$c_UPneumoSm, .95)
    q975U <- quantile(DAT$c_UPneumoSm, .975)
    
    # quantile values - lower pneumo
    q025L <- quantile(DAT$c_LPneumoSm, .025)
    q05L <- quantile(DAT$c_LPneumoSm, .05)
    q10L <- quantile(DAT$c_LPneumoSm, .10)
    q25L <- DAT$c_LPneumo_Q25[1]
    q40L <- quantile(DAT$c_LPneumoSm, .40)
    q50L <- DAT$c_LPneumo_Q50[1]
    q60L <- quantile(DAT$c_LPneumoSm, .60)
    q75L <- DAT$c_LPneumo_Q75[1]
    q90L <- quantile(DAT$c_LPneumoSm, .90)
    q95L <- quantile(DAT$c_LPneumoSm, .95)
    q975L <- quantile(DAT$c_LPneumoSm, .975)
    
    # get the inhalation and exhalation lines
    tsInhU <- DAT$c_UPneumoInh
    tsInhL <- DAT$c_LPneumoInh
    tsExhU <- DAT$c_UPneumoExh
    tsExhL <- DAT$c_LPneumoExh
    
    # compute the quantiles for the difference vector
    q25diffVc <- quantile(diffVc, .25)
    q50diffVc <- quantile(diffVc, .50)
    q75diffVc <- quantile(diffVc, .75)
    
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
    assign("q10U", q10U, envir=.GlobalEnv)
    assign("q25U", q25U, envir=.GlobalEnv)
    assign("q40U", q40U, envir=.GlobalEnv)
    assign("q50U", q50U, envir=.GlobalEnv)
    assign("q60U", q60U, envir=.GlobalEnv)
    assign("q75U", q75U, envir=.GlobalEnv)
    assign("q90U", q90U, envir=.GlobalEnv)
    assign("q10L", q10L, envir=.GlobalEnv)
    assign("q25L", q25L, envir=.GlobalEnv)
    assign("q40L", q40L, envir=.GlobalEnv)
    assign("q50L", q50L, envir=.GlobalEnv)
    assign("q60L", q60L, envir=.GlobalEnv)
    assign("q75L", q75L, envir=.GlobalEnv)
    assign("q90L", q90L, envir=.GlobalEnv)
    assign("tsInhU", tsInhU, envir=.GlobalEnv)
    assign("tsInhL", tsInhL, envir=.GlobalEnv)
    assign("tsExhU", tsExhU, envir=.GlobalEnv)
    assign("tsExhL", tsExhL, envir=.GlobalEnv)
    assign("q25diffVc", q25diffVc, envir=.GlobalEnv)
    assign("q50diffVc", q50diffVc, envir=.GlobalEnv)
    assign("q75diffVc", q75diffVc, envir=.GlobalEnv)
    assign("responseOnsetRow", responseOnsetRow, envir=.GlobalEnv)
    assign("responseEndRow", responseEndRow, envir=.GlobalEnv)
    assign("answerRow", answerRow, envir=.GlobalEnv)
    assign("segmentDF", DAT, envir=.GlobalEnv)
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
    # # fix some missing parameters
    # if(is.null(responseOnsetRow)) responseOnsetRow <- length(tsDataUp)
    # if(is.null(responseEndRow)) responseEndRow <- length(tsDataUp)
    # if(responseEndRow > length(tsDataUp)) responseEndRow <- length(tsDataUp)
    # # check for missing answer
    # answerRow <- answerRow[which((answerRow <= responseEndRow) & (answerRow >= responseOnsetRow))]
    # if(length(answerRow) == 0) answerRow <- NULL
    # # check for very late answer
    # if(!is.null(answerRow) && answerRow >= length(tsDataUp)) answerRow <- NULL
  }
  
  # assign("segmentDF", DAT, envir = .GlobalEnv)
  # stop()
  
  #### initialize a vector for the verbal answer buffer rows ####
  
  ansBuffRows <- NULL
  
  {
    # compute the answer distortion buffer
    if(!is.null(answerRow) && length(answerRow) != 0) {
      # a loop is necesary when using the chartDF, not for the segmentDF
      for(i in 1:length(answerRow)) {
        ansBuffOn <- answerRow[i] - round((pneumoAnsBuff * cps)) - 0
        ansBuffOff <- answerRow[i] + round((pneumoAnsBuff * cps)) + 0
        # make sure the answer buffer does not exceed the segment data frame
        if(ansBuffOn < 1) ansBuffOn <- 1
        if(ansBuffOff > nrow(DAT)) ansBuffOff <- nrow(DAT)
        # if(ansBuffOn[1] < 1) ansBuffOn[1] <- 1
        # if(ansBuffOff[length(ansBuffOff)] > nrow(DAT)) ansBuffOff[length(ansBuffOff)] <- nrow(DAT)
        # if(any(ansBuffOn < 1)) ansBuffOn[which(ansBuffOn < 1)] <- 1
        # if(any(ansBuffOff > nrow(DAT))) ansBuffOff[which(ansBuffOff > nrow(DAT))] <- nrow(DAT)
        ansBuffRows <- c(ansBuffRows, c(ansBuffOn:ansBuffOff))
      }
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
    # ## compute some vectors for the prestim, stim, and poststim rows
    # preStimRows <- c(1:(responseOnsetRow-1))
    # stimSegRows <- c(responseOnsetRow:(responseOnsetRow+round(measuredSeg*cps)-1))
    # postStimRows <- c((responseOnsetRow+round(measuredSeg*cps)-1):length(tsDataUp))
  }
  
  {
    ## initialize the segments indices
    # prestimSegUp <- tsDataUp[preStimRows]
    # prestimSegLp <- tsDataLp[preStimRows]
    # stimSegUp <- tsDataUp[stimSegRows]
    # stimSegLp <- tsDataLp[stimSegRows]
    # poststimSegUp <- tsDataUp[postStimRows]
    # poststimSegLp <- tsDataLp[postStimRows]
    
    # prestimDiffUp <- diffUp[preStimRows]
    # prestimDiffLp <- diffLp[preStimRows]
    # stimDiffUp <- diffUp[stimSegRows]
    # stimDiffLp <- diffLp[stimSegRows]
    # poststimDiffUp <- diffUp[postStimRows]
    # poststimDiffLp <- diffLp[postStimRows]
    
    ## these are used for ratio comparison of RLE measurments
  } 
  
  #### get the measurements without excluding the answer distortion  ####
  
  {
    # prestimMeasureUp <- pneumoMeasurementFn(prestimSegUp, verbalAnswer=NULL)
    # prestimMeasureLp <- pneumoMeasurementFn(prestimSegLp, verbalAnswer=NULL)
    # 
    # stimSegMeasureUp <- pneumoMeasurementFn(stimSegUp, verbalAnswer=NULL)
    # stimSegMeasureLp <- pneumoMeasurementFn(stimSegLp, verbalAnswer=NULL)
    # 
    # poststimMeasureUp <- pneumoMeasurementFn(poststimSegUp, verbalAnswer=NULL)
    # poststimMeasureLp <- pneumoMeasurementFn(poststimSegLp, verbalAnswer=NULL)
    
    ## these are used for ratio comparison of RLE measurments
  }
  
  #### 1. evaluate the desynchronization and distance between the respiration data ####
  
  {
    
    
    # plot.ts(tsMidU)
    # plot.ts(tsMidL)

    # diffVc <- tsMidU - tsMidL
    # diffVc <- (tsDataUp - tsDataLp)
    # plot.ts(diffVc)
    
    # diffVc is the difference between the thoracic and abdominal respiration lines
    
    # artifacts are marked when the difference exceeds the Tukey fences

    artifactRowsDesync <- respDesyncFn(diffVc=diffVc, q1=q25diffVc, q2=q50diffVc, q3=q75diffVc, inF=1.5, outF=3, fences="outer", side="both")

    # if(length(artifactRowsDesync) > 0) {
    #   assign("segmentDF", DAT, envir=.GlobalEnv)
    #   stop()
    # }

    # remove desynchronization artifacts from the answer buffer
    artifactRowsDS <- artifactRowsDesync[!(artifactRowsDesync %in% ansBuffRows)]

    # desynchronization artifacts are marked at both abdominal and thoracic sensors
    outVcDF$outVcU[artifactRowsDS] <- "ArtifactDS"
    outVcDF$outVcL[artifactRowsDS] <- "ArtifactDS"
    
  }
  
  #### 2. evaluate the tonic stability of the respiration data using the mid line ####
  
  {
    
    # using the midline, computed during signal processing, along with upper and lower fences
    # artifacts are marked where the midline equals or exceeds the limits
    
    tonicInstabilityRowsUP <- respTonicInstabilityFn(lf=q05U, uf=q95U, mid=tsMidU)
    tonicInstabilityRowsLP <- respTonicInstabilityFn(lf=q05L, uf=q95L, mid=tsMidL)
    
    # combine the upper and lower artifact vectors
    tonicInstabilityRowsUL <- sort(unique(c(tonicInstabilityRowsUP, tonicInstabilityRowsLP)))
    
    # remove tonic instability artifacts from the answer buffer
    # tonicInstabilityRowsUL <- tonicInstabilityRowsUL[!(tonicInstabilityRowsUL %in% ansBuffRows)]
    
    outVcDF$outVcU[as.numeric(tonicInstabilityRowsUP)] <- "ArtifactTonicity"
    outVcDF$outVcL[as.numeric(tonicInstabilityRowsLP)] <- "ArtifactTonicity"
    
  }
  
  #### 3. evaluate respiration instability using the inhalation and exhalation lines ####
  
  {
    
    # artifacts are marked when the inhalation line is less than the 25th quantile
    # or when the exhalation line is greater than the 75th quantile
    
    instabilityRowsUP <- respInstabilityFn2(uf=q75U, lf=q25U, Inh=tsInhU, Exh=tsExhU)
    instabilityRowsLP <- respInstabilityFn2(uf=q75L, lf=q25L, Inh=tsInhL, Exh=tsExhL)
    
    # combine the upper and lower artifact vectors
    instabilityRowsUL <- sort(unique(c(instabilityRowsUP, instabilityRowsLP)))
    
    {
      # remove activity instability artifacts from the answer buffer
      instabilityRowsUP <- instabilityRowsUP[!(instabilityRowsUP %in% ansBuffRows)]
      instabilityRowsLP <- instabilityRowsLP[!(instabilityRowsLP %in% ansBuffRows)]
      
      # remove tonic instability artifacts from the answer buffer
      instabilityRowsUL <- instabilityRowsUL[!(instabilityRowsUL %in% ansBuffRows)]
    }

    # submit the instability artifacts to the output buffer
    outVcDF$outVcU[as.numeric(instabilityRowsUP)] <- "ArtifactInst"
    outVcDF$outVcL[as.numeric(instabilityRowsLP)] <- "ArtifactInst"
    
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
  
  #### compare the ratio of prestimulus and stimulus measurements ####
   
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
  
  #### 4. check for activity artifact - upper and lower peaks with unexpected values ####
  
  {
    ## biting, swallowing, tussis, other distortions ##
    
    # artifacts are marked where upper or lower peaks are on an unexpected side of the mid line

    # Call a function to separately compare the upper and lower peaks to a mid line
    artifactRowsUp <- respActivityArtifactFn2(tsData=tsDataUp, tsMid=q50U)
    artifactRowsLp <- respActivityArtifactFn2(tsData=tsDataLp, tsMid=q50L)

    # plot.ts(tsDataUP)
    # plot.ts(tsDataMidU)

    # exclude artifacts around the verbal answer
    artifactRowsUp <- artifactRowsUp[!(artifactRowsUp %in% ansBuffRows)]
    artifactRowsLp <- artifactRowsLp[!(artifactRowsLp %in% ansBuffRows)]

    # combine the upper and lower artifact vectors
    # artifactRowsUL <- sort(unique(c(artifactRowsUp, artifactRowsLp)))

    # outVcDF[artifactRows] <- "Artifact"
    outVcDF$outVcU[artifactRowsUp] <- "ArtifactBU"
    outVcDF$outVcL[artifactRowsLp] <- "ArtifactBL"
  } 
  
  #### 5. evaluate the data for deep breaths ####
  
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
    
    # artifactRowsDB <- sort(unique(c(artifactRows7, artifactRows8)))
    
    # keep the thoracic and abdominal separate to avoid excess data loss 
    outVcDF$outVcU[artifactRows7] <- "ArtifactDBU"
    outVcDF$outVcL[artifactRows8] <- "ArtifactDBL"
  } 
  
  #### 6. evaluate the data for BIG deep breaths ####
  
  {
    # BIG deep breaths are not removed from the answering buffer
    
    # call a function to locate deep breath artifacts
    # artifactRows7 <- respDBFn(x=tsDataUp, mid=tsMidU)
    # artifactRows8 <- respDBFn(x=tsDataLp, mid=tsMidL)
    # # this function will return the row index where deep breath peaks are located
    
    # quantiles were initialized earlier in this function 
    
    artifactRows9 <- respDBFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="outer", side="upper")
    artifactRows10 <- respDBFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1.5, outF=3, fences="outer", side="upper")
    # this function will return the row index where deep breath peaks are located
    
    # exclude artifacts surrounding the verbal answer
    # artifactRows9 <- artifactRows7[!(artifactRows7 %in% ansBuffRows)]
    # artifactRows10 <- artifactRows8[!(artifactRows8 %in% ansBuffRows)]
    
    artifactRowsDB <- sort(unique(c(artifactRows9, artifactRows10)))
    
    # keep the thoracic and abdominal separate sto avoid excess data loss 
    outVcDF$outVcU[artifactRowsDB] <- "ArtifactBDB"
    outVcDF$outVcL[artifactRowsDB] <- "ArtifactBDB"
  } 
  
  #### 7. evaluate the data for apnea ####
  
  {
    UPApn <- respApneaFn(x=tsDataUp)
    LPApn <- respApneaFn(x=tsDataLp)
    
    # artifacts are indicated when the segmented excursion length for a 3.75 second moving window, 
    # equal to one respiration cycle at the median rate of 16 cpm, is less than 
    # a threashold value of 1% of the y-axis range
    
    # exclude artifacts surrounding the verbal answer
    # UPApn <- UPApn[!(UPApn %in% ansBuffRows)]
    # LPApn <- LPApn[!(LPApn %in% ansBuffRows)]
    # 
    # combine both respiration sensors
    apneaArtifactsUL <- sort(unique(c(UPApn, LPApn)))
    
    # is not apnea if respiration acitivity is extant in one of the sensors
    # apneaArtifacts <- apneaArtifacts[which(apneaArtifacts %in% UPApn & apneaArtifacts %in% LPApn)]
    
    outVcDF$outVcU[apneaArtifactsUL] <- "ArtifactAP"
    outVcDF$outVcL[apneaArtifactsUL] <- "ArtifactAP"
  }
  
  #### 8. evaluate the baseline for artifacts ####
  
  {
    
    # call a function to locate baseline instability artifacts artifacts
    # baselineArtifactsUp <- baselineInstablityFn(x=tsDataUp, mid=tsMidU, inF=1.5, outF=3, fences="inner", side="both")
    # baselineArtifactsLp <- baselineInstablityFn(x=tsDataLp, mid=tsMidL, inF=1.5, outF=3, fences="inner", side="both")
    
    baselineArtifactsUp <- baselineInstablityFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1, outF=2, fences="inner", side="lower")
    baselineArtifactsLp <- baselineInstablityFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1, outF=2, fences="inner", side="lower")
    
    # exclude artifacts surrounding the verbal answer
    # baselineArtifactsUp <- baselineArtifactsUp[!(baselineArtifactsUp %in% ansBuffRows)]
    # baselineArtifactsLp <- baselineArtifactsLp[!(baselineArtifactsLp %in% ansBuffRows)]
    
    # compbine the baseline artifacts for the thoracic and abdominal sensors
    # baselineArtifactsUL <- sort(unique(c(baselineArtifactsUp, baselineArtifactsLp)))
    
    # keep the thoracic and abdominal separate to avoid excess data loss 
    outVcDF$outVcU[baselineArtifactsUp] <- "ArtifactBLU"
    outVcDF$outVcL[baselineArtifactsLp] <- "ArtifactBLL"
    
  }
  
  #### 9. evaluate the baseline for BIG artifacts ####
  
  {

    # call a function to locate baseline instability artifacts artifacts
    # baselineArtifactsUp <- baselineInstablityFn(x=tsDataUp, mid=tsMidU, inF=1.5, outF=3, fences="outer", side="both")
    # baselineArtifactsLp <- baselineInstablityFn(x=tsDataLp, mid=tsMidL, inF=1.5, outF=3, fences="outer", side="both")

    baselineArtifactsUp <- baselineInstablityFn2(x=tsDataUp, q25=q25U, q50=q50U, q75=q75U, inF=1.5, outF=3, fences="outer", side="lower")
    baselineArtifactsLp <- baselineInstablityFn2(x=tsDataLp, q25=q25L, q50=q50L, q75=q75L, inF=1.5, outF=3, fences="outer", side="lower")

    # exclude artifacts surrounding the verbal answer
    # baselineArtifactsUp <- baselineArtifactsUp[!(baselineArtifactsUp %in% ansBuffRows)]
    # baselineArtifactsLp <- baselineArtifactsLp[!(baselineArtifactsLp %in% ansBuffRows)]

    # compbine the baseline artifacts for the thoracic and abdominal sensors
    baselineArtifactsUL <- sort(unique(c(baselineArtifactsUp, baselineArtifactsLp)))

    # keep the thoracic and abdominal separate to avoid excess data loss
    # outVcDF$outVcU[baselineArtifactsUL] <- "ArtifactBBLU"
    # outVcDF$outVcL[baselineArtifactsUL] <- "ArtifactBBLL"

  }
  
  #### exclude respiration artifacts in the poststimulus segment 
  
  {
    
    # outVcDF$outVcU[postStimRows] <- "0"
    # outVcDF$outVcL[postStimRows] <- "0"
    
  }
  
  #### submit the artifacts to the input data frame and output the chartDF ####
  
  {
    
    # 2026Nov13
    
    
  }
  
  #### output ####
  
  # return(outVc)
  # output is a vector of 0s the same length as the DAT
  # artifacted indices are are indicated with the "Artifact" message
  return(outVcDF)
  
  # end checkPneumoArtifactsFn()
} 





