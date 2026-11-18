# R Functions to check for respiration artifacts during feature extraction
# November 13, 2026
# Raymond Nelson 
####
# taken from the checkPneumoArtifact.R script that was sourced during feature extraction
# this function is sourced during artifact processing
####
#
# respDesyncFn - to determine badly descynchronized thoracic and abdominal data 
# respTonicInstabilityFn - tonic instability using the mid line
# respInstabilityFn - inhalation and exhalation line instability 
# respActivityArtifactFn - another method for biting, swallowing and strange activity
# respApneaFn - to identify apnea during testing 
# respTukeyFn - to identify deep breaths and baseline artifacts
#
## Main Function ##
# checkPneumoArtifactsFn - to call the helper functions for different types of resp artifacts
#
####



# source a script for helper functions
# minPeak
# maxPeak
# source(paste0(RPath, "Parse/sigProcHelper.R", echo=FALSE)
# this is already sourced by the workFlow.R script during signal processing





respDesyncFn <- function(diffVc=diffVc, inF=1.5, outF=3, fences="inner", side="both") {
  # R function to identify desynchronized thoracic and abdominal respiration activity
  # using a Tukey fence
  # November 1, 2026
  # Raymond Nelson
  #### 
  # diffVc is the distance between thoracic and abdominal time series data
  # inF is the inner fence, in multiples of the IQR
  # inF is the outer fence, in multiples of the IQR
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # artifacts are marked when the difference exceeds the Tukey fences for the diff (upper - lower)
  # output is a vector of row number where the thoracic - abdominal distance exceeded the Tukey fence limits
  ###
  names(diffVc) <- NULL
  ## adjust the data vector by removing the 1st and last sample rows to avoid extreme values
  diffVc <- diffVc[2:(length(diffVc)-1)]
  ## compute the quantiles
  q1 <- quantile(diffVc, .25)
  q2 <- quantile(diffVc, .5)
  q3 <- quantile(diffVc, .75)
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



respTonicInstabilityFn <- function(mid, lf, uf) {
  # R function to evaluate respiration tonic instability artifacts
  # using the mid line and limiting values computed from the IQR
  # November 12, 2026
  # Raymond Nelson
  ####
  # mid is the respiration mid line - computed via a smoothing function
  # these lines are computed at the time of signal processing and scaling
  # by a function called MASmooth() from the in the scaleOffset.R script
  # lf is the lower Tukey fence computed from the 25th and 75th percentiles
  # it can also be  10th percentile for the entire chart or from X to XX
  # uf is upper tukey fence, and can also be the 90th percentile
  # instability artifacts are identified when the midline exceeds the IQR
  # output is a vector of row indices where the mid line exceeded the lf and uf limits
  ###
  # locate rows where the midline exceeds the upper fence
  upLimit <- which(mid >= uf)
  # locate rows where the midline exceeds the lower fence
  lwLimit <- which(mid <= lf)
  # combine them
  unstableRows <- sort(unique(c(upLimit, lwLimit)))
  # output
  return(unstableRows)
  # end respTonicInstabilityFn
} 



respInstabilityFn <- function(Inh, Exh, lf, uf) {
  # R function to evaluate respiration instability artifacts 
  # inhalation and exhalation lines and limiting values computed from the IQR
  # October 13, 2026
  # Raymond Nelson
  ###
  # Inh is the interpolated inhalation line
  # Exh is the interpolated exhalation line 
  # these lines are computed at the time of signal processing and scaling
  # by a function called interpolatePeaks() in the scaleOffset.R script
  # lf is the lower fence, such as the lower tukey fence, median, or other quantile
  # uf is the upper fence, such as the upper tukey fence, median, or oither quantile
  ###
  # the inhalation line is expected to remain above the lf value
  # the exhalation line is expected to remain below the uf value
  # inhalation or exhalation values on the unexpected side of the fences may indicate instability
  ###
  # output is a vector of row indices 
  # where the inhalation or exhalation points exceeded the lf and uf values
  ###
  # locate rows where the inhalation line exceeds the 25th percentile
  inhLimit <- which(Inh <= lf)
  # locate rows where the exhalation line exceeds the 75th percentile
  exhLimit <- which(Exh >= uf)
  # combine them
  unstableRows <- sort(unique(c(inhLimit, exhLimit)))
  # output
  return(unstableRows)
} # end respInstabilityFn



respPeakInstabilityFn <- function(tsData=tsDataUp, med=q50U, offset=0) {
  # R function to extract unstable segments of respiration data using peaks
  # November 5, 2026
  # Raymond Nelson
  ###
  # compare min and max peaks to a mid line
  # extract biting, swallowing, coughing, and strange respiration cycles
  ###
  # tsData is the time series data for thoracic or abdominal respiration
  # med is the mid line for the thoracic or abdominal respiration
  # med can also be the median
  # offset is an adjustment value to be added or subtracted to a peak
  # to increase or decrease sensitivity
  # output is a vector of indices at which artifacts are found
  # artifacts are marked where upper or lower peaks are on an unexpected side of the mid line
  ### 
  # this function calls the maxPeaks and minPeak functions
  # in the sigProcHelper.R script
  ###
  
  # ## make sure s is a vector
  # if(length(med) < length(tsData)) med <- rep(med[1], times = length(tsData))
  
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
      # check the sum of absolute differences and ignore small changes in slope (less than 0.5%)
      if(sum(abs(diff(c(minMaxVals[(i-1)], minMaxVals[i], minMaxVals[(i+1)])))) <= 10) minMaxPeaks2[i] <- NA
    }
    minMaxPeaks2 <- minMaxPeaks2[!is.na(minMaxPeaks2)]
    maxPeaks <- maxPeaks[maxPeaks %in% minMaxPeaks2]
    minPeaks <- minPeaks[minPeaks %in% minMaxPeaks2]
  }
  
  ## max peaks below the mid line
  sigMax <- which((tsData[maxPeaks] - offset) <= med)
  ## min peaks above the mid line
  sigMin <- which((tsData[minPeaks] + offset) >= med)
  ##  aggregate the artifact rows
  artifactRows <- unique(sort(c(maxPeaks[sigMax], minPeaks[sigMin])))
  ##
  return(artifactRows)
  # end respPeakInstabilityFn()
}



respApneaFn <- function(x=segmentDF$c_UPneumoSm, w=3.75, y=20, seg=4) {
  # R function to extract respiration apnea from a stimulus segment
  # October 13, 2024
  # Raymond Nelson
  ####
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



respTukeyFn <- function(tsData=tsDataUp, inF=1.5, outF=3, peaks="both", fences="inner", side="upper") {
  # R function to extract deep breaths and sudden downward baseline activity
  # using a Tukey fence method
  # November 18, 2026
  # Raymond Nelson
  ####
  # uses inhalation and exhalation peaks and compares them to the Tukey fences
  ####
  # tsData input is a time series vector for thoracic or abdominal respiration
  # inF is the distance to the inner Tukey fence, in multiples of the iqr
  # outF is the distance to the outer Tukey fence, in multiples of the iqr
  # peaks can be "inhalation" "exhalation" or "both"
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  # output is a vector of 0's with significant points indicated by the value 1
  ###
  # calls the minPeaks() and maxPeaks() functions from the sigProcHelper.R script
  ###
  # plot.ts(x)
  ## initialize the output vector
  outVc <- NULL
  ### calculate the interquartile range
  q3 <- quantile(tsData[c(2:(length(tsData)-1))], .75) 
  q2 <- quantile(tsData[c(2:(length(tsData)-1))], .5) 
  q1 <- quantile(tsData[c(2:(length(tsData)-1))], .25) 
  iqr <- abs(q3 - q1)
  ## fix the iqr for very stable or very unstable data
  {
    # # sensitivity to DBs increases when the respiration data are very stable
    # # this occurs because the iqr becomes restricted as a result of little variation
    # # to reduce hypersensitivity a min iqr value 
    # # can be obtained from the scalVals in the NCCAASCII_init.R scripe
    # minIQR <- scaleVals['uPneumo'] * .15
    # # commented out 2026Mar01
    # # if(iqr <= minIQR) iqr <- minIQR
    # # sensitivity to DBs decreases when the data are unstable
    # # this is because the IQR becomes larger with more variation
    # # to reduce hyposensitivyt the iqr can be replace with a max value
    # maxIQR <- scaleVals['uPneumo'] * .67
    # # commented out 2026Mar01
    # # if(iqr >= maxIQR) iqr <- maxIQR
  }
  {
    ## call a function to get the inhalation peaks
    maxPeaks <- maxPeak(tsData)
    ## remove the first and last peaks if they occur at the begining or end of the data vector
    maxPeaks <- maxPeaks[!(maxPeaks %in% c(1,length(tsData)))]
    ## remove peaks that are below the median
    maxPeaks <- maxPeaks[tsData[maxPeaks] >= q2]
    ## call a function to get the exhalation troughs (lower peaks)
    minPeaks <- minPeak(tsData)
    ## remove the first and last peaks if they occur at the begining or end of the data vector
    minPeaks <- minPeaks[!(minPeaks %in% c(1,length(tsData)))]
    ## remove peaks that are above the mid line
    minPeaks <- minPeaks[tsData[minPeaks] <= q2]
    if(peaks=="inhalation") {
      peakIdcs <- maxPeaks
    } else if(peaks=="exhalation") {
      peakIdcs <- minPeaks
    } else {
      peakIdcs <- sort(c(maxPeaks, minPeaks))
    }
  }
  ## get the peak values
  DAT <- tsData[peakIdcs]
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
  outVc <- peakIdcs[pkVc]
  return(outVc)
  # end respTukeyFn()
}



maxPeakFn <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # R function to get the cyclic peaks from the time series data
  # November 3, 2026
  # Raymond Nelson
  ####
  # x input is a time series vector
  # y input is the number of offset samples 
  # will keep the index number of max peak samples
  # firstLast 
  ###
  # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
  xOut <- rep(NA, times=(length(x)))
  if(firstLast==TRUE){
    xOut[1] <- 1 # keep the first
    xOut[length(xOut)] <- length(xOut) # keep the last
  }
  # buffer will be double the offset value
  input_buffer <- x[2:(2*y+1)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    # check to see if the middle value of the buffer is the max
    ifelse(input_buffer[(y+1)]==max(input_buffer),
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           # work on this 4/23/2016
           # another way is to
           # create a buffer of NA and keep only the i+y+1 set to the row index
           # slice the buffer into the vector 
           # the result should be the same
           # non max samples in the buffer are NA
           next()
    )
  } # end for loop
  return(as.numeric(na.omit(xOut)))
}



minPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # R function to get the cyclic peaks from the time series data
  # November 3, 2026
  # Raymond Nelson
  ####
  # will keep the index number of min peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
  xOut <- rep(NA, times=(length(x)))
  if(firstLast==TRUE){
    xOut[1] <- 1 # keep the first
    xOut[length(xOut)] <- length(xOut) # keep the last
  }
  # buffer will be double the offset value
  input_buffer <- x[2:(2*y+1)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    # check to determine if the middle of the buffer (y) is the min
    ifelse(input_buffer[(y+1)]==min(input_buffer), #
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           next()
    )
  } # end for loop
  return(as.numeric(na.omit(xOut)))
}



########## main function ##############



newPneumoCheckFn <- function(chartDF=chartDF) { 
  # R Function to check for respiration artifacts during artifact processing
  # November 13, 2026
  # Raymond Nelson 
  ####
  # main function to check for significant physical activity 
  # input is the chartDF
  # output is the chartDF with artifacts labeled in the Move1_a vector
  ####
  # also calls other helper functions in this script
  #
  # this function makes use of both thoracic and abdominal sensor data
  # c_UPneumoSm
  # c_LPneumoSm
  #
  # this function is called by the artifactProcFn() in the artifactProc.R script,
  #
  ####
  
  {
    # chart information
    examName <- chartDF$examName[1]
    seriesName <- chartDF$seriesName[1]
    chartName <- chartDF$chartName[1]
    sensorName <- "pneumo"
  }
  
  {
    # reset the artifact columns
    chartDF$UPneumo_a <- "0"
    chartDF$LPneumo_a <- "0"
    chartDF$Pneumo_a <- "0"
  }
  
  {
    ## get the respiration data
    tsDataUp <- chartDF$c_UPneumoSm
    tsDataLp <- chartDF$c_LPneumoSm
    
    if(length(tsDataUp) != length(tsDataLp)) {
      stop("problem with pneumo data")
    }
    
    # get the mid lines (slow moving average)
    tsMidU <- chartDF$c_UPneumoMid
    tsMidL <- chartDF$c_LPneumoMid
    
    # tsMidU <- MASmooth(x=chartDF$c_UPneumoSm, y=round(12.5*cps,0), times=4)
    # tsMidL <- MASmooth(x=chartDF$c_LPneumoSm, y=round(12.5*cps,0), times=4)
    
    # use the pneumo scale value for the distance
    # pnDist <- scaleVals[1] * 2
    
    # offset the upper and lower pneumo mid lines
    tsMidUa <- tsMidU - quantile(tsMidU, .5)
    tsMidLa <- tsMidL - quantile(tsMidL, .5)
    
    # compute the distance between the thoracic and abdominal data
    diffVc <- tsMidUa - tsMidLa
    # diffVc <- diffVc - min(diffVc) + 1
  }
  
  {
    ## get the answer indices
    answerRows <- which(chartDF$Label %in% c("no", "yes", "No", "Yes", "NO", "YES", "Ans", "ans", "ANS"))
  }
  
  {  
    ## lines/values needed for analysis
    ## these are computed during scaling and offseting
    
    # get the quantile values - upper pneumo
    q25U <- chartDF$c_UPneumo_Q25[1]
    q50U <- chartDF$c_UPneumo_Q50[1]
    q75U <- chartDF$c_UPneumo_Q75[1]
    
    # quantile values - lower pneumo
    q25L <- chartDF$c_LPneumo_Q25[1]
    q50L <- chartDF$c_LPneumo_Q50[1]
    q75L <- chartDF$c_LPneumo_Q75[1]
    
    # get the inhalation and exhalation lines for the upper and lower pneumo
    tsInhU <- chartDF$c_UPneumoInh
    tsExhU <- chartDF$c_UPneumoExh
    tsInhL <- chartDF$c_LPneumoInh
    tsExhL <- chartDF$c_LPneumoExh
    
    # plot.ts(tsInhU)
    # plot.ts(tsMidU)
    
    ## not computed during signal processing
    
    q025U <- quantile(chartDF$c_UPneumoSm, .025)
    q05U <- quantile(chartDF$c_UPneumoSm, .05)
    q10U <- quantile(chartDF$c_UPneumoSm, .10)
    q40U <- quantile(chartDF$c_UPneumoSm, .40)
    q60U <- quantile(chartDF$c_UPneumoSm, .60)
    q90U <- quantile(chartDF$c_UPneumoSm, .90)
    q95U <- quantile(chartDF$c_UPneumoSm, .95)
    q975U <- quantile(chartDF$c_UPneumoSm, .975)
    
    
    q025L <- quantile(chartDF$c_LPneumoSm, .025)
    q05L <- quantile(chartDF$c_LPneumoSm, .05)
    q10L <- quantile(chartDF$c_LPneumoSm, .10)
    q40L <- quantile(chartDF$c_LPneumoSm, .40)
    q60L <- quantile(chartDF$c_LPneumoSm, .60)
    q90L <- quantile(chartDF$c_LPneumoSm, .90)
    q95L <- quantile(chartDF$c_LPneumoSm, .95)
    q975L <- quantile(chartDF$c_LPneumoSm, .975)
    
  }
  
  {
    ## save the values to the global envir for inspection
    assign("chartDF", chartDF, envir=.GlobalEnv)
    assign("chartName", chartName, envir=.GlobalEnv)
    assign("seriesName", seriesName, envir=.GlobalEnv)
    assign("examName", examName, envir=.GlobalEnv)
    assign("sensorName", sensorName, envir=.GlobalEnv)
    
    assign("tsDataUp", tsDataUp, envir=.GlobalEnv)
    assign("tsDataLp", tsDataLp, envir=.GlobalEnv)
    assign("tsMidU", tsMidU, envir=.GlobalEnv)
    assign("tsMidL", tsMidL, envir=.GlobalEnv)
    
    assign("diffVc", diffVc, envir=.GlobalEnv)
    
    # assign("q25diffVc", q25diffVc, envir=.GlobalEnv)
    # assign("q50diffVc", q50diffVc, envir=.GlobalEnv)
    # assign("q75diffVc", q75diffVc, envir=.GlobalEnv)
    
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
    
    # not computed during signal processing,
    # these are computed in this script
    assign("q10U", q10U, envir=.GlobalEnv)
    assign("q40U", q40U, envir=.GlobalEnv)
    assign("q60U", q60U, envir=.GlobalEnv)
    assign("q90U", q90U, envir=.GlobalEnv)
    assign("q10L", q10L, envir=.GlobalEnv)
    assign("q40L", q40L, envir=.GlobalEnv)
    assign("q60L", q60L, envir=.GlobalEnv)
    assign("q90L", q90L, envir=.GlobalEnv)
    
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
  
  #### initialize some vectors to hold the artifact indices ####
  
  {
    outVcU <- rep(0, times=length(tsDataUp))
    outVcL <- rep(0, times=length(tsDataLp))
    
    # and an output data frame
    outVcDF <- cbind.data.frame(outVcU, outVcL)
  }
  
  # assign("chartDF", chartDF, envir = .GlobalEnv)
  # stop()
  
  #### initialize a vector for the verbal answer buffer rows ####
  
  {
    ansBuffRows <- NULL
    if(!is.null(answerRows) && length(answerRows) != 0) {
      # a loop is necesary when using the chartDF, not for the segmentDF
      for(i in 1:length(answerRows)) {
        ansBuffOn <- answerRows[i] - round((pneumoAnsBuff * cps)) - 0
        ansBuffOff <- answerRows[i] + round((pneumoAnsBuff * cps)) + 0
        # make sure the answer buffer does not exceed the segment data frame
        if(ansBuffOn < 1) ansBuffOn <- 1
        if(ansBuffOff > nrow(chartDF)) ansBuffOff <- nrow(chartDF)
        # if(ansBuffOn[1] < 1) ansBuffOn[1] <- 1
        # if(ansBuffOff[length(ansBuffOff)] > nrow(chartDF)) ansBuffOff[length(ansBuffOff)] <- nrow(chartDF)
        # if(any(ansBuffOn < 1)) ansBuffOn[which(ansBuffOn < 1)] <- 1
        # if(any(ansBuffOff > nrow(chartDF))) ansBuffOff[which(ansBuffOff > nrow(chart))] <- nrow(chartDF)
        ansBuffRows <- c(ansBuffRows, c(ansBuffOn:ansBuffOff))
      }
    } 
  }
  
  #### 1. evaluate the desynchronization and distance between the respiration data ####
  
  {
    
    
    # plot.ts(diffVc)
    
    # artifacts are marked when the difference exceeds the Tukey fences
    
    artifactRowsDesync <- respDesyncFn(diffVc=diffVc, inF=1, outF=2, fences="outer", side="both")
    
    # if(length(artifactRowsDesync) > 0) {
    #   assign("chartDF", , envir=.GlobalEnv)
    #   stop()
    # }
    
    # remove desynchronization artifacts during the answer buffer segements
    # artifactRowsDesync <- artifactRowsDesync[!(artifactRowsDesync %in% ansBuffRows)]
    
    # desynchronization artifacts are marked at both abdominal and thoracic sensors
    outVcDF$outVcU[artifactRowsDesync] <- "ArtifactDesync"
    outVcDF$outVcL[artifactRowsDesync] <- "ArtifactDesync"
    
  }
  
  #### 2. evaluate the tonic stability of the respiration data using the mid line ####
  
  {
    
    # plot.ts(tsMidU)
    # plot.ts(tsMidL)
    
    # using the midline, computed during signal processing, along with upper and lower fences
    # artifacts are marked where the midline equals or exceeds the limits
    
    tonicInstabilityRowsUP <- respTonicInstabilityFn(mid=tsMidU, lf=q25U, uf=q75U)
    tonicInstabilityRowsLP <- respTonicInstabilityFn(mid=tsMidL, lf=q25L, uf=q75L)
    
    # combine the upper and lower artifact vectors
    tonicInstabilityRowsUL <- sort(unique(c(tonicInstabilityRowsUP, tonicInstabilityRowsLP)))
    
    # do not remove tonic instability artifacts from the answer buffer
    # tonicInstabilityRowsUL <- tonicInstabilityRowsUL[!(tonicInstabilityRowsUL %in% ansBuffRows)]
    
    # submit the instability artifacts to the output vectors
    outVcDF$outVcU[as.numeric(tonicInstabilityRowsUP)] <- "ArtifactTonicity"
    outVcDF$outVcL[as.numeric(tonicInstabilityRowsLP)] <- "ArtifactTonicity"
    
  }
  
  #### 3. evaluate respiration instability using the inhalation and exhalation lines ####
  
  {
    
    # plot.ts(tsInhU)
    # plot.ts(tsExhU)
    # plot.ts(tsInhL)
    # plot.ts(tsExhL)
    
    # artifacts are marked when the inhalation line is less than the 25th quantile
    # or when the exhalation line is greater than the 75th quantile
    
    peakInstabilityRowsUP <- respInstabilityFn(uf=q90U, lf=q10U, Inh=tsInhU, Exh=tsExhU)
    peakInstabilityRowsLP <- respInstabilityFn(uf=q90L, lf=q10L, Inh=tsInhL, Exh=tsExhL)
    
    # combine the upper and lower artifact vectors
    peakInstabilityRowsUL <- sort(unique(c(peakInstabilityRowsUP, peakInstabilityRowsLP)))
    
    {
      # remove activity instability artifacts from the answer buffer
      peakInstabilityRowsUP <- peakInstabilityRowsUP[!(peakInstabilityRowsUP %in% ansBuffRows)]
      peakInstabilityRowsLP <- peakInstabilityRowsLP[!(peakInstabilityRowsLP %in% ansBuffRows)]
      
      # remove tonic instability artifacts from the answer buffer
      peakInstabilityRowsUL <- peakInstabilityRowsUL[!(peakInstabilityRowsUL %in% ansBuffRows)]
    }
    
    # submit the instability artifacts to the output vectors
    outVcDF$outVcU[as.numeric(peakInstabilityRowsUP)] <- "ArtifactInst"
    outVcDF$outVcL[as.numeric(peakInstabilityRowsLP)] <- "ArtifactInst"
    
  }
  
  #### 4. check for activity artifact - upper and lower peaks with unexpected values ####
  
  {
    
    ## biting, swallowing, tussis, other distortions ##
    
    # plot.ts(tsDataUp)
    # plot.ts(tsDataLp)
    
    # artifacts are marked where upper or lower peaks are on an unexpected side of the mid line
    
    # Call a function to separately compare the upper and lower peaks to a mid line
    activityArtifactRowsUp <- respPeakInstabilityFn(tsData=tsDataUp, med=q50U)
    activityArtifactRowsLp <- respPeakInstabilityFn(tsData=tsDataLp, med=q50L)
    
    # exclude artifacts around the verbal answer
    activityArtifactRowsUp <- activityArtifactRowsUp[!(activityArtifactRowsUp %in% ansBuffRows)]
    activityArtifactRowsLp <- activityArtifactRowsLp[!(activityArtifactRowsLp %in% ansBuffRows)]
    
    # combine the upper and lower artifact vectors
    # activityArtifactRowsUL <- sort(unique(c(activityArtifactRowsUp, activityArtifactRowsLp)))
    
    # submit the activity artifacts to the output vectors
    outVcDF$outVcU[activityArtifactRowsUp] <- "ActivityArtifact"
    outVcDF$outVcL[activityArtifactRowsLp] <- "ActivityArtifact"
  } 
  
  #### 5. evaluate the data for deep breaths ####
  
  {
    
    # deep breaths are evaluated at the inner Tukey fence
    
    # plot.ts(tsDataUp)
    # plot.ts(tsDataLp)
    
    deepBreathArtifactsUp <- respTukeyFn(tsData=tsDataUp, inF=1.5, outF=3, peaks="inhalation", fences="inner", side="upper")
    deepBreathArtifactsLp <- respTukeyFn(tsData=tsDataLp, inF=1.5, outF=3, peaks="inhalation", fences="inner", side="upper")
    # this function will return the row index where deep breath peaks are located
    
    # exclude deep breath artifacts surrounding the verbal answer
    deepBreathArtifactsUp <- deepBreathArtifactsUp[!(deepBreathArtifactsUp %in% ansBuffRows)]
    deepBreathArtifactsLp <- deepBreathArtifactsLp[!(deepBreathArtifactsLp %in% ansBuffRows)]
    
    # deepBreathArtifactsUL <- sort(unique(c(deepBreathArtifactsUp, deepBreathArtifactsLp)))
    
    # keep the thoracic and abdominal separate to avoid excess data loss 
    outVcDF$outVcU[deepBreathArtifactsUp] <- "ArtifactDB"
    outVcDF$outVcL[deepBreathArtifactsLp] <- "ArtifactDB"
    
  } 
  
  #### 6. evaluate the data for apnea ####
  
  {
    
    # apena activity is subject to a threashold value of 1% of the y-axis range
    
    # plot.ts(tsDataUp)
    # plot.ts(tsDataLp)
    
    apneaArtifactUp <- respApneaFn(x=tsDataUp)
    apneaArtifactLp <- respApneaFn(x=tsDataLp)
    # artifacts are indicated when the segmented excursion length for a 3.75 second moving window, 
    # equal to one respiration cycle at the median rate of 16 cpm
    
    
    # do not exclude artifacts surrounding the verbal answer
    # apneaArtifactUp <- apneaArtifactUp[!(apneaArtifactUp %in% ansBuffRows)]
    # apneaArtifactLp <- apneaArtifactLp[!(apneaArtifactLp %in% ansBuffRows)]
    
    # combine both respiration sensors
    apneaArtifactsUL <- sort(unique(c(apneaArtifactUp, apneaArtifactLp)))
    
    # is not apnea if respiration activity is extant in one of the sensors
    # apnea (cessation of breathing) occurs in both sensors
    apneaArtifactsUL <- apneaArtifactsUL[which(apneaArtifactsUL %in% apneaArtifactUp & apneaArtifactsUL %in% apneaArtifactLp)]
    
    # submit apnea artifacts to both upper and lower sensors
    outVcDF$outVcU[apneaArtifactsUL] <- "ArtifactAP"
    outVcDF$outVcL[apneaArtifactsUL] <- "ArtifactAP"
    
  }
  
  #### 7. evaluate the baseline for artifacts ####
  
  {
    
    ## baseline artifacts are unexpected downward activity,
    # during a single respiration cycle
    # possibly due to physical movement or covert muscle activity,
    # often occuring at or near the point of verbal answer
    
    # baseline artifacts are evaluated at inner Tukey fence
    
    # plot.ts(tsDataUp)
    # plot.ts(tsDataLp)
    
    # call a function to locate baseline instability artifacts artifacts
    baselineArtifactsUp <- respTukeyFn(tsData=tsDataUp, inF=1, outF=2, fences="inner", side="lower")
    baselineArtifactsLp <- respTukeyFn(tsData=tsDataLp, inF=1, outF=2, fences="inner", side="lower")
    
    # exclude artifacts surrounding the verbal answer
    baselineArtifactsUp <- baselineArtifactsUp[!(baselineArtifactsUp %in% ansBuffRows)]
    baselineArtifactsLp <- baselineArtifactsLp[!(baselineArtifactsLp %in% ansBuffRows)]
    
    # do no compbine the baseline artifacts for the thoracic and abdominal sensors
    # baselineArtifactsUL <- sort(unique(c(baselineArtifactsUp, baselineArtifactsLp)))
    
    # keep the thoracic and abdominal separate to avoid excess data loss 
    outVcDF$outVcU[baselineArtifactsUp] <- "ArtifactBLU"
    outVcDF$outVcL[baselineArtifactsLp] <- "ArtifactBLL"
    
  }
  
  #### 8. evaluate the data for BIG deep breaths ####
  
  {
    # BIG deep breaths exceed the outer Tukey fence,
    # and are not removed from the answering buffer

    # plot.ts(tsDataUp)
    # plot.ts(tsDataLp)
    
    bigDeepBreathArtifactsUp <- respTukeyFn(tsData=tsDataUp, inF=1, outF=2, peaks="inhalation", fences="outer", side="upper")
    bigDeepBreathArtifactsLp <- respTukeyFn(tsData=tsDataLp, inF=1, outF=2, peaks="inhalation", fences="outer", side="upper")
    # this function will return the row index where deep breath peaks are located
  
    # do not exclude artifacts surrounding the verbal answer for BIG deep breaths
    # bigDeepBreathArtifactsUp <- bigDeepBreathArtifactsUp[!(bigDeepBreathArtifactsUp %in% ansBuffRows)]
    # bigDeepBreathArtifactsLp <- bigDeepBreathArtifactsLp[!(bigDeepBreathArtifactsLp %in% ansBuffRows)]

    deepBreathArtifactsUL <- sort(unique(c(bigDeepBreathArtifactsUp, bigDeepBreathArtifactsLp)))

    # compbingeartifacts for upper and lower sensors for BIG deep breaths
    outVcDF$outVcU[deepBreathArtifactsUL] <- "ArtifactBDB"
    outVcDF$outVcL[deepBreathArtifactsUL] <- "ArtifactBDB"
  } 
  
  #### 9. evaluate the baseline for BIG artifacts ####
  
  {
    
    # plot.ts(tsDataUp)
    # plot.ts(tsDataLp)
    
    # BIG baseline artifacts are evaluated at outer Tukey fence 
    # these are not excluded from the answering distortion 
    
    # call a function to locate baseline instability artifacts artifacts
    bigBaselineArtifactsUp <- respTukeyFn(tsData=tsDataUp, inF=1, outF=2, fences="outer", side="lower")
    bigBaselineArtifactsLp <- respTukeyFn(tsData=tsDataLp, inF=1, outF=2, fences="outer", side="lower")
    
    # do not exclude artifacts surrounding the verbal answer
    # bigBaselineArtifactsUp <- bigBaselineArtifactsUp[!(bigBaselineArtifactsUp %in% ansBuffRows)]
    # bigBaselineArtifactsLp <- bigBaselineArtifactsLp[!(bigBaselineArtifactsLp %in% ansBuffRows)]

    # compbine the baseline artifacts for the thoracic and abdominal sensors
    bigBaselineArtifactsUL <- sort(unique(c(bigBaselineArtifactsUp, bigBaselineArtifactsLp)))

    # propagate artifacts for upper and lower sensors
    outVcDF$outVcU[bigBaselineArtifactsUL] <- "ArtifactBBL"
    outVcDF$outVcL[bigBaselineArtifactsUL] <- "ArtifactBBL"
    
  }
  
  #### remove artifact marks from the first and last 1 sec ###
  
  {
    outVcDF$outVcU[c(1:(1*cps))] <- "0"
    outVcDF$outVcU[c((nrow(outVcDF)-(1*cps)+1):nrow(outVcDF))] <- "0"
    outVcDF$outVcL[c(1:(1*cps))] <- "0"
    outVcDF$outVcL[c((nrow(outVcDF)-(1*cps)+1):nrow(outVcDF))] <- "0"
  }
  
  #### submit the artifacts to the input data frame and output the chartDF ####
  
  {
    
    {
      # select which artifacts to share with other sensors
      # shareArtifacts <- c("ArtifactDB", "ArtifactAP")
      shareArtifacts <- c("ArtifactDB", "ArtifactAP", "ArtifactsDBD", "ArtifactBBL")
      
      artifactRowsUpS <- which(outVcDF$outVcU %in% shareArtifacts)
      artifactRowsLpS <- which(outVcDF$outVcL %in% shareArtifacts)
      
      # combine sharedartifacts from thoracic and abdominal sensors
      artifactRowsS <- sort(unique(c(artifactRowsUpS, artifactRowsLpS)))
    }
    
    {
      # View(outVcDF)
      
      chartDF$UPneumo_a[which(outVcDF$outVcU != "0")] <- outVcDF$outVcU[which(outVcDF$outVcU != "0")]
      chartDF$LPneumo_a[which(outVcDF$outVcL != "0")] <- outVcDF$outVcL[which(outVcDF$outVcL != "0")]
      
     # propagated artifacts
      chartDF$Artifacts_a[artifactRowsS] <- "Artifact"
    }
    
  }
  
  #### output ####
  
  return(chartDF)
  
  # end newPneumoCheckFn()
} 





