# pneumograph caliper functions 
# June 2025
# Raymond Nelson
####


# source a scripte to load the pneumoMeasurementFn() function
source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R", echo=FALSE)


# pneumoDAT <- chartDF$c_UPneumoSm
# plot.ts(pneumoDAT[1000:4000])


####


fixPnPeakFn <- function(x=chartDF$c_UPneumoSm, times=4) {
  # June, 2025
  # Raymond Nelson
  # R function to fix max and min peaks that are repeated 2 or more times
  # input x is the smoothed time series data for the upper lower respiration sensor
  # times is the number of times to repeat the operaation from 1 to 8
  # this function will keep the first of all adjacent max and min peaks
  # and interpolate the subsequent equal peak with the next adjacent sample
  # this function is useful when the cardio or pneumo data is very noisy 
  # and when the pneumo data has flat spots 
  ###
  if(length(which(is.na(x)))==length(x)) { return(x) }
  #may need to re-write this with a while loop
  for (j in 1:times) {
    for (i in 9:(length(x)-9)) {
      if(any(is.na(x[i:(i-8)]))) next()
      if(x[i]==x[(i-1)]) { x[i] <- mean(c(x[(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-2)]) { x[i] <- mean(c(x[(i-2):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-3)]) { x[i] <- mean(c(x[(i-3):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-4)]) { x[i] <- mean(c(x[(i-4):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-5)]) { x[i] <- mean(c(x[(i-5):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-6)]) { x[i] <- mean(c(x[(i-6):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-7)]) { x[i] <- mean(c(x[(i-7):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-8)]) { x[i] <- mean(c(x[(i-8):(i-1)], x[(i+1)])) }
    }
  }
  return(x)
} 


# pneumoDAT <- fixPnPeakFn(x=pneumoDAT, times=4)
# plot.ts(pneumoDAT[1000:4000])


####


bufferLenFn <- function(x, y=.6) {
  # R function to calculate the buffer length for the maxPeak and minPeak functions
  # May 11, 2025
  # Raymond Nelson
  # x input is the rate per min scalar output from the ratePerMin function
  # y is the number of cycles
  # this function is called before the maxPeakFn, 
  # and is used to set the buffer length for varying cardio and respiration rates
  # to support accurate peak extraction
  # output is an integer that defines the 1/2 buffer width 
  # for the number of cardiac cycles
  # in samples surrounding the index sample of the minPeak and maxPeak function
  bLen <- floor(1 / x * 60 * (cps*y)) - 1
  if(bLen <= 0) bLen <- 1
  return(bLen)
}


####


lowPass2hz.2ndFn <- function(x, GAIN = 2.97868961737845e+001, zplane0 = -0.55326988968868, zplane1 = 1.41898265221812) {
  # R function
  # 2nd order Butterworth low-pass filter
  # May 11, 2025
  # Raymond Nelson
  # used to remove high
  # and improve the initial calculation/estimation of the cardio and respiration rate
  # the cyclic rate estimate is used to set the buffer length 
  # that is used for accurate peak extraction
  # this can also be used to smooth the pneumo data when calculating the pneumo rate
  # x input is a column vector from the time series 
  # output is a filtered time series vector
  ####
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  yv1 <- 0
  yv2 <- 0
  # inititalize the output
  output <- rep(NA, length(x))
  # use a loop
  for (i in 1:length(x)) {
    # shift the xv registers
    xv0 <- xv1
    xv1 <- xv2
    # compute xv2
    xv2 <- x[i] / GAIN
    # shift the yv registers
    yv0 <- yv1
    yv1 <- yv2
    # computer yv2
    yv2 <- xv0 +
      xv2 +
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    # set the output
    output[i] <- yv2
  } # end loop
  return(output)
}


# pneumoDAT <- lowPass2hz.2ndFn(x=chartDF$c_UPneumoSm)
# plot.ts(pneumoDAT[1000:4000])


#### 


getAnswerIndicesFn <- function(x=chartDF$Label, ansBuffLen=pneumoAnsBuff, buffer=FALSE) {
  # R function to get the verbal answer indices for an NCCA ASCII chart
  # May 13, 2025
  # Raymond Nelson
  # x is the label column from chartDF, obtained from the NCCA ASCCI time series data
  # ansBuffLen is the length of the buffer zone surrounding a verbal answer
  # buffer=TRUE will return the sample indices of the answer including the buffer zone
  # buffer=FALSE will return only the verbal answer indices
  # output is a vector of sample indices
  # answer buffer is excluded from computation of the respiration rate and RLE measurements
  ####
  # get the answer indices
  answerIndices <- which(x %in% c("ANS", "YES", "NO", "no", "yes", "ans", "Ans", "Yes", "No"))
  if(isTRUE(buffer)) {
    # cps is an environment variable that is set in the NCCASCII_init.R script
    if(!exists("cps", envir=.GlobalEnv)) cps=30
    bufferStartLocs <- answerIndices - (cps * ansBuffLen)
    bufferEndLocs <- answerIndices + (cps * ansBuffLen) - 1
    pneumoAnswerBuffers <- NULL
    for(i in 1:length(bufferStartLocs)) {
      pneumoAnswerBuffers <- c(pneumoAnswerBuffers, c(bufferStartLocs[i]:bufferEndLocs[i]))
    }
    answerIndices <- pneumoAnswerBuffers
  }
  return(answerIndices)
} 

# getAnswerIndicesFn(x=chartDF$Label, ansBuffLen=pneumoAnsBuff, buffer=FALSE)
# getAnswerIndicesFn(x=chartDF$Label, ansBuffLen=pneumoAnsBuff, buffer=TRUE)


####


ratePerMinFn <- function(x=chartDF$c_UPnemoSm, buffer=40, peaks="upper", lowPass=TRUE) {
  # R function to calculate the cyclic rate per minute
  # May 11, 2025
  # Raymond Nelson 
  # for cardio and pneumo time series data
  # x input is series vector cardio or respiration data for the entire recorded chart
  # peaks input is a switch to choose "upper" or "lower" peaks
  # upper peaks are normally used 
  # because these are the working phase of the cardio and respiratory cycles, 
  # buffer input is the number of pre and post index samples to include in the search space
  # buffer = 40 for pneumo
  # buffer = 10 for cardio
  # output is the cardio or respiration rate per minute 
  # use lowPass=FALSE for respiration and lowPass=TRUE for cardio
  ####
  {
    if(!exists("lowPass")) lowPass <- TRUE
    if(!exists("buffer")) buffer <- 10
    # use 42 for pneumo
    if(!exists("peaks")) peaks <- "upper"
  }
  ## smooth the input data to remove false peaks and dichrotic notches
  if(lowPass == TRUE) { 
    x1 <- lowPass2hz.2ndFn(x) 
  } else {
    x1 <- x
  }
  ## then get the max or min peak
  ifelse(peaks=="lower",
         Peaks <- minPeak(x=x1, y=buffer, firstLast=FALSE),
         Peaks <- maxPeak(x=x1, y=buffer, firstLast=FALSE) 
  ) 
  ## remove peak diffs of 1 that may occur 
  ## such as when the respiration sensor is railed or collapsed or damaged
  peakDiffs <- diff(Peaks)
  # peak diffs of 1 will distort the cyclic rate measurement
  peakDiffs <- peakDiffs[which(peakDiffs != 1)]
  ## calculate the rate
  rateMin <- ifelse(length(peakDiffs)>1,
                    60 / (mean(peakDiffs) / cps ),
                    60/peakDiffs/cps
  )
  ## output
  return( round(rateMin , 2) )
} 


# UPneumoRate <- ratePerMinFn(x=chartDF$c_UPneumoSm, buffer=40, peaks="upper", lowPass=TRUE)
# LPneumoRate <- ratePerMinFn(x=chartDF$c_LPneumoSm, buffer=40, peaks="upper", lowPass=TRUE)
# pneumoBufferLen <- bufferLenFn(UPneumoRate)


####


pneumoRatePerCycleFn <- function(x=chartDF$c_UPneumoSm, buffer=65, peaks="upper", lowPass=TRUE) {
  # R function to calculate the cyclic rate for each respiration cycle
  # May 11, 2025
  # Raymond Nelson 
  # x input is the time series vector of respiration data for the entire recorded chart
  # peaks input is a switch to choose "upper" or "lower" peaks
  # upper peaks are normally used 
  # because these are the working phase respiration cycle, 
  # buffer input is the number of pre and post index samples to include in the search space
  # buffer = 40 for pneumo
  # buffer = 10 for cardio
  # output is the mean rate 
  # use lowPass=TRUE for cardio, and lowPass=FALSE for respiration
  # output is a vector of cyclic rates
  # for which the rate value changes with each systolic peak
  # call this function during signal processing
  # it does not need to be called again when offsetting or scaling the pneumo data for display
  ####
  {
    if(!exists("lowPass")) lowPass <- TRUE
    if(!exists("buffer")) buffer <- 9
    # use 42 for pneumo
    if(!exists("peaks")) peaks <- "upper"
  }
  ## get the cyclic rate per min ##
  thisRate <- ratePerMinFn(x=x, buffer=buffer, peaks="upper", lowPass=TRUE)
  ## compute the buffer length for accurate peak extraction ##
  thisBufferLen <- bufferLenFn(x=thisRate, y=.6)
  # dynamically setting the search buffer this way
  # helps to accurately account for a wide range of cardio and respiration rates
  ## then get the max or min peak ##
  ifelse(peaks=="lower",
         Peaks <- minPeakFn(x=x, y=thisBufferLen, firstLast=FALSE),
         Peaks <- maxPeakFn(x=x, y=thisBufferLen, firstLast=FALSE) 
  ) 
  if(length(Peaks) < length(x)) Peaks <- c(Peaks, length(x))
  ## compute the peak to peak distance
  peakDistance <- c(Peaks[1], diff(Peaks))
  ## and the rate per min for each peak
  peakRate <- (cps / peakDistance) * 60
  if(length(Peaks) != length(peakRate)) stop()
  ## then submit the rate per min values to the output vector
  xOut <- rep(NA, times=length(x))
  ## iterate over the vector of peakRates
  # length(Peaks)
  # length(peakDistance)
  # length(peakRate)
  startIdx <- 1
  i=1
  for(i in 1:length(Peaks)) {
    # length(xOut)
    xOut[c(startIdx:Peaks[i])] <- peakRate[i]
    startIdx <- startIdx + peakDistance[i]
  }
  # which(is.na(xOut))
  if(length(xOut) != length(x)) stop()
  ## output
  return(xOut)
} 


# UPneumoRateVc <-
#   pneumoRatePerCycleFn(x=chartDF$c_UPneumoSm, buffer=pneumoBufferLen, peaks="upper", lowPass=TRUE)
# LPneumoRateVc <-
#   pneumoRatePerCycleFn(x=chartDF$c_LPneumoSm, buffer=pneumoBufferLen, peaks="upper", lowPass=TRUE)

# plot.ts(UPneumoRateVc[1000:4000])
# chartDF$c_UPneumoRate <- UPneumoRateVc


####


# source a script to load the pneumoMeasurementFn() function
source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R", echo=FALSE)


respirationLineExcursionFn <- function(x=chartDF$c_UPneumoSm, pnBufferLen=pneumoMeasurementBuffer) {
  # R function to calculate the respiration line excursion for a chart
  # May 11, 2025
  # Raymond Nelson 
  #
  # called when scaling and offsetting the respiration data
  #
  # x input is the time series vector of respiration data for the entire recorded chart
  # y is the length (seconds) for a moving window
  #
  # output is a vector of the same length as the input,
  # for which each value is the sum of differences for a of moving window
  # 
  # this function is a wrapper
  # that will call the pneumoMeasurmentFn() in the pneumoMeasurement.R script
  # this function is called by a caliper function that will
  # some these output values for a Window of Evaluation
  # 
  ####
  # call the pneumoMeasurementFn from the pneumoMeaasurement.R scrip
  xOut <- pneumoMeasurementFn(dataVector=x, verbalAnswer=NULL, pnBufferLen=pnBufferLen, output="vector")
  # this function needs the cps and pneumoMeasurementBuffer scalars from the .globalEnv
  return(xOut)
} 

# UPneumoRLEVc <- respirationLineExcursionFn(x=chartDF$c_UPneumoSm, pnBufferLen=1)
# LPneumoRLEVc <- respirationLineExcursionFn(x=chartDF$c_LPneumoSm, pnBufferLen=1)
# chartDF$c_UPneumoExcursion <- UPneumoRLEVc
# chartDF$c_LPneumoExcursion <- LPneumoRLEVc

# chartDF$c_UPneumoExcursion <- chartDF$c_UPneumoExcursion + (950 - max(chartDF$c_UPneumoExcursion))
# chartDF$c_LPneumoExcursion <- chartDF$c_LPneumoExcursion + (700 - max(chartDF$c_LPneumoExcursion))

# plot.ts(chartDF$c_UPneumoExcursion)
# plot.ts(chartDF$c_LPneumoExcursion)

# head(UPneumoRLEVc, 1000)

# plot.ts(UPneumoRLEVc[1000:1450])
# plot.ts(UPneumoRLEVc[2000:2450])

# plot.ts(chartDF$c_UPneumoSm[1000:4000])



############ compute some respiration metrics #############



# meanRespirationRateChart <- mean(c(UPneumoRate, LPneumoRate))


# compute the number of samples for 3 charts
# threeCyclePeriod <- round((1800 / meanRespirationRateChart) * 3)




########### respiration caliper functions ################



#### respiration rate caliper function ####


pneumoRateCaliperFn <- function(x=chartDF$UPneumoRate, caliperStart=NULL, caliperStop=NULL, caliperLen=15) {
  # R function to compute the respiration rate for a caliper selection
  # May 14, 2025
  # Raymond Nelson
  #
  # this function is called by the respiration signal processing function 
  #
  # x input is the output vector from the pneumoRatePerCycleFn() function
  # caliperStart is the start index for the caliper space
  # caliperStop is the end index for the caliper space
  # caliperStop=NULL will use the caliperLen to locate the caliper end index
  # caliperLen is the length (seconds), for the caliper window
  # outputVal is the respiration rate for the caliper selection
  #
  # the caliper start and stop points can be place at any recorded sample indices
  # when using caliperStop=NULL only the caliperStart index needs to be input
  #
  ####
  if(is.null(caliperLen)) caliperLen <- 15
  if(is.null(caliperStop)) caliperStop <- caliperStart + (cps * caliperLen) - 1
  if(is.null(caliperStart)) {
    # compute the respiration rate for the entire chart if the input parameters are NULL
    caliperStart <- 1
    caliperStop <- length(x)
  }
  if(is.null(caliperStop)) caliperStop <- caliperStart + (cps * caliperLen) - 1
  # output is the weighted mean respiration rate for the caliper selection
  return( mean(x[c(caliperStart:caliperStop)]) )
}

# respirationRate <-
# pneumoRateCaliperFn(x=UPneumoRateVc, caliperStart=NULL, caliperStop=NULL, caliperLen=15)


#### RLE caliper function ####


pneumoRLECaliperFn <- function(x=chartDF$c_UPneumoExcursion, caliperStart=NULL, caliperStop=NULL, caliperLen=15) {
  # R function to compute the respiration rate or RLE for a caliper selection
  # May 14, 2025
  # Raymond Nelson
  # x input is the output vector from the cardioRatePerCycleFn() function
  # x is a vector of cardio pulse rates that are computed for each systolic peak
  # caliperStart is the start index for the caliper space
  # caliperStop is the end index for the caliper space
  # caliperStop=NULL will use the caliperLen to locate the caliper end index
  # caliperLen is the length, in seconds, for the caliper window
  # outputVal is either "RLE" or "rate"
  #
  # the caliper start and stop points can be place at any recorded sample
  # when using caliperStop=NULL only the caliperStart index needs to be input
  #
  # output is RLE measurement
  # for the cardio caliper selection from caliperStart to caliperStop
  ####
  if(is.null(caliperLen)) caliperLen <- 15
  if(is.null(caliperStop)) caliperStop <- caliperStart + (cps * caliperLen) - 1
  if(is.null(caliperStart)) {
    caliperStart <- 1
    caliperStop <- length(x)
  }
  
  x <- x - x[1]
  
  # output is the weighted mean cardio rate for the caliper selection
  round(mean(x[c(caliperStart:caliperStop)]))
}


# pneumoRLECaliperFn(x=chartDF$c_UPneumoExcursion, caliperStart=NULL, caliperStop=NULL, caliperLen=15)
# pneumoRLECaliperFn(x=chartDF$c_LPneumoExcursion, caliperStart=NULL, caliperStop=NULL, caliperLen=15)

# pneumoRLECaliperFn(x=chartDF$c_UPneumoExcursion0, caliperStart=NULL, caliperStop=NULL, caliperLen=15)
# pneumoRLECaliperFn(x=chartDF$c_LPneumoExcursion0, caliperStart=NULL, caliperStop=NULL, caliperLen=15)


#### respiration amplitude caliper ####


pneumoAmplitudeCaliperFn <- function(x=chartDF$c_UpneumoAmp0, caliperStart=NULL, caliperStop=NULL, caliperLen=15) {
  # R function to compute the respiration rate or RLE for a caliper selection
  # May 14, 2025
  # Raymond Nelson
  # x input is the output vector from the cardioRatePerCycleFn() function
  # x is a vector of cardio pulse rates that are computed for each systolic peak
  # caliperStart is the start index for the caliper space
  # caliperStop is the end index for the caliper space
  # caliperStop=NULL will use the caliperLen to locate the caliper end index
  # caliperLen is the length, in seconds, for the caliper window
  # outputVal is either "RLE" or "rate"
  #
  # the caliper start and stop points can be place at any recorded sample
  # when using caliperStop=NULL only the caliperStart index needs to be input
  #
  # output is the weigthted mean cardio rate 
  # for the cardio caliper selection from caliperStart to caliperStop
  ####
  if(is.null(caliperLen)) caliperLen <- 15
  if(is.null(caliperStop)) caliperStop <- caliperStart + (cps * caliperLen) - 1
  if(is.null(caliperStart)) {
    caliperStart <- 1
    caliperStop <- length(x)
  }
  # output is the weightec mean cardio rate for the caliper selection
  return( mean(x[c(caliperStart:caliperStop)]) )
} 


# pneumoAmplitudeCaliperFn(x=chartDF$c_UpneumoAmp0, caliperStart, caliperStop=NULL, caliperLen=15) {
  

#### operational example ####


runExample <- TRUE
runExample <- FALSE


if(runExample) {
  
  # get the time series data for the respiration sensor
  pneumoDAT <- chartDF$c_UPneumoSm
  
  UPneumoRate <- ratePerMinFn(lowPass2hz.2ndFn(x=pneumoDAT), buffer=40, peaks="upper", lowPass=TRUE)
  pneumoBufferLen <- bufferLenFn(UPneumoRate)
  
  # get the event labels
  eventLabels <- chartDF$Label[stimOnset]
  
  # get the onset index for each stimulus segment
  stimOnset <- which(chartDF$Events=="onsetRow")
  names(stimOnset) <- eventLabels
  
  # remove the X and XX from the events
  stimOnset <- stimOnset[!(eventLabels %in% c("X", "XX"))]
  
  # set the end of the Window of Evaluation (WOE) #measuredSeg is set in the init scripts
  WOEEnd <- stimOnset+(measuredSeg*cps) - 1
  
  # call the functions (above)
  respirationRateVc <- 
    pneumoRatePerCycleFn(x=pneumoDAT, buffer=pneumoBufferLen, peaks="upper", lowPass=TRUE)
  # plot.ts(respirationRateVc)
  
  # compute the mean respiration rate for the stimulus segments
  respirationRateCaliperVals <- rep(NA, times=length(stimOnset))
  names(respirationRateCaliperVals) <- names(stimOnset)
  for(l in 1:length(stimOnset)) {
    theseRows <- c(stimOnset[l]:WOEEnd[l])
    respirationRateCaliperVals[l] <- round(mean(respirationRateVc[theseRows]), 2)
  }
  
  # a vector of respiration caliper rates for each stimulus event 
  respirationRateCaliperVals
  
  runExample <- FALSE
  
} 





