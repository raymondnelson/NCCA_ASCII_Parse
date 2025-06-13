# cardio caliper functions 
# May 2025
# Raymond Nelson
####



# source(paste0(RPath, "amplitudeExtractHelperFunctions.R"), echo=FALSE)

source(paste0(RPath, "getResponsePeaks.R"), echo=FALSE)
source(paste0(RPath, "getResponseOnsets.R"), echo=FALSE)
source(paste0(RPath, "getMaxOnsetPeakDistance.R"), echo=FALSE)
source(paste0(RPath, "getSlopeDirection.R"), echo=FALSE)
source(paste0(RPath, "slopeChange.R"), echo=FALSE)




pulseRateFn <- function(x=30, y=30) {
  # R function to calculate the pulse rate per min for an input sample space
  # x is the sampling rate in seconds
  # y is the number of samples per cycle
  # May 11, 2025
  # Raymond Nelson
  secsPerSample <- x / y 
  pulseRate <- secsPerSample * 60
  return(pulseRate)
}
# pulseRate(x=cps, y=30)


####


sampleSpaceFn <- function(x=30, y=30) {
  # R function to calculate the sample space for an input pulse rate
  # x is the sampling rate in seconds
  # y is the pulse rate per minute
  # output is the number of samples
  # May 11, 2025
  # Raymond Nelson
  secsPerCycle <- 60 / y
  samplesPerCycle <- secsPerCycle * x
  return(samplesPerCycle)
  
}
# sampleSpace(x=cps, y=130)


####


fixPeakFn <- function(x=chartDF$c_Cardio1, times=4) {
  # May 11, 2025
  # Raymond Nelson
  # R function to fix max and min peaks that are repeated 2 or more times
  # input x is the time series data
  # times is the number of times to repeat the operaation from 1 to 8
  # this function will keep the first of all adjacent max and min peaks
  # and interpolate the subsequent equal peak with the next adjacent sample
  # this function is useful when the cardio data is very noisy 
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


####


lowPass2hz.2ndFn <- function(x, GAIN = 2.97868961737845e+001, zplane0 = -0.55326988968868, zplane1 = 1.41898265221812) {
  # R function
  # 2nd order Butterworth low-pass filter
  # May 11, 2025
  # Raymond Nelson
  # used to remove the cardio dichrotic notch 
  # and improve the initial calculation/estimation of the cardio rate
  # the cardio rate estimate is used to set the buffer length 
  # that is used for cardio peak extraction
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


####


bufferLenFn <- function(x, y=.6) {
  # R function to calculate the buffer length for the maxPeak and minPeak functions
  # May 11, 2025
  # Raymond Nelson
  # x input is the rate per min scalar output from the ratePerMin function
  # y is the number of cycles
  # this function is called before the maxPeakFn, 
  # and is used to set the buffer length for varying cardio rates
  # to support accurate peak extraction
  # output is an integer that defines the 1/2 buffer width 
  # for the number of cardiac cycles
  # in samples surrounding the index sample of the minPeak and maxPeak function
  bLen <- floor(1 / x * 60 * (cps*y)) - 1
  if(bLen <= 0) bLen <- 1
  return(bLen)
}


####


maxPeakFn <- function(x, y=round(.25*cps,0), firstLast=FALSE) {
  # R function to get the cyclic peaks from the time series data
  # May 11, 2025
  # Raymond Nelson
  # will keep the index number of all max peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # firstLast = FALSE will omit the first and last value 
  # output is vector of max peak sample indices to compute the rate or interpolate the line
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
           # keep the value if it is the max
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           # non max samples in the buffer are NA
           next()
    )
  } # end for loop
  return(as.numeric(na.omit(xOut)))
} 


####


minPeakFn <- function(x, y=round(.25*cps,0), firstLast=FALSE) {
  # R function to get the cyclic peaks from the time series data
  # May 11, 2025
  # Raymond Nelson
  # to keep the index number of all min peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # firstLast = FALSE will omit the first and last value 
  # output is vector of max peak sample indices to compute the rate or interpolate the line
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


####


ratePerMinFn <- function(x=chartDF$c_Cardio1, buffer=10, peaks="upper", lowPass=TRUE) {
  # R function to calculate the cyclic rate per minute
  # May 11, 2025
  # Raymond Nelson 
  # for cardio and pneumo time series data
  # x input is series vector cardio data for the entire recorded chart
  # peaks input is a switch to choose "upper" or "lower" peaks
  # upper peaks are normally used 
  # because these are the working phase of the cardio and respiratory cycles, 
  # buffer input is the number of pre and post index samples to include in the search space
  # buffer = 40 for pneumo
  # buffer = 10 for cardio
  # output is the cardio rate per minute 
  # use lowPass=FALSE for respiration and lowPass=TRUE for cardio
  ####
  {
    if(!exists("lowPass")) lowPass <- TRUE
    if(!exists("buffer")) buffer <- 10
    # use 42 for pneumo
    if(!exists("peaks")) peaks <- "upper"
  }
  ## smooth the cardio input to remove artifact peaks
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
  ## remove peak diffs of 1 because these occur when the cardio cuff is deflated
  peakDiffs <- diff(Peaks)
  # peak diffs of 1 will distort the cardio rate measurement
  peakDiffs <- peakDiffs[which(peakDiffs != 1)]
  ## calculate the rate
  rateMin <- ifelse(length(peakDiffs)>1,
                    60 / (mean(peakDiffs) / cps ),
                    60/peakDiffs/cps
  )
  ## output
  return( round(rateMin , 2) )
} 


# cardioRate <- ratePerMinFn(x=chartDF$c_Cardio1, buffer=10, peaks="upper", lowPass=TRUE)
# cardioBufferLen <- bufferLenFn(x=cardioRate, y=.6) 

####


cardioRatePerCycleFn <- function(x=chartDF$c_Cardio1, buffer=9, peaks="upper", lowPass=TRUE) {
  # R function to calculate the cyclic rate for each cardiac cycle
  # May 11, 2025
  # Raymond Nelson 
  # x input is the time series vector of cardio data for the entire recorded chart
  # peaks input is a switch to choose "upper" or "lower" peaks
  # upper peaks are normally used 
  # because these are the working phase of the cardio and respiratory cycles, 
  # buffer input is the number of pre and post index samples to include in the search space
  # buffer = 40 for pneumo
  # buffer = 10 for cardio
  # output is the mean rate 
  # use lowPass=TRUE for cardio, and lowPass=FALSE for respiration
  # output is a vector of cyclic rates
  # for which the rate value changes with each systolic peak
  # call this function during signal processing
  # it does not need to be called again when scaling or offsetting the cardio data for display
  ####
  {
    if(!exists("lowPass")) lowPass <- TRUE
    if(!exists("buffer")) buffer <- 9
    # use 42 for pneumo
    if(!exists("peaks")) peaks <- "upper"
  }
  ## get the cardio rate ##
  thisCardioRate <- ratePerMinFn(x=x, buffer=buffer, peaks="upper", lowPass=TRUE)
  ## compute the buffer length for cardio peak selection ##
  thisBufferLen <- bufferLenFn(x=thisCardioRate, y=.6)
  # dynamically setting the search buffer this way
  # helps to accurately account for a wide range of cardio rates
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


# cardioRateVc <- cardioRatePerCycleFn(x=chartDF$c_Cardio1, buffer=cardioBufferLen, peaks="upper", lowPass=TRUE)
# chartDF$c_CardioRate <- cardioRateVc


####. cardio amplitude caliper function  ####


cardioAmplitudeCaliperFn <- function(x=chartDF$c_CardioMA, 
                                     caliperStart=NULL, 
                                     caliperStop=NULL, 
                                     caliperLen=15, 
                                     lat=.5, 
                                     rowEnd=13.5,
                                     startIdx=FALSE) {
  # R function to compute the cardio amnpitude for a caliper selection
  # May 27, 2025
  # Raymond Nelson
  ####
  # x input is the time series cardio data at the mid line
  # can also be used at the diastolic or systolic line
  # caliperStart is the start index for the caliper space
  # caliperStop is the end index for the caliper space
  # caliperStop=NULL will use the caliperLen to locate the caliper end index
  # caliperLen is the length, in seconds, for the caliper window
  # lat is the minimum response latency. onsets must be after this 
  # rowEnd is the end of the response onset window. onsets must be before this
  # startIdx = TRUE will include the caliper start index,
  # when there is no onset of a + slope during the ROW
  #
  # the caliper start and stop points can be placed at any recorded sample
  # when using caliperStop=NULL only the caliperStart parameter (sample index) needs to be input
  #
  # output is a list including:
  # the response onset index (x-axis)
  # the response onset value (y-axis)
  # the response peak index
  # the response peak value
  # the cardio amplitude (peak - onset) in dimensionless units
  # 
  # cardio amplitude is the maximum distance between a response peak and preceding response onset
  # response peak is the change in slope from a pos to neg, including the caliperEnd index
  # response onset is the change in slope from neg to pos, including the caliper onset index
  # this function will not attempt to impute a response onset via change in inflection
  ####
  if(is.null(caliperLen)) {
    caliperLen <- 15
  }
  if(is.null(caliperStart)) {
    # compute the cardio rate for the entire chart if the input parameters are NULL
    caliperStart <- 1
    caliperStop <- length(x)
  }
  if(is.null(caliperStop) || caliperStop < caliperStart) {
    caliperStop <- caliperStart + (cps * caliperLen) - 1
  }
  if(caliperStart < 2) {
    caliperStart <- 2
  }
  if(caliperStop > length(x)) {
    caliperStop <- length(x)
  }
  if(is.null(lat)) {
    lat <- 0
  }
  if(is.null(rowEnd)) {
    rowEnd <- caliperLen / 2
  }
  ## slice the time series data for the caliper segment
  # start with the sample before caliperStart so that the difference can 
  DAT <- x[c((caliperStart-1):caliperStop)] 
  ## source a script for some helper functions
  # source(paste0(RPath, "getSlopeDirection.R"), echo=FALSE)
  ## call a function to get the slope of the time series data 
  diff1 <- c(diff(DAT))
  theSlope <- ifelse(diff1==0,
                     # ifelse is vectorized and requires no control loop
                     theSlope <- 0,
                     ifelse(diff1>0,
                            theSlope <- 1,
                            theSlope <- -1) )
  # re-slide the DAT vector using the stimulus onset index
  DAT <- x[c(caliperStart:caliperStop)]
  ## locate the response onset indices
  xOnset <- ifelse(theSlope[2:length(theSlope)] == 1,
                   # ifelse is vectorized and needs no control loop
                   # locate the onset of neg slope
                   # by checking each slope value with the next
                   ifelse(( theSlope[2:length(theSlope)] +
                              theSlope[1:(length(theSlope)-1)] ) == 2,
                          xOnset <- 0,
                          xOnset <- 1),
                   xOnset <- 0)
  # xOnset is the onset of each positive slope segment
  xOnset <- which(xOnset != 0)
  # keep positive slope onset indices after latency
  if(!is.null(lat)) xOnset <- xOnset[which(xOnset > (lat * cps))]
  # keep positive slope onset indices before rowEnd
  if(!is.null(rowEnd)) xOnset <- xOnset[which(xOnset <= round(rowEnd * cps))]
  
  if(length(xOnset) == 0 || is.null(xOnset)) {
    # call a function to impute a response onset via change in inflection
    xOnset <- sort(unique(c(xOnset, maxSlopeChangeFn(x=DAT))))
  }
  
  # add the first index if there are no response onsets after lat and before rowEnd
  if(length(xOnset) == 0 || is.null(xOnset)) {
    if(theSlope[1] == 1) {
      xOnset <- 1
      # this will permit placement of the caliper start at an inflection point 
      # when there is no + slope onset after latency and before the row
    }
  }
  
  ## locate the response peak indices
  xPeak <- ifelse(theSlope[2:length(theSlope)] == -1,
                     # ifelse is vectorized and needs no control loop
                     # locate the onset of neg slope
                     # by checking each slope value with the next
                     ifelse(( theSlope[2:length(theSlope)] +
                                theSlope[1:(length(theSlope)-1)] ) == -2,
                            xPeak <- 0,
                            xPeak <- -1),
                     xPeak <- 0) 
  # peak is the last sample index for each positive slope segment
  xPeak <- which(xPeak != 0)
  # keep the last index as a peak if the slope is positive
  if(theSlope[length(theSlope)] == 1) {
    xPeak <- sort(unique(c(xPeak, length(theSlope))))
  }
  # keep xPeak indices after the first xOnset
  xPeak <- xPeak[which(xPeak > xOnset[1])]
  ## find the max distance from a peak to a preceeding onset
  # initialize a scalar for the max amplitude change
  maxYDistance <- 0
  maxOnsetIdx <- 0
  maxPeakIdx <- 0
  onsetVal <- NULL
  peakVal <- NULL
  # compare each peak to all preceding response onsets
  i=1
  for(i in length(xPeak):1) {
    # interact backwards over the peaks
    thisPeak <- xPeak[i]
    thisPeakVal <- DAT[thisPeak]
    # initialize some scalars
    peakDistVc <- NULL
    xOnset1 <- xOnset[which(xOnset < thisPeak)]
    if(length(xOnset1) == 0) next()
    j=1
    for(j in 1:length(xOnset1)) {
      # iterate forward over the onsets
      if(xOnset1[j] >= thisPeak) next()
      thisOnset <- xOnset1[j]
      thisOnsetVal <- DAT[thisOnset]
      if(thisOnsetVal >= thisPeakVal) next()
      thisYDistance <- thisPeakVal - thisOnsetVal
      if(thisYDistance > maxYDistance) {
        maxYDistance <- thisYDistance
        maxOnsetIdx <- thisOnset
        maxPeakIdx <- thisPeak
        onsetVal <- thisOnsetVal
        peakVal <- thisPeakVal
      }
    }
  }
  ## construct the output list
  outputList <- list(maxOnsetIdx=maxOnsetIdx, maxPeakIdx=maxPeakIdx, onsetVal=onsetVal, peakVal, maxYDistance=maxYDistance)
  ## output 
  return(outputList)
  # end cardioAmplitudeCaliperFn()
}


# ampCaliper <- cardioAmplitudeCaliperFn(x=chartDF$c_CardioMA,
#                                        caliperStart=2742,
#                                        caliperStop=NULL,
#                                        caliperLen=15,
#                                        lat=.5,
#                                        rowEnd=13.5)
  
# ampCaliper$maxYDistance

####. cardio rate caliper function  ####


cardioRateCaliperFn <- function(x=chartDF$c_CardioRate, caliperStart=NULL, caliperStop=NULL, caliperLen=15) {
  # R function to compute the cardio rate for a caliper selection
  # May 10, 2025
  # Raymond Nelson
  ####
  # x input is the output vector from the cardioRatePerCycleFn() function
  # x is a vector of cardio pulse rates that are computed for each systolic peak
  # caliperStart is the start index for the caliper space
  # caliperStop is the end index for the caliper space
  # caliperStop=NULL will use the caliperLen to locate the caliper end index
  # caliperLen is the length, in seconds, for the caliper window
  #
  # the caliper start and stop points can be place at any recorded sample
  # when using caliperStop=NULL only the caliperStart index needs to be input
  #
  # output is the weigthted mean cardio rate 
  # for the cardio caliper selection from caliperStart to caliperStop
  ####
  if(is.null(caliperLen)) caliperLen <- 15
  if(is.null(caliperStart)) {
    # compute the cardio rate for the entire chart if the input parameters are NULL
    caliperStart <- 1
    caliperStop <- length(x)
  }
  if(is.null(caliperStop)) caliperStop <- caliperStart + (cps * caliperLen) - 1
  # output is the weighted mean cardio rate for the caliper selection
  return( mean(x[c(caliperStart:caliperStop)]) )
  # end cardioRateCaliperFn()
}

# chartDF$c_CardioRate <-
#   cardioCaliperFn(x=chartDF$c_CardioRate, caliperStart, caliperStop=NULL, caliperLen=15)
  
  
#### operational example ####

runExample <- TRUE
runExample <- FALSE

if(runExample) {
  
  # get the time series data for the cardio sensor
  cardioDAT <- chartDF$c_Cardio1
  
  cardioRate <- ratePerMinFn(x=cardioDAT, buffer=10, peaks="upper", lowPass=TRUE)
  cardioBufferLen <- bufferLenFn(x=cardioRate, y=.6)
  
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
  cardioRateVc <- 
    cardioRatePerCycleFn(x=cardioDAT, buffer=cardioBufferLen, peaks="upper", lowPass=TRUE)
  
  # compute the mean cardio rate for the stimulus segments
  cardioCaliperRates <- rep(NA, times=length(stimOnset))
  names(cardioCaliperRates) <- names(stimOnset)
  for(l in 1:length(stimOnset)) {
    theseRows <- c(stimOnset[l]:WOEEnd[l])
    cardioCaliperRates[l] <- round(mean(cardioRateVc[theseRows]), 2)
  }
  
  # a vector of cardio caliper rates for each stimulus event 
  cardioCaliperRates
  
  runExample <- FALSE
  
} 



