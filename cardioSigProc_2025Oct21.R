# R functions for cardio data signal processing 
# October 21, 2025
# Raymond Nelson 
####
#
# this script contains several functions that make up the signal processing method
#
# fixPeak() is used to ensure that diastolic and systolic peaks occur on a single data sample
# 
# NAInterp() to fix missing or dropped cardio data values
#
# ratePerMin() to get a rough calculation of the cardio rate, used to set the buffer for more precise calculation
#
# bufferLenFn() used to compute an optimal buffer length to compute a more precise cardio rate
#
# ratePerMin and bufferLenFn are used together to ignore the dichrotic notch,
# and extract the cardio rate using only the systolic peaks
#
# getFirstLastEventFn() to obtain the sample indices for the first and last events
# used for scaling and offsetting
#
# scaleDataFn() adjust the y-axis magnitude of the cardio pulse
# 
# offsetDataFn() set the y-axix location for the cardio data
#
# cardioRatePerCycleFn() to compute the cardiac rate at each cardiac pulse
#
# maxPeak() to get the sample indices for systolic peaks
#
# minPeak() to get the sample indices for diastolic peaks
#
# interpolatePeaks() to compute an interpolated line for the systolic and diastolic peaks
#
# centerColumn() to set the y-axis origin value to during signal processing
# the y-axis location will be reset to an aesthetic preference after signal processing
#
#
# #### main functions ####
#
#
# MASmooth() the main function, used to compute the cardio mid line and moving average
# the MA line is used for feature extraction
#
# boxcarSmoothFn() another main function to compute the midline using a boxcar convolution
# 
#
####


# This script contains all necessary functions for cardio signal processing,
# including functions to check for data problems such as systolic or diastolic peaks
# that exist on multiple adjacent samples (each peak must be a single sample), or
# missing (NA) values. These functions will also scale and offset the cardio 
# data to a desire aethetic (input parameters). They will also  compute the cardio 
# rate, using the systolic peaks, and will use systolic and diastolic peak 
# indices to compute interpolated lines for the systolic and diastolic peaks. 
# Finally, a mid-line and moving average (MA) is computed.
# The MA line is used for cardio feature extraction. 


# Interpolated systolic and diastolic lines, and the mid-line and MA-line must be 
# computed after scaling and offsetting, and must be recalculated 
# following any aestetic adjustment or dressing of the data (scaling and offsetting)
# during data visualization (data review).

# these functions work during review, after all data are recorded
# doing this in real-time is not necessary, and may be more complicated

####


library(readr)


# write_csv(chartDF, file="chartDF.csv")

chartDF <- as.data.frame(read_csv(file="chartDF.csv"))


####


fixPeak <- function(x=chartDF$c_Cardio1, times=4) {
  # R function to fix max and min peaks that are repeated 2 or more times
  # input x is the time series data with the first sample set to 0
  # the y-scale is -1000 to +1000
  # times is the number of times to repeat the operaation from 1 to 8
  # this function will keep the first of all adjacent max and min peaks
  # and interpolate the subsequent equal peak with the next sample
  ###
  # this function is useful when the cardio data is very noisy
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
} # end fixPeak() function


NAInterp <- function(x) {
  # R function to interpolate NA values in the time series data
  # when down sampling or upsampling,
  # or when data values are missing
  # called by the sigProc functions for each sensor
  # x input is a vector of the time series cardio data
  # output is a vector 
  ###
  # check if the entire input vector is NA
  if(length(which(is.na(x)))==length(x)) { return(x) }
  # fix the situation when the first value is NA
  if(is.na(x[1])) { x[1] <- x[min(which(!is.na(x)))] }
  # fix if the last value is NA
  if(is.na(x[length(x)])) { x[length(x)] <- x[max(which(!is.na(x)))] }
  # then use a loop to fix remaining values
  for(i in 2:(length(x)-1)) {
    # replace the NA value with a mean
    if(is.na(x[i])) { x[i] <- mean(x[i-1],x[min(which(!is.na(x)))]) } 
  } 
  return(x)
} # end NAInterp() function


ratePerMin <- function(x=chartDF$c_UPneumo, buffer=42, peaks="upper", lowPass=FALSE) {
  # function to calculate the cyclic rate per minute
  # for cardio and pneumo time series data
  # x input is a a time series vector
  # peaks input is a switch to choose "upper" or "lower" peaks
  # buffer input is the number of pre and post index samples to include in the search space
  # buffer = 40 for pneumo
  # buffer = 9 for cardio
  # output is the mean rate 
  # use lowPass=FALSE for respiration and lowPass=TRUE for cardio
  ####
  {
    if(!exists("lowPass")) lowPass <- FALSE
    if(!exists("buffer")) buffer <- 42
    if(!exists("peaks")) peaks <- "upper"
  }
  # smooth the cardio input to remove artifact peaks
  if(lowPass == TRUE) { 
    x1 <- lowPass2hz.2nd(x) 
  } else {
    x1 <- x
  }
  # then get the max or min peak
  ifelse(peaks=="lower",
         Peaks <- minPeak(x=x1, y=buffer, firstLast=FALSE),
         Peaks <- maxPeak(x=x1, y=buffer, firstLast=FALSE) 
  ) 
  # calculate the rate
  peakDiffs <- diff(Peaks)
  # remove peak diffs of 1 because these occur when the cardio cuff is deflated
  # peak diffs of 1 will distort the cardio rate measurement
  peakDiffs <- peakDiffs[which(peakDiffs != 1)]
  # compute the rate
  rateMin <- ifelse(length(peakDiffs)>1,
                    60 / (mean(peakDiffs) / cps ),
                    60/peakDiffs/cps
  )
  # output
  return( round(rateMin , 2) )
} # end ratePerMin() function


bufferLenFn <- function(x, y=.6) {
  # R function to calculate the buffer length for the maxPeak and minPeak functions
  # x input is the rate per min scalar output from the ratePerMin function
  # y is the number of cycles
  # output is an integer that defines the 1/2 buffer width 
  # for the number of cardiac cycles
  # in samples surrounding the index sample of the minPeak and maxPeak function
  ###
  bLen <- floor(1 / x * 60 * (cps*y)) - 1
  if(bLen <= 0) bLen <- 1
  return(bLen)
} # end bufferLenFn()


getFirstLastEventFn <- function(x=chartDF) {
  # R function to compute the row indices for the first and last events in a chart data frame
  # x input is a chart DF after signal processing
  # uses the excludedEvents variable from the global env - loaded by excludedEvent.R
  # output is a named vector of two items: firstEvent, lastEventEnd
  ###
  chartDF <- x
  # make a vector of event names
  # eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
  eventNames <- toupper(chartDF$Label[chartDF$Label!=""])
  # make a vector of event onset rows
  # eventRows <- which(chartDF$eventLabel!="")  
  eventRows <- which(chartDF$Label!="")  
  # get the onset of the first event and the end of the last event
  if(length(eventNames)==0) {
    print("no stimulus events. none processed.rn")
    # next()
    firstEvent=1
    lastEventEnd=nrow(chartDF)
  } else {
    # get the first event for scaling and centering
    firstEvent <- eventRows[!(eventNames %in% excludeEvents)][1]
    # fix missing firstEvent
    if(is.na(firstEvent)) { firstEvent <- 1 }
    # get the last event for scaling and centering
    lastEvent <- eventRows[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
    eventNames[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
    
    # fix missing lastEvent
    if(length(lastEvent)==0) { lastEvent <- nrow(chartDF) }
    # changed 10-1-2016 because some examiners rush to end the chart recording 
    # and others present more stimuli after the XX end announcement
    # lastEventEnd <- lastEvent
    # changed back 11-1-2016
    # use the measuredSeg and cps values from the global env
    lastEventEnd <- lastEvent + measuredSeg * cps
    if(length(lastEventEnd)==0) { lastEventEnd <- nrow(chartDF) }
    # fix potential problem when lastEventEnd exceeds the data frame rows for the chart
    if(lastEventEnd > nrow(chartDF)) { lastEventEnd <- nrow(chartDF) }
    if(is.na(lastEventEnd)) { lastEventEnd <- nrow(chartDF) }
  }
  outVector <- c(firstEvent, lastEventEnd)
  names(outVector) <- c("firstEvent", "lastEventEnd")
  return(outVector)
} # end getFirstLastEventFn()


scaleDataFn <- function(x=chartDF$c_Cardio1, 
                        sec=30, 
                        times=8, 
                        ignore=6, 
                        yRange=300, 
                        maxY=950, 
                        minY=-950,  
                        firstRow=NULL, 
                        lastRow=NULL) {
  # R function to scale the time series data 
  # Raymond Nelson
  ### input
  # x input is the time series vector for one sensor from a single chart
  # sec is the number of seconds to sample
  # times is the number of random samples to get
  # ignore is an integer that specifies the number of largest samples,
  # to ignore in the scaling calcluation
  # yRange is the range to scale the data within (the y-axis chart range is -1000 to +1000)
  # maxY is the max y value for the data on the printed chart
  # minY is the min y value for the data on the printed chart
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  ### output
  # output is a the rescaled time series vector vector
  # also assigns a scalar scaleVal to the global environment,
  # for use when scaling the additional channels
  ####
  {
    if(!exists("firstRow")) firstRow <- 1
    if(!exists("lastRow")) lastRow <- length(x)
    if(is.null(firstRow)) firstRow <- 150
    if(is.null(lastRow)) lastRow <- length(x) - 450
  }
  # exit if no data
  if(x[1] == -9.9) { return(x) }
  # for replication of the random scaling process
  set.seed(1234567890)
  # get the length of the input vector
  dataLength <- length(x)
  # exit if the vector is NA
  if(length(which(is.na(x)))==dataLength) return(x)
  # determine the number of items to include in each sample
  sampleLength <- sec * cps 
  # check the firstRow and lastRow
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength-sampleLength + 1
  # short input vector
  if(dataLength <= (45 * cps)) {
    # for short charts less than 45 seconds
    if(max(x, na.rm=TRUE) == min(x, na.rm=TRUE)) {
      # for flatline data
      dataRange <- 1
    } else {
      # use the entire vector
      dataRange <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    } # end else
    scaleVal <- yRange / dataRange
    # scale the output vector
    xOut <- x * scaleVal
    # initialize the default rescale value for this if condition
    rescaleVal <- 1
  } else {
    # for charts > 45 seconds
    # get the sample onset row indices using random numbers
    if(firstRow < (lastRow-sampleLength)) {
      # for most charts - get the sample onset indices
      # check the length and times
      if(length(c(firstRow:(lastRow-sampleLength))) <= times) { 
        times <- length(c(firstRow:(lastRow-sampleLength))) 
      } 
      { 
        # get the sample onset and end indices
        # set the first sample at 1/2 the sampling period
        if(times > 1) {
          # adjustment to work with a single sample iteration
          sampleInterval <- trunc(sec / 2) * cps
          sampleOnset <- sampleInterval
          while(sampleOnset[length(sampleOnset)] < (lastRow-sampleLength)) {
            # while the last sample onset does not exceed the last data row
            sampleOnset <- c(sampleOnset, sampleOnset[length(sampleOnset)] + sampleInterval)
          } 
          sampleOnset <- sampleOnset[sampleOnset < (lastRow-sampleLength)]
          if(length(sampleOnset) == 0 ) sampleOnset <- firstRow
        } else { 
          # if the sample times is 1
          sampleOnset <- firstRow
        } #
        # then get the sample offset indices
        sampleEnd <- sampleOnset + sampleLength - 1
        # make sure that samples do not exceed the time series data
        if(any(sampleEnd > dataLength)) {
          # for short/aborted charts
          sampleEnd <- dataLength
        } else {
          sampleEnd <- sampleEnd[sampleEnd <= dataLength]
        }
        # adjust the times parameter
        times <- length(sampleEnd)
        sampleOnset <- sampleOnset[c(1:times)]
        # adjust the ignore parameter
        ignore <- round(times * .5)
      } # end else
      # make a data frame for the sample segments
      DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
      # each row is a sample from the time series input 
      # populate the data frame
      for (i in 1:times) {
        DF[i,] <- x[sampleOnset[i]:sampleEnd[i]]
      }
      # make a private function to get the range from the DF rows
      dfRangeFn <- function(x) { max(x, na.rm=TRUE)-min(x, na.rm=TRUE) }
      # apply the range function to each row of the DF to get the range of each row
      # apply() is vectorized and needs no loop in R
      dfRange <- apply(DF, 1, dfRangeFn)
      # check and fix the ignore value if necessary
      if(ignore >= length(dfRange)) ignore <- round(length(dfRange) / 2, 0)
      # sort and remove the largest changes using the ignore input parameter 
      dfRange <- sort(dfRange, decreasing = TRUE)[(ignore+1):length(dfRange)]
      # summary(dfRange)
      # get the scaling value from the dfRange
      if( median(dfRange)!=0 ) {
        # scaleVal <- yRange/median(dfRange)
        scaleVal <- yRange / mean(dfRange)
      } else {
        # to avoid /0 problems with flat-line data
        scaleVal <- 1
      } # end else 
    } else { 
      # for charts that are shorter than the sampling length
      # set the scale value using the max and min
      if(max(x[firstRow:lastRow]) != min(x[firstRow:lastRow])) {
        scaleVal <- yRange / abs( max(x[firstRow:lastRow]) - min(x[firstRow:lastRow]) )
      } else {
        # for flat-line data
        scaleVal <- 1 
      } # end else for charts shorter than the sampling length
    } # end else
    # scale the output vector
    xOut <- x * scaleVal
    # get the offset max value and offset the data if necessary
    offsetMaxVal <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
    if(offsetMaxVal < 0) { xOut <- xOut + offsetMaxVal }
    # get the offset min value and offset the data if necessary
    offsetMinVal <- minY - min(xOut[firstRow:lastRow], na.rm=TRUE)
    if(offsetMinVal > 0) { xOut <- xOut + offsetMinVal }
    # check the range again
    newRange <- 
      max(xOut[firstRow:lastRow], na.rm=TRUE) - min(xOut[firstRow:lastRow], na.rm=TRUE)
    # initialize the default rescale value for this else condition
    rescaleVal <- 1
    # rescale the data if the range exceeds the maxY and minY values
    # if(newRange > ( maxY - (minY + (.4 * yRange))) ) { 
    # adjust the maxY
    if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
      maxOffset <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + maxOffset
    }
    # adjust the minY
    if(min(xOut[firstRow:lastRow], na.rm=TRUE) < minY) {
      rescaleVal <- ( maxY - minY ) / newRange
      xOut <- xOut * rescaleVal
    }
    # check the max again
    if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
      maxOffset <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + maxOffset
    }
  } # end else for charts > 45 seconds
  # assign the scale value to the global environment
  # the scale value needs to be used to scale additional channels for each sensor
  assign("scaleVal", (scaleVal * rescaleVal), pos=1)
  return(xOut)
} # end scaleDataFn()  


offsetDataFn <- function(x=chartDF$c_Cardio1, 
                         y=-250, 
                         maxY=950, 
                         minY=-950, 
                         firstRow=NULL, 
                         lastRow=NULL) {
  # R function to offset the time series data for plotting
  ###
  # x input is the time series vector
  # y is the offset value to locate the onset of the first event
  # yMax is the maximum y value to display the data (less than the max y index on the chart)
  # yMin is the minimum y value to display the data (more than the min y index on the chart)
  # firstRow is the row index of the onset of the first stimulus event in the time series data
  # lastRow is the row index of the end of evaluation window for the last stimulus event in the time series data 
  # firstRow and lastRow should ignore the X and XX announcements
  # output is the offset time series data
  # also places 2 scalars newScaleVal and offsetVal in the global environment
  ####
  {
    if(!exists("firstRow")) firstRow <- 1
    if(!exists("lastRow")) lastRow <- length(x)
    if(is.null(firstRow)) firstRow <- 150
    if(is.null(lastRow)) lastRow <- length(x) - 450
  }
  # exit if NA
  if(length(which(is.na(x)))==length(x)) return(x)
  # continue
  dataLength <- length(x)
  # check the firstRow and lastRow input parameters
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength
  # initialize the offset value
  newOffsetAdjust <- 0
  # initialize a new scale value
  newScaleVal <- 1
  # offset the data
  if(dataLength > (30*cps)) {
    # for charts greater than 30 seconds
    # get the offset value for the first event
    offsetVal <- x[firstRow]
    # compute the offset adjustment
    offsetAdjust <- y - offsetVal
    # offset the data
    xOut <- x + offsetAdjust
    offsetAdjust2 <- y - quantile(xOut, .2, na.rm=TRUE)
    xOut <- xOut + offsetAdjust2
    # get the max and min values
    Xmax <- max(xOut[firstRow:lastRow], na.rm=TRUE)
    Xmin <- min(xOut[firstRow:lastRow], na.rm=TRUE)
    # check to ensure that the data to not exceed the difference between maxY and minY
    if(Xmax - Xmin > maxY-minY) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (Xmax - Xmin)
      # rescale the data
      xOut <- xOut * newScaleVal
    } 
    # re-check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(min(xOut[firstRow:lastRow], na.rm=TRUE) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    }
    if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    } 
  } else { 
    # for shart charts less than 30 seconds
    offsetVal <- median(x)
    # use the median of x instead of the onset of the first event
    offsetAdjust <- y - offsetVal
    xOut <- x + offsetVal
    # June 20, 2025
    Xmax <- max(xOut[firstRow:lastRow], na.rm=TRUE)
    Xmin <- min(xOut[firstRow:lastRow], na.rm=TRUE)
    # check to ensure that the data to not exceed the difference between maxY and minY
    if(Xmax - Xmin > maxY-minY) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (Xmax - Xmin)
      # rescale the data
      xOut <- xOut * newScaleVal
    } 
    # re-check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(min(xOut[firstRow:lastRow], na.rm=TRUE) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    } 
    if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    }
  } # end short charts < 30 seconds
  assign("newScaleVal", newScaleVal, pos=1)
  assign("offsetVal", offsetVal + newOffsetAdjust, pos=1)
  return(xOut)
} # end offsetDataFn() function


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
} # end cardioRatePerCycleFn()


maxPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # R function to get the cyclic peaks from the time series data
  # in the sigProcHelper.R script
  # will keep the index number of max peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
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
}  # end maxPeak()


minPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # R function to get the cyclic peaks from the time series data
  # in the sigProcHelper.R script
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
} # end minPeak() function


interpolatePeaks <- function(x=na.omit(maxOut), y=na.omit(maxVal)) {
  # private function to interpolate between peak segments of time series input data
  # x is a vector of peak row indices in the data vector
  # y is a vector of peak row values in the data vector
  # output is a vector
  ###
  # first get the difference vectors
  peakOutDiff <- diff(x)
  peakValDiff <- diff(y)
  # then make sure the difference vectors are equal length
  if (length(peakValDiff) < length(peakOutDiff)) {
    peakValDiff <- c(peakValDiff, rep(peakValDiff[length(peakValDiff)], times=(length(peakOutDiff)-length(peakValDiff))))
  }
  # use a divisor
  # calculate the vector of difference values for samples between each peak
  peakValDiff2 <- peakValDiff / peakOutDiff
  # calculate a filled vector by repeating each difference value according to the difference between peaks
  peakFill <- rep(peakValDiff2, times=peakOutDiff)
  # then calculate the cumulative sum of the peak fill vector as the interpolated peak line
  # include the initial value from the vector of input values 
  # to restore the length of the data vector and set the onset value 
  peakFill2 <- cumsum(c(y[1], peakFill))
  # to preserve the exact peak values for verification of the interpolation
  # peakFill2[x] <- y # 
  return(peakFill2)
} # end interpolatePeaks()


centerColumn <- function(x) {
  # function to set the onset value of the sensor data to zero
  # called by the preProc function
  # x is a data column from a data frame of recorded time series data
  # output is a vector of centered values
  if(x[1] == -9.9) { return(x) }
  ifelse(max(x, na.rm=TRUE)==min(x, na.rm=TRUE),
         x <- x*0,
         x <- x - x[1] )
  return(x)
} # end centerColumn() function



########### main function #############



MASmooth <- function(x=myData, y, times) {
  # $ function to calculate a smoothed average of the time series data
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to smooth the data
  #
  # output is a smoothed time series vector
  # 
  # this function can be used to remove high frequency noise from EDA and respiration data,
  # and can also compute the cardio moving average (MA) for feature extraction
  ###
  # make the output vector 
  # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
  xOut <- x
  # loop over the number of times
  for (j in 1:times) {
    # for loop to compute the moving average
    # buffer will be double the offset value + 1
    input_buffer <- x[1:(2*y+1)]
    # starts at sample y + 1
    for (i in (y+1):(length(x)-y)) { 
      # replace the middle value of the buffer with the mean
      xOut[i] <- mean(input_buffer) 
      # increment the input buffer
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
    } 
    # fix the start and end segments
    xOut[1:y] <- xOut[y+1]
    xOut[(length(xOut)-y):length(xOut)] <- xOut[length(xOut)-(y+1)]
    # replace the input vector
    x <- xOut # commenting this out negates the "times" input parameter
  } # end loop over j times
  return(xOut)
} # end MASmooth function()



boxcarSmoothFn <- function(x, cps=30, mult=c(1,0.5,0.25)) {
  # R function to compute the cardio midline using a 
  # Raymond Nelson
  # October 22, 2025
  ##
  # another method of smoothing, instead of cascaded moving averages,
  # using convolution and private functions for the smoothing cascade
  # 
  # output of this filter is identical 
  # to the cascade of MASmooth() with 1sec, .5sec, and .25sec
  #
  ##
  # x is the time series cardio data
  # cps is the sampling rate
  # mult is a vector of time periods (seconds) for each stage of the multi-stage filter
  outVc <- x
  # define a private functions
  .apply_stage <- function(x,y){
    # private function to call the convolve function with the computed smoothing space
    # x is the time series cardio data
    # y is the number of samples in the boxcar (moving window)
    # this private function will call the .boxcar and .convolv_open functions
    ##
    n <- length(x)
    # compute the boxcar (moving window)
    boxcar <- rep(1/(2*y+1), 2*y+1)
     # call the convolve() function to compute the smoothed vector
    full <- convolve(x,rev(boxcar),type="open")
    # remove the start values to avoid phasing
    z <- full[(y+1):(y+n)]
    # fix the edge values at the start of the z vector
    z[1:y] <- z[y+1] 
    # fix the edge values at the end of the z vector
    z[(n-y+1):n] <- z[n-(y+1)]
    # output
    return(z)
  }
  # iterate over the items in mult to execute the multi-stage smoothing
  for(m in mult) {
    # m will be each item in mult
    y <- as.integer(round(m*cps,0))
    # call a function to execute each stage of the convolution and update the outVc
    outVc <- .apply_stage(outVc,y)
  }
  return(outVc)
} # end boxcarSmoothFn()



#### 



################# call the cardio signal processing functions #################



{
  
  ## check for problems, and center the cardio data so the origin == 0 ##
  
  # fix NA values
  chartDF$c_Cardio1 <- NAInterp(chartDF$c_Cardio1)
  
  chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1, times=3)
  # this is also done in the scaleOffsetData script
  
  # offset the data so the first sample value is 0 
  chartDF$c_Cardio1 <- centerColumn(x=chartDF$Cardio1)
    
}

{
  
  ## comput the cardio rate ##
  
  # first make a rough estimate of the cardio rate
  cardioRate <- ratePerMinFn(x=chartDF$c_Cardio1, buffer=10, peaks="upper", lowPass=TRUE)
  # use the rough estimate to compute the cardio buffer length
  cardioBufferLen <- bufferLenFn(x=cardioRate, y=.6)
  # use the cardioBufferLen to compute a more precise vector of the cardio rate
  
  # call a function to compute the cardiac rate at each pulse
  chartDF$c_CardioRate <- cardioRatePerCycleFn(x=chartDF$c_Cardio1, buffer=cardioBufferLen, peaks="upper", lowPass=TRUE)
  
}


{
  
  ## get the indices for the first and last events (usally X and XX) ##
  
  firstLastEvents <- getFirstLastEventFn(x=chartDF)
  firstEvent <- firstLastEvents[1] - 150
  lastEventEnd <- firstLastEvents[2] + 300
  
  if(firstEvent < 1) firstEvent <- 1
  if(lastEventEnd > nrow(chartDF)) lastEventEnd <- nrow(chartDF)
  
}

{
  
  ## scale and offset the data ##
  
  # scale the cardio data 
  chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=30, times=8, ignore=6, yRange=300, maxY=950, minY=-950, firstRow=firstEvent, lastRow=(lastEventEnd-300))
  
  # offset the cardio data
  chartDF$c_Cardio1 <- offsetDataFn(x=chartDF$c_Cardio1, y=-250, maxY=950, minY=-950, firstRow=firstEvent, lastRow=(lastEventEnd-300))
  
}

{
  
  ## cardio midline - not used for feature extraction ##
  
  # compute the cardio mid line for artifact monitoring
  chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=1)
  # this line has some visible pulse information
  
  # the smoothing filter (.5 seconds) will produce the same output 
  # as a first order butteroworth filter of .886Hz 
  # the MASmooth() has the advantage that it will be exactly centered 
  # within the cardio data, where as the Butterworth will be difficult to center
  
}

{
  
  ## cardio moving average - used for feature extraction ##
  
  # then compute a slower moving average for cardio feature extraction
  chartDF$c_CardioMA <- boxcarSmoothFn(x=chartDF$c_Cardio1, cps=30, mult=c(1,0.5,0.25))
  # this line has substantially less visible pulse and follows the raw cardio very nicely
  
  # the boxcar function is equivalent to the following cascaded smoothing function
  DAT2 <- MASmooth(x=chartDF$c_Cardio1, y=round(1*cps,0), times=1)
  DAT2 <- MASmooth(x=DAT2, y=round(.5*cps,0), times=1)
  DAT2 <- DAT2, y=round(.25*cps,0), times=1)
  
  # the output can be approximated using a 4th order Butterworth filter at .11Hz
  # however the output of Butterworth filter is not conveniently centered 
  
  plot.ts(chartDF$c_Cardio1, col="grey")
  lines(chartDF$c_CardioMA, col="black", lwd=1)
  lines(DAT2, col="red", lwd=1)
  
}

{
  
  ## compute the systolic line ##
  
  # locate the systolic peaks
  # cardioBufferLen was computed earlier in this script
  systPeaks <- maxPeak(x=chartDF$c_Cardio1, y=cardioBufferLen)
  
  # interpolate the systolic line
  chartDF$c_CardioSystolic <-
    interpolatePeaks(x=systPeaks,
                     y=chartDF$c_Cardio1[systPeaks])[1:nrow(chartDF)]
  
}

{
  
  ## compute the diastolic line ##
  
  # locate the diastolic peaks
  diastPeaks <- minPeak(x=chartDF$c_Cardio1, y=cardioBufferLen)
  
  # interpolate the diastolic line
  chartDF$c_CardioDiastolic <-
    interpolatePeaks(x=diastPeaks,
                     y=chartDF$c_Cardio1[diastPeaks])[1:nrow(chartDF)]
  
}

{ 
  
  ## cardio artifacts ##
  
  # a very simple cardio artifact paradigm using the systolic and diastolic peaks
  
  # after scaling and offsetting
  # get the y-axis values for all systolic peak and all diastolic peaks
  
  ## systolic 
  
  systPeakVals <- chartDF$c_Cardio1[systPeaks]
  
  systPeakDiffs <- diff(systPeakVals)
  
  # a vector of indices where artifacts are observed at the systolic line
  systolicArtifacts <- which(systPeakDiffs >= 100) # 5% of the y-axis range
  # these artifacts may be voluntary or involuntary and may or may not be countermeasures
  
  ## diastolic
  
  diastPeakVals <- chartDF$c_Cardio1[diastPeaks]
  
  diastPeakDiffs <- diff(diastPeakVals)
  
  # a vector of indices where artifacts are observed at the diastolic line
  diastolicArtifacts <- which(diastPeakDiffs >= 100) # 5% of the y-axis range
  # these artifacts may be voluntary or involuntary and may or may not be countermeasures
  
}




