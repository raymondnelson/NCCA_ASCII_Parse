# helper functions for signal processing and feature extraction
# 8-23-2105
#
# these functions are called by the signal processing function
#
# #####

# sampleSpace() function to calculate the sample space for a pulse rate
# 
# pulseRate() function to calculate the pulse rate for a sample space
#
# fixPeak() function to ensure that peak points are recorded at a single sample
#
# minPeak() to get the min peak row and value from an occilating time series
#
# maxPeak() to get the max peoak
# 
# minMaxPeakFn() to get both the min and max peak values 
# 
# interpolatePeaks() to interpolate the space between min peaks or max peaks
#
# MASmooth() function to create a moving average smoothing filter to reduce the time series to a slow wave form
#
# minMaxMean() another method to reduce the diastolic and systolic data to a single line
#
# ratePerMin() function to estimate the cyclic rate per min, used to set the buffer length for peak extraction
#
# bufferLenFn() to set the buffer length for peak extraction from pneumo or cardio data
# 
# scaleDataFn() function to resize the data for plotting
# 
# offsetDataFn() function to offset the data for plotting
#
# centerColumn() to center the sensor data at zero
#
# setColRange() to set the initial range for each sensor 
# 
# NAInterp() to interpolate missing values in the sensor data
# 
# PercentRank() to calculate the percentile rank of a single value and a vector 
#
# eventNamesFn() to compute the indices for the first event onset and last event end
# 
# getFirstLastEventFn() to compute the indices for the first event onset and last event end
# 
# colSDs() to compute the columns standard deviations for a data frame
#
# cardioLeakFn() to check for leaking or descending cardio sensor data
#
# SRQ2001_cardioSquareWaveFn() cardio square wave function
# 
# cardioSRQFn() cardio reduction using the method of Stern, Ray & Quigley (2001)


###############################################


library(stringr)


####


pulseRate <- function(x=30, y=30) {
  # function to calculate the pulse rate per min for an input sample space
  # x is the sampling rate in seconds
  # y is the number of samples per cycle
  secsPerSample <- x / y 
  pulseRate <- secsPerSample * 60
  return(pulseRate)
}
# pulseRate(x=cps, y=32)


####


sampleSpace <- function(x=30, y=17) {
  # function to calculate the sample space for an input pulse rate
  # x is the sampling rate in seconds
  # y is the pulse rate per minute
  # output is the number of samples
  secsPerCycle <- 60 / y
  samplesPerCycle <- secsPerCycle * x
  return(samplesPerCycle)
  
}
# sampleSpace(x=cps, y=17)


####


fixPeak <- function(x=chartDF$c_Cardio1, times=4) {
  # function to fix max and min peaks that are repeated 2 or more times
  # input x is the time series data
  # times is the number of times to repeat the operaation from 1 to 8
  # this function will keep the first of all adjacent max and min peaks
  # and interpolate the subsequent equal peak with the next sample
  #
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

# cOut <- fixPeak(x=chartDF$c_Cardio1, times=2)
# diff(maxPeak(x=chartDF$c_Cardio1, y=7))
# length(diff(maxPeak(x=chartDF$c_Cardio1, y=7)))
# length(which(diff(maxPeak(x=chartDF$c_Cardio1, y=7))==1))
# diff(maxPeak(x=cOut, y=7))
# length(diff(maxPeak(x=cOut, y=7)))

# aOut <- fixPeak(x=chartDF$c_SE, times=2)
# diff(maxPeak(x=chartDF$c_SE, y=7))
# length(diff(maxPeak(x=chartDF$c_SE, y=7)))
# length(which(diff(maxPeak(x=chartDF$c_SE, y=7))==1))
# diff(maxPeak(x=cOut, y=7))
# length(diff(maxPeak(x=cOut, y=7)))


####


# use y=12 for slow pulse rate below 60
# round(.4*cps,0)

minPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # private function to get the cyclic peaks from the time series data
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
} # end minPeak function


####


maxPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # function to get the cyclic peaks from the time series data
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
}  # end maxPeak

# maxPeak(x=chartDF$c_Cardio1, y=7, firstLast=TRUE)
# diff(maxPeak(x=chartDF$c_Cardio1, y=7, firstLast=TRUE))
# mean(diff(maxPeak(x=chartDF$c_Cardio1, y=7, firstLast=TRUE)))
# 60 / ( mean(diff(maxPeak(x=chartDF$c_Cardio1, y=7, firstLast=TRUE))) / cps )

# maxPeak(x=chartDF$c_UPneumo, y=40, firstLast=TRUE)
# diff(maxPeak(x=chartDF$c_UPneumo, y=40, firstLast=TRUE))
# mean(diff(maxPeak(x=chartDF$c_UPneumo, y=40, firstLast=TRUE)))
# 60 / ( mean(diff(maxPeak(x=chartDF$c_UPneumo, y=40, firstLast=TRUE))) / cps )


####


minMaxPeakFn <- function(x=chartDF$c_Cardio1, y=round(.25*cps,0)) {
  # function to get the diastolic and systolic peaks from the cardio data
  # in the sigProcHelper.R script
  # x input is a time series vector
  # y input is the number of offset samples
  # xOut is the length of the input vector and always includes the first and last sample
  xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
  # buffer will be double the offset value
  input_buffer <- x[2:(2*y)+1] # start at 2 because we already keep the first sample
  for (i in 2:(length(x)-(2*y)+1)) {
    input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+(2*y)])
    # check to determine if the middle value is the min or max
    ifelse(input_buffer[(y+1)]==min(input_buffer),
           xOut[(i+y+1)] <- (i+y+1),
           ifelse(input_buffer[(y+1)]==max(input_buffer),
                  xOut[(i+y+1)] <- (i+y+1),
                  next() ) )
    } # end for loop
  return(as.numeric(na.omit(xOut)))
} # end minMaxPeakFn() function

# minMaxPeak()
# plot.ts(x[minMaxPeak()])


####


interpolatePeaks <- function(x=na.omit(maxOut), y=na.omit(maxVal)) {
  # private function to interpolate between peak segments of time series input data
  # x is a vector of peak row indices in the data vector
  # y is a vector of peak row values in the data vector
  # output is
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
} # end interpolatePeaks

# diastolicInterp <- c(interpolatePeaks(x=minOut, y=minVal),0 )
# plot.ts(diastolicInterp, ylim=c(-3,10))


####


# myData <- chartDF$c_Cardio1

# function to smooth the time series data to the midpoint
MASmooth <- function(x=myData, y=round(.25*cps,0), times=5) {
  # function to calculate a smoothed average of the time series data
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to smooth the data
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
    # x <- xOut
  } # end loop over j times
  return(xOut)
} # end MASmooth function()

# smoothedCardio <- MASmooth(x=MASmooth(x=MASmooth()))
# smoothedCardio <- MASmooth(x=chartDF$Cardio1, y=15, times = 4)
# plot.ts(MASmooth[1:3000], ylim=c(-3,10))


####


# function to smooth the time series data to the midpoint
MASmoothB <- function(x=myData, y=round(.5*cps,0), times=1) {
  # function to calculate a smoothed average of the time series data
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to smooth the data
  ###
  # make the output vector 
  xOut <- rep(mean(x[1:y]), times=length(x))
  # loop over the number of times
  for (j in 1:times) {
    # buffer 
    input_buffer <- x[1:y] 
    # for loop to compute the moving average
    for (i in y:length(x)) { 
      xOut[i-round(y/2)] <- mean(input_buffer)
      # move the input buffer
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i])
    } # end for loop for moving average
    # replace the input vector
    # x <- c(xOut[2:length(xOut)], xOut[length(xOut)])
    x <- xOut
  } # end loop over times
  xOut[(i-round(y/2)):length(xOut)] <- xOut[i-round(y/2)]
  # return(na.omit(xOut))
  return(xOut)
} # end MASmoothB function()

# plot.ts(chartDF$c_SE[1:1000])
# plot.ts(MASmoothB(chartDF$c_SE, 8, times=1)[1:1000])


####


MASmoothC <- function(x=chartDF$c_SE[1:1000], y=round(.5*cps,0)) {
  # times must be greater equal than y + 1
  xOut <- x
  input.buffer <- xOut[1:(y-1)]
  for (i in y:length(xOut)) {
    input.buffer <- c(input.buffer, xOut[i])
    xOut[i-round(y/2,0)] <- mean(input.buffer)
    # xOut[i] <- mean(input.buffer)
    # input.buffer[round(length(input.buffer/2),0)] <- mean(input.buffer)
    input.buffer[round(y/2,0)] <- mean(input.buffer)
    input.buffer <- input.buffer[2:y]
  }
  # for (i in (y+times-1):length(x)) {
  #   input.buffer <- c(input.buffer[1:length(input.buffer)], xOut[i])
  #   for (j in 1:y) {
  #     input.buffer[(i-times+j)] <- mean(input.buffer[(i-y+1-times+j):(i-times+j)])
  #   }
  #   xOut[i] <- input.buffer[length(input.buffer)]
  #   input.buffer <- input.buffer[2:length(input.buffer)]
  # }
  return(xOut)
}


# plot.ts(chartDF$c_SE[1:5000])
# plot.ts(MASmoothC(chartDF$c_SE, 4)[1:5000])


####


minMaxMean <- function(x=chartDF$c_Cardio1, y=round(.8*cps,0)) {
  #  function to compute the mean of the min and max for a moving window 
  # x is the time series cardio data
  # y is 1/2 the number of samples in the buffer
  xOut <- c(rep(mean(x[1:y]), times=y), 
            numeric(length(x)-2*y), 
            rep(mean(x[length(x)-2*y+1:length(x)]), times=y))
  input_buffer <- x[1:(2*y)]
  for (i in (y+1):(length(x)-y+1)) {
    xOut[i] <- mean(min(input_buffer), max(input_buffer))
    input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y])
  } 
  xOut <- na.omit(xOut)
  xOut <- c(xOut, rep(xOut[length(xOut)], times=(y-1)))
  return(xOut) 
} # end minMaxMean function


####


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

  rateMin <- ifelse(length(peakDiffs)>1,
                    60 / (mean(peakDiffs) / cps ),
                    60/peakDiffs/cps
  )
  
  return( round(rateMin , 2) )
  
} # end ratePerMin() function

# ratePerMin(x=chartDF$c_Cardio1, buffer=9, peaks="upper", lowPass=FALSE)
# ratePerMin(x=chartDF$c_CardioMid, buffer=8)
# ratePerMin(x=chartDF$c_UPneumo, buffer=40, peaks="upper", dataRate=cps, lowPass=TRUE)
# ratePerMin(x=chartDF$c_Cardio1, , buffer=7peaks="upper")
# ratePerMin(lowPass2hz(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps)
# ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper")


#### 


# 3-3-2017 was y=.67
bufferLenFn <- function(x, y=.6) {
  # function to calculate the buffer length for the maxPeak and minPeak functions
  # x input is the rate per min scalar output from the ratePerMin function
  # y is the number of cycles
  # output is an integer that defines the 1/2 buffer width 
  # for the number of cardiac cycles
  # in samples surrounding the index sample of the minPeak and maxPeak function
  bLen <- floor(1 / x * 60 * (cps*y)) - 1
  if(bLen <= 0) bLen <- 1
  return(bLen)
}
# bufferLenFn(ratePerMin(chartDF$c_Cardio1,buffer=3,dataRate=30,peaks="upper",lowPass=TRUE))


####


scaleDataFn <- function(x=chartDF$c_UPneumoSm, 
                        sec=6, 
                        times=20, 
                        ignore=2, 
                        yRange=(.1*yRange), 
                        maxY=(yMax-.05*yRange), 
                        minY=(yMin+.05*yRange),  
                        firstRow=firstEvent, 
                        lastRow=(lastEventEnd-450)) {
  # helper function to scale the time series data 
  # this function is located in the sigProcHelper.R script
  # 4-17-2016
  # modified 1/25/2019
  # Raymond Nelson
  # this is called by the ScaleOffsetDataFn() function
  ### input
  # x input is the time series vector for a a single time series channel,
  #  from a single chart
  # sec is the number of seconds to sample
  # times is the number of random samples to get
  # ignore is an integer that specifies the number of largest samples,
  # to ignore in the scaling calcluation
  # yRange is the range to scale the data within
  # maxY is the max y value for the data on the printed chart
  # minY is the min y value for the data on the printed chart
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  ### output
  # output is a the rescaled time series vector vector
  # also assigns a scalar scaleVal to the global environment,
  # for use when scaling the additional channels
  ####
  if(!exists("firstRow")) firstRow <- 1
  if(!exists("lastRow")) lastRow <- length(x)
  
  if(x[1] == -9.9) { return(x) }
  
  set.seed(1234567890)
  
  dataLength <- length(x)
  # exit if the vector is NA
  if(length(which(is.na(x)))==dataLength) return(x)
  # determine the number of items to include in each sample
  sampleLength <- sec * cps 
  # check the firstRow and lastRow
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength-sampleLength + 1
  
  
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
    # sample the data
    # get the sample onset row indices using random numbers
    if(firstRow < (lastRow-sampleLength)) {
      # for most charts - get the sample onset indices
      # check the length and times
      if(length(c(firstRow:(lastRow-sampleLength))) <= times) { 
         times <- length(c(firstRow:(lastRow-sampleLength))) 
      } 
      
      { 
        
        # get the sample onset and end indices
        
        # old method - commented out 2023Jul16
        # sampleOnset <- sample(c(firstRow:(lastRow-sampleLength)), size=times)
        
        # new method is fixed so that results will not vary
        # July 16, 2023
        # set the first sample at 1/2 the sampling period
        if(times > 1) {
          # Aug 2, 2023 adjustment to work with a single sample iteration
          sampleInterval <- trunc(sec / 2) * cps
          sampleOnset <- sampleInterval
          while(sampleOnset[length(sampleOnset)] < (lastRow-sampleLength)) {
            # while the last sample onset does not exceed the last data row
            sampleOnset <- c(sampleOnset, sampleOnset[length(sampleOnset)] + sampleInterval)
          } 
          sampleOnset <- sampleOnset[sampleOnset < (lastRow-sampleLength)]
          if(length(sampleOnset) == 0 ) sampleOnset <- firstRow
        } else { 
          sampleOnset <- firstRow
        } 
        
        # then get the sample offset indices
        sampleEnd <- sampleOnset + sampleLength - 1
        # make sure that samples do not exceed the time series data
        if(any(sampleEnd > dataLength)) {
          # improved for short/aborted chart July 28, 2024
          sampleEnd <- dataLength
        } else {
          sampleEnd <- sampleEnd[sampleEnd <= dataLength]
        }
        # adjust the times parameter
        times <- length(sampleEnd)
        sampleOnset <- sampleOnset[c(1:times)]
        # adjust the ignore parameter
        ignore <- round(times * .5)
        
      }
      
      # make a data frame for the sample segments
      DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
      # each row is a sample from the time series input 
      # populate the data frame
      for (i in 1:times) {
        DF[i,] <- x[sampleOnset[i]:sampleEnd[i]]
      }
      # View(DF) 
      
      # make a function to get the range from the DF rows
      dfRangeFn <- function(x) { max(x, na.rm=TRUE)-min(x, na.rm=TRUE) }
      # apply the range function to each row of the DF to get the range of each row
      # apply is vectorized and needs no loop
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
      }
    } # end else
    # scale the output vector
    xOut <- x * scaleVal
    # get the offset max value and offset the data if necessary
    offsetMaxVal <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
    # 12-31-2016 change this to + offsetVal
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
    
    # if(newRange >  maxY - minY ) { 
    #   # not sure why .4 * yRange
    #   # rescaleVal <- ( maxY - (minY + (.4 * yRange)) ) / newRange
    #   rescaleVal <- ( maxY - minY ) / newRange
    #   xOut <- xOut * rescaleVal
    # } # end if
     
  } # end else
  # assign the scale value to the global environment
  # the scale value needs to be used to scale additional channels for each sensor
  assign("scaleVal", (scaleVal * rescaleVal), pos=1)
  return(xOut)
} # end scaleDataFn()  


####


offsetDataFn <- function(x=chartDF$c_Cardio1, 
                         y=0, 
                         maxY=(yMax-.05*yRange), 
                         minY=(yMin+.05*yRange), 
                         firstRow=firstEvent, 
                         lastRow=(lastEventEnd-450)) {
  # function to offset the time series data for plotting
  # 4-17-2016
  # modified 1/25/2019
  # Raymond Nelson
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
  #####
  
  if(!exists("firstRow")) firstRow <- 1
  if(!exists("lastRow")) lastRow <- length(x)
  
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
    
    # Nov 18, 2023 improvement to prevent occasional odd placement of EDA
    # offsetVal2 <- y - xOut[firstRow]
    # offsetAdjust2 <- offsetVal2 - quantile(c(x), .25, na.rm=TRUE)
    
    # max(xOut)
    # min(xOut)
    # plot.ts(xOut)
    # summary(xOut)
    
    offsetAdjust2 <- y - quantile(xOut, .2, na.rm=TRUE)
    xOut <- xOut + offsetAdjust2
    
    # plot.ts(xOut)
    
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


####


centerColumn <- function(x) {
  # function to set the onset value of the sensor data to zero
  # called by the preProc function
  # x is a data column from a data frame of recorded time series data
  # output is a vector of centered values
  if(x[1] == -9.9) { return(x) }
  ifelse(max(x, na.rm=TRUE)==min(x, na.rm=TRUE),
         x <- x*0,
         x <- x - x[1]
  )
  return(x)
} # end private centerColumn() function


####


setColRange <- function(DAT, y=30000, firstRow=firstEvent, lastRow=lastEventEnd) {
  # function to set each column range
  # called by the sigProc function
  # in the sigProcHelper.R script
  # DAT is a zero centered column from the data frome of recorded time series data
  # y is the max range value 
  ###
  # set the range for the segment from 1 to 3 min or 1 to min or the entire segment
  # ifelse(length(x)>=5400,
  #        rangeVal <- max(x[1801:5400])-min(x[1801:5400]),
  #        ifelse(length(x)>=3600,
  #               rangeVal <- max(x[1801:3600])-min(x[1801:3600]),
  #               rangeVal <- max(x)-min(x)
  #        )
  # )
  # deadRows <- which(x == -.9.9)
  rangeVal <- max(DAT[firstRow:lastRow], na.rm=TRUE) - min(DAT[firstRow:lastRow], na.rm=TRUE)
  rangeCoef <- y / rangeVal
  # in case there is a dead sensor with no activity
  ifelse(rangeVal<5,
         DAT <- DAT * 0,
         DAT <- DAT * rangeCoef
  )
  DAT <- DAT - DAT[firstRow]
  {
    xMed <- median(DAT)
    x25th <- quantile(DAT, .25, na.rm=TRUE)
    x75th <- quantile(DAT, .75, na.rm=TRUE)
    x99th <- quantile(DAT, .99, na.rm=TRUE)
    x01th <- quantile(DAT, .01, na.rm=TRUE)
    xIQR <- x75th - x25th
    xLimitLow <- xMed - (1 * xIQR)
    xLimitHigh <- xMed - (1 * xIQR)
    DAT[which(DAT <= x01th)] <- x01th
  }
  return(DAT)
} # end setColRange function


####


NAInterp <- function(x) {
  # function to interpolate NA values in the time series data
  # when down sampling or upsampling
  # called by the sigProc functions for each sensor
  # x input is a vector of time series data
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
    if(is.na(x[i])) { x[i] <- mean(x[i-1],x[min(which(!is.na(x)))]) } 
  } 
  return(x)
} # end NAInterp() function



###################################################################################


# eventNamesFn <- function(x=chartDF) {
# 	# R function to compute the indices for the first and last events in a chart data frame
# 	# x input is a chart DF after signal processing
# 	# output is a named vector of two items: firstEvent, lastEventEnd
# 	###
#   # make a vector of event names
#   eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
#   # get the onset of the first event and the end of the last event
#   if(length(eventNames)==0) {
#     print("no stimulus events. none processed")
#     # next()
#     firstEvent=1
#     lastEventEnd=nrow(chartDF)
#   } else {
#     # make a vector of event onset rows
#     eventRows <- which(chartDF$eventLabel!="")
#     # get the first event for scaling and centering
#     firstEvent <- eventRows[!(eventNames %in% excludeEvents)][1]
#     # fix missing firstEvent
#     if(is.na(firstEvent)) { firstEvent <- 1 }
#     # get the last event for scaling and centering
#     lastEvent <- eventRows[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
#     # fix missing lastEvent
#     if(length(lastEvent)==0) { lastEvent <- nrow(chartDF) }
#     # changed 10-1-2016 because some examiners rush to end the chart recording 
#     # and others present more stimuli after the XX end announcement
#     # lastEventEnd <- lastEvent
#     # changed back 11-1-2016
#     lastEventEnd <- lastEvent + measuredSeg * cps
#     if(length(lastEventEnd)==0) { lastEventEnd <- nrow(chartDF) }
#     # fix potential problem when lastEventEnd exceeds the data frame rows for the chart
#     if(lastEventEnd > nrow(chartDF)) { lastEventEnd <- nrow(chartDF) }
#   }
#   outVector <- c(firstEvent, lastEventEnd)
#   names(outVector) <- c("firstEvent", "lastEventEnd")
#   return(outVector)
# } # end eventNamesFn()



####


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



####



colSDs <- function(x=CQSensorMeansDF[2:ncol(CQSensorMeansDF)],pop=FALSE) {
  # R function to calculate the standard deviation for data frame cols
  # x input is a data frame
  # pop selects either sample or population SD
  ###
  if(isFALSE(pop)) {
    return(apply(x, 2, sd))
  } else {
    sdp <- function(x) { (sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x))) }
    return(apply(x, 2, sdp))
  }
}



####



cardioLeakFn <- function(x=chartDF$c_CardioMA, midVal=0) {
  # R function to to check for leaking or descending cardio sensor data
  # x input is the slow moving cardio data mean
  # midVal input is the middle value of the y axis display
  # output is a message
  ###
  xLen <- length(x)
  # exit if less than 20 seconds
  if(xLen < 600) return
  # set the selection ranges for the first and second halfs
  start1 <- round(xLen * .1)
  end1 <- round(xLen * .35)
  start2 <- round(xLen * .65)
  end2 <- round(xLen * .9)
  # get the means
  mean1 <- mean(x[start1:end1])
  mean2 <- mean(x[start2:end2])
  # a fault is indicated 
  # if the first half is > midVal and second half is < midVal
  if(mean1 > midVal && mean2 < midVal) {
    cardioLeakMsg <- "POSSIBLE CARDIO SENSOR PROBLEM"
  } else {
    cardioLeakMsg <- "none"
  }
  return(cardioLeakMsg)
}



####



SRQ2001_cardioSquareWaveFn <- function(x=chartDF$c_Cardio1, segLen=15) {
  # R function to reduce the cardio data to a square wave 
  # using the procedure described by Stern, Ray & Quigley (2001)
  ###
  dataLen <- length(x)
  DAT <- rep(NA, times=dataLen)
  # calculate the number of .5sec segments using integer division
  nSegs <- length(x) %/% segLen 
  segIdcs <- c(1:nSegs) * segLen
  # get the modulus
  modLen <- length(x) %% segLen
  if(modLen > 0) {
    nSegs <- nSegs+1
    segIdcs <- c(segIdcs, dataLen)
  }
  for(i in 1:length(segIdcs)) {
    # locate the end row for the .5 sec segment
    thisIdx <- segIdcs[i]
    prevIdx <- segIdcs[(i-1)]
    if(length(prevIdx) == 0) prevIdx <- 0
    theseRows <- c((prevIdx+1):thisIdx)
    theseRows <- theseRows[theseRows <= dataLen]
    meanVal <- mean(x[theseRows])
    DAT[theseRows] <- meanVal
  }  
  # output the sqare wave
  return(DAT)
}



cardioSRQFn <- function(x=chartDF$c_Cardio1, segLen=15) {
  # R function to reduce the cardio data to a square wave 
  # using the procedure described by Stern, Ray & Quigley (2001)
  ###
  dataLen <- length(x)
  DAT <- rep(NA, times=dataLen)
  for(i in 1:(length(DAT)-(segLen-1))) {
    theseRows <- c(i:(i+(segLen-1)))
    meanVal <- mean(x[theseRows])
    DAT[theseRows] <- meanVal
  }
  # output the sqare wave
  return(DAT)
}





