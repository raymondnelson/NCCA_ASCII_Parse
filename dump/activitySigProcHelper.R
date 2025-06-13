### some helper functions for activity sensor signal processing

# maxPeak
# minPeak
# interpolatePeaks
# MASmooth



minPeak <- function(x, y=40) {
  # function to get the diastolic peaks from the cardio data
  # will keep the index number of min peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  xOut <- rep(NA, times=(length(x)))
  xOut[1] <- 1 # keep the first sample
  xOut[length(xOut)] <- length(xOut) # keep the last sample
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



maxPeak <- function(x, y=40) {
  # function to get the systolic peaks from the cardio data
  # will keep the index number of max peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  xOut <- rep(NA, times=(length(x)))
  xOut[1] <- 1 # keep the first
  xOut[length(xOut)] <- length(xOut) # keep the last
  # buffer will be double the offset value
  input_buffer <- x[2:(2*y+1)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    # check to see if the middle value of the buffer is the max
    ifelse(input_buffer[(y+1)]==max(input_buffer),
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           next()
    )
  } # end for loop
  return(as.numeric(na.omit(xOut)))
}  # end maxPeak



interpolatePeaks <- function(x=minIndices, y=minVals) {
  # helper function to interpolate between peak segments of time series input data
  # x is a vector of peak row numbers in the data frame
  # y is a vector of peak row values in the data frame
  peakOutDiff <- diff(x)
  peakValDiff <- diff(y)
  if (length(peakValDiff) < length(peakOutDiff)) {
    peakValDiff <- c(peakValDiff, rep(peakValDiff[length(peakValDiff)], times=(length(peakOutDiff)-length(peakValDiff))))
  }
  peakDivisor <- peakOutDiff
  # peakDivisor[which(peakOutDiff>0)] <- (peakOutDiff[which(peakOutDiff>0)])
  peakValDiff2 <- peakValDiff / peakDivisor
  peakFill <- rep(peakValDiff2, times=peakOutDiff)
  peakFill2 <- cumsum(peakFill)
  # peakFill2[x] <- y # to preserve the exact peak values for verification of the interpolation
  return(c(y[1], peakFill2))
} # end interpolatePeaks



MASmooth <- function(x=myData, y=75, times=4) {
  # private function to calculate a smoothed average of the time series data
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to recursively smooth the data
  # make the output vector 
  # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
  xOut <- c(rep(mean(x[1:(2*y)]), times=y), rep(mean(x[(length(x)-(2*y)):length(x)]), times=(length(x)-y)))
  # xOut <- c(rep(mean(x[1:(2*y)]), times=y), numeric(length=(length(x)-y)))
  # buffer will be double the offset value + 1
  input_buffer <- c(x[1:(y+1)], rep(x[y+1], times=y)) 
  # loop over the number of times to repeat the smoothing
  for (j in 1:times) {
    # for loop to compute the moving average
    for (i in (y+1):(length(x)-(y)+1)) { # starts 1 sample after y
      bufferMean <- mean(input_buffer)
      xOut[i] <- bufferMean # fills the middle value of the buffer with the mean
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y])
    } # end for loop for moving average
    # re-initialize the input
    x <- xOut
    input_buffer <- c(x[1:(y+1)], rep(x[y+1], times=y))
  } # end loop over times
  # xOut[(i-y):length(xOut)] <- mean(input_buffer) # 10-6-15 fill the end of the vector
  xOut[(length(x)-(y)+1):length(x)] <- xOut[(length(x)-(y)+1)]
  return(na.omit(xOut))
} # end MASmooth function



minMaxPeakFn <- function(x=chartDF$c_Cardio1, y=7) {
  # private function to get the diastolic and systolic peaks from the cardio data
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



fixPeak <- function(x=chartDF$c_SE, times=2) {
  # function to fix max and min peaks that are repeated 2 or more times
  # input x is the time series cardio data
  # times is the number of times to repeat the operaation
  # this function will keep the first of all adjacent max and min peaks
  # and interpolate the subsequent equal peak with the next sample
  for (j in times) {
    for (i in 4:(length(x)-1)) {
      if(x[i]==x[(i-1)]) { x[i] <- mean(c(x[(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-2)]) { x[i] <- mean(c(x[(i-2):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-3)]) { x[i] <- mean(c(x[(i-3):(i-1)], x[(i+1)])) }
    }
  }
  return(x)
} # end fixPeak() function
# aOut <- fixPeak(x=chartDF$c_SE, times=2)
# diff(maxPeak(x=chartDF$c_SE, y=7))
# length(diff(maxPeak(x=chartDF$c_SE, y=7)))
# length(which(diff(maxPeak(x=chartDF$c_SE, y=7))==1))
# diff(maxPeak(x=cOut, y=7))
# length(diff(maxPeak(x=cOut, y=7)))

