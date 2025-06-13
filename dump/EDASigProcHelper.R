# EDA signal processing helper functions
# 
# MASmooth()
# minPeak()
# maxPeak()
# interpolatePeaks()
#
#


# function to smooth the EDA data to the midpoint

MASmooth <- function(x=myData, y=round(7.5*cps,0), times=10) {
  # private function to calculate a smoothed average of the time series data
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to recursively smooth the data
  xOut <- c(rep(x[1], times=y), numeric(length=(length(x)-y)))
  # buffer will be double the offset value + 1
  input_buffer <- c(x[1:(y+1)], rep(x[y+1], times=y)) 
  # loop over the number of times
  for (j in 1:times) {
    # for loop to compute the moving average
    for (i in (y+1):(length(x)-(y))) { # starts 1 sample after y
      xOut[i] <- mean(input_buffer) # fills the middle value of the buffer with the mean
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y])
    } # end for loop for moving average
    # re-initialize the input
    x <- xOut
    input_buffer <- c(x[1:(y+1)], rep(x[y+1], times=y))
  } # end loop over times
  xOut[(i-y):length(xOut)] <- mean(input_buffer) # 10-6-15 fill the end of the vector
  return(na.omit(xOut))
} # end MASmooth function()

minPeak <- function(x, y=round(1.333*cps,0)) {
  # private function to get the response peaks from the pneumo data
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
} # end minPeak function()

maxPeak <- function(x, y=round(1.333*cps,0)) {
  # function to get the response peaks from the EDA data
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
}  # end maxPeak()

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
  peakValDiff2 <- peakValDiff / peakDivisor
  peakFill <- rep(peakValDiff2, times=peakOutDiff)
  peakFill2 <- cumsum(peakFill)
  # peakFill2[x] <- y # to preserve the exact peak values for verification of the interpolation
  return(c(y[1], peakFill2))
} # end interpolatePeaks()





# 
# g <- ggplot()
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myPLE$c_PL), color="blue", size=.75) + coord_cartesian(ylim=c(150, -150))
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myMinInterp))
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myMaxInterp))
# print(g)
# 
# 
