# EDA signal processing helper functions
# 
# MASmooth()
# 
# # minPeak
# maxPeak
# interpolatePeaks
#
#


# function to smooth the EDA data to the midpoint

MASmooth <- function(x=myData, y=7.5*cps, times=10) {
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
} # end MASmooth function


tukeyFence1 <- function(x=myPLE$c_PL, y=myMaxPeaks, z=5) {
  # function to exclude minPeak and maxPeak points that do not exceed the inner Tukey Fence
  # x is the time series data
  # y is the minPeak or maxPeak vector - or a minMaxPeak vector
  # z is the measurement window in seconds
  n <- z * cps
  xOut <- NULL
  for (i in 1:length(y[y<length(x)])) {
    # mananage the end of the time series and the inputBuffer
    if((y[i]) > (length(x)-n)) {
      xOut <- c(xOut, y[i])
      next 
    }
    # load the inputBuffer
    inputBuffer <- x[(y[i]):(y[i]+n-1)]
    # set the upper and lower fences
    tukeyUpper <- median(inputBuffer)+(1.5*IQR(inputBuffer))
    tukeyLower <- median(inputBuffer)-(1.5*IQR(inputBuffer))
    # evalutate the min and max peak
    if(x[y[i]] <= tukeyLower) { xOut <- c(xOut, y[i]) }
    if(x[y[i]] >= tukeyUpper) { xOut <- c(xOut, y[i]) }
  } # end for loop
  xOut <- c(1, xOut, length(myPLE$c_PL))
  return(xOut)
} # end tukeyFence1 function

minPeak <- function(x, y=40) {
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
} # end minPeak function

maxPeak <- function(x, y=40) {
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
  peakValDiff2 <- peakValDiff / peakDivisor
  peakFill <- rep(peakValDiff2, times=peakOutDiff)
  peakFill2 <- cumsum(peakFill)
  # peakFill2[x] <- y # to preserve the exact peak values for verification of the interpolation
  return(c(y[1], peakFill2))
} # end interpolatePeaks









# myPLE <- as.data.frame(chartDF$c_PL[1:750])
# names(myPLE) <- "c_PL"
# myMinPeaks <- minPeak(myPLE[,1])
# myMaxPeaks <- maxPeak(myPLE[,1])
# myMinPeakVals <- myPLE$c_PL[myMinPeaks]
# myMaxPeakVals <- myPLE$c_PL[myMaxPeaks]
# 
# keepMin <- tukeyFence1(x=myPLE$c_PL, y=myMinPeaks, z=5)
# keepMax <- tukeyFence1(x=myPLE$c_PL, y=myMaxPeaks, z=5)
# keepMinVals <- myPLE[keepMin,1]
# keepMaxVals <- myPLE[keepMax,1]
# # keepMinVals <- keepMinVals - myPLE[1,1]
# myMinInterp <- interpolatePeaks(x=keepMin, y=keepMinVals)
# myMaxInterp <- interpolatePeaks(x=keepMax, y=keepMaxVals)
# # myMinInterp <- myMinInterp + myPLE[1]
# 
# g <- ggplot()
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myPLE$c_PL), color="blue", size=.75) + coord_cartesian(ylim=c(150, -150))
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myMinInterp))
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myMaxInterp))
# print(g)
# 
# 
