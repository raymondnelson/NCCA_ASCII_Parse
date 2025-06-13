# helper functions for signal processing and feature extraction
# 8-23-2105
#
# these functions are called by the signal processing script
#
# #####
# sampleSpace() function to calculate the sample space for a pulse rate
# 
# pulseRate() function to calculate the pulse rate for a sample space
#
# minPeak() to get the min peak row and value from an occilating time series
#
# maxPeak() to get the max peoak
# 
# interpolatePeaks() to interpolate the space between min peaks or max peaks
#
# minMaxPeak() to get both the min and max peak values 
# 
# cardioSmooth1() to reduce the cardio to the slow wave form
#
# minMaxMean() another method to reduce the diastolic and systolic data to a single line
#
# tukeyFence1() function to exclude minPeak and maxPeak points that do not exceed the inner Tukey Fence
#
###############################################




library(stringr)





#####



sampleSpace <- function(x=cps, y=17) {
  # function to calculate the sample space for a pulse rate
  # x is the sampling rate in seconds
  # y is the pulse rate per minute
  # output is the number of samples
  secsPerCycle <- 60 / y
  samplesPerCycle <- secsPerCycle * x
  return(samplesPerCycle)
  
}
# sampleSpace(x=cps, y=17)



pulseRate <- function(x=cps, y=30) {
  # function to calculate the pulse rate for a sample space
  # x is the sampling rate in seconds
  # y is the number of samples per cycle
  secsPerSample <- x / y 
  pulseRate <- secsPerSample * 60
  return(pulseRate)
}
# pulseRate(x=cps, y=32)



######

fixPeak <- function(x=chartDF$c_Cardio1, times=3) {
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

# cOut <- fixPeak(x=chartDF$c_Cardio1, times=2)
# diff(maxPeak(x=chartDF$c_Cardio1, y=7))
# length(diff(maxPeak(x=chartDF$c_Cardio1, y=7)))
# length(which(diff(maxPeak(x=chartDF$c_Cardio1, y=7))==1))
# diff(maxPeak(x=cOut, y=7))
# length(diff(maxPeak(x=cOut, y=7)))



#####

  # use y=12 for slow pulse rate below 60
  # round(.4*cps,0)

minPeak <- function(x, y=round(.25*cps,0)) {
  # private function to get the diastolic peaks from the cardio data
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

#####

maxPeak <- function(x, y=round(.25*cps,0)) {
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
# maxPeak(x=chartDF$c_Cardio1, y=7)
# diff(maxPeak(x=chartDF$c_Cardio1, y=7))

#####


minMaxPeakFn <- function(x=chartDF$c_Cardio1, y=round(.25*cps,0)) {
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

##### 


interpolatePeaks <- function(x=na.omit(minOut), y=na.omit(minVal)) {
  # private function to interpolate between peak segments of time series input data
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
# diastolicInterp <- c(interpolatePeaks(x=minOut, y=minVal),0 )
# plot.ts(diastolicInterp, ylim=c(-3,10))


#####


cardioSmooth1 <- function(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=4) {
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
} # end cardioSmooth1 function

# smoothedCardio <- cardioSmooth1(x=cardioSmooth1(x=cardioSmooth1()))
# smoothedCardio <- cardioSmooth1(x=chartDF$Cardio1, y=15, times = 4)
# plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))

#####

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
  } # end minMaxMean() function
  
  xOut <- na.omit(xOut)
  xOut <- c(xOut, rep(xOut[length(xOut)], times=(y-1)))
  return(xOut) 
} # end minMaxMean function


######



# tukeyFence1 <- function(x, y="min", z=5, inner=1.5) {
#   # function to exclude minPeak and maxPeak points that do not exceed the inner Tukey Fence
#   # x is the time series data
#   # y is the minPeak or maxPeak vector - or a minMaxPeak vector
#   # z is the measurement window in seconds
#   # output is a vector of peak indexes including first and last
#   ###
#   # set the length of the buffer
#   n <- z * cps
#   # 
#   ifelse(y=="max", peakVector <- maxPeak(x),
#                        ifelse(y=="both", peakVector <- minMaxPeak(x),
#                               peakVector <- minPeak(x) ) )
#   minMax <- minMaxPeak(x) # need this for the tukey fence
#   # inputBuffer <- x[1:(n-1)] # not needed 10-7-15
#   xOut <- NULL
#   for (i in 1:length(peakVector[peakVector<length(x)])) { 
#     # mananage the end of the time series and the inputBuffer
#     if((peakVector[i]) > (length(x)-2*n)) {
#       xOut <- c(xOut, peakVector[i])
#       next 
#     }
#     # initialize the inputBuffer
#     # inputBuffer <- x[(peakVector[i]):(peakVector[i]+n-1)]
#     inputBuffer <- x[peakVector[i:(i+2)]]
#     # set the upper and lower fences
#     mySummary <- summary(inputBuffer) # to get the quartiles
#     # set the tukey fences
#     tukeyUpper <- median(inputBuffer)+(inner*IQR(inputBuffer))
#     tukeyLower <- median(inputBuffer)-(inner*IQR(inputBuffer))
#     # evalutate the min and max peak
#     if(x[peakVector[i]] <= tukeyLower) { xOut <- c(xOut, peakVector[i]) }
#     if(x[peakVector[i]] <= min(inputBuffer)) { xOut <- c(xOut, peakVector[i]) } 
#     if(x[peakVector[i]] >= tukeyUpper) { xOut <- c(xOut, peakVector[i]) }
#     # if(x[peakVector[i]] >= max(inputBuffer)) { xOut <- c(xOut, peakVector[i]) }
#   } # end for loop
#   xOut <- c(1, xOut, length(x))
#   return(xOut)
# } # end tukeyFence1 function
# 
# 
# 
# # get the time series data into a data frame for plotting
# myPLE <- as.data.frame(chartDF$c_PL[1:600])
# names(myPLE) <- "c_PL"
# 
# myMinPeaks <- minPeak(myPLE[,1])
# myMaxPeaks <- maxPeak(myPLE[,1])
# # myMinPeakVals <- myPLE$c_PL[myMinPeaks]
# # myMaxPeakVals <- myPLE$c_PL[myMaxPeaks]
# 
# keepMin <- tukeyFence1(x=myPLE$c_PL, y="min")
# keepMax <- tukeyFence1(x=myPLE$c_PL, y="min")
# 
# keepMinVals <- myPLE[keepMin,1]
# keepMaxVals <- myPLE[keepMax,1]
# 
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

cardioMinMax <- function(x=chartDF$c_Cardio1, y=round(.8*cps,0)) {
  #  function to find and keep the local the local min and max within a moving window
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


