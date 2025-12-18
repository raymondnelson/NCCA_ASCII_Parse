# R functions to compute the cardio rate
# November 21, 2025
# Raymond Nelson
####
#
# improved from earlier versions of maxPeak() in the sigProchelper.R script
# 
#
####



systolicPeakFn <- function(x=chartDF$c_Cardio1, y=5) {
  # function to get the systolic peaks from the cardio time series data
  # November 21, 2025
  # Raymond Nelson
  ###
  # will keep the index number of max peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # 
  # this function is called by the cardioRatePerMinFn()
  ###
  # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
  xOut <- rep(NA, times=(length(x)))
  # keep the first and last sample as a peak
  xOut[1] <- 1 # keep the first
  xOut[length(xOut)] <- length(xOut) # keep the last
  # initialize an input buffer for the loop
  # buffer will be double the y value
  input_buffer <- x[1:(2*y)]
  # iterate on the cardio time series data
  i=1
  for (i in 1:(length(x)-(2*y))) {
    # advance the input buffer, dropping the first value, and adding the next
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    # check to see if the middle value of the buffer is the max
    ifelse(input_buffer[(y+1)]==max(input_buffer),
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           next()
    )
  } # end for loop
  xOut <- as.numeric(na.omit(xOut))
  return(xOut)
}  # end systolicPeakFn




cardioRatePerMinFn <- function(x=chartDF$c_Cardio1) {
  # function to calculate the cyclic rate per minute for cardio and vasomotor data
  # for cardio and pneumo time series data
  # x input is a a time series vector
  # this function will call the systolicPeakFn()
  ####
  # set a buffer to help ignore peaks associated with the dichrotic notch
  buffer <- 5
  # then call the systolicPeakFn() to get the systolic peak indices 
  Peaks <- systolicPeakFn(x=x)
  # compute the number of samples between systolic peams, omitting the first and last
  peakDiffs <- diff(Peaks[c(2:(length(Peaks)-1))])
  # remove peak less than the buffer length because these occur when the cardio cuff is deflated
  # peak diffs of < buffer length will distort the cardio rate measurement
  peakDiffs <- peakDiffs[which(peakDiffs > buffer)]
  # calculate the rate
  rateMin <- ifelse(length(peakDiffs)>1,
                    60 / (mean(peakDiffs) / cps ),
                    60/peakDiffs/cps
  )
  rateMin <- round(rateMin, 2)
  return(rateMin)
} # end cardioRatePerMinFn() function


# cardioRatePerMinFn(x=chartDF$c_Cardio1)


