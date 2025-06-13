# 10-27-2015 Raymond Nelson
# 10-29-2015 fixed NA problem

robustPneumoMeasurement <- function(dataVector, verbalAnswer , cps=30) {
  # function to compute a pneumo excursion measurement
  # that is robust against common answer distortion artifacts
  # and can also be made more robust against 
  # other identified respiration artifacts
  # 
  # Respiratory excursion is the sum of 
  # the absolute difference of all successive respiration samples.
  #
  # The robust excursion measurement is achieved by removing 
  # the data from 1 second before to 1 second after 
  # the point of verbal answer, and can be made more robust by 
  # removing other identified artifact segments 
  # from the input data vector.
  # 
  # A dimmensionally stable measurement can be obtained by 
  # first summing the excursion lengths not for the entire segment,
  # but for a 1 second moving average across the non-artifacted samples,
  # and then averaging the excursion sum by the number of 
  # 1 second segments of non-artifacted sample differences.
  # 
  # input dataVector is the time series data for 15 seconds
  # input verbalAnswer is the sample index, in the dataVector,
  # where the verbal answer is located
  # input cps is the data rate (30 samples per second x 15 = 150 samples)
  # 
  # ouput RExcursion is the mean absolute differences for 
  # successive samples for a 1 second moving average of all
  # non-artifacted respiration samples in the input data vector
  #
  ##############################
  #
  # use the maxPeak function check to see if the respiration rate is in the normal range
  if(round(60 / (mean(diff(maxPeak(dataVector, y=40))) / 30), 2) < 12) { 
    RExcursion <- "ONR"
  }
  if(round(60 / (mean(diff(maxPeak(dataVector, y=40))) / 30), 2) > 20) { 
    RExcursion <- "ONR"
    # "ONR" signifies "outside the normal range"
  }
  
  # compute the absolute difference in respiratory excursion
  REVector1 <- abs(diff(dataVector)) # changed this 10-29-2015
  
  # set the buffer around the verbal answer 
  ansBufferOn <- verbalAnswer - (1 * cps) 
  ansBufferOff <- verbalAnswer + (1 * cps) + 1
  REVector1[ansBufferOn:(verbalAnswer-1)] <- 0 # changed this 10-29-2015
  # use of 0 during the 1 sec prior to verbal answer will prevent NAs until 1 second prior 
  REVector1[verbalAnswer:ansBufferOff] <- NA # changed this 10-29-2015
  
  # then compute the 1 second moving sum of absolute difference
  # if the RExcursion vector is not already "ONR"
  REVector2 <- NULL
  for (i in 1:(length(REVector1)-cps+1)) {
    REVector2 <- c( REVector2, sum( REVector[ i:(i+cps-1) ] ) )
    # square brackets are used in R for indexing a vector
    # white space in R is not meaningful
    # except to improve readability within an expression
    # unterminated expressions are simply continued to the next line 
    #
    # any sum that includes an NA input value will be NA
    # this will ensure that artifacted segments are not included
  } # end for loop
  
  # remove NA sums
  REVector3 <- na.omit(REVector2) 
  # could be done without defining a new vector
  
  # and the mean of REVector3 
  REVector4 <- REVector3 / length(REVector3)
  
  # output
  RExcurtion <- ifelse(RExcursion != "ONR",
                       REVector4,
                       RExcursion
                       ) # end iflse
  
  return(RExcursion)
} # end robustPneumoMeasurement function

#############################

maxPeak <- function(x, y=40) {
  # helper function to get the positive slope peaks from the time series data
  # will keep the index number of max peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
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
    ) # end if
  } # end for loop
  return(as.numeric(na.omit(xOut)))
}  # end maxPeak function
  