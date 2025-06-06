# R function for Detrended-2 EDA filter
# 8-17-2012
# Raymond Nelson
#
####


# edaData <- chartDF$EDA1
# x <- edaData

detrended2EDAFilter <- function(x=edaData, cps=30, accel=1.05, wait=5) {
  # R function for Detrended-2 EDA filter
  # 8-17-2012
  # Raymond Nelson
  #
  ####
  #
  # the Detrended-2 EDA filter operation is simple
  # 1) calculate the slope direction or all successive samples in the input vector
  # 2) calculate the duration in seconds for all runs of negative slope samples
  # 3) calculate the difference for all successive samples
  # 4) first sample is the origin at 0
  # 5) sum the adjusted differences using the following conditions
  # 5a) for each sample
  # 5b) if the slope is positive add the difference to the sum starting at origin
  # 5c) else if the slope is negative check if the sum is 0 or < than the origin
  # 5d) if the sum is zero then add zero to the sum to keep the data at baseline
  # 5e) else if the sum is > 0 then check the duration of the negative slope
  # 5f) if the duration of descent is >= 5 sec then add the diff x 1.05 
  # 5i) else if the duration is < 5 sec then add the diff to the sum
  # 5g) check the cumulative sum 
  # 5h) if cumulative sum is < 0 subtract the sum from 0 so that the sum is 0
  #
  # input x is the vector of Manual EDA samples
  # input cps is the number of samples per second
  # input accel is the acceleration multiplier for descending data ofter 5 sec
  # input wait is the number of sec to wait before accelerating the data to origin
  # 
  # output is the Detrended EDA data
  # 
  # requirements for the Detrended EDA are that ascending + slope segments 
  # are visually and measurably identical to the + slope segments for Manual EDA
  # and the correlation coef and coef of determination for Det EDA will be
  # identical to the Manual EDA
  # numerical scores will be identical for Manual EDA and Detrended EDA
  #
  ####
  
  # private function to determine the slope
  slopeDir <- function(x=x) {
    # helper function to determine the positive or negative slope direction of a time series vector
    # input x is a vector of time series measurements
    # ouput x1 is a vector of slope values for all values in the input vector
    x1 <- x
    diff1 <- diff(x)
    x1 <- ifelse(diff1==0,
                 x1 <- 0,
                 # ifelse is vectorized and does not require a control loop
                 ifelse(diff1>0,
                        x1 <- 1,
                        x1 <- -1) )
    return(c(x1, 0))
    
  } # end slopeDir function
  
  # calculate the duration in seconds for all descending runs
  xSlope <- slopeDir(x=x)
  
  # private function to calculate the length of descending runs
  descSec <- function(x=xSlope, cps=cps) {
    # helper function to calculate the length in seconds for descending runs
    x1 <- x
    # x1[which(x1==1)] <- 0
    sampleTime <- 1/cps
    # check the first sample
    x1[1] <- ifelse(x[1] == -1,
                    sampleTime,
                    0
    )
    # iterate over the input vector
    # i=2
    for(i in 2:length(x)) {
      # check the x vector not x1 because x1 values are replaced here
      x1[i] <- ifelse(x[i] == -1,
                      x1[(i-1)] + sampleTime,
                      0
      )
    }
    return(x1)
  } # end descSec() function
  
  # calculate the length of all descending runs in seconds
  descTime <- descSec(x=xSlope, cps=cps)
  
  ###
  
  # initialize the output vector
  y <- rep(0, times=length(x))
  
  # calculate the difference for each sample in the input vector
  xDiff <- c(0, diff(x))
  
  # # center the data at the origin
  # originValue <- x[1]
  # x2 <- x - originValue
  
  # i=2
  for(i in 2:length(x)) {
    y[i] <- ifelse(xSlope[i] == 1,
                   y[(i-1)] + xDiff[i],
                   ifelse(y[(i-1)] == 0,
                          0,
                          ifelse(descTime[i] >= 5,
                                 # this accel is to bring the data down
                                 # if the data stay 0 slope
                                 y[(i-1)] + xDiff[i] * accel,
                                 y[(i-1)] + xDiff[i]
                          )
                   )
    )
    # fix y[i] if it is less than zero
    if(y[i] < 0) y[i] <- 0
  }
  
  # set the output vector to the originValue
  y <- y + x[1]
  
  return(y)
  
} # end detrended2EDAFilter() function




# outData <- detrended2EDAFilter(x=edaData, cps=30, accel=1.05, wait=5)
# plot.ts(outData)


