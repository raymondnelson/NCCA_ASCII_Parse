# function to scale the time series data
# 4-17-2016
# Raymond Nelson
# started and not completed



scaleDataFn <- function(x=chartDF$c_UPneumo, sec=6, times=20, xRange=35, yCenter=135, maxY=165, minY=-165) {
  # function to scale the time series data 
  # 4-17-2016
  # Raymond Nelson
  # x input is the time series vector
  # sec is the number of seconds to sample
  # times is the number of random samples to get
  # xRange is the range to scale the data within
  # offset is the position of the sensor data on the printed chart
  # max is the max y value for the data on the printed chart
  # min is the min y value for the data on the printed chart
  # output is a named vector of two numberial values: scaleVal, offsetVal
  #################################
  dataLength <- length(x)
  # handle shart charts
  if(dataLength <= (sec*2*cps)){
    dataRange <- max(x)-min(x)
    scaleVal <- xRange/dataRange
    xOut <- x * scaleVal
  } else {
    # set the number of indices to include in each sample
    sampleLength <- sec * cps 
    # ignore the first and last segments of sampling seconds
    useStart <- sampleLength+1
    useLength <- length(x)-sampleLength
    # x <- x[useStart:useLength]
    # get the sample onset indices
    sampleOnset <- sample(useStart:useLength, times, FALSE)
    sampleOffset <- sampleOnset + sampleLength - 1
    # make a data frame for the sample segments
    # each row is a sample from the time series input 
    DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
    # set the sampe data
    for (i in 1:times) {
      DF[i,] <- x[sampleOnset[i]:sampleOffset[i]]
    }
    # make a function to get the range from the DF rows
    dfRangeFn <- function(x) { max(x)-min(x) }
    # get the range for each row of the DF
    dfRange <- apply(DF, 1, dfRangeFn)
    # get the scaling value
    scaleVal <- xRange/median(dfRange)
    # scale the input vector
    xOut <- x * scaleVal
    # get the offset max value and offset the data if necessary
    offsetVal <- maxY - max(xOut[useStart:useLength])
    if(offsetVal < 0) xOut <- xOut - offsetVal
    # get the offset min value and offset the data if necessary
    offsetVal <- minY - min(xOut[useStart:useLength])
    if(offsetVal > 0) xOut <- xOut + offsetVal
    # check the range and rescale the data if necessary
    newRange <- max(xOut[useStart:useLength]) - min(xOut[useStart:useLength])
    if(newRange > (maxY - minY)) {
      rescaleVal <- (maxY - minY) / newRange
      xOut <- xOut * rescaleVal
    }
  } # end else
  return(xOut)
} # end scaleDataFn() function 




