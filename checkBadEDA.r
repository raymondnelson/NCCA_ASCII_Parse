checkBadEDADataFn <- function(x=chartDF$c_AutoEDA, 
															sec=6, 
															times=20, 
															ignore=2, 
															xRange=(.1*yRange), 
															maxY=(yMax-.05*yRange), 
															minY=(yMin+.05*yRange),  
															firstRow=firstEvent, 
															lastRow=lastEventEnd) {
  # helper function to check for excessive high frequency noise in the EDA data
  # 4-17-2016
  # Raymond Nelson
  # this is called by the ScaleOffsetDataFn() function
  ### input
  # x input is the time series vector for a single for one sensor from a single chart
  # sec is the number of seconds to sample
  # times is the number of random samples to get
  # ignore is an integer that indicates the number of large samples to ignore
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  
  
  # xRange is the range to scale the data within
  # maxY is the max y value for the data on the printed chart
  # minY is the min y value for the data on the printed chart
  
  
  ### output
  # output is a the rescaled time series vector vector
  # also assigns a scalar scaleVal to the global environment,
  # for use when scaling the additional channels
  ##########
  dataLength <- length(x)
  # first set the number of indices to include in each sample
  sampleLength <- sec * cps 
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength-sampleLength + 1
  # calculate the scaled data
  if(dataLength <= (sec * 2 * cps)) {
    # handle short charts less than 2 seconds
    if(max(x) == min(x)) {
      dataRange <- 1
    } else {
      dataRange <- max(x) - min(x)
    } # end else
    scaleVal <- xRange / dataRange
    # initialize the default rescale value for this if condition
    rescaleVal <- 1
    xOut <- x * scaleVal
  } else {
    # for charts > 2 seconds
    # sample the data
    # get the sample onset row indices using random numbers
    # use an if else condition for short charts
    if(firstRow < (lastRow-sampleLength)) {
      # get the sample onset indices
      sampleOnset <- sample(c(firstRow:(lastRow-sampleLength)), size=times, replace=TRUE)
      # then get the sample offset indices
      sampleOffset <- sampleOnset + sampleLength - 1
      # make a data frame for the sample segments
      DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
      # each row is a sample from the time series input 
      # populate the data frame
      for (i in 1:times) {
        DF[i,] <- x[sampleOnset[i]:sampleOffset[i]]
      }
      # View(DF)
      # make a function to get the range from the DF rows
      dfRangeFn <- function(x) { max(x)-min(x) }
      # apply the range function to each row of the DF to get the range of each row
      # apply is vectorized and needs no loop
      dfRange <- apply(DF, 1, dfRangeFn)
      # sort and remove the largest changes using the ignore parameter 
      dfRange <- sort(dfRange, decreasing = TRUE)[(ignore+1):length(dfRange)]
      # get the scaling value from the dfRange
      # summary(dfRange)
      if(median(dfRange)!=0) {
        scaleVal <- xRange/median(dfRange)
      } else {
        scaleVal <- 1
      } # end else
    } else { 
      # another condition for short charts
      if(max(x[firstRow:lastRow]) != min(x[firstRow:lastRow])) {
        scaleVal <- xRange / abs( max(x[firstRow:lastRow]) - min(x[firstRow:lastRow]) )
      } else scaleVal <- 1
    } # end else
    # scale the input vector
    xOut <- x * scaleVal
    # get the offset max value and offset the data if necessary
    offsetVal <- maxY - max(xOut[firstRow:lastRow])
    # 12-31-2016 change this to + offsetVal
    if(offsetVal < 0) { xOut <- xOut + offsetVal }
    # get the offset min value and offset the data if necessary
    offsetVal <- minY - min(xOut[firstRow:lastRow])
    if(offsetVal > 0) { xOut <- xOut + offsetVal }
    # check the range 
    newRange <- max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow])
    # initialize the default rescale value for this else condition
    rescaleVal <- 1
    # rescale the data if the range exceeds the maxY and minY values
    if(newRange > (maxY - (minY + (.25 * yRange)))) { 
      rescaleVal <- (maxY - (minY + (.25 * yRange))) / newRange
      xOut <- xOut * rescaleVal
    } # end if
  } # end else
  # assign the scale value to the global environment
  # the scale value needs to be used to scale additional channels for each sensor
  assign("scaleVal", (scaleVal * rescaleVal), pos=1)
  return(xOut)
} # end checkBadEDADataFn()  

