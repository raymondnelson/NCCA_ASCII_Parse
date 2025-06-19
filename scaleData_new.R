# new function to scale and offset data 
# Oct 25,  2020
# 



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
  ###################################
  if(!exists("firstRow")) firstRow <- 1
  if(!exists("lastRow")) lastRow <- length(x)
  
  set.seed(1234567890)
  
  dataLength <- length(x)
  # exit if the vector is NA
  if(length(which(is.na(x)))==dataLength) return(x)
  # determine the number of items to include in each sample
  sampleLength <- sec * cps 
  # check the firstRow and lastRow
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength-sampleLength + 1
  # calculate the scaled data
  if(dataLength <= (sec * 2 * cps)) {
    # for short charts less than 2 seconds
    if(max(x) == min(x)) {
      # for flatline data
      dataRange <- 1
    } else {
      # use the entire vector
      dataRange <- max(x) - min(x)
    } # end else
    scaleVal <- yRange / dataRange
    # scale the output vector
    xOut <- x * scaleVal
    # initialize the default rescale value for this if condition
    rescaleVal <- 1
  } else {
    # for charts > 2 seconds
    # sample the data
    # get the sample onset row indices using random numbers
    # use an if condition
    if(firstRow < (lastRow-sampleLength)) {
      # for most charts - get the sample onset indices
      # check the length and times
      if(length(c(firstRow:(lastRow-sampleLength))) <= times) {
         times <- length(c(firstRow:(lastRow-sampleLength))) 
      }
      sampleOnset <- sample(c(firstRow:(lastRow-sampleLength)), size=times)
      # then get the sample offset indices
      sampleEnd <- sampleOnset + sampleLength - 1
      # make a data frame for the sample segments
      DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
      # each row is a sample from the time series input 
      # populate the data frame
      for (i in 1:times) {
        DF[i,] <- x[sampleOnset[i]:sampleEnd[i]]
      }
      # View(DF)
      # make a function to get the range from the DF rows
      dfRangeFn <- function(x) { max(x)-min(x) }
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
        scaleVal <- yRange/median(dfRange)
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
    offsetMaxVal <- maxY - max(xOut[firstRow:lastRow])
    # 12-31-2016 change this to + offsetVal
    if(offsetMaxVal < 0) { xOut <- xOut + offsetMaxVal }
    # get the offset min value and offset the data if necessary
    offsetMinVal <- minY - min(xOut[firstRow:lastRow])
    if(offsetMinVal > 0) { xOut <- xOut + offsetMinVal }
    # check the range again
    newRange <- max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow])
    # initialize the default rescale value for this else condition
    rescaleVal <- 1
    # rescale the data if the range exceeds the maxY and minY values
    # if(newRange > ( maxY - (minY + (.4 * yRange))) ) { 
    # adjust the maxY
    if(max(xOut[firstRow:lastRow]) > maxY) {
      maxOffset <- maxY - max(xOut[firstRow:lastRow])
      xOut <- xOut + maxOffset
    }
    # adjust the minY
    if(min(xOut[firstRow:lastRow]) < minY) {
      rescaleVal <- ( maxY - minY ) / newRange
      xOut <- xOut * rescaleVal
    }
    # check the max again
    if(max(xOut[firstRow:lastRow]) > maxY) {
      maxOffset <- maxY - max(xOut[firstRow:lastRow])
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
    # get the offset value for the first event
    offsetVal <- x[firstRow]
    # compute the offset adjustment
    offsetAdjust <- y - offsetVal
    # offset the data
    xOut <- x + offsetAdjust
    # check to ensure that the data to not exceed the difference between maxY and minY
    if(max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]) > maxY-minY) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]))
      # rescale the data
      xOut <- xOut * newScaleVal
    } 
    # check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(min(xOut[firstRow:lastRow]) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    }
    if(max(xOut[firstRow:lastRow]) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    } 
  } else { 
    # for shart charts less than 30 seconds
    offsetVal <- median(x)
    # use the median of x instead of the onset of the first event
    offsetAdjust <- y - offsetVal
    xOut <- x + offsetVal
    # check to ensure that the data to not exceed the difference between maxY and minY
    if(max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]) > maxY-minY) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]))
      # rescale the data
      xOut <- xOut * newScaleVal
    } 
    # check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(min(xOut[firstRow:lastRow]) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    } 
    if(max(xOut[firstRow:lastRow]) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    }
  } 
  assign("newScaleVal", newScaleVal, pos=1)
  assign("offsetVal", offsetVal + newOffsetAdjust, pos=1)
  return(xOut)
} # end offsetDataFn() function


