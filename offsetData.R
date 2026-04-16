# R function to offset (y-axis) the time series data in NCCA ASCII charts
# Raymond Nelson
# first version 4-17-2016
# modified 1-25-2019
# moved from the sigProcHelper.R script to newOffsetData.R script 2026Apr14
# called by the ScaleOffsetDataFn() function
####



offsetDataFn <- function(x=chartDF$c_Cardio1, 
                         y=0, 
                         maxY=(yMax-.05*yRange), 
                         minY=(yMin+.05*yRange), 
                         firstRow=firstEvent, 
                         lastRow=(lastEventEnd-450)) {
  # R function to offset (y-axis) the time series data in NCCA ASCII charts
  # Raymond Nelson
  # first version 4-17-2016
  # modified 1-25-2019
  # 2026Apr14 moved from the sigProcHelper.R script to newOffsetData.R script 
  # called by the ScaleOffsetDataFn() function
  ####
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
  
  ## offset the data ##
  
  if(dataLength > (60*cps)) {
    
    # for charts greater than 60 seconds
    
    # get the offset value for the first event
    offsetVal <- x[firstRow]
    
    # 2026Apr15 new method using the median
    offsetVal <- median(x[c(firstRow:lastRow)])
    
    # compute the offset adjustment
    offsetAdjust <- y - offsetVal
    # offset the data
    xOut <- x + offsetAdjust
    
    # Nov 18, 2023 improvement to prevent occasional odd placement of EDA
    # offsetVal2 <- y - xOut[firstRow]
    # offsetAdjust2 <- offsetVal2 - quantile(c(x), .25, na.rm=TRUE)
    
    # max(xOut)
    # min(xOut)
    # plot.ts(xOut)
    # summary(xOut)
    
    # commented out 2026Apr15
    offsetAdjust2 <- y - quantile(xOut, .2, na.rm=TRUE)
    xOut <- xOut + offsetAdjust2
    
    # plot.ts(xOut)
    
    # June 20, 2025
    Xmax <- max(xOut[firstRow:lastRow], na.rm=TRUE)
    Xmin <- min(xOut[firstRow:lastRow], na.rm=TRUE)
    
    # check to ensure that the data to not exceed the difference between maxY and minY
    if(Xmax - Xmin > maxY-minY) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (Xmax - Xmin)
      # rescale the data
      xOut <- xOut * newScaleVal
    } 
    # re-check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(min(xOut[firstRow:lastRow], na.rm=TRUE) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    }
    if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    } 
    
  } else { 
    
    # for shart charts less than 60 seconds
    
    offsetVal <- median(x)
    # use the median of x instead of the onset of the first event
    offsetAdjust <- y - offsetVal
    xOut <- x + offsetVal
    # June 20, 2025
    Xmax <- max(xOut[firstRow:lastRow], na.rm=TRUE)
    Xmin <- min(xOut[firstRow:lastRow], na.rm=TRUE)
    # check to ensure that the data to not exceed the difference between maxY and minY
    if(Xmax - Xmin > maxY-minY) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (Xmax - Xmin)
      # rescale the data
      xOut <- xOut * newScaleVal
    } 
    # re-check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(min(xOut[firstRow:lastRow], na.rm=TRUE) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    } 
    if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      xOut <- xOut + newOffsetAdjust
    }
  } # end short charts < 30 seconds
  
  assign("newScaleVal", newScaleVal, pos=1)
  assign("offsetVal", offsetVal + newOffsetAdjust, pos=1)
  
  return(xOut)
  
} # end offsetDataFn() function




