# R function to offset the time series data for plotting

offsetDataFn <- function(x=chartDF$c_Cardio1, y=-45, yMax=165, yMin=-165, firstRow=firstEvent, lastRow=lastEventEnd) {
  # function to offset the time series data for plotting
  # x is the time series vector
  # y is the offset value to locate the onset of the first event
  # yMax is the maximum y value to display the data (less than the max y index on the chart)
  # yMin is the minimum y value to display the data (more than the min y index on the chart)
  # firstRow is the row index of the onset of the first stimulus event in the time series data
  # lastRow is the row index of then end of the last stimulus event in the time series data 
  # output is the offset time series data
  ######
  dataLength <- length(x)
  # handle shart charts
  if(dataLength <= 600) {
    offsetVal <- median(x)
    offsetAdjust <- y - offsetVal
    xOut <- x + offsetVal
  } else { 
    # get the offset value for the first event
    offsetVal <- x[firstRow]
    # compute the offset adjustment
    offsetAdjust <- y - offsetVal
    # offset the data
    xOut <- x + offsetAdjust
    # check to ensure that the data to not exceed the difference between yMax and yMin
    if(max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]) > yMax-yMin) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (yMax-yMin) / (max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]))
      # rescale the data
      xOut <- xOut * newScaleVal
    } # end if
    # check to ensure that the data do not exceed yMax or yMin
    if(max(xOut[firstRow:lastRow]) > yMax) {
      newOffsetAdjust <- yMax - max(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    } else if(max(xOut[firstRow:lastRow]) < yMin) {
      newOffsetAdjust <- yMin - min(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    } # end else if
  }
  return(xOut)
} # end offsetDataFn() function
# offsetDataFn(x=chartDF$c_Cardio1, y=-45, yMax=165, yMin=-165, firstRow=firstEvent, lastRow=lastEventEnd)

