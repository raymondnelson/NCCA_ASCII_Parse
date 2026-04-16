# R function to scale the time series data in NCCA ASCII charts
# Raymond Nelson
# first version 4-17-2016
# modified 1-25-2019
# moved from the sigProcHelper.R script to newScaleData.R script 2026Apr14
# called by the ScaleOffsetDataFn() function
####



scaleDataFn <- function(x=chartDF$c_UPneumoSm, 
                        sec=6, 
                        times=20, 
                        ignore=2, 
                        yRange=(.1*yRange), 
                        maxY=(yMax-.05*yRange), 
                        minY=(yMin+.05*yRange),  
                        firstRow=firstEvent, 
                        lastRow=(lastEventEnd-450)) {
  # R function to scale the time series data in NCCA ASCII charts
  # Raymond Nelson
  # first version 4-17-2016
  # modified 1-25-2019
  # 2026Apr14 moved from the sigProcHelper.R script to newScaleData.R script 
  # called by the ScaleOffsetDataFn() function
  ####
  # x input is the time series vector for a a single time series channel, from a single chart
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
  # output is the rescaled time series vector vector
  # also assigns a scalar scaleVal to the global environment,
  # for use when scaling the additional channels
  ####
  
  if(!exists("firstRow")) firstRow <- 1
  if(!exists("lastRow")) lastRow <- length(x)
  
  if(x[1] == -9.9) { return(x) }
  
  set.seed(1234567890)
  
  dataLength <- length(x)
  
  sampleLength <- sec * cps
  
  # exit if the vector is NA
  if(length(which(is.na(x)))==dataLength) return(x)
  
  # determine the number of items to include in each sample
  # check the firstRow and lastRow
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength-sampleLength + 1
  
  # initialize the default rescale value
  rescaleVal <- 1
  
  # initialize the default scaleVal
  scaleVal <- 1
  
  ## for flat-line data ##
  
  if(max(x[firstRow:lastRow]) == min(x[firstRow:lastRow])) {
    # set the scale value using the max and min
    # scalVal is already 1 at this point,
    # rescaleVal is also 1 at this point
    xOut <- x * scaleVal
    assign("scaleVal", (scaleVal * rescaleVal), pos=1)
    return(xOut)
  }
  
  ## charts that are shorter than the sampling length ##
  
  if(firstRow >= (lastRow-sampleLength)) {
    if(max(x[firstRow:lastRow]) != min(x[firstRow:lastRow])) {
      scaleVal <- yRange / abs( max(x[firstRow:lastRow]) - min(x[firstRow:lastRow]) )
      xOut <- x * scaleVal
      assign("scaleVal", (scaleVal * rescaleVal), pos=1)
      return(xOut)
    } else {
      # for flat-line data
      # scaleVal <- 1 
      xOut <- x * scaleVal
      assign("scaleVal", (scaleVal * rescaleVal), pos=1)
      return(xOut)
    }
  }
  
  ## non flat-lined charts that are not shorter than the sampling length ##
  
  if(dataLength <= (60 * cps)) {
    
    ### for short charts less than 60 seconds ###
    
    if(max(x, na.rm=TRUE) <= min(x, na.rm=TRUE)) {
      # for flatline data
      dataRange <- 1
    } else {
      # use the entire vector
      dataRange <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    } # end else
    
    # compute the scaleVal for short charts < 60 sectons
    scaleVal <- yRange / dataRange
    # scale the output vector
    
    xOut <- x * scaleVal
    
    assign("scaleVal", (scaleVal * rescaleVal), pos=1)
    
    return(xOut)
    
  } else {
    
    ### for charts > 60 seconds - most charts ###
    
    { 
      
      # get the sample onset row indices using random numbers
      
      # check the length and times
      if(length(c(firstRow:(lastRow-sampleLength))) <= times) { 
        times <- length(c(firstRow:(lastRow-sampleLength))) 
      } 
      
      if(times > 1) {
        # Aug 2, 2023 adjustment to work with a single sample iteration
        sampleInterval <- trunc(sec / 2) * cps
        sampleOnset <- sampleInterval
        while(sampleOnset[length(sampleOnset)] < (lastRow-sampleLength)) {
          # while the last sample onset does not exceed the last data row
          sampleOnset <- c(sampleOnset, sampleOnset[length(sampleOnset)] + sampleInterval)
        } 
        sampleOnset <- sampleOnset[sampleOnset < (lastRow-sampleLength)]
        if(length(sampleOnset) == 0 ) sampleOnset <- firstRow
      } else { 
        # when times == 1
        sampleOnset <- firstRow
      } 
      
      # then get the sample offset indices
      sampleEnd <- sampleOnset + sampleLength - 1
      # make sure that samples do not exceed the time series data
      if(any(sampleEnd > dataLength)) {
        # improved for short/aborted chart July 28, 2024
        sampleEnd <- dataLength
      } else {
        sampleEnd <- sampleEnd[sampleEnd <= dataLength]
      }
      
      # adjust the times parameter
      times <- length(sampleEnd)
      
      sampleOnset <- sampleOnset[c(1:times)]
      
      # adjust the ignore parameter
      ignore <- round(times * .5)
      
    }
    
    {
      
      ## make a data frame for the sample segments ##
      
      DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
      # each row is a sample from the time series input 
      
      # populate the data frame
      for (i in 1:times) {
        DF[i,] <- x[sampleOnset[i]:sampleEnd[i]]
      }
      # View(DF) 
      
    }
    
    ## get the scaleVal from the dfRange vector that was obtained using the DF data frame ##
    
    {
      
      # make a private function to get the range from the DF rows
      dfRangeFn <- function(x) { max(x, na.rm=TRUE)-min(x, na.rm=TRUE) }
      
      # apply the range function to each row of the DF to get the range of each row
      # apply is vectorized and needs no loop
      dfRange <- apply(DF, 1, dfRangeFn)
      # check and fix the ignore value if necessary
      if(ignore >= length(dfRange)) ignore <- round(length(dfRange) / 2, 0)
      # sort and remove the largest changes using the ignore input parameter 
      dfRange <- sort(dfRange, decreasing = TRUE)[(ignore+1):length(dfRange)]
      
      # summary(dfRange)
      
      if( median(dfRange)!=0 ) {
        # get the scaling value from the dfRange
        # scaleVal <- yRange/median(dfRange)
        scaleVal <- yRange / mean(dfRange)
      } else {
        # to avoid /0 problems with flat-line data
        scaleVal <- 1
      } 
      
    }
    
    ### scale the output vector ###
    
    {
      
      ## only for charts > 60 seconds when the data are not flat-lined ##
      
      xOut <- x * scaleVal
      
      # get the offset max value and offset the data if necessary
      offsetMaxVal <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
      if(offsetMaxVal < 0) { xOut <- xOut + offsetMaxVal }
      
      # get the offset min value and offset the data if necessary
      offsetMinVal <- minY - min(xOut[firstRow:lastRow], na.rm=TRUE)
      if(offsetMinVal > 0) { xOut <- xOut + offsetMinVal }
      
      # check the range again
      newRange <- 
        max(xOut[firstRow:lastRow], na.rm=TRUE) - min(xOut[firstRow:lastRow], na.rm=TRUE)
      
      # rescale the data if the range exceeds the maxY and minY values
      # adjust the maxY
      if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
        maxOffset <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
        xOut <- xOut + maxOffset
      }
      
      # adjust the minY
      if(min(xOut[firstRow:lastRow], na.rm=TRUE) < minY) {
        rescaleVal <- ( maxY - minY ) / newRange
        xOut <- xOut * rescaleVal
      }
      
      # check the max again
      if(max(xOut[firstRow:lastRow], na.rm=TRUE) > maxY) {
        maxOffset <- maxY - max(xOut[firstRow:lastRow], na.rm=TRUE)
        xOut <- xOut + maxOffset
      }
      
    }
    
  } # end else for charts > 60 seconds 
  
  # assign the scale value to the global environment
  # the scale value needs to be used to scale additional channels for each sensor
  assign("scaleVal", (scaleVal * rescaleVal), pos=1)
  
  return(xOut)
  
} # end scaleDataFn()  


####






