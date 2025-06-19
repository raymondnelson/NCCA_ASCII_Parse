# EDA signal processing
# 5-1-2016
# Raymond Nelson


# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)


PTTFixSquareWaveFn <- function(x=chartDF$PTTPTT) {
  # R function to correct a PTT square wave that includes unexpected transitional values 
  xOut <- x
  # iterate backward and remove unchanged PTT values
  for(i in length(xOut):2) {
    if(is.na(xOut[i]) || is.na(xOut[(i-1)])) next()
    if(xOut[i] == xOut[(i-1)]) {
      xOut[i] <- NA
    }
  }
  # get the non-NA indices
  nonNAIndices <- which(!is.na(xOut))
  # calculate the distance between non-NA indices
  nonNADistance <- diff(rev(nonNAIndices))
  # remove the transitional value
  xOut[nonNAIndices[nonNADistance != -1]] <- NA
  # fill the NA values by iterating backward
  for(i in length(xOut):2) {
    if(is.na(xOut[i])) { xOut
      # use the value from the max row with a non-NA value
      xOut [i] <- xOut[max(which(!is.na(xOut[1:(i-1)])))]
    }
  }
  return(xOut)
}


# chartDF$c_PTTPTT <- PTTFixSquareWaveFn(x=chartDF$PTTPTT)
# chartDF$c_PTTPTT <- chartDF$c_PTTPTT - chartDF$c_PTTPTT[1]


PTTFixNAFn <- function(x=chartDF$PTTPTT) {
  # PTTFixNAFn
  # in in the PTTSigProc.R script
  # R function to replace NA values in the time series data
  # takes the next value to replace each NA value
  # x input is a vector of time series data
  # output is a vector 
  ###
  # exit if the entire input vector is NA
  if(length(which(is.na(x)))==length(x)) { return(x) }
  # fix the situation when the first value is NA
  if(is.na(x[1])) { x[1] <- x[min(which(!is.na(x)))] }
  # fix the situation when the last value is NA
  if(is.na(x[length(x)])) { x[length(x)] <- x[max(which(!is.na(x)))] }
  # use a loop to fix remaining NA values
  for(i in 2:(length(x)-1)) {
    # use the mean of 
    # if(is.na(x[i])) { x[i] <- mean(x[i-1],x[min(which(!is.na(x)))]) } 
    if(is.na(x[i])) { 
      if(is.na(x[(i-1)]) || is.na(x[(i+1)])) next()
      x[i] <- x[(i+1)] }
  } 
  return(x)
}


# chartDF$c_PTTPTT <- PTTFixNAFn(x=chartDF$PTTPTT)
# chartDF$c_PTTPTT <- chartDF$c_PTTPTT - chartDF$c_PTTPTT[1]


PTTInterpolateSquareWaveFn <- function(x=chartDF$c_PTTPTT) {
  # R function to interpolate a square wave
  xOut <- x
  for(i in length(xOut):2) {
    if(xOut[i] == xOut[(i-1)]) xOut[i] <- NA
  } 
  # locate the PTT rows that are not NA
  nonNAIndices <- which(!is.na(xOut))
  # calculate the distance between non-NA values
  nonNADistance <- diff(rev(nonNAIndices))
  # keep the onset indices for each step of the square wave
  nonNAIndices <- c(1, rev( rev(nonNAIndices)[which(diff(rev(nonNAIndices)) != -1)] ))
  # get the y-axis value for each index
  nonNAValues <- x[nonNAIndices]
  ## interpolation
  # first get the difference vectors
  indexDiff <- diff(nonNAIndices)
  valueDiff <- diff(nonNAValues)
  # make sure the difference vectors are equal length
  if (length(indexDiff) < length(valueDiff)) {
    valueDiff <- c(valueDiff, rep(valueDiff[length(valueDiff)], times=(length(indexDiff)-length(valueDiff))))
  }
  # use a divisor
  
  
  
  
  
  
  return(xOut)
}


# chartDF$c_PTTPTT <- 


PTTInterpolateNAFn <- function(x=chartDF$c_PTTPTT_abs) {
  # PTTInterpolateNAFn
  # in the PTTSigProc.R script
  # R function to interpolate NA runs in the PTT time series data
  # x is the PTT time series vector
  
  
  
  # x is a vector of peak row indices in the data vector
  # y is a vector of peak row values in the data vector
  
  
  # output is a vector
  ###
  # locate the data rows with missing values
  naIdices <- which(is.na(x))
  # locate the PTT rows that are not NA
  nonNAIndices <- which(!is.na(x))
  # calculate the distance between non-NA values
  nonNADistance <- diff(rev(nonNAIndices))
  
  # keep only the rows that are not adjacent
  which(diff(rev(nonNAIndices)) != 1)
  
  nonNAIndices <- rev( rev(nonNAIndices)[which(diff(rev(nonNAIndices)) != -1)] )
  
  nonNAValues <- x[nonNAIndices]
  
  nonNADistance <- nonNADistance[nonNADistance != 1]
  
  
  
  nonNAValue <- x[]


  # first get the difference vectors
  peakOutDiff <- diff(x)
  peakValDiff <- diff(y)
  # then make sure the difference vectors are equal length
  if (length(peakValDiff) < length(peakOutDiff)) {
    peakValDiff <- c(peakValDiff, rep(peakValDiff[length(peakValDiff)], times=(length(peakOutDiff)-length(peakValDiff))))
  }
  # use a divisor
  # calculate the vector of difference values for samples between each peak
  peakValDiff2 <- peakValDiff / peakOutDiff
  # calculate a filled vector by repeating each difference value according to the difference between peaks
  peakFill <- rep(peakValDiff2, times=peakOutDiff)
  # then calculate the cumulative sum of the peak fill vector as the interpolated peak line
  # include the initial value from the vector of input values 
  # to restore the length of the data vector and set the onset value 
  peakFill2 <- cumsum(c(y[1], peakFill))
  # to preserve the exact peak values for verification of the interpolation
  # peakFill2[x] <- y # 
  return(peakFill2)
}




######## main function ########




PTTSigProcFn <- function(x=chartDF, 
                         EDAHigh=TRUE, 
                         EDALow=TRUE, 
                         EDAFilter="none",
                         first=firstEvent, 
                         last=lastEventEnd ) {
  # apply the filter functions to the data frame columns
  # first and last are used for scaling the data
  
  chartDF <- x
  
  {
    if(!exists("EDAHigh")) EDAHigh <- TRUE
    if(!exists("EDALow")) EDALow <- TRUE
    if(!exists("EDAFilter")) EDAFilter <- EDAFilt
    if(!exists("first")) first <- firstEvent
    if(!exists("last")) last <- lastEventEnd
  }
  
  {
    assign("EDAHigh", EDAHigh, envir=.GlobalEnv)
    assign("EDALow", EDALow, envir=.GlobalEnv)
    assign("EDAFilter", EDAFilter, envir=.GlobalEnv)
    assign("first", first, envir=.GlobalEnv)
    assign("last", last, envir=.GlobalEnv)
  }
  
  assign("chartDF", chartDF, envir=.GlobalEnv)
  # stop()
  
  # EDAHigh=EDAH
  # EDALow=EDAL
  # EDAFilter=EDAFilter
  
  
  
  # remove unexpected transitional values from the PTT square wave
  # chartDF$c_PTTPTT <- PTTFixSquareWaveFn(x=chartDF$PTTPTT)
  # plot.ts(chartDF$c_PTTPTT)
  
  # invert the PTT signal 
  chartDF$c_PTTPTT <- chartDF$c_PTTPTT * -1

  # {
  #   cor(chartDF$c_CardioMA[1000:5000], chartDF$c_PTTPTT[1000:5000])
  #   cor(chartDF$c_CardioMid[1000:5000], chartDF$c_PTTPTT[1000:5000])
  #   cor(chartDF$c_CardioSRQ1[1000:5000], chartDF$c_PTTPTT[1000:5000])
  #   cor(chartDF$c_CardioSRQ2[1000:5000], chartDF$c_PTTPTT[1000:5000])
  # 
  #   cor(chartDF$c_Cardio1[1000:5000], chartDF$c_PTTPTT[1000:5000])
  # 
  #   cor(chartDF$c_PTTPTT[1000:5000], chartDF$c_PTTPPG[1000:5000])
  # 
  #   cor(chartDF$c_PTTPTT[1000:5000], chartDF$c_PTTPPG[1000:5000])
  # 
  #   cor(chartDF$c_PTTPTT[1000:5000], chartDF$c_PTTECG[1000:5000])
  # 
  #   cor(chartDF$c_PTTPPG[1000:5000], chartDF$c_PTTECG[1000:5000])
  # 
  #   # chartDF$
  # }
  
  
  
  # initialize an abstracted PTT channel and set the PTT data origin (first sample) to 0 
  chartDF$c_PTTPTT_abs <- chartDF$c_PTTPTT - chartDF$c_PTTPTT[1]
  
  # invert the signal
  # chartDF$c_PTTPTT_abs <- chartDF$c_PTTPTT_abs * -1
  
  # first=firstEvent 
  # last=lastEventEnd
  
  # set the range
  # chartDF$c_PTTPTT_abs <-
  #   setColRange(DAT=chartDF$c_PTTPTT_abs, y=colRange, firstRow=first, lastRow=last)
  
  # # fix NA values
  # chartDF$c_PTTPTT_abs <- NAInterp(chartDF$c_PTTPTT_abs)
  
  
  
  
  # # get the onset value in case it is not already zero
  # onsetValue <- chartDF$c_PTTPTT_abs[1]

  # # keep only the PTT values that are repeated (ignore transitional values
  # for(i in nrow(chartDF):2) {
  #   if(is.na(chartDF$c_PTTPTT_abs[(i-1)])) next()
  #   if(chartDF$c_PTTPTT_abs[i] == chartDF$c_PTTPTT_abs[(i-1)]) {
  #     chartDF$c_PTTPTT_abs[i] <- NA
  #   }
  # }
  # 
  # # interpolate the PTT line 
  # chartDF$c_PTT_abs <- NAInterp(chartDF$c_PTTPTT_abs)
  
  # plot.ts(chartDF$c_PTT_abs[1000:3000])
  
  return(chartDF)
  
} # end PTTSigProcFn()

