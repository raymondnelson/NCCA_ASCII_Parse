# PLE signal processing
# 5-1-2016
# Raymond Nelson


PLESigProcFn <- function(x=chartDF, first=firstEvent, last=lastEventEnd, baseLine=TRUE) {
  
  chartDF <- x
  
  # reset the zero-centered PLE data
  chartDF$c_PPG1 <- chartDF$PPG1 - chartDF$PPG1[1]
  
  chartDF$c_PPG1 <- 
    setColRange(DAT=chartDF$c_PPG1, y=colRange, firstRow=first, lastRow=last)
    
  if(!exists("PLEBaseline")) PLEBaseline <- TRUE
  
  if(isTRUE(PLEBaseline)) {
  	# chartDF$c_PPG1 <- highPass.338628(chartDF$c_PPG1)
    # Nov 28, 2022
  	chartDF$c_PPG1 <- highPass2nd.5Hz(chartDF$c_PPG1)
  }
  
  # ts.plot(chartDF$c_PPG1)
  # ts.plot(highPass.338628(chartDF$c_PPG1))
  
  # # scale the PLE data 
  # chartDF$c_PPG1 <- scaleDataFn(chartDF$c_PPG1, sec=10, times=40, ignore=4, xRange=scaleVals[5], maxY=165, minY=-165, firstRow=first, lastRow=last)
  
  # # offset the PLE data
  # chartDF$c_PPG1 <- offsetDataFn(x=chartDF$c_PPG1, y=yOffset[5], yMax=165, yMin=-165, firstRow=first, lastRow=last)
  
  ####
  
  # if(length(is.na(chartDF$PL)) == nrow(chartDF)) {
  #   chartDF$c_PPG1 <- 0
  #   # PLMaxInterp <- 0
  #   # PLMinInterp <- 0
  #   return(chartDF)
  # }
  
  # assign("chartDF", chartDF, envir=.GlobalEnv)
  
  # first ensure that all peaks are recorded on a single sample
  # because there are rare times in which the time series reduction 
  # can result in a peak value over 2 adjacent samples
  # interpolate and eliminate duplicated peak points
  chartDF$c_PPG1 <- fixPeak(x=chartDF$c_PPG1, times=2)

  # get the cardio rate and buffer length for peak extraction
  # 7-29-2017 to fix the bd systolic line in PLE data
  cardioRate <- ratePerMin(chartDF$c_PPG1,buffer=9,peaks="upper",lowPass=TRUE)
  bufferLen <- bufferLenFn(cardioRate, y=.6)
  # cardioRate <- ratePerMin(chartDF$c_PPG1,buffer=3,peaks="upper",lowPass=TRUE)
  # bufferLen <- bufferLenFn(cardioRate)
  
  # use a function to get the min peak rows
  # minOut <- minPeak(x=chartDF$c_PPG1, y=bufferLen)
  # minOut <- minPeak(x=chartDF$c_PPG1, y=8)
  # change to y=12 for slow pulse 
  
  # get the min peak values for the min peak rows
  # minVal <- chartDF$c_PPG1[na.omit(minOut)]
  
  ### 10-6-2015 use the Tukey fence to remove 
  
  # keep only those min values that exceed the Tukey lower inner fence
  #         minOut <- tukeyFence1(x=chartDF$c_PPG1, y=minOut, z=5)
  #         minVal <- chartDF$c_PPG1[na.omit(minOut)]        
  
  ####
  
  # interpolate between the min peak values
  # PLMinInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
  # interpolatePeaks(x=maxOut, y=chartDF$c_PPG1[maxOut])[1:nrow(chartDF)]
  # plot.ts(diastolicInterp, ylim=c(-3,10))
  
  # add the vector to the diastolic cardio column 
  # chartDF$c_PPG1Min <- PLMinInterp[1:nrow(chartDF)]
  
  # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
  # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
  
  ####
  
  # get the max peak indices
  # maxOut <- maxPeak(x=chartDF$c_PPG1, y=8)
  # maxOut <- maxPeak(x=chartDF$c_PPG1, y=bufferLen)
  
  # get the max peak values
  # maxVal <- chartDF$c_PPG1[maxOut]
  
  ### use the tukey fence to remove problems
  
  # keep only those min values that exceed the Tukey lower inner fence
  #         maxOut <- tukeyFence1(x=chartDF$c_PPG1, y=maxOut, z=5)
  #         maxVal <- chartDF$c_PPG1[na.omit(maxOut)]
  
  ###
  
  # interpolate between max peak values
  # PLMaxInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
  # plot.ts(systolicInterp, ylim=c(-3,10))
  # myCardioData2$CardioSyst <- systolicInterp
  # ts.plot(myCardioData2[1:3000,c(1,2,6)])
  
  # add the systolic time series to the data frame
  # chartDF$c_PPG1Max <- PLMaxInterp
  
  #### 
  
  # compute the smoothed PLE data
  # smoothedPL <- MASmooth(x=chartDF$c_PPG1, y=30, times=4) # y=15, times=3 will show the respiration
  # plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))
  
  # add the smoothed PLE to the time series data frame
  # chartDF$c_PPG1MA <- smoothedPL[1:nrow(chartDF)]
  # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
  # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
  
  # compute the PLE diastolic and systolic pulse amplitude compared to the middle
  # chartDF$c_PPG1SystAmp <- chartDF$c_PPG1Max - chartDF$c_PPG1MA
  # chartDF$c_PPG1DiastAmp  <- chartDF$c_PPG1MA - chartDF$c_PPG1Min
  
  # add the PLE pulse amplitude data to the daeta frame
  # chartDF$c_PPG1Amp <- chartDF$c_PPG1Max - chartDF$c_PPG1Min
  
  return(chartDF)
  
  } # end newPLESigProcFn()




highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.035475913e+00, zplane = 0.9314790191) {
  # highPass.338628()
  # high pass filter to constrain the PLE data to a stable baseline
  # first order Butterworth filter
  xv1 <- data[1]
  yv1 <- 0
  output <- rep(NA, length(data))
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end highPass.338628() function




fixPeak <- function(x=chartDF$c_Cardio1, times=4) {
  # function to fix max and min peaks that are repeated 2 or more times
  # input x is the time series data
  # times is the number of times to repeat the operaation from 1 to 8
  # this function will keep the first of all adjacent max and min peaks
  # and interpolate the subsequent equal peak with the next sample
  ###
  if(length(which(is.na(x)))==length(x)) { return(x) }
  #may need to re-write this with a while loop
  for (j in 1:times) {
    for (i in 9:(length(x)-9)) {
      if(any(is.na(x[i:(i-8)]))) next()
      if(x[i]==x[(i-1)]) { x[i] <- mean(c(x[(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-2)]) { x[i] <- mean(c(x[(i-2):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-3)]) { x[i] <- mean(c(x[(i-3):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-4)]) { x[i] <- mean(c(x[(i-4):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-5)]) { x[i] <- mean(c(x[(i-5):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-6)]) { x[i] <- mean(c(x[(i-6):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-7)]) { x[i] <- mean(c(x[(i-7):(i-1)], x[(i+1)])) }
      if(x[i]==x[(i-8)]) { x[i] <- mean(c(x[(i-8):(i-1)], x[(i+1)])) }
    }
  }
  return(x)
} # end fixPeak() function





ratePerMin <- function(x=chartDF$c_UPneumo, buffer=42, peaks="upper", lowPass=FALSE) {
  # function to calculate the cyclic rate per minute
  # for cardio and pneumo time series data
  # x input is a a time series vector
  # peaks input is a switch to choose "upper" or "lower" peaks
  # buffer input is the number of pre and post index samples to include in the search space
  # buffer = 40 for pneumo
  # buffer = 9 for cardio
  # output is the mean rate 
  ####
  # first smooth the input to remove artifact peaks
  if(lowPass == TRUE) { 
    x1 <- lowPass2hz.2nd(x) 
  } else {
    x1 <- x
  }
  # then get the max or min peak
  ifelse(peaks=="lower",
         Peaks <- minPeak(x=x1, y=buffer, firstLast=FALSE),
         Peaks <- maxPeak(x=x1, y=buffer, firstLast=FALSE) 
  )  
  # calculate the rate
  
  peakDiffs <- diff(Peaks)
  # remove peak diffs of 1 because these occur when the cardio cuff is deflated
  # peak diffs of 1 will distort the cardio rate measurement
  peakDiffs <- peakDiffs[which(peakDiffs != 1)]

  rateMin <- ifelse(length(peakDiffs)>1,
                    60 / (mean(peakDiffs) / cps ),
                    60/1/cps
  )
  return( round(rateMin , 2) )
} # end ratePerMin() function






bufferLenFn <- function(x, y=.6) {
  # function to calculate the buffer length for the maxPeak and minPeak functions
  # x input is the rate per min scalar output from the ratePerMin function
  # y is the number of cycles
  # output is an integer that defines the 1/2 buffer width 
  # for the number of cardiac cycles
  # in samples surrounding the index sample of the minPeak and maxPeak function
  bLen <- floor(1 / x * 60 * (cps*y)) - 1
  if(bLen <= 0) bLen <- 1
  return(bLen)
}




minPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # private function to get the cyclic peaks from the time series data
  # will keep the index number of min peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
  xOut <- rep(NA, times=(length(x)))
  if(firstLast==TRUE){
    xOut[1] <- 1 # keep the first
    xOut[length(xOut)] <- length(xOut) # keep the last
  }
  # buffer will be double the offset value
  input_buffer <- x[2:(2*y+1)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    # check to determine if the middle of the buffer (y) is the min
    ifelse(input_buffer[(y+1)]==min(input_buffer), #
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           next()
    )
  } # end for loop
  return(as.numeric(na.omit(xOut)))
} # end minPeak function





maxPeak <- function(x, y=round(.25*cps,0), firstLast=TRUE) {
  # function to get the cyclic peaks from the time series data
  # will keep the index number of max peak samples
  # x input is a time series vector
  # y input is the number of offset samples 
  # firstLast 
  ###
  # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
  xOut <- rep(NA, times=(length(x)))
  if(firstLast==TRUE){
    xOut[1] <- 1 # keep the first
    xOut[length(xOut)] <- length(xOut) # keep the last
  }
  # buffer will be double the offset value
  input_buffer <- x[2:(2*y+1)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    # check to see if the middle value of the buffer is the max
    ifelse(input_buffer[(y+1)]==max(input_buffer),
           xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
           # work on this 4/23/2016
           # another way is to
           # create a buffer of NA and keep only the i+y+1 set to the row index
           # slice the buffer into the vector 
           # the result should be the same
           # non max samples in the buffer are NA
           next()
    )
  } # end for loop
  return(as.numeric(na.omit(xOut)))
}  # end maxPeak





interpolatePeaks <- function(x=na.omit(maxOut), y=na.omit(maxVal)) {
  # private function to interpolate between peak segments of time series input data
  # x is a vector of peak row indices in the data vector
  # y is a vector of peak row values in the data vector
  # output is
  ###
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
} # end interpolatePeaks



highPass2nd.5Hz <- function(data=chartDF$c_PPG1, GAIN = 1.07686236756531e+000, zplane0 = -0.86234862603008, zplane1 = 1.85214648539594) {
  # highPass2nd.5Hz()
  # high pass filter to constrain the PLE data to a stable baseline
  # second order Butterworth filter
  # Nov 28, 2022
  # Raymond Nelson
  xv1 <- data[1]
  xv2 <- data[1]
  yv1 <- 0
  yv2 <- 0
  output <- rep(NA, length(data))
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- (xv0 +
              xv2) -
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end highPass2nd.5Hz() function​



