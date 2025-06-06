# cardio signal processing
# 5-1-2016
# Raymond Nelson

SRQ2001_cardioSquareWaveFn <- function(x=chartDF$c_Cardio1, segLen=15) {
  # R function to reduce the cardio data to a square wave 
  # using the procedure described by Stern, Ray & Quigley (2001)
  ###
  dataLen <- length(x)
  DAT <- rep(NA, times=dataLen)
  # calculate the number of .5sec segments using integer division
  nSegs <- length(x) %/% segLen 
  segIdcs <- c(1:nSegs) * segLen
  # get the modulus
  modLen <- length(x) %% segLen
  if(modLen > 0) {
    nSegs <- nSegs+1
    segIdcs <- c(segIdcs, dataLen)
  }
  for(i in 1:length(segIdcs)) {
    # locate the end row for the .5 sec segment
    thisIdx <- segIdcs[i]
    prevIdx <- segIdcs[(i-1)]
    if(length(prevIdx) == 0) prevIdx <- 0
    theseRows <- c((prevIdx+1):thisIdx)
    theseRows <- theseRows[theseRows <= dataLen]
    meanVal <- mean(x[theseRows])
    DAT[theseRows] <- meanVal
  }  
  # output the sqare wave
  return(DAT)
}



cardioSRQFn <- function(x=chartDF$c_Cardio1, segLen=15) {
  # R function to reduce the cardio data to a square wave 
  # using the procedure described by Stern, Ray & Quigley (2001)
  ###
  dataLen <- length(x)
  DAT <- rep(NA, times=dataLen)
  for(i in 1:(length(DAT)-(segLen-1))) {
    theseRows <- c(i:(i+(segLen-1)))
    meanVal <- mean(x[theseRows])
    DAT[theseRows] <- meanVal
  }
  # output the sqare wave
  return(DAT)
}



# plot.ts(SRQ2001_cardioSquareWaveFn(x=chartDF$c_Cardio1)[1000:3000])
# plot.ts(chartDF$c_Cardio1[1000:3000])

# cor(chartDF$c_Cardio1[1000:3000], SRQ2001_cardioSquareWaveFn(x=chartDF$c_Cardio1[1000:3000]) )
# cor(chartDF$c_Cardio1[1000:3000], chartDF$c_CardioMA[1000:3000])
# cor(chartDF$c_Cardio1[1000:3000], chartDF$c_CardioMid[1000:3000])
# cor(chartDF$c_CardioMid[1000:3000], chartDF$c_CardioMA[1000:3000])


#### May 2025 caliper functions ####


# source the script with the functions computing the cardio caliper 

source("~/Dropbox/R/NCCA_ASCII_Parse/cardioCaliperFunctions.R", echo=FALSE)




############## main function #################




cardioSigProcFn <- function(x=chartDF, first=firstEvent, last=lastEventEnd) {
  # function to process the cardio
  
  chartDF <- x
  
  # reset the zero-centered cardio data
  chartDF$c_Cardio1 <- chartDF$Cardio1 - chartDF$Cardio1[1]
  
  # rescale the range of the cardio data
  chartDF$c_Cardio1 <- 
    setColRange(DAT=chartDF$c_Cardio1, y=colRange, firstRow=first, lastRow=last)
  
  # {
  #   DAT <- chartDF$c_Cardio1
  #   xMed <- median(DAT)
  #   x25th <- quantile(DAT, .25)
  #   x75th <- quantile(DAT, .75)
  #   x99th <- quantile(DAT, .99)
  #   x01th <- quantile(DAT, .01)
  #   xIQR <- x75th - x25th
  #   xLimitLow <- xMed - (1 * xIQR)
  #   xLimitHigh <- xMed - (1 * xIQR)
  #   DAT[which(DAT <= x01th)] <- x01th
  #   chartDF$c_Cardio1 <- DAT
  #   }
  
  # fix NA values
  chartDF$c_Cardio1 <- NAInterp(chartDF$c_Cardio1)
  
  # first ensure that all peaks are recorded on a single sample
  # because there are rare times in which the time series reduction 
  # can result in a peak value over 2 adjacent samples
  # interpolate and eliminate duplicated peak points
  chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1, times=3)
  # this is also done in the scaleOffsetData script
  
  # very light smoothing of cardio data
  # because some LX4K and LX5K instruments have noisy cardio data
  chartDF$c_Cardio1 <- MASmooth(x=chartDF$c_Cardio1, y=1, times=1)
  
  # reset the zero-centered cardio data
  chartDF$c_Cardio1 <- chartDF$Cardio1 - chartDF$Cardio1[1]
  
  
  ##### compute the cardio rate using the peak to peak distance for the systolic line #####
  
  {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/cardioCaliperFunctions.R", echo=FALSE)
    
    cardioRate <- ratePerMinFn(x=chartDF$c_Cardio1, buffer=10, peaks="upper", lowPass=TRUE)
    cardioBufferLen <- bufferLenFn(x=cardioRate, y=.6)
    
    chartDF$c_CardioRate <- cardioRatePerCycleFn(x=chartDF$c_Cardio1, buffer=cardioBufferLen, peaks="upper", lowPass=TRUE)
    
    # cardioCaliperFn(x=cardioRateVc, caliperStart, caliperStop=NULL, caliperLen=15)
    
  }
  
  ##### slow moving average for cardio feature extraction #####
  
  {
    # compute a slower moving average for cardio feature extraction
    # was 1.5 sec and times=2 3/17/2017
    # was 1.2 sec and times=3 6/9/2020
    # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(.8*cps,0), times=4)
    
    # this is now done later when scaling and offsetting the cardio data
    # doing this later means that 
    # supplemental cardio lines are recreated when data are rescaled or adjusted
  }
  
  ###########
  
  # all this is done in the scaleOffsetData function
  
  # # compute the cardio mid line
  # chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
  # 
  # # computer the slower moving average for cardio data
  # # was 2 times, changed to 3 on 8-11-2016
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(.5*cps,0), times=2)
  # 
  # # diff(maxPeak(fixPeak(chartDF$c_Cardio1)))
  # 
  # # calculate the cardio rate
  # cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",lowPass=TRUE)
  # bufferLen <- bufferLenFn(cardioRate)
  # 
  # # calculate the cardio diastolic and systolic lines
  # maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # chartDF$c_CardioSystolic <- interpolatePeaks(x=maxOut, y=chartDF$c_Cardio1[maxOut])[1:nrow(chartDF)]
  # minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # chartDF$c_CardioDiastolic <- interpolatePeaks(x=minOut, y=chartDF$c_Cardio1[minOut])[1:nrow(chartDF)]
  
  ##########
  
  # # scale the cardio data 
  # chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=5, times=40, ignore=1, xRange=scaleVals[4], maxY=165, minY=-165, firstRow=first, lastRow=last)
  # 
  # # offset the cardio data
  # chartDF$c_Cardio1 <- offsetDataFn(x=chartDF$c_Cardio1, y=yOffset[4], yMax=165, yMin=-165, firstRow=first, lastRow=last)
  # 
  # calculate the cardio rate
  
  # max(chartDF$c_Cardio1[first:last])-min(chartDF$c_Cardio1[first:last])
  
  # cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",lowPass=TRUE)
  # # cardioRate <- ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps)
  # # ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="lower",dataRate=cps)
  # 
  # # ts.plot(lowPass1.7hz.2nd(chartDF$c_Cardio1)[1000:1500])
  # # ts.plot(chartDF$c_Cardio1[1000:1500])
  # 
  # # calculate the buffer length for the cardio rate
  # # bufferLen <- floor(1/cardioRate*60*cps*.60) - 1  # round down
  # # bufferLen <- bufferLenFn(x = ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps))
  # bufferLen <- bufferLenFn(cardioRate)
  
  #### systolic peaks
  
  # # get the max peak row indices
  # maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # 
  # # ts.plot(lowPass2hz(chartDF$c_Cardio1[1000:3000]))
  # # ts.plot(chartDF$c_Cardio1[1000:3000])
  # 
  # # 1/60*60*30/2
  # # 1/80*60*30/2
  # # 1/100*60*30/2
  # 
  # # get the max peak values
  # # maxVal <- chartDF$c_Cardio1[maxOut]
  # 
  # # interpolate between max peak values
  # systolicInterp <- interpolatePeaks(x=maxOut, y=chartDF$c_Cardio1[maxOut])[1:nrow(chartDF)]
  # 
  # # plot.ts(systolicInterp, ylim=c(-3,10))
  # # myCardioData2$CardioSyst <- systolicInterp
  # # ts.plot(myCardioData2[1:3000,c(1,2,6)])
  # 
  # # add the systolic time series to the data frame
  # chartDF$c_CardioSystolic <- systolicInterp
  
  #### diastolic peaks
  
  # # use a function to get the min peak row indices
  # minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # # minOut <- minPeak(x=chartDF$c_Cardio1, y=4)
  # 
  # # get the min peak values for the min peak rows
  # minVal <- chartDF$c_Cardio1[na.omit(minOut)]
  # 
  # # interpolate between the min peak values
  # diastolicInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal <- chartDF$c_Cardio1[na.omit(minOut)]))
  # 
  # # a test plot
  # # plot.ts(diastolicInterp, ylim=c(-3,10))
  # 
  # # add the vector to the diastolic cardio column
  # chartDF$c_CardioDiastolic <- diastolicInterp[1:nrow(chartDF)]

  # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
  # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
  
  ####
  
  # # compute a min-Max of the cardio time series data
  # 
  # # get the minMax row indices
  # minMaxOut <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
  # 
  # # get the minMax values
  # minMaxVal <- chartDF$c_Cardio1[minMaxOut]
  # 
  # # interpolate the minMax values
  # minMaxInterp <- na.omit(c(interpolatePeaks(x=minMaxOut, y=minMaxVal), 0))[1:nrow(chartDF)]
  # 
  # # plot.ts(minMaxInterp, ylim=c(-3,10))
  # 
  # # add the time series to the data frame
  # chartDF$c_CardioMinMax <- minMaxInterp
  
  #### 
  
  # # add the smoothed cardio to the time series data frame
  # chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=2)

  # a test plot
  # plot.ts(chartDF$c_CardioMid[1:3000], ylim=c(-3,10))
  # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
  # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
  
  # # compute a more stable moving average for the cardio data feature extraction
  # # to help with evaluation of stability of the data
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMid, y=round(5*cps,0), times=1)
  
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=1)
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(1*cps,0), times=1)
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(.5*cps,0), times=1)
  
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=2)
  
  
  
  # compute the distance between systolic and diastolic lines and the slow moving average
  # chartDF$c_CardioSystDiff <- chartDF$c_CardioSystolic - chartDF$c_CardioMA
  # chartDF$c_CardioDiastDiff <- chartDF$c_CardioMA - chartDF$c_CardioDiastolic
  
  # add the cardio pulse amplitude to the time series data frame
  # chartDF$c_CardioAmp <- chartDF$c_CardioSystolic - chartDF$c_CardioDiastolic
  
  return(chartDF)
  
} # end newCardioSigProcFn()




# MASmooth <- function(x=myData, y=round(.25*cps,0), times=5) {
#   # function to calculate a smoothed average of the time series data
#   # x input is a time series vector
#   # y input is the number of offset samples
#   # times is the number of times to smooth the data
#   ###
#   # make the output vector 
#   # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
#   xOut <- x
#   # loop over the number of times
#   for (j in 1:times) {
#     # for loop to compute the moving average
#     # buffer will be double the offset value + 1
#     input_buffer <- x[1:(2*y+1)]
#     # starts at sample y + 1
#     for (i in (y+1):(length(x)-y)) { 
#       # replace the middle value of the buffer with the mean
#       xOut[i] <- mean(input_buffer) 
#       # increment the input buffer
#       input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
#     } 
#     # replace the input vector
#     x <- xOut
#   } # end loop over times
#   return(xOut)
# } # end MASmooth function()
# 
# 
# 
# 
# fixPeak <- function(x=chartDF$c_Cardio1, times=4) {
#   # function to fix max and min peaks that are repeated 2 or more times
#   # input x is the time series data
#   # times is the number of times to repeat the operaation from 1 to 8
#   # this function will keep the first of all adjacent max and min peaks
#   # and interpolate the subsequent equal peak with the next sample
#   ###
#   if(length(which(is.na(x)))==length(x)) { return(x) }
#   #may need to re-write this with a while loop
#   for (j in 1:times) {
#     for (i in 9:(length(x)-9)) {
#       if(any(is.na(x[i:(i-8)]))) next()
#       if(x[i]==x[(i-1)]) { x[i] <- mean(c(x[(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-2)]) { x[i] <- mean(c(x[(i-2):(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-3)]) { x[i] <- mean(c(x[(i-3):(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-4)]) { x[i] <- mean(c(x[(i-4):(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-5)]) { x[i] <- mean(c(x[(i-5):(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-6)]) { x[i] <- mean(c(x[(i-6):(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-7)]) { x[i] <- mean(c(x[(i-7):(i-1)], x[(i+1)])) }
#       if(x[i]==x[(i-8)]) { x[i] <- mean(c(x[(i-8):(i-1)], x[(i+1)])) }
#     }
#   }
#   return(x)
# } # end fixPeak() function







