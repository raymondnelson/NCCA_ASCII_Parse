# electronic cardio signal processing
# 9-16-2016
# Raymond Nelson
######################


# x=chartDF
# first=firstEvent
# last=lastEventEnd


eCardioSigProcFn <- function(x=chartDF, first=firstEvent, last=lastEventEnd) {
  
  chartDF <- x
  
  # reset the zero-centered e-Cardio data
   chartDF$c_eCardio <-  chartDF$eCardio - chartDF$eCardio[1]
  
  chartDF$c_eCardio <- 
    setColRange(x=chartDF$c_eCardio, y=colRange, firstRow=first, lastRow=last)
  
  # fix NA values
  chartDF$c_eCardio <- NAInterp(chartDF$c_eCardio)
  
  # first ensure that all peaks are recorded on a single sample
  # because there are rare times in which the time series reduction 
  # can result in a peak value over 2 adjacent samples
  # interpolate and eliminate duplicated peak points
  chartDF$c_eCardio <- fixPeak(x=chartDF$c_eCardio)
  
  # # scale the electronic cardio data 
  # chartDF$c_eCardio <- scaleDataFn(chartDF$c_eCardio, sec=5, times=40, ignore=1, xRange=scaleVals[4], maxY=165, minY=-165, firstRow=first, lastRow=last)
  # 
  # # offset the electronic cardio data
  # chartDF$c_eCardio <- offsetDataFn(x=chartDF$c_eCardio, y=yOffset[4], yMax=165, yMin=-165, firstRow=first, lastRow=last)
  # 
  # calculate the electronic cardio pulse rate
  cardioRate <- ratePerMin(chartDF$c_eCardio,buffer=3,peaks="upper",dataRate=cps,lowPass=TRUE)
  # cardioRate <- ratePerMin(lowPass1.7hz.2nd(chartDF$c_eCardio),buffer=3,peaks="upper",dataRate=cps)
  # ratePerMin(lowPass1.7hz.2nd(chartDF$c_eCardio),buffer=3,peaks="lower",dataRate=cps)
  
  # ts.plot(lowPass1.7hz.2nd(chartDF$c_eCardio)[1000:1500])
  # ts.plot(chartDF$c_eCardio[1000:1500])
  
  # calculate the buffer length for the cardio rate
  # bufferLen <- floor(1/cardioRate*60*cps*.60) - 1  # round down
  # bufferLen <- bufferLenFn(x = ratePerMin(lowPass1.7hz.2nd(chartDF$c_FC),buffer=3,peaks="upper",dataRate=cps))
  bufferLen <- bufferLenFn(cardioRate)
  
  #### systolic peaks
  
  # get the max peak row indices
  maxOut <- maxPeak(x=chartDF$c_eCardio, y=bufferLen)
  
  # ts.plot(lowPass2hz(chartDF$c_eCardio[1000:3000]))
  # ts.plot(chartDF$c_eCardio[1000:3000])
  
  # 1/60*60*30/2
  # 1/80*60*30/2
  # 1/100*60*30/2
  
  # get the max peak values
  maxVal <- chartDF$c_eCardio[maxOut]
  
  # interpolate between max peak values
  # systolicInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
  
  # plot.ts(systolicInterp, ylim=c(-3,10))
  # myCardioData2$CardioSyst <- systolicInterp
  # ts.plot(myCardioData2[1:3000,c(1,2,6)])
  
  # add the systolic time series to the data frame
  chartDF$c_eCardioSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
  
  #### diastolic peaks
  
  # use a function to get the min peak row indices
  minOut <- minPeak(x=chartDF$c_eCardio, y=bufferLen)
  # minOut <- minPeak(x=chartDF$c_eCardio, y=4)
  
  # get the min peak values for the min peak rows
  minVal <- chartDF$c_eCardio[na.omit(minOut)]
  
  # interpolate between the min peak values
  # diastolicInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
  
  # a test plot 
  # plot.ts(diastolicInterp, ylim=c(-3,10))
  
  # add the vector to the diastolic cardio column 
  chartDF$c_eCardioDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]
  
  # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
  # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
  
  ####
  
  # compute a min-Max of the cardio time series data
  
  # get the minMax row indices
  minMaxOut <- minMaxPeakFn(x=chartDF$c_eCardio, y=bufferLen)
  
  # get the minMax values
  minMaxVal <- chartDF$c_eCardio[minMaxOut]
  
  # interpolate the minMax values
  minMaxInterp <- na.omit(c(interpolatePeaks(x=minMaxOut, y=minMaxVal), 0))[1:nrow(chartDF)]
  
  # plot.ts(minMaxInterp, ylim=c(-3,10))
  
  # add the time series to the data frame
  chartDF$c_eCardioMinMax <- minMaxInterp
  
  #### 
  
  # compute the smoothed cardio dta
  # smoothedCardio <- MASmooth(x=chartDF$c_eCardio, y=round(.5*cps,0), times=3)
  
  # add the smoothed cardio to the time series data frame
  # chartDF$c_eCardioMid <- smoothedCardio[1:nrow(chartDF)]
  chartDF$c_eCardioMid <- MASmooth(x=chartDF$c_eCardio, y=round(.5*cps,0), times=3)
  
  # a test plot
  # plot.ts(chartDF$c_eCardioMid[1:3000], ylim=c(-3,10))
  # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
  # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
  
  # compute a more stable moving average for the cardio data
  # to help with evaluation of stability of the data
  chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardio, y=round(5*cps,0), times=3)
  # chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardioMid, y=round(5*cps,0), times=1)
  
  # compute the distance between systolic and diastolic lines and the slow moving average
  chartDF$c_eCardioSystDiff <- chartDF$c_eCardioSystolic - chartDF$c_eCardioMA
  chartDF$c_eCardioDiastDiff <- chartDF$c_eCardioMA - chartDF$c_eCardioDiastolic
  
  # add the cardio pulse amplitude to the time series data frame
  chartDF$c_eCardioAmp <- chartDF$c_eCardioSystolic - chartDF$c_eCardioDiastolic
  
  return(chartDF)
  
} # end eCardioSigProcFn()



