# PLE artifact extraction

pleArtifactFn <- function(x=chartDF) {
  
  chartDF <- x

  if(sum(pmatch(names(chartDF), "c_PLE1", nomatch=0)) != 0) {
    
    # none yet 4/23/2016 rn
    
    #   #### calculate the cardio rate
    # 
    # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=1)
    # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(.5*cps,0), times=3)
    # 
    # # calculate the cardio diastolic and systolic lines
    # 
    # cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",dataRate=cps,lowPass=TRUE)
    # bufferLen <- bufferLenFn(cardioRate)
    # 
    # maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
    # chartDF$c_CardioSystolic <- interpolatePeaks(x=maxOut, y=chartDF$c_Cardio1[maxOut])[1:nrow(chartDF)]
    # 
    # minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
    # chartDF$c_CardioDiastolic <- interpolatePeaks(x=minOut, y=chartDF$c_Cardio1[minOut])[1:nrow(chartDF)]
    # 
    # cardioRate <- ratePerMin(x=chartDF$c_Cardio1, buffer=3, peaks="upper",dataRate=cps, lowPass=TRUE)
    # 
    # # calculate the buffer length for the cardio rate
    # # bufferLen <- floor(1/cardioRate*60*cps*.6) -1  # round down
    # bufferLen <- bufferLenFn(cardioRate)
    # 
    # ### systolic peaks
    # maxPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
    # 
    # ## calculate the systolic beat to beat intervals
    # maxBtoB <- diff(maxPeaks)
    # # calculate the rate per min for the maxBtoB vector
    # maxRate <- 60 / (c(mean(maxBtoB), maxBtoB) / cps)
    # 
    # # chartDF$CardioSystolic_a <- ""
    # maxBtoBChange <- tukeyFence5(x=maxRate, buffer=round(.333*cps,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
    # chartDF$CardioSystolic_a[maxPeaks[which(!is.na(maxBtoBChange))]]  <- "ArtifactMaxBtoB"
    # which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB")
    # 
    # # new 5-12-2016 compute significant changes in sistolic amplitude
    # # chartDF$CardioSystolic_a <- ""
    # systInputVector <- chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks]
    # maxAmpChange <- tukeyFence5(x=systInputVector, buffer=6, side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=2, scaleVal=1)
    # chartDF$CardioSystolic_a[maxPeaks[which(!is.na(maxAmpChange))]]  <- "ArtifactMaxAmp"
    # which(chartDF$CardioSystolic_a=="ArtifactMaxAmp")
    # 
    # ### diastolic peaks
    # minPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
    # 
    # ## calculate the diastolic beat to beat intervals
    # minBtoB <- diff(minPeaks)
    # # calculate the rate per min for the maxBtoB vector
    # minRate <- 60 / (c(mean(minBtoB), minBtoB) / cps)
    # 
    # chartDF$CardioDiastolic_a <- ""
    # minBtoBChange <- tukeyFence5(x=minRate, buffer=round(.333*cps,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=3, scaleVal=1)
    # chartDF$CardioDiastolic_a[maxPeaks[which(!is.na(minBtoBChange))]]  <- "ArtifactMinBtoB"
    # which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB")
    # 
    # # new 5-18-2016 compute significant changes in diastolic amplitude
    # diastInputVector <- chartDF$c_CardioMid[minPeaks] - chartDF$c_Cardio1[minPeaks]
    # minAmpChange <- tukeyFence5(x=diastInputVector, buffer=2, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=3, scaleVal=1)
    # chartDF$CardioDiastolic_a[minPeaks[which(!is.na(minAmpChange))]]  <- "ArtifactMinAmp"
    # 
    # # systolic and diastolic peaks
    # minMaxPeak <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
    # ### pulse amplitude changes
    # minMaxAmp <- abs(diff(chartDF$c_Cardio1[minMaxPeak]))
    # # minMaxAmp <- c(minMaxAmp, mean(minMaxAmp))
    # 
    # # new 5-18-2016 compute significant changes in sistolic to diastolic amplitude
    # # minMaxAmp
    # # minMaxAmp <- chartDF$c_CardioMid[maxPeaks] - chartDF$c_Cardio1[minPeaks]
    # minMaxChange <- tukeyFence5(x=minMaxAmp, buffer=2, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=2, scaleVal=1)
    # # chartDF$CardioMid_a[minMaxPeak[which(!is.na(minMaxChange))]]  <- "ArtifactMinMaxAmp"
    # 
    # ### changes in the slow cardioMA
    # MAChange <- NULL
    # # systolic Y value is less than the cardioMA
    # MAChange <- c(MAChange, which(chartDF$c_CardioSystolic <= chartDF$c_CardioMA))
    # # diastolic Y value is greater than the cardioMA
    # MAChange <- c(MAChange, which(chartDF$c_CardioDiastolic >= chartDF$c_CardioMA))
    # MAChange <- sort(MAChange)
    # chartDF$CardioMA_a[MAChange] <- "ArtifactMA"
    
  }
  
  return(chartDF)
   
}

