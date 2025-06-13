# cardio artifact extraction

cardioArtifactFn <- function(x=chartDF) {
  
  chartDF <- x
  
  #### calculate the cardio rate
  
  cardioRate <- ratePerMin(x=chartDF$c_Cardio1, buffer=3, peaks="upper",dataRate=cps, lowPass=TRUE)

  # calculate the buffer length for the cardio rate
  # bufferLen <- floor(1/cardioRate*60*cps*.6) -1  # round down
  bufferLen <- bufferLenFn(cardioRate)
  
  ### systolic peaks
  maxPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
  
  ## calculate the systolic beat to beat intervals
  maxBtoB <- diff(maxPeaks)
  # calculate the rate per min for the maxBtoB vector
  maxRate <- 60 / (c(maxBtoB, mean(maxBtoB)) / cps)
  # # calculate the 3 cycle mean rate
  # maxRateMean3 <- rep(mean(maxRate), length(maxRate))
  # for (l in 11:length(maxRate)) { maxRateMean3[l] <- median(maxRate[(l-10):(l-1)]) }
  # # calculate the ratio of each cycle length to the 3 cycle mean length
  # maxRateRatio <- maxRate / maxRateMean3
  # # get significant changes
  # maxRateChange <- maxPeaks[which(maxRateRatio <= .57 | maxRateRatio >= 1.75)]
  # # chartDF$CardioSystolic_a[maxRateChange] <- "Artifact"
  
  # chartDF$CardioSystolic_a <- ""
  maxBtoBChange <- tukeyFence5(x=maxRate, buffer=10, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
  chartDF$CardioSystolic_a[maxPeaks[which(!is.na(maxBtoBChange))]]  <- "ArtifactMaxBtoB"
  which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB")
  
  # ## compute any signficant changes in amplitude for successive resp cycles
  # maxAmp <- chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks]
  # # calculate the 3 cycle mean peak amplitude
  # maxAmpMean3 <- rep(mean(maxAmp), length(maxAmp))
  # for (m in 4:length(maxAmp)) { maxAmpMean3[m] <- median(maxAmp[(m-3):(m-1)]) }
  # # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
  # maxAmpRatio <- maxAmp / maxAmpMean3
  # # calculate any significant change
  # maxAmpChange <- maxPeaks[which(maxAmpRatio <= .67 | maxAmpRatio >= 1.5)]
  # # chartDF$CardioSystolic_a[maxAmpChange] <- "Artifact"
  
  # new 5-12-2016 compute significant changes in sistolic amplitude
  # chartDF$CardioSystolic_a <- ""
  systInputVector <- chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks]
  maxAmpChange <- tukeyFence5(x=systInputVector, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
  chartDF$CardioSystolic_a[maxPeaks[which(!is.na(maxAmpChange))]]  <- "ArtifactMaxAmp"
  which(chartDF$CardioSystolic_a=="ArtifactMaxAmp")
  
  ### diastolic peaks
  minPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
  
  ## calculate the diastolic beat to beat intervals
  minBtoB <- diff(minPeaks)
  # calculate the rate per min for the maxBtoB vector
  minRate <- 60 / (c(minBtoB, mean(minBtoB)) / cps)
  # # calculate the 3 cycle mean rate
  # minRateMean3 <- rep(mean(minRate), length(minRate))
  # for (l in 11:length(minRate)) { minRateMean3[l] <- median(minRate[(l-10):(l-1)]) }
  # # calculate the ratio of each cycle length to the 3 cycle mean length
  # minRateRatio <- minRate / minRateMean3
  # # get significant changes
  # minRateChange <- minPeaks[which(minRateRatio <= .57 | minRateRatio >= 1.75)]
  # # chartDF$CardioDiastolic_a[minRateChange] <- "Artifact"
  
  # chartDF$CardioDiastolic_a <- ""
  minBtoBChange <- tukeyFence5(x=minRate, buffer=10, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=10, scaleVal=1)
  chartDF$CardioDiastolic_a[maxPeaks[which(!is.na(minBtoBChange))]]  <- "ArtifactMinBtoB"
  which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB")
  
  # ## compute any signficant changes in amplitude for successive resp cycles
  # minAmp <- chartDF$c_CardioMid[minPeaks] - chartDF$c_Cardio1[minPeaks]
  # # calculate the 3 cycle mean peak amplitude
  # minAmpMean3 <- rep(mean(minAmp), length(minAmp))
  # for (m in 4:length(minAmp)) { minAmpMean3[m] <- mean(minAmp[(m-3):(m-1)]) }
  # # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
  # minAmpRatio <- minAmp / minAmpMean3
  # # minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
  # # calculate any significant change
  # minAmpChange <- minPeaks[which(minAmpRatio <= .67 | minAmpRatio >= 1.5)]
  # # chartDF$CardioDiastolic_a[minAmpChange] <- "Artifact"
  
  # new 5-18-2016 compute significant changes in diastolic amplitude
  diastInputVector <- chartDF$c_CardioMid[minPeaks] - chartDF$c_Cardio1[minPeaks]
  minAmpChange <- tukeyFence5(x=diastInputVector, buffer=6, side="lower", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  chartDF$CardioDiastolic_a[minPeaks[which(!is.na(minAmpChange))]]  <- "ArtifactMinAmp"
  
  # systolic and diastolic peaks
  minMaxPeak <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
  ### pulse amplitude changes
  minMaxAmp <- abs(diff(chartDF$c_Cardio1[minMaxPeak]))
  # minMaxAmp <- c(minMaxAmp, mean(minMaxAmp))
  
  # minMaxAmpMean3 <- rep(mean(minMaxAmp), length(minMaxAmp))
  # for (n in 4:length(minMaxAmp)) { minMaxAmpMean3[n] <- mean(minMaxAmp[(n-3:(n-1))]) }
  # # minMaxRatio <- c(minMaxAmp[2:length(minMaxAmp)] / minMaxAmp[1:(length(minMaxAmp)-1)])
  # minMaxRatio <- minMaxAmp / minMaxAmpMean3
  # # calculate any significant change
  # minMaxChange <- minMaxPeak[which( minMaxRatio <= .5 | minMaxRatio >= 2) ]
  # # chartDF$Cardio1_a[minMaxChange] <- "Artifact"

  # new 5-18-2016 compute significant changes in sistolic to diastolic amplitude
  # minMaxAmp
  # minMaxAmp <- chartDF$c_CardioMid[maxPeaks] - chartDF$c_Cardio1[minPeaks]
  minMaxChange <- tukeyFence5(x=minMaxAmp, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  chartDF$CardioMid_a[minMaxPeak[which(!is.na(minMaxChange))]]  <- "ArtifactMinMaxAmp"
  
  ### changes in the slow cardioMA
  MAChange <- NULL
  # systolic Y value is less than the cardioMA
  MAChange <- c(MAChange, which(chartDF$c_CardioSystolic <= chartDF$c_CardioMA))
  # diastolic Y value is greater than the cardioMA
  MAChange <- c(MAChange, which(chartDF$c_CardioDiastolic >= chartDF$c_CardioMA))
  MAChange <- sort(MAChange)
  chartDF$CardioMA_a[MAChange] <- "ArtifactMA"
  
  # check the artifacts
  # which(chartDF$CardioMA_a=="Artifact")
  # which(chartDF$CardioMid_a=="Artifact")
  # which(chartDF$Cardio1_a=="Artifact")
  # which(chartDF$CardioDiastolic_a=="Artifact")
  # which(chartDF$CardioSystolic_a=="Artifact")
  # which(chartDF$CardioAmp_a=="Artifact")
  
  return(chartDF)
  
} # end cArtifactFn()





