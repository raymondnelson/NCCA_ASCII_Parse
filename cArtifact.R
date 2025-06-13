# cardio artifact extraction

cArtifactFn <- function(x=chartDF) {
  
  chartDF <- x
  
  #### calculate the cardio rate
  
  cardioRate <- ratePerMin(x=chartDF$c_Cardio1,buffer=3,peaks="upper",dataRate=cps,lowPass=TRUE)
  
  # calculate the buffer length for the cardio rate
  # bufferLen <- floor(1/cardioRate*60*cps*.6) -1  # round down
  bufferLen <- bufferLenFn(cardioRate)
  
  ###### get the cardio sensor artifacts using the ocsillating peak points
  
  ### systolic peaks
  maxPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
  
  ## calculate the systolic beat to beat intervals
  # maxRate <- 60/ (c(mean(diff(maxPeaks)), diff(maxPeaks)) / cps)
  # ## calculate the 3 cycle mean rate
  # maxRateMean3 <- rep(mean(maxRate), length(maxRate)) 
  # for (l in 4:length(maxRate)) { maxRateMean3[l] <- median(maxRate[(l-3):(l-1)]) }
  # # calculate the ratio of each cycle length to the 3 cycle mean length
  # maxRateRatio <- maxRate / maxRateMean3 
  # # get significant changes
  # maxRateChange <- maxPeaks[which(maxRateRatio <= .57 | maxRateRatio >= 1.75)]
  # chartDF$CardioSystolic_a[maxRateChange] <- "Artifact"
  
  ## compute any signficant changes in amplitude for successive resp cycles
  maxAmp <- chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks]
  # calculate the 3 cycle mean peak amplitude
  maxAmpMean3 <- rep(mean(maxAmp), length(maxAmp))
  for (m in 4:length(maxAmp)) { maxAmpMean3[m] <- median(maxAmp[(m-3):(m-1)]) }
  # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
  maxAmpRatio <- maxAmp / maxAmpMean3
  # calculate any significant change
  maxAmpChange <- maxPeaks[which(maxAmpRatio <= .67 | maxAmpRatio >= 1.5)]
  chartDF$CardioSystolic_a[maxAmpChange] <- "Artifact"
  
  ### diastolic peaks
  minPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
  
  ## calculate the diastolic beat to beat intervals
  # minRate <- 60/ (c(mean(diff(minPeaks)), diff(minPeaks)) / cps)
  # ## calculate the 3 cycle mean rate
  # minRateMean3 <- rep(mean(minRate), length(minRate)) 
  # for (l in 3:length(minRate)) { minRateMean3[l] <- median(minRate[(l-3):(l-1)]) }
  # # calculate the ratio of each cycle length to the 3 cycle mean length
  # minRateRatio <- minRate / minRateMean3 
  # # get significant changes
  # minRateChange <- minPeaks[which(minRateRatio <= .5 | minRateRatio >= 2)]
  # chartDF$CardioDiastolic_a[minRateChange] <- "Artifact"
  
  ## compute any signficant changes in amplitude for successive resp cycles
  minAmp <- chartDF$c_CardioMid[minPeaks] - chartDF$c_Cardio1[minPeaks]
  # calculate the 3 cycle mean peak amplitude
  minAmpMean3 <- rep(mean(minAmp), length(minAmp))
  for (m in 4:length(minAmp)) { minAmpMean3[m] <- mean(minAmp[(m-3):(m-1)]) }
  # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
  minAmpRatio <- minAmp / minAmpMean3
  # minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
  # calculate any significant change
  minAmpChange <- minPeaks[which(minAmpRatio <= .67 | minAmpRatio >= 1.5)]
  chartDF$CardioDiastolic_a[minAmpChange] <- "Artifact"
  
  ### pulse amplitude changes
  minMaxPeak <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
  minMaxAmp <- abs(diff(chartDF$c_Cardio1[minMaxPeak]))
  minMaxAmpMean3 <- rep(mean(minMaxAmp), length(minMaxAmp))
  for (n in 4:length(minMaxAmp)) { minMaxAmpMean3[n] <- mean(minMaxAmp[(n-3:(n-1))]) }
  # minMaxRatio <- c(minMaxAmp[2:length(minMaxAmp)] / minMaxAmp[1:(length(minMaxAmp)-1)])
  minMaxRatio <- minMaxAmp / minMaxAmpMean3
  # calculate any significant change
  minMaxChange <- minMaxPeak[which( minMaxRatio <= .5 | minMaxRatio >= 2) ]
  chartDF$Cardio1_a[minMaxChange] <- "Artifact"

  ### changes in the slow cardioMA
  MAChange <- NULL
  # systolic Y value is less than the cardioMA
  MAChange <- c(MAChange, which(chartDF$c_CardioSystolic <= chartDF$c_CardioMA))
  # diastolic Y value is greater than the cardioMA
  MAChange <- c(MAChange, which(chartDF$c_CardioDiastolic >= chartDF$c_CardioMA))
  MAChange <- sort(MAChange)
  chartDF$CardioMA_a[MAChange] <- "Artifact"
  
  # check the artifacts
  # which(chartDF$CardioMA_a=="Artifact")
  # which(chartDF$CardioMid_a=="Artifact")
  # which(chartDF$Cardio1_a=="Artifact")
  # which(chartDF$CardioDiastolic_a=="Artifact")
  # which(chartDF$CardioSystolic_a=="Artifact")
  # which(chartDF$CardioAmp_a=="Artifact")
  
  return(chartDF)
  
} # end cArtifactFn()





