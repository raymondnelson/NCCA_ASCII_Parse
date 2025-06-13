# activity artifact extraction

activityArtifactFn <- function(x=chartDF) {
  
  chartDF <- x
  
  # get the activity sensor artifacts using the ocsillating peak points
  
  # get the max peaks for the activity sensor oscillation - similar to respiration
  maxPeaks <- maxPeak(x=chartDF$c_SE, y=40)
  
  # calculate the rate of the intervals
  maxRate <- c(diff(maxPeaks), mean(diff(maxPeaks))) / 30
  
  # # calculate the 3 cycle mean 
  # maxRateMean3 <- rep(mean(maxRate), length(maxRate)) 
  # for (l in 4:length(maxRate)) { maxRateMean3[l] <- mean(maxRate[(l-3):l]) }
  # # calculate the ratio of each cycle length to the 3 cycle mean length
  # maxRateRatio <- maxRate^1 / maxRateMean3^1
  # # get significant changes
  # maxRateChange <- maxPeaks[which(maxRateRatio <= .67 | maxRateRatio >= 1.5)]
  # chartDF$SEMax_a[maxRateChange] <- "ArtifactMaxRate"
  
  # chartDF$SEMax_a <- ""
  maxRateChange <- tukeyFence5(x=maxRate, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
  chartDF$SEMax_a[maxPeaks[!is.na(maxRateChange)]] <- "ArtifactMaxRate"
  which(chartDF$SEMax_a=="ArtifactMaxRate") 
  
  # next compute any signficant changes in amplitude for successive resp cycles
  # first get the difference between the peak amplitude and the moving average
  maxAmp <- chartDF$c_SE[maxPeaks] - chartDF$c_SEMA[maxPeaks]
  # # calculate the 3 cycle mean peak amplitude
  # maxAmpMean3 <- rep(mean(maxAmp), length(maxAmp))
  # for (m in 4:length(maxAmp)) { maxAmpMean3[m] <- mean(maxAmp[(m-2):m]) }
  # # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
  # maxAmpRatio <- (abs((maxAmp / maxAmpMean3))+0)^1
  # # maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
  # maxAmpChange <- maxPeaks[which(maxAmpRatio <= .67 | maxAmpRatio >= 1.5)]
  # chartDF$SEMax_a[maxAmpChange] <- "ArtifactMaxAmp"
  
  # chartDF$SEMax_a <- ""
  maxAmpChange <- tukeyFence5(x=maxAmp, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  chartDF$SEMax_a[maxPeaks[!is.na(maxAmpChange)]] <- "ArtifactMaxAmp"
  which(chartDF$SEMax_a=="ArtifactMaxAmp") 
  
  # get the min peaks for the activity sensor data
  minPeaks <- minPeak(x=chartDF$c_SE, y=40)
  # calculate the rate of oscillation
  minRate <- c(diff(minPeaks), mean(diff(minPeaks))) / 30
  
  # # calculate the 3 cycle mean
  # minRateMean3 <- rep(mean(minRate), length(minRate)) 
  # for (l in 3:length(minRate)) minRateMean3[l] <- mean(minRate[l:(l-2)]) 
  # # calculate the ratio of each cycle length to the 3 cycle mean length
  # minRateRatio <- minRate^1 / minRateMean3^1 
  # # get significant changes
  # minRateChange <- minPeaks[which(minRateRatio <= .67 | minRateRatio >= 1.5)]
  # # chartDF$SEMin_a[minRateChange] <- "ArtifactMinRate"
  
  # chartDF$SEMin_a <- ""
  minRateChange <- tukeyFence5(x=minRate, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
  chartDF$SEMin_a[minPeaks[!is.na(minRateChange)]] <- "ArtifactMinRate"
  which(chartDF$SEMin_a=="ArtifactMinRate") 
  
  # next compute any signficant changes in amplitude for successive resp cycles
  minAmp <- chartDF$c_SEMA[minPeaks] - chartDF$c_SE[minPeaks]
  # # calculate the 3 cycle mean peak amplitude
  # minAmpMean3 <- rep(mean(minAmp), length(minAmp))
  # for (m in 3:length(minAmp)) minAmpMean3[m] <- mean(minAmp[m:(m-2)])
  # # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
  # minAmpRatio <- minAmp^1 / minAmpMean3^1
  # # minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
  # minAmpChange <- minPeaks[which(minAmpRatio <= .67 | minAmpRatio >= 1.5)]
  # chartDF$SEMin_a[minAmpChange] <- "ArtifactMinAmp"
  
  # chartDF$SEMin_a <- ""
  minAmpChange <- tukeyFence5(x=minAmp, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
  chartDF$SEMin_a[minPeaks[!is.na(minAmpChange)]] <- "ArtifactMinAmp"
  which(chartDF$SEMin_a=="ArtifactMinAmp") 
  
  # min max activity sensor peaks
  minMaxPeak <- minMaxPeakFn(x=chartDF$c_SE, y=40)
  minMaxAmp <- abs(diff(chartDF$c_SE[minMaxPeak]))
  
  # chartDF$SEAmp_a <- ""
  minMaxChange <- tukeyFence5(x=minMaxAmp, buffer=6, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
  chartDF$SEMA_a[minMaxPeak[which(!is.na(minMaxChange))]]  <- "ArtifactMinMaxAmp"
  which(chartDF$SEMA_a=="ArtifactMinMaxAmp")
  

  return(chartDF)
  
}