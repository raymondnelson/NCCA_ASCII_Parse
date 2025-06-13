# pneumo artifact extraction

pneumoArtifactFn <- function(x=chartDF) {
  
  #################################
  
  # get the pneumo artifacts using the peak points
  
  numCycles <-2
  IQRMult <- 1
  rateRatio <- 2
  multRatio <- 2
  
  #### upper pneumo
  
  # upper respiration rate
  
  # get the peak points for the stimulus segment
  maxPeaks <- maxPeak(x=chartDF$c_UPneumo, y=round(.25*cps,0))
  # then compute the resp rate in seconds using the peaks
  maxPeakRate <- c(mean(diff(maxPeaks)), diff(maxPeaks)) / cps
  # initialize the multi cycle mean vector
  maxPeakRateMean <- rep(mean(maxPeakRate), length(maxPeakRate)) 
  # calculate the multi cycle median rate
  for (l in (numCycles+1):length(maxPeakRate)) {
    maxPeakRateMean[l] <- median(maxPeakRate[(l-numCycles):(l-1)])
  }
  # calculate the ratio of each cycle length to the multi cycle mean length
  maxPeakRateRatio <- maxPeakRate / maxPeakRateMean 
  # get significant changes
  maxPeakRateChange <- maxPeaks[which( maxPeakRateRatio <= (1/rateRatio) | maxPeakRateRatio >= (2*rateRatio) )]
  
  # upper respiration amplitude
  
  # next compute any signficant changes in amplitude for successive resp cycles
  # maxAmp <- chartDF$c_UPneumo[maxPeaks] - chartDF$c_UPneumoMid[maxPeaks]
  maxAmp <- chartDF$c_UPneumo[maxPeaks]
  midAmp <- chartDF$c_UPneumoMid[maxPeaks]
  
  ## Tukey fences - upper max ammplitude
  
  # compute the multi cycle interquantile range for upper respiration amplitude
  maxAmpQ25 <- rep(quantile(c(chartDF$c_UPneumo[maxPeaks], chartDF$c_UPneumoMid[maxPeaks]), .25), length(maxPeaks))
  maxAmpQ75 <- rep(quantile(c(chartDF$c_UPneumo[maxPeaks], chartDF$c_UPneumoMid[maxPeaks]), .75), length(maxPeaks))        
  #         maxAmpQ25 <- rep(quantile(maxAmp, .25), length(maxAmp))
  #         maxAmpQ75 <- rep(quantile(maxAmp, .75), length(maxAmp))        
  for (l in (numCycles+1):length(maxAmpQ25)) { maxAmpQ25[l] <- quantile(c(maxAmp[(l-numCycles):(l-1)],midAmp[(l-numCycles):(l-1)]), .25) }
  for (l in (numCycles+1):length(maxAmpQ75)) { maxAmpQ75[l] <- quantile(c(maxAmp[(l-numCycles):(l-1)],midAmp[(l-numCycles):(l-1)]), .75) }
  # then the upper and lower Tukey fences for all peaks
  upperFence <- maxAmpQ75 + (IQRMult * (maxAmpQ75-maxAmpQ25) * 1.5)
  lowerFence <- maxAmpQ25 - (IQRMult * (maxAmpQ75-maxAmpQ25))
  # finally, locate the row index of any outliers that are outside the Tukey fences
  maxAmpChange <- maxPeaks[which(maxAmp > upperFence | maxAmp < lowerFence)]
  
  ##
  
  #         # initialize the multi cycle mean vector
  #         maxAmpMean <- rep(mean(maxAmp), length(maxAmp))
  #         # calculate the multi cycle mean peak amplitude
  #         for (m in (numCycles+1):length(maxAmp)) maxAmpMean[m] <- median(maxAmp[(m-numCycles):(m-1)])
  #         # calculate the ratio of each cycle amplitude to the mean of the previous cycles
  #         maxAmpRatio <- maxAmp / maxAmpMean
  #         # maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
  #         maxAmpChange <- maxPeaks[which(maxAmpRatio <= (1/rateRatio) | maxAmpRatio >= rateRatio)]
  
  # upper respiration rate using minPeaks
  
  # get the min peaks
  minPeaks <- minPeak(x=chartDF$c_UPneumo, y=round(.25*cps,0))
  # then get the upper pnuemo rate in seconds using the min Peaks
  minPeakRate <- c(mean(diff(minPeaks)), diff(minPeaks)) / cps
  # calculate the multi cycle median rate
  minPeakRateMean <- rep(mean(minPeakRate), length(minPeakRate)) 
  for (l in (numCycles+1):length(minPeakRate)) minPeakRateMean[l] <- median(minPeakRate[(l-numCycles):(l-1)]) 
  # calculate the ratio of each cycle length to the multi cycle mean length
  minPeakRateRatio <- minPeakRate / minPeakRateMean 
  # get significant changes
  minPeakRateChange <- minPeaks[which(minPeakRateRatio <= (1/rateRatio) | minPeakRateRatio >= (2*rateRatio))]
  
  # upper baseline 
  
  # next compute any signficant changes in baseline for successive resp cycles
  #         minAmp <- chartDF$c_UPneumoMid[minPeaks] - chartDF$c_UPneumo[minPeaks]
  minAmp <- chartDF$c_UPneumo[minPeaks]
  midAmp <- chartDF$c_UPneumoMid[minPeaks]
  
  ## Tukey fences - upper min amplitude
  
  # compute the multi cycle interquantile range
  minAmpQ25 <- rep(quantile(c(chartDF$c_UPneumo[minPeaks], chartDF$c_UPneumoMid[minPeaks]), .25), length(minPeaks))
  minAmpQ75 <- rep(quantile(c(chartDF$c_UPneumo[minPeaks], chartDF$c_UPneumoMid[minPeaks]), .75), length(minPeaks))
  for (l in (numCycles+1):length(minAmpQ25)) { minAmpQ25[l] <- quantile(c(minAmp[(l-numCycles):(l-1)],midAmp[(l-numCycles):(l-1)]), .25) }
  for (l in (numCycles+1):length(minAmpQ75)) { minAmpQ75[l] <- quantile(c(minAmp[(l-numCycles):(l-1)],midAmp[(l-numCycles):(l-1)]), .75) }
  # then the upper and lower Tukey fences for all peaks
  upperFence <- minAmpQ75 + (IQRMult * (minAmpQ75-minAmpQ25))
  lowerFence <- minAmpQ25 - (IQRMult * (minAmpQ75-minAmpQ25) * 1.5)
  # finally, locate the row index of any outliers that are outside the Tukey fences
  minAmpChange <- minPeaks[which(minAmp > upperFence | minAmp < lowerFence)]
  
  ##
  
  #         # calculate the multi cycle mean peak amplitude
  #         minAmpMean <- rep(mean(minAmp), length(minAmp))
  #         for (m in (numCycles+1):length(minAmp)) minAmpMean[m] <- median(minAmp[(m-numCycles):(m-1)])
  #         # calculate the ratio of each cycle amplitude to the mean of the previous cycles
  #         # minAmpRatio <- minAmp / minAmpMean
  #         minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
  #         minAmpChange <- minPeaks[which(minAmpRatio <= (1/multRatio) | minAmpRatio >= multRatio)]
  #         # was .67 1.5 #changed 2-11-2016
  
  # add the pneumo artifacts to the data frame
  chartDF$UPneumoMid_a[maxPeakRateChange] <- "Artifact"
  chartDF$UPneumoInh_a[maxAmpChange] <- "Artifact"
  chartDF$UPneumoMid_a[minPeakRateChange] <- "Artifact"
  chartDF$UPneumoExh_a[minAmpChange] <- "Artifact"
  chartDF$UPneumo_a[maxPeakRateChange] <- "Artifact"
  chartDF$UPneumo_a[maxAmpChange] <- "Artifact"
  chartDF$UPneumo_a[minPeakRateChange] <- "Artifact"
  chartDF$UPneumo_a[minAmpChange] <- "Artifact"
  
  ######## lower pneumo
  
  # lower respiration rate
  
  # get the peak points for segment
  maxPeaks <- maxPeak(x=chartDF$c_LPneumo, y=round(.25*cps,0))
  # then compute the resp rate in seconds using the peaks
  maxPeakRate <- c(mean(diff(maxPeaks)), diff(maxPeaks)) / cps
  # calculate the multi cycle median rate
  maxPeakRateMean <- rep(mean(maxPeakRate), length(maxPeakRate)) 
  for (l in (numCycles+1):length(maxPeakRate)) maxPeakRateMean[l] <- median(maxPeakRate[(l-numCycles):(l-1)]) 
  # calculate the ratio of each cycle length to the multi cycle mean length
  maxPeakRateRatio <- maxPeakRate / maxPeakRateMean 
  # get significant changes
  maxPeakRateChange <- maxPeaks[which(maxPeakRateRatio <= (1/rateRatio) | maxPeakRateRatio >= (2*rateRatio))]
  
  # lower respiration amplitude
  
  # next compute any signficant changes in amplitude for successive resp cycles
  maxAmp <- chartDF$c_LPneumo[maxPeaks] - chartDF$c_LPneumoMid[maxPeaks]
  # calculate the multi cycle mean peak amplitude
  maxAmpMean <- rep(mean(maxAmp), length(maxAmp))
  for (m in (numCycles+1):length(maxAmp)) maxAmpMean[m] <- median(maxAmp[(m-numCycles):(m-1)])
  # calculate the ratio of each cycle amplitude to the mean of the previous cycles
  maxAmpRatio <- maxAmp / maxAmpMean
  # maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
  maxAmpChange <- maxPeaks[which(maxAmpRatio <= (1/multRatio) | maxAmpRatio >= multRatio)]
  
  # lower respiration rate using minPeaks
  
  minPeaks <- minPeak(x=chartDF$c_LPneumo, y=round(.25*cps,0))
  # then get the lower pnuemo rate in seconds using the min Peaks
  minPeakRate <- c(mean(diff(minPeaks)), diff(minPeaks)) / cps
  # calculate the multi cycle median rate
  minPeakRateMean <- rep(mean(minPeakRate), length(minPeakRate)) 
  # calculate the ratio of each cycle length to the multi cycle medeian length
  for (l in (numCycles+1):length(minPeakRate)) minPeakRateMean[l] <- median(minPeakRate[(l-numCycles):(l-1)]) 
  minPeakRateRatio <- minPeakRate / minPeakRateMean
  # get significant changes
  minPeakRateChange <- minPeaks[which(minPeakRateRatio <= (1/rateRatio) | minPeakRateRatio >= (2*rateRatio))]
  
  # lower baseline 
  
  # next compute any signficant changes in baseline for successive resp cycles
  minAmp <- chartDF$c_LPneumoMid[minPeaks] - chartDF$c_LPneumo[minPeaks]
  # calculate the multi cycle mean peak amplitude
  minAmpMean <- rep(mean(minAmp), length(minAmp))
  for (m in (numCycles+1):length(minAmp)) minAmpMean[m] <- median(minAmp[(m-numCycles):m])
  # calculate the ratio of each cycle amplitude to the mean of the previous cycles
  # minAmpRatio <- minAmp / minAmpMean
  minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
  minAmpChange <- minPeaks[which(minAmpRatio <= (1/multRatio) | minAmpRatio >= multRatio)]
  # was .67 1.5 #changed 2-11-2016
  
  # add the pneumo artifacts to the data frame
  chartDF$LPneumoMid_a[maxPeakRateChange] <- "Artifact"
  chartDF$LPneumoInh_a[maxAmpChange] <- "Artifact"
  chartDF$LPneumoMid_a[minPeakRateChange] <- "Artifact"
  chartDF$LPneumoExh_a[minAmpChange] <- "Artifact"
  chartDF$LPneumo_a[maxPeakRateChange] <- "Artifact"
  chartDF$LPneumo_a[maxAmpChange] <- "Artifact"
  chartDF$LPneumo_a[minPeakRateChange] <- "Artifact"
  chartDF$LPneumo_a[minAmpChange] <- "Artifact"
  
  ####################################
  
  #         # get the pneumo artifacts using Tukey Fences 
  #         
  #         # pneumoAU <- sigChange(x=chartDF$c_UPneumo)
  #         pneumoAU_Inh <- tukeyFence1(x1=chartDF$c_UPneumoInh, x2=chartDF$c_UPneumoMid, inner=3)
  #         # which(pneumoAU_Inh=="X")
  #         pneumoAU_Exh <- tukeyFence1(x1=chartDF$c_UPneumoMid, x2=chartDF$c_UPneumoExh, inner=3)
  #         # which(pneumoAU_Exh=="X")
  #         # print(pneumoAU)
  #         pneumoAL_Inh <- tukeyFence1(x1=chartDF$c_LPneumoInh, x2=chartDF$c_LPneumoMid, inner=3)
  #         # which(pneumoAL_Inh=="X")
  #         pneumoAL_Exh <- tukeyFence1(x1=chartDF$c_LPneumoMid, x2=chartDF$c_LPneumoExh, inner=3)
  #         # which(pneumoAL_Exh=="X")
  #         # pneumoAL <- sigChange(x=chartDF$c_LPneumo)
  #         # print(pneumoAL)
  
  #         # add all the artifacts to both pneumo channels in the data frame - 
  #         chartDF$UPneumoInh_a[which(pneumoAU_Inh=="X")] <- "Artifact"
  #         chartDF$UPneumoExh_a[which(pneumoAU_Exh=="X")] <- "Artifact"
  #         chartDF$LPneumoInh_a[which(pneumoAL_Inh=="X")] <- "Artifact"
  #         chartDF$LPneumoExh_a[which(pneumoAL_Exh=="X")] <- "Artifact"
  
  return(chartDF)
  
}


