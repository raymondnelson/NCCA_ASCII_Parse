# activity artifact extraction


source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)


activityArtifactFn <- function(x=chartDF) {
  
  chartDF <- x
  
  # only if the Move1 sensor is available
  if(sum(pmatch(names(chartDF), "c_Move1", nomatch=0))!=0) {
    
    # only if there is activity in the activity sensor data
    if(max(chartDF$c_Move1) != min(chartDF$c_Move1)) {
      
      #### process the activity data ####
      
      # chartDF$c_Move1Proc <- MASmoothB(x=chartDF$c_Move1, y=round(.5*cps,0), times=1)
      # chartDF$c_Move1ProcMA <-  MASmooth(x=chartDF$c_Move1Proc, y=round(5*cps,0), times=3)
      
      # was .75 6-15-2016
      # bufferLen <- bufferLenFn(ratePerMin(chartDF$c_Move1Proc, round(1*cps,0))) 
      # chartDF$c_Move1Min <- interpolatePeaks(x=minPeak(x=chartDF$c_Move1Proc, y=bufferLen), y=chartDF$c_Move1Proc[minPeak(x=chartDF$c_Move1Proc, y=bufferLen)])
      # chartDF$c_Move1Max <- interpolatePeaks(x=maxPeak(x=chartDF$c_Move1Proc, y=bufferLen), y=chartDF$c_Move1Proc[maxPeak(x=chartDF$c_Move1Proc, y=bufferLen)])
      
      # chartDF$c_Move1ProcMA <- bandPass.2.3.2nd(chartDF$c_Move1Proc)
      # source('~/Dropbox/R/NCCA_ASCII_Parse/bandPass.3rdOrder5hz6hz.R')
      # chartDF$c_Move1ProcMA <- bandPass.3rd((chartDF$c_Move1Proc - chartDF$c_Move1Proc[1]))
      # chartDF$c_Move1ProcMA <- (chartDF$c_Move1ProcMA + chartDF$c_Move1Proc[1])
      
      # source('~/Dropbox/R/NCCA_ASCII_Parse/splitHalfTukey.R')
      # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
      # source('~/Dropbox/R/NCCA_ASCII_Parse/TukeyFences.R')
      
      ########### activity rate ##############
      
      # get the max peaks for the activity sensor oscillation
      maxPeaks <- maxPeak(x=chartDF$c_Move1Proc, y=round(.5*cps,0))
      # chartDF$c_Move1Max <- interpolatePeaks(maxPeaks, chartDF$c_Move1Proc[maxPeaks])
      # remove peak points accross consecutive samples
      consecPeaks <- which((maxPeaks[2:length(maxPeaks)]-1)==maxPeaks[1:(length(maxPeaks)-1)])
      if(length(consecPeaks > 0)) maxPeaks <- maxPeaks[-consecPeaks]
      
      # calculate the rate of the intervals
      maxRate <- c(mean(diff(maxPeaks)), diff(maxPeaks)) / round(1*cps,0)
      
      # compute the amplitude from the tracing average to the upper peak points
      maxAmp <- chartDF$c_Move1Proc[maxPeaks] - chartDF$c_Move1MA[maxPeaks]
      
      chartDF$SEMax_a <- 0
      
      maxRateChange <- tukeyFence7(x=maxRate, buffer=3, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=3, scaleVal=1)
      # chartDF$SEMax_a[maxPeaks[!is.na(maxRateChange)]] <- maxRateChange[!is.na(maxRateChange)] 
      # which(chartDF$SEMax_a=="ArtifactMaxRate") 
      # which(chartDF$SEMax_a!=0)
      
      # chartDF$SEMax_a <- ""
      maxAmpChange <- tukeyFence7(x=maxAmp, buffer=3, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
      # chartDF$SEMax_a[maxPeaks[!is.na(maxAmpChange)]] <- maxAmpChange[!is.na(maxAmpChange)] 
      chartDF$SEMax_a[maxPeaks[!is.na(maxAmpChange)]] <- 15
      # which(chartDF$SEMax_a=="ArtifactMaxAmp") 
      which(chartDF$SEMax_a!=0) 
      
      chartDF$SEMA_a <- 0
      chartDF$SEMA_a[maxPeaks[which(chartDF$c_Move1Proc[maxPeaks] <= chartDF$c_Move1MA[maxPeaks])]] <- 7.5
      # maxPeaks[chartDF$c_Move1Max[maxPeaks] <= chartDF$c_Move1MA[maxPeaks]]
      # which(chartDF$SEMA_a == "Artifact")
      which(chartDF$SEMA_a!=0) 
      
      ###############################
      
      # get the min peaks for the activity sensor data
      minPeaks <- minPeak(x=chartDF$c_Move1Proc, y=round(.5*cps,0))
      # chartDF$c_Move1Min <- interpolatePeaks(minPeaks, chartDF$c_Move1Proc[minPeaks])
      # remove peak points accross consecutive samples
      consecPeaks <- which((minPeaks[2:length(minPeaks)]-1)==minPeaks[1:(length(minPeaks)-1)])
      if(length(consecPeaks > 0)) minPeaks <- minPeaks[-consecPeaks]
      
      # calculate the rate of oscillation
      minRate <- c(mean(diff(minPeaks)), diff(minPeaks)) / round(1*cps,0)
      
      # compute the amplitude from the tracing average to the lower peak points
      minAmp <- chartDF$c_Move1MA[minPeaks] - chartDF$c_Move1Proc[minPeaks]
      
      chartDF$SEMin_a <- 0
      minRateChange <- tukeyFence7(x=minRate, buffer=3, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=3, scaleVal=1)
      # chartDF$SEMin_a[minPeaks[!is.na(minRateChange)]] <- "ArtifactMinRate"
      # which(chartDF$SEMin_a=="ArtifactMinRate") 
      which(chartDF$SEMin_a!=0)
      
      
      # chartDF$SEMin_a <- ""
      minAmpChange <- tukeyFence7(x=minAmp, buffer=3, side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=4, scaleVal=1)
      # chartDF$SEMin_a[minPeaks[!is.na(minAmpChange)]] <- minAmpChange[!is.na(minAmpChange)] 
      chartDF$SEMin_a[minPeaks[!is.na(minAmpChange)]] <- -15
      # chartDF$SEMin_a[minPeaks[!is.na(minAmpChange)]] <- "ArtifactMinAmp"
      # which(chartDF$SEMin_a=="ArtifactMinAmp") 
      which(chartDF$SEMin_a!=0) 
      
      # chartDF$SEMA_a <- 0
      chartDF$SEMA_a[minPeaks[which(chartDF$c_Move1MA[minPeaks] <= chartDF$c_Move1[minPeaks])]] <- -7.5
      # minPeaks[chartDF$c_Move1MA[minPeaks] <= chartDF$c_Move1Min[minPeaks]]
      # which(chartDF$SEMA_a == "Artifact")
      which(chartDF$SEMA_a!=0) 
      
      ###################
      
      # min max activity sensor peaks
      minMaxPeak <- minMaxPeakFn(x=chartDF$c_Move1, y=round(.333*cps,0))
      # remove peak points accross consecutive samples
      consecPeaks <- which((minMaxPeak[2:length(minMaxPeak)]-1)==minMaxPeak[1:(length(minMaxPeak)-1)])
      if(length(consecPeaks > 0)) minMaxPeak <- minMaxPeak[-consecPeaks]
      
      # minMaxAmp <- abs(diff(chartDF$c_Move1[minMaxPeak]))
      minMaxAmp <- abs(chartDF$c_Move1Max[minMaxPeak] - chartDF$c_Move1Min[minMaxPeak])
      
      # chartDF$SE_a <- ""
      # minMaxChange <- tukeyFence5(x=minMaxAmp, buffer=4, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=7, scaleVal=1)
      # chartDF$SE_a[minMaxPeak[which(!is.na(minMaxChange))]]  <- "ArtifactMinMaxAmp"
      # which(chartDF$SE_a=="ArtifactMinMaxAmp")
      
      ###############################
      
      chartDF$c_Move1Result <- chartDF$c_Move1Proc[1] - (.05*yRange)
      # chartDF$c_Move1Result <- chartDF$c_Move1Result + chartDF$SEMA_a 
      chartDF$c_Move1Result <- chartDF$c_Move1Result + chartDF$SEMax_a
      chartDF$c_Move1Result <- chartDF$c_Move1Result + chartDF$SEMin_a
      
      #############################################
      
      # minMaxPeak[SplitHalfTukey(x=minMaxAmp)]
      
      # chartDF$SEMax_a <- ""
      # chartDF$SEMax_a[maxPeaks[unique(SplitHalfTukey(x=maxAmp))]] <- "ArtifactMaxAmp"
      # which(chartDF$SE_a!="")
      
      # chartDF$SEMin_a <- ""
      # chartDF$SEMin_a[minAmp[SplitHalfTukey(minAmp)]] <- "ArtifactMinAmp"
      # which(chartDF$SE_a!="")
      
      # chartDF$SE_a <- ""
      # chartDF$SE_a[minMaxPeak[SplitHalfTukey(x=minMaxAmp)]] <- "Artifact"
      # which(chartDF$SE_a!="")
      
      
      #######################
      
      # SEProcMax <- maxPeak(x=chartDF$c_Move1Proc, y=round(.75*cps, 0))
      # SEProcMin <- minPeak(x=chartDF$c_Move1Proc, y=round(.75*cps, 0))
      
      # chartDF$c_Move1Max <- interpolatePeaks(SEProcMax, chartDF$c_Move1Proc[SEProcMax])
      # chartDF$c_Move1Min <- interpolatePeaks(SEProcMin, chartDF$c_Move1Proc[SEProcMin])
      
      # chartDF$SE_a <- ""
      # maxSEProcAmp <- chartDF$c_Move1Proc[SEProcMax] - chartDF$c_Move1ProcMA[SEProcMax]
      # maxSEProcAmpChange <- tukeyFence5(x=maxSEProcAmp, buffer=3, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=3, scaleVal=1)
      # chartDF$SE_a[SEProcMax[!is.na(maxSEProcAmpChange)]] <- "Artifact"
      # which(chartDF$SE_a=="Artifact")
      # maxSEProcAmpChange[!is.na(maxSEProcAmpChange)]
      
      # minSEProcAmp <- chartDF$c_Move1ProcMA[SEProcMin] - chartDF$c_Move1Proc[SEProcMin]
      # minSEProcAmpChange <- tukeyFence5(x=minSEProcAmp, buffer=3, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=3, scaleVal=1)
      # chartDF$SE_a[SEProcMin[!is.na(minSEProcAmpChange)]] <- "Artifact"
      # which(chartDF$SE_a=="Artifact")
      
      # chartDF$SE_a[SEProcMax[which(chartDF$c_Move1Proc[SEProcMax] <= chartDF$c_Move1ProcMA[SEProcMax])]] <- "Artifact"
      # chartDF$SE_a[SEProcMin[which(chartDF$c_Move1Proc[SEProcMin] >= chartDF$c_Move1ProcMA[SEProcMin])]] <- "Artifact"
      
      ####################
      
    }
    
  }
  
  return(chartDF)
  
}

