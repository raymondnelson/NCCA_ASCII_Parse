# R function to identify respiration artifacts 
# 2-28-2017
# Raymond Nelson
# 
##############


# source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)


# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/tukeyFences.R', echo=FALSE)


pneumoArtifactFn <- function(x=chartDF) {
  # R function to identify respiration artifacts 
  
  chartDF <- x
  
  # chartDF$chartName[1]
  
  {
    chartDF$Pneumo_a <- 0
    chartDF$UPneumo_a <- 0
    chartDF$LPneumo_a <- 0
    chartDF$UPneumoExh_a <- 0
    chartDF$UPneumoInh_a <- 0
    chartDF$LPneumoExh_a <- 0
    chartDF$LPneumoInh_a <- 0
    chartDF$UPneumoMid_a <- 0
    chartDF$LPneumoMid_a <- 0
  }
  
  ####
  
  # get the pneumo artifacts using the peak points
  
  # {
  #   numCycles <- 3
  #   IQRMult <- 1
  #   rateRatio <- 2
  #   multRatio <- 2
  # }
  
  ###### create a 1 sec pre and post buffer for verbal answers ######
  
  {
    
    answerRows <- which(chartDF$Label %in% c("YES", "NO", "ANS"))
    
    if(length(answerRows) == 0 ) return(chartDF)
    
    answerBuffOn <- answerRows - pneumoAnsBuff * cps
    answerBuffOff <- answerRows + pneumoAnsBuff * cps
    
    answerBuffer <- NULL
    
    for(i in 1:length(answerBuffOn)) {
      answerBuffer <- c(answerBuffer, answerBuffOn[i]:answerBuffOff[i])
    }
    
  }
  
  ###### upper pneumo ######
  
  {
    
    #### upper respiration rate using inhalation peaks ####
    
    # get the peak points for the stimulus segment
    maxPeaksU <- maxPeak(x=chartDF$c_UPneumoSm, y=round(.5*cps,0))
    
    # remove peaks near the verbal answer
    # maxPeaksU <- maxPeaksU[!(maxPeaksU %in% answerBuffer)]
    
    # remove peak points across consecutive samples
    consecPeaks <- which((maxPeaksU[2:length(maxPeaksU)]-1)==maxPeaksU[1:(length(maxPeaksU)-1)])
    if(length(consecPeaks > 0)) { maxPeaksU <- maxPeaksU[-consecPeaks] }
    # remove peak changes less than 1/4 second
    shortSegments <- which(diff(maxPeaksU) <= round(.25*cps,0)) + 1
    if(length(shortSegments) > 0) maxPeaksU <- maxPeaksU[-shortSegments]
    # recalculate the inhalation line
    chartDF$c_UPneumoInh <- interpolatePeaks(maxPeaksU, chartDF$c_UPneumoSm[maxPeaksU])
    
    # compute the resp rate in seconds using the peaks
    maxRateU <- c(mean(diff(maxPeaksU)), diff(maxPeaksU)) / cps
    
    # locate significant changes in the upper respiration rate
    maxRateChangeU <- maxPeaksU[which(!is.na(tukeyFence5(x=maxRateU, buffer=4, bufferStop=0, side="both", fence="outer", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # add the artifact to the chartDF 
    chartDF$UPneumo_a[maxRateChangeU] <- "Artifact"
    # print in red
    
  }
  
  {
    
    #### upper respiration rate using exhalation peaks ####
    
    # get the min peaks
    minPeaksU <- minPeak(x=chartDF$c_UPneumoSm, y=round(.25*cps,0))
    
    # remove exhalation peaks near the verbal answer
    # minPeaksU <- minPeaksU[!(minPeaksU %in% answerBuffer)]
    
    # chartDF$c_UPneumoExh <- interpolatePeaks(minPeaks, chartDF$c_UPneumoSm[minPeaks])
    # # remove peak points across consecutive samples
    # consecPeaks <- which((maxPeaks[2:length(maxPeaks)]-1)==maxPeaks[1:(length(maxPeaks)-1)])
    # if(length(consecPeaks > 0)) { minPeaks <- minPeaks[-consecPeaks] }
    # # remove peak changes less than 1/2 second
    # shortSegments <- which(diff(minPeaks) <= round(.24*cps,0)) + 1
    # if(length(shortSegments) > 0) minPeaks <- minPeaks[-shortSegments]
    
    # then get the upper pneumo rate in seconds using the min Peaks
    minRateU <- c(mean(diff(minPeaksU)), diff(minPeaksU)) / cps
    
    # compute changes in upper respiration rate using minPeaks
    minRateChangeU <- minPeaksU[which(!is.na(tukeyFence5(x=minRateU,buffer=4, bufferStop=0, side="both", fence="outer", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # add the artifact to the chartDF
    chartDF$UPneumo_a[minRateChangeU] <- "Artifact"
    # print in red
    
  }
  
  {
    
    #### upper respiration amplitude ####
    
    # get the distance from inhalation peak to mid line
    maxAmpU <- chartDF$c_UPneumoSm[maxPeaksU] - chartDF$c_UPneumoMid[maxPeaksU]
    
    maxAmpChangeU <- maxPeaksU[which(!is.na(tukeyFence5(x=maxAmpU,buffer=3, bufferStop=0, side="both", fence="inner", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # exlcude artifacts during first 5 seconds and last 5 seconds
    # maxAmpChangeU <- 
    #   maxAmpChangeU[maxAmpChangeU > 5 * cps & maxAmpChangeU < (nrow(chartDF) - (5 * cps))]
    
    # maxAmpChangeU <- 
    #   sort(c(maxAmpChange, maxPeaks[which(!is.na(tukeyFence5(x=maxAmp,buffer=3, bufferStop=0, side="lower", fence="inner", innerFence=1.5, outerFence=3, expVal=.5, scaleVal=1)))]))
    
    # add the artifact to the chartDF
    # chartDF$UPneumo_a[maxAmpChangeU] <- "Artifact"
    chartDF$UPneumoInh_a[maxAmpChangeU] <- "Artifact"
    
  }
  
  {
    
    #### upper pneumo baseline ####
    
    minAmpU <- chartDF$c_UPneumoMid[minPeaksU] - chartDF$c_UPneumoSm[minPeaksU]
    
    minAmpChangeU <- minPeaksU[which(!is.na(tukeyFence5(x=minAmpU,buffer=2, bufferStop=0, side="both", fence="inner", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # minAmpChangeU <- 
    #   sort(c(minAmpChange, minPeaks[which(!is.na(tukeyFence5(x=minAmp,buffer=3, bufferStop=0, side="lower", fence="outer", innerFence=1.5, outerFence=3, expVal=.5, scaleVal=1)))]))
    
    # add the artifact to the chartDF
    # chartDF$UPneumo_a[minAmpChangeU] <- "Artifact"
    chartDF$UPneumoExh_a[minAmpChangeU] <- "Artifact"
    
  }
  
  {
    
    ## add the pneumo artifacts to the data frame ##
    
    
    # chartDF$UPneumoMid_a[maxRateChange] <- "Artifact"
    # chartDF$UPneumoMid_a[minRateChange] <- "Artifact"
    
  }
  
  ########### lower pneumo ###########
  
  {
    
    #### lower respiration rate using inhalation peaks ####
    
    # get the peak points for segment
    maxPeaksL <- maxPeak(x=chartDF$c_LPneumoSm, y=round(.5*cps,0))
    
    # remove inhalation peaks near the verbal answer
    # maxPeaksL <- maxPeaksL[!(maxPeaksL %in% answerBuffer)]
    
    # remove peak points across consecutive samples
    consecPeaks <- which((maxPeaksL[2:length(maxPeaksL)]-1)==maxPeaksL[1:(length(maxPeaksL)-1)])
    if(length(consecPeaks > 0)) { maxPeaksL <- maxPeaksL[-consecPeaks] }
    # remove peak changes less than 1/4 second
    shortSegments <- which(diff(maxPeaksL) <= round(.25*cps,0)) + 1
    if(length(shortSegments) > 0) maxPeaksL <- maxPeaksL[-shortSegments]
    # recalculate the inhalation line
    chartDF$c_LPneumoInh <- interpolatePeaks(maxPeaksL, chartDF$c_LPneumoSm[maxPeaksL])
    
    # then compute the resp rate in seconds using the peaks
    maxRateL <- c(mean(diff(maxPeaksL)), diff(maxPeaksL)) / cps
    
    # compute the changes in lower respiration rate
    maxRateChangeL <- maxPeaksL[which(!is.na(tukeyFence5(x=maxRateL, buffer=4, bufferStop=0, side="both", fence="outer", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # add the artifact to the chartDF
    chartDF$LPneumo_a[maxRateChangeL] <- "Artifact"
    
  }
  
  {
    
    #### lower respiration rate using exhalation peaks ####
    
    minPeaksL <- minPeak(x=chartDF$c_LPneumoSm, y=round(.25*cps,0))
    
    # remove exhalation peaks near the verbal answer
    # minPeaksL <- minPeaksL[!(minPeaksL %in% answerBuffer)]
    
    # chartDF$c_LPneumoExh <- interpolatePeaks(minPeaks, chartDF$c_LPneumoSm[minPeaks])
    # # remove peak points accross consecutive samples
    # consecPeaks <- which((maxPeaks[2:length(maxPeaks)]-1)==maxPeaks[1:(length(maxPeaks)-1)])
    # if(length(consecPeaks > 0)) { minPeaks <- minPeaks[-consecPeaks] }
    # # remove peak changes less than 1/2 second
    # shortSegments <- which(diff(minPeaks) <= round(.5*cps,0)) + 1
    # if(length(shortSegments) > 0) minPeaks <- minPeaks[-shortSegments]
    
    # then get the lower pneumo rate in seconds using the min Peaks
    minRateL <- c(mean(diff(minPeaksL)), diff(minPeaksL)) / cps
    
    # get the abdominal respiration rate using minPeaks
    minRateChangeL <- minPeaksL[which(!is.na(tukeyFence5(x=minRateL, buffer=4, bufferStop=0, side="both", fence="outer", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # add the artifact to the chartDF
    chartDF$LPneumo_a[minRateChangeL] <- "Artifact"
    
  }
  
  {
    
    #### lower respiration amplitude ####
    
    maxAmpL <- chartDF$c_LPneumoSm[maxPeaksL] - chartDF$c_LPneumoMid[maxPeaksL]
    
    maxAmpChangeL <- maxPeaksL[which(!is.na(tukeyFence5(x=maxAmpL, buffer=3, bufferStop=0, side="both", fence="inner", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # maxAmpChangeL <- 
    #   sort(c(maxAmpChangeL, maxPeaks[which(!is.na(tukeyFence5(x=maxAmp, buffer=3, bufferStop=0, side="lower", fence="inner", innerFence=1.5, outerFence=3, expVal=.5, scaleVal=1)))]))
    
    # add the artifact to the chartDF
    chartDF$LPneumoInh_a[maxAmpChangeL] <- "Artifact"
    
  }
  
  {
    
    #### lower pneumo baseline ####
    
    minAmpL <- chartDF$c_LPneumoMid[minPeaksL] - chartDF$c_LPneumoSm[minPeaksL]
    
    minAmpChangeL <- minPeaksL[which(!is.na(tukeyFence5(x=minAmpL, buffer=2, bufferStop=0, side="both", fence="inner", innerFence=1.5, outerFence=4, expVal=1, scaleVal=1)))]
    
    # minAmpChangeL <- 
    #   sort(c(minAmpChangeL, minPeaks[which(!is.na(tukeyFence5(x=minAmp,buffer=3, side="lower", fence="outer", innerFence=1.5, outerFence=3, expVal=.5, scaleVal=1)))]))
    
    # add the artifact to the chartDF
    chartDF$LPneumoExh_a[minAmpChangeL] <- "Artifact"
    
  }
  
  {
    
    ## add the lower pneumo artifacts to the data frame ##
    
    # chartDF$LPneumoInh_a[maxAmpChange] <- "Artifact"
    # chartDF$LPneumoMid_a[maxRateChange] <- "Artifact"
    # chartDF$LPneumoExh_a[minAmpChange] <- "Artifact"
    # chartDF$LPneumoMid_a[minRateChange] <- "Artifact"
    
  }
  
  ###### apnea ######
  
  ######## compare the upper and lower pneumo ########
  
  # 5-18-2017
  
  # which(chartDF$Pneumo_a != 0)
  
  # chartDF$c_UPneumoSm <- MASmooth(x=chartDF$c_UPneumoSm, y=round(.25*cps,0), times=1)
  # chartDF$c_LPneumoSm <- MASmooth(x=chartDF$c_LPneumoSm, y=round(.25*cps,0), times=1)
  
  {
    
    ## compute the excursion length for the upper and lower respiration data ##
    
    # these are set in the workFlow.R script
    sec=.5
    cutProp=.1
    
    # initialize a vector for the upper and lower
    UPneumoExcursion <- rep(0, times=nrow(chartDF))
    LPneumoExcursion <- rep(0, times=nrow(chartDF))
    
    diffVectorUP <- c(0, abs(diff(chartDF$c_UPneumoSm)))
    diffVectorLP <- c(0, abs(diff(chartDF$c_LPneumoSm)))
    
    for (i in (sec * cps):nrow(chartDF)) {
      UPneumoExcursion[i] <- sum(diffVectorUP[(i-(sec * cps - 1)):i])
      LPneumoExcursion[i] <- sum(diffVectorLP[(i-(sec * cps - 1)):i])
    }
    
    # compare the upper and lower excursion values
    diffProp <- exp(-abs(log(UPneumoExcursion/LPneumoExcursion)))
    diffProp[is.na(diffProp)] <- 1
    ULDiffRows <- which(diffProp < cutProp)
    
    diffProp[ULDiffRows]
    
    # 2020-June 5 this does not seem to add much if anything
    # chartDF$Pneumo_a[ULDiffRows] <- "Artifact"
    
  }
  
  ######## compare each pneumo segment to the previous  ########
  
  {
    
    # these are set in the workFlow.R script
    sec2 <- .25
    cutProp2 <- .001
    
    vectorLength <- nrow(chartDF)
    
    ##### upper respiration #####
    
    # calculate the summed diffs of UPneumo over the observation period
    for (i in (sec2 * cps):vectorLength) {
      UPneumoExcursion[i] <- sum(diffVectorUP[(i-(sec2 * cps - 1)):i])
    }
    
    # make 3 vectors for comparison #
    
    # start at beginning
    seg1 <- UPneumoExcursion[1:(vectorLength-2*(sec2 * cps))]
    # start at the end of the sec2
    seg2 <- UPneumoExcursion[(sec2 * cps + 1):(vectorLength-(sec2 * cps))]
    # start at 2xsec@
    seg3 <- UPneumoExcursion[(2*(sec2 * cps) + 1):vectorLength]
    
    # should be all equal lengths
    length(seg1)
    length(seg2)
    length(seg3)
    
    artifactRows2 <- which(exp(-abs(log(seg2/seg1))) <= cutProp2) # + sec2 * cps
    artifactRows3 <- which(exp(-abs(log(seg2/seg3))) >= (1-cutProp2)) # + sec2 * cps
    
    # 2020-06-03 these artifacts seem to be ineffective
    
    # chartDF$Pneumo_a[artifactRows2] <- "Artifact"
    # chartDF$Pneumo_a[artifactRows3] <- "Artifact"
    
    # chartDF$UPneumo_a[artifactRows2] <- "Artifact"
    # chartDF$UPneumoMid_a[artifactRows3] <- "Artifact"
    
    ##### lower respiration #####
    
    for (i in (sec2 * cps):nrow(chartDF)) {
      LPneumoExcursion[i] <- sum(diffVectorLP[(i-(sec2 * cps - 1)):i])
    }
    
    seg1 <- LPneumoExcursion[1:(vectorLength-2*(sec2 * cps))]
    seg2 <- LPneumoExcursion[(sec2 * cps + 1):(vectorLength-(sec2 * cps))]
    seg3 <- LPneumoExcursion[(2*(sec2 * cps) + 1):vectorLength]
    
    # compare the excursion lengths
    artifactRows4 <- which( exp(-abs(log(seg2/seg1))) <= cutProp2 ) # + sec2 * cps
    artifactRows5 <- which( exp(-abs(log(seg3/seg2))) >= (1-cutProp2) ) # + sec2 * cps
    
    # June 5 , 2020 this seems to add more problems than help
    # chartDF$Pneumo_a[artifactRows4] <- "Artifact"
    # chartDF$Pneumo_a[artifactRows5] <- "Artifact"
    
    # chartDF$LPneumo_a[artifactRows4] <- "Artifact"
    # chartDF$LPneumoMid_a[artifactRows5] <- "Artifact"
    
  }
  
  ########## check the pneumo stability #########
  
  #### upper pneumo stability ####
  
  {
    
    UPneumoMaxX <- maxPeaksU[which(maxAmpU <= 0)]
    UPneumoMinX <- minPeaksU[which(minAmpU <= 0)]
    
    UPneumoMinMaxX <- sort(c(UPneumoMaxX, UPneumoMinX))
    
    # chartDF$UPneumoMid_a[UPneumoMaxX] <- "Artifact"
    # chartDF$UPneumoMid_a[UPneumoMinX] <- "Artifact"
    
    chartDF$UPneumoMid_a[UPneumoMinMaxX] <- "Artifact"
    # chartDF$Pneumo_a[UPneumoMinMaxX] <- "Artifact"
    
  }
  
  #### lower pneumo stability ####
  
  {
    
    LPneumoMaxX <- maxPeaksL[which(maxAmpL <= 0)]
    LPneumoMinX <- minPeaksU[which(minAmpL <= 0)]
    
    LPneumoMinMaxX <- sort(c(LPneumoMaxX, LPneumoMinX))
    
    chartDF$LPneumoMid_a[LPneumoMinMaxX] <- "Artifact"
    # chartDF$Pneumo_a[LPneumoMinMaxX] <- "Artifact"
    
    # chartDF$LPneumoMid_a[LPneumoMaxX] <- "Artifact"
    # chartDF$LPneumoMid_a[LPneumoMinX] <- "Artifact"
    # 
  }
  
  ########## buffer around the verbal answer ##########
  
  {
    
    # remove common artifacts surrounding the verbal answer
    
    chartDF$Pneumo_a[answerBuffer] <- "0"
    chartDF$UPneumo_a[answerBuffer] <- "0"
    chartDF$LPneumo_a[answerBuffer] <- "0"
    chartDF$UPneumoExh_a[answerBuffer] <- "0"
    chartDF$UPneumoInh_a[answerBuffer] <- "0"
    chartDF$LPneumoExh_a[answerBuffer] <- "0"
    chartDF$LPneumoInh_a[answerBuffer] <- "0"
    chartDF$UPneumoMid_a[answerBuffer] <- "0"
    chartDF$LPneumoMid_a[answerBuffer] <- "0"
    
  }
  
  #### return ####
  
  # which(chartDF$Pneumo_a == "Artifact")
  # which(chartDF$UPneumo_a == "Artifact")
  # which(chartDF$LPneumo_a == "Artifact")
  
  # assign("chartDF", chartDF, envir=.GlobalEnv)
  # stop()
  
  return(chartDF)
  
} # end pneumoArtifactFn()


