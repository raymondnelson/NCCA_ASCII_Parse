# PLE measurement function



# source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
# source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)
# source(paste0(RPath, "abstractScale.R", echo=FALSE)



PLEMeasurementFn <- function(dataVector=segmentDF$c_PPG1, stimOnsetRow=301) {
  # function to compute the PLE measurement
  # 3/14/2016
  # Raymond nelson
  # PLE measurement function
  # called by the newPLEExtractFn() function in the PLEExtract.R script
  # 3 sec prestimulus compared with the five sec period begining 5 sec after stim onset
  # 
  ###
  # dataVector is the time series PLE data
  # stimOnsetRow is the sample index for the stimulus onset
  # stimOnsetRow == 151 for 5 second prestim segment
  # stimOnsetRow == 301 when using 10 prestimulus seconds
  # cps is the data rate (30 samples per second)
  # 
  # requires some helper functions from the sigProcHelper.R script
  # minPeak()
  # maxPeak()
  # interpolatePeaks()
  # ratePerMin
  # bufferLengthFn()
  #
  # requires a filter to remove the dichrotic notch to get the cardio rate 
  # from the sigProcExtra.R script
  # lowPass1.7hz.2nd()
  # 
  # output is a list with 3 elements, including
  # 1. the prestimulus mean pulse amplitude
  # 2. the poststimulus mean pulse amplitude
  # 3. the ratio of the prestimulus and poststimulus pulse amplitudes
  # 
  # Nov 20, 2023 rounded measurements to 0 decimals and log(pre/post) ratios to 3 decimals)
  #
  ##########
  
  #### calculate the cardio rate and buffer length to compute the syst and diast peaks ####
  
  {
    
    # 5-3-2016
    # get the cardio rate
    # cardioRate <- ratePerMin(dataVector,buffer=4)
    
    # 2-21-2019 attempt to get a more robust estimate of the cardio rate
    cardioRate1 <- ratePerMin(dataVector,buffer=2)
    cardioRate2 <- ratePerMin(dataVector,buffer=4)
    cardioRate3 <- ratePerMin(dataVector,buffer=6)
    cardioRate4 <- ratePerMin(dataVector,buffer=8)
    cardioRate5 <- ratePerMin(dataVector,buffer=10)
    cardioRate6 <- ratePerMin(dataVector,buffer=12)
    cardioRate7 <- ratePerMin(dataVector,buffer=14)
    cardioRate8 <- ratePerMin(dataVector,buffer=16)
    
    cardioRate <- median(c(cardioRate1, 
                           cardioRate2, 
                           cardioRate3, 
                           cardioRate4, 
                           cardioRate5, 
                           cardioRate6,
                           cardioRate7,
                           cardioRate8 ))
    
    # cardioRate <- ratePerMin(dataVector,buffer=8)
    
    # use the cardio rate to set the buffer length for peak extraction
    bufferLen <- bufferLenFn(cardioRate)
    
  }
  
  #### systolic peaks ####
  
  {
    # get the max peak indices
    maxOut <- maxPeak(x=dataVector, y=bufferLen) # may need to change to y=12 for slow pulse 
    # get the max peak values
    maxVal <- dataVector[maxOut]
    # interpolate between max peak values
    PLMaxInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:length(dataVector)]
    
    # 2-21-2019 need to locate and remove outlier spike cycles
    # mark an artifact and do not score
  }
  
  #### diastolic peaks ####
  
  {
    # then get the min peak indices
    minOut <- minPeak(x=dataVector, y=bufferLen) # may need to change to y=12 for slow pulse 
    # get the min peak values for the min peak rows
    minVal <- dataVector[na.omit(minOut)]
    # interpolate between the min peak values
    PLMinInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
  }
  
  #### three methods to compute a vector of amplitude differences between max and min peaks ####
  
  
  #### systolic - diastolic line SDL method - the preferred method #### 
  
  {
    
    PLEAmp <- round(PLMaxInterp - PLMinInterp, 0)
    
    # mean(PLEAmp)
    
  }
  
  #### Vandenbosch, Verschuere, et al. 2009 FPLL method ####
  
  {
    
    # 3 prestim seconds
    # 15 stimulus seconds
    # no latency
    
    # they used the pythagorean method
    # could also be completed using excursion method
    
    PLEDiff <- c(0, abs(diff(dataVector))) # include 0 so data are the same length
    
    #excursion method
    # FPLL <- cumsum(PLEDiff)
    
    # calculate the cumsum later
    FPLL <- PLEDiff
    
    RLLFn <- function(x=PLEDiff) {
      # private function
      xOut <- rep(NA, times=length(x))
      for(m in 1:length(x)) {
        xOut[m] <- sqrt( (1000/30)^2 + PLEDiff[m]^2 )
      }
      return(xOut)
    }
    
    # calculate the cumsum later
    FPLL <- round(RLLFn(x=PLEDiff), 0)
    
    # mean(FPLL)
    
    # adjust the scale 
    FPLL <- round(FPLL * 2.3, 0)
    
  }
  
  #### finger pulse amplitude - FPA peak difference method ####
  
  {
    
    # difference between each min and subsequent peak
    
    # duplicate the min peak (diastolic) vectors
    minOut2 <- minOut
    minVal2 <- minVal
    
    # duplicate the max peak (systolic) vectors
    # systolic peaks are always after diastolic mins
    if(maxOut[1] < minOut[1]) {
      maxOut2 <- maxOut[2:length(maxOut)]
      maxVal2 <- maxVal[2:length(maxVal)]
    } else {
      maxOut2 <- maxOut
      maxVal2 <- maxVal
    }
    
    # make sure the last maxOut index is after the last minOut index
    if(length(maxOut2) < length(minOut2)) {
      minOut2 <- minOut2[1:length(maxOut2)]
      minVal2 <- minVal2[1:length(maxVal2)]
    } else if(length(maxOut2 > length(minOut2))) {
      maxOut2 <- maxOut2[1:length(minOut2)]
      maxVal2 <- maxVal2[1:length(minVal2)]
    }
   
    # initialize the FPA output 
    
    FPA <- rep(0, times=length(dataVector))
    
    # vector of differences for systolic and diastolic peaks
    FPADiffs <- round(maxVal2 - minVal2, 0)
    # mean(FPADiffs)
    
    FPA[minOut2] <- FPADiffs
    
    for(m in 2:length(FPA)) {
      if(FPA[m] == 0) { 
        FPA[m] <- round(FPA[(m-1)], 0)
      }
    }
   
  }
  
  #### select the PLEAmp, FPLL or FPA data ####
  
  {
    
    # PLEMethod is set in the NCCAASCII_init.R script
    
    PLEData <- switch(PLEMethod,
            "SDL"=PLEAmp,
            "FPA"=FPA,
            "FPLL"=FPLL )

  }
  
  #### locate the prestim and stimulus segments ####
  
  {
    # set the prestim and poststim segments
    # uses PLEPrestim, PLELat, and PLEPostEnd 
    # parameters from the NCCAASCI_init.R script
    
    # prestim segment
    prestimOn <- stimOnsetRow - (PLEPrestim * cps)
    if(prestimOn < 1) { prestimOn <- 1 }
    prestimOff <- stimOnsetRow - 1
    # latency
    poststimOn <- stimOnsetRow + (PLELat * cps)
    # stimulus segment
    poststimOff <- stimOnsetRow + (PLEPostEnd * cps) - 1
    if(poststimOff > length(dataVector)) { poststimOff <- length(dataVector) }
  }
  
  #### get the prestim and poststim amplitudes ####
    
  {
    
    # compute the means for the prestim and stimulus segments
    prestimMeanAmp <- mean(PLEData[prestimOn:prestimOff])
    poststimMeanAmp <- mean(PLEData[poststimOn:poststimOff])
    
  }
  
  #### Nov 20, 2023 adjust the PLE measurements for the gain value ####
  
  {
    
    # source(paste0(RPath, "abstractScale.R", echo=FALSE)
    
    prestimMeanAmp <- abstractScaleFn(x=prestimMeanAmp, sensorName="PLE")
    poststimMeanAmp <- abstractScaleFn(x=poststimMeanAmp, sensorName="PLE")
    
  }
  
  #### Nov 20, 2023 round the pre and stim pulse amplitude measurements ####
  
  {
    
    prestimMeanAmp <- round(prestimMeanAmp, 0)
    poststimMeanAmp <- round(poststimMeanAmp, 0)
    
  }
  
  #### finally compute the pre post ratio ####
  
  {
    
    # 10-11-2018 changed to logged ratio
    prePostRatio <- round(log(prestimMeanAmp / poststimMeanAmp), 3)
    # there will always be a log(ratio)
    # even if no response constriction
    
    # 9-24-2021 pre/post response is output as 0 when no response, when the log val > 0 
    prePostRatio[which(prePostRatio < 0)] <- 0
    
  }
  
  ##### construct the output ####
  
  outputList <- list(prestimMeanAmp=prestimMeanAmp, 
                     poststimMeanAmp=poststimMeanAmp,
                     prePostRatio=prePostRatio
                     )
                     
  return(outputList)
  
} # end PLEMeasurementFn

#####


