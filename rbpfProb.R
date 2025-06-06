# function to determine the presence or probility of a respiration cycle in the cardio data

# uses the ratePerMin() function from the sigProcHelper.R script
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProc_extra.R'), echo=FALSE)

rbpfProbFn <- function(x=chartDF, y=NULL) {
  # function to determine proportional differences in cyclic rate for pneumo and cardio data
  # 5-4-2016
  # 2-29-2020
  # 11-6-2020
  # Raymond Nelson
  # x input is the chart data from which the respiration and cardio channels are used
  # y input is the evaluation period in seconds
  # if y == NULL then the RBPF proportion is returned for the chart
  # if y != NULL then the chartDF is returned with the RBPF info in a time series column
  # this function calls another function from the global environment: ratePerMin() 
  # output is one of three rbpf messages: none, possible rbpf, or different rates for thoracic and abdominal respiration
  ####
  
  {
  
  	if(!exists("chartDF")) chartDF <- x
  	if(!exists("y")) y <- NULL
  
    # y is the number of seeconds  for the evaluation period
  	# y=15
  
    if(nrow(chartDF) < 45 * cps) { return("RBPF: NA") }
  
  	uniqueEvents <- unique(chartDF$eventLabel) 
  	uniqueEvents <- uniqueEvents[uniqueEvents != ""]
  
  	# use the getFirstLastEventFn() in the sigProcHelper.R script
  	# this is to ignore the trashy unused data segment before the X
  	firstLast <- getFirstLastEventFn(x=chartDF)
  	# print(firstLast)
  	firstEvent <- firstLast['firstEvent']
  	lastEvent <- firstLast['lastEventEnd'] - 450
  
  	# chartDF is not returned when y == NULL
  	chartDF$CardioRBPF_a <- 0
  	chartDF$CardioRBPFMessage_a <- 0
  
  }
  
  #######################################
  
  # # re-calculate the cardio mid line
  # chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
  # # re-process the slow moving average - normally done in the scale offset
  # # move this to the init script when done
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)
  # # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(1*cps,0), times=1)
  # 
  # # calculate the cardio rate and buffer length
  # # was buffer=3 4-21-2-2017
  # cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=12,peaks="upper",lowPass=TRUE)
  # bufferLen <- bufferLenFn(x=cardioRate, y=.6)
  # 
  # # locate the systolic peaks
  # maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # 
  # # compute the systolic line
  # chartDF$c_CardioSystolic <- interpolatePeaks(x=maxOut, 
  #                                              y=chartDF$c_Cardio1[maxOut])[1:nrow(chartDF)]
  # 
  # # locate the diastolic peaks
  # minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # 
  # # compute the diastolic line
  # chartDF$c_CardioDiastolic <- interpolatePeaks(x=minOut, 
  #                                               y=chartDF$c_Cardio1[minOut])[1:nrow(chartDF)]
  
  #### flat pneumo data ####
  
  if( var(chartDF$c_UPneumoSm) == 0 || var(chartDF$c_LPneumoSm == 0) ) {
    
    return("none")
    
  }
  
  ###### calculate the respiration rate for upper and lower pneumo sensors ######
  
  {
    nrow(chartDF) / 30
    # smooth the respiration data a little more before calculating the rate
    # to calculate and compart upper and lower resp rate with ugly data
    UPneumoSmooth <- MASmooth(x=chartDF$c_UPneumoSm[firstEvent:lastEvent], y=15, times = 2)
    LPneumoSmooth <- MASmooth(x=chartDF$c_LPneumoSm[firstEvent:lastEvent], y=15, times = 2)
    
    # plot.ts(UPneumoSmooth[600:2400])
    # plot.ts(LPneumoSmooth[600:2400])
    
    # call the ratePerMin() function from the sigProcHelper.R script
    # 2/1/2017 was buffer=30 not working well for slow resp rate
    UPRate <- ratePerMin(x=UPneumoSmooth, buffer=42)
    LPRate <- ratePerMin(x=LPneumoSmooth, buffer=42)
    
    # without the additional smoothing 
    # commented out 2-26-2017
    # UPRate <- ratePerMin(chartDF$c_UPneumoSm[firstEvent:lastEvent], buffer=42)
    # LPRate <- ratePerMin(chartDF$c_LPneumoSm[firstEvent:lastEvent], buffer=42)
    
    PRate <- mean(c(UPRate, LPRate))
    
    #### check the respiration rate ratio ####
    
    # respiration rate ratio
    # a ratio is a quotient of two numbers
    # a rate is a ratio of two measurements with different units (e.g., miles per hour) 
    # the denominator of a ratio is 1
    # respiration rate ratio is the ratio of two respiration rates (e.g., cycles per minute)
    UPLPRatio <- exp(-abs(log(UPRate/LPRate)))
    
    # interpret the dissimilarity proportion
    if(UPLPRatio < .9) {
      PRate <- "respiration may be different for thoracic and abdominal sensors"
      # if(is.null(y)) {
      #   # return(PRate)
      # }
    }
        
  }

  ########## get the cardio data ############
  
  {
  
  	# cardioData <- switch(cardioLine,
  	#                      "mid" = chartDF$c_CardioMid,
  	#                      "diastolic" = chartDF$c_CardioDiastolic,
  	#                      "systolic" = chartDF$c_CardioSystolic,
  	#                      "ma" = chartDF$c_CardioMA,
  	#                      "otherwise: first")
  
  	cardioData <- chartDF$c_CardioMid
  
  	cardioData <- cardioData[firstEvent:lastEvent]
  
  }
  
  ######### calculate the the rbpf rate using the cardio mid line  #########
  
  if(PRate != "respiration may be different for thoracic and abdominal sensors") {
    
    rbpfRate <- ratePerMin(x=cardioData, 
                           buffer=15, 
                           # was buffer=8 on 5/8/16
                           peaks="upper", 
                           lowPass=TRUE)
    
    # to avoid some problems
    if(round(PRate,4)==round(rbpfRate,4)) { rbpfRate <- rbpfRate + .0001 }
    
    #### check the amplitude of cardio respiratory oscillationn ####
    
    {
      
      
      rbpfDiff <- 
        mean(abs(diff(cardioData[minMaxPeakFn(x=cardioData, y=round(.25*cps,0))])))
      
    }
    
    
    # then get the rbpf score
    if(rbpfDiff > 10) {
      rbpfRatio <- exp(-abs(log(PRate/rbpfRate)))
      rbpfMsg <- "none"
      # rbpfRatio <- "none"
    } else {
      rbpfMsg <- "none"
      rbpfRatio <- "none"
    }
    
  } else { 
    # if significant differences in upper and lower respiration rates
    rbpfRatio <- "unusual respiration"
    rbpfMsg <- "unusual respiration"
  }
  
  #############
  
  # # get the the rbpf rate for the cardio mid line
  # # was buffer=8 on 5/8/16
  # # was buffer=2 on 3/13/17 this was giving high RBPF scores for some stable cardio (GF exam)
  # rbpfRateMid <- ratePerMin(chartDF$c_CardioMid[firstEvent:lastEvent], buffer=15, peaks="lower", lowPass=TRUE) # was buffer=8 on 5/8/16
  # if(round(PRate,4)==round(rbpfRateMid,4)) { rbpfRate1 <- rbpfRateMid + .0001 }
  # 
  # # get the rbpf rate for the cardio diastolic
  # # was buffer=8 on 5/8/16
  # rbpfRateDiast <- ratePerMin(chartDF$c_CardioDiastolic[firstEvent:lastEvent], buffer=15, peaks="lower", lowPass=TRUE) 
  # if(round(PRate,4)==round(rbpfRateDiast,4)) { rbpfRate1 <- rbpfRateDiast + .0001 }
  # 
  # # get the rbpf rate for the cardio systolic
  # # was buffer=8 on 5/8/16
  # rbpfRateSyst <- ratePerMin(chartDF$c_CardioSystolic[firstEvent:lastEvent], buffer=15, peaks="upper", lowPass=TRUE) 
  # if(round(PRate,4)==round(rbpfRateSyst,4)) { rbpfRate1 <- rbpfRateSyst + .0001 }
  # 
  # # get the rbpf rate for the cardio slow moving average
  # # was buffer=8 on 5/8/16
  # rbpfRateMA <- ratePerMin(chartDF$c_CardioMA[firstEvent:lastEvent], buffer=12, peaks="upper", lowPass=TRUE) 
  # if(round(PRate,4)==round(rbpfRateMid,4)) { rbpfRate1 <- rbpfRateMid + .0001 }
  # 
  # 
  # # get the RBPF ratio
  # rbpfRatioMid <- exp(-abs(log(PRate/rbpfRateMid)))
  # rbpfRatioDiast <- exp(-abs(log(PRate/rbpfRateDiast)))
  # rbpfRatioSyst <- exp(-abs(log(PRate/rbpfRateSyst)))
  # rbpfRatioMA <- exp(-abs(log(PRate/rbpfRateMA)))
  # 
  # rbpfVector <- c(rbpfRatioMid, rbpfRatioDiast, rbpfRatioSyst, rbpfRatioMA)
  # 
  # # select the rbpf coefficient for the chanel used for analysis
  # # cardioLine is scalar in the global environment and 
  # # it is set in the NCCAASCII_init.R script and used for feature extraction
  # selectRBPF <- switch(cardioLine,
  #                      "mid" = rbpfVector[1],
  #                      "diastolic" = rbpfVector[2],
  #                      "systolic" = rbpfVector[3],
  #                      "ma" = rbpfVector[4],
  #                      "otherwise: first")
  
  #############################################
  
  # iterate over the respiration and cardio data using the y period
  
  if(!is.null(y)) {
    
    # to calculate the moveing RBPF score over the chart 
  
    # if(is.null(y)) { y <- nrow(chartDF) }
  
    # y is the size of each sub-sample
    
    # y=300
    
    # first initialize a vector to hold the result
    rbpfVector <- rep(0, times=length(cardioData))
    
    # initialize the starting index
    startIdx <- y * cps
    
    # and get the samples
    sampleEnd <- sample(c(startIdx:length(cardioData)), size=100, replace=TRUE)
    sampleStart <- sampleEnd - startIdx + 1

    # set the number of samples
    # sampleSize <- round((length(cardioData)/30) / y, 0) * 2
    sampleSize <- 100
    
    # then iterate over the data
    # i=startIdx
    # for(i in startIdx:length(cardioData)) {
    for(i in 1:sampleSize) {
      
      # get the rbpf rate for each iteration
      # rbpfRate <- ratePerMin(x=cardioData[(i-startIdx+1):i], 
      rbpfRate <- ratePerMin(x=cardioData[sampleStart[i]:sampleEnd[i]], 
                             buffer=15, 
                             peaks="upper", 
                             lowPass=TRUE)
      
      # get the respiration rate for each iteration
      UPRate <- ratePerMin(UPneumoSmooth[sampleStart[i]:sampleEnd[i]], buffer=42)
      LPRate <- ratePerMin(LPneumoSmooth[sampleStart[i]:sampleEnd[i]], buffer=42)
      
      # then compare the upper and lower pneumo
      # UPLPRatio <- exp(-abs(log(UPRate/LPRate)))
      # interpret the dissimilarity proportion
      # if(UPLPRatio < .9) {
      #   PRate <- "respiration may be different for thoracic and abdominal sensors"
      #   if(is.null(y)) {
      #     next()
      #   }
      # } else {
        PRate <- mean(c(UPRate, LPRate))
      # } # end if UPLPRatio < .9
      
      # to avoid some problems
      if(round(PRate,4)==round(rbpfRate,4)) { rbpfRate <- rbpfRate + .0001 }
      
      # then get the rbpf score
      if(rbpfDiff > 10) { 
        rbpfRatio <- exp(-abs(log(PRate/rbpfRate)))
        # add the rbpfRatio to the rbpfVector
        replaceVector <- which(rbpfVector[sampleStart[i]:sampleEnd[i]] < rbpfRatio)
        if(length(replaceVector) > 0) {
          rbpfVector[sampleStart[i]:sampleEnd[i]][replaceVector] <- rbpfRatio
        }
      } else {
        rbpfRatio <- "none"
      }
      
    } # end for loop over cardioData vector
    
    # which(rbpfVector > .999)
    
    chartDF$CardioRBPF_a[firstEvent:lastEvent] <- rbpfVector
    
    rbpfRatio <- mean(chartDF$CardioRBPF_a[firstEvent:lastEvent])
    
    chartDF$CardioRBPFMessage_a[which(rbpfVector > .9)] <- "possible RBPF" 
    
  } # end if !is.null(y)
  
  #############################################
  
  # iterate over the data and calculate the unresponse 
  
  
  
  #############################################
  
  # check the rbpf for the entire vector if y == NULL
  
  if(is.null(y)) {
  
    # make a private function to format the numbers 
    # to eliminate zeros before the decimal and show only 2 places
    formatRatio <- function(val) { 
      if(!is.numeric(val)) return(val)
      sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) 
    }
    
    if(rbpfRatio != "unusual respiration" && rbpfRatio != "none") { 
      rbpfRatio <- round(rbpfRatio, 2)
      
      if(rbpfRatio>=.99) rbpfRatio <- .99
      if(rbpfRatio<=.01) rbpfRatio <- .01
      
      # if(rbpfRatio != "none") rbpfRatio <- as.character(formatRatio(rbpfRatio))
      
    }
    
    # if (rbpfRate >= .95) {
    #   rbpfRatio <- rbpfRate
    # } else {
    #   # rbpfRatio <- "none"
    #   rbpfRatio <- rbpfRate
    #   # rbpfRatio <- mean(rbpfVector[1:3])
    # }
    
    # select the warning messaage
    if(rbpfMsg == "unusual respiration") {
      PRate == "respiration may be different for thoracic and abdominal sensors" 
      # rbpfMsg <- paste(as.character(formatRatio(UPLPRatio)), PRate)
      rbpfMsg <- paste("RBPF:", "unusual respiration")
    } else {
    
      rbpfMsg <- ifelse(rbpfRatio >= .9,
                        paste("RBPF:", as.character(formatRatio(rbpfRatio)) ),
                        paste("RBPF:", as.character(formatRatio(rbpfRatio))) )
    } # end if
    
    return(rbpfMsg)
    # return(rbpfRatio)
    
  } else {
    return(chartDF)
  }
  
} # end rbpfProbFn() 



# getSegmentFn(examNum=9, seriesNum=1, chartNum=1)
# print(rbpfProbFn(x=chartDF))
# getSegmentFn(examNum=6, seriesNum=1, chartNum=2)
# print(rbpfProbFn(x=chartDF))
# getSegmentFn(examNum=6, seriesNum=1, chartNum=3)
# print(rbpfProbFn(x=chartDF))

