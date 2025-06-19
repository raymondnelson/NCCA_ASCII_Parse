# R Function to extract activity sensor artifacts
# April 7, 2025
# Raymond Nelson 
####
# 
# activityRestrictionFn() # ceasation of activity due to breath holding
# 
# activityFn() # deep breath activity
# 
# activityPatternFn() # significant changes in normal activity pattern
#
# tonicActivityFn() # significant changes in tonic activity level
#
# activityAmplitudeFn() # signficant reduction in activity amplitude
#
# newActivityCheckFn() # main function
#
####



activityRestrictionFn <- function(DAT=chartDF$c_Move1Proc, w=3.75, y=(scaleVals['activity']/5), seg=15) {
  # R function to extract artifact from seat activity sensor data
  # activity freeze artifacts occur when an examinee remains unusually still 
  # such as when holding one's breath
  # April 8, 2025
  # Raymond Nelson
  ###
  # DAT is the smooothed time series data for the seat activity sensor
  # w is the width in seconds for a moving window 
  # a respiration rate of 16 (median) will have 3.75 sec cycles
  # y is the threshold value (y scale of the polygraph chart) where 20 is 1% of the range
  # an artifact is marked if the abs sum y distance is less than y for each w segment
  # seg is the number of measurements to check for each 3.75 sec segment
  # 
  # output is a vector with the sample indices where artifacts were observed
  ##
  # initialize an output vector
  outVc <- rep(0, length(DAT))
  # initialize the length of an apnea segment
  winLen <- c(1:round(w * cps)) # cps is a value in the global envir (30 cps)
  # select the segment indices to check
  winPoints <- c(1, cumsum(rep(round(length(winLen)/seg), (seg-1))), length(winLen))
  # iterate over the time series data with a moving window
  i=1
  for(i in 1:(length(outVc)-length(winLen)+1)) {
    # mark an artifact if the y axis change is small for the w period
    if(sum(abs(diff(DAT[i+winPoints-1]))) < round(y)) {
      outVc[i+winLen-1] <- "Artifact"
    }
  }
  which(outVc != "0")
  # end activityRestrictionFn()
}



activityDBFn <- function(DAT=chartDF$c_Move1Proc, inF=1.5, outF=3, fences=NULL, side=NULL) {
  # R function to extract deep breaths from the seat activity sensor data
  # using a Tukey fence method
  # by extracting the inhalation peaks and comparing them to the median and the interquartile range
  # April 8, 2025
  # Raymond Nelson
  ####
  # DAT input is a time series vector for a chart (X to XX)
  # DAT can be the raw or smoothed activity data, or the inhalation peak to peak line
  # quantiles can be input as parameters when using the peak to peak line
  # or can be computed from the DAT if they are NULL
  # inF is the inner Tukey fence
  # outF is the outer Tukey fence
  # fences can be "inner" "outer" or "both"
  # side can be "upper" "lower" or "both"
  #
  # calls the minPeak and maxPeak functions from the sigProgHelper.R script
  #
  # output is a vector with the sample indices where deep breath activity artifacts were observed
  ####
  # plot.ts(x)
  ## initialize the output vector for the upper and lower respiration peaks that exceed the fences
  pkVc <- NULL
  ## compute the quartiles, interquartile range, and Tukey fences
  {
    # compute the quartiles
    q25 <- quantile(DAT, na.rm=TRUE, .25)
    q50 <- quantile(DAT, na.rm=TRUE, .5)
    q75 <- quantile(DAT, na.rm=TRUE, .75)
    # calculate the interquartile range
    iqr <- abs(q75[1] - q25[1])
    # initialize the Tukey fence values 
    {
      # index the fences with teh median 2025Apr15
      # inner fences
      lfI <- q25[1] - (inF * iqr)
      ufI <- q75[1] + (inF * iqr)
      # outer fences
      lfO <- q25[1] - (outF * iqr)
      ufO <- q75[1] + (outF * iqr)
    }
  }
  ## compare the data values to the Tukey fences
  if(fences=="outer") {
    # outer fences only
    if(side=="upper") {
      pkVc <- which(DAT>=ufO)
    } else if(side=="lower") {
      pkVc <- which(DAT<=lfO)
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfO), which(DAT>=ufO))))
    }
  } else if( any(fences=="inner", fences=="both", is.null(fences)) ) {
    # inner fences will capture data that exceed both the inner and outer fences
    if(side=="upper") {
      pkVc <- which(DAT>=ufI)
    } else if(side=="lower") {
      pkVc <- which(DAT<=lfI)
    } else {
      # both upper and lower sides
      pkVc <- sort(unique(c(which(DAT<=lfI), which(DAT>=ufI))))
    }
  } 
  ## output
  return(pkVc)
  # end activityDBFn()
}



activityPatternFn <- function(DAT=chartDF$c_Move1Max, limit=q50, side="lower") {
  # R function to extract changes in physical activity pattern using the seat activity sensor data
  # using the quartile values and peak-to-peak lines
  # by extracting the inhalation peaks and comparing them to the median and the interquartile range
  # March 8, 2025
  # Raymond Nelson
  ####
  #
  # DAT is the peak to peak line for the activity sensor
  # peaks indicate inhalation and exhalation cycles
  #
  # activity sensor data includes normal respiratory oscilation,
  # in addition to pulse information
  # physical movement, covert muscle activity, and other covert activity
  # 
  # limit is the cutoff value (the median of the smoothed Move1 time series data)
  # 
  # side is "greater" or "lesser"
  # 
  # output is a vector of sample indices where the peak-to-peak line 
  # has unexpectedly crossed the cutoff or limit value (median)
  #
  ####
  if(side=="upper") {
    return( which(DAT > limit) )
  } else if(side=="lower") {
    return( which(DAT < limit) )
  } else {
    print("side must be either upper or lower")
  }
  # end activityPatternFn()
}



tonicActivityFn <- function(DAT=chartDF$c_Move1ProcMA, q25, q75) {
  # R function to extract changes in tonic activity using the seat activity sensor data
  # using the median value and interquartile range
  # by extracting the inhalation peaks and comparing them to the median and the interquartile range
  # April 10, 2025
  # Raymond Nelson
  ####
  # DAT is the tonic activity line, obtained by smoothing the seat activity sensor data
  # q25 is the 25th percentile from X to XX
  # q75 is the 75th percentile
  #
  # # output is a vector of sample indices where the tonic activity has changed significantly
  ####
  # initialize an output vector
  tncVc <- NULL
  # intialize. the tonic value
  tncVal <- rep(0, times=length(DAT))
  # locate tonic samples that are outside the interquartile range
  tncVal[which(DAT > q75)] <- 1
  tncVal[which(DAT < q25)] <- -1
  # keep only the change in tonicity
  tncVal[(which(tncVal[1:(length(tncVal)-1)] == tncVal[2:length(tncVal)]) + 1)] <- 0
  # output
  return( which(tncVal != 0) )
  # end tonicActivityFn()
}



activityAmplitudeFn <- function(x, y) {
  # R function to extract changes in sea actitvity data using min-peak and max-peak lines
  # by comparing the difference to the interquartile range
  # April 22, 2025
  # Raymond Nelson
  ####
  # x is a time series vector
  # of the amplitude difference between the seat activity max-peak and min-peak lines
  # min-peak and max-peak lines are computed after data acquistion
  # after signal processing, scaling, and offsetting
  # difference must be computed prior to calling this function
  # y is the threshold range at which the distance betwen peak-to-peak lines
  # is interpreted as significantly reduced
  # y is obtained via the interquartile range
  # #
  # output is a vector of sample indices where the amplitude differences is reduced to a significant degree
  # significant reduction of amplitude is defined using the IQR
  ####
  return( which(x < y) )
  # end activityAmplitudeFn()
}



##############. main function  #################



newActivityCheckFn <- function(chartDF=chartDF) {
  # R function to extract phyiscal activity artifacts from seat activity sensor data
  # main function to check for significant physical activity 
  # input is the chartDF
  # output is the chartDF with artifacts labeled in the Move1_a vector
  # 
  # also calls other helper functions in this script
  # 
  ####
  
  {
    examName <- chartDF$examName[1]
    seriesName <- chartDF$seriesName[1]
    chartName <- chartDF$chartName[1]
    sensorName <- "activity"
    # get the activity sensor column
    tsData <- chartDF$c_Move1Proc
    # exit for short charts less than 20 seconds
    if(nrow(chartDF) < 600) { return(chartDF) }
    # exit if the data are NA
    if(length(which(is.na(chartDF$c_Move1)))==nrow(chartDF)) return(chartDF)
    # exit if the data are unresponsive
    if(max(chartDF$c_Move1) == min(chartDF$c_Move1)) return(chartDF)
  }
  
  #### reset the artifact vector in the chartDF ####
  
  chartDF$Move1_a <- "0"
  
  #### create a pre and post buffer for verbal answers #### 
  
  {
    answerRows <- which(chartDF$Label %in% c("YES", "Yes", "No", "NO", "ANS", "Ans"))
    answerBuffOn <- answerRows - 1.5 * cps
    answerBuffOff <- answerRows + 1.5 * cps
    answerBuffer <- NULL
    if(length(answerRows) > 0) {
      for(i in 1:length(answerBuffOn)) {
        answerBuffer <- c(answerBuffer, answerBuffOn[i]:answerBuffOff[i])
      }
    } 
  }
  
  #### initialize an artifact vector #### 
  
  {
    DBVc <- NULL
    APVc <- NULL
    maxVc <- NULL
    minVc <- NULL
    xOut <- NULL
  }
  
  #### compute the IQR #### 
  
  {
    ## compute the quartiles
    q25 <- quantile(chartDF$c_Move1Proc, na.rm=TRUE, .25)
    q50 <- quantile(chartDF$c_Move1Proc, na.rm=TRUE, .5)
    q75 <- quantile(chartDF$c_Move1Proc, na.rm=TRUE, .75)
    ## calculate the interquartile range
    iqr <- abs(q75[1] - q25[1])
    ## fix the iqr for very stable or very unstable data
    {
      # sensitivity to DBs may increass when the activity data are scaled to a small display value
      # this occurs because the iqr becomes restricted as a result of little variation
      # to reduce hypersensitivity a min iqr value
      # can be obtained from the scalVals in the NCCAASCII_init.R scripe
      minIQR <- round(scaleVals['activity'] * .15)
      if(iqr <= minIQR) iqr <- minIQR
      # sensitivity to DBs may decrease when the data are unstable
      # this is because the IQR becomes larger with more variation
      # to reduce hyposensitivity the iqr can be replace with a max value
      maxIQR <- round(scaleVals['activity'] * .67)
      if(iqr >= maxIQR) iqr <- maxIQR
    }
  }

  #### initialize the Tukey fence values ####
  
  {
    ## initialize the fences
    inF <- 1.5
    outF <- 3
    # inner fences
    lfI <- q25[1] - (inF * iqr)
    ufI <- q75[1] + (inF * iqr)
    # outer fences
    lfO <- q25[1] - (outF * iqr)
    ufO <- q75[1] + (outF * iqr)
  }
  
  #### 1. deep breath and excess movement activity #### 
  
  {
    ## deep breath events that appear on the seat activity sensor ##
    
    ## also sensitive to excessive movement activity ##
    
    # inititalize some vectors to 
    DBVc1 <- NULL
    DBVc2 <- NULL
    DBVc3 <- NULL
    
    # call a function to locate deep breaths that influence the activity sensor data
    # DBVc1 <- activityDBFn(DAT=chartDF$c_Move1Proc, inF=inF, outF=outF, fences="outer", side="both")
    
    # remove artifacts associated with the verbal answer
    # DBVc1 <- DBVc1[which(!(DBVc1 %in% answerBuffer))]
    
    #s inhalation peaks only
    DBVc2 <- activityDBFn(DAT=chartDF$c_Move1Proc, inF=inF, outF=outF, fences="outer", side="upper")
    
    # 2025Apr21 use the exhalation peaks to improve sensitivity to movement artifacts
    DBVc3 <- activityDBFn(DAT=chartDF$c_Move1Proc, inF=inF, outF=outF, fences="outer", side="lower")
    
    # combine them
    DBVc <- sort(unique(c(DBVc1, DBVc2, DBVc3))) 
    
    # DBVc <- NULL
  }
  
  #### 2. activity restriction events #### 
  
  {
    ## cessation of normal movement activity during respiration apnea and other unusual behavior ##
    
    # 2025Apr21
    # use 1/2 the IQR as the min range for freeze/apnea artifacts
    
    # call a function to check for cesation of activity due to respiration apnea
    # 2025Apr21 segments are computed to sample the moving window at .25 sec intervals
    # 2025Apr21 w (moving window) approximates the width of the median respiration rate (16 cpm)
    APVc <- activityRestrictionFn(DAT=chartDF$c_Move1Proc, w=3.75, y=iqr/3, seg=10)
    
    # scaleVals['activity'] / 5
    
    
    # APVc <- NULL
  }
    
  #### 3. significant changes in physical activity pattern #### 
  
  {
    ## compare the oscillation peaks to a limit value obtained from the IQR
    
    # upper activity peaks below the median 
    # maxVc <- activityPatternFn(DAT=chartDF$c_Move1Max, limit=limitVal, side="lower")
    maxVc <- activityPatternFn(DAT=chartDF$c_Move1Max, limit=q25, side="lower")

    # lower activity peaks above the median
    # minVc <- activityPatternFn(DAT=chartDF$c_Move1Min, limit=limitVal, side="upper")
    minVc <- activityPatternFn(DAT=chartDF$c_Move1Min, limit=q75, side="upper")
      
    actVc <- sort(unique(c(maxVc, minVc)))
    
    # remove artifacts associated with the verbal answer
    actVc <- actVc[which(!(actVc %in% answerBuffer))]
    
    # actVc <- NULL
  }
  
  #### 4. changes in tonic activity level ####
  
  {
    ## compare a slow moving average to the interquartile range
    
    # 2025Apr26 not effective at locating activity artifacts
    
    tncVc <- NULL
    
    # tncVc <- tonicActivityFn(DAT=chartDF$c_Move1ProcMA, q25=q25, q75=q75)
  }
  
  #### 5. compare the activity amplitude to the IQR ####
  
  {
    ampVc <- NULL
    
    # 2025Apr26 not effective at 
      
    # ampVc <- activityAmplitudeFn(x=(chartDF$c_Move1Max - chartDF$c_Move1Min), y=iqr/3)
    
    # remove artifacts associated with the verbal answer
    # ampVc <- ampVc[which(!(ampVc %in% answerBuffer))]
    
    
  }
  
  #### aggregate the artifacts and submit them to the chart data frame ####
  
  {
    artifactVc <- sort(unique(c(DBVc, APVc, actVc, tncVc, ampVc)))
    
    chartDF$Move1_a[artifactVc] <- "Artifact"
  }
  
  #### output ####
  
  return(chartDF)
  
} # end activityCheckFn() function







