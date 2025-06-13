# amplitude extract function for EDA and Cardio data
# 10-31-2015
# Raymond Nelson
#
####


# source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R')


amplitudeExtractFn <- function(extractList=AutoExtractList, env.params=env.params)  {
  # function to extract the amplitude of EDA and cardio rise or increase in response to a stimulus
  # 10-31-2015
  # Raymond Nelson
  
  # called by the EDAExtractFn() and CardioExtactFn() in the EDAExtract.R  and CardioExtract.R scripts 
  
  # extractList is a list of all input info needed to extract the response
  # extractList is created by the EDAExtractFn and CardioExtractFn
  # 1. begin is a scalar indicating the row number of the onset of the stimulus question
  # 2. end is a scalar indicating the row number of the end of the stimulus question
  # 3. answer is a scalar indicating the row number of the verbal answer
  # 4. segmentName is the name of the stimulus event
  # 5. segmentTitle is the full segment name including examName, seriesName, chartName and segmentName
  # 6. dataVector is is a vector of time series data for a single stimulus segment
  # 7. artifactVector is a time series vector of artifacts from the segment DF
  # 8. useArtifacts is a logical value to control the integration of artifact and feature extraction
  
  # env.params is a list of environment parameters used for feature extraction
  # env.params is created by the EDAExtractFn and CardioExtractFn
  # 1. dataRate is a scalar that indicates that data rate in samples per second
  # 2. Lat is the required latency after stimulus onset before which a responses is not evaluated
  # 3. ROWEnd is a scalar indicating the end of ROW in seconds after verbal answer
  # 4. nSmooth is the number of samples to smooth and ignore slope changes of small duration
  # 5. strictWindow <- FALSE # use TRUE to stop responses at the end of the measurement window
  # 6. strictROW <- FALSE # use TRUE to ignore all positive slope segments that begin after end of ROW
  # 7. prop <- .5 # is the cutoff proportion of descent from max peak value for the descentStop/descentRule
  # 8. descentStop <- uses the descentRule paramenter 0=off, 1=on, 2=only after ROWEndRow
  # 9. slopeChange <- 0 will disable, 1 will enable and 2 will use significant changes when there is no positive slope onset in ROW
  
  # could get the env.params from the global environment
  # seems more controlled to input the env.params as  list
  
  # procedure
  # 1. locate the row index and value for the onset of all positive slope segments after latency
  # 1a.  infer the a response onset from a significant change in + slope energy 
  # 1b. option: only infer repsonse onset from slope change where there is no + slope onset in the ROW
  # 2a. strictly exclude positive slope segments for which the onset is after the ROWEnd
  # 2b. option included all positive slope onset points from latetency 
  # 3. locate the row index and value for all positive slope peaks after the first onset point
  # 4. keep all peaks in the EW plus one additional peak if the slope is + at the end of the EW
  # 4a. option: strictly exclude all peaks after the endRow even if the slope is + at the end of the EW
  #    endRow is the end of the 15 second stimulus segmemt
  # 5. exclude + slope changes that occur after the data have descended to a new lower + slope onset value
  # 5a. option: keep all + slope onsets in the ROW and only exclude onset points after ROWEndRow when data have descended below the onset value
  # 7. option: exclude changes that occur after the data have descended 50% (or any arbitrary proportion) from the previous max peak
  # 8. select the onset and peak for the max change value for each + slope onset and all subsequent peaks that are not excluded
  
  # output is a list of extracted information 
  
  ######## begin ##########
  
  # extractList<-AutoExtractList
  # extractList<-ManualExtractList
  
  # get the information for the 8 items in the input list
  
  {
    Begin <- as.numeric(extractList$begin)
    End <- as.numeric(extractList$end)
    Answer <- as.numeric(extractList$answer)
    segmentName <- extractList$segmentName
    segmentTitle <- extractList$segmentTitle
    useArtifacts <- extractList$useArtifacts
    
    tsData <- extractList$dataVector
    
    artifactVector <- extractList$artifactVector
  }
  
  # get the information from the env.params input List
  # 2018-11-24 maybe could use the same names as the global env variables
 
  {
    cps <- env.params$dataRate
    Lat <- env.params$Lat
    useROW <- env.params$useROW
    ROWStart <- env.params$ROWStart
    ROWStop <- env.params$ROWStop
    ROWEnd <- env.params$ROWEnd
    nSmooth <- env.params$ignore
    strictWindow <- env.params$strictWindow
    strictROW <- env.params$strictROW
    descentRule <- env.params$descentRule
    descProp <- env.params$descProp
    slopeChangeRule <- env.params$slopeChangeRule
  }
  
  ### set the starting row and ending row
  
  {
    startRow <- 1
    DFRows <- length(tsData)
  }
  
  ####   initialize the event indices   ####
  
  {
    # prestimRow is usually the first row of the time series vector
    prestimRow <- Begin - (startRow-1) - (cps*prestimSeg) 
    if(prestimRow<=0) prestimRow <- 1
    # onset of the stimulus in the time series vector
    onsetRow <- Begin - (startRow-1) 
    # end of the measurement window
    endRow <- onsetRow + (cps*measuredSeg) - 1 
    if(endRow > DFRows) endRow <- DFRows
    # end of the question stimulus
    offsetRow <- End - (startRow-1) 
    if(offsetRow >= (endRow-2)) offsetRow <- endRow - 2
    # repsonse latency period
    latRow <- onsetRow + cps*Lat - 1
    if(latRow >= (endRow-4)) latRow <- endRow - 4
    # verbal answer
    answerRow <- Answer - (startRow-1) 
    if(answerRow==offsetRow) answerRow <- offsetRow + 1
    # set the ROWStartRow using the ROWStart variable
    # normally the latency index
    # can be "stimOn" "stimOff" "latency" or "verbalAnswer"
    ROWStartRow <- switch(ROWStart,
                          "latency"=latRow,
                          "stimOn"=onsetRow,
                          "stimOff"=offsetRow,
                          "verbalAnswer"=answerRow )
    # set the end of the response onset window
    # response onset window is typically 5 seconds after the verbal answer but can be set arbitrarily
    # ROWStop sets the reference point for the ROW End
    # can be "answer" "onset" "offset" "latency" or "EWEnd"
    # set the ROWStopRef to select the reference point for ROW End
    ROWStopRef <- switch(ROWStop,
                         "answer"=answerRow,
                         "onset"=onsetRow,
                         "offset"=offsetRow,
                         "latency"=latRow,
                         "EWEnd"=endRow )
    ROWEndRow <- ROWStopRef + (cps*ROWEnd) - 1
    if(ROWEndRow > (DFRows-3)) ROWEndRow <- DFRows - 3
  }
  
  ####  locate the onset of positive slope segements ####
  
  {
    # make a vector of slope values
    # theSlope <- slopeDir(x=tsData)
    diff1 <- diff(tsData)
    theSlope <- ifelse(diff1==0,
                       # ifelse is vectorized and requires no control loop
                       theSlope <- 0,
                       ifelse(diff1>0,
                              theSlope <- 1,
                              theSlope <- -1) )
    theSlope <- c(0,theSlope)
    
    # use a helper function to smooth the slope by removing slope changes of small duration
    # theSlope <- smoothSlope(x=theSlope, nSmooth=nSmooth)
    xPos <- ifelse(theSlope == 1, 1, 0)
    # make an vector of zeros
    xPos1 <- rep(0, times=length(xPos))
    # make a run sum for all positive slope segements
    for (i in 2:length(xPos)) {
      if(xPos[i] != 0) { xPos1[i] <- xPos[i] + xPos1[i-1] }
    }
    # propagate the positive slope run length
    for (j in length(xPos1):2) {
      if(xPos1[j] != 0 & xPos1[j-1] != 0) { xPos1[j-1] <- xPos1[j] }
    }
    # remove short runs
    for (k in 1:length(xPos1)) {
      if(xPos1[k] <= nSmooth) { xPos1[k] <- 0 }
    }
    
    # xPos1 is a vector of 0s
    # with other integers indicating the length of + slope runs
  }
  
  #### negative slope segments ####
  
  {
    xNeg <- ifelse(theSlope == -1, -1, 0)
    # make a vector of zeros
    xNeg1 <- rep(0, times=length(xNeg))
    # run sum for all negative slope segments
    for (l in 2:length(xNeg)) {
      if(xNeg[l] != 0) xNeg1[l] <- xNeg[l] + xNeg1[l-1] }
    # propagate the run length
    for (m in length(xNeg1):2) {
      if(xNeg1[m] != 0 & xNeg1[m-1] != 0) { xNeg1[m-1] <- xNeg1[m] } }
    # remove short runs
    for (o in 1:length(xNeg1)) {
      # use o so we do not abuse the n variable
      # uses >= -nSmooth instead of >= nSmooth because we are working on negative slope segments
      if(xNeg1[o] >= -nSmooth) xNeg1[o] <- 0 }
    # xNeg1 is a vector of 0s
    # with other integers indicating the length of - slope runs
  }
  
  #### combine the positive and negative vectors ####
  
  {
    theSlope <- ifelse(xPos1 > 0,
                       theSlope <- xPos1,
                       ifelse(xNeg1 < 0,
                              theSlope <- xNeg1,
                              theSlope <- 0) )
    
    # fill the zero slope segments with the preceding value for positive slope segments
    # theSlope <- fillSlope(x=theSlope)
    for (i in 2:length(theSlope)) { 
      if(theSlope[i] == 0) theSlope[i] <- theSlope[i-1] 
    }
    
    # keep only 1, -1 and 0 values to indicate the slope
    theSlope <- ifelse(theSlope > 0,
                       theSlope <- 1,
                       ifelse(theSlope < 0,
                              theSlope <- -1,
                              0) )
    # at this point there should be no zero slope indices
    
    # print(theSlope)
    # stop()
    
    # use a helper function to make a vector of positive and non-positive slope activity
    # theSlope <- fillSlope(smoothSlope(x=slopeDir(x=tsData), nSmooth=nSmooth))
    
    # posSlope <- positiveSlope(x=theSlope)
    posSlope <- theSlope
    posSlope <- ifelse(theSlope>=1,
                       posSlope <- 1,
                       posSlope <- 0)
    # posSlope is a vector of 0s and 1s for positive slope segments
  }
  
  #### keep only the positive slope onset indices ####
  
  # posSlopeOnset <- positiveOnset(positiveSlope(x=theSlope))
  # posSlopeOnset <- positiveOnset(x=posSlope)
  # posSlopeOnset <- rep(0, times=length(posSlope))
  posSlopeOnset <- ifelse(posSlope==0,
                          # compare every x + next x > 0
                          ifelse( ( posSlope[1:(length(posSlope)-1)] + 
                                      posSlope[2:length(posSlope)] ) > 0,
                                  1,
                                  0 ),
                          0 )
  
  #### private function to determine the significance of an up slope change ####
  
  maxSlopeChangeFn <- function(x=tsData) {
    # new function to determine the max significant value 
    # in a series of significant changes in positive slope
    # supercedes the slopeChangeFn 
    # which always selected the first sig value in a run 
    # 11-24-2016 raymond nelson
    # improved 10-7-2018
    # called by the amplitudeExtractFn() function
    # used to impute a response onset 
    # within a positive slope segment 
    # such as when the slope is positive prior to stimulus onset
    # compare each nPre period (seconds) with the next nPost seconds
    ###
    # x input is the vector of time series data for a stim segment
    # including 10 prestimulus seconds 
    # and 10 additional seconds after the evaluation window
    ### these parameters are set in the global environment by the init script
    # nPre is the number of pre change seconds
    # nPost is the number of post change seconds
    # aChange is the level of sig for the z test # currently .999
    # p=.999 will be +3.09 standard deviations
    # p=.9999 will be +3.72 standard deviations
    # p=.998650102 = 3 SD
    # 4 standard deviations is p=.9999683
    # cps is data sampling rate # normally 30cps 
    ###
    # output is a vector of 0s the same length as input 
    # and including +1 change in slope energy,
    # located at the max zScore during a series of sig changes in nPre and nPo,
    ####
    
    # tsData <- x
    
    # calculate the difference for all values in the x input
    # modified 10-7-2018 to use the absolute value
    xDiff <- c(0, abs(diff(x)))
    
    ###
    
    # use a function to compute the z score cutpoint from the aChange quantile
    zCut <- qnorm(aChange)
    
    # round the nPre and nPost segments to the nearest sample
    preLen <- round(cps*nPre,0)
    postLen <- round(cps*nPost,0)
    tonicLen <- round(cps*tonicSec,0)
    
    # initialize the output vector
    y <- rep(0, times=length(xDiff))
    
    # exit if the input vector is too short
    if(length(xDiff) <= (preLen+1)) return(y)
    
    ###
    
    # initialize some vectors
    preDiffMean <- rep(0, times=length(xDiff))
    preDiffSD <- rep(0, times=length(xDiff))
    postDiffMean <- rep(0, times=length(xDiff))
    zScore <- rep(0, times=length(xDiff))
    
    ###
    
    # calculate all preDiff means
    for (i in preLen:length(xDiff)) {
      preDiffMean[i] <- mean(xDiff[(i-preLen+1):i], na.rm=TRUE)
    }
    
    # calculate all preDiff standard deviations
    # uses a helper function sdp() to compute the population standard deviation
    for (i in preLen:length(xDiff)) {
      preDiffSD[i] <- sdp(xDiff[(i-preLen+1):i])
      # preDiffSD[i] <- sd(xDiff[(i-preLen+1):i])
    }
    
    # calculate all postDiff means
    for (i in (preLen+1):(length(xDiff)-postLen+1)) {
      postDiffMean[i] <- mean(xDiff[i:(i+postLen-1)], na.rm=TRUE)
    } 
    
    # compute the z-score for all posDiff means compared to the preDiff means
    for (i in (preLen+1):(length(xDiff)-postLen+1)) {
      # increment the loop if preDiffSD[(i-1)] == 0
      # to avoid NaN result when divide by zero
      if(preDiffSD[(i-1)] == 0) next()
      # calculate the zScore for each postDiff mean,
      # using the mean and SD from the previous sample
      zScore[i] <- (postDiffMean[i] - preDiffMean[(i-1)]) / preDiffSD[(i-1)]
    }
    
    # next determine which values are significant in the zScore vector
    zScore[which(zScore < zCut)] <- 0
    zScoreIdcs <- which(zScore != 0)
    
    # make a vector of slope data for the input
    slopeDat <- fillSlope(smoothSlope(slopeDir(x), 
                                      nSmooth=round(cps*ignoreTonicChange,0)))
    
    # 10-7-2018 remove zScore for which the tonicLen slope is not +
    if(length(zScoreIdcs) > 0) {
      # i=26
      for (i in 1:length(zScoreIdcs)) {
        preSlope <- zScoreIdcs[i]-tonicLen+1
        preSlope <- ifelse(preSlope < 1, 1, preSlope)
        if( sum(slopeDat[preSlope:zScoreIdcs[i]]) != tonicLen ) {
          zScore[zScoreIdcs[i]] <- 0
        }
      }
    }
    
    # then remove zScores for non-ascending segments
    zScore[which(slopeDat != 1)] <- 0
    
    # first initialize the holding variables
    z <- 0
    zKeep <- NULL
    
    # then finally iterate over the zScore vector,
    # to locate the max zScore in each run of non-zero values
    # and remove zScores for which the data are not,
    # ascending throughout the preDiff
    i=342
    for (i in 1:length(zScore)) {
      # increment the loop if the current value is zero
      if(zScore[i] == 0) next()
      # set the z holding variable to the non-zero i index
      z <- i
      # another loop to the max index for each non-zero run,
      # in the zScore vector
      # cannot use which.max() because there may be,
      # multiple non-zero runs in the zScore vector
      j=z+1
      for (j in (z+1):length(zScore)) {
        # stop the loop if the zScore value is zero
        if(zScore[j] == 0) {
          z <= j+1
          break()
        }
        # change the z index to advance the loop
        # if zScore[j] is greater than zScore[z]
        if(zScore[j] > zScore[z]) z <- j
      } # end inner loop
      
      # # keep the z index if the value is greater than the last zKeep value
      # if(is.null(zKeep)) {
      #   zKeep <- c(zKeep, z)
      # } else {
      #   # this the problem 4/30/2018
      #   if(zScore[z] > zScore[zKeep[length(zKeep)]]) zKeep <- c(zKeep, z)
      # }
      
      zKeep <- c(zKeep, z)
      
    } # end for loop
    
    zKeep <- unique(zKeep)
    # zScore[zKeep]
    
    # initialize the output vector
    # y <- rep(0, times=length(xDiff))
    
    # add the slope change onset indices to the output vector
    y[zKeep] <- 1
    # y is now a vector of 0s 
    # with the onset of signficant changes in slope marked by 1
    
    # output the result
    return(y)
    
  } # end maxSlopeChangeFn
  
  #### call the function to infer onset via sig change in + slope variance ####
  
  {
    # sChange <- slopeChangeFn(x=tsData) 
    # new slope change function 11-25-2016
    sChange <- maxSlopeChangeFn(x=tsData) 
    # which(sChange == 1)
    
    # set sChange onsets to 0 during an sChange latency period
    sChange[1:(sChangeLat*cps+Begin-1)] <- 0
    # sChange is a vector of 0s 
    # with 1s to indicate significant changes in upward slope activity
  }
  
  ###### evaluate tonicity - still in progress 10-23-2018 ######
  
  {
    # evaluate tonicity
    
    # calculate the slope value
    # tsData <- chartDF$c_AutoEDA
    # tonicSlopeVector <- tonicSlope(x=(tsData+2000), time=, period=.0333333, rate=cps, gain=1000)
    # assign("tonicSlopeVector", tonicSlopeVector, pos=1)
    
    #  tsSlopeVal <- abs(tsSlopeVal) * sign(mySlope1)
    # tsSlopeVal can be compared to the tonicValue scalar from the global env
    # to determine tonicity as it may affect the onset or end of response
    # not used 3/30/2018
    
    # tonicValue <- 20
    # tonicVa;ie is 200 set in the init
    
    # remove sChange values if the slope exceeds the tonicValue scalar
    # which(tsSlopeVal[which(sChange==1)] > tonicValue)
    # sChange[which(sChange==1)][(tsSlopeVal[which(sChange==1)] > tonicValue)] <- 0 
    
    # not used 3/30/2018 - does not increase the effect size
  }
  
  ############ combine the sChange and xOnset vectors ############
  
  if(slopeChangeRule != 0) {
    # slopechange is a switch to control the use of a response onset location
    # as a function of a significant change (increase) in positive slope
    # when the slope is positive at stimulus onset 
    # slopeChangeRule = 0 will disable the use of signficant changes in positive slope as a response onset
    # slopeChangeRule = 1 will enable the use of significant changes in positive slope as a response onset
    # slopeChangeRule = 2 enable use of significant changes in + slope 
    # but only when there is no onset for a positive slope segement during the ROW
    ###
    if(slopeChangeRule==1) {
      # include all sChange to xOnset
      posSlopeOnset[which(sChange==1)] <- 1
    } else if(slopeChangeRule==2) {
      # including sChange only if there are no xOnset values in the ROW
      if(length(which(posSlopeOnset[ROWStartRow:ROWEndRow]==1)) == 0) {
        # and only if there are any onset values in sChange
        if(length(sChange) > 0) posSlopeOnset[which(sChange==1)] <- 1 
      }
    } # end else if slopeChangeRule == 2
  } # end if slopeChangeRule != 0
  
  #### finalize the xOnset rows ####
  
  {
    # get the sample indices for upward slope onsets
    xOnset <- which(posSlopeOnset == 1)
    
    # in case there is no positive slope onset
    if(length(xOnset) == 0) xOnset <- endRow
    
    # keep onset rows after the required response latency period
    xOnset <- xOnset[xOnset >= ROWStartRow]
    
    # keep only those onset rows that occur before ROWEndRow
    xOnset <- xOnset[xOnset <= ROWEndRow]
    
    # fix xOnset again if there is no response onset in the ROW
    if(length(xOnset) == 0) {
      xOnset <- endRow 
      # may need to be endRow -1 if response onset and end cannot be the same
    }
    
    # fix condition where xOnset is >= the number of rows in the data frame  
    if(xOnset[1] >= length(tsData)) xOnset <- length(tsData)
  }
  
  #################   work on the response peaks   ###################
  
  {
    # use a helper function to locate the peak of all positive slope sections
    xPeak <- slopePeak(x=theSlope)
    
    # add the ROWEndRow to the xPeak1 vector if it is positive
    if(posSlope[ROWEndRow] == 1) xPeak[ROWEndRow] <- 1
    
    # add the endRow slope to xPeak1 vector if it is positive.
    if(posSlope[endRow] == 1) xPeak[endRow] <- 1 
    
    # add the last sample in the tsData vector if the slope is + at the end of the vector
    # without this the measurement will end at the strict EW if the slope stays + to the end of the tsData vector
    if(posSlope[DFRows] == 1) xPeak[DFRows] <- 1
    
    # construct the vector of xPeak indices
    xPeak <- which(xPeak == 1)
    
    if(length(xPeak) == 0) xPeak <- endRow
    
    # Keep only those slope peaks that are after the first xOnset row
    # remove peaks that occur before the first onset after latency
    # xOnset[1] is after the latency
    xPeak <- xPeak[xPeak >= xOnset[1]]
    # xPeak will be NA if there are no xOnset indices
  }
  
  ###### work on the ROWEndRow and the strictROW input parameters #######
  
  # keep all slope peaks that are before the ROWEndRow 
  # plus one additional peak if the slope at ROWEndRow is positive 
  # it is possible the the additional peak will be outside the measurement window 
  # if FALSE this will keep all peaks in xPeak vector
  if(strictROW == TRUE) {
    ifelse(posSlope[ROWEndRow]==1,
           # to keep only one additional peak after ROWEndRow if the slope at ROWEndRow is +
           # xPeak includes the ROWEndRow and EndROW if the slope is + at these indices
           xPeak <- xPeak[1:(length(which(xPeak <= ROWEndRow))+1)],
           # this will keep only those xPeak indices that are less than ROWEndRow if the slope is not + at ROWEndRow 
           xPeak <- xPeak[xPeak <= ROWEndRow] 
    ) 
  } # end if strictROW == TRUE
  
  ######## work on the endRow and strictWindow parameters ########
  
  # keep only those slope xPeaks that are before endRow (end of measurement window)
  # if TRUE this will keep only those peaks in the measurement window 
  # including the endRow if the there are xPeak items after the endRow and the slope is + at endRow
  ifelse(strictWindow == FALSE,
         { # check to see if there is at least one onset in the ROW
           ifelse(xOnset[1] <= ROWEndRow, 
                  # keep all xPeak indices <= endRow plus one additional xPeak if the slope is + at the end of EW 
                  ifelse(posSlope[endRow] == 1,
                         xPeak <- xPeak[1:(length(which(xPeak <= endRow)) + 1)],  
                         # if not + slope at endRow keep only peaks < endRow
                         { xPeak <- xPeak[xPeak <= endRow] 
                         
                         ### keep only 2 xPeak indices after ROWEndRow before endRow
                         # this is to prevent the use of peaks after multiple pos/neg slope changes after ROWEndRow
                         # does not work 3-5-2017 when the data are noisy
                         # xPeak <- c(xPeak[which(xPeak <= ROWEndRow)], xPeak[which(xPeak > ROWEndRow & xPeak <= endRow)[c(1:2)]])
                         
                         } ),
                  # if no XOnset rows are less than ROWEndRow then keep only xPeaks less than endRow
                  # this is to prevent unexpectedly keeping an additional peak when there is no onset in the ROW
                  xPeak <- xPeak[xPeak <= endRow]) 
         },
         { # strictWindow TRUE will stop responses at the end of the measurement window
           xPeak <- xPeak[xPeak <= endRow] 
           # keep only 2 xPeak indices after ROWEndRow before endRow
           # commented out Mar 25, 2020 to improve the cardio feature extraction
           # xPeak <- c(xPeak[which(xPeak <= ROWEndRow)], xPeak[which(xPeak > ROWEndRow & xPeak <= endRow)[c(1:2)]])
           # remove NAs 2018-11-12
           xPeak <- xPeak[!is.na(xPeak)]
         }
  ) # end ifelse strictWindow==FALSE
  
  # remove NAs 2018-11-12
  xPeak <- xPeak[!is.na(xPeak)]
  # xPeak will be empty if all peaks are NA because there is no onset
  
  ##### remove xPeak values that are after the data has become tonic #####
  
  {
    
    # # 3/30/2018
    # # calculate the slope value again
    # tsSlopeVal2 <- tonicSlope(x=(tsData+2000), time=3, rate=cps)
    # tsSlopeVal2 <- abs(tsSlopeVal2) * sign(theSlope)
    # tsSlopeVal2 <- abs(tsSlopeVal2)
    # # tsSlopeVal can be compared to the tonicValue2 scalar from the global env
    # # to determine tonicity as it may affect the onset or peak of response
    # 
    # tonicValue2 <- 200
    
    # # remove xPeak values if the slope becomes tonic after the first xOnset
    # # keep only xPeak values after the first xOnset
    # xPeak <- xPeak[which(xPeak > xOnset[1])]
    # # determine when the slope become tonic
    # if(length(xPeak) > 0) {
    #   # set the stop index
    #   xPeakStop <- NULL
    #   xPeakStop <- which(tsSlopeVal2[xPeak[1]:length(tsSlopeVal2)] < tonicValue2)[1] + 
    #     (xPeak[1]-1)
    #   # remove xPeak indicides after the slope becomes tonic add the tonic index
    #   if(length(xPeakStop) > 0 ) {
    #     xPeak <- c(xPeak[1:min(which(xPeak < xPeakStop[1]))], xPeakStop[1])
    #   }
    #   # xPeak <- xPeak[1:min(which(tsSlopeVal2[xPeak] <= tonicValue2))]
    # }
    # not used 3/30/2018 - does not increase the effect size
    
  }
  
  ######## fix a potential problem with empty xPeak vector ########
  
  # set the xPeak to the first onset if no peak indices
  if(length(xPeak) == 0) xPeak <- xOnset[1]
  
  ############ integrate artifacts with response onsets ##########
  
  # no feature extraction if any prestim artifacts prior to latRow
  
  if(useArtifacts == TRUE) {
    
    # integration of artifact extraction with onset values
    
    # check for artifacts within a few seconds before stim onset 
    prestimStart <- (onsetRow-(artifactLat*cps))
    if(prestimStart < 1) prestimStart <- 1
    
    # if any artifacts in the artifact vector
    if(length(which(artifactVector[prestimStart:ROWEndRow] != "0")) > 0) { 
      # check for any artifacts during the prestim segment prior to xOnset
      artifactIndices <- which(artifactVector != "0")
      artifactIndices <- artifactIndices[artifactIndices >= prestimStart]
      artifactIndices <- artifactIndices[artifactIndices <= ROWEndRow]
      
      # check each xOnset
      # exclude xOnset items when artifact within a few seconds prior
      # keepXOnset <- xOnset
      # if(length(xOnset) > 0) {
      #   for(k in 1:length(xOnset)) {
      #     # compare each to to artifactIndices
      #     checkIndx <- xOnset[k] - (artifactLat * cps)
      #     which(artifactIndices >= checkIndx & artifactIndices <= xOnset[k])
      #     artifactVector[checkIndx:xOnset[k]]
      #     
      #   }
      # }
      
      keepXOnset <- NULL
      if(length(xOnset) > 0) {
        for (k in 1:length(xOnset)) {
          # keep xOnset indices before any artifact
          if(xOnset[k] < artifactIndices[1]) {
            keepXOnset <- c(keepXOnset, xOnset[k])
          }
        }
        xOnset <- keepXOnset
        if(is.null(xOnset)) xPeak <- NULL
      }
      
      # remove xPeak if artifact after xOnset 
      
      # keep xPeak indices before any artifact 
      keepXPeak <- NULL
      if(length(xPeak) > 0) {
        for (k in 1:length(xPeak)) {
          # keep xPeak indices before any artifact
          if(xPeak[k] < artifactIndices[1]) {
            keepXPeak <- c(keepXPeak, xPeak[k])
          }
        }
        xPeak <- keepXPeak
        if(is.null(xPeak)) xOnset <- NULL
      }
      
    }
    
  } # end if useArtifacts == TRUE
  
  ##### need to handle condition where no xOnset or xPeak values #####
  
  {
    
    if(length(xOnset) == 0) xOnset <- endRow
    # may need to be endRow -1 if response onset and end cannot be the same
    # should not be endRow -1 
    # when xOnset == xPeak there is no extraction
    
    # remove peaks that occur before the first onset after latency
    # xOnset[1] is after the latency so use it here
    # Keep only those slope peaks that are after the first xOnset row
    xPeak <- xPeak[xPeak >= xOnset[1]]
    
    if(length(xPeak) == 0) xPeak <- endRow
    
  }
  
  ########  get the onset and response peak values  #########
  
  {
    
    # get the + slope onset values
    xOnsetVal <- tsData[xOnset]
    
    # get the response peak values
    # xOnsetVal <- tsData[xOnset]
    xPeakVal <- tsData[xPeak]
    
    # now we have vectors for xOnset, xPeak, xOnsetVal and xPeakVal
    # print(xOnset)
    # print(xOnsetVal)
    # print(xPeak)
    # print(xPeakVal)
    
  }
  
  ###### an evil loop to get the max onset to peak difference for each onset #######
  
  # evil loop to select the max change for each xOnset row and all subsequent xPeak rows that are not excluded
  
  # xPeak rows may be excluded depending on how the data descend prior to a peak after a previous peak
  # this loop will call the descentProp() helper function
  # descentProp() is in the amplitudeExtractHelperFunctions.R script
  
  #### presently not completely sure if this rule is imposed during and after ROW or only after ROW
  
  # first make an empty vector for the loop output
  yChange <- rep("", times=length(xOnset)) 
  
  n=1
  for (n in 1:length(xOnset)) {
    # this has to be in a loop to iteratively shorten the comparison of peak values
    # if the data descend below onset value or if the data descend by 
    # more than a proportion p after the peak of a positive slope segment
    # set the stopRow to stop including xPeak values if they descend below the xOnset value
    stopRow <- which( tsData[(xOnset[n]+1):length(tsData)] < xOnsetVal[n] )[1] + xOnset[n] - 1
    # ignore xPeak indices after the stopRow 
    # because data have descended below the value at xOnset[n]
    # there is no stop row when the data do not descend below onset, so use the last row instead
    if(is.na(stopRow)) stopRow <- length(tsData)
    # for this iteration of the loop keep only xPeak indices after xOnset[n] and before stopRow
    xPeakLoop <- xPeak[which( xPeak > xOnset[n] & xPeak <= stopRow )]
    # set the default stopRow2
    # stopRow2 is the default descent cutoff row after which xPeaks are excluded
    # initialize the stopRow2 to the length of the time series data
    stopRow2 <- length(tsData)
    # this stopRow2 will be used when descentRule = 0
    # descentRule=0 will disable the descent rule, 
    # when disabled all peaks in the EW are used until the data descend below the onset value
    # descentRule=1 will enable the rule for all negative slope segments after a response peak
    # descentRule=2 will enable the rule only after ROWEndRow during the EW
    
    # locate the last xOnset before ROWEndRow
    # this seems only to apply to descentRule==2
    # if(any(xOnset <= ROWEndRow)) {
    #   xOnsetStart <- max(which(xOnset <= ROWEndRow))
    #   # xOnsetStart is the last xOnset in the ROW
    #   xPeakStart <- xPeak[max(which(xPeak <= ROWEndRow))]
    # } else {
    #   # if no xOnset indices are before ROWEndRow,
    #   # set the xOnsetStart to the length of the time series data segment
    #   xOnsetStart <- length(tsData)
    #   xPeakStart <- xPeak[1]
    # }
    
    # locate the stopRow2 for the descentRule
    if(descentRule != 0) {
      # descentRule 0 will disable the rule
      # call the descentProp() helper function to get the stopRow2,
      # descentProp() is 
      # to exclude xPeak indices after the data descend a proportion prop,
      # from a peak value to the onset value
      # prop is from env.params
      # the descentRule parameter in the init script     
      if(descentRule==2) {
        # rule 2 will include all positive slope segments that begin in the ROW,
        # and positive slope segments beginning after ROWEndRow,
        # if the data have not descended more than a proportion prop from the max peak
        
        # 10-24-2016 use the xOnset[n] only if is == or after the last xOnset before ROWEndRow
        
        # to do this locate a stopRow2 only after xPeak indices after the ROW
        
        if(length(which(xPeak <= ROWEndRow)) != 0) {
          # get the last xPeak before the ROWEndRow
          xPeakStart <- xPeak[max(which(xPeak <= ROWEndRow))]
          # get a subset of xPeakLoop values after the xPeakStart
          xPeakLoop2 <- xPeakLoop[xPeakLoop >= xPeakStart] # do not use >= here
          # not using >= means that xPeakLoop2 does not include the xPeakStart
          # use the descentProp function to get the stopRow2
          stopRow2 <- descentProp(x=xOnset[n], y=xPeakLoop2, z=tsData, dProp=descProp)
        } else {
          stopRow2 <- length(tsData)
        }
        
        # if(xOnset[n] > xOnsetStart) {
        #   stopRow2 <- descentProp(x=xOnset[n], y=xPeakLoop2, z=tsData, dProp=prop, ROWEnd=ROWEndRow)
        # } else {
        #   stopRow2 <- descentProp(x=xOnset[n], y=xPeakLoop, z=tsData, dProp=prop, ROWEnd=ROWEndRow)
        # }
        
      } else {
        # descentRule == 1 will use all xOnset indices
        # descentProp will use the ROWEndRow from the parent env
        stopRow2 <- descentProp(x=xOnset[n], y=xPeakLoop, z=tsData, dProp=descProp)
      }
      
    } # end of descentRule != 0
    
    # use the xOnset[n] and stopRow2 
    # to keep items in the xPeak vector for each xOnset[n] in the loop
    xPeakLoop <- xPeakLoop[which((xPeakLoop > xOnset[n]) & (xPeakLoop <= stopRow2))]
    # use the xPeakLoop vector to determine the max xPeak-xOnset for each xOnset
    if(length(xPeakLoop) > 0) {  
      # use XPeakLoop to select the max distance to xOnsetVal[n]
      # yChange is the xPeakLoop value 
      # that gives the tsData row index for the max change for each xOnset values
      yChange[n] <- xPeakLoop[which.max(tsData[xPeakLoop[xPeakLoop >= xOnset[n]]] - xOnsetVal[n])]
      # yChange will remain NA if none
    }
    # loop output is a vector 'yChange' to index the peak row in the tsData for the max response for each xOnset index
  } # end of evil loop to select the max change for each xOnset row
  
  # print(yChange)
  
  ############## select the max from the yChange vector ############## 
  
  {
    
    # to get the xOnset and xPeak indices
    
    # remove NAs that may result from different vector lengths
    yChange <- as.numeric(na.omit(yChange))
    
    # fix condition where there is no onset or peak
    if(is.na(yChange[1])) yChange[1] <- xOnset[1]
    
    # compute the differences between onset and peak values
    yChangeDiffs <- tsData[yChange] - tsData[xOnset]
    
    # compute the index of the max change from xOnset to xPeak
    yChangeMaxIndex <- which.max(yChangeDiffs)
    
    # compute the onset for the max change from xOnset to xPeak
    yChangeOnset <- xOnset[yChangeMaxIndex]
    yChangeOnsetValue <- tsData[yChangeOnset]
    
    # compute the peak for the max change xOnset to xPeak
    yChangePeak <- yChange[yChangeMaxIndex]
    yChangePeakValue <- tsData[yChange[yChangeMaxIndex]]
    
    # and finally calculate the max change from xOnset to xPeak
    yChangeValue <- yChangePeakValue - yChangeOnsetValue
    
  }
  
  ####### exclude measurements of 0 #######
  
  
  
  # this linne was commented out 8-8-2020
  # if(yChangeValue == 0) yChangeValue <- NA
  
  # note <> Aug 8, 2020
  # values of 0 can result 
  # when there is no onset of + slope segment 
  # and also no statistically significant change in variance during a + slope
  # values of 0 may also result during - slope segments
  
  # the something vs nothing rule allows a score 
  # when there is a response in the RQ or CQ and not the other
  # Something vs Nothing should be handled differently 
  # for + slope and - slope segments for which no response is extracted
  # + slope segments should return the value NA when no respose
  # - slope segments can return the value 0 when no response, 
  # this way 0 and small values can be subject to the
  # something vs nothing rule 
  # - slope segments are nothing
  # however + slope segments are not nothing but are unusable
  
  # use the latRow and endRow values to get the slope direction in the evaluation window
  
  if(yChangeValue == 0) {
  
    EWSlope <- sign(tsData[endRow] - tsData[latRow])
    
    # keep the 0 value if the slope is - or 0 during the ROW
    yChangeValue <- ifelse(EWSlope == 1,
                           NA,
                           0 ) 
    
  }
  
  ####### We now have the maximum change from xOnset to xPeak   ####### 
  
  # print(yChangeOnset)
  # print(yChangeOnsetValue)
  # print(yChangePeak)
  # print(yChangePeakValue)
  # print(yChangeValue)
  
  ############# fix a potential problem  ##############
  
  # fix condition where yChangeOnset == yChangePeak
  # does not recompute the yChangeValue which will remain 0
  if(yChangeOnset == yChangePeak) { 
    yChangeOnset <- yChangeOnset - 1
    yChangeOnsetValue <- tsData[yChangeOnset]
  }
  
  ###### determine the 1/2 recovery time or response duration  here #####
  
  {
    
    # 1/2 recovery is response duration
    # using 1/2 recovery is correlated with full recovery
    # but more usable
    # because some data to not return completely to the start value
    
    if(yChangeValue == 0 || is.na(yChangeValue)) {
      recoveryIdx <- ""
      recoveryTime <- ""
    } else {
      
      # get the half recovery value that indicates the y-axis level at end of 1/2 recovery
      recoveryValue <- yChangePeakValue - (recoveryProp * yChangeValue)
      
      # compute the intermediate sample index after the peak
      recInt <- which(tsData[yChangePeak:length(tsData)] <= recoveryValue)[1]
      # correct potential problem if the half rec exceeds the tsData length
      # tsData length is the nrows for the segmentDF 
      
      if(is.na(recInt)) {
        # set the half recovery point to the end of the tsData vector
        recInt <- length(tsData) - yChangePeak
      }
      
      # then compute the half recovery index
      recoveryIdx <- yChangePeak + recInt - 1
      
      # and finally the half recovery time
      recoveryTime <- (recoveryIdx - yChangeOnset + 1) / cps
      
    }
    
    # tsData[?] after yChangePeak 
    # where data descend 50% 
    # of the distance from yChangePeak Value to yChangeOnsetValue
    
  }
  
  ######## determine the response complexity here ########
  
  {
    
    # complexity is the number of xOnset indices 
    # after yChangeOnset and before response peak
    
    # initialize the variables
    
    # if(yChangeValue == 0) {
    complexityVal <- NULL
    complexityRows <- NULL
    # } else {
    # complexityRows are xOnset indices after response onset and before response peak  
    # complexityRows <- xOnset[xOnset > yChangeOnset & xOnset < yChangePeak]
    complexityRows <- xPeak[xPeak > yChangeOnset & xPeak < yChangePeak]
    # correct for no complexity
    if(length(complexityRows) == 0) complexityRows <- NULL
    complexityVal <- ifelse(length(complexityRows) > 1,
                            length(complexityRows),
                            1) 
    # }
    
    # if(length(complexityRows) == 0) complexityRows <- ""
    # if(length(complexityVal) == 0) complexityVal <- ""
    
  }
  
  #################### output ##################
  
  # construct the output vector
  output <- list(yChangeOnset, 
                 yChangePeak, 
                 yChangeOnsetValue, 
                 yChangePeakValue, 
                 yChangeValue,
                 recoveryIdx,
                 recoveryTime,
                 complexityRows,
                 complexityVal,
                 stopRow2,
                 segmentTitle)
  names(output) <- c("responseOnsetRow",
                     "responsePeakRow",
                     "responseOnsetValue",
                     "responsePeakValue",
                     "responseChangeValue",
                     "recoveryRow",
                     "recoveryTime",
                     "complexityRows",
                     "complexityValue",
                     "stopRow",
                     "segmentTitle")
  
  return(output)
  # end amplitudeExtractFn() function 
  
} # end amplitudeExtractFn() function 



