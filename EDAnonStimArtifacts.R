# R function to identify EDA artifacts during non-stimulus segments
# 11-8-2016
# Raymond Nelson
# 
#
# contains 4 functions
#
# nonStimArtifactFn() # the main function
#
# amplitudeIncreaseFn()  # called by the nonStimArtifactFn
# 
# maxSlopeChangeFn() called by the amplitudeIncreaseFn
# also in the amplitudeExtractHelperFunctions.R script
#
# tonicActivityFn() # called by the nonStimArtifactFn
#
####



###################################################



nonStimArtifactFn <- function(x=chartDF) {
  # R function to identify EDA artifacts during non-stimulus segments
  # 11-8-2016
  # Raymond Nelson
  #
  # called by the edaArtifactFn() in the edaArtifact.R script
  #
  # requires the amplitudeIncreaseFn()
  # and tonicActivityFn()
  #
  # x input is a data frame for a recorded chart
  # output is the chartDF after marking artifacts 
  # in the form of signiticant prestimulus activity
  # 
  ####
  
  {
    chartDF <- x
    
    # reset the artifact columns
    
    # this can be reset here
    chartDF$AutoEDA_a <- "0"
    
    # this is best reset in the main artifact function
    # chartDF$Artifacts_a
  }
  
  ####  get the stimulus events ####
  
  {
    
    # use both the Label and eventLabel columns here to avoid problems
    eventNames <- chartDF$Label[which(chartDF$eventLabel != "")]
    eventIndices <- which(chartDF$eventLabel != "")
    
    # remove excluded events from the eventIndices
    # do the indices first because eventNames will change
    eventIndices <- eventIndices[ which(!(eventNames %in% excludeEvents)) ]
    
    # now set the eventNames using the eventLabel column
    eventNames <- chartDF$eventLabel[eventIndices]
    # eventNames <- eventNames[!(eventNames %in% excludeEvents)]
    # this will keep the correct event names
    # including repeated stimulus events
    # and exclusing annotations 
    
    ###
    
    # keep only RQ and CQ events
    # keepEvents <- NULL
    # # 12/7/2016
    # # keepEvents <- eventNames[grep("[CR]+", eventNames)]
    # # keepEvents <- eventNames
    # # keep all events if there are not 2 RQs and 2 CQs
    # if(length(keepEvents) == 0){
    #   # still need to work on this 12-7-2016
    #   # exclude the first and last event
    #   keepEvents <- eventNames
    # } 
    # eventNames <- keepEvents
    
    if(length(eventNames) == 0) return(chartDF)
    
  }
  
  #### determine the row indices for stimulus events ####
  
  {
    
    # get the stimulus onset rows using the eventLabel column
    # events in the Label column occur on multiple rows
    # events in the eventLabel column occur on a single row
    eventOnsetRows <- which(chartDF$eventLabel %in% eventNames)
    
    # get the stimulus offset rows
    # use the chartDF$Label colum to get the stimulus offset rows
    eventOffsetRows <- chartDF$Label
    
    # iterate over the eventOffsetRows to keep the last of each run
    i=1
    for(i in 1:(length(eventOffsetRows)-1)) {
      if(eventOffsetRows[i] == eventOffsetRows[(i+1)]) eventOffsetRows[i] <- NA
    }
    eventOffsetRows <- which(!is.na(eventOffsetRows))
    
    # keep only those event offset rows for which the name is in eventNames
    eventOffsetRows <- 
      eventOffsetRows[which(chartDF$Label[eventOffsetRows] %in% eventNames)]
    
    # inspect the labels
    # chartDF$Label[eventOffsetRows]
    
    # get the stimulus segment end rows
    # artifactSeg is in the global env and set by the init script
    eventEndRows <- eventOnsetRows + (artifactSeg * cps) - 1
    
    # fix last event end if it exceeds the number of rows in the data frame
    eventEndRows[which(eventEndRows > nrow(chartDF))] <- nrow(chartDF) - 5
    
    # get the answer rows
    answerRows <- which(chartDF$Label %in% c("YES",  "NO", "ANS"))
    
    ### manage condition where there are missing verbal answers ###
    
    # initialize a vector for answers
    keepAnswers <- eventOffsetRows + 1
    
    # iterate over the eventOffsetRows to re-build the keepAnswers vector
    i=1
    for(i in length(eventOffsetRows):1) {
      # increment the loop 
      # if no answer row indices are greater than event offset row
      if(length(which(answerRows > eventOffsetRows[i])) == 0) next()
      # use the min answer row that is greater than the event end row
      useAnswer <- min(which(answerRows > eventOffsetRows[i]))
      keepAnswers[i] <- answerRows[useAnswer]
      # remove the useAnswer from the answerRows vector before the next iteration
      answerRows <- answerRows[-useAnswer]
    }
    
    # get the indices for missing answers
    replaceAnswers <- 
      which(keepAnswers[1:(length(keepAnswers)-1)] > 
              eventOffsetRows[2:length(eventOffsetRows)])
    keepAnswers[replaceAnswers] <- eventEndRows[replaceAnswers] + 1
    answerRows <- keepAnswers
    
    # fix condidition where last answerRow exceeds nrow(chartDF)
    answerRows[which(answerRows > nrow(chartDF))] <- nrow(chartDF) - 3
    
    # get the ROWEnd rows
    # ROWEnd is in the global env set by the init script
    ROWEndRows <- answerRows + (ROWEnd*cps)
    
    # fix condition where the last ROWEndRow exceeds nrow(chartDF)
    ROWEndRows[which(ROWEndRows > nrow(chartDF))] <- nrow(chartDF) - 2
    
  }
  
  ######## get the row indices for the prestimulus segment ########
  
  {
    
    # get the prestimulus onset rows
    # prestimSeg is in the global env set by the init script
    prestimOnsetRows <- eventOnsetRows - (prestimSeg*cps)
    
    # fix condition where prestimOnset Rows are less than 1
    prestimOnsetRows[which(prestimOnsetRows < 1)] <- 1
    
    # get the 10 sec prestimulus onset rows
    prestimRows10 <- eventOnsetRows - (10*cps)
    
    # get the 5 sec prestimulus onset rows
    prestimRows5 <- eventOnsetRows - (5*cps)
    
    # fix condition where prestimRows5 are less than 1
    prestimRows5[which(prestimRows5 < 1)] <- 1
    
    # get the 3 sec prestimulus onset rows
    prestimRows3 <- eventOnsetRows - (3*cps)
    
    # fix condition where prestimRows are less than 1
    prestimRows3[which(prestimRows3 < 1)] <- 1
    prestimRows5[which(prestimRows5 < 1)] <- 1
    prestimRows10[which(prestimRows10 < 1)] <- 1
    
    # get the prestimlus end rows
    # set the prestimulus end 1 second after stimulus onset
    # to increase artifact detection
    # add 1 second now to avoid problems later
    prestimEndRows <- eventOnsetRows + (1 * cps) - 1
    # fix prestimEndRows less than 1
    prestimEndRows[which(prestimEndRows <= 1)] <- 2
    
  }
  
  ######## get the row indices for the poststimulus segment ########
  
  {
    
    # fix last prestim end if it exceeds the number of rows in the data frame
    prestimEndRows[which(prestimEndRows > nrow(chartDF))] <- nrow(chartDF) - 4
    
    # get the poststim onset rows
    poststimOnsetRows <- eventEndRows + 1
    
    # fix condition where the last poststimOnsetRow exceeds nrow(chartDF)
    poststimOnsetRows[which(poststimOnsetRows > nrow(chartDF))] <- nrow(chartDF) - 2
    
    # get the poststim end rows
    # addSeg is in the global env set by the init script
    poststimEndRows <- poststimOnsetRows + (addSeg*cps) - 1
    
    # fix condition where the last poststimEndRow exceeds nrow(chartDF)
    poststimEndRows[which(poststimEndRows > nrow(chartDF))] <- nrow(chartDF) - 1
    
  }
  
  ######## compute the max diff for each prestim and  stim segment ########
  
  # iterate over the stimulus segments
  i=8
  for (i in 1:length(eventOnsetRows)) {
    
    # for debugging
    # assign("i", i, pos=1)
    
    ### get the stimulus segment indices
    
    {
      # from stimulus onset to the end of the evaluation window
      stimOnset <- eventOnsetRows[i]
      stimOffset <- eventOffsetRows[i]
      stimAnswer <- answerRows[i]
      stimEnd <- eventEndRows[i]
      stimROWEnd <- ROWEndRows[i]
    }
    
    ### get the prestim segment indices
    
    {
      # from several seconds prior to stim onset to stim onset
      prestimOnset <- prestimOnsetRows[i]
      prestimEnd <- prestimEndRows[i]
      
      # get the 10 second prestim index
      prestimOnset10 <- prestimRows10[i]
      
      # get the 5 second prestim index
      prestimOnset5 <- prestimRows5[i]
      
      # get the 3 second prestim index
      prestimOnset3 <- prestimRows3[i]
    }
    
    ### get the data segments
    
    {
      # stimSegment <- chartDF$c_EDAFiltDiff[stimOnset:stimEnd]
      stimSegment <- chartDF$c_AutoEDA[(stimOnset+(EDALat*cps)):stimEnd]
      
      # prestimSegment <- chartDF$c_EDAFiltDiff[prestimOnset:prestimEnd]
      prestimSegment <- chartDF$c_AutoEDA[prestimOnset:prestimEnd]
      
      # from 5 seconds prior to stim onset to stim onset
      prestimSegment5 <- chartDF$c_AutoEDA[prestimOnset5:prestimEnd]
      
      # get the segment to check for tonicity
      prestimSegTonic <- chartDF$c_AutoEDA[prestimOnset5:prestimEnd]
      # (prestimEnd-(1*cps))
    }
    
    ######## get the amplitude of increase for all segments ########
    
    {
      
      # amplitudeIncreaseFn is another function in this R script
      
      # use the amplitudeIncreaseFn with the stimSegment
      stimIndices <- amplitudeIncreaseFn(x=stimSegment)
      stimAmplitude <- 
        -(stimSegment[stimIndices['onset']] - stimSegment[stimIndices['peak']])
      
      # use the amplitudeIncreaseFn with the prestimulus segment
      prestimIndices <- amplitudeIncreaseFn(x=prestimSegment)
      prestimAmplitude <- 
        -(prestimSegment[prestimIndices['onset']] - 
            prestimSegment[prestimIndices['peak']])
      
      # use the amplitudeIncreaseFn with the prestimulus 5sec segment
      prestimIndices5 <- amplitudeIncreaseFn(prestimSegment5)
      prestimAmplitude5 <- 
        -(prestimSegment5[prestimIndices5['onset']] - 
            prestimSegment5[prestimIndices5['peak']])
      
    }
    
    ######## compare the segments ########
    
    {
      
      # compare the stimAmplitude and prestimAmplitude
      
      if( stimAmplitude != 0 && !is.na(stimAmplitude) &&
          prestimAmplitude != 0 && !is.na(prestimAmplitude) ) {
        # nonStimRatio is from the globale env and is set in the init scripet
        if(prestimAmplitude / stimAmplitude >= nonStimRatio) {
          # compute the prestim artifact onset and peak indices in the chartDF
          artifactOnset <- prestimOnset + prestimIndices['onset'] - 1
          artifactPeak <- prestimOnset + prestimIndices['peak'] - 1
          # mark the EDA artifact vector
          chartDF$AutoEDA_a[artifactOnset:artifactPeak] <- "artifact1a"
        }
      }
      
      # compare the stimAmplitude with the prestimAmplitude5
      
      if( stimAmplitude != 0 && !is.na(stimAmplitude) &&
          prestimAmplitude5 != 0 && !is.na(prestimAmplitude5) ) {
        # nonStimRatio is from the globale env and is set in the init scripet
        if(prestimAmplitude5 / stimAmplitude >= nonStimRatio) {
          # compute the prestim artifact onset and peak indices in the chartDF
          artifactOnset5 <- prestimOnset5 + prestimIndices5['onset'] - 1
          artifactPeak5 <- prestimOnset5 + prestimIndices5['peak'] - 1
          # mark the EDA artifact vector
          chartDF$AutoEDA_a[artifactOnset5:artifactPeak5] <- "artifact1a"
        }
      }
      
    }
    
    #### look at the tonicity of the data for  prestimulus seconds ####
    
    {
      
      # use the tonicActivityFn() function in this R script
      
      tonicMessage <- tonicActivityFn(x=prestimSegTonic)
      
      tonicAmplitude <- as.numeric(tonicMessage["value"])
      
      if(stimAmplitude != 0 && !is.na(stimAmplitude) &&
         tonicMessage["message"] == "non-tonic") {
        if(tonicAmplitude / stimAmplitude >= nonStimRatio) {
          activityOnset <- as.numeric(tonicMessage["activityOnset"])
          activityEnd <- as.numeric(tonicMessage["activityEnd"])
          actOnset <- prestimOnset5 + activityOnset - 1
          actEnd <- prestimOnset5 + activityEnd - 1
          chartDF$AutoEDA_a[actOnset:actEnd] <- "artifact1a"
        }
      }
      
    }
    
    ################# get the non-stimulus segment ################
        
    #### commented this out to use only the prestim 2018-11-09 ####
    
    ## skip if i is the last stimulus event
    if(i != length(eventOnsetRows)) {

      # get the subsequent stimulus segment
      # from the onset of the stimulus to the end of the evaluation window
      stim2Onset <- eventOnsetRows[(i+1)]
      answer2Row <- answerRows[(i+1)]
      stim2ROWEnd <- ROWEndRows[(i+1)]
      stim2End <- eventEndRows[(i+1)]
      # stim1Segment <- chartDF$c_EDAFiltDiff[stimOnset:stimEnd]
      stim2Segment <- chartDF$c_AutoEDA[stim2Onset:stim2End]
      # use the amplitudeIncreaseFn(),
      # sourced in the amplitudeExtractHelperFunctions.R script
      # to get the stim segment onset and peak
      stim2Indices <- amplitudeIncreaseFn(x=stim2Segment)
      stim2Amplitude <- -(stim2Segment[stim2Indices['onset']] -
                            stim2Segment[stim2Indices['peak']])

      # get the non-stimulus segment
      # from end of the evaluation window to the next stimulus onset
      # use -1 sample index to evaluate the slope direction at ROWEnd
      nonstimOnset <- stimEnd - (1*cps)
      # nonstimOnset <- stimROWEnd - 1
      nonstimEnd <- stim2Onset - 1 + (.5*cps)
      # 3-29-2017 fix condition where nonstimEnd <= nonstimOnset
      # when question pacing is less than 15 seconds
      if(nonstimOnset >= nonstimEnd) nonstimOnset <- nonstimEnd - (3.5*cps)
      # nonstimSegment <- chartDF$c_EDAFiltDiff[nonstimOnset:nonstimEnd]
      nonstimSegment <- chartDF$c_AutoEDA[nonstimOnset:nonstimEnd]
      # get the nonstim segment onset and peak

      print(i)

      ### this is the problem with Chad's test 2-17-2017
      # nonstimSegment is too short
      ### because the questions are too close together
      nonstimIndices <- amplitudeIncreaseFn(x=nonstimSegment)
      # get the nonstimulus amplitude
      nonstimAmplitude <- -(nonstimSegment[nonstimIndices['onset']] -
                              nonstimSegment[nonstimIndices['peak']])

      #### compare the stimAmplitude and stim2Amplitude
      # with the nonstimAmplitude
      if(nonstimAmplitude != 0 && !is.na(nonstimAmplitude)) {
        # need to do a better job when there is no feature extraction
        if(stimAmplitude != 0 && !is.na(stimAmplitude)) {
          # if (nonstimAmplitude >= stimAmplitude) {
          if (nonstimAmplitude / stimAmplitude >= .9) {
            # compute the nonstim artifact onset and peak indices in chartDF
            artifactOnset <- nonstimOnset + nonstimIndices['onset'] - 1
            artifactPeak <- nonstimOnset + nonstimIndices['peak'] - 1
            # mark the artifact
            chartDF$AutoEDA_a[artifactOnset:artifactPeak] <- "artifact1c"
          }
        } # end if for stimAmplitude != 0
        if(stim2Amplitude != 0 && !is.na(stim2Amplitude)) {
          # if (nonstimAmplitude >= stim2Amplitude) {
          if (nonstimAmplitude / stim2Amplitude >= 1) {
            # compute the nonstim artifact onset and peak indices in chartDF
            artifactOnset <- nonstimOnset + nonstimIndices['onset'] - 1
            artifactPeak <- nonstimOnset + nonstimIndices['peak'] - 1
            # mark the artifact
            # chartDF$AutoEDA_a[artifactOnset:artifactPeak] <- "artifact1c"
          }
        } # end if for stim2Amplitude != 0
      } # end if(nonstimAmplitude != 0)

    } # end if for non-stimulus segement
    
    ################## get the post-stimulus segment #############
    
    {
      # from the end of the response onset window for 10 seconds
      # use - 1 sample to evaluate the slope direction at ROWEnd
      # poststimOnset <- ROWEndRows[min(which(ROWEndRows > stimOnset))] - 1
      poststimOnset <-
        poststimOnsetRows[min(which(poststimOnsetRows > stimOnset))] - (1*cps)
      # poststimEnd <- poststimOnset + (10*cps)
      # poststimEndRows vector is set to 10 seconds after poststimOnsetRows
      poststimEnd <-
        poststimEndRows[min(which(poststimEndRows > poststimOnset))]
      # fix condition where poststimEnd exceeds nrow(chartDF)
      poststimEnd[which(poststimEnd > nrow(chartDF))] <- nrow(chartDF) - 6
      # manage condition where poststimEnd
      # exceeds the onset of the next stimulus event
      if(i < length(eventOnsetRows) & poststimEnd > eventOnsetRows[(i+1)]) {
        # set the poststim slice to end .5 sec after onset of the next stimulus
        # this could result in a slice greater than 10 seconds,
        # this is not a problem
        poststimEnd <- eventOnsetRows[(i+1)] + (.5*cps)
      }
      # poststimSegment <- chartDF$c_EDAFiltDiff[poststimOnset:poststimEnd]
      poststimSegment <- chartDF$c_AutoEDA[poststimOnset:poststimEnd]
    }

    # not used if no poststimSegement because time between events is too short
    if(length(poststimSegment) > 1) {
      
      # get the poststim segment onset and peak
      # 9-7-2017
      poststimIndices <- amplitudeIncreaseFn(poststimSegment)
      # get the poststimulus amplitude
      poststimAmplitude <- -(poststimSegment[poststimIndices['onset']] -
                               poststimSegment[poststimIndices['peak']])

      #### compare the stimAmplitude and poststimAmplitude ####
      
      {
        
        if(stimAmplitude != 0 && !is.na(stimAmplitude)) {
          if(poststimAmplitude != 0 && !is.na(poststimAmplitude)) {
            # if (poststimAmplitude >= stimAmplitude) {
            if (poststimAmplitude / stimAmplitude >= 1) {
              # compute the poststim artifact onset and peak indices in chartDF
              artifactOnset <- poststimOnset + poststimIndices['onset'] - 1
              artifactPeak <- poststimOnset + poststimIndices['peak'] - 1
              # mark the artifact
              # chartDF$AutoEDA_a[artifactOnset:artifactPeak] <- "artifact1b"
            }
          }
        }
        
      }
      
    } # end if for poststim segments
    
  } # end loop over i stimulus events
  
  # get the artifact rows and add the artifacts to the Artifacts_a column
  AutoEDAArtifacts <- which(chartDF$AutoEDA_a != "0")
  chartDF$Artifacts_a[AutoEDAArtifacts] <- "artifact"
  
  return(chartDF)
  
  # end nonStimArtifactFn function
  
} # end nonStimArtifactFn function



# which(examDF$AutoEDA_a != "0")



#########################################################################



amplitudeIncreaseFn <- function(x=stimSegment) {
  # R function to compute the amplitude of increase in EDA and cardio data
  # used to compare activity in the prestimulus segment with the stimulus segment
  # 11-18-2016
  # raymond nelson
  #
  # called by the nonStimArtifactFn()
  #
  # x input is vector of time series values from the data segment
  # 
  # requires helper functions from the amplitudeExtractHelperFunctions.R script
  #
  # 1) identify all onset indices
  # defined as the onset of a positive slope segment
  # including the segment onset index (1)
  # 2) identify all peak points
  # defined as the onset of a negative slope segment
  # including the segment end index example: length(segment)
  # 3) compute the inverse of the max difference between each onset 
  # and all subsequent peak points and select the max peak for each onset
  # 4) select the onset and peak with the max difference
  #
  # output is a named vector of two values for onsetRow and endRow with the max difference
  # 
  # another function will compare the max prestim with the max stim segment
  #
  ####
  
  # tsDataSegment <- stimSegment
  stimSegment <- x
  
  # make a vector of slope changes
  xSlope <- fillSlope(smoothSlope(slopeDir(stimSegment)))
  
  # always include the segment onset as a postive slope onset
  # xSlope[1] <- 1
  
  # make vector of positive slope onset indices 
  onsetIndices <- which(positiveOnset(positiveSlope(xSlope)) == 1)
  
  ### use a function to check for significant changes in postive slope
  # call the amplitudeExtractHelperFunctions.R script 
  # to load the maxSlopeChangeFn()
  slopeChangeIndices <- which(maxSlopeChangeFn(x=stimSegment) == 1)
  
  # add sig changes to the onset indices
  onsetIndices <- sort(c(onsetIndices, slopeChangeIndices))
  
  # remove the first positive slope onset if it is the second sample
  if(length(onsetIndices) > 0 && onsetIndices[1] == 2) onsetIndices <- onsetIndices[-1]
  
  # make a vector of peak indices
  peakIndices <- slopePeak(xSlope)
  
  # always include the segment end as a peak
  # added again 10-8-2018
  peakIndices[length(peakIndices)] <- 1
  peakIndices <- which(peakIndices == 1)
  
  # make a vector of 0s
  maxOnsetIndices <- integer(length=length(onsetIndices))
  
  # compute the inverse of the max diff between each onset and subsequent peaks
  j=1
  for (j in 1:length(onsetIndices)) {
    # use only those peaks that occur after the j onset
    usePeaks <- which(peakIndices > onsetIndices[j])
    # exit the loop if there are no peaks
    if(length(usePeaks)==0) next()
    # get the max peak
    # which.max is vectorized and requires no control loop
    maxPeakRow <- which.max(-(stimSegment[onsetIndices[j]] - 
                                stimSegment[peakIndices[usePeaks]]))
    maxOnsetIndices[j] <- peakIndices[usePeaks[maxPeakRow]]
  }
  
  differenceDF <- cbind.data.frame("onset"=onsetIndices, 
                                   "peak"=maxOnsetIndices)
  
  # now repeat the process looking for abs peak to onset differences
  # maxPeakIndices <- integer(length=length(peakIndices))
  # j=1
  # for(j in 1:length(peakIndices)) {
  #   # use only those onsets that occur after the j peak
  #   useOnsets <- which(onsetIndices > peakIndices[j])
  #   if(length(useOnsets)==0) next()
  #   # use abs() here because we are looking for peak-onset
  #   maxOnsetRow <- which.max(abs(stimSegment[peakIndices[j]] -
  #                                  stimSegment[onsetIndices[useOnsets]]))
  #   maxPeakIndices[j] <- onsetIndices[useOnsets[maxOnsetRow]]
  # }
  # 
  # peakDifferenceDF <- cbind.data.frame("onset"=maxPeakIndices,
  #                                      "peak"=peakIndices)
  #
  # differenceDF <- rbind(onsetDifferenceDF, peakDifferenceDF)
  
  # select the onset and peak with the max difference
  useMax <- which.max(-(stimSegment[differenceDF$onset] - stimSegment[differenceDF$peak]))
  
  # output the result
  # out put is a named vector of two indices "onset" and "peak"
  return(c("onset"=differenceDF$onset[useMax], "peak"=differenceDF$peak[useMax]))
  # end amplitudeIncreaseFn function
  
} # end amplitudeIncreaseFn function



###########################################################################



tonicActivityFn <- function(x=prestimSegTonic) {
  # function to determine if data are not tonic 
  # during the 5 second prestimulus period
  # 11-18-2018
  # raymond nelson
  #
  # called by the nonStimArtifactFn()
  #
  # x input is vector of time series values from the data segment
  # from 3 sec prior to stimulus onset until stimulus onset
  #
  # output is a named vector of 4 items
  # tonic message, activity onset index, and activity end index, value
  #
  # non-tonic prestimulus segments can be marked as an artifact 
  # and no feature extraction will be completed for the stimulus segment
  #
  ####
  
  prestimSegTonic <- x
  
  # get the max and min row indices for the prestim segment
  maxIdx <- which.max(prestimSegTonic)
  minIdx <- which.min(prestimSegTonic)
  
  maxVal <- prestimSegTonic[maxIdx]
  minVal <- prestimSegTonic[minIdx]
  
  diffVal <- abs(maxVal - minVal)
  
  # compare the difference to the tonicValue (200) set in the NCCAASCII_init.R script
  tonicMessage <- ifelse(diffVal >= tonicValue, "non-tonic", "tonic")
  
  sortIdx <- sort(c(maxIdx, minIdx))
 
  c("message"=tonicMessage, 
    "activityOnset"=sortIdx[1], 
    "activityEnd"=sortIdx[2],
    "value"=diffVal)
   
  # end tonicActivityFn
  
} # end tonicActivityFn

# tonicActivityFn()



#########################################################################



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
  
  # initialize some vectors
  preDiffMean <- rep(0, times=length(xDiff))
  preDiffSD <- rep(0, times=length(xDiff))
  postDiffMean <- rep(0, times=length(xDiff))
  zScore <- rep(0, times=length(xDiff))
  
  # use a function to compute the z score cutpoint from the aChange quantile
  # aChange is set in the NCCAASCII_init.R script
  zCut <- qnorm(aChange)
  
  # round the nPre and nPost segments to the nearest sample
  preLen <- round(cps*nPre,0)
  postLen <- round(cps*nPost,0)
  tonicLen <- round(cps*tonicSec,0)
  
  # initialize the output vector
  y <- rep(0, times=length(xDiff))
  
  # exit if the input vector is too short
  if(length(xDiff) <= (preLen+1)) return(y)
  
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



