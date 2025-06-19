# new function for feature extraction with the finger cuff or forearm cuff
# May 22, 2019
# Raymond Nelson



newFCExtractFn <- function(x=segmentDF, y=extract.params) {
  # new function for feature extraction with the finger cuff or forearm cuff
  # May 22, 2019
  # Raymond Nelson
  #
  # copied from the cardioExtractFn
  #
  # called from the feature extraction function
  # x input is a data frame for a stimulus segment
  # other parameters are obtained from a list made from information in 
  # the parent and global environment
  # output is the segmentDF 
  # after extracting the features and adding the extraction data to the extraction and measurement columns 
  ###
  
  segmentDF <- x
  
  extract.params <- y
  
  {
    stimOnsetRow <- extract.params$onset
    stimOffsetRow <- extract.params$offset
    answerRow <- extract.params$answer
    segEndRow <- extract.params$end
    segmentName <- extract.params$segName
    chartName <- extract.params$chart
    seriesName <- extract.params$series
    examName <- extract.params$exam
    
    rbpfMsg <- extract.params$rbpfMsg
    cardioRate <- extract.params$cardioRate
    UPRate <- extract.params$UPRate
    LPRate <- extract.params$LPRate
    rateRatio <- extract.params$rateRatio
  }
  
  # make a list of parameters from the global env
  env.params <- list(dataRate=cps, 
                     Lat=CardioLat, 
                     useROW=useROW,
                     ROWStart=ROWStart,
                     ROWStop=ROWStop,
                     ROWEnd=ROWEnd, 
                     ignore=ignore, 
                     strictWindow=cardioStrictWindow, 
                     strictROW=cardioStrictROW, 
                     descentRule=descentRule, 
                     descProp=descProp, 
                     slopeChangeRule=cardioSlopeChangeRule,
                     inflection=inflection, 
                     nothingIsSomething=nothingIsSomething )
  
  #######################################
  
  # reset the FCExtract columns in the segmentDF
  {
    segmentDF$FCExtract[stimOnsetRow:nrow(segmentDF)] <- ""
    segmentDF$FCExtract_d[stimOnsetRow:nrow(segmentDF)] <- ""
    segmentDF$FCExtract_s[stimOnsetRow:nrow(segmentDF)] <- ""
    segmentDF$FCExtract_m[stimOnsetRow:nrow(segmentDF)] <- ""
    segmentDF$FCExtract_v[stimOnsetRow:nrow(segmentDF)] <- ""
    
    segmentDF$FCMeasure[stimOnsetRow:nrow(segmentDF)] <- 0
    segmentDF$FCDuration[stimOnsetRow:nrow(segmentDF)] <- 0
    segmentDF$FCComplexity[stimOnsetRow:nrow(segmentDF)] <- 0
    
  }
  
  # get a setting from the global environment
  useArtifacts <- integrateCardioArtifacts
  
  # make a list of parameters for feature extraction
  extractList <- list(begin=stimOnsetRow,
                      end=stimOffsetRow,
                      answer=answerRow,
                      examName=examName,
                      seriesName=seriesName,
                      chartName=chartName,
                      segmentName=segmentName,
                      segmentTitle=paste(examName, seriesName, chartName, segmentName, sep="_"),
                      useArtifacts=useArtifacts )
  
  # artifactVector <- segmentDF$Cardio1_a
  artifactVector <- segmentDF$Artifacts_a
  
  extractList[["artifactVector"]] <- artifactVector
  
  # iterate over the four FC lines: mid, systolic, diastolic, ma
  i=4
  for (i in 1:4) {
    
    dataVector <- switch(as.character(i),
                         "1" = segmentDF$c_FCDiastolic,
                         "2" = segmentDF$c_FCSystolic,
                         "3" = segmentDF$c_FCMid,
                         "4" = segmentDF$c_FCMA,
                         "otherwise: last" )
    
    # add the data vector to the extracList
    extractList[["dataVector"]] <- dataVector
    
    sensorName <- switch(as.character(i),
                         "1" = "CardioDiastolic",
                         "2" = "CardioSystolic",
                         "3" = "CardioMid",
                         "4" = "CardioMA", # slow moving average
                         "otherwise: last" )
    
    extractList[["sensorName"]] <- sensorName
    
    assign("cardioExtractList", extractList, envir=.GlobalEnv)
    
    
    
    ############
    
    ### use the amplitude extractFunction to compute the FC response
    extractResult <- amplitudeExtractFnPC(extractList, env.params)
    
    # extractResult is a named character vector with 7 items:
    # responseOnsetRow, responsePeakRow, responseOnsetValue, responsePeakValue,
    # responseChangeValue, stopRow, segmentTitle
    
    # print(extractResult)
    
    # process the extractResult ouput of the amplitudeExtract function
    responseOnsetRow <- as.numeric(extractResult$responseOnsetRow)
    responseEndRow <- as.numeric(extractResult$responsePeakRow)
    stopRow <- as.numeric(extractResult$stopRow)
    responseChange <- as.numeric(extractResult$responseChangeValue)
    recIdx <- as.numeric(extractResult$recoveryRow)
    recTime <- as.numeric(extractResult$recoveryTime)
    complexityValue <- as.numeric(extractResult$complexityValue)
    
    complexityRows <- as.numeric(extractResult$complexityRows)
    
    print(unlist(extractResult[-8]))
    print(extractResult$complexityRows)
    
    # make sure that events are on distinct rows
    # onsetRow <- segOnsetRow
    latencyRow <- stimOnsetRow + round((CardioLat * cps), 0)
    ROWEndRow <- answerRow + (ROWEnd * cps)
    if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3
    
    # use the segEndRow to remove responses at the end of the segment
    if( any( (length(responseOnsetRow) != 0 && responseOnsetRow == (segEndRow - 1)),
             is.na(responseOnsetRow),
             length(responseOnsetRow) == 0 ) ) {
      responseOnsetRow <- NULL
      responseEndRow <- NULL
      stopRow <- NULL
      responseChange <- NULL
      recIdx <- NULL
      recTime <- NULL
      complexityRows <- NULL
      complexityValue <- NULL
    }
    
    # construct a named vector to hold the results
    events <- list(stimOnsetRow,
                   stimOffsetRow,
                   answerRow,
                   latencyRow,
                   ROWEndRow,
                   segEndRow,
                   responseOnsetRow,
                   responseEndRow,
                   stopRow,
                   responseChange,
                   recIdx,
                   recTime,
                   complexityRows,
                   complexityValue )
    
    # name items in the events vector
    ifelse(length(events)==14,
           names(events) <- c("onsetRow",
                              "offsetRow",
                              "answerRow",
                              "latencyRow",
                              "ROWEndRow",
                              "segEndRow",
                              "responseOnsetRow",
                              "responseEndRow",
                              "stopRow",
                              "responseChange",
                              "recoveryRow",
                              "recoveryTime",
                              "complexityRows",
                              "complexityValue"),
           names(events) <- c("onsetRow",
                              "offsetRow",
                              "answerRow",
                              "latencyRow",
                              "ROWEndRow",
                              "segEndRow"))
    
    ## work with the selected cardio line
    
    if(as.character(i) == "1") {
      # FC diastolic line
      segmentDF$c_FCExt
      segmentDF$FCExtract_d[events$onsetRow] <- "onsetRow"
      segmentDF$FCExtract_d[events$offsetRow] <- "offsetRow"
      segmentDF$FCExtract_d[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$FCExtract_d[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$FCExtract_d[events$responseEndRow] <- "responseEndRow"
        segmentDF$FCExtract_d[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$FCExtract_d[events$complexityRows] <- "complexityRow"
        }
      }
      if(!is.null(responseChange)) {
        segmentDF$FCMeasure_d[stimOnsetRow:stimOffsetRow] <- responseChange
        if(length(recTime) > 0) {
          segmentDF$FCDuration_d[stimOnsetRow:stimOffsetRow] <- recTime
        }
        if(length(complexityValue) > 0) {
          segmentDF$FCComplexity_d[stimOnsetRow:stimOffsetRow] <- complexityValue
        }
      }
    } else if(as.character(i) == "2") {
      # FC systolic line
      segmentDF$FCExtract_s[events$onsetRow] <- "onsetRow"
      segmentDF$FCExtract_s[events$offsetRow] <- "offsetRow"
      segmentDF$FCExtract_s[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$FCExtract_s[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$FCExtract_s[events$responseEndRow] <- "responseEndRow"
        segmentDF$FCExtract_s[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$FCExtract_s[events$complexityRows] <- "complexityRow"
        }
      }
      if(!is.null(responseChange)) {
        segmentDF$FCMeasure_s[stimOnsetRow:stimOffsetRow] <- responseChange
        if(length(recTime) > 0) {
          segmentDF$FCDuration_s[stimOnsetRow:stimOffsetRow] <- recTime
        }
        if(length(complexityValue) > 0) {
          segmentDF$FCComplexity_s[stimOnsetRow:stimOffsetRow] <- complexityValue
        }
      }
    } else if(as.character(i) == "3") {
      # FC mid line
      segmentDF$FCExtract_m[events$onsetRow] <- "onsetRow"
      segmentDF$FCExtract_m[events$offsetRow] <- "offsetRow"
      segmentDF$FCExtract_m[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$FCExtract_m[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$FCExtract_m[events$responseEndRow] <- "responseEndRow"
        segmentDF$FCExtract_m[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$FCExtract_m[events$complexityRows] <- "complexityRow"
        }
      }
      if(!is.null(responseChange)) {
        segmentDF$FCMeasure_m[stimOnsetRow:stimOffsetRow] <- responseChange
        if(length(recTime) > 0) {
          segmentDF$FCDuration_m[stimOnsetRow:stimOffsetRow] <- recTime
        }
        if(length(complexityValue) > 0) {
          segmentDF$FCComplexity_m[stimOnsetRow:stimOffsetRow] <- complexityValue
        }
      }
    } else {
      # FC slow moving average # added may 9, 2019
      segmentDF$FCExtract_v[events$onsetRow] <- "onsetRow"
      segmentDF$FCExtract_v[events$offsetRow] <- "offsetRow"
      segmentDF$FCExtract_v[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$FCExtract_v[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$FCExtract_v[events$responseEndRow] <- "responseEndRow"
        segmentDF$FCExtract_v[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$FCExtract_v[events$complexityRows] <- "complexityRow"
        }
      }
      if(!is.null(responseChange)) {
        segmentDF$FCMeasure_v[stimOnsetRow:stimOffsetRow] <- responseChange
        if(length(recTime) > 0) {
          segmentDF$FCDuration_v[stimOnsetRow:stimOffsetRow] <- recTime
        }
        if(length(complexityValue) > 0) {
          segmentDF$FCComplexity_v[stimOnsetRow:stimOffsetRow] <- complexityValue
        }
      }
    } # end if else for FC slow moving average
    
    
    
    
  } # end iteration over 4 FC lines: diastolic, systolic, mid, ma
  
  #######################################
  
  # select the FC line
  
  # cardioLine is set in the NCCAASCI_init.R script
  
  segmentDF$FCExtract <- switch(cardioLine,
                                    "diastolic" = segmentDF$FCExtract_d,
                                    "systolic" = segmentDF$FCExtract_s,
                                    "mid" = segmentDF$FCExtract_m,
                                    "ma" = segmentDF$FCExtract_v,
                                    "otherwise: last" )
  
  segmentDF$FCMeasure <- switch(cardioLine,
                                    "diastolic" = segmentDF$FCMeasure_d,
                                    "systolic" = segmentDF$FCMeasure_s,
                                    "mid" = segmentDF$FCMeasure_m,
                                    "ma" = segmentDF$FCMeasure_v,
                                    "otherwise: last" )
  
  segmentDF$FCDuration <- switch(cardioLine,
                                     "diastolic" = segmentDF$FCDuration_d,
                                     "systolic" = segmentDF$FCDuration_s,
                                     "mid" = segmentDF$FCDuration_m,
                                     "ma" = segmentDF$FCDuration_v,
                                     "otherwise: last" )
  
  segmentDF$FCComplexity <- switch(cardioLine,
                                       "diastolic" = segmentDF$FCComplexity_d,
                                       "systolic" = segmentDF$FCComplexity_s,
                                       "mid" = segmentDF$FCComplexity_m,
                                       "ma" = segmentDF$FCComplexity_v,
                                       "otherwise: last" )
  
  #######################################
  
  ### get the FC rate changes 
  
  # get the stimulus and prestimulus data segments and check the systolic rate
  tsDataPreStim_s <- chartDF$FCPeak[1:(stimOnsetRow-1)]
  tsDataStim_s <- chartDF$FCPeak[stimOnsetRow:segEndRow]
  # locate the systolic peaks and calculate the rate per minute
  prestimRate_s <- mean(diff(which(tsDataPreStim_s == "systolic"))) / 30 * 60
  stimulusRate_s <- mean(diff(which(tsDataStim_s == "systolic"))) / 30 * 60
  
  print(paste("prestim rate:", prestimRate_s))
  print(paste("cardoi rate:", stimulusRate_s))
  
  segmentDF$FCRate_s <- stimulusRate_s
  
  # get the stimulus and prestimulus data segments and check the diastolic rate
  tsDataPreStim_d <- chartDF$FCPeak[1:(stimOnsetRow-1)]
  tsDataStim_d <- chartDF$FCPeak[stimOnsetRow:segEndRow]
  # locate the systolic peaks and calculate the rate per minute
  prestimRate_d <- mean(diff(which(tsDataPreStim_d == "diastolic"))) / 30 * 60
  stimulusRate_d <- mean(diff(which(tsDataStim_d == "diastolic"))) / 30 * 60
  
  print(paste("prestim rate:", prestimRate_d))
  print(paste("FC rate:", stimulusRate_d))
  
  segmentDF$FCRate_d[stimOnsetRow:segEndRow] <- stimulusRate_d
  segmentDF$FCRate_s[stimOnsetRow:segEndRow] <- stimulusRate_s
  segmentDF$FCRate[stimOnsetRow:segEndRow] <- mean(c(stimulusRate_d, stimulusRate_s))
  
  # check the difference between the systolic and diastolic
  
  FCRateRatio <- stimulusRate_s / stimulusRate_d
  print(paste("FC rate ratio:", FCRateRatio))
  
  # segmentDF
  
  # check the result
  # segmentDF$FCExtract[segmentDF$FCExtract!=""]
  
  # print(events)
  
  return(segmentDF)
  
} # end newFCExtractFn()

