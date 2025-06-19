# Cardio feature extraction
# 4/27/2016
# Raymond Nelson


# CardioExtractFn <- function(x=segmentDF, onset, offset, answer, end, segName, chart, series, exam) {


CardioExtractFn <- function(x=segmentDF, 
                            y=extract.params, 
                            cardioRate=cardioRate, 
                            rbpfMsg=rbpfMsg) {
  # cardio feature extraction function is called from the feature extraction function
  # x input is a data frame for a stimulus segment
  # other parameters are obtained from a list made from information in 
  # the parent and global environment
  # output is the segmentDF 
  # after extracting the features and adding the extraction data to the extraction and measurement columns 
  ####
  
  extract.params <- y
  segmentDF <- x
  
  # assign("segmentDF", segmentDF, envir=.GlobalEnv)
  
  # to stop on warnings
  options(warn=2)
  # getOption("warn")
  # to reset default warning level
  # options(warn=0)
  
  # if(segmentName == "R5") stop()
  
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
    
    segmentTitle <- paste(examName, seriesName, chartName, segmentName, sep="_")
    # print(segmentTitle)
  }
  
  # if(all(examName=="DTedTestGlenda0", seriesName=="1", chartName=="05A", segmentName=="R2")) {
  #   stop()
  # }
  
  #### reset the cardioExtract column in the segmentDF ####
  
  {
    # output
    segmentDF$CardioExtract[stimOnsetRow:nrow(segmentDF)] <- ""
    segmentDF$CardioMeasure[stimOnsetRow:nrow(segmentDF)] <- 0
    segmentDF$CardioDuration[stimOnsetRow:nrow(segmentDF)] <- 0
    segmentDF$CardioComplexity[stimOnsetRow:nrow(segmentDF)] <- 0
    # 
    
    {
      # April 6, 2024 check for and fix long reaction peaks from the previous stimulus
      
      # compute the replacement response end row
      # when a new stimulus is presented during a long reaction
      # the response to the previous stimulus is terminated
      subStimEndRow <- stimOnsetRow - 2
      
      stimRows <- c(stimOnsetRow:nrow(segmentDF))
      
      # check if there is a response end from the previous stimulus
      dEnd <- which(segmentDF$CardioExtract_d[stimRows] == "responseEndRow")
      sEnd <- which(segmentDF$CardioExtract_s[stimRows] == "responseEndRow")
      mEnd <- which(segmentDF$CardioExtract_m[stimRows] == "responseEndRow")
      vEnd <- which(segmentDF$CardioExtract_v[stimRows] == "responseEndRow")
      allEnd <- which(segmentDF$CardioExtract[stimRows] == "responseEndRow")
      
      # move the response end to a point prior to the new stimulus onset
      if(length(dEnd) > 0) segmentDF$CardioExtract_d[subStimEndRow] <- "responseEndRow"
      if(length(sEnd) > 0) segmentDF$CardioExtract_s[subStimEndRow] <- "responseEndRow"
      if(length(mEnd) > 0) segmentDF$CardioExtract_m[subStimEndRow] <- "responseEndRow"
      if(length(vEnd) > 0) segmentDF$CardioExtract_v[subStimEndRow] <- "responseEndRow"
      if(length(allEnd) > 0) segmentDF$CardioExtract[subStimEndRow] <- "responseEndRow"
      
      # remove the response following the new stimulus onset
      if(length(dEnd) > 0) segmentDF$CardioExtract_d[stimRows][dEnd] <- ""
      if(length(sEnd) > 0) segmentDF$CardioExtract_s[stimRows][sEnd] <- ""
      if(length(mEnd) > 0) segmentDF$CardioExtract_m[stimRows][mEnd] <- ""
      if(length(vEnd) > 0) segmentDF$CardioExtract_v[stimRows][vEnd] <- ""
      if(length(allEnd) > 0) segmentDF$CardioExtract[stimRows][vEnd] <- ""
      
      
      
      
      # segmentDF$CardioExtract_d[stimOnsetRow:nrow(segmentDF)] <- "" # diastolic
      # segmentDF$CardioExtract_s[stimOnsetRow:nrow(segmentDF)] <- "" # systolic
      # segmentDF$CardioExtract_m[stimOnsetRow:nrow(segmentDF)] <- "" # mid
      # segmentDF$CardioExtract_v[stimOnsetRow:nrow(segmentDF)] <- "" # very slow
      # segmentDF$CardioExtract[stimOnsetRow:nrow(segmentDF)] <- ""
    }
    
    segmentDF$Cardio1_a <- 0
  }
  
  #### make a list of parameters from the global env ####
  
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
                     nothingIsSomething=nothingIsSomething,
                     prestim=cardioPrestim ) # prestim added Aug 8, 2023
  
  assign("env.params", env.params, pos=1)
  assign("env.params.cardio", env.params, pos=1)
  
  #### make a list of parameters for feature extraction ####
  
  {
    
    # get a setting from the global environment
    useArtifacts <- integrateCardioArtifacts
    
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
    
    # # Aug 26, 2023
    extractList[["segmentDF"]] <- segmentDF
    
    # assign("extractList", extractList, envir=.GlobalEnv)
    
    # extractList is then subject to iteration for the different cardio lines
    
  }

  #### iterate over four cardio lines and call amplitudeExtractFn ####
  
  i=4 # 3 is the mid line # 4 is the MA line
  for (i in 1:4) {
    
    # cardioLine is set in the NCCAASCI_init.R script
    # dataVector <- switch(cardioLine,
    #                      "diastolic" = segmentDF$c_CardioDiastolic,
    #                      "systolic" = segmentDF$c_CardioSystolic,
    #                      "mid" = segmentDF$c_CardioMid,
    #                      "ma" = segmentDF$c_CardioMA, 
    #                      "otherwise: last" )
    
    # dataVector <- segmentDF$c_CardioDiastolic
    # dataVector <- segmentDF$c_CardioSystolic
    # dataVector <- segmentDF$c_CardioMid
    # dataVector <- segmentDF$c_CardioMA
    
    dataVector <- switch(as.character(i),
                         "1" = segmentDF$c_CardioDiastolic,
                         "2" = segmentDF$c_CardioSystolic,
                         "3" = segmentDF$c_CardioMid,
                         "4" = segmentDF$c_CardioMA, # slow moving average
                         "otherwise: last" )
    
    # add the data vector to the extracList
    extractList[["dataVector"]] <- dataVector
    
    # plot.ts(dataVector)
    
    sensorName <- switch(as.character(i),
                         "1" = "CardioDiastolic",
                         "2" = "CardioSystolic",
                         "3" = "CardioMid",
                         "4" = "CardioMA", # slow moving average
                         "otherwise: last" )
    
    extractList[["sensorName"]] <- sensorName
    
    #### add the artifact vector here ####
    
    assign("sensorName", sensorName, envir=.GlobalEnv)
    assign("cardioExtractList", extractList, envir=.GlobalEnv)
    assign("extractList", extractList, envir=.GlobalEnv)
    
    # if(i=4) stop()
    
    #### call the amplitude extractFunction to compute the cardio response ####
    
    extractResult <- amplitudeExtractFnPC(extractList, 
                                          env.params )
    
    # # August 26, 2023 now submitting the segment DF
    # segmentDF <- extractResult$segmentDF
    
    # extractResult is a named character vector with 7 items:
    # responseOnsetRow, responsePeakRow, responseOnsetValue, responsePeakValue,
    # responseChangeValue, stopRow, segmentTitle
    
    # print(extractResult)
    
    
    
    output <- list(NA, 
                   NA, 
                   NA, 
                   NA, 
                   0, # response change value 
                   NULL, # not used with simplified amplitude extraction
                   NULL, # but the EDAExtract and CardioExtract 
                   NULL, # functions will look for these items
                   NULL, # so include them for now
                   NULL,
                   segmentTitle, 
                   NA,
                   segmentDF)
    # name the output items
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
                       "segmentTitle",
                       "artifactVector",
                       "segmentDF")
    
    
    
    
    
    
    {
      
      segmentDF <- extractResult[['segmentDF']]
      
      # NA values will remain NA
      responseChange <- as.numeric(extractResult$responseChangeValue)
      
      {
        #### April 7, 2024 check for small unusable (noise) cardio extraction values ####
        if(is.na(responseChange) || responseChange <= 10) {
          extractResult$responseChange <- NA
          responseChange <- NA
          extractResult$responseOnsetRow <- NA
          extractResult$responsePeakRow <- NA
          extractResult$stopRow <- NULL
          extractResult$recoveryRow <- NULL
          extractResult$recoveryTime <- NULL
          extractResult$complexityValue <- NULL
          extractResult$complexityRows <- NULL
          extractResult$responseOnsetValue <- NA
          extractResult$responsePeakValue <- NA
        }
        
      }
      
      # process the extractResult ouput of the amplitudeExtract function
      responseOnsetRow <- as.numeric(extractResult$responseOnsetRow)
      responseEndRow <- as.numeric(extractResult$responsePeakRow)
      stopRow <- as.numeric(extractResult$stopRow)
      
      recIdx <- as.numeric(extractResult$recoveryRow)
      recTime <- as.numeric(extractResult$recoveryTime)
      complexityValue <- as.numeric(extractResult$complexityValue)
      
      complexityRows <- as.numeric(extractResult$complexityRows)
      
      # print(unlist(extractResult[-8]))
      # print(extractResult$complexityRows)
      
      # make sure that events are on distinct rows
      # onsetRow <- segOnsetRow
      latencyRow <- stimOnsetRow + round((CardioLat * cps), 0)
      ROWEndRow <- answerRow + (ROWEnd * cps)
      if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3
      
    }
    
    # use the segEndRow to remove responses at the end of the segment
    if( any( (length(responseOnsetRow) != 0 && responseOnsetRow == (segEndRow - 1)),
            is.na(responseOnsetRow),
            length(responseOnsetRow) == 0 ) ) {
      responseOnsetRow <- NULL
      responseEndRow <- NULL
      stopRow <- NULL
      recIdx <- NULL
      recTime <- NULL
      complexityRows <- NULL
      complexityValue <- NULL
      
      # this needs to stay NA or 0 depending on the nothingIsSomething parameter
      # responseChange <- NA # this needs to stay NA or 0 depending 
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
                              "segEndRow") )
    
    ## work with the selected cardio line
    
    if(as.character(i) == "1") {
      # cardio diastolic line
      segmentDF$CardioExtract_d[events$onsetRow] <- "onsetRow"
      segmentDF$CardioExtract_d[events$offsetRow] <- "offsetRow"
      segmentDF$CardioExtract_d[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$CardioExtract_d[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$CardioExtract_d[events$responseEndRow] <- "responseEndRow"
        segmentDF$CardioExtract_d[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$CardioExtract_d[events$complexityRows] <- "complexityRow"
        }
      }
      # add the response regardless of whether NA NULL or a numeric value
      {
        segmentDF$CardioMeasure_d[stimOnsetRow:stimOffsetRow] <- responseChange
        # a value of 0 will give an ESSM integer score
        # NA will result in no ESSM score
      }
      if(length(recTime) > 0) {
        segmentDF$CardioDuration_d[stimOnsetRow:stimOffsetRow] <- recTime
      }
      if(length(complexityValue) > 0) {
        segmentDF$CardioComplexity_d[stimOnsetRow:stimOffsetRow] <- complexityValue
      }
    } else if(as.character(i) == "2") {
      # cardio systolic line
      segmentDF$CardioExtract_s[events$onsetRow] <- "onsetRow"
      segmentDF$CardioExtract_s[events$offsetRow] <- "offsetRow"
      segmentDF$CardioExtract_s[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$CardioExtract_s[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$CardioExtract_s[events$responseEndRow] <- "responseEndRow"
        segmentDF$CardioExtract_s[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$CardioExtract_s[events$complexityRows] <- "complexityRow"
        }
      }
      {
        # add the response regardless of whether NA NULL or a numeric value
        segmentDF$CardioMeasure_s[stimOnsetRow:stimOffsetRow] <- responseChange
        # a value of 0 will give an ESSM integer score
        # NA will result in no ESSM score
      }
      if(length(recTime) > 0) {
        segmentDF$CardioDuration_s[stimOnsetRow:stimOffsetRow] <- recTime
      }
      if(length(complexityValue) > 0) {
        segmentDF$CardioComplexity_s[stimOnsetRow:stimOffsetRow] <- complexityValue
      }
    } else if(as.character(i) == "3") {
      # cardio mid line
      segmentDF$CardioExtract_m[events$onsetRow] <- "onsetRow"
      segmentDF$CardioExtract_m[events$offsetRow] <- "offsetRow"
      segmentDF$CardioExtract_m[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$CardioExtract_m[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$CardioExtract_m[events$responseEndRow] <- "responseEndRow"
        segmentDF$CardioExtract_m[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$CardioExtract_m[events$complexityRows] <- "complexityRow"
        }
      }
      if(!is.null(responseChange)) {
        segmentDF$CardioMeasure_m[stimOnsetRow:stimOffsetRow] <- responseChange
        
      }
      if(length(recTime) > 0) {
        segmentDF$CardioDuration_m[stimOnsetRow:stimOffsetRow] <- recTime
      }
      if(length(complexityValue) > 0) {
        segmentDF$CardioComplexity_m[stimOnsetRow:stimOffsetRow] <- complexityValue
      }
    } else {
      # cardio MA slow moving average # added may 9, 2019
      segmentDF$CardioExtract_v[events$onsetRow] <- "onsetRow"
      segmentDF$CardioExtract_v[events$offsetRow] <- "offsetRow"
      segmentDF$CardioExtract_v[events$answerRow] <- "answerRow"
      if(!is.null(responseOnsetRow)) {
        segmentDF$CardioExtract_v[events$responseOnsetRow] <- "responseOnsetRow"
        segmentDF$CardioExtract_v[events$responseEndRow] <- "responseEndRow"
        segmentDF$CardioExtract_v[events$recoveryRow] <- "recoveryRow"
        if(length(events$complexityRows) > 0) {
          segmentDF$CardioExtract_v[events$complexityRows] <- "complexityRow"
        }
      }
      # add the response regardless of whether NA NULL or a numeric value
      {
        segmentDF$CardioMeasure_v[stimOnsetRow:stimOffsetRow] <- responseChange
        # a value of 0 will give an ESSM integer score
        # NA will result in no ESSM score
      }
      if(length(recTime) > 0) {
        segmentDF$CardioDuration_v[stimOnsetRow:stimOffsetRow] <- recTime
      }
      if(length(complexityValue) > 0) {
        segmentDF$CardioComplexity_v[stimOnsetRow:stimOffsetRow] <- complexityValue
      }
    } # end if else for cardio slow moving average
    
  } # end iteration over 4 cardio lines: diastolic, systolic, mid, ma
  
  #### select the cardio line that was set in the NCCAASCI_init.R script ####
  
  # cardioLine is set in the NCCAASCII_init.R script
  thisCardio <- cardioLine
  
  # choose the slow moving cardio data if the RPBF ratio >= .9  
  # if(is.na(as.numeric(str_sub(rbpfMsg, -3, -1)))) {
  #   thisCardio <- "ma"
  # } else {
  #   thisCardio <- ifelse(as.numeric(str_sub(rbpfMsg, -3, -1)) >= .9,
  #                        "ma",
  #                        cardioLine)
  # } 
                       
  segmentDF$CardioExtract <- switch(thisCardio,
                       "diastolic" = segmentDF$CardioExtract_d,
                       "systolic" = segmentDF$CardioExtract_s,
                       "mid" = segmentDF$CardioExtract_m,
                       "ma" = segmentDF$CardioExtract_v,
                       "otherwise: last" )
  
  segmentDF$CardioMeasure <- switch(thisCardio,
                                    "diastolic" = segmentDF$CardioMeasure_d,
                                    "systolic" = segmentDF$CardioMeasure_s,
                                    "mid" = segmentDF$CardioMeasure_m,
                                    "ma" = segmentDF$CardioMeasure_v,
                                    "otherwise: last" )
  
  segmentDF$CardioDuration <- switch(thisCardio,
                                     "diastolic" = segmentDF$CardioDuration_d,
                                     "systolic" = segmentDF$CardioDuration_s,
                                     "mid" = segmentDF$CardioDuration_m,
                                     "ma" = segmentDF$CardioDuration_v,
                                     "otherwise: last" )
  
  segmentDF$CardioComplexity <- switch(thisCardio,
                                       "diastolic" = segmentDF$CardioComplexity_d,
                                       "systolic" = segmentDF$CardioComplexity_s,
                                       "mid" = segmentDF$CardioComplexity_m,
                                       "ma" = segmentDF$CardioComplexity_v,
                                       "otherwise: last" )
  
  #### no cardio extraction under some conditions ####
  
  # Feb 20, 2020
  # segmentDF$CardioExtract_d
  # segmentDF$CardioExtract_s
  
  # mid line
  responseOnset_m <- which(segmentDF$CardioExtract_m == "responseOnsetRow")
  # very slow moving average
  responseOnset_v <- which(segmentDF$CardioExtract_v == "responseOnsetRow")
  
  # distance for mid and MA onset
  onsetDiff <- abs(diff(c(responseOnset_m[1], responseOnset_v[1])))
  
  # if(length(onsetDiff) == 0) stop("onsetDiff problem")
  
  responseEnd_m <- which(segmentDF$CardioExtract_m =="responseEndRow")
  responseEnd_v <- which(segmentDF$CardioExtract_v =="responseEndRow")
  
  # distance for mid and MA end
  endDiff <- abs(diff(c(responseEnd_m, responseEnd_v)))
  
  # if(length(endDiff) == 0) stop("endDiff problem")
  
  # if( length(onsetDiff) == 0 || is.na(onsetDiff) ) {
  #   resetThese <-
  #     which(segmentDF$CardioExtract %in% c("responseOnsetRow", "responseEndRow"))
  #   segmentDF$CardioExtract[resetThese] <- 0
  #   segmentDF$CardioMeasure <- 0
  #   segmentDF$CardioDuration <- 0
  #   segmentDF$CardioComplexity <- 0
  # } else if( onsetDiff > 60 ) {
  #   # reset these if the distance exceeds 2 sec for response onset or end
  #   resetThese <-
  #     which(segmentDF$CardioExtract %in% c("responseOnsetRow", "responseEndRow"))
  #   segmentDF$CardioExtract[resetThese] <- 0
  #   segmentDF$CardioMeasure <- 0
  #   segmentDF$CardioDuration <- 0
  #   segmentDF$CardioComplexity <- 0
  # }
  
  # select the max end from mid and MA
  
  # midEndVal <- segmentDF$c_CardioMid[responseEnd_m]
  # MAEndVal <- segmentDF$c_CardioMA[responseEnd_v]
  # 
  # endVal <- which.max(c(midEndVal, MAEndVal))
  # 
  # segmentDF$CardioMeasure <- endVal - segmentDF$c_CardioMid[responseOnset_m]
  
  #### get the cardio rate changes ####
  
  # get the stimulus and prestimulus data segments and check the systolic rate
  # tsDataPreStim_s <- as.numeric(chartDF$CardioPeak_s[1:(stimOnsetRow-1)])
  # tsDataStim_s <- as.numeric(chartDF$CardioPeak_s[stimOnsetRow:segEndRow])
  # 
  # # locate the systolic peaks and calculate the rate per minute
  # prestimRate_s <- mean(diff(which(tsDataPreStim_s == "systolic"))) / 30 * 60
  # stimulusRate_s <- mean(diff(which(tsDataStim_s == "systolic"))) / 30 * 60
  # 
  # print(paste("prestim rate:", prestimRate_s))
  # print(paste("cardio rate:", stimulusRate_s))
  # 
  # segmentDF$CardioRate_s <- stimulusRate_s
  # 
  # # get the stimulus and prestimulus data segments and check the diastolic rate
  # tsDataPreStim_d <- as.numeric(chartDF$CardioPeak_d[1:(stimOnsetRow-1)])
  # tsDataStim_d <- as.numeric(chartDF$CardioPeak_d[stimOnsetRow:segEndRow])
  # # locate the systolic peaks and calculate the rate per minute
  # prestimRate_d <- mean(diff(which(tsDataPreStim_d == "diastolic"))) / 30 * 60
  # stimulusRate_d <- mean(diff(which(tsDataStim_d == "diastolic"))) / 30 * 60
  # 
  # print(paste("prestim rate:", prestimRate_d))
  # print(paste("cardio rate:", stimulusRate_d))
  # 
  # segmentDF$CardioRate_d <- stimulusRate_d
  # 
  # segmentDF$CardioRate_d[stimOnsetRow:segEndRow] <- stimulusRate_d
  # segmentDF$CardioRate_s[stimOnsetRow:segEndRow] <- stimulusRate_s
  # segmentDF$CardioRate[stimOnsetRow:segEndRow] <- mean(c(stimulusRate_d, stimulusRate_s))
  # 
  # # check the difference between the systolic and diastolic
  # 
  # cardioRateRatio <- stimulusRate_s / stimulusRate_d
  # print(paste("rate ratio:", cardioRateRatio))
  
  #### output ####
  
  # check the result
  # segmentDF$CardioExtract[segmentDF$CardioExtract!=""]
  
  # print(events)
  
  return(segmentDF)
  
} # end newCardioExtractFn()



