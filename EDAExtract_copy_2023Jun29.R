# EDA feature extraction
# 4/27/2016
# Raymond Nelson

EDAExtractFn <- function(x=segmentDF, y=extract.params) {
  # feature extraction function is called from the feature extraction function
  # x input is a data frame for a stimulus segment
  # y is a list of parameters that are obtained from the parent environment
  # output is the segmentDF 
  # after extracting the features and adding the extraction data to the extraction and measurement columns 
  ###
  
  if(!exists("segmentDF")) segmentDF <- x
  if(!exists("extract.params")) extract.params <- y
  
  #### separate the extract parameters ####
  
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
  
  {
    
    # reset the AutoEDAExtract column that indicates response onset and offset
    segmentDF$AutoEDAExtract[stimOnsetRow:nrow(segmentDF)] <- ""
    segmentDF$ManualEDAExtract[stimOnsetRow:nrow(segmentDF)] <- ""

    # get a setting from the global env, set by NCCAASCII_init.R
    # useArtifacts <- integrateEDAArtifacts
  
  }
  
  #### fix any indices from the subsequent stimulus event ####
  
  {
    
    # auto EDA
    fixTheseAuto <- which(segmentDF$AutoEDAExtract[stimOnsetRow:nrow(segmentDF)] != "")
    if(length(fixTheseAuto) > 0 ) fixTheseAuto <- fixTheseAuto + stimOnsetRow - 1
    
    if(length(fixTheseAuto) > 0) {
      for(i in length(fixTheseAuto):1) {
        segmentDF$AutoEDAExtract[(stimOnsetRow-i)] <- segmentDF$AutoEDAExtract[fixTheseAuto[i]]
        segmentDF$AutoEDAExtract[fixTheseAuto[i]] <- ""
      }
    }
    
    # manual EDA
    fixTheseManual <- which(segmentDF$ManualEDAExtract[stimOnsetRow:nrow(segmentDF)] != "")
    if(length(fixTheseManual) > 0) fixTheseManual <- fixTheseManual + stimOnsetRow - 1
    
    if(length(fixTheseManual) > 0) {
      for(i in length(fixTheseManual):1) {
        segmentDF$ManualEDAExtract[(stimOnsetRow-i)] <- segmentDF$ManualEDAExtract[fixTheseManual[i]]
        segmentDF$ManualEDAExtract[fixTheseManual[i]] <- ""
      }
    }
    
  }
  
  ##### make  a list of environment parameters #####
  
  {
    # could get all these from the global env but this is an opportunity to change them 
    # the advantage of using a list is that they can be changed here
    env.params <- list(dataRate=cps, 
                       Lat=EDALat, 
                       useROW=useROW,
                       ROWStart=ROWStart,
                       ROWStop=ROWStop,
                       ROWEnd=ROWEnd, 
                       ignore=ignore, 
                       strictWindow=EDAStrictWindow, 
                       strictROW=EDAStrictROW, 
                       descentRule=descentRule, 
                       descProp=descProp, 
                       slopeChangeRule=slopeChangeRule,
                       inflection=inflection, 
                       nothingIsSomething=nothingIsSomething )
    
    assign("env.params", env.params, pos=1)
  }
  
  #### make a list of parameters for feature extraction ####
  
  {
    AutoExtractList <- list(begin=stimOnsetRow,
                            end=stimOffsetRow,
                            answer=answerRow,
                            examName=examName,
                            seriesName=seriesName,
                            chartName=chartName,
                            segmentName=segmentName,
                            segmentTitle=paste(examName, seriesName, chartName, segmentName, sep="_"),
                            useArtifacts=integrateEDAArtifacts,
                            dataVector=segmentDF$c_AutoEDA,
                            sensorName="AutoEDA",
                            artifactVector=segmentDF$Artifacts_a
                            # artifactVector=segmentDF$AutoEDA_a 
    )
    
    assign("AutoExtractList", AutoExtractList, pos=1)
  }
  
  #### make a list for extraction using manually centered EDA ####
  
  {
    ManualExtractList <- list(begin=stimOnsetRow,
                              end=stimOffsetRow,
                              answer=answerRow,
                              examName=examName,
                              seriesName=seriesName,
                              chartName=chartName,
                              segmentName=segmentName,
                              segmentTitle=paste(examName, seriesName, chartName, segmentName, sep="_"),
                              useArtifacts=integrateEDAArtifacts,
                              dataVector=segmentDF$c_ManualEDA,
                              sensorName="ManualEDA",
                              artifactVector=segmentDF$ManualEDA_a 
    )
    
    assign("ManualExtractList", ManualExtractList, pos=1)
  }
  
  ###### call the amplitudeExtract function ######
  
  {
    
    # if(AutoExtractList$segmentTitle == "DP1118NXX_9_03A_R6") stop()
    
    extractList <- AutoExtractList
    assign("extractList", extractList, envir=.GlobalEnv)
    
    AutoExtractResult <- amplitudeExtractFnPC(extractList=extractList, 
                                              env.params=env.params )
    
    # print(unlist(AutoExtractResult[-8]))
    # print(AutoExtractResult$complexityRows)
    
    extractList <- ManualExtractList
    # July 28, 2023 added this assignment 
    # assign("extractList", extractList, envir=.GlobalEnv)
    
    ManualExtractResult <- amplitudeExtractFnPC(extractList=extractList, 
                                                env.params=env.params )
    
    # print(unlist(ManualExtractResult[-8]))
    # print(ManualExtractResult$complexityRows)
    
  }
  
  #### process the AutoExtractResult ####
  
  {
    
    responseOnsetRow <- as.numeric(AutoExtractResult$responseOnsetRow)
    responseEndRow <- as.numeric(AutoExtractResult$responsePeakRow)
    stopRow <- as.numeric(AutoExtractResult$stopRow)
    recIdx <- as.numeric(AutoExtractResult$recoveryRow)
    
    # NA values will remain NA
    responseChange <- as.numeric(AutoExtractResult$responseChangeValue)
    
    recTime <- as.numeric(AutoExtractResult$recoveryTime)
    complexityValue <- as.numeric(AutoExtractResult$complexityValue)
    complexityRows <- as.numeric(AutoExtractResult$complexityRows)
    
    # make sure that all events are on distinct rows
    # onsetRow <- stimOnsetRow
    latencyRow <- stimOnsetRow + round(EDALat * cps,0)
    ROWEndRow <- answerRow + (ROWEnd * cps)
    if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3
    
    # use the segEndRow to remove measurements when there is no response
    if( any((length(responseOnsetRow) != 0 && 
             responseOnsetRow == (segEndRow - 1)),
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
    # events <- as.numeric(c(stimOnsetRow,
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
    
    # add the feature extraction to the events DF
    segmentDF$AutoEDAExtract[events$onsetRow] <- "onsetRow"
    segmentDF$AutoEDAExtract[events$offsetRow] <- "offsetRow"
    segmentDF$AutoEDAExtract[events$answerRow] <- "answerRow"
    if(!is.null(responseOnsetRow)) {
      segmentDF$AutoEDAExtract[events$responseOnsetRow] <- "responseOnsetRow"
      segmentDF$AutoEDAExtract[events$responseEndRow] <- "responseEndRow"
      segmentDF$AutoEDAExtract[events$recoveryRow] <- "recoveryRow"
      if(length(events$complexityRows) > 0) {
        segmentDF$AutoEDAExtract[events$complexityRows] <- "complexityRow"
      }
    }
    # add the response regardless of whether NA NULL or a numeric value
    {
      segmentDF$AutoEDAMeasure[stimOnsetRow:stimOffsetRow] <- responseChange
      # a value of 0 will give an ESSM integer score
      # NA will result in no ESSM score
    }
    if(length(recTime) > 0) {
      segmentDF$AutoEDADuration[stimOnsetRow:stimOffsetRow] <- recTime
    }
    if(length(complexityValue) > 0) {
      segmentDF$AutoEDAComplexity[stimOnsetRow:stimOffsetRow] <- complexityValue
    }
    
  }
  
  ####
  
  ### process the ManualExtractResult
  
  {
    
    responseOnsetRow <- as.numeric(ManualExtractResult$responseOnsetRow)
    responseEndRow <- as.numeric(ManualExtractResult$responsePeakRow)
    stopRow <- as.numeric(ManualExtractResult$stopRow)
    
    responseChange <- as.numeric(ManualExtractResult$responseChangeValue)
    
    recIdx <- as.numeric(ManualExtractResult$recoveryRow)
    recTime <- as.numeric(ManualExtractResult$recoveryTime)
    complexityValue <- as.numeric(ManualExtractResult$complexityValue)
    complexityRows <- as.numeric(ManualExtractResult$complexityRows)
    
    # make sure that all events are on distinct rows
    # onsetRow <- stimOnsetRow
    latencyRow <- stimOnsetRow + round(EDALat * cps,0)
    ROWEndRow <- answerRow + (ROWEnd * cps)
    if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3
    
    # use the segEndRow to remove measurements when there is no response
    if( any((length(responseOnsetRow) != 0 && 
       responseOnsetRow == (segEndRow - 1)),
       is.na(responseOnsetRow),
       length(responseOnsetRow) == 0 ) ) {
      responseOnsetRow <- NULL
      responseEndRow <- NULL
      stopRow <- NULL
      recIdx <- NULL
      recTime <- NULL
      complexityRows <- NULL
      complexityValue <- NULL
      # responseChange <- NULL
    }
    
    # construct a named vector to hold the results
    # events <- as.numeric(c(stimOnsetRow,
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
    
    # add the feature extraction to the events DF
    segmentDF$ManualEDAExtract[events$onsetRow] <- "onsetRow"
    segmentDF$ManualEDAExtract[events$offsetRow] <- "offsetRow"
    segmentDF$ManualEDAExtract[events$answerRow] <- "answerRow"
    if(!is.null(responseOnsetRow)) {
      segmentDF$ManualEDAExtract[events$responseOnsetRow] <- "responseOnsetRow"
      segmentDF$ManualEDAExtract[events$responseEndRow] <- "responseEndRow"
      segmentDF$ManualEDAExtract[events$recoveryRow] <- "recoveryRow"
      if(length(events$complexityRows) > 0) {
        segmentDF$ManualEDAExtract[events$complexityRows] <- "complexityRow"
      }
    }
    # add the response regardless of whether NA NULL or a numeric value
    {
      segmentDF$ManualEDAMeasure[stimOnsetRow:stimOffsetRow] <- responseChange
      # a value of 0 will give an ESSM integer score
      # NA will result in no ESSM score
    }
    if(length(recTime) > 0) {
      segmentDF$ManualEDADuration[stimOnsetRow:stimOffsetRow] <- recTime
    }
    if(length(complexityValue) > 0) {
      segmentDF$ManualEDAComplexity[stimOnsetRow:stimOffsetRow] <- complexityValue
    }
    
  }
  
  ####
  
  # print(events)
  
  # check the result
  # segmentDF$AutoEDAExtract[segmentDF$AutoEDAExtract!=""]
  # segmentDF$ManualEDAExtract[segmentDF$ManualEDAExtract!=""]
  
  return(segmentDF)
  
} # end newEDAExtractFn() 



