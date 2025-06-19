# pneumo feature extraction
# 4-28-2016
# Raymond Nelson



{
  
  # source a script for the pneumoMeasurementFn function
  source(paste0(RPath, 'pneumoMeasurement.R'), echo=FALSE)
  
  # source(paste0(RPath, "checkPneumoArtifacts.R"), echo=FALSE)
  
}


pneumoExtractFn <- function(segmentDF=segmentDF, 
                            extract.params=extract.params, 
                            tRate="normal", 
                            aRate="normal") {
  # pneumo feature extraction function 
  # is called from the feature extraction function
  # x input is a data frame for a stimulus segment
  # other parameters are obtained from the parent and global environment
  # output is the segmentDF 
  # after extracting the features 
  # and adding the extraction data to the extraction and measurement columns 
  ###
  
  {
    # View(segmentDF)
    
    examName <- segmentDF$examName[301]
    seriesName <- segmentDF$seriesName[301]
    chartName <- segmentDF$chartName[301]
    eventLabel <- segmentDF$eventLabel[301]
  }
  
  {
    if(!exists("tRate")) tRate <- "normal"
    if(!exists("aRate")) aRate <- "normal"
    
    UPMeasurement <- tRate
    LPMeasurement <- aRate
    
    # needed to manually steps through this function
    # UPMeasurement <- "normal"
    # LPMeasurement <- "normal"
    
    UPMeasurementRatio <- NULL
    LPMeasurementRatio <- NULL
  }
  
  {
    stimOnsetRow <- extract.params$onset
    stimOffsetRow <- extract.params$offset
    answerRow <- extract.params$answer
    segEndRow <- extract.params$end
    segmentName <- extract.params$segName
    chartName <- extract.params$chart
    seriesName <- extract.params$series
    examName <- extract.params$exam
  }
  
  #### stop for inspection ####
  
  {
    assign("segmentDF", segmentDF, envir=.GlobalEnv)
    assign("stimOnsetRow", stimOnsetRow, envir=.GlobalEnv)
    assign("stimOffsetRow", stimOffsetRow, envir=.GlobalEnv)
    assign("answerRow", answerRow, envir=.GlobalEnv)
    assign("segEndRow", segEndRow, envir=.GlobalEnv)
    assign("segmentName", segmentName, envir=.GlobalEnv)
    assign("chartName", chartName, envir=.GlobalEnv)
    assign("seriesName", seriesName, envir=.GlobalEnv)
    assign("examName", examName, envir=.GlobalEnv)
  }
  

  # if(all(examName=="DDaveTestMikeA0",
  #        chartName=="02A",
  #        seriesName=="1",
  #        segmentName=="C3")) {
  #   stop()
  # }
  
  
  #### get the stimulus indices ####
  
  {
    # get the prestim segment indices
    prestimOnset <- stimOnsetRow - (6*cps)
    if(prestimOnset < 1) prestimOnset <- 1
    prestimOffset <- stimOnsetRow - 1
    if(prestimOffset < 1) prestimOffset <- 1
  }
  
  {
    # reset the extracted values 
    segmentDF$UPneumoExtract <- ""
    segmentDF$LPneumoExtract <- ""

    segmentDF$UPneumoMeasure <- 0
    segmentDF$LPneumoMeasure <- 0
    
    segmentDF$UPneumo_a <- "0"
    segmentDF$LPneumo_a <- "0"
    segmentDF$Artifacts_a <- "0"
  }
  
  {
    # get the responseOnsetRow and responseEndRow
    responseOnsetRow <- stimOnsetRow + 1 
    responseEndRow <- segEndRow + 1
    # correct for end row after the end of the segment 
    if(responseEndRow > nrow(segmentDF))  responseEndRow <- nrow(segmentDF) - 1
  }
  
  if(excludePneumoAnswerBuffer) {
    # Nov 6, 2023
    # buffer around the verbal answer 
    # this is done here for artifacts
    # the answer sub-segment is excluded by the pneumoMeasurementF() in another script
    # pneumoAnsBuff is intialized in the NCCAASCII_init.R script
    aBuffOn <- answerRow - (pneumoAnsBuff * cps)
    if(aBuffOn <= stimOnsetRow) aBuffOn <- stimOnsetRow + 2
    aBuffOff <- answerRow + (pneumoAnsBuff * cps) - 1
    if(aBuffOff >= nrow(segmentDF)) aBuffOff <- nrow(segmentDF)
    
    ansBuffRows <- c(aBuffOn:aBuffOff)
  }
  
  #### submit the response indices to the time series data ####
  
  {
    
    # Dec 8, 2023
    # add the prestim onset to the 2 pneumo extract columns
    segmentDF$UPneumoExtract[prestimOnset] <- "prestimOnsetRow"
    segmentDF$LPneumoExtract[prestimOnset] <- "prestimOnsetRow"
    segmentDF$UPneumoExtract[prestimOffset] <- "prestimOffsetRow"
    segmentDF$LPneumoExtract[prestimOffset] <- "prestimOffsetRow"
    
    # add the stimulus and answer data to the 2 pneumo extract columns
    segmentDF$UPneumoExtract[stimOnsetRow] <- "onsetRow"
    segmentDF$LPneumoExtract[stimOnsetRow] <- "onsetRow"
    segmentDF$UPneumoExtract[stimOffsetRow] <- "offsetRow"
    segmentDF$LPneumoExtract[stimOffsetRow] <- "offsetRow"
    segmentDF$UPneumoExtract[answerRow] <- "answerRow"
    segmentDF$LPneumoExtract[answerRow] <- "answerRow"
    
    # add the response onset and response end data to the 2 pneumo extract columns
    segmentDF$UPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
    segmentDF$LPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
    segmentDF$UPneumoExtract[responseEndRow] <- "responseEndRow"
    segmentDF$LPneumoExtract[responseEndRow] <- "responseEndRow"
    
    # add the answer distortion buffer 
    segmentDF$UPneumoExtract[aBuffOn] <- "aBuffOn"
    segmentDF$UPneumoExtract[aBuffOff] <- "aBuffOff"
    segmentDF$LPneumoExtract[aBuffOn] <- "aBuffOn"
    segmentDF$LPneumoExtract[aBuffOff] <- "aBuffOff"  
  }
  
  #### get the respiration measurements ####
  
  {
    
    # source(paste0(RPath, 'R/NCCA_ASCII_Parse/pneumoMeasurement.R'), echo=FALSE)
    
    # assign("segmentDF", segmentDF, envir=.GlobalEnv)
    # assign("responseOnsetRow", responseOnsetRow, envir=.GlobalEnv)
    # assign("responseEndRow", responseEndRow, envir=.GlobalEnv)
    # assign("answerRow", answerRow, envir=.GlobalEnv)
    # stop()
    
    tsDataUp <- segmentDF$c_UPneumoSm
    tsDataLp <- segmentDF$c_LPneumoSm

    # compute the pneumo excursion measurement
    UPMeasurement <- pneumoMeasurementFn(dataVector=tsDataUp[responseOnsetRow:responseEndRow],
                                         verbalAnswer=answerRow,
                                         pnBufferLen=pneumoMeasurementBuffer, 
                                         output="mean")
    LPMeasurement <- pneumoMeasurementFn(dataVector=tsDataLp[responseOnsetRow:responseEndRow],
                                         verbalAnswer=answerRow,
                                         pnBufferLen=pneumoMeasurementBuffer, 
                                         output="mean")

    # Dec 8 2023, compute the respiration prestim measurements
    UPPrestimMeasurement <- pneumoMeasurementFn(tsDataUp[prestimOnset:prestimOffset])
    LPPrestimMeasurement <- pneumoMeasurementFn(tsDataLp[prestimOnset:prestimOffset])

    # abort the measurements if the respiration rate is not normal
    if(tRate != "normal" || aRate != "normal") {
      UPMeasurement <- "ONR"
      LPMeasurement <- "ONR"
      UPPrestimMeasurement<- "ONR"
      LPPrestimMeasurement <- "ONR"
    }
    
  }
  
  #### adjust the scale of the respiration measurements ####
  
  # 2020-07-25
  if( all(!is.na(UPMeasurement),
          !is.na(LPMeasurement),
          !is.na(UPPrestimMeasurement),
          !is.na(LPPrestimMeasurement)) ) {
    if( all(UPMeasurement != "ONR",
            LPMeasurement != "ONR",
            UPPrestimMeasurement != "ONR",
            LPPrestimMeasurement != "ONR") ) {
      
      # pneumoFEFactor is initialized in the NCCAASCII_init.R script

      UPMeasurement <- UPMeasurement * pneumoFEFactor
      LPMeasurement <- LPMeasurement * pneumoFEFactor

      UPPrestimMeasurement <- UPPrestimMeasurement * pneumoFEFactor
      LPPrestimMeasurement <- LPPrestimMeasurement * pneumoFEFactor
      
    }
  } else {
    
    # if the respiration data are outside the normal range
    
    UPMeasurement <- NA
    LPMeasurement <- NA

    UPPrestimMeasurement <- NA
    LPPrestimMeasurement <- NA
    
  }
  
  
  #### ARTIFACTS Sep 4, 2023 check the respiration data for ARTIFACTS ####
    
  
  {
    assign("extract.params", extract.params, pos=1)
    # assign("extractList", extractList, pos=1)
    assign("chartDF", chartDF, envir=.GlobalEnv)
    assign("segmentDF", segmentDF, pos=1)
    assign("segmentName", segmentName, pos=1)
    # assign("sensorName", sensorName, pos=1)
    assign("tRate", UPMeasurement, pos=1)
    assign("aRate", LPMeasurement, pos=1)
    assign("UPMeasurement", UPMeasurement, pos=1)
    assign("LPMeasurement", LPMeasurement, pos=1)
    assign("tsDataUp", tsDataUp, pos=1)
    assign("tsDataLp", tsDataLp, pos=1)
    assign("responseOnsetRow", responseOnsetRow, pos=1)
    assign("responseEndRow", responseEndRow, pos=1)
    assign("answerRow", answerRow, pos=1)
  }
  
  # if(all(i == 1, seriesName == "X", chartName == "02A" && segmentName == "C5")) {
  # # if(all(examName=="X4GSTT6X0011AF13", seriesName=="X", chartName=="02A", segmentName=="C5")) {
  #   # View(segmentDF)
  #   # extractList
  #   # extract.params
  #   # segmentDF$AutoEDAMeasure[301]
  #   # segmentDF$AutoEDAPrestim[301]
  #   stop()
  # }
  
  # {
  #   View(segmentDF)
  #   extractList
  #   extract.params
  #   segmentDF$AutoEDAMeasure[301]
  #   segmentDF$AutoEDAPrestim[301]
  # }
  
  # if(all(examName=="X4GSTT6X0011AF13",
  #        chartName=="02A",
  #        seriesName=="1",
  #        segmentName=="C3")) {
  #   stop()
  # }
  
  
  if( all(!is.na(c(UPMeasurement, LPMeasurement)), artifactPneumo, processArtifacts) ) {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/checkPneumoArtifacts.R", echo=FALSE)
    
    # call a function to check for artifacts  
    artifactResultDF <- checkPneumoArtifactsFn(responseOnsetRow=responseOnsetRow,
                                               responseEndRow=responseEndRow,
                                               answerRow=answerRow,
                                               segmentDF=segmentDF ) 
    
    # View(artifactResultDF)
    
    # exclude the first sample and last sample
    artifactResultDF[c(1,nrow(artifactResultDF)),] <- "0"
    
    artifactRowsUp <- which(artifactResultDF$outVcU != "0")
    artifactRowsLp <- which(artifactResultDF$outVcL != "0")
    
    # select which artifacts to share with other sensors
    # shareArtifacts <- c("ArtifactDB", "ArtifactAP")
    shareArtifacts <- c("ArtifactBDB", "ArtifactAP")
    
    # combine the artifacts for upper and lower
    artifactRows2 <- sort(unique(c(artifactRowsUp, artifactRowsLp)))
    
    # combine sharedartifacts from thoracic and abdominal sensors
    # retain only deep breath and apnea artifacts to share with other sensors
    artifactRowsUpS <- which(artifactResultDF$outVcU %in% shareArtifacts)
    artifactRowsLpS <- which(artifactResultDF$outVcL %in% shareArtifacts)
    artifactRowsS <- sort(unique(c(artifactRowsUpS, artifactRowsLpS)))
    
    # exclude artifact marks during the normal answer distortion artifacts
    # before sharing the artifacts with other sensors
    
    # Oct 18, 2024 - this is already done during artifact extraction
    # because some artifacts are removed and some are preserved at the verbal answer
    # if(excludePneumoAnswerBuffer) {
    #   artifactRowsUpS <- artifactRowsUpS[!(artifactRowsUpS %in% ansBuffRows)]
    #   artifactRowsLpS <- artifactRowsLpS[!(artifactRowsLpS %in% ansBuffRows)]
    #     
    #   artifactRows <- artifactRows[!(artifactRows %in% ansBuffRows)]
    #   
    #   # artifactRowsUp <- artifactRowsUp[!(artifactRowsUp %in% ansBuffRows)]
    #   # artifactRowsLp <- artifactRowsLp[!(artifactRowsLp %in% ansBuffRows)]
    #   # 
    #   # artifactRows2 <- artifactRows2[!(artifactRows2 %in% ansBuffRows)]
    # }
    
    # submit the artifacts to the segment data frame - separately for upper and lower sensors
    if(length(length(artifactRowsUp) > 0)) {
      segmentDF$UPneumo_a[artifactRowsUp] <- "Artifact"
    } 
    if(length(length(artifactRowsLp) > 0)) {
      segmentDF$LPneumo_a[artifactRowsLp] <- "Artifact"
    } 
  
    if(integrateArtifacts) {
      # only the deep breath and apnea artifacts to the shared artifact vector
      segmentDF$Artifacts_a[artifactRowsS] <- "Artifact"
    }
    
    # check for artifacts during the measured segment
    measuredSegmentRows <- c((stimOnsetRow):(segEndRow))
    artifactRows3 <- which(artifactRowsUp %in%  measuredSegmentRows)
    artifactRows4 <- which(artifactRowsLp %in%  measuredSegmentRows)
    artifactRows5 <- which(artifactRowsS %in%  measuredSegmentRows)
    
    # artifactRows3 <- sort(unique(c(artifactRowsUp, artifactRowsLp, artifactRowsS)))
    # artifactCount3 <- which(artifactRows3 %in%  measuredSegmentRows)
    # # which(artifactRows %in%  c((stimOnsetRow-round(pneumoAnsBuff * cps)):(segEndRow+15)))
    
    if(length(artifactRows3) > 0) {
      # no feature extraction if artifacts exist within the measured segment
      UPMeasurement <- NA
    }
    
    if(length(artifactRows4) > 0) {
      # no feature extraction if artifacts exist within the measured segment
      LPMeasurement <- NA
    }
    
    if(length(artifactRows5) > 0) {
      # no feature extraction if artifacts exist within the measured segment
      UPMeasurement <- NA
      LPMeasurement <- NA
    }
    
  } # end pneumo artifact section
  
  #### add the excursion values to the data frame ####
  
  {
    if(!is.null(UPMeasurement)) {
      segmentDF$UPneumoMeasure[stimOnsetRow:stimOffsetRow] <- UPMeasurement
    }
    if(!is.null(LPMeasurement)) {
      segmentDF$LPneumoMeasure[stimOnsetRow:stimOffsetRow] <- LPMeasurement
    }
    # View(segmentDF)
  }
  
  #### compute the pattern extraction for the upper and lower respiration ####

  {

    ## pattern extraction is accomplished from a separate function called in the featureExtractionFn() ##

  }
  
  #### output ####
  
  # print the results
  # print(paste("upper:", UPMeasurement))
  # print(paste("lower:", LPMeasurement))
  
  # check the result
  # segmentDF$UPneumoExtract[segmentDF$UPneumoExtract!=""]
  # segmentDF$LPneumoExtract[segmentDF$LPneumoExtract!=""]
  
  return(segmentDF)
  
} # end newPneumoExtractFn()



