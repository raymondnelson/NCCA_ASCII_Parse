# pneumo feature extraction
# 4-28-2016
# Raymond Nelson



# first source a script for the pneumoMeasurementFn function
# already sourced by the featureExtraction.R script
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/pneumoMeasurement.R'), echo=FALSE)

# source("~/Dropbox/R/NCCA_ASCII_Parse/checkPneumoArtifacts.R", echo=FALSE)


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
    
    # segmentDF <- x
    # 
    # extract.params <- y
    
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
  }
  
  {
    # buffer around the verbal answer
    # pneumoAnsBuff is intialized in the NCCAASCII_init.R script
    aBuffOn <- answerRow - (pneumoAnsBuff * cps)
    if(aBuffOn <= stimOnsetRow) aBuffOn <- stimOnsetRow + 2
    aBuffOff <- answerRow + (pneumoAnsBuff * cps) - 1
    if(aBuffOff >= nrow(segmentDF)) aBuffOff <- nrow(segmentDF)
    
    ansBuffrows <- c(aBuffOn:aBuffOff)
  }
  
  if(responseEndRow > nrow(segmentDF)) {
    # correct for segments shorter than the measurement segment
    responseEndRow <- nrow(segmentDF) - 1
  } 
  
  #### submit the response indices to the time series data ####
  
  {
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
    
    # first source the pneumoMeasurement.R script
    # source(paste0(RPath, 'R/NCCA_ASCII_Parse/pneumoMeasurement.R'), echo=FALSE)
    
    tsDataUp <- segmentDF$c_UPneumoSm
    tsDataLp <- segmentDF$c_LPneumoSm
      
    # answer relative to stimulus onset
    verbalAnswer <- answerRow-(prestimSeg*cps)
    
    # compute the pneumo excursion measurement 
    # dataVector <- segmentDF$c_UPneumoSm[responseOnsetRow:responseEndRow]
    UPMeasurement <- pneumoMeasurementFn(dataVector=tsDataUp[responseOnsetRow:responseEndRow], 
                                         verbalAnswer=verbalAnswer)
    LPMeasurement <- pneumoMeasurementFn(dataVector=tsDataLp[responseOnsetRow:responseEndRow], 
                                         verbalAnswer=verbalAnswer)
    
    # abort the measurements if the respiration rate is not normal
    if(tRate != "normal" || aRate != "normal") {
      UPMeasurement <- "ONR"
      LPMeasurement <- "ONR"
    }
    
  }
  
  #### compute the respiration prestim measurements ####
  
  {
    # 
    # UPPrestimMeasurement <- 0
    # 
    # LPPrestimMeasurement <- 0
    
    UPPrestimMeasurement <-
      pneumoMeasurementFn(dataVector=segmentDF$c_UPneumoSm[prestimOnset:prestimOffset],
                          verbalAnswer=NULL)
    LPPrestimMeasurement <-
      pneumoMeasurementFn(dataVector=segmentDF$c_LPneumoSm[prestimOnset:prestimOffset],
                          verbalAnswer=NULL)

    # # abort the measurements if the respiration rate is not normal
    # if(tRate != "normal" || aRate != "normal") {
    #   UPPrestimMeasurement <- "ONR"
    #   LPPrestimMeasurement <- "ONR"
    # }
    
  }
  
  #### adjust the scale of the respiration measurements ####
  
  # 2020-07-25
  if( all(!is.null(UPMeasurement),
          !is.null(LPMeasurement),
          UPMeasurement != "ONR",
          LPMeasurement != "ONR") ) {
    
    # all segments need to have the same scaling factor
    # pneumoFEFactor is initialized in the NCCAASCII_init.R script
    UPMeasurement <- UPMeasurement * pneumoFEFactor
    LPMeasurement <- LPMeasurement * pneumoFEFactor
    
    UPPrestimMeasurement <- UPPrestimMeasurement * pneumoFEFactor
    LPPrestimMeasurement <- LPPrestimMeasurement * pneumoFEFactor

    # 2021-08-27 pneumoFEFactor is set in the NCCAASCII_init.R script
    # used to set the range or scale for the extracted values
    # UPMeasurement <- UPMeasurement * pneumoFEFactor
    # LPMeasurement <- LPMeasurement * pneumoFEFactor

    # UPPrestimMeasurement <- UPPrestimMeasurement * pneumoFEFactor
    # LPPrestimMeasurement <- LPPrestimMeasurement * pneumoFEFactor
    
  }
  
  #### check the dissimilarity in upper and lower respiration data ####
    
  # Sept 14, 2021
  # requires first auto-scaling the respiration data
  if( all(!is.null(UPMeasurement),
          !is.null(LPMeasurement),
          UPMeasurement != "ONR",
          LPMeasurement != "ONR") ) {
    # check the ratio of the upper and lower measurement
    
    # PRatio <- exp(abs(log(UPMeasurement / LPMeasurement)))
    # 
    # if(PRatio >= 1.12) {
    #   UPMeasurement <- NULL
    #   LPMeasurement <- NULL
    # }

  }
  
  #### compute the pre/post ratios for the upper and lower pneumos ####
  
  if( all(UPMeasurement != "ONR",
          LPMeasurement != "ONR",
          # !is.null(UPMeasurement),
          # !is.null(LPMeasurement), 
          UPPrestimMeasurement != "ONR",
          LPPrestimMeasurement != "ONR") ) {
    
    # pre/post method
    # smaller values indicate greater changes in physiology
    # UPMeasurement <- -log(UPPrestimMeasurement / UPMeasurement)
    # LPMeasurement <- -log(LPPrestimMeasurement / LPMeasurement)
    
    # RLE method
    # smaller values indicate greater changes in physiology
    UPMeasurement <- (UPMeasurement)
    LPMeasurement <- (LPMeasurement)
    
  } else {
    UPMeasurement <- NA
    LPMeasurement <- NA
  }
  
  #### remove pre/post ratios < 0 ####
  
  {
    # Oct 1, 2021 remove non-responses so that algorithms don't try to use them
    # UPMeasurement[which(UPMeasurement <= 0)] <- 0
    # LPMeasurement[which(LPMeasurement <= 0)] <- 0
    
    # log(pre/post) values are now either 0 or greater than 0
    # larger values indicate greater changes in physiology
  }
  
  #### apply the constraint to the pre/post ratios ####
  
  {
    # Sep 30, 2021 constraints are now applied in the pneumo_RC.R script
    
    # UPMeasurement[which(UPMeasurement < pneumoConstraintLow)] <- NA
    # LPMeasurement[which(LPMeasurement < pneumoConstraintLow)] <- NA
    
    # UPMeasurement[which(UPMeasurement > pneumoConstraintHigh)] <- 0
    # LPMeasurement[which(LPMeasurement > pneumoConstraintHigh)] <- 0
    
  }
  
  #### invert the sign of the respiration scores ####
  
  {
    
    # necessary because traditionally smaller numbers are larger reactions
    
    # UPMeasurement <- UPMeasurement * -1
    # LPMeasurement <- LPMeasurement * -1
    
    # this way the algorithms need no modification 
    
  }
  
  #### Sep 4, 2023 check the pneumo data for artifacts and instability ####
    
  # if(all(i == 1, seriesName == 2, chartName == "01A" && segmentName == "C6")) {
  #   assign("extract.params", extract.params, pos=1)
  #   assign("chartDF", chartDF, envir=.GlobalEnv)
  #   assign("segmentDF", segmentDF, pos=1)
  #   assign("segmentName", segmentName, pos=1)
  #   assign("sensorName", sensorName, pos=1)
  #   assign("tRate", UPMeasurement, pos=1)
  #   assign("aRate", LPMeasurement, pos=1)
  #   assign("tsDataUp", tsDataUp, pos=1)
  #   assign("tsDataLp", tsDataLp, pos=1)
  #   assign("responseOnsetRow", responseOnsetRow, pos=1)
  #   assign("responseEndRow", responseEndRow, pos=1)
  #   assign("answerRow", answerRow, pos=1)
  #   stop()
  #   # View(segmentDF)
  #   # extractList
  #   # extract.params
  #   # segmentDF$AutoEDAMeasure[301]
  #   # segmentDF$AutoEDAPrestim[301]
  # }
  
  {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/checkPneumoArtifacts.R", echo=FALSE)
    
    ## artifacts are evaluated using both the thoracic and abdominal data together ##
    
    artifactResult <- checkPneumoArtifactsFn(tsDataUp=tsDataUp,
                                               tsDataLp=tsDataLp,
                                               responseOnsetRow=responseOnsetRow,
                                               responseEndRow=responseEndRow,
                                               answerRow=answerRow,
                                               segmentDF=segmentDF ) 
    
    artifactCount2 <- length(which(artifactResult == "Artifact"))
    
    if(artifactCount2 > 0) {
      # submit the artifacts to the segment data frame
      artifactRowsUp <- which(artifactResult == "Artifact")
      artifactRowsLp <- which(artifactResult == "Artifact")
      
      segmentDF$UPneumo_a[artifactRowsUp] <- "Artifact"
      segmentDF$LPneumo_a[artifactRowsLp] <- "Artifact"
      
      # exclude answer distortion artifacts 
      # before sharing the artifacts with other sensors
      
      artifactRows <- sort(unique(c(artifactRowsUp, artifactRowsLp)))
      artifactRows <- artifactRows[!(artifactRows %in% ansBuffrows)]
      
      segmentDF$Artifacts_a[artifactRows] <- "Artifact"
    }
  
    artifactCount2 <- length(which(artifactResult[(stimOnsetRow-15):(segEndRow+15)] == "Artifact"))
    if(artifactCount2 > 0) {
      # no feature extraction if artifacts within the measured segment
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
  
  # {
  #   
  #   # call this from feature Extraction instead of here
  #   
  #   dataVector <- segmentDF$c_UPneumoSm
  #   inhVector <- segmentDF$c_UPneumoInh
  #   exhVector <- segmentDF$c_UPneumoExh
  #   
  #   answerRow <- extract.params$answer
  #   verbalAnswer <- verbalAnswer <- answerRow-(prestimSeg*cps)
  #   
  #   UPPatterns <- pneumoPatternsFn(dataVector=dataVector,
  #                                  inhVector=inhVector,
  #                                  exhVector=exhVector,
  #                                  verbalAnswer=verbalAnswer,
  #                                  rateDiff=05,
  #                                  ampDiff=.05,
  #                                  baseDiff=.05)
  #   
  #   dataVector <- segmentDF$c_LPneumoSm
  #   inhVector <- segmentDF$c_LPneumoInh
  #   exhVector <- segmentDF$c_LPneumoExh
  #   
  #   LPPatterns <- pneumoPatternsFn(dataVector=dataVector,
  #                                  inhVector=inhVector,
  #                                  exhVector=exhVector,
  #                                  verbalAnswer=verbalAnswer,
  #                                  rateDiff=05,
  #                                  ampDiff=.05,
  #                                  baseDiff=.05)
  #   
  #   # Feb 4 2022 still need to output the pattern data for each series
  #   
  #   patternDF <- rbind.data.frame(UPPatterns, LPPatterns)
  #   names(patternDF) <- c("rRate", "rAmp", "rBase", "RLE")
  #   patternDF <- cbind(examName=examName,
  #                      seriesName=seriesName,
  #                      chartName=chartName,
  #                      eventLabel=eventLabel,
  #                      patternDF)
  #   
  #   
  # }
  
  
  #### output ####
  
  # print the results
  # print(paste("upper:", UPMeasurement))
  # print(paste("lower:", LPMeasurement))
  
  # check the result
  # segmentDF$UPneumoExtract[segmentDF$UPneumoExtract!=""]
  # segmentDF$LPneumoExtract[segmentDF$LPneumoExtract!=""]
  
  return(segmentDF)
  
} # end newPneumoExtractFn()



