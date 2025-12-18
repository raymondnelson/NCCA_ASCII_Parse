# amplitude extract function for EDA and Cardio data
# 10-31-2015
# Aug 1, 2020 simplified version
# Raymond Nelson
#
####



{
  
  # source(paste0(RPath, "amplitudeExtractHelperFunctions.R'))
  
  # source the maxSlopeChangeFn
  # source(paste0(RPath, 'slopeChange.R'), echo=FALSE)
  
  # source(paste0(RPath, "checkEDATonicity.R", echo=FALSE)
  
  # source(paste0(RPath, "checkCardioArtifacts.R", echo=FALSE)
  
  # # August 2023 functions were abstracted from amplitudeExtractPC.R
  # source(paste0(RPath, "getResponsePeaks.R"), echo=FALSE)
  # source(paste0(RPath, "getResponseOnsets.R"), echo=FALSE)
  # source(paste0(RPath, "getMaxOnsetPeakDistance.R"), echo=FALSE)
  # source(paste0(RPath, "getSlopeDirection.R"), echo=FALSE)
  # source(paste0(RPath, "abstractScale.R"), echo=TRUE)
  
  
}



amplitudeExtractFnPC <- function(extractList=AutoExtractList, env.params=env.params) {
  # function to extract the amplitude of EDA and cardio rise or increase in response to a stimulus
  # 7-25-2020
  # Raymond Nelson

  # simple feature extraction method for EDA and cardio
  # without the nuances around latency, ROW and slope onset
  
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
  
  # procedure for each stimulus segment
  # a. each stimulus segment consists of the 15 second evaluation window along with 10 prestimulus seconds and 10 poststimulus seconds
  # 1. locate the row indices for the onset of all negative slope segments - these are the peak of reaction
  # 2. keep only those response peak indices from 2.5 sec to 15 sec
  # 2a. include the index at 15 seconds 
  # 2b. locate one additional peak after the end of the 15 second evaluation window
  #      this is the inclusive evaluation window (score to the end of reaction even if it is outside the evaluation window)
  # 2c.  discard the additional peak (outside the evaluation window) if the slope changes to negative between the 13.5 seconds and the additional peak.
  #      this prevents scoring a positive slope segment that begins very late in the evaluation window.
  #      (do not change the 15 second evaluation window)
  # 2d. use 14.5 seconds for cardio because cardio data is inherently more complex than EDA 
  # 3. locate the row indices for the onset of all positive slope segments
  # 4. keep only those response onsets from latency to ROWEnd
  #    latency is .5 sec, and
  #    ROWEnd is 5 sec after the verbal answer or stimulus offset if no answer
  # 5. always add the index at at 2.5 seconds as a response onset
  # 6. remove peak indices after ROWEnd after the data have descended
  #    below the lowest onset 
  #    get the data values for all peak and onset indices
  # 7. for each peak, compare the data value with all onset values that precede the peak and locate the onset index with the max y (vertical) distance
  # 8. select the onset and peak indices with the max distance
  # 9. response measurement is max y distance from onset to peak
  
  # output is a list of extracted information 
  
  #### load some helper functions ####
  
  # {
  #   
  #   # these are sourced earlier 
  #
  #   source(paste0(RPath, "getResponsePeaks.R"), echo=FALSE)
  #   
  #   source(paste0(RPath, "getResponseOnsets.R"), echo=FALSE)
  #   
  #   source(paste0(RPath, "getMaxOnsetPeakDistance.R"), echo=FALSE)
  #   
  #   source(paste0(RPath, "getSlopeDirection.R"), echo=FALSE)
  #   
  # }
  
  #### begin ####
  
  # extractList <- AutoExtractList
  # extractList <- ManualExtractList
  
  # get the information from the env.params input List
  
  {
    cps <- env.params$dataRate
    Lat <- env.params$Lat
    useROW <- env.params$useROW
    ROWStart <- env.params$ROWStart
    ROWStop <- env.params$ROWStop
    ROWEnd <- env.params$ROWEnd
    nSmooth <- env.params$ignore
    strictROW <- env.params$strictROW
    strictWindow <- env.params$strictWindow
    descentRule <- env.params$descentRule
    descProp <- env.params$descProp
    slopeChangeRule <- env.params$slopeChangeRule
    inflection <- env.params$inflection
    nothingIsSomething <- env.params$nothingIsSomething
    prestim <- env.params$prestim # Aug 8, 2023
  }
  
  # if(segmentName == "C12" && sensorName == "CardioSystolic") {
  #   stop()
  # }
  
  # get the information from the input list
  
  {
    Begin <- as.numeric(extractList$begin)
    End <- as.numeric(extractList$end)
    Answer <- as.numeric(extractList$answer)
    
    examName <- extractList$examName
    seriesName <- extractList$seriesName
    chartName <- extractList$chartName
    segmentName <- extractList$segmentName
    segmentTitle <- extractList$segmentTitle
    sensorName <- extractList$sensorName
    
    useArtifacts <- extractList$useArtifacts
    
    tsData <- extractList$dataVector
    
    artifactVector <- extractList$artifactVector
    
    # segmentDF is not used by the EDA tonicity function
    # but is used by the cardio artifact function
    segmentDF <- extractList$segmentDF
    # View(segmentDF)
  
    assign("AutoExtractList", extractList, envir = .GlobalEnv)
  }
  
  
  # if(all(chartName == "03A", segmentName == "C7", sensorName == "AutoEDA")) {
  # if(all(examName=="D5HPNYP", seriesName=="X", chartName == "01A", segmentName == "C3", sensorName == "AutoEDA")) {
  #     # stop for inspection
  #   assign("AutoExtractList", extractList, envir = .GlobalEnv)
  #   assign("extractList", extractList, envir = .GlobalEnv)
  #   assign("segmentDF", segmentDF, envir = .GlobalEnv)
  #   assign("tsData", tsData, envir = .GlobalEnv)
  #   print(segmentName)
  #   print(sensorName)
  #   assign("sensorName", sensorName, envir= .GlobalEnv)
  #   assign("segmentName", segmentName, envir= .GlobalEnv)
  #   assign("chartName", chartName, envir = .GlobalEnv)
  #   # plot.ts(tsData)
  #   stop()
  # }
  
  # get the information from the input list
  
  #### Nov 11, 2023 smooth the EDA data more to reduce high frequency information ####
  
  tsDataB <- tsData
  # tsDataB is used by the getResponsePeaksfn() and getResponseOnsetsFn()
  # tsDataB is passed to the maxSlopechangeFn()

  if(sensorName %in% c("AutoEDA", "ManualEDA")) {
    # additional smoothing to reduce location of response onset due to high frequency noise
    tsDataB <- MASmooth(tsData, y=3, times=1) # was 7
    if(length(tsDataB) < length(tsData)) {
      tsDataB <- c(rep(tsDataB[1], length(tsData) - length(tsDataB)))
    }
    # plot.ts(tsDataB)
  } 
  
  #### reset the artifact column for this sensor Nov 29, 2023 ####
  
  if(processArtifacts) {
    
    if(sensorName == "AutoEDA") {
      # segmentDF$AutoEDA_a <- 0
    } else if(sensorName == "ManualEDA") {
      # segmentDF$ManualEDA_a <- 0
    } else if(sensorName == "CardioMA") {
      segmentDF$Cardio1_a <- 0
    }
    
  }
  
  #### exit if the time series data are flatlined ####
  
  # to avoid problems with flatlined EDA data
  if(sd(tsData)==0)  {
    # standard deviation will be 0 for flatLined data
    
    flatLined <- TRUE
    assign("flatLined", flatLined, envir=.GlobalEnv)
    
    # construct the output vector
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
    return(output)
  } else {
    flatLined <- FALSE
    assign("flatLined", flatLined, envir=.GlobalEnv)
  }
  
  #### set the starting row and ending row ####
  
  {
    startRow <- 1
    DFRows <- length(tsData)
    
    # additional latency for ascending reactions
    # addLat <- 2 # maybe use the sChangeLat environment variable
    # 2024Jul29
    addLat <- sChangeLat
    # will be added to the .5 sec latency
    # so that the value at  2.5seconds may be used as an onset
    # if there is no positive slope onset
    # using the blunt approximation method (not the statistical method)
    
    if(!exists("strictWindow")) strictWindow <- TRUE
    
    # measuredSeg and shortenEW are set in the init script
    if(sensorName == "CardioMA") {
      measuredSegA <- ifelse(!isTRUE(strictWindow), 
                             measuredSeg  - shortenEW,
                             measuredSeg) 
      # was -.5 sec until Oct 27 2020
    } else {
      # for manual and auto EDA
      measuredSegA <- ifelse(!isTRUE(strictWindow), 
                             measuredSeg  - shortenEW,
                             measuredSeg) 
      # was - 1.5 sec until Oct 27, 2020
    }
    
    # meaasuredSegA is the shortened measurement window 
    # used to avoid scoring segments that ascend late in the EW 
    # when using the non-strict EW
  }
  
  ####   initialize the event indices   ####
  
  {
    # prestimRow is usually the first row (1) of the time series vector
    # Aug 8, 2023
    # EDA and Cardio have 5 sec prestim for now and may utilize different lengths
    prestimRow <- Begin - (startRow-1) - (cps*prestim) 
    if(prestimRow<=0) prestimRow <- 1
    # onset of the stimulus in the time series vector
    onsetRow <- Begin - (startRow-1) 
    # end of the evaluation window (EW)
    endRow <- onsetRow + (cps*measuredSeg) - 1 
    if(endRow > DFRows) endRow <- DFRows
    # also make another endRow named endRowA 
    # for the shortened EW when using strictWindow==FALSE
    endRowA <- onsetRow + (cps*measuredSegA) - 1
    # check the the measured segment does not exceed the data frame rows
    if(endRowA > DFRows)  endRowA  <- DFRows
    # end of the question stimulus
    offsetRow <- End - (startRow-1) 
    if(offsetRow >= (endRow-2)) offsetRow <- endRow - 2
    # response latency period
    latRow <- onsetRow + cps*Lat + (0*cps) # maybe add 1.5 second for PCASS 
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
    ROWEndRow <- ROWStopRef + (cps*ROWEnd)
    if(ROWEndRow > endRow) ROWEndRow <- endRow - 1
    if(ROWEndRow > (DFRows-3)) ROWEndRow <- DFRows - 2
  } 
  
  #### initialize some output objects ####
  
  { 
    yChangeOnset <- NA
    yChangeOnsetValue <- NA
    yChangePeak <- NA
    yChangePeakValue <- NA
    yChangeValue <- NA
  }
  
  ####   locate the response peak indices   ####
  
  # source("~/Dropbox/R/NCCA_ASCII_Parse/getResponsePeaks.R")
  xPeak <- getResponsePeaksFn(tsData=tsDataB, 
                              latRow=latRow, 
                              ROWEndRow=ROWEndRow, 
                              endRow=endRow, 
                              endRowA=endRowA,
                              addLat=addLat,
                              strictROW=strictROW, 
                              strictWOE=strictWindow )
  
  ####   locate the response onset indices   ####
  
  # source("~/Dropbox/R/NCCA_ASCII_Parse/getResponseOnsets.R")
  xOnset <- getResponseOnsetsFn(tsData=tsDataB, 
                                xPeak=xPeak,
                                onsetRow=onsetRow,
                                latRow=latRow, 
                                ROWEndRow=ROWEndRow,
                                endRow=endRow,
                                slopeChangeRule=slopeChangeRule, 
                                addLat=addLat)
  # plot.ts(tsData)
  # plot.ts(tsDataB)
  # tsData_save <- tsData
  # tsData <- tsDataB
  
  ####  keep response onset indices prior to the last peak index  ####
  
  {
    # Feb 03, 2024
    # xOnset <- xOnset[which(xOnset < xPeak[length(xPeak)])]
  }
  
  #### exclude peaks after the data descend below the onset value ####
  
  {
    # initialize this to NA to avoid problems
    postROWXOnset <- NULL
    # use the xPosSlope vector from earlier
    
    # August 2023
    # call a function to get the + slope indices
    xPosSlope <- getPosSlopeFn(getTheSlopeFn(tsData))
    
    # get the pos slope onset indices after ROWEndRow
    postROWXOnset <- which(xPosSlope[ROWEndRow:endRow] == 1) + ROWEndRow - 1
    
    if(length(postROWXOnset) > 0) {
      # get the data values for onset indices after the ROW
      postROWXOnsetVals <- tsData[postROWXOnset]
      minPostROWVal <- min(tsData[postROWXOnset])
      
      # get min xOnset
      # August 19, 2023 # restricted to xOnset during the ROW
      thisMinOnset <- xOnset[which.min(tsData[xOnset[xOnset <= ROWEndRow]])]
      minOnsetVal <- tsData[thisMinOnset]
      # exclude peaks after thisMinOnset
      # if the min post ROW onset val < minOnsetVal
      if(any(postROWXOnsetVals <= minOnsetVal)) {
        # remove xPeak indices 
        # after the data have descended below the minOnsetVal
        # select the first if several
        stopHere <- postROWXOnset[which(postROWXOnsetVals < minOnsetVal)[1]]
        # keep only xPeak indices before data descend below the lowest point in the ROW
        xPeak <- xPeak[which(xPeak < stopHere)]
        # Aug 22, 2023 
        xOnset <- xOnset[xOnset < stopHere]
      }
    }
    
    # xPeak may be NA or empty at this point 
    # if the data are descending from 2.5 seconds
  }
  
  #### discard xOnset indices after the last xPeak ####
  
  {
    # Feb 03, 2024
    xOnset <- xOnset[which(xOnset < xPeak[length(xPeak)])]
  }
  
  #### check for no usable response at this point #####
  
  # Nov 19, 2022 fixed vectorized is.na()
  if(length(xOnset)==0 || any(is.na(xOnset))) {
    xOnset <- NA
    xPeak <- NA
  }
  
  #### strict ROW option ####
  
  # August 2023
  # now occurs in the getOnsets function that was abstracted from this function
  
  #### extract max distance for each xPeak to all preceding xOnset vals ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) {
    
    # # for each xPeak value, calculate the max y distance
    # # to all preceding xOnset value
    
    # Nov 1, 2023
    # there may have been an error in the previous code here
    # the max distance function appears to do a better job 
    # when the onset is imputed via slope change
    
    # August 2023
    # now abstracted to a separate function
    yChangeList <- maxOnsetPeakDistFn(tsData=tsData, 
                                      xOnset=xOnset, 
                                      xPeak=xPeak, 
                                      sensorName )
    
    yChangeOnset <-yChangeList[['yChangeOnset']]
    yChangeOnsetValue <- yChangeList[['yChangeOnsetValue']]

    yChangePeak <- yChangeList[['yChangePeak']]
    yChangePeakValue <- yChangeList[['yChangePeakValue']]

    yChangeValue <- yChangeList[['yChangeValue']]
    
  }
  
  #### fix some possible problems when there is no usable response ####
  
  {
    # in case there is no yChangeValue
    # because the slope is persistently negative
    
    # only if there are any + difference values
    if(any(is.na(yChangeValue) || yChangeValue < 0 || length(yChangeValue) == 0)) {
      # check if the slope is negative from 2.5 sec to endRow
      if(tsData[endRow] - tsData[latRow+round(addLat*cps,0)] < 0) {
        yChangeOnset <- (endRow - 1)
        yChangeOnsetValue <- tsData[endRow]
        yChangePeak <- endRow
        yChangePeakValue <- tsData[endRow]
        yChangeValue <- 0
        # yChangeValue should be 0 for persistent descending data
      } else {
        # if the slope is + then change the yChangeValue to NA
        yChangeOnset <- (endRow - 1)
        yChangeOnsetValue <- tsData[endRow]
        yChangePeak <- endRow
        yChangePeakValue <- tsData[endRow]
        yChangeValue <- NA
        # yChange Value should be NA for persistent ascending data
      }
    } # end if there are no + difference values
    
    # double check against the potential for - y distance vals
    if(!is.na(yChangeValue) && yChangeValue <= 0) {
      yChangeOnset <- NA
      yChangeOnsetValue <- NA
      yChangePeak <- NA
      yChangePeakValue <- NA
      yChangeValue <- NA
    }
    
    # Sep 27, 2021 
    # use an environment parameter to set NA response value to 0
    if(isTRUE(nothingIsSomething)) {
      yChangeOnset <- NA
      yChangeOnsetValue <- NA
      yChangePeak <- NA
      yChangePeakValue <- NA
      yChangeValue <- 0
      # yChangeValue will be NA if no response and nothingIsSomething=FALSE
    }
    
  }
  
  ##############################################################
  
  #### check the respiration data for artifacts Sep 11, 2023 ####
  
  {
    
    # respiration feature extraction is handled by the 
    # pneumoExtractFn() in the pneumoExtract.R script 
    
    # this amplitudeExtract.R script is not called for respiration
    
  }
  
  
  ############################################################
  
  #### check the cardio data for artifacts - Aug 24, 2023 ####
  
  if( all(!is.na(yChangeValue),
          sensorName == "CardioMA", 
          artifactCardio, 
          processArtifacts) ) {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/checkCardioArtifacts.R", echo=FALSE)
    
    {
      cardioMA <- segmentDF$c_CardioMA
      cardioMid <- segmentDF$c_CardioMid
      tsData <- segmentDF$c_Cardio1
      artifactVector <- segmentDF$Cardio1_a
    }
    
    # if(all(i == 1, seriesName == 2, chartName == "03A", segmentName == "R1", sensorName=="CardioMA")) {
    # # if(all(examName=="D7JQCNA" , chartName == "03A", segmentName == "C6", sensorName=="CardioMA")) {
    #   assign("extract.params", extract.params, pos=1)
    #   assign("examName", examName, pos=1)
    #   assign("chartDF", chartDF, envir=.GlobalEnv)
    #   assign("segmentDF", segmentDF, pos=1)
    #   assign("sensorName", sensorName, pos=1)
    #   assign("segmentName", segmentName, envir=.GlobalEnv)
    #   assign("tsData", tsData, envir=.GlobalEnv)
    #   assign("cardioMA", cardioMA, envir=.GlobalEnv)
    #   assign("cardioMid", cardioMid, envir=.GlobalEnv)
    #   assign("onsetRow", onsetRow, envir=.GlobalEnv)
    #   assign("yChangeOnset", yChangeOnset, envir=.GlobalEnv)
    #   assign("yChangePeak", yChangePeak, envir=.GlobalEnv)
    #   assign("cardioPrestim", cardioPrestim, envir=.GlobalEnv)
    #   stop()
    #   # View(segmentDF)
    #   # extractList
    #   # extract.params
    #   # segmentDF$AutoEDAMeasure[301]
    #   # segmentDF$AutoEDAPrestim[301]
    # }
    
    # plot.ts(cardioMA)
    # plot.ts(cardioMid)
    # plot.ts(tsData)
    
    if(!exists("prestim")) prestim <- cardioPrestim
    
    artifactResult <- checkCardioArtifactsFn(segmentName=segmentName,
                                             tsData=tsData,
                                             # artifactVector=artifactVector,
                                             cardioMA=cardioMA,
                                             cardioMid=cardioMid,
                                             onsetRow=onsetRow,
                                             yChangeOnset=yChangeOnset,
                                             yChangePeak=yChangePeak,
                                             cardioPrestim=prestim,
                                             segmentDF=segmentDF )
    
    artifactRowsC <- which(artifactResult != "0") 
    # artifactResult[artifactRowsC]
    
    # fasiculation artifacts
    artifactRowsFasic <- artifactRowsC[artifactResult[artifactRowsC] == "Artifact1s"]
    
    # extrasystole artifacts
    artifactRowsExt <- artifactRowsC[artifactResult[artifactRowsC] == "Artifact3"]
    
    # exclude fasiculation artifacts
    artifactRows <- artifactRowsC[artifactResult[artifactRowsC] != "Artifact1s"]
    
    if(all(integrateCardioArtifacts)) {
      # submit the artifacts to a column for cross-talk between the sensors 
      # all cardio artifacts can be shared with other sensors
      segmentDF$Artifacts_a[artifactRows] <- "Artifact"
      
      
    }
    
    # artifactRows <- NULL
    
    responseRows <- c(c(yChangeOnset - (cardioPrestim*cps)):c(yChangePeak + 1))
    
    # integration of artifacts from other sensors
    if(all(integrateArtifacts)) {
      # obtain  the artifacts from the activity and respiration sensors
      artifactRowsC <- 
        sort(unique(c(artifactRows, which(segmentDF$Artifacts_a == "Artifact"))))
      
      if(is.na(yChangeOnset) || is.na(yChangePeak)) {
        # when there is no usable cardio response
        artifactRows <- artifactRows
      } else {
        # when there is a usable cardio response
        # limit the artifacts to the response rows when cancelling the feature extraction
        # responseRows <- c(c(yChangeOnset - (cardioPrestim*cps)):c(yChangePeak + 1))
        artifactRows <- artifactRows[which((artifactRowsC %in% responseRows))]
      } 
    }
    
    segmentDF$Cardio1_a[artifactRows] <- "Artifact"
    
    # check the artifacts during the response
    artifactRowR <- artifactRows[which((artifactRows %in% responseRows))]
    
    # plot.ts(tsData)
    # artifactCount <- length(artifactRowsC)
    
    if(all(length(artifactRowR) > 0)) {
      # remove the extracted response information if the EDA data are not tonic
      yChangeOnset <- NA
      yChangeOnsetValue <- NA
      yChangePeak <- NA
      yChangePeakValue <- NA
      yChangeValue <- NA
    }
    
  } # end cardio artifact section 
  
  ############################################
  
  #### check the EDA data for artifacts and tonicity Aug 8, 2023 ####
  
  if(all(!is.na(yChangeValue),
         sensorName %in% c("AutoEDA", "ManualEDA"), 
         artifactEDA, 
         processArtifacts)) {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/checkEDATonicity.R", echo=FALSE)
    
    # plot.ts(tsData)
    
    # if(all(seriesName==1, chartName=="01A", segmentName=="4", sensorName=="AutoEDA")) {
    # if(all(chartName=="01A", segmentName=="R10", sensorName=="AutoEDA")) {
    # if(all(examName=="D5HPNYP", seriesName=="X", chartName=="01A", segmentName=="C3", sensorName=="AutoEDA")) {
    #   assign("extract.params", extract.params, pos=1)
    #   assign("chartDF", chartDF, envir=.GlobalEnv)
    #   assign("segmentDF", segmentDF, pos=1)
    #   assign("segmentName", segmentName, envir=.GlobalEnv)
    #   assign("sensorName", sensorName, pos=1)
    #   assign("tsData", tsData, envir=.GlobalEnv)
    #   assign("onsetRow", onsetRow, envir=.GlobalEnv)
    #   assign("yChangeOnset", yChangeOnset, envir=.GlobalEnv)
    #   assign("yChangeValue", yChangeValue, envir=.GlobalEnv)
    #   assign("yChangePeak", yChangePeak, envir=.GlobalEnv)
    #   assign("yChangePeakValue", yChangePeakValue, envir=.GlobalEnv)
    #   assign("EDAPrestim", EDAPrestim, envir=.GlobalEnv)
    #   stop()
    #   # View(segmentDF)
    #   # extractList
    #   # extract.params
    #   # segmentDF$AutoEDAMeasure[301]
    #   # segmentDF$AutoEDAPrestim[301]
    # }
    
    if(!exists("prestim")) prestim <- env.params$prestim
    
    EDAPrestim <- prestim
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/checkEDATonicity.R", echo=FALSE)
    artifactResult <- checkEDATonicityFn(segmentName=segmentName,
                                         tsData=tsData, 
                                         # artifactVector=artifactVector,
                                         onsetRow=onsetRow, 
                                         yChangeOnset=yChangeOnset, 
                                         yChangeOnsetValue=yChangeOnsetValue,
                                         yChangePeak=yChangePeak,
                                         yChangePeakValue=yChangePeakValue,
                                         yChangeValue=yChangeValue,
                                         EDAPrestim=EDAPrestim )
    
    # artifactResult is a vector of 0s and "Artifact"
    
    artifactRowsE <- NULL
    artifactRowsE2 <- NULL
    
    # these artifacts are not shared with other sensors
    artifactRowsE <- which(artifactResult != "0")
    
    # these artifacts are shared with other senosrs
    artifactRowsE2 <- which(artifactResult == "Artifact2")
    
    # only some EDA artifacts are submitted to the artifacts vector
    segmentDF$Artifacts_a[artifactRowsE2] <- "Artifact"
    
    # limit the artifacts to the EDA response
    responseRows <- c(c(yChangeOnset - (EDAPrestim*cps)):c(yChangePeak))
    artifactRows <- artifactRowsE[which(artifactRowsE %in% responseRows)]
    
    # if(all(integrateArtifacts)) {
      if(sensorName == "AutoEDA") {
        segmentDF$AutoEDA_a[artifactRows] <- "Artifact"
      }
      if(sensorName == "ManualEDA") {
        segmentDF$ManualEDA_a[artifactRows] <- "Artifact"
      }
    # }
    
    #############################################################
    
    #### Oct 15, 2023 integrate artifacts from other sensors ####
    
    if(all(integrateArtifacts)) {
      
      # re-acquire artifacts from teh Artifacts_1 column
      # so that artifacts from other sensors are used here
      
      allArtifacts <- which(segmentDF$Artifacts_a != "0")
      
      allArtifacts <- allArtifacts[allArtifacts %in% responseRows]
      
      artifactRows <- sort(unique(c(artifactRows, allArtifacts)))
      
    }
    
    if(length(artifactRows) > 0) {
      # remove the extracted response information if the EDA data are not tonic
      yChangeOnset <- NA
      yChangeOnsetValue <- NA
      yChangePeak <- NA
      yChangePeakValue <- NA
      yChangeValue <- NA
    }
    
    # no change is made when no tonic instability is identified
    
  } # end EDA artifact section
  
  #### August 24, 2023 - need to make sure this exists, for output ####
  
  {
    
    if(!exists("prestimRange")) prestimRange <- NA
    
    # Aug 29 2023 for manual EDA there may be not artifactResult
    if(!exists("artifactResult")) artifactResult <- rep(0, times=length(tsData))
    
  }
  
  #################################
  
  #### output ####
  
  {
    # construct the output vector
    output <- list(yChangeOnset, 
                   yChangePeak, 
                   yChangeOnsetValue, 
                   yChangePeakValue, 
                   yChangeValue,
                   NULL, # not used with simplified amplitude extraction
                   NULL, # but the EDAExtract and CardioExtract 
                   NULL, # functions will look for these items
                   NULL, # so include them for now
                   NULL,
                   prestimRange,
                   segmentTitle, 
                   artifactResult,
                   segmentDF )
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
                       "prestimRange",
                       "segmentTitle", 
                       "artifactVector",
                       "segmentDF" )
  }
  
  return(output)
  
} # end amplitudeExtractFn() function 



