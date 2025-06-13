


source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)



RCScoresFn <- function(seriesMeasurementDF=RqCqDFSeries,
                       useMean=FALSE,
                       makeDF=FALSE,
                       saveCSV=FALSE,
                       output=FALSE,
                       assignOutputList=FALSE,
                       analysisListName="analysisResultList" ) {
  # R function to compute logged R/C scores from the measurements data frame
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  #
  # input is a data frame of measurements 
  # for all stimulus questions in all charts for a series
  # use the seriesMeasurementDF not the RqCqSeriesDF
  # because we need to have all questions for the CQ selection
  # the CQ selector should not select over a Sy question or artifact
  #
  # useMean input parameter will force the CQmean for all R/C ratios
  # instead of the strong side CQ
  #
  # output=FALSE will supress the output list
  # leaving the
  #
  # visible output is the series measurement data frame 
  # including all RQs CQs other Qs and annotations
  # with the "RCScore" column populated with the R/C ratios for all RQs
  # and the CQName column populated with the selected CQ for each RQ
  # 
  # source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
  # to load the selectCQFn function
  # based on the heuristic in Nelson (2017) 
  # "Heuristic Principles to Select Comparison and 
  # Relevant Question Pairs When Scoring Any CQT Format"
  #
  ##################
  
  {
    
    print("calculate the R/C ratios")
    
    if(!exists("useMean")) useMean <- FALSE
    if(!exists("makeDF")) makeDF <- FALSE
    if(!exists("saveCSV")) saveCSV <- FALSE
    if(!exists("output")) output <- FALSE
    if(!exists("assignOutputList")) assignOutputList <- FALSE
    if(!exists("analysisListName")) analysisListName <- FALSE
    
    #### use the seriesMeasurementDF instead of the RqCqDFSeries ####
    # so that all events can be included in selection of the CQ
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    assign("seriesMeasurementDF", seriesMeasurementDF, envir=.GlobalEnv)
    
    # reset the RCScore column
    seriesMeasurementDF$RCScore <- ""
    seriesMeasurementDF$CQName <- ""
    # View(seriesMeasurementDF)
    
    # if(seriesName == "4"){
    #   assign("seriesMeasurementDF", seriesMeasurementDF, envir=.GlobalEnv)
    #   stop()
    # }
    
    # stop()
    
  }

  #### remove repeated questions from the analysis ####
  
  {
    
    # stop()
    
    allEvents <- paste0(seriesMeasurementDF$chartName, seriesMeasurementDF$eventLabel)
    allEvents <- toupper((allEvents))
    
    # repeated events
    dupEvntsA <- which(str_sub(allEvents,  -1, -1) == "A")
    # seriesMeasurementDF$eventLabel[dupEvntsA]
    
    if(length(dupEvntsA) > 0) {
      # events that are repeated
      dupEvents <-
        which(allEvents %in%  str_sub(allEvents[dupEvntsA], 1, -2))
      
      renameRows <- sort(c(dupEvents, dupEvntsA))
      # seriesMeasurementDF$eventLabel[renameRows]
      
      # save the original event names
      # so they can be restored later in this function
      saveLabels <-  seriesMeasurementDF$Label[renameRows]
      saveELabels <- seriesMeasurementDF$eventLabel[renameRows]
      # saveEventLabels is already in use later
      
      tempLabels <- rep(paste0("Q", c(1:length(renameRows))))
      
      seriesMeasurementDF$Label[renameRows] <- tempLabels
      seriesMeasurementDF$eventLabel[renameRows] <- tempLabels
      
    }
    
    # View(seriesMeasurementDF)
    
    # need to restore the Label and eventLabel columns later
    # seriesMeasurementDF$Label[renameRows] <- saveLabels
    # seriesMeasurementDF$eventLabel[renameRows] <- saveELabels
    
  }
    
  #### get the questions and sensors ####
    
  {
    
    rqRowsSeries <- grep("R", seriesMeasurementDF$eventLabel)
    cqRowsSeries <- grep("C", seriesMeasurementDF$eventLabel)
    
    seriesRQs <- unique(seriesMeasurementDF$eventLabel[rqRowsSeries])
    seriesCQs <- unique(seriesMeasurementDF$eventLabel[cqRowsSeries])
    
    # increment the series if there not are at least 2 RQs and at least 2 CQs
    if( length(seriesRQs) < 2 || length(seriesCQs) < 2 ) {
      return(seriesMeasurementDF)
    }
    
    # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(seriesMeasurementDF$sensorName))
    
    RCSensors <- c("UPneumo", 
                   "LPneumo", 
                   # "Pneumo",
                   "AutoEDA", 
                   # "ManualEDA", 
                   "Cardio", 
                   # "FC",
                   "PLE")
    
    # increment the chart if ESS-M sensors are missing
    if(length(which(!(RCSensors %in% uniqueSensors))) > 2) {
      # the PLE sensor is optional, so there may be 1 missing sensor
      # FC sensor is for study and comparison with the traditional cardio sensor
      return(seriesMeasurementDF)
    }
    
    RCSensors <- RCSensors[RCSensors %in% uniqueSensors]
    
    uniqueCharts <- unique(seriesMeasurementDF$chartName)
    
    uniqueQuestionsSeries <- unique(seriesMeasurementDF$eventLabel)
    
  }
  
  ##### check for DLST DLDT and PCASS type charts #####
  
  {
    
    saveQuestionLabels <- RqCqDFSeries$Label
    saveEventLabels <- RqCqDFSeries$eventLabel
    
    saveChartNames <- rep(uniqueCharts[1], length=nrow(RqCqDFSeries))
    # RqCqDFSeries$chartName <- saveUniqueCharts[1]
    
    saveUniqueCharts <- uniqueCharts[1]
    
    # in case these are needed for replacement with DLST/DLDT formats
    saveUniqueQuestions <- unique(saveEventLabels)
    saveUniqueRQs <- 
      unique(saveUniqueQuestions[grep("R", saveUniqueQuestions)])
    saveUniqueCQs <- 
      unique(saveUniqueQuestions[grep("C", saveUniqueQuestions)])
    
    # DLST/DLDT/PCASS RQ labels 
    R1Labels <- c("1R1", "2R1", "3R1", "4R1")
    R2Labels <- c("1R2", "2R2", "3R2", "4R2")
    R3Labels <- c("1R3", "2R3", "3R3", "4R3")
    R4Labels <- c("1R4", "2R4", "3R4", "4R4")
    R5Labels <- c("1R5", "2R5", "3R5", "4R5")
    R6Labels <- c("1R6", "2R6", "3R6", "4R6")
    
    RQLabels <- c(R1Labels, R2Labels, R3Labels, R4Labels, R5Labels, R6Labels)
    
    # DLST/DLDT/PCASS CQ Labels
    C1Labels <- c("1C1", "2C1", "3C1", "4C1")
    C2Labels <- c("1C2", "2C2", "3C2", "4C2")
    C3Labels <- c("1C3", "2C3", "3C3", "4C3")
    C4Labels <- c("1C4", "2C4", "3C4", "4C4")
    C5Labels <- c("1C5", "2C5", "3C5", "4C5")
    C6Labels <- c("1C6", "2C6", "3C6", "4C6")
    
    CQLabels <- c(C1Labels, C2Labels, C3Labels, C4Labels, C5Labels, C6Labels)
    
    CQRQLabels <- c(CQLabels, RQLabels)
    
    if( length(which(saveEventLabels %in% CQRQLabels)) == 
        length(saveEventLabels) ) {
      DLSTType <- TRUE
    } else {
      DLSTType <- FALSE
    }
    
  }
  
  #### iterate over the charts in this series ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    #### isolate the chart 
    
    {
      # chartRows <- which(RqCqDFSeries$chartName==uniqueCharts[i])
      chartRows <- which(seriesMeasurementDF$chartName==uniqueCharts[i])
      
      # initiate the chart measurement data frame for the selectCQFn
      chartMeasurementDF <- seriesMeasurementDF[chartRows,]
      # View(chartMeasurementDF)
    }
    
    #### isolate the RQs and CQs
    
    {
      # initialize a vector of unique RQ and CQ events in the chart
      uniqueQuestionsChart <- unique(chartMeasurementDF$eventLabel)
      
      # increment the chart if less than 4 events in the chart
      if(length(uniqueQuestionsChart) < 4) {
        next()
      }
      
      rqRows <- grep("R", chartMeasurementDF$eventLabel)
      cqRows <- grep("C", chartMeasurementDF$eventLabel)
      
      rqDF <- chartMeasurementDF[rqRows,]
      cqDF <- chartMeasurementDF[cqRows,]
      # View(rqDF)
      # View(cqDF)
      
      uniqueRQs <- unique(rqDF$eventLabel)
      uniqueCQs <- unique(cqDF$eventLabel)
      
      # increment the chart if there not are at least 2 RQs and at least 2 CQs
      if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
        next()
      }
      
      # assign("rqDF", rqDF, pos=1)
      # assign("cqDF", cqDF, pos=1)
    }
    
    #### calculate the mean CQ for each sensor in the chart
    
    {
      # for use when each RQ is compared to the within chart mean CQ
      # initialize a vector to hold the CQ means
      cqMeans <- rep("", times=length(RCSensors))
      # then iterate over the sensors to compute the means
      for (m in 1:length(RCSensors)) {
        # may need to manage NA values in case there is no EDA measurement
        # may also need to manage small values
        # get the CQRow
        getRows <- which(cqDF$sensorName==RCSensors[m])
        # may need to remove small  CQ measurements here
        cqMeans[m] <- mean(cqDF$sensorMeasurement[getRows], na.rm=TRUE)
      }
      # coerce to numeric
      cqMeans <- as.numeric(cqMeans)
      names(cqMeans) <- RCSensors
      # print(cqMeans)
    }
    
    #### iterate on RQs and sensors to calculate the logged RQ/CQ ratio ####
    
    # requires the selectCQ function
    # source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
    
    j=2
    for (j in 1:length(uniqueRQs)) {
      
      # locate the rows for the RQ in the rqDF
      thisRQName <- uniqueRQs[j]
      selectRQRows <- rqDF$eventLabel==thisRQName
      # View(rqDF)
      
      # iterate on the sensors
      k=5
      for (k in 1:length(RCSensors)) {
        
        {
          
          ## get the RQ value
          
          # locate the rows for the sensor in the rqDF
          thisSensorName <- RCSensors[k]
          selectSensorRows <- rqDF$sensorName==thisSensorName
          
          # get the row index for the RQ and sensor
          theseRows <- which(selectRQRows & selectSensorRows)
          
          # stop if wrong sensor
          if(rqDF$sensorName[theseRows] != thisSensorName) { stop() }
          
          # initialize a scalar to hold the RQ Name
          RQName <- unique(rqDF$eventLabel[theseRows])
          
          # stop if the RQ name is incorrect
          if(thisRQName != RQName) { stop() }
          
          # initialize a scalar to hold the RQ Values
          RQValue <- rqDF$sensorMeasurement[theseRows]
          names(RQValue) <- thisSensorName
          
          # Aug 1, 2020 commented out
          # score comes from the CQ when no RQ response
          # if( is.na(RQValue) || as.numeric(RQValue)==0 ) {
          # need to keep working on CQs when RQValue==0
          # if( is.na(RQValue) ) {
          #   RQScore <- NA
          #   # rqDF$RCScore[theseRows] <- RQScore
          #   # next() # next k sensor
          # }
          
        }
        
        {
          
          ## get the CQ value
          
          # make an input list for the selectCQFn
          CQSelectInputList <- list(thisRQName=thisRQName,
                                    chartMeasurementDF=chartMeasurementDF,
                                    thisSensorName=thisSensorName )
          
          # assign("CQSelectInputList", CQSelectInputList, pos=1)
          
          ###### call the selectCQFn function to select the CQ ######
          
          CQSelectOutputList <- selectCQFn(CQSelectInputList=CQSelectInputList)
          
          CQName <- CQSelectOutputList$CQName
          CQValue <- CQSelectOutputList$CQValue
          
          # Aug 1, 2020 commented out
          # if( is.na(CQValue) || as.numeric(CQValue)==0 ) {
          # if( is.na(CQValue) ) {
          #   RQScore <- RQValue
          #   # rqDF$RCScore[theseRows] <- RQScore
          #   # next() # next k sensor
          # }
          
        }
        
        #### or use the CQ Means, contingent upon an input parameter ####
        
        if(!exists("useMean")) useMean <- FALSE
        if(isTRUE(useMean)) {
          CQValue <- cqMeans[k]
          CQName <- "mean"
          
          # manage the possible condition where the CQmean==0
          # commented out Aug 1, 2020
          # if(is.na(CQValue) || cqMeans[k] == 0) {
          #   RQScore <- NA
          #   rqDF$RCScore[rqDF$sensorName==thisSensorName] <- NA
          #   next() # next k sensor
          # }
        }
        
        #### check for small response measurements ####
        
        # 2019-0324
        # PLE respoonses can be small
        # also PLE feature extraction has its own constraints
        # pneumo measurements vals tend to be larger 
        # and are not affected by this
        if(thisSensorName != "PLE") {
          
          # increment if the sum of RQ and CQ is small
          # commented out Aug 1, 2020
          # if(sum(c(RQValue, CQValue), na.rm=TRUE) <= 30) {
          #   RQScore <- 0
          #   rqDF$RCScore[theseRows] <- RQScore
          #   next()
          # }
          
          # increment if both RQ and CQ are < 1% of the y axis
          if( as.numeric(RQValue) <= 20 && as.numeric(CQValue) <= 20 ) {
            # this will most likely do the same thing as the previous
            # but there may be some conditions where it helps
            RQScore <- 0
            rqDF$RCScore[theseRows] <- RQScore
            next() # next k sensor
          }
        }
        
        #### calculate the R/C ratio ####
        
        # check the PLE logged RQ and CQ values for negatives
        # commented out Aug 1, 2020
        # if(thisSensorName == "PLE") { 
        #   if(!is.na(RQValue) && RQValue < 0) {
        #     RQValue <- NA
        #   }
        #   if(!is.na(CQValue) && CQValue < 0) {
        #     CQValue <- NA
        #   } 
        # }
        
        if(thisSensorName == "PLE") {
          # PLE constraint at 10% difference
          # ignore RQ or RQ log(pre/post) ratios with insufficient difference
          # PLE constraint for pre/post ratios
          PLEConstraint <- log(1.051271) # 0.04999991
          # an additional constraint exists for ESS-M  R/C scores
          
          if(RQValue < PLEConstraint) RQValue <- 0
          if(CQValue < PLEConstraint) CQValue <- 0
          
          if(is.na(RQValue)) RQValue <- 0
          if(is.na(CQValue)) CQValue <- 0
        }
        
        # use the sensorName to handle the PLE ratio different than others
        RQScore <- ifelse(thisSensorName == "PLE",
                          # for PLE scores 
                          # because the R and C are the log(pre/post) ratio
                          # so log(R/C) ratiod are handled differently 
                          # than for other sensors
                          # for which the R and C are measurement or extraction vals
                          ifelse(is.na(RQValue) || RQValue<=PLEConstraint,
                                 # use the extant log RC ratio 
                                 # if either of the the RQ or CQ PLE score is missing
                                 # neeed to check for negative CQ value
                                 ifelse(is.na(CQValue) || CQValue<0,
                                        0,
                                        ifelse(is.na(CQValue) || CQValue<=PLEConstraint,
                                               0,
                                               # invert the sign of the logged CQ Value
                                               # so - scores are associated with deception
                                               -CQValue )
                                 ),
                                 ifelse(is.na(CQValue) || CQValue<=PLEConstraint,
                                        # no need to check for negative RQ Value here
                                        # because neg logRC ratios for RQs will be < PLE constraint
                                        ifelse(is.na(RQValue) || RQValue<=PLEConstraint,
                                               0,
                                               # don't invert the sign of the logged RQ Value
                                               RQValue ),
                                        # use this formula 
                                        # if logRCs are usable for both RQ and CQ
                                        log( RQValue^.5 / CQValue^.5 )
                                        # sign value for PLE is inverted in the next step
                                        # same result using this formula
                                        # log(((RQValue)^2/(CQValue)^2)^.25)
                                        # log(.08^.5 / .02^.5)
                                        # log(.02^.5 / .08^.5)
                                        # log(((.08)^2/(.02)^2)^.25)
                                        # log(((.02)^2/(.08)^2)^.25)
                                 )
                          ),
                          # for all sensors other than the PLE
                          # use the logged ratio of RQ and CQ measurements
                          log(RQValue/CQValue) 
                          # sign values are inverted 
                          # for EDA and Cardio in the next step
        )
        
        # save the RQ score
        rqDF$RCScore[theseRows] <- RQScore
        # save the CQ name 
        rqDF$CQName[theseRows] <- CQName
        # View(rqDF)
        
        # assign("rqDF", rqDF, envir=.GlobalEnv)
        # if(k == 1 && j == 3) stop()
        
      } # end for loop for k unique sensors
      
    } # end for loop for j unique RQs
    
    #### invert the logged EDA, Cardio, and PLE scores ####
    
    {
      # so that + log scores correspond to truth-telling
      # and - log scores correspond to deception
      
      EDARows <- which(rqDF$sensorName=="AutoEDA" | 
                         rqDF$sensorName=="ManualEDA")
      rqDF$RCScore[EDARows] <- -as.numeric(rqDF$RCScore[EDARows])
      
      CardioRows <- which(rqDF$sensorName=="Cardio")
      rqDF$RCScore[CardioRows] <- -as.numeric(rqDF$RCScore[CardioRows])
      
      PLERows <- which(rqDF$sensorName=="PLE")
      rqDF$RCScore[PLERows] <- -as.numeric(rqDF$RCScore[PLERows])
    }
    
    #### pass the rqDF back to the chart measurement DF ####
    
    # pass the rqDF back to the chartMeasurementDF
    chartMeasurementDF[rqRows,] <- rqDF
    
    # assign("rqDF", rqDF, envir=.GlobalEnv)
    # stop()
    
    # pass the chartmeasurementDF back to the seriesMeasurementDF
    seriesMeasurementDF[chartRows,] <- chartMeasurementDF
    
  } # end loop i over uniqueCharts
  
  #### restore the labels with repeated questions ####
  
  {
    seriesMeasurementDF$Label[renameRows] <- saveLabels
    seriesMeasurementDF$eventLabel[renameRows] <- saveELabels
    # not saveEventLabels
    # View(seriesMeasurementDF)
  }
  
  ################# output section  ####################
  
  if(isTRUE(output)) {
    
    ##### initialize a data frame with the question sequence for each chart ####
    
    questionSequenceDF <- questionSequenceFn(measurementDF=RqCqDFSeries,
                                             outputName="RCQuestionSequence",
                                             makeDF=FALSE,
                                             saveCSV=FALSE)
    
    ##### initialize the RqCqDFSeries #####
    
    {
      RqCqSeriesRows <- grep("[CR]+", seriesMeasurementDF$eventLabel)
      if(length(RqCqSeriesRows) != 0) {
        # exclude sacrifice relevant questions
        SRRows <- grep("SR", seriesMeasurementDF$eventLabel)
        SRRows <- c(SRRows, grep("RS", seriesMeasurementDF$eventLabel))
        # make a vector of working rows for CQs and RQs
        RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% SRRows)]
        # exclude "CT" (cleared throat) annotations
        CTRows <- grep("CT", seriesMeasurementDF$eventLabel)
        RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% CTRows)]
      }    
      RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
    }
    
    # RC scores are logged ratios
    # for RQs only 
    # no aggregation
    # no decision rules
    
    RCSensors2 <- c("UPneumo", 
                    "LPneumo", 
                    "AutoEDA", 
                    "Cardio", 
                    "PLE")
    
    RCSensors2 <- 
      RCSensors2[RCSensors2 %in% seriesMeasurementDF$sensorName]
    
    RCScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                   useSensors=RCSensors2,
                                   scoreType="RCScore",
                                   decimals=2,
                                   DLSTType=DLSTType,
                                   outputName="RCScoresheetDF",
                                   makeDF=FALSE,
                                   saveCSV=FALSE)
    # View(scoreSheetDF)
    
    CQSelectionDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                  useSensors=RCSensors2,
                                  scoreType="CQName",
                                  decimals="non-numeric",
                                  outputName="CQSelectionDF",
                                  makeDF=FALSE,
                                  saveCSV=FALSE)
    
    measurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=RCSensors2,
                         decimals=2,
                         makeDF=FALSE,
                         saveCSV=FALSE )
    
    ## construct a list to hold the rank score results ####
    
    outputListName <- paste(examName, seriesName, "RCOutputList", sep="_")
    
    RCOutputList <- list(RQCQRatios="R/C Ratios -  used by ESS/ESS-M, (Nelson, Krapohl & Handler 2008; Nelson 2017)",
                         examName=examName,
                         seriesName=seriesName,
                         RCQuestions=uniqueQuestionsSeries,
                         RCSensors=RCSensors2,
                         RCQuestionSequence=questionSequenceDF,
                         RCQSelectionTable=CQSelectionDF,
                         RCScoreSheetDF=RCScoreSheetDF,
                         RCMeasurementsDF=measurementsDF )
    
  }
  
  #### save the list to the global env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, RCOutputList, env=.GlobalEnv)
  
  if(!exists("assignOutputList")) assignOutputList <- FALSE
  
  if(isTRUE(assignOutputList)) {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "RCRatioOutput"
    analysisResultList[[seriesListName]][[outputListName]] <-
      RCOutputList
    if(assignOutputList) {
      assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    }

  }
  
  #### save it ####
  
  outputDFNameRCScores <- paste(examName, seriesName, "RCScoreDF", sep="_")
  
  outputDFNameCQSelection <- paste(examName, seriesName, "CQSelectionDF", sep="_")
  
  if(isTRUE(makeDF)) {
    assign(outputDFNameRCScores, RCScoreSheetDF, pos=1)
    assign(outputDFNameCQSelection, CQSelectionDF, pos=1)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(RCScoreSheetDF,
              file=paste0(str_sub(outputDFNameRCScores, 1, -3), ".csv"),
              row.names=FALSE)
    write.csv(CQSelectionDF,
              file=paste0(str_sub(outputDFNameCQSelection, 1, -3), ".csv"),
              row.names=FALSE)
  }

  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(seriesMeasurementDF)
  
  # end RCScoresFn
} 



