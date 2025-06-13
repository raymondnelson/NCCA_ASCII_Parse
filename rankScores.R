


rankScoreFn <- function(RqCqDFSeries=RqCqDFSeries, 
                        makeDF=TRUE,
                        saveCSV=TRUE,
                        analysisListName="analysisResultList" ) { 
  # R function to compute the rank order scores 
  # for each chart in a series 
  # Raymond Nelson
  # 12-5-2017
  # 2-14-2019
  # 
  # called by the newScoresFn() in the newScores.R script
  # 
  ###
  # 
  # x input is a data frame of measurements for the RQs and CQs for a test chart
  # output is the RqCqDF data frame with the rankScore column populated 
  # with rank order scores
  # 
  ##################
  
  {
    # makeDF <- FALSE
    # saveCSV <- FALSE
  }
  
  {
    
    print("calculate the rank scores for all charts in the series")
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # reset the rankScore column
    RqCqDFSeries$rankScore <- ""
    # View(RqCqDFSeries)
    
    # assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    # stop()
    
    # if(seriesName == "4") {
    #   assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    #   stop()
    # }
    
  }
  
  #### identify the sensors and questions for the analysis ####
  
  {
    
    # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    # uniqueSensors <- unique(names(measurementsVc))
    
    rankSensors <- c("UPneumo", 
                     "LPneumo", 
                     "Pneumo",
                     "AutoEDA", 
                     "ManualEDA", 
                     "Cardio", 
                     # "FC",
                     "PLE")
    
    # remove rank sensors not in the data 
    rankSensors <- rankSensors[rankSensors %in% uniqueSensors]
    
    # exclude the PLE using a setting and if missing
    if("PLE" %in% rankSensors && !isTRUE(includePLEScores)) {
      rankSensors <- rankSensors[-which(rankSensors %in% "PLE")]
    }
    
    # initialize a vector of unique RQ and CQ events
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) < 4) return(RqCqDFSeries) 
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
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
  
  #### iterate over the charts ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
  
    {
      
      chartRows <- which(RqCqDFSeries$chartName == uniqueCharts[i])
      
      # initialize the chart data frame
      RqCqDFChart <- RqCqDFSeries[chartRows,]
      # View(RqCqDFChart)
      
      chartName <- RqCqDFChart$chartName[1]
      
      # next chaart if less than 2 sensors
      rankSensorRows <- which(RqCqDFChart$sensorName %in% rankSensors)
      if(length(rankSensorRows) < 2) next()
        
      # next chart if less than 4 events
      uniqueEventsChart <- unique(RqCqDFChart$eventLabel)
      if(length(uniqueEventsChart) < 4) next()
      
    }
  
    # iterate over all sensor names and calculate the rank scores
    j=1
    for (j in 1:length(rankSensors)) {
      
      {
        
        sensorName <- rankSensors[j]
        
        sensorRows <- which(RqCqDFChart$sensorName==sensorName)
        
        # make a vector of measurements for the sensor
        measurementVector <- RqCqDFChart[sensorRows,'sensorMeasurement']
        
        # next sensor if there are no measurements
        if(length(which(!is.na(measurementVector))) == 0) {
          next()
        }
        
        # zero as missing
        measurementVector[measurementVector == 1] <- NA
        
      }
      
      #### rank the measurements in reverse order ####
      
      if(rankSensors[j] %in% c("UPneumo", "LPneumo", "Pneumo")) {
        
        #### smaller respiration excursion indicates greater response ####
        
        # do not invert pneumo
        # so smallest measurement is the greatest change in physiology
        # greatest change in physiology has rank 1
        
        # oct 1, 2021 invert the ranks of pneumos with new feature extraction
        # greatest change in physiology has rank 1

        rankMeasurements <- rank(measurementVector, 
                                 ties.method="average", 
                                 # use "keep" to assign rank NA
                                 # na.last="keep" )
                                 # use TRUE Mar 17, 2019
                                 na.last="keep" )
        
        RqCqDFChart$rankScore[sensorRows] <- rankMeasurements
        
      } else {
        
        # for EDA Cardio and PLE
        
        # larger values indicate greater changes in physiology
        
        # invert so largest measurement has rank 1
        
        rankMeasurements <- rank(-measurementVector, 
                                 ties.method="average", 
                                 # use "keep" to assign rank NA
                                 # na.last="keep")
                                 # use TRUE Mar 17, 2019
                                 na.last="keep" )
        
        RqCqDFChart$rankScore[sensorRows] <- rankMeasurements
      
      }
      
    } # end loop j over sensors
      
    #### average the rank scores for 2 pneumo sensors ####
    
    {
      
      pneumoRows <- which(RqCqDFChart$sensorName == "Pneumo")
      UpPneumoRows <- which(RqCqDFChart$sensorName == "UPneumo")
      LpPneumoRows <- which(RqCqDFChart$sensorName == "LPneumo")
      
      UPRanks <- as.numeric(RqCqDFChart$rankScore[UpPneumoRows])
      LPRanks <- as.numeric(RqCqDFChart$rankScore[LpPneumoRows])
      
      for(k in 1:length(pneumoRows)) {
        RqCqDFChart$rankScore[pneumoRows][k] <- 
          mean(c(UPRanks[k], LPRanks[k]), na.rm=TRUE) 
      }
      # View(RqCqDFChart)
      
      # re-rank the rank pneumo scores
      RqCqDFChart$rankScore[pneumoRows] <- rank(as.numeric(RqCqDFChart$rankScore[pneumoRows]), 
                                                ties.method="average", 
                                                # use "keep" to assign rank NA
                                                # na.last="keep")
                                                # use TRUE Mar 17, 2019
                                                na.last="keep" )
       
    }
    
    #### pass the chart data frame back to the series data frame ####
    
    RqCqDFSeries[chartRows,] <- RqCqDFChart
    # View(RqCqDFSeries)
    
  } # end loop i over charts
  
  ################ rank scores output section #################
  
  #### initialize a data frame of rank scores
  
  {
    
    {
      # initialize a data frame for the rank 
      
      rankScoresDF <- data.frame(matrix(ncol=(length(uniqueQuestions)), 
                                        nrow=length(uniqueCharts)*length(rankSensors)))
      names(rankScoresDF) <- uniqueQuestions
      rankScoresDF <- cbind(sensorName=rep(rankSensors, times=length(uniqueCharts)), 
                            rankScoresDF)
      rankScoresDF$sensorName <- as.character(rankScoresDF$sensorName)
      rankScoresDF <- cbind(chartName=rep(uniqueCharts, each=length(rankSensors)), 
                            rankScoresDF)
      rankScoresDF$chartName <- as.character(rankScoresDF$chartName)
      # View(rankScoresDF)
    }
    
    # populate the data frame with the measurements
    # iterate over the rows in the rankScoresDF
    i=1
    for(i in 1:nrow(rankScoresDF)) {
      thisChart <- rankScoresDF[i,1]
      thisSensor <- rankScoresDF[i,2]
      # iterate over the question columns in the rankScoresDF
      j=3
      for(j in 3:ncol(rankScoresDF)) {
        thisQuestion <- names(rankScoresDF)[j]
        # now get the measurement
        thisOne <- which(RqCqDFSeries$chartName==thisChart &
                           RqCqDFSeries$sensorName==thisSensor &
                           RqCqDFSeries$eventLabel==thisQuestion)
        if(length(thisOne) == 0 ) next()
        rankScoresDF[i,j] <- RqCqDFSeries$rankScore[thisOne]
      }
    }
    
    # View(rankScoresDF)
    # View(RqCqDFSeries)
    
  }
  
  #### save it ####
  
  {
    
    outputDFName <- paste(examName, seriesName, "rankScoreDF", sep="_")
    
    if(isTRUE(makeDF)) {
      assign(outputDFName, rankScoresDF, pos=1)
    }
    
    if(isTRUE(saveCSV)) {
      write.csv(rankScoresDF,
                file=paste0(str_sub(outputDFName, 1, -3), ".csv"),
                row.names=FALSE)
    }
    
  }
  
  ################# output section ################
  
  {
    
    ## calculate the measurement table used for rank scores
    
    rankMeasurementSensors <- c("UPneumo", 
                                "LPneumo", 
                                "AutoEDA", 
                                "Cardio", 
                                "PLE")
    
    rankMeasurementSensors <- 
      rankMeasurementSensors[rankMeasurementSensors %in% RqCqDFSeries$sensorName]
    
    # exclude the PLE
    if("PLE" %in% rankMeasurementSensors && !isTRUE(includePLEScores)) {
      rankMeasurementSensors <- rankMeasurementSensors[-which(rankMeasurementSensors %in% "PLE")]
    }
    
    measurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=rankMeasurementSensors,
                         decimals=2,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    measurementsDF[,3:ncol(measurementsDF)] <- 
      round(measurementsDF[,5:ncol(measurementsDF)], 3)
    # View(measurementsDF)
    
    ## calculate the summed rank scores for all questions in the series 
    
    rankSensors2 <- c("Pneumo",
                      "AutoEDA", 
                      "Cardio", 
                      "PLE")
    rankSensors2 <- rankSensors2[rankSensors2 %in% RqCqDFSeries$sensorName]
    
    # exclude the PLE
    if("PLE" %in% rankSensors2 && !isTRUE(includePLEScores)) {
      rankSensors2 <- rankSensors2[-which(rankSensors2 %in% "PLE")]
    }
    
    # reduce the rankScoresDF to the sensors in use
    rankScoresDF <- rankScoresDF[rankScoresDF$sensorName %in% rankSensors2,]
    
    # set the columns to numeric
    for(k in 3:ncol(rankScoresDF)) {
      rankScoresDF[,k] <- as.numeric(rankScoresDF[,k])
    }
    
    # calculate the rank sums
    summedRankScores <- 
      round(colSums(rankScoresDF[,3:ncol(rankScoresDF)], na.rm=TRUE), 2)
    
    meanRankScores <- 
      round(colMeans(rankScoresDF[,3:ncol(rankScoresDF)], na.rm=TRUE), 2)
    
    ## get the question with the greatest summed rank score ####
    
    minRankSum <- summedRankScores[which.min(summedRankScores)]
    minRankSumQuestion  <- uniqueQuestions[which.min(summedRankScores)]
    
    # rank the summed ranks
    # do not invert, 
    # so that smaller sums indicate greater change in physiology
    rankSummedRanks <- rank(summedRankScores, 
                            ties.method="average",
                            na.last="keep")
    
    # mean is more robust with 
    minRankMean <- meanRankScores[which.min(meanRankScores)]
    minRankMeanQuestion <- uniqueQuestions[which.min(meanRankScores)]
    rankMeanRanks <- rank(meanRankScores, 
                          ties.method="average",
                          na.last="keep")
    
    ## construct a list to hold the rank score results ####
    
    outputListName <- paste(examName, seriesName, "RankOutputList", sep="_")
    
    RankOutputList <- list(rankScores="Rank Order Scores",
                           examName=examName,
                           seriesName=seriesName,
                           rankQuestions=uniqueQuestions,
                           # summedRankScores=summedRankScores,
                           # rankSummedRanks=rankSummedRanks,
                           # maxRankSumScore=maxRankSum,
                           # maxRankSumQuestion=maxRankSumQuestion,
                           meanRankScores=meanRankScores,
                           rankMeanRanks=rankMeanRanks,
                           maxRankMeanScore=minRankMean,
                           maxRankMeanQuestion=minRankMeanQuestion,
                           rankMeasurementSensors=rankMeasurementSensors,
                           rankScoresDF=rankScoresDF,
                           rankMeaasurementsDF=measurementsDF )
    
  }
  
  #### save the list to the global env as a side effect ####
  
  # use this to save the output list directly to the global env
  # assign(outputListName, RankOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "rankAnalysisOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      RankOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(RqCqDFSeries)
  
  # end rankScoreFn
} 
  
  
  
