




RRMScoreFn <- function(RqCqDFSeries=RqCqDFSeries, 
                       makeDF=FALSE,
                       saveCSV=FALSE,
                       analysisListName="analysisResultList" ) {
  # R function to compute the Relative Response Magnitudes from the measurements data frame
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  # x input is a data frame of measurements for the RQs and CQs for a test chart
  # output is the RqCqDF data frame with the RRM column populated 
  # 
  # RRM is (x-min)/(max-min)
  # largest response = 1
  # smallest response = 0
  ##################
  
  {
    
    print("calculate the RRM score for all charts in the series")
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # reset the rankScore column
    RqCqDFSeries$RRMScore <- ""
    # View(RqCqDFSeries)
    
  }
  
  {
    
    # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    RRMSensors <- c("UPneumo", 
                    "LPneumo", 
                    "Pneumo",
                    "AutoEDA", 
                    "ManualEDA", 
                    "Cardio", 
                    "FC",
                    "PLE")
    
    # remove RRM sensors not in the data 
    RRMSensors <- RRMSensors[RRMSensors %in% uniqueSensors]
    
    # exclude the PLE using a setting and if missing
    if("PLE" %in% RRMSensors && !isTRUE(includePLEScores)) {
      RRMSensors <- RRMSensors[-which(RRMSensors %in% "PLE")]
    }
    
    # initialize a vector of unique RQ and CQ events
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) < 4) { 
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
  }
  
  #### iterate over the charts to calculate the RRM scores ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    {
      
      chartRows <- which(RqCqDFSeries$chartName == uniqueCharts[i])
      
      # initialize the chart data frame
      RqCqDFChart <- RqCqDFSeries[chartRows,]
      
      uniqueEventsChart <- unique(RqCqDFChart$eventLabel)
      
      if(length(uniqueEventsChart) < 4) next()
      
    }
    
    # View(RqCqDFChart)
    
    {
      
      # set 0 measurement values as missing
      
      saveTheseMeasurements <- RqCqDFChart$sensorMeasurement
      
      theseSensorRows <- which(RqCqDFChart$sensorName %in% RRMSensors)
      zeroValues <- 
        which(RqCqDFChart$sensorMeasurement[theseSensorRows] == 0)
      RqCqDFChart$sensorMeasurement[theseSensorRows][zeroValues] <- NA
      
    }
    
    # iterate over the sensor names to calculate the RRM score
    j=1
    for (j in 1:length(RRMSensors)) {
      
      {
        
        thisSensor <- RRMSensors[j]
        
        # get the rows for the sensor
        sensorRows <- which(RqCqDFChart$sensorName==thisSensor)
        
        # initialize a vector of measurements for the sensor
        measurementVector <- RqCqDFChart[sensorRows,'sensorMeasurement']
        
        # oct 1, 2021
        # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
        #   measurementVector <- exp(measurementVector)
        #   # pneumo R/C ratios are now decimals < 1 
        #   # and smaller values are larger responses 
        # }
        
        # next j iteration if there are no measurements
        if(length(which(!is.na(measurementVector))) == 0) {
          next()
        }
        
        # zero as missing
        measurementVector[measurementVector == 0] <- NA
        
        # Oct 4, 2021
        # rescale the respiration measurements 
        # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
        #   measurementVector <- round(exp(-measurementVector) * 100, 3)
        #   # larger values now signify greater changes in physiology
        # }
        
        # get the max min and range for the sensor scores
        if( length(which(!is.na(measurementVector))) > 2 ) {
          maxResponse <- max(measurementVector, na.rm=TRUE) 
          minResponse <- min(measurementVector, na.rm=TRUE)
        } else {
          # submit the measurementVector to the RqCqDFChart
          RqCqDFChart$RRMScore[sensorRows] <- measurementVector
          next() # next j
        }
        
        # calculate the range of response magnitudes for this sensor
        responseRange <- maxResponse - minResponse
        
        # initialize a vector
        RRMVector <- rep(NA, times=length(measurementVector))
        
      } # end j loop over sensors
      
      # View(RqCqDFChart)
      
      # iterate over the stimulus events to calculate the RRM for each event
      k=1
      for(k in 1:length(measurementVector)) {
        thisResponse <- measurementVector[k]
        # next measurement if this one is NA or range is 0
        if(is.na(thisResponse)) next()
        if(responseRange == 0) { 
          RRMVector[k] <- 1
          next() 
        }
        # calculate the RRM
        thisRRM <- round((thisResponse - minResponse) / responseRange, 3)
        # add the RRM to the measurementVector
        RRMVector[k] <- thisRRM
      } # end iteration over k events for each sensor
      
      # submit the RRM to the RqCqDFChart
      RqCqDFChart$RRMScore[sensorRows] <- round(RRMVector, 3)
      
    } # end iteration over j uniqueSensors
    
    #### average the 2 pneumo sensors and recalculate the RRM ####
    
    {
      
      pneumoRows <- which(RqCqDFChart$sensorName == "Pneumo")
      UpPneumoRows <- which(RqCqDFChart$sensorName == "UPneumo")
      LpPneumoRows <- which(RqCqDFChart$sensorName == "LPneumo")
      
      RRMUp <- as.numeric(RqCqDFChart$RRMScore[UpPneumoRows])
      RRMLp <- as.numeric(RqCqDFChart$RRMScore[LpPneumoRows])
      
      for(k in 1:length(pneumoRows)) {
        RqCqDFChart$RRMScore[pneumoRows][k] <- 
          round(mean(c(RRMUp[k], RRMLp[k]), na.rm=TRUE), 3)
      }
      
    }
    
    # View(RqCqDFChart)
    
    #### recalculate RRM for combined pneumo values
    
    {
      
      PneumoVals <- as.numeric(RqCqDFChart$RRMScore[pneumoRows])
      maxPneumoResponse <- max(as.numeric(PneumoVals))
      minPneumoResponse <- min(as.numeric(PneumoVals))
      pneumoRange <- maxPneumoResponse  - minPneumoResponse
      
      # iterate over the stimulus events to recalculate
      
      l=5
      for(l in 1:length(PneumoVals)) {
        thisPneumo <- as.numeric(RqCqDFChart$RRMScore[pneumoRows][l])
        RqCqDFChart$RRMScore[pneumoRows][l] <-
          round((thisPneumo - minPneumoResponse) / pneumoRange, 3)
      }
      
    }
    
    #### invert the scores for pneumo and PLE sensors ####
    
    {
      
      # invert the pneumo and PLE scores
      # so that greater changes in physiology 
      # are associated with larger RRM values
      
      invertTheseSensors <- c("UPneumo", "LPneumo", "Pneumo")
      invertTheseRows <- 
        which(RqCqDFChart$sensorName %in% invertTheseSensors)
      theseRRMScores <- 
        as.numeric(RqCqDFChart$RRMScore[invertTheseRows])
      RqCqDFChart$RRMScore[invertTheseRows] <- 1 - theseRRMScores

    }
    
    #### restore the measurements ####
    
    RqCqDFChart$sensorMeasurement <- saveTheseMeasurements
    
    #### pass the chart data frame back to the series data frame ####
    
    RqCqDFSeries[chartRows,] <- RqCqDFChart
    # View(RqCqDFSeries)
    # View(RqCqDFChart)
    
  } # end loop i over charts
  
  #### adjust ceiling and floor values ####
  
  # {
  #   
  #   # set 1.0 to  .999 and .000 to .001
  #   
  #   fixThese <- which(as.numeric(RqCqDFChart$RRMScore) == 0)
  #   RqCqDFChart$RRMScore[fixThese] <- .001
  #   
  #   fixThese <- which(as.numeric(RqCqDFChart$RRMScore) == 1)
  #   RqCqDFChart$RRMScore[fixThese] <- .999
  #   
  # }
  # View(RqCqDFSeries)
  
  ################ output #################
  
  #### initialize and populate a data frame of RRM scores ####
  
  {
    
    # initialize a data frame for the RRM scores
    RRMScoresDF <- data.frame(matrix(ncol=(length(uniqueQuestions)), 
                                     nrow=length(uniqueCharts)*length(RRMSensors)))
    names(RRMScoresDF) <- uniqueQuestions
    RRMScoresDF <- cbind(sensorName=rep(RRMSensors, times=length(uniqueCharts)), 
                         RRMScoresDF)
    RRMScoresDF$sensorName <- as.character(RRMScoresDF$sensorName)
    RRMScoresDF <- cbind(chartName=rep(uniqueCharts, each=length(RRMSensors)), 
                         RRMScoresDF)
    RRMScoresDF$chartName <- as.character(RRMScoresDF$chartName)
    # View(RRMScoresDF)
    # str(RRMScoresDF)
    
    # populate the data frame with the measurements
    i=1
    for(i in 1:nrow(RRMScoresDF)) {
      thisChart <- RRMScoresDF[i,1]
      thisSensor <- RRMScoresDF[i,2]
      # iterate over the questions
      j=3
      for(j in 3:ncol(RRMScoresDF)) {
        thisQuestion <- names(RRMScoresDF)[j]
        # now get the measurement
        thisOne <- which(RqCqDFSeries$chartName==thisChart &
                           RqCqDFSeries$sensorName==thisSensor &
                           RqCqDFSeries$eventLabel==thisQuestion)
        if(length(thisOne) == 0 ) next()
        # get the RRM value 
        RRMScoresDF[i,j] <- 
          round(as.numeric(RqCqDFSeries$RRMScore[thisOne]), 2)
      }
    }
    # View(RRMScoresDF)
    # str(RRMScoresDF)
    
    # and fix again
    
    # set 1.0 to  .999 and .000 to .001
    # iterate over the colums
    # m=3
    # for(m in 3:ncol(RRMScoresDF)) {
    #   fixThese1 <- which(RRMScoresDF[,m] == 0)
    #   RRMScoresDF[fixThese1,m] <- .001
    #   fixThese2 <- which(RRMScoresDF[,m] == 1)
    #   RRMScoresDF[fixThese2,m] <- .999
    # }
    
  }
  # View(RRMScoresDF)
  # str(RRMScoresDF)
  
  #### save it to the global envir and current working directory ####
  
  {
    
    outputDFName <- paste(examName, seriesName, "RRMScoreDF", sep="_")
    
    if(isTRUE(makeDF)) {
      assign(outputDFName, RRMScoresDF, pos=1)
    }
    
    if(isTRUE(saveCSV)) {
      write.csv(RRMScoresDF,
                file=paste0(str_sub(outputDFName, 1, -3), ".csv"),
                row.names=FALSE)
    }
    
  }
  
  ################# output section ################
  
  {
    
    ## calculate the measurement table used for rank scores
    
    RRMMeasurementSensors <- c("UPneumo", 
                               "LPneumo", 
                               "AutoEDA", 
                               "Cardio", 
                               "PLE")
    
    RRMMeasurementSensors <- 
      RRMMeasurementSensors[RRMMeasurementSensors %in% 
                              RqCqDFSeries$sensorName]
    
    # exclude the PLE
    if("PLE" %in% RRMMeasurementSensors && !isTRUE(includePLEScores)) {
      RRMMeasurementSensors <- RRMMeasurementSensors[-which(RRMMeasurementSensors %in% "PLE")]
    }
    
    RRMMeasurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=RRMMeasurementSensors,
                         decimals=2,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    # View(RRMMeasurementsDF)
    # str(RRMMeasurementsDF)
    
    ## aggregate the RRM scores for all questions in the series 
    
    RRMSensors2 <- c("Pneumo",
                      "AutoEDA", 
                      "Cardio", 
                      "PLE")
    
    dfSensors <- unique(RRMScoresDF$sensorName)
    
    RRMSensors2 <- RRMSensors2[RRMSensors2 %in% dfSensors]

    # exclude the PLE
    if("PLE" %in% RRMSensors2 && !isTRUE(includePLEScores)) {
      RRMSensors2 <- RRMSensors2[-which(RRMSensors2 %in% "PLE")]
    }
    
    RRMScoresDF <- RRMScoresDF[RRMScoresDF$sensorName %in% RRMSensors2,]
    # View(RRMScoresDF)
    # str(RRMScoresDF)
    
    # fix the column type
    for(n in 3:ncol(RRMScoresDF)) {
      RRMScoresDF[,n] <- as.numeric(RRMScoresDF[,n])
    }
    
    # initialize a vector to hold the aggregated RRM
    summedRRMScores <- rep(NA, ncol(RRMScoresDF) - 2)
    meanRRMScores <- rep(NA, ncol(RRMScoresDF) - 2)
    
    names(summedRRMScores) <- colnames(RRMScoresDF)[3:ncol(RRMScoresDF)]
    names(meanRRMScores) <- colnames(RRMScoresDF)[3:ncol(RRMScoresDF)]
    
    # iterate over the RQ columns to aggregate
    for(n in 3:ncol(RRMScoresDF)) {
      summedRRMScores[(n-2)] <- 
        round(sum(as.numeric(RRMScoresDF[,n]), na.rm=TRUE), 3)
    }
    for(n in 3:ncol(RRMScoresDF)) {
      meanRRMScores[(n-2)] <- 
        round(mean(as.numeric(RRMScoresDF[,n]), na.rm=TRUE), 3)
    }
    
    # recalculate the RRM
    maxSummedRRM <- max(summedRRMScores)
    minSummedRRM <- min(summedRRMScores)
    summedRRMRange <- maxSummedRRM - minSummedRRM
    
    maxMeanRRM <- max(meanRRMScores)
    minMeanRRM <- min(meanRRMScores)
    meanRRMRange <- maxMeanRRM - minMeanRRM
    
    # iterate on the aggregated scores
    
    RRMSums <- rep(NA, length(summedRRMScores))
    names(RRMSums) <- names(summedRRMScores)
    for(o in  1:length(summedRRMScores)) {
      RRMSums[o] <- 
        round((summedRRMScores[o] - minSummedRRM) / summedRRMRange, 3)
    }
    
    RRMMeans <- rep(NA, length(meanRRMScores))
    names(RRMMeans) <- names(meanRRMScores)
    for(o in  1:length(meanRRMScores)) {
      RRMMeans[o] <- 
        round((meanRRMScores[o] - minMeanRRM) / meanRRMRange, 3)
    }
    
    # fix the ceiling and floor values
    
    RRMSums[which(RRMSums == 0)] <- .001
    RRMSums[which(RRMSums == 1)] <- .999
    
    RRMMeans[which(RRMMeans == 0)] <- .001
    RRMMeans[which(RRMMeans == 1)] <- .999
  
    #### get the question with the greatest summed rank score ####
    
    maxSummedRRM <- RRMSums[which.max(RRMSums)]
    maxSummedRRMQuestion <- uniqueQuestions[which.max(RRMSums)]
    
    maxMeanRRM <- meanRRMScores[which.max(meanRRMScores)]
    maxmeanRRMQuestion <- uniqueQuestions[which.max(meanRRMScores)]
    
    #### construct a list to hold the rank score results ####
    
    outputListName <- paste(examName, seriesName, "RRMOutputList", sep="_")
    
    
    
    RRMOutputList <- list(RRM="Relative Response Magnitude (Raskin, Kircher, Honts & Horowitz, 1988)",
                          examName=examName,
                          seriesName=seriesName,
                          RRMQuestions=uniqueQuestions,
                          summedRRMScores=summedRRMScores,
                          maxSummedRRM=maxSummedRRM,
                          maxSummedRRMQuestion=maxSummedRRMQuestion,
                          meanRRMScores=meanRRMScores,
                          maxmeanRRMQuestion=maxmeanRRMQuestion,
                          maxMeanRRM=maxMeanRRM,
                          RRMSensors=RRMMeasurementSensors,
                          RRMScoresDF=RRMScoresDF,
                          RRMMeasurementsDF=RRMMeasurementsDF )
    
  }
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, RRMOutputList, env=.GlobalEnv)
    
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "RRMAnalysisOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      RRMOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  return(RqCqDFSeries)
  
  # end RRMScoreFn()
} 


