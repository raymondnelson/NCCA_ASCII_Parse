



miritelloRankFn <- function(RqCqDFSeries=RqCqDFSeries, 
                            makeDF=TRUE,
                            saveCSV=FALSE,
                            analysisListName="analysisResultList" ) {
  # R function to compute the Miritello rank scores from the measurements data frame
  # Miritello (1999) rank proportion scores
  #
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  # x input is a data frame of measurements for the RQs and CQs for a test chart
  # output is the RqCqDF data frame with the Miritello rank score column populated 
  # 
  # 1) count the number of questions to be evaluated and rank them from N-1 to 0
  # 2) calculate tied ranks
  # 3) assign the mean rank to all rank scores for any sensors that have unusable data
  # 4) sum the sensor ranks for each question
  # 5) calcualte the rank ratio by dividing the question total by the max value,
  #    where the max value is the number of sensors * N-1
  #
  # Miritello ranks are inverse
  # with smallest response rank = 0
  # and largest = n-1
  #
  # this function differs slightly from Miritello (1999) in that
  # ranks are not combined between sensors prior to
  # calculation of the rank proportion - this can be easily done later.
  # Use of rank proportions conveys information about about
  # the relative strengthg of each item compared to others.
  #
  ####
  
  {
    
    print("calculate the Miritello rank score for all charts in the series")
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # if(seriesName == "4") {
    #   assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    #   stop()
    # }
    
    # reset the miritelloRankScore column
    RqCqDFSeries$miritelloRankScore <- ""
    # View(RqCqDFSeries)
    
    # reset to input values
    # RqCqDFSeries$chartName <- RqCqDFSeries$chartName[1]
    # RqCqDFSeries$Label <- RqCqDFSeries$eventLabel
    
  }
  
  {
    
    # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    miritelloRankSensors <- c("UPneumo", 
                              "LPneumo", 
                              "Pneumo",
                              "AutoEDA", 
                              # "ManualEDA", 
                              "Cardio", 
                              # "FC", 
                              "PLE")
    
    # remove sensors not in the data 
    miritelloRankSensors <- 
      miritelloRankSensors[miritelloRankSensors %in% uniqueSensors]
    
    # exclude the PLE using a setting and if missing
    if("PLE" %in% miritelloRankSensors && !isTRUE(includePLEScores)) {
      miritelloRankSensors <- miritelloRankSensors[-which(miritelloRankSensors %in% "PLE")]
    }
    
    # use only the combined pneumo
    miritelloRankSensorsP <- 
      miritelloRankSensors[!(miritelloRankSensors %in% c("UPneumo", "LPneumo"))]
    
    # initialize a vector of unique RQ and CQ events
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    # unique(RqCqDFSeries$Label)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) < 4) { 
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
  }
  
  #### iterate over the charts to calculate the miritello rank scores ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    {
      
      chartRows <- which(RqCqDFSeries$chartName == uniqueCharts[i])
      
      # initialize the chart data frame
      RqCqDFChart <- RqCqDFSeries[chartRows,]
      # View(RqCqDFChart)
      
      uniqueEventsChart <- unique(RqCqDFChart$eventLabel)
      
      if(length(uniqueEventsChart) < 4) next()
      
    }
    
    # iterate over the sensor names 
    # to calculate the Miritello rank scores
    j=4 # 4 is the EDA sensor
    for (j in 1:length(miritelloRankSensors)) {
      
      {
        
        thisSensor <- miritelloRankSensors[j]
        
        sensorRows <- which(RqCqDFChart$sensorName==thisSensor)
        
        # initialize a vector of measurements for the sensor
        measurementVector <- RqCqDFChart[sensorRows,'sensorMeasurement']
        
        # oct 1, 2021
        # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
        #   measurementVector <- exp(measurementVector)
        #   # pneumo R/C ratios are now decimals < 1 
        #   # and smaller values are larger responses 
        #   # values of 0 are now 1 because exp(0) = 1
        # }
        
        # next iteration if there are no measurements
        if(length(which(!is.na(measurementVector))) == 0) {
          next()
        }
        
        # zero as missing
        measurementVector[measurementVector == 1] <- NA
        
        # Oct 4, 2021
        # # rescale the respiration measurements 
        # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
        #   measurementVector <- round(exp(-measurementVector) * 100, 3)
        #   # larger values now signify greater changes in physiology
        # }
        
        # make a vector to hold NA values
        naRanks <- is.na(measurementVector)
        
        # increment the loop if all are NA, probably do not need this
        if(length(which(naRanks))==length(measurementVector)) {
          # assign the mean rank to all 
          RqCqDFChart$miritelloRankScore[sensorRows] <- 
            mean(c(1:length(measurementVector)))
          next()
        }
        
      }
      
      #### calculate the ranks beginning at 0 ####
      
      # rank values start at 0 for the smallest change in physiology
      rankVals <- 
        rank(measurementVector, ties.method="average", na.last="keep") - 1
      # reset the NA values
      # not needed when using na.last="keep"
      # rankVals[which(naRanks)] <- NA
      
      # pass the rank values to the chart DF
      RqCqDFChart$miritelloRankScore[sensorRows] <- rankVals
      
    } # end iteration over j uniqueSensors
    
    #### average the 2 pneumo ranks ####
    
    {
      
      # Miritello (1999) did not describe a procedure
      # to combine the 2 respiration sensors to a single rank score
      
      pneumoRows <- which(RqCqDFChart$sensorName == "Pneumo")
      UpPneumoRows <- which(RqCqDFChart$sensorName == "UPneumo")
      LpPneumoRows <- which(RqCqDFChart$sensorName == "LPneumo")
      
      miritelloRankUp <- as.numeric(RqCqDFChart$miritelloRankScore[UpPneumoRows])
      miritalloRankLp <- as.numeric(RqCqDFChart$miritelloRankScore[LpPneumoRows])
      
      for(k in 1:length(pneumoRows)) {
        RqCqDFChart$miritelloRankScore[pneumoRows][k] <- 
          round(mean(c(miritelloRankUp[k], miritalloRankLp[k]), na.rm=TRUE), 3)
      }
      # View(RqCqDFChart)
      
    }
    
    #### pass the chart data frame back to the series data frame ####
    
    RqCqDFSeries[chartRows,] <- RqCqDFChart
    
    # at this point we have the rank0 values for all RQs and all charts
    
  } # end loop i over charts
  
  ##### check for DLST DLDT and PCASS type charts #####
  
  {
    
    saveQuestionLabels <- RqCqDFSeries$Label
    saveEventLabels <- RqCqDFSeries$eventLabel
    
    saveChartNames <- rep(uniqueCharts[1], length=nrow(RqCqDFSeries))
    saveRqCQDFSeriesChartName <- RqCqDFSeries$chartName
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
  
  #### initialize and populate a data frame of rank proportion scores ####
  
  {
    
    {
      
      ## initialize the data frame ##
      
      miritelloRankScoresDF <- 
        data.frame(matrix(ncol=(length(uniqueQuestions)), 
                          nrow=length(uniqueCharts)*length(miritelloRankSensors)))
      names(miritelloRankScoresDF) <- uniqueQuestions
      # add the sensor column
      miritelloRankScoresDF <- 
        cbind(sensorName=rep(miritelloRankSensors, times=length(uniqueCharts)), 
              miritelloRankScoresDF)
      miritelloRankScoresDF$sensorName <- 
        as.character(miritelloRankScoresDF$sensorName)
      # add the chartName column
      miritelloRankScoresDF <- 
        cbind(chartName=rep(uniqueCharts, each=length(miritelloRankSensors)), 
              miritelloRankScoresDF)
      miritelloRankScoresDF$chartName <- 
        as.character(miritelloRankScoresDF$chartName)
      # add  the series column
      miritelloRankScoresDF <- 
        cbind(seriesName=rep(seriesName, length=nrow(miritelloRankScoresDF)),
              miritelloRankScoresDF)
      miritelloRankScoresDF$seriesName <- as.character(miritelloRankScoresDF$seriesName)
      # add the exam name column
      miritelloRankScoresDF <- 
        cbind(examName=rep(examName, length=nrow(miritelloRankScoresDF)),
              miritelloRankScoresDF)
      miritelloRankScoresDF$examName <- as.character(miritelloRankScoresDF$examName)
      # View(miritelloRankScoresDF)
      
    }
    
    {
      
      ## populate the data frame with the measurements ##
      
      # iterate over the question columns
      i=6
      for(i in 5:ncol(miritelloRankScoresDF)) {
        thisQuestion <- names(miritelloRankScoresDF)[i]
        # iterate over the rows for all sensors and all charts
        j=9
        for(j in 1:nrow(miritelloRankScoresDF)) {
          thisChart <- miritelloRankScoresDF[j,3]
          thisSensor <- miritelloRankScoresDF[j,4]
          # now get the measurement
          thisOne <- which(RqCqDFSeries$chartName==thisChart &
                             RqCqDFSeries$sensorName==thisSensor &
                             RqCqDFSeries$eventLabel==thisQuestion)
          if(length(thisOne) == 0 ) next()
          miritelloRankScoresDF[j,i] <- 
            round(as.numeric(RqCqDFSeries$miritelloRankScore[thisOne]), 2)
        }
      }
      
    }
    
    # View(miritelloRankScoresDF)
  }
  
  #### save it to the global envir and current working directory ####
  
  outputDFName <- paste(examName, seriesName, "miritelloRankScoreDF", sep="_")
  
  if(isTRUE(makeDF)) {
    assign(outputDFName, miritelloRankScoresDF, pos=1)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(miritelloRankScoresDF,
              file=paste0(str_sub(outputDFName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  ###### calculate the within chart rank sums for all questions #######
  
  {
    
    miritelloRankSensors2 <- c("Pneumo",
                               "AutoEDA", 
                               "Cardio", 
                               "PLE")
    
    dfSensors <- unique(miritelloRankScoresDF$sensorName)
    
    miritelloRankSensors2 <- 
      miritelloRankSensors2[miritelloRankSensors2 %in% dfSensors]
    
    # exclude the PLE
    if("PLE" %in% miritelloRankSensors2 && !isTRUE(includePLEScores)) {
      miritelloRankSensors2 <- miritelloRankSensors2[-which(miritelloRankSensors2 %in% "PLE")]
    }
    
    miritelloRankScoresDF2 <- 
      miritelloRankScoresDF[miritelloRankScoresDF$sensorName %in% miritelloRankSensors2,]
    # str(miritelloRankScoresDF)
    # View(miritelloRankScoresDF)
    
    # call a function to get the within chart RQ subtotals
    chartTotalsDF <- chartTotalsFn(scoreSheetDF=miritelloRankScoresDF2,
                                   outputName="ESSMChartTotalsDF",
                                   aggType="sum",
                                   weightingCoefs=NULL,
                                   makeDF=FALSE,
                                   saveCSV=FALSE)
    
    # chartTotalsDF include all CQs and RQs
    # because the input data from contains all CQs and RQs
    
  }
  
  ##### calculate the rank proportions for the within chart rank sums #####
  
  {
    
    # make a copy to hold the rank proportions
    rankProportionsDF <- chartTotalsDF
    
    # iterate over the charts and calculate the rank proportions
    k=1
    for(k in 1:nrow(chartTotalsDF)) {
      # calculate the max score 
      maxScoreVal <- 
        (length(which(!is.na(chartTotalsDF[k,(4:ncol(chartTotalsDF))])))-1) * 
        length(miritelloRankSensors2)
      # calculate the rank proportion and save it
      rankProportionsDF[k,(4:ncol(chartTotalsDF))] <- 
        round(chartTotalsDF[k,(4:ncol(chartTotalsDF))] / maxScoreVal, 2)
    }
    # View(rankProportionsDF)
    
  }
  
  #### initialize a measurement table for output ####
    
  {
    
    # # reset the chartNames
    # if(isTRUE(DLSTType)) {
    #   RqCqDFSeries$chartName <- saveChartNames 
    # }
    
    miritelloRankMeasurementSensors <- c("UPneumo", 
                                         "LPneumo", 
                                         "AutoEDA", 
                                         "Cardio", 
                                         "PLE")
    
    miritelloRankMeasurementSensors <- 
      miritelloRankMeasurementSensors[miritelloRankMeasurementSensors %in% 
                                        RqCqDFSeries$sensorName]
    
    # exclude the PLE
    if("PLE" %in% miritelloRankMeasurementSensors && !isTRUE(includePLEScores)) {
      miritelloRankMeasurementSensors <- miritelloRankMeasurementSensors[-which(miritelloRankMeasurementSensors %in% "PLE")]
    }
    
    # call a function to make the measurement data frame
    miritelloRankMeasurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=miritelloRankMeasurementSensors,
                         decimals=2,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    # View(miritelloRankMeasurementsDF)
    # str(miritelloRankMeasurementsDF)
    
  }
  
  ######## fix the question labels for DLST DLDT type exams ########
  
  if(isTRUE(DLSTType)) {
    
    # parse the RQs for DLST/DLDT/PCASS type formats 
    
    RqCqDFSeries$Label <- 
      fixDLSTDLDTFn(saveQuestionLabels, CQRQLabels)
    
    # eventLabels column will retain the original question labels
    
    # make a new vector of unique RQs
    uniqueQuestions <- unique(RqCqDFSeries$Label)
    uniqueRQs <- unique(uniqueQuestions[grep("R", uniqueQuestions)])
    uniqueCQs <- unique(uniqueQuestions[grep("C", uniqueQuestions)])
    
  }
  
  ##### adjust the chartNames for DLST type formats #####
  
  if(isTRUE(DLSTType)) {
    
    # call a function to fix the RQ Labels
    
    # RqCqDFSeries$Label <-
    #   fixPCASS2RQsFn(saveQuestionLabels, CQRQLabels)
    
    # nov 22, 2021
    RqCqDFSeries$Label <- 
      fixDLSTDLDTFn(saveQuestionLabels, CQRQLabels)
    
    # RqCqDFSeries$eventLabel <-
    #   fixPCASS2RQsFn(saveQuestionLabels, CQRQLabels)
    
    # View(RqCqDFSeries)
    
    saveUniqueCharts <- uniqueCharts
    saveRqCQDFSeriesChartName <- RqCqDFSeries$chartName
    
    # for DLST/DLDT/PCASS exams the number of charts
    # is equal to the number of iterations of the RQs
    
    # make new chart names
    newChartNames <- rep(NA, length=length(saveRqCQDFSeriesChartName))
    lastChartNumber=0
    i=1
    for(i in 1:length(newChartNames)){
      thisChartNumber <- str_sub(saveEventLabels[i], 1, 1)
      if(as.numeric(thisChartNumber < as.numeric(lastChartNumber))) {
        thisChartNumber <- lastChartNumber
      }
      newChartNames[i] <- paste0("0", thisChartNumber, "A")
      lastChartNumber <- thisChartNumber
    }
    
    RqCqDFSeries$chartName <- newChartNames
    
    uniqueCharts <- unique(newChartNames)
    
    # RqCqDFSeries$
    
  }
  
  ###### rearrange the charts for DLST type formats ######
  
  {
    
    # this code section applies to DLST and conventional chart formats
    
    # parse the repeated question iterations into pseudo-charts #
    # the get the between chart mean for the rank proportions
    # for all questions 
    
    ## initialize a data frame ##
    
    rankProportionsDF2 <- data.frame(matrix(ncol=(length(uniqueQuestions)), 
                                            nrow=length(uniqueCharts)))
    names(rankProportionsDF2) <- uniqueQuestions
    # add the chartName column
    rankProportionsDF2 <- cbind(chartName=uniqueCharts, rankProportionsDF2)
    rankProportionsDF2$chartName <- as.character(rankProportionsDF2$chartName)
    # add  the series column
    rankProportionsDF2 <- cbind(seriesName=seriesName, rankProportionsDF2)
    rankProportionsDF2$seriesName <- as.character(rankProportionsDF2$seriesName)
    # add the exam name column
    rankProportionsDF2 <- cbind(examName=examName, rankProportionsDF2)
    rankProportionsDF2$examName <- as.character(rankProportionsDF2$examName)
    # View(rankProportionsDF2)
    
    ## populate the data frame with the measurements ##
    
    # iterate over the question columns of rankProportionsDF
    i=4
    for(i in 4:ncol(rankProportionsDF)) {
      thisQuestion <- names(rankProportionsDF)[i]
      thisChart <- paste0("0", str_sub(thisQuestion, 1, 1), "A")
      newQuestion <- str_sub(thisQuestion, 2, -1)
      
      thisRankPropValue <- rankProportionsDF[1, thisQuestion]
      
      # get the column and row
      thisRow <- which(rankProportionsDF2$chartName == thisChart)
      thisCol <- which(names(rankProportionsDF2) ==  newQuestion)
      
      # assign the value to the new data frame
      rankProportionsDF2[thisRow, thisCol] <- thisRankPropValue
    }
    
    # View(rankProportionsDF2)
    
  }
  
  ####### between chart mean rank proportions #######
  
  {
    
    if(isTRUE(DLSTType)) {
      meanRankProportions <-
        colMeans(rankProportionsDF2[,4:ncol(rankProportionsDF2)], na.rm=TRUE)
    } else {
      meanRankProportions <-
        colMeans(rankProportionsDF[,4:ncol(rankProportionsDF)], na.rm=TRUE)
    }
    
    meanRankProportions <- round(meanRankProportions, 2)
    
    maxMeanRankProportion <- meanRankProportions[which.max(meanRankProportions)]
    
    maxMeanRankPropQuestion <- names(meanRankProportions)[which.max(meanRankProportions)]
    
  }
  
  ############### output section ###############
  
  {
      
      #### construct a list to hold the rank score results ####
      
      outputListName <- 
        paste(examName, seriesName, "miritelloRankOutputList", sep="_")
      
      miritelloRankOutputList <- 
        list(miritelloRank="Field Rank Order Analysis (Miritello, 1999)",
             examName=examName,
             seriesName=seriesName,
             uniqueQuestions=uniqueQuestions,
             
             meanRankProportions=meanRankProportions,
             maxMeanRankProportion=maxMeanRankProportion,
             maxMeanRankPropQuestion=maxMeanRankPropQuestion,
            
             miritelloRankScoresDF=miritelloRankScoresDF,
             rankSensors=miritelloRankSensors2,
             rankScoresDF=miritelloRankScoresDF2,
             rankProportionsDF=rankProportionsDF,
             rankSumsDF=chartTotalsDF,
             miritelloRankMeasurementsDF=miritelloRankMeasurementsDF )
      
    }
    
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, miritelloRankOutputList, env=.GlobalEnv)
    
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "miritelloRankAnalysisOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      miritelloRankOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames 
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(RqCqDFSeries)
  
  # end miritelloRankFn()
} 


