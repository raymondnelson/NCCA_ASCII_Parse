# Rank Order Scoring System 
# Honts and Driscoll (1987, 1988)



source(paste0(RPath, 'ROSSModel.R'), echo=FALSE)



ROSSScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                         ROSSDecisionRule="GTR",
                         forced=FALSE,
                         makeDF=makeDF,
                         saveCSV=saveCSV,
                         analysisListName="analysisResultList" ) {
  # R function to compute the ROSS scores from the measurements data frame
  # Raymond Nelson
  # 3-3-2019
  #
  # called by the getScoresFn() in the scores.R script
  #
  # Rank Order Scoring System 
  # Honts and Driscoll (1987, 1988)
  #
  ###
  #
  # x input is a data frame of measurements for the RQs and CQs for a series
  # including all charts in the series
  # output is the RqCqDF data frame with the ESSScore column populated 
  # with ESS scores
  #
  ####

  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    print("calculate the ROSS scores")
    
    RqCqDFSeries$ROSSScore <- ""
    
    if(!exists("ROSSDecisionRule")) ROSSDecisionRule <- "GTR"
    
    uniqueSensors <- unique(as.character(RqCqDFSeries$sensorName))
    
    ROSSSensors <- c("UPneumo", 
                     "LPneumo", 
                     "Pneumo",
                     "AutoEDA", 
                     # "ManualEDA",
                     "Cardio"
                     )
    
    # ROSSSensors <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
    
    # exit if ROSS Sensors are missing
    if(length(which(!(ROSSSensors %in% uniqueSensors))) > 1) {
      return(RqCqDFSeries)
    }
    
    # keep only the extant sensors
    ROSSSensors <- ROSSSensors[ROSSSensors %in% uniqueSensors]
    
  }
  
  ######## get the questions and charts ##########
    
  {
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      return(RqCqDFSeries) 
    }
    
    # unique RQs and CQs for the series
    uniqueRQs <- 
      unique(RqCqDFSeries$eventLabel[grep("R", RqCqDFSeries$eventLabel)])
    uniqueCQs <- 
      unique(RqCqDFSeries$eventLabel[grep("C", RqCqDFSeries$eventLabel)])
    
    # exit if not at least 2RQs and 2 CQs
    if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
  }
  
  ############## iterate over the ROSS charts ###############
  
  i=1
  for (i in 1:min(c(3, length(uniqueCharts)))) {
    
    {
      
      # use only 3 charts
      
      ### first get the RqCq data frame for this chart
      
      thisChart <- uniqueCharts[i]
      thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
      
      RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      
      uniqueRQsChart <- 
        unique(RqCqDFChart$eventLabel[grep("R", RqCqDFChart$eventLabel)])
      
      ## limit to 3 RQs ##
      # if(length(uniqueRQsChart) > 3) {
      #   ignoreTheseRQs <- uniqueRQsChart[c(4:length(uniqueRQsChart))]
      #   thisChartRows <- 
      #     thisChartRows[!(RqCqDFChart$eventLabel %in% ignoreTheseRQs)]
      #   RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      # }
      
      # increment the loop to the next chart if no events
      if(nrow(RqCqDFChart) < 2) { 
        next()
      }
      
    }
    
    ##  iterate on the selected sensor rows and rank the scores
    
    j=1
    for (j in 1:length(ROSSSensors)) {
      
      thisSensor <- ROSSSensors[j]
      sensorRows <- which(RqCqDFChart$sensorName == thisSensor)
      
      # rank the sensor score, 
      if( any(thisSensor=="UPneumo", 
              thisSensor=="LPneumo", 
              thisSensor=="Pneumo") ) {
        theseMeasurements <- RqCqDFChart$sensorMeasurement[sensorRows]
        if(length(which(is.na(theseMeasurements))) != length(sensorRows)) {
          RqCqDFChart$ROSSScore[sensorRows] <- 
            # rank the pneumo scores in reverse
            # smallest measurement gets the largest rank value
            rank(-RqCqDFChart$sensorMeasurement[sensorRows],
                 na.last=TRUE,
                 ties.method="average")
        }
      } else {
        # rank the other sensors 
        # largest response measurement gets the largest rank value
        RqCqDFChart$ROSSScore[sensorRows] <- 
          rank(RqCqDFChart$sensorMeasurement[sensorRows],
               na.last=TRUE,
               ties.method="average")
      }
      
    } # end loop j over sensors
    
    #### combine the upper and lower pneumo scores ####
    
    {
      
      UPneumoRows <- which(RqCqDFChart$sensorName == "UPneumo")
      P2 <- RqCqDFChart$ROSSScore[UPneumoRows]
      P2 <- as.numeric(P2)
      LPneumoRows <- which(RqCqDFChart$sensorName == "LPneumo")
      P1 <- RqCqDFChart$ROSSScore[LPneumoRows]
      P1 <- as.numeric(P1)
      
      pneumoRows <- which(RqCqDFChart$sensorName == "Pneumo")
      
      # add the upper and lower pneumo rows and rank the result
      if(length(which(is.na(P1 + P2))) != length(P1 + P2)) {
        # RqCqDFChart$ROSSScore[pneumoRows] <- rank(P1 + P2,
        #                                           na.last=TRUE,
        #                                           ties.method="average")
        # same results this way
        # average the upper and lower pneumo rows and rank the result
        RqCqDFChart$ROSSScore[pneumoRows] <- rank((P1 + P2)/2,
                                                  na.last=TRUE,
                                                  ties.method="average")
      }
      
      
    }
    
    #### pass the chart to the series ####
    
    RqCqDFSeries[thisChartRows,] <- RqCqDFChart
    
    # assign("RqCqDFSeries", RqCqDFSeries, env=.GlobalEnv)
    
    # now we have the rank scores for all charts in the series
    
  } # end loop i over charts in the series
  
  ###### separate the comparison and relevant questions ######
  
  {
    
    rqRows <- grepl("R", RqCqDFSeries$eventLabel)
    cqRows <- grepl("C", RqCqDFSeries$eventLabel)
    
    # get the RQs and CQs for this chart
    # uniqueRQs <- unique(RqCqDFSeries[rqRows,'eventLabel'])
    # uniqueCQs <- unique(RqCqDFSeries[cqRows,'eventLabel'])
    
    if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) {
      next() # next
      # Dec 17 2021 not clear what is next bc we are not iterating over charts
    }
    
    ROSSSensors2 <- c("Pneumo", "AutoEDA", "Cardio")
    ROSSSensors2 <- ROSSSensors2[ROSSSensors2 %in% uniqueSensors]
    
    ROSSRows <- RqCqDFSeries$sensorName %in% ROSSSensors2
    
    rqDF <- RqCqDFSeries[which(rqRows & ROSSRows),]
    cqDF <- RqCqDFSeries[which(cqRows & ROSSRows),]
    # View(rqDF)
    # View(cqDF)
    
    # assign("rqDF", rqDF, pos=1)
    # assign("cqDF", cqDF, pos=1)
    
  } 
  
  ################# calculate the ROSS result #################
  
  {
    
    # Honts & Driscoll (1987) - single issue exams
    
    CQTotal <- sum(as.numeric(cqDF$ROSSScore), na.rm=TRUE) 
    
    RQTotal <- sum(as.numeric(rqDF$ROSSScore), na.rm=TRUE) 
    
    #### adjust for imbalanced RQs and CQs
    if(isTRUE(forced)) {
      
      # adjustFactor <- nrow(rqDF) / nrow(cqDF)
      # CQTotal <- CQTotal * adjustFactor
      
      adjRQ <- 27 / nrow(rqDF)
      adjCQ <- 27 / nrow(cqDF)
      
      RQTotal <- RQTotal * adjRQ
      CQTotal <- CQTotal * adjCQ
      
    }
    
    # sum(as.numeric(RqCqDFSeries$ROSSScore), na.rm=TRUE) 
    
    # calculate the difference
    CRDiffScore <- round(CQTotal - RQTotal, 1)
    
  }
  
  ########## Honts & Driscoll (1988) - mixed issue exams ###########
  
  {
    
    # calculate the comparison question subtotal score 
    CQScore <- 
      round(sum(as.numeric(cqDF$ROSSScore), na.rm=TRUE) / length(uniqueCQs), 2)
    
    # View(cqDF)
    
    # calculate the RQ scores
    
    # create a vector of RQScores
    RQScores <- rep(NA, length=length(uniqueRQs))
    # iterate over the RQs and calculate the RQScores
    k=1
    for(k in 1:length(uniqueRQs)) {
      thisRQ <- uniqueRQs[k]
      # RQRows <- which(RqCqDFSeries$eventLabel == thisRQ)
      RQRows <- which(rqDF$eventLabel == thisRQ)
      # RQScores[k] <- 
      #   round(sum(as.numeric(RqCqDFSeries$ROSSScore[RQRows]), na.rm=TRUE), 2)
      RQScores[k] <- 
        round(sum(as.numeric(rqDF$ROSSScore[RQRows]), na.rm=TRUE), 2)
    }
    
    RQDiffScores <- round(CQScore - RQScores, 2)
    names(RQDiffScores) <- uniqueRQs
    
    thisSubtotal <- which.min(RQDiffScores)
    minSubtotalScore <- RQDiffScores[thisSubtotal]
    minSubtotal <- uniqueRQs[thisSubtotal]
    
  }
  
  ###### adaptation for TES DLST PCASS PCAT PCAT2 LXCQT and LXCAT2 formats ######
  
  if(any(uniqueRQs[1] == "1R1", uniqueRQs == "1R3", uniqueRQs == "1R5")) {
    
    ifelse(uniqueRQs[1] == "1R1",
           uniqueRQsP <- c("R1", "R2"),
           ifelse(uniqueRQs[1] == "1R3",
                  uniqueRQsP <- c("R3", "R4"),
                  ifelse(uniqueRQs[1] == "1R5",
                         uniqueRQsP <- c("R5", "R6"),
                         uniqueRQsP <- c("R1", "R2") ) ) )
    
    if(length(uniqueRQs) == 8) {
      stR1 <- sum(RQDiffScores[c(1, 3, 5, 7)])
      stR2 <- sum(RQDiffScores[c(2, 4, 6, 8)])
    } else {
      stR1 <- sum(RQDiffScores[c(1, 3, 5)])
      stR2 <- sum(RQDiffScores[c(2, 4, 6)])
    }
    
    RQDiffScoresP <- c(stR1, stR2)
    names(RQDiffScoresP) <- uniqueRQsP
    
    # substitute the adjusted values
    uniqueRQs <- uniqueRQsP
    RQDiffScores <- RQDiffScoresP
    
    thisSubtotal <- which.min(RQDiffScores)
    minSubtotalScore <- RQDiffScores[thisSubtotal]
    minSubtotal <- uniqueRQs[thisSubtotal]
    
  }
  
  ###### ROSS reference model (Honts & Driscoll, 1987; 1988) ######
  
  # source(paste0(RPath, 'ROSSModel.R'), echo=FALSE)
  
  # ROSSCutScores <- c(GTDI=-14, GTNDI=14, STDI=-3, STNDI=3)
  
  # ROSSCutScores are in the NCCAASCII_init.R script
  
  ############### ROSS results ###############
    
  {
  
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    GTRResult <- GTRFn(totalScore=CRDiffScore, 
                       RQNames=uniqueRQs,
                       cutScores=ROSSCutScores, 
                       flip=FALSE )
    
    SSRResult <- SSRFn(subtotalScores=RQDiffScores, 
                       cutScores=ROSSCutScores, 
                       flip=FALSE )
    
    ### select the result per the decision rule
    
    ROSSTestResult <- ifelse(ROSSDecisionRule=="GTR",
                             GTRResult$testResult,
                             SSRResult$testResult )
    
    # ifelse is vecorized internally, but returns only the first item
    ifelse(ROSSDecisionRule=="GTR",
           ROSSQuestionResults <- GTRResult$subtotalResults,
           ROSSQuestionResults <- SSRResult$subtotalResults )
    
    ifelse(ROSSDecisionRule=="GTR",
           resultUsing <- GTRResult$resultUsing,
           resultUsing <- SSRResult$resultUsing )
    
    ifelse(ROSSDecisionRule == "GTR",
           outputCutscores <- ROSSCutScores[1:2],
           outputCutscores <- ROSSCutScores[3:4])
    
  }
  
  ############ ROSS output section ############
  
  {
    
    ### ROSS measurements data frame 
    
    measurementsDF <- 
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=ROSSSensors,
                         decimals=2,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    # View(measurementsDF)
    
    ### rank scores table
    
    rankScoreDF <- rankTableFn(RqCqDFSeries=RqCqDFSeries, 
                               useRank="ROSS",
                               useSensors=ROSSSensors2,
                               makeDF=makeDF,
                               saveCSV=saveCSV )
    # View(rankScoreDF)
    
  }
  
  ########## construct a list to hold the ROSS result ###########
  
  {
    
    ROSSMeasurementSensors <- c("UPneumo", 
                                "LPneumo", 
                                "AutoEDA", 
                                "Cardio" )
    
    measurementsDF <- 
      measurementsDF[measurementsDF$sensorName %in% ROSSMeasurementSensors,]
    
    
    outputListName <- paste(examName, seriesName, "ROSSOutputList", sep="_")
    
    ROSSOutputList <- list(ROSS="Rank Order Scoring System (Honts & Driscoll (1987, 1988)",
                           examName=examName,
                           seriesName=seriesName,
                           ROSSResult=ROSSTestResult,
                           ROSSQuestionResults=ROSSQuestionResults,
                           ROSSQuestions=uniqueQuestions,
                           ROSSRQNames=uniqueRQs,
                           ROSSDecisionRule=ROSSDecisionRule,
                           ROSSResultUsing=resultUsing,
                           ROSSCutscores=outputCutscores,
                           ROSSGrandTotal=CRDiffScore,
                           ROSSMinSubtotal=minSubtotalScore,
                           ROSSSubtotalScores=RQDiffScores,
                           ROSSRQTotal=RQTotal,
                           ROSSCQTotal=CQTotal,
                           ROSSCQScore=CQScore,
                           ROSSRQScores=RQScores,
                           ROSSSensors=ROSSSensors,
                           ROSSRankScoreDF=rankScoreDF, 
                           ROSSMeasurementDF=measurementsDF )
  }
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, ROSSOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "ROSSOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      ROSSOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### output ####
  
  return(RqCqDFSeries)
  
}


