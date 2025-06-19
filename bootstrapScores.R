# Honts and Dewitt Bootstrap Scoring Algorithm (1992)



# a function to calculate the population st dev
sdp <- function(x) {
  # using the sample variance function to recalc the pop st dev
  # input is a vector of numeric values
  (sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
}


bootstrapScoresFn <- function (RqCqDFSeries=RqCqDFSeries,
                               bootstrapDecisionRule="GTR",
                               bootstrapCutProbT=.3,
                               bootstrapCutProbD=.7,
                               forced=FALSE,
                               numberReSamples=6000,
                               makeDF=makeDF,
                               saveCSV=saveCSV,
                               analysisListName="analysisResultList" ) {
  # R function to compute the bootstrap scoring system
  # Raymond Nelson June 1, 2019
  # 
  # a similar method was described by Honts & Devitt (1992)
  # 
  # input is a data frame of RQ and CQ measurements for a series (all charts)
  # including all test charts with RQs and CQs
  # and all sensors (upper and lower respiration, electrodermal, cardio)
  #
  # output is the RqCqDFSeries data frame 
  # with the bootstrapScore column populated 
  # additional output is via side effect
  # 
  ####
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    print("calculate the Bootstrap scores")
    
    # View(RqCqDFSeries)
    
    # forced <- TRUE
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    {
      if(!exists("forced")) forced <- FALSE
      if(!exists("bootstrapDecisionRule")) bootstrapDecisionRule <- "GTR"
      if(!exists("bootstrapCutProbT")) bootstrapCutProbT <- .3
      if(!exists("bootstrapCutProbD")) bootstrapCutProbD <- .7
      if(!exists("numberReSamples")) numberReSamples <- 3000
      if(!exists("makeDF")) makeDF <- TRUE
      if(!exists("saveCSV")) saveCSV <- FALSE
    }
    
    bootstrapSensors <- c("UPneumo", 
                    "LPneumo", 
                    "Pneumo", 
                    "AutoEDA", 
                    "ManualEDA", 
                    # "FC",
                    "Cardio" )
    
    # uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    # uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
  }
  
  ######## check some boundary conditions ########
  
  {
    
    # default message
    bootstrapWarning <- "none"
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    # exit if boostrap Sensors are missing
    if(length(which(!(bootstrapSensors %in% uniqueSensors))) > 2) {
      return(RqCqDFSeries)
    }
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      return(RqCqDFSeries) 
    }
    
    # boostrap algorithm does not work with data at the chart level
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    # used only to determine the correct number of charts
    if( (length(uniqueCharts) < 3 || length(uniqueCharts) > 5) && 
        forced==FALSE ) { 
      "exiting - requires 3 to 5 charts"
      return(RqCqDFSeries) 
    }
    
    rqRows <- grep("R", RqCqDFSeries$eventLabel)
    cqRows <- grep("C", RqCqDFSeries$eventLabel)
    
    # exit if there are no RQs or no CQs
    if(length(rqRows) == 0 || length(cqRows) == 0) { 
      return(RqCqDFSeries) 
    }
    
    uniqueRQs <- unique(RqCqDFSeries[rqRows,'eventLabel'])
    uniqueCQs <- unique(RqCqDFSeries[cqRows,'eventLabel'])
    
    # exit if not at least 2RQs and 2 CQs
    if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) {
      "WARNING: incorrect number of RQs and CQs"
      return(RqCqDFSeries) 
    } else if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) {
      bootstrapWarning <- 
        "WARNING: bootstrap algorithm assumes a minimum of 2 of RQs and 2 CQs"
    }
    
    # exit if unequal number of RQs and CQs
    if(length(uniqueRQs) != length(uniqueCQs) && !isTRUE(forced)) {
      print("WARNING: bootstrap algorithm assumes an equal number of RQs and CQs")
      return(RqCqDFSeries)
      # mean replacement is used later as an option
    } else if(length(uniqueRQs) != length(uniqueCQs)) {
      bootstrapWarning <- "WARNING: bootstrap algorithm requires an equal number of RQs and CQs"
    }
    
  }
  
  ##############   standardize the sensor data   ###############
  
  {
    
    # initialize some vectors
    sensorMeans <- rep(NA, length=length(bootstrapSensors))
    sensorSDs <- rep(NA, length=length(bootstrapSensors))
    
    # # private function to calculate the population st dev
    # sdp <- function(x) {
    #   # using the sample variance function to recalc the pop st dev
    #   # input is a vector of numeric values
    #   (sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
    # }
    
    # calculate the sensor means and standard deviations
    for(i in 1:length(bootstrapSensors)) {
      theseRows <- which(RqCqDFSeries$sensorName == bootstrapSensors[i])
      sensorMeans[i] <- 
        mean(RqCqDFSeries$sensorMeasurement[theseRows], na.rm=TRUE)
      # use the private function for the  population standard deviation
      sensorSDs[i] <- 
        sdp(RqCqDFSeries$sensorMeasurement[theseRows])
    }
    
    # calculate the z scores so all sensor data have a common metric
    for(j in 1:length(sensorMeans)) {
      theseSensorRows <- 
        which(RqCqDFSeries$sensorName == bootstrapSensors[j])
      RqCqDFSeries$bootstrapScore[theseSensorRows] <- 
        round((RqCqDFSeries$sensorMeasurement[theseSensorRows] - 
           sensorMeans[j]) / sensorSDs[j], 2)
    }
    
  }
  
  #### invert the sign of the pnuemo measurements ####
  
  {
    
    # smaller pneumo values are greater changes in physiology
    
    thesePneumoRows <- which(RqCqDFSeries$sensorName %in% 
                               c("UPneumo", "LPneumo"))
    
    RqCqDFSeries$bootstrapScore[thesePneumoRows] <- 
      as.numeric(RqCqDFSeries$bootstrapScore[thesePneumoRows]) * -1
    
    # z-scores are in the bootstrapScore column
    
    # as.numeric(RqCqDFSeries$bootstrapScore[theseSensorRows])
    
  }
  
  #### average the z scores for the upper and lower pneumo sensors ####
  
  {
    
    UPneumoRows <- which(RqCqDFSeries$sensorName == "UPneumo")
    LPneumoRows <- which(RqCqDFSeries$sensorName == "LPneumo")
    
    PneumoRows <- which(RqCqDFSeries$sensorName == "Pneumo")
    
    RqCqDFSeries$bootstrapScore[PneumoRows] <- 
      round((as.numeric(RqCqDFSeries$bootstrapScore[UPneumoRows]) + 
         as.numeric(RqCqDFSeries$bootstrapScore[LPneumoRows])) / 2, 2)
    # View(RqCqDFSeries)
    
  }
  
  ######## calculate the CQ - RQ difference ########
  
  {
    
    rqDFSeries <- RqCqDFSeries[rqRows,]
    cqDFSeries <- RqCqDFSeries[cqRows,]
    
    # View(rqDFSeries)
    # View(cqDFSeries)
    
    # assign("rqDFSeries", rqDFSeries, pos=1)
    # assign("cqDFSeries", cqDFSeries, pos=1)
    
    # sensors used in the calculation of the test result
    bootstrapSensors2 <- c("Pneumo", "AutoEDA", "Cardio")
    
    theseCQRows <- which(cqDFSeries$sensorName %in% bootstrapSensors2)
    theseRQRows <- which(rqDFSeries$sensorName %in% bootstrapSensors2)
    
    CQScores <- as.numeric(cqDFSeries$bootstrapScore[theseCQRows])
    RQScores <- as.numeric(rqDFSeries$bootstrapScore[theseRQRows])
    
    # check that all CQs and RQs are present in all charts
    if(length(RQScores) != length(CQScores)) {
      if(!isTRUE(forced)) {
        print("questions missing from some charts")
        return(RqCqDFSeries)
      } else {
        # use mean replacement by averaging the z-scores
        # can average z-scorese between sensors
        # determine whether fewer RQs or CQs
        padThese <- ifelse(which.max(c(length(RQScores), length(CQScores))) == 1,
                           "CQs",
                           "RQs" )
        padLength <- max(c(length(RQScores), length(CQScores))) - 
          min(c(length(RQScores), length(CQScores)))
        if(padThese == "RQs") {
          # mean replacement for RQs
          RQScores <- 
            c(RQScores, rep(round(mean(RQScores, na.rm=TRUE), 2), padLength))
        } else {
          # mean replacement for CQs
          CQScores <- c(CQScores, rep(round(mean(CQScores, na.rm=TRUE), 2), padLength))
        }
      }
    }     
    
    # mean replacement for remaining NA values
    CQScores[is.na(CQScores)] <- round(mean(CQScores, na.rm=TRUE), 2)
    RQScores[is.na(RQScores)] <- round(mean(RQScores, na.rm=TRUE), 2)
  
    # bootstrap the CQ - RQ differencee
    # CQRQDiff <- rep(NA, times=numberReSamples)
    # for (l in 1:numberReSamples) {
    #   CQRQDiff[l] <- 
    #     sum( sample(CQScores, size=length(CQScores), replace=FALSE) - 
    #            sample(RQScores, size=length(RQScores), replace=FALSE) )
    # }
    # diffScore <- mean(CQRQDiff, na.rm=TRUE)
    
    
    # calculate the CQ-RQ difference
    diffScore <- ifelse(length(CQScores) == length(RQScores),
                        sum(CQScores - RQScores, na.rm=TRUE),
                        ifelse(isTRUE(forced),
                               sum(mean(CQScores, na.rm=TRUE) -
                                     mean(RQScores, na.rm=TRUE)) * length(RQScores),
                               return(RqCqDFSeries) )
    )
    diffScore <- round(diffScore, 2)
    
    # sum(CQScores)
    # sum(RQScores)
    
    # diffScore is the is the CQ-RQ score 
    # that will be compared to a chance distribution 
    # to obtain a likelihood statistic
    # includes only Pneumo, EDA, and cardio sensors
    
    # print(diffScore)
  
  }
  
  #### bootstrap the parameters for a reference distribution ####
  
  {
    # get the sensor rows from RqCqDFSeries
    # theseRows <- which(RqCqDFSeries$sensorName %in% bootstrapSensors2)
    
    # get the z-scores for the bootstrap
    # scoreVector <- as.numeric(RqCqDFSeries$bootstrapScore[theseRows])
    
    # use the RQScores and CQScores with mean replacement
    scoreVector <- c(CQScores, RQScores)
    
    # numberReSamples <- 3000
    
    # initialize the resample vectors
    # resampleCQs <- rep(NA, length=numberReSamples)
    # resampleRQs <- rep(NA, length=numberReSamples)
    
    resampleDiffs <- rep(NA, length=numberReSamples)
    
    # uses mean repelacement if isTRUE(forced)
    CQLength <- length(CQScores)
    RQLength <- length(RQScores)
    
    # iterate over the number of resamples
    for(k in 1:numberReSamples) { 
      resampleDiffs[k] <- 
        sum(sample(scoreVector, CQLength, replace=TRUE) - 
              sample(scoreVector, RQLength, replace=TRUE), na.rm=TRUE)
    } 
    
    # calculate the bootstrap mean and sd for this exam
    resampleMean <- round(mean(resampleDiffs, na.rm=TRUE), 2)
    resampleSD <- round(sd(resampleDiffs, na.rm=TRUE), 2)
    
    # use these parameters to calculate a likelihood statistic for the diffScore
    
    # summary(resampleDiffs)
    # hist(resampleDiffs)
    
  }
  
  ########## calculate a z value for the test score #########
  
  {
    
    # invert the z value so that p values for DI are high side
    # 202-04-05
    zVal <- round(((diffScore - resampleMean) / resampleSD), 2)
    
    # probability of obtaining a z score 
    # as positive or less positive than the observed score
    pVal <- round(pnorm(-zVal, lower.tail=FALSE), 2)
    
  }
  
  ######### then use the odds form of Bayes theorem #########
  
  # {
  #   
  #   postOdds <- (pVal / (1-pVal)) * (priorProb / (1-priorProb))
  #   
  #   # convert the posterior odds to posterior probability
  #   postP <- postOdds / (1+postOdds)
  #   
  #   # probabilities > .5 can be classified truthful
  #   # probabilities < .5 can be classified deceptive 
  #   
  # }
  
  #### calculate the categorical result using the probability cutpoints ####
  
  {
    
    cutScores <- c(GTDI=bootstrapCutProbD, GTNDI=bootstrapCutProbT)
    # cutScores <- 1 - cutScores
    
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    # posterior probability of deception
    postP <- round(1 - pVal, 2)
    # if(!exists("postP")) { postP <- pVal }
    # totalScore <- pVal
    
    postP <- ifelse( postP > .99, 
                     .99, 
                     ifelse( postP < .01, 
                             .01, 
                             round(postP, 2)
                     )
    )
    
    # call the private function to use the grand total rule
    # use flip=TRUE to invert the probability score and probability cutscores
    GTRResult <- GTRFn(totalScore=(postP), 
                       RQNames=uniqueRQs,
                       cutScores=cutScores,
                       flip=TRUE )
    # using only the postTruthful value 
    # because it is the compliment of postDeceptive
    
    bootstrapCategoricalResult <- GTRResult$testResult
    
    bootstrapQuestionResults <- GTRResult$subtotalResults
    
    resultUsing <- GTRResult$resultUsing 
    
  }
  
  ############################### output ###############################
  
  {
    
    # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
    
    bootstrapMeasurementSensors <- 
      bootstrapSensors[bootstrapSensors %in% c("UPneumo", "LPneumo", "AutoEDA", "Cardio")]
    
    bootstrapMeasurementsDF <- 
      measurementTableFn(RqCqDFSeries=RqCqDFSeries, 
                         useSensors=bootstrapMeasurementSensors,
                         decimals=2,
                         makeDF=makeDF, 
                         saveCSV=saveCSV )
    # View(bootstrapMeasurementsDF)
    
  }
  
  ############ construct a list to hold the PSS result #############
  
  {
    
    outputListName <- 
      paste(examName, seriesName, "bootstrapOutputList", sep="_")
    
    bootstrapOutputList <- list(BSS="Bootstrap Scoring System (Honts & Devitt, 1992)",
                                examName=examName,
                                seriesName=seriesName,
                                bootstrapResult=bootstrapCategoricalResult,
                                bootstrapQuestionResults=bootstrapQuestionResults,
                                bootstrapDecisionRule=bootstrapDecisionRule,
                                bootstrapResultUsing=resultUsing,
                                bootstrapPostProb=postP,
                                bootstrapDiffScore=diffScore,
                                bootstrapQuesetions=uniqueQuestions,
                                bootstrapRQNames=uniqueRQs,
                                bootstrapResampleMean=resampleMean,
                                bootstrapResampleSD=resampleSD,
                                bootstrapCutProbD=bootstrapCutProbD,
                                bootstrapCutProbT=bootstrapCutProbT,
                                bootstrapSensors=bootstrapMeasurementSensors,
                                bootstrapWarning="",
                                bootstrapCQScores=CQScores,
                                bootstrapRQScores=RQScores, 
                                bootstrapMeasurements=bootstrapMeasurementsDF )
    
  }
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, bootstrapOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "bootstrapOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      bootstrapOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  return(RqCqDFSeries)
  
}



