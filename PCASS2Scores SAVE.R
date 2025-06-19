# PCASS Algorithm (2020)
# Raymond Nelson




{
  
  # for the measurementTableFn and scoreSheetFn
  source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/PCASS2_helper_functions.R', echo=FALSE)
  
}




PCASS2ScoresFn <- function (RqCqDFSeries=RqCqDFSeries,
                           PCASS2Sensors=c("AutoEDA", "PLE"),
                           PCASS2DecisionRule="SSR",
                           PCASS2Prior=.5,
                           PCASS2CutOddsT=2,
                           PCASS2CutOddsD=2,
                           numberReSamples=3000,
                           makeDF=TRUE,
                           saveCSV=FALSE,
                           analysisListName=NULL ) {
  # R function to compute the PCASS2 scoring system
  # Raymond Nelson July 4, 2020
  # 
  # 
  # input is a data frame of RQ and CQ measurements for a series (all charts)
  # including all test charts with RQs and CQs
  # and all sensors (upper and lower respiration, electrodermal, cardio)
  #
  # PCASS2DecisionRule is normally "SSR" but can also use "GTR" or "TSR"
  # PCASS2Prior is a decimmal proportion with default = .5
  # PCASS2Cutscores are in the form of posterior odds of deception and truth
  # numberReSamples=3000 sets the number of random selections
  # for the bootstrap null distribution
  # makeDF will save the bootstrap distribution, measurementDF and z-scores 
  # saveCSV will write to .csv the bootstrap distribution, measurementDF and z-scores
  # analysisListName is the name of an object in the .GlobalEnv 
  # where the output list will be saved
  # 
  # output is a list
  #
  ####
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/PCASS2_helper_functions.R', echo=FALSE)
  
  # for the measurementTableFn and scoreSheetFn
  # source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
  
  #### intial set up ####
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # if(seriesName=="3") {
    #   assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    #   stop()
    # }
  
    print("calculate the PCASS2 scores")
    
    analysisListName <- paste(examName, "ANALYSIS", sep="_")
    
    if(!exists("analysisListName")) {
      assign("analysisListName", analysisListName, envir=.GlobalEnv)
    }
    
    # View(RqCqDFSeries)
    
    {
      if(!exists("PCASS2DecisionRule")) PCASS2DecisionRule <- "SSR"
      if(!exists("PCASS2Sensors")) PCASS2Sensors <- c("AutoEDA", "PLE")
      if(!exists("PCASS2Prior")) PCASS2Prior <- .5
      if(!exists("PCASS2CutScoreT")) PCASS2CutOddsT <- 2
      if(!exists("PCASS2CutScoreD")) PCASS2CutOddsD <- 2
      if(!exists("numberReSamples")) numberReSamples <- 3000
      if(!exists("makeDF")) makeDF <- TRUE
      if(!exists("saveCSV")) saveCSV <- FALSE
    }
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    PCASS2Sensors <- c("UPneumo", 
                      "LPneumo", 
                      # "Pneumo", 
                      "AutoEDA", 
                      # "ManualEDA", 
                      # "FC",
                      "Cardio",
                      "PLE" )
    
    PCASS2Sensors <- PCASS2Sensors[PCASS2Sensors %in% uniqueSensors]
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    # PCASS2 can work with data on traditional charts
    # or DLST/DLDT type formats 
    # with multiple RQ and CQ iterations (charts) in a single recording
    
    rqRows <- grep("R", RqCqDFSeries$Label)
    cqRows <- grep("C", RqCqDFSeries$Label)
    
    # use the 'Label' column not 'eventLabel'
    uniqueRQs <- unique(RqCqDFSeries[rqRows,'Label'])
    uniqueCQs <- unique(RqCqDFSeries[cqRows,'Label'])
    
    uniqueQuestions <- unique(RqCqDFSeries$Label)
    
    # RqCqDFSeries$Label <- RqCqDFSeries$eventLabel
    # RqCqDFSeries$chartName <- RqCqDFSeries$chartName[1]
    
  }
  
  ##### check for DLST DLDT and PCASS type charts #####
  
  {
    
    saveQuestionLabels <- RqCqDFSeries$Label
    saveEventLabels <- RqCqDFSeries$eventLabel
    
    saveChartNames <- RqCqDFSeries$chartName
    # saveChartNames <- rep(uniqueCharts[1], length=nrow(RqCqDFSeries))
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
  
  ##### adjust the RqCqDFSeriesDF chartNames for DLST type formats #####
  
  if(isTRUE(DLSTType)) {
    
    # RqCqDFSeries$Label <- RqCqDFSeries$eventLabel
    
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
    
    # call a function to fix the RQ Labels 
    RqCqDFSeries$Label <- 
      fixPCASS2RQsFn(saveQuestionLabels, CQRQLabels)
    
    uniqueQuestions <- unique(RqCqDFSeries$Label)
    uniqueRQs <- unique(RqCqDFSeries[rqRows,'Label'])
    uniqueCQs <- unique(RqCqDFSeries[cqRows,'Label'])
    
  }
  
  ######## check some boundary conditions ########
  
  {
    
    # exit if PCASS2 Sensors are missing
    if(length(which(!(PCASS2Sensors %in% uniqueSensors))) > 2) {
      return(RqCqDFSeries)
    }
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      return(RqCqDFSeries) 
    }
    
    # exit if not at least 2RQs and 2 CQs
    if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) {
      "incorrect number of RQs and CQs"
      return(RqCqDFSeries) 
    }
    
  }
  
  ############# remove small EDA and cardio values ################
  
  {
    
    
    # replace small values with NA
    
    # save the sensor Measurements so they can be reset later
    saveSensorMeasurements <- RqCqDFSeries$sensorMeasurement
    
    # View(RqCqDFSeries)
    
    theseSensors <- c("AutoEDA", "Cardio")
    
    EDARows <- which(RqCqDFSeries$sensorName == "AutoEDA")
    cardioRows <- which(RqCqDFSeries$sensorName == "Cardio")
      
    # theseSensorRows <- 
    #   which(RqCqDFSeries$sensorName %in% theseSensors)
    
    theseSensorRows <- c(EDARows, cardioRows)
    
    theseSensorMeasurements <- 
      RqCqDFSeries$sensorMeasurement[theseSensorRows]
    
    theseSensorRowsB <- 
      theseSensorRows[which(theseSensorMeasurements < 20)]
    
    RqCqDFSeries$sensorMeasurement[theseSensorRowsB] <- NA
    
    # calculate some values for mean replacement
    # EDAMean <- mean(RqCqDFSeries$sensorMeasurement[EDARows], na.rm=TRUE)
    # cardioMean <- mean(RqCqDFSeries$sensorMeasurement[cardioRows], na.rm=TRUE)
    
    # RqCqDFSeries$sensorMeasurement[EDARows]
    # RqCqDFSeries$sensorMeasurement[cardioRows]
    
  }
  
  ###### iterate onthe charts and standardize the sensor data ######
  
  {
    
    # load the sdp() function for the population standard deviation
    # source('~/Dropbox/R/NCCA_ASCII_Parse/PCASS2_helper_functions.R', echo=FALSE)
    
    # initialize some vectors
    sensorMeans <- rep(NA, length=length(PCASS2Sensors))
    sensorSDs <- rep(NA, length=length(PCASS2Sensors))
    
    # this standardization is for each sensor, within charts
    # could possibly standardize between each chart
    # standardizing between charts would preserve 
    # between chart differences in sensor response level
    
    for(i in 1:length(uniqueCharts)) {
      
      chartName <- uniqueCharts[i]
      
      chartRows <- which(RqCqDFSeries$chartName == chartName)
      RqCqDFChart <- RqCqDFSeries[chartRows,]
      
      # calculate the sensor means and standard deviations
      for(i in 1:length(PCASS2Sensors)) {
        # get the rows for each i sensor
        theseRows <- which(RqCqDFChart$sensorName == PCASS2Sensors[i])
        sensorMeans[i] <- 
          mean(RqCqDFChart$sensorMeasurement[theseRows], na.rm=TRUE)
        # call the helper function for the  population standard deviation
        sensorSDs[i] <- 
          sdp(RqCqDFChart$sensorMeasurement[theseRows])
      }
      
      # calculate the z scores so all sensor data have a common metric
      for(j in 1:length(sensorMeans)) {
        theseSensorRows <- 
          which(RqCqDFChart$sensorName == PCASS2Sensors[j])
        RqCqDFChart$PCASS2Score[theseSensorRows] <- 
          round((RqCqDFChart$sensorMeasurement[theseSensorRows] - 
                   sensorMeans[j]) / sensorSDs[j], 2)
      }
      
      # pass the chartDF back to the seriesDF
      RqCqDFSeries[chartRows,] <- RqCqDFChart
      
    } # end i loop over charts
    
    # RqCqDFSeries$PCASS2Score[RqCqDFSeries$ sensorName %in% PCASS2Sensors]
    
    # all sensor measurements are now have a common z-score metric
    # and can compared and aggregated than raw measurements
    
    # z-scores are stored as a character vector in the PCASS2Score column
    
    ##
    
    thesePneumoRows <- 
      which(RqCqDFSeries$sensorName %in% c("UPneumo", "LPneumo"))
    # RqCqDFSeries$PCASS2Score[thesePneumoRows]
    theseEDARows <- which(RqCqDFSeries$sensorName == "AutoEDA")
    # RqCqDFSeries$PCASS2Score[theseEDARows]
    theseCardioRows <- which(RqCqDFSeries$sensorName == "Cardio")
    # RqCqDFSeries$PCASS2Score[theseCardioRows]
    thesePLERows <- which(RqCqDFSeries$sensorName == "PLE")
    # RqCqDFSeries$PCASS2Score[thesePLERows]
    
    ## invert the sign of the pnuemo measurements ##
    
    {
      
      # smaller pneumo values are greater changes in physiology
      # this inversion assures that signed log scores
      # are associated with deception and truth-telling
      # in ways that are familiar to polygraph examiners
      # where - scores are associated with deception
      # and + scores are associated with truth-telling
      
      thesePneumoRows <- which(RqCqDFSeries$sensorName %in% 
                                 c("UPneumo", "LPneumo"))
      
      # use as.numeric because PCASS2Score column is a character vector
      RqCqDFSeries$PCASS2Score[thesePneumoRows] <- 
        as.numeric(RqCqDFSeries$PCASS2Score[thesePneumoRows]) * -1
      
    }
    
    ## average the z scores for the upper and lower pneumo sensors ##
    
    {
      
      UPneumoRows <- which(RqCqDFSeries$sensorName == "UPneumo")
      LPneumoRows <- which(RqCqDFSeries$sensorName == "LPneumo")
      
      PneumoRows <- which(RqCqDFSeries$sensorName == "Pneumo")
      
      RqCqDFSeries$PCASS2Score[PneumoRows] <- 
        round((as.numeric(RqCqDFSeries$PCASS2Score[UPneumoRows]) + 
                 as.numeric(RqCqDFSeries$PCASS2Score[LPneumoRows])) / 2, 2)
      # View(RqCqDFSeries)
      
    }
    
    # all PCASS2Score values are now z-scores
    # RqCqDFSeries$PCASS2Score[RqCqDFSeries$sensorName %in% PCASS2Sensors]
    
  }
  
  ########## constraints ###########
  
  {
    
    # PLE_RC_Fn <- function(RQValue, CQValue, PLEConstraint)
    
    qnorm(.55)
    pnorm(.45)
    
    qnorm(.75)
    qnorm(.25)
    
  }
  
  ########## remove extreme z-values as outliers ###########
  
  {

    #   theseSensorRows <- which(RqCqDFSeries$sensorName %in% PCASS2Sensors)
    #   
    #   PCASS2Scores <- 
    #     RqCqDFSeries$PCASS2Score[theseSensorRows]
    #   PCASS2Scores <- as.numeric(PCASS2Scores)
    #   
    #   theseExtremeRows <- which(PCASS2Scores >= 6)
    #   PCASS2Scores

  }
  
  ############ calculate the CQ - RQ difference scores #############
  
  {
    
    {
      
      ## initialize separate vectors for RQ and CQ scores ##
      
      # sensors used in the calculation of the test result
      PCASS2Sensors2 <- c("Pneumo", "AutoEDA", "Cardio", "PLE")
      
      if(!isTRUE(includePLEScores)) {
        PCASS2Sensors2 <- c("Pneumo", "AutoEDA", "Cardio")
      }
      
      PCASS2Sensors2 <- PCASS2Sensors2[PCASS2Sensors2 %in% unique(RqCqDFSeries$sensorName)]
      
      # use grepl for logical ouput
      rqRows <- grepl("R", RqCqDFSeries$Label)
      cqRows <- grepl("C", RqCqDFSeries$Label)
      
      PCASS2SensorRows <- 
        RqCqDFSeries$sensorName %in% PCASS2Sensors2
      
      # get the row numbers for RQs and PCASS2Sensors2
      rqRows <- which(rqRows & PCASS2SensorRows)
      cqRows <- which(cqRows & PCASS2SensorRows)
      
      # need to separate the RQs and CQs here
      # after aggregating the two pneumo sensors
      # and after standardization
      rqDFSeries <- RqCqDFSeries[rqRows,]
      cqDFSeries <- RqCqDFSeries[cqRows,]
      
      # View(rqDFSeries)
      # View(cqDFSeries)
      
      # rqDFSeries$PCASS2Score
      # cqDFSeries$PCASS2Score
      
      # assign("rqDFSeries", rqDFSeries, pos=1)
      # assign("cqDFSeries", cqDFSeries, pos=1)
      
      # will select all rows because sensors were already selected
      theseCQRows <- which(cqDFSeries$sensorName %in% PCASS2Sensors2)
      theseRQRows <- which(rqDFSeries$sensorName %in% PCASS2Sensors2)
      
      CQScores <- as.numeric(cqDFSeries$PCASS2Score) # [theseCQRows])
      RQScores <- as.numeric(rqDFSeries$PCASS2Score) # [theseRQRows])
      
      # Aug 14, 2020
      # transform the z-scores to decimal probabilities
      # to avoid problems with signs 
      CQScores <- pnorm(CQScores)
      RQScores <- pnorm(RQScores)
        
      # fix NA values with mean replacement
      CQScores[which(is.na(CQScores))] <- mean(CQScores, na.rm=TRUE)
      RQScores[which(is.na(RQScores))] <- mean(RQScores, na.rm=TRUE)
      
      # Aug 13, 2020
      # submit the scores to the data frames with mean replacement
      cqDFSeries$PCASS2Score <- CQScores
      rqDFSeries$PCASS2Score <- RQScores
      
      # submit the RQ and CQ scores to the RqCqDFSeries
      
      RqCqDFSeries$PCASS2Score[rqRows] <- RQScores
      RqCqDFSeries$PCASS2Score[cqRows] <- CQScores
      
      # not necessary
      names(CQScores) <- cqDFSeries$sensorName[theseCQRows]
      names(RQScores) <- rqDFSeries$sensorName[theseRQRows]
      
    }
    
    {
      
      ## initialize some vectors to hold the CQ-RQ difference scores ##
      
      # the number of unique RQs will be incorrect for PCASS/DLST
      # unless the question labels are corrected
      
      # for RQ subtotals
      CQRQDiffScoresST <- rep(0, times=length(uniqueRQs))
      names(CQRQDiffScoresST) <- uniqueRQs
      
      # for the grand total of all RQs
      CQRQDiffScoreGT <- 0 
      
    }
    
    {
      
      ## initialize a matrix for the CQ-RQ differences ##
      
      # for the sensor difference scores
      # used to calculate the sensor contribution
      sensorDiffScoreMtx <- 
        matrix(rep(0, length=length(uniqueRQs) * length(PCASS2Sensors2)),
               ncol=length(uniqueRQs))
      colnames(sensorDiffScoreMtx) <- uniqueRQs
      row.names(sensorDiffScoreMtx) <- PCASS2Sensors2
      
    }
    
    ## iterate over the RQs and sensors to get the CQ-RQ scores  ##
    
    # for each RQ 
    # for each sensor
    # calculate the CQMean - RQ differences 
    # then sum the sensor scores for an RQ difference score
    # total scoore is the 
    # sum all difference scores for all RQs and all sensors
    
    i=1
    j=1
    for(i in 1:length(uniqueRQs)) {
      
      thisRQ <- uniqueRQs[i]
      # get the rows for all sensors for this RQ
      theseRQRows <- which(rqDFSeries$Label == thisRQ)
      
      for(j in 1:length(PCASS2Sensors2)) {
        
        # start with the RQ vals for this sensor
        thisSensor <- PCASS2Sensors2[j]
        
        # get the values for this RQ for all charts for this sensor 
        # first get the sensor rows
        theseRQSensorRows <- 
          which(rqDFSeries$sensorName == thisSensor)
        
        # then reduce the sensor rows to this RQ
        theseRQSensorRows <- 
          theseRQRows[which(theseRQRows %in% theseRQSensorRows)]
        
        # average the sensor score for all iterations (charts) of this RQ
        RQSensorVal <- 
          mean(as.numeric(rqDFSeries$PCASS2Score[theseRQSensorRows]), na.rm=TRUE)
        
        # next get the CQ sensor values for all charts for all CQs
        theseCQSensorRows <- which(cqDFSeries$sensorName == thisSensor)
        # calculate the sensor mean for all CQs and all charts
        CQSensorVal <- 
          mean(as.numeric(cqDFSeries$PCASS2Score[theseCQSensorRows]), na.rm=TRUE)
        
        # each RQ is compared to the aggregate of all CQs
        # RQCQSensorDiffScore <- CQSensorVal - RQSensorVal
        
        # Aug 15, 2020
        # use the CQ/CQ ratio instead of the CQ-RQ difference
        RQCQSensorDiffScore <- log(CQSensorVal / RQSensorVal)
        
        # add the sensor measurement to the i difference score and total score
        # subtotal and total scors are the sum of sensor CQ-RQ z scores 
        if(!is.na(RQCQSensorDiffScore)) {
          # first the subtotal  diff scores
          # commented out 7-16-2020
          # CQRQDiffScoresST[i] <- CQRQDiffScoresST[i] + RQCQSensorDiffScoore
          # then the grand total diff scores
          # CQRQDiffScoreGT <- CQRQDiffScoreGT + RQCQSensorDiffScoore
        }
        
        # commented out 7-16-2020
        # CQRQDiffScoresST <- round(CQRQDiffScoresST, 3)
        
        # CQRQDiffScoresST are the sum of CQ-RQ sensor z-scores for each RQ
        # CQRQDiffScoreGT is the same as summing the subtotal diff scores
        
        # save the CQ-RQ diff score for each sensor and each RQ
        sensorDiffScoreMtx[j,i] <- RQCQSensorDiffScore
        # used to calculate the sensor contribution
        
      } # end loop j over sensors
      
    } # end loop i over RQs
    
    # invert the sign of the pneumo and PLE sensors
    fixTheseRows <- which(row.names(sensorDiffScoreMtx) %in% c("Pneumo", "PLE"))
    sensorDiffScoreMtx[fixTheseRows,] <- 
      sensorDiffScoreMtx[fixTheseRows,] * -1
    
    
    # replace the grand total sum with the grand mean of RQs
    # CQRQDiffScoreGT <- mean(CQRQDiffScoresST)
    
    # stop if error
    # if(round(sum(CQRQDiffScoresST),3) != 
    #    round(CQRQDiffScoreGT, 3)) stop()
    
    # these will be the same
    print(round(sensorDiffScoreMtx, 8))
    print(round(colMeans(sensorDiffScoreMtx, na.rm = TRUE), 8))
    # print(round(mean(colMeans(sensorDiffScoreMtx, na.rm = TRUE), na.rm=TRUE), 3))
    
    # abs(rowMeans(sensorDiffScoreMtx, na.rm=TRUE)) / sum(abs(rowMeans(sensorDiffScoreMtx, na.rm=TRUE)))
    # same result as when using rowSums
    
    print(round(mean(colMeans(sensorDiffScoreMtx, na.rm = TRUE), na.rm=TRUE), 8))
    
    # print(CQRQDiffScoresST)
    # print(mean(CQRQDiffScoresST))
    # print(CQRQDiffScoreGT)
    
    # these will be the same
    # print(rowMeans(sensorDiffScoreMtx, na.rm=TRUE))
    # print(sum(rowMeans(sensorDiffScoreMtx, na.rm=TRUE)))
    # print(rowSums(sensorDiffScoreMtx, na.rm=TRUE))
    # print(sum(rowSums(sensorDiffScoreMtx, na.rm=TRUE)))
    
    # diff scores are then subject to a likelihood function
    # calculated as an empirical bootstrap of the data for each case
    # under the null hypothesis to the analytic theory of the polygraph.
    # the analytic theory says that greater changes in physiological activity 
    # are loaded at different types of test stimuli as a s function
    # of deception or truth-telling in response to relevant target stimuli.
    # null hypothesis to this theory says that scores are not loaded 
    # in any systematic way and can be characterized as random.
    # not loaded systematically for sensor, RQ, or chart
    
    # could possibly use a permutation instead
    # could also use an empirical distribution,
    # but this would require high quality data
    # high quality means representative and generalizable
    # and randomly selected from the population
    # as well as being conducted correctly according to standards
    # with data of satisfactory quality and stability
    
    CQRQDiffScoresST <- round(colMeans(sensorDiffScoreMtx, na.rm = TRUE), 8)
    # names(CQRQDiffScoresST) <- uniqueRQs
    
    # replace the grand total with the mean of RQs
    CQRQDiffScoreGT <- round(mean(CQRQDiffScoresST, na.rm=TRUE), 8)
    # same as mean(sensorDiffScoreMtx)
    
    # print(CQRQDiffScoreGT)
    
    # CQRQDiffScoreGT will be used as the grand total score
    # CQRQDiffScoresST are the subtotal scores
    
  }
  
  #### calculate the sensor contribution to the ST and GT scores ####
  
  {
    
    # CQRQDiffScoresST
    # CQRQDiffScoreGT
    
    # sensorContributions <-
    #   (rowSums(sensorDiffScoreMtx, na.rm=TRUE)) / CQRQDiffScoreGT
    # sensorContributions <- sensorContributions / sum(sensorContributions)
    
    sensorContributions <-
      abs(rowMeans(sensorDiffScoreMtx, na.rm=TRUE)) / sum(abs(rowMeans(sensorDiffScoreMtx, na.rm=TRUE)), na.rm=TRUE)
    
    sensorContributions <- round(sensorContributions, 6)

    sensorContributions[which(is.na(sensorContributions))] <- 0
    
    #### correlation method ####
    
    # sensorContributions <- rep(0, times=nrow(sensorDiffScoreMtx))
    # 
    # for(i in 1:nrow(sensorDiffScoreMtx)) {
    #   sensorContributions[i] <- 
    #     cor(sensorDiffScoreMtx[i,], colMeans(sensorDiffScoreMtx, na.rm=TRUE))
    # }
    # 
    # sensorContributions <- sensorContributions / sum(sensorContributions)
    
    print("sensor contributions")
    print(sensorContributions)
    # will be the same for both ST and GT
    
  }
  
  ##### bootstrap the parameters for a reference distribution #####
  
  {
    
    # load the PCASS2SamplerFn()
    # source('~/Dropbox/R/NCCA_ASCII_Parse/PCASS2_helper_functions.R', echo=FALSE)
    
    # bootstrap a random distribution of scores for RQ subtotals 
    # RQ subtotals are the sum of CQ-RQ difference scores for all sensors
    # grand total is the sum of RQ subtotals or
    # the sum of all CQ-RQ difference scores for all sensors and all RQs
    
    # bootstrap by
    # pooling all CQs and RQs into a single vector for each sensor
    # then for each sensor
    # randomly select pairs and randomly assign as CQ and RQ
    
    # sensors can be weighted for aggregation
    # using the sensor contribution values for each series
    
    # similar to the test statistic
    # CQ values are aggregated for all CQs and all charts for each sensor
    # RQ vales are aggregaated for each RQ and all chards for each sensor
    # the n of RQ values will be less than the n of CQ values
    # using means, not sums, makes the comparison possible
    
    # test score uses sum of individual RQ sensor scores for all charts
    # and the sum of all CQ sensor z scores for all CQs and all charts
    
    # bootstrap distribution needs to work at the series level 
    
    # this likelihood function should work OK with 1 to 5 charts
    # including PCASS and DLST/DLDT type exams 
    # for which all "charts" are in 1 recording
    # for this reason diff scores and null distribution of diff scores
    # are the sum of between schart means for each sensor for each RQ
    
    # CQMean-RQ difference scores are summed for all sensors for each RQ
    # RQ sums are then summed again for the grand total score
    
    # calculate the CQ - RQ difference and add it to the bootstrap
    
    # the bootstrap distribution also 
    # needs to work with unequal numbers of RQs and CQs
    # samples must be drawn from the RQ and CQ z values with equal probaability
    
    # the null hypothesis distribution says that 
    # there is no systematic loading
    # of responses for RQs and CQs, or for charts
    # and for this reason the loading of responses 
    # can be characterized as random
    
    # also, the null hypothesis of random loading
    # says that differences sensors are meaningless
    # so that sensors can also be characterized as random
    # and so bootstrap selections can be made randomly among the sensors
    # because the sensor z-scores have a common scale
    
    # the number of RQs, number of CQs and number of sensors is still important
    # because the number of sensors will influence the RQ scores
    # and the number of RQs will influence the total score
    # and the number of CQs may differ from the number of RQs
    
    # the number of charts makes no difference
    # because CQ-RQ difference scores are aggregated between charts
    # for each sensor
    # before aggregation of scores between sensors and between RQs
    
    ## slice the RQ and CQ scores to sample from ##
    
    {
      
      selectColumns <-c("chartName", "Label", "sensorName", "PCASS2Score")
      
      # initialize a vector of all RQ and CQ scores to sample from
      theseRows <- which(RqCqDFSeries$sensorName %in% PCASS2Sensors2)
      theseCols <- which(names(RqCqDFSeries) %in% selectColumns)
      PCASS2ScoresDF <-  RqCqDFSeries[theseRows, theseCols]
      # this data frame contains the CQ-RQ scores for PCASS2Sensors2
      
      # View(PCASS2ScoresDF)
      
      PCASS2Scores <- as.numeric(PCASS2ScoresDF$PCASS2Score)
      # could sample from this single vector for all charts, all RQs and all sensors
      # but this will require equal number of RQs and CQs
      # or else the bootstrap sampling distribution will be loaded or biased
      
      # mean replacement for NA values
      PCASS2Scores[is.na(PCASS2Scores)] <- 
        round(mean(PCASS2Scores, na.rm=TRUE), 8)
      # could also use zero replacement for NA
      
      # names(PCASS2Scores) <- PCASS2ScoresDF$Label
      
      # get the sensor names to help slice the data for each sensor
      names(PCASS2Scores) <- PCASS2ScoresDF$sensorName
      
      # separate the RQs and CQs
      # necessary in case of un-equal number of RQs and CQs
      PCASS2ScoresRQ <- PCASS2Scores[grepl("R", PCASS2ScoresDF$Label)]
      PCASS2ScoresCQ <- PCASS2Scores[grepl("C", PCASS2ScoresDF$Label)]
      # the bootstrap parameters are sampled from these 2 vectors
      
      # mean(PCASS2ScoresRQ)
      # mean(PCASS2ScoresCQ)
      
      # PCASS2 may become unstable with uplanned question repetitions
      # but can tollerate some missing or artifacted data
      
      # assign("PCASS2ScoresCQ", PCASS2ScoresCQ, envir=.GlobalEnv)
      # assign("PCASS2ScoresRQ", PCASS2ScoresRQ, envir = .GlobalEnv)
      # stop()
      
    }
    
    ## slice the RQs and CQs for each sensor ##
    
    {
      
      # this way the sampler can work with each sensor separately
      # and the sensors can be weighted for aggregation
      
      RQSensorNames <- unique(names(PCASS2ScoresRQ))
      CQSensorNames <- unique(names(PCASS2ScoresCQ))
      
      # initialize some names for the sensor RQ vectors
      RQScoreVectorNames <- paste0("PCASS2ScoresRQ", RQSensorNames)
      CQScoreVectorNames <- paste0("PCASS2ScoresCQ", CQSensorNames)
      
      
      o=1
      for(o in 1:length(RQScoreVectorNames)) {
        assign(RQScoreVectorNames[o], 
               PCASS2ScoresRQ[names(PCASS2ScoresRQ) == RQSensorNames[o]])
      }
      
      for(o in 1:length(CQScoreVectorNames)) {
        assign(CQScoreVectorNames[o], 
               PCASS2ScoresCQ[names(PCASS2ScoresCQ) == CQSensorNames[o]])
      }
      
      # PCASS2ScoresRQ 
      # PCASS2ScoresCQ
      
      # there are now separate vectors of z-scores 
      # for RQs and CQs for each sensor
      
      # > CQScoreVectorNames
      # [1] "PCASS2ScoresCQPneumo"  "PCASS2ScoresCQAutoEDA"
      # [3] "PCASS2ScoresCQCardio"  "PCASS2ScoresCQPLE"    
      
      # > RQScoreVectorNames
      # [1] "PCASS2ScoresRQPneumo"  "PCASS2ScoresRQAutoEDA"
      # [3] "PCASS2ScoresRQCardio"  "PCASS2ScoresRQPLE"    
      
    }
    
    ## initialize the bootstrap distribution data frame ##
    
    {
      
      # initialize some names for the random selection scores
      RNames <- paste0("R", c(1:length(uniqueRQs)))
      CNames <- paste0("C", c(1:length(uniqueCQs)))
      
      # initialize a data frame for the bootstrap distribution
      bootstrapDF <- matrix(rep(0, numberReSamples * (length(RNames) + 2)), 
                            nrow=numberReSamples)
      bootstrapDF <- as.data.frame(bootstrapDF)
      names(bootstrapDF) <- c(paste0(RNames, "Diff"), "RQDiffMean", "RQDiffSD")
      # View(bootstrapDF)
      
    }
    
    ## initialize some bootstrap vectors for the CQ-RQ difference scores ##
    
    {
      
      RScores <- rep(0, times=length(RNames))
      names(RScores) <- uniqueRQs
      CScores <- rep(0, times=length(RNames)) 
      # use length(RNames) for CScores 
      # in case the number of RQs and CQs differs unexpectedly
      # due to missed or skipped questions or ad hoc insertions
      
      RQSensorSamples <- rep(0, times=length(PCASS2Sensors2))
      CQSensorSamples <- rep(0, times=length(PCASS2Sensors2))
      
      # the bootstrap will iterate on these
      # to populate the bootstrap data frame 
      
    }
    
    ## iterate on the bootstrap data frame to calculate the distribution ##
    
    # iterate over the bootstrap samples
    k=1
    for(k in 1:nrow(bootstrapDF)) {
      
      # iterate over the RQs 
      l=1
      for(l in 1:length(RScores)) {
        
        # iterate over the sensors and sample the data for RQs and CQs
        m=1
        for(m in 1:length(PCASS2Sensors2)) {
          
          # call the private sampler function
          # the seed vectors for RQs and CQs include all sensors and charts
          
          # the null hypothesis is that there is no systematic effect
          # for any of the sensors or charts
          # they are all random and interchangeable
          # the sampler function will select randomly from the RQ or CQ scores
          
          ## select the RQ and CQ z-score vectors for each sensor
          theseRQSensorScores <- get(RQScoreVectorNames[m])
          theseCQSensorScores <- get(CQScoreVectorNames[m])
          
          # first the RQ value for this sensor
          RQSensorSamples[m] <- 
            # PCASS2SamplerFn(n=,
            PCASS2SamplerFn(n=length(uniqueCharts),
                            input1=theseRQSensorScores, 
                            input2=theseCQSensorScores, 
                            cutProp=.5 )
          # number of RQ samples is the number of charts
          
          # then get the CQ value for this sensor
          CQSensorSamples[m] <- 
            # PCASS2SamplerFn(n=,
            # PCASS2SamplerFn(n=length(uniqueCharts),
            PCASS2SamplerFn(n=(length(uniqueCharts) * length(CNames)),
                            input1=theseRQSensorScores, 
                            input2=theseCQSensorScores, 
                            cutProp=.5 )
          
          # number of CQ samples will differ from the number of RQ samples
          # number of CQ samples is the number of charts * number of CQs
          
          # RQ and CQ aggregates the same scale
          # regardless of the number of RQs and CQs
          # and regardless of the number of charts
          # because the PCASS2ScoresRQ and PCASS2ScoresCQ are z scores
          
        } # end m loop for sensors for this RQ and this iteration of the boot
        
        # sum the sensor means for RQs and CQs 
        # to an RScore and CScore for this RQ
        # RScores[l] <- sum(RQSensorSamples)
        # CScores[l] <- sum(CQSensorSamples)
        
        # 1/23/2021 trying the unweighed solution again
        # RScores[l] <- mean(RQSensorSamples,  na.rm=TRUE)
        # CScores[l] <- mean(CQSensorSamples,  na.rm=TRUE)

        # use weighted mean instead
        # 7-16-2020
        RScores[l] <- weighted.mean(RQSensorSamples, sensorContributions)
        CScores[l] <- weighted.mean(CQSensorSamples, sensorContributions)
        
      } # end l loop for RQs
      
      # get the diff score for all RQs for this bootstrap iteration
      diffScores <- round(CScores - RScores, 8)

      # calculate the mean of summed CQ-RQ sensor difference scores
      meanDiffScore <- round(mean(diffScores, na.rm=TRUE), 8)
      
      # calculate the mean and stdev for the difference scores
      sdpDiffScore <-  round(sdp(x=diffScores), 8)
    
      # add the diff scores, mean and stdev to the bootstrap data frame
      bootstrapDF[k,] <- c(diffScores, meanDiffScore, sdpDiffScore)
      # View(bootstrapDF)
      
    } # end k loop for the bootstrap distribution 
    
    ## aggregate the data and calculate the bootstrap parameters ##
    
    {
      
      # calculate the column means for the CQ - RQ difference scores
      bootstrapMeans <- round(colMeans(bootstrapDF), 8)

      # print(bootstrapMeans)
      
      # # calculate the column standard devs 
      # # aggregate the RQ scores
      # print(mean(colMeans(bootstrapDF)[1:length(uniqueRQs)]))
      # print(mean(colSDs(bootstrapDF)[1:length(uniqueRQs)]))
      
    }
    
  }
  
  #### calculate a z value and Bayes Factor for the total score ####
  
  {    
    
    # z-scores for RQs and the total scores to this point
    # are in-test standard scores for each sensor and each chart
    
    # difference scores for the RQs and total scores are
    # CQ -  RQ
    # negative diff scores are associated with deception
    # positive diff scores are associated with truth-telling
    
    # after this point z-scores are the  diff scores standardized to
    # the bootstrap likelihood function under the null hypothesis
    
    zGT <- 
      (CQRQDiffScoreGT - bootstrapMeans['RQDiffMean']) / bootstrapMeans['RQDiffSD']
    
    names(zGT) <- NULL
    
    # for testing
    # zGT <- zGT * -1
    
    # transform to decimal probability and round
    pGT <- ifelse( pnorm(zGT) > .999, 
                   .999, 
                   ifelse( pnorm(zGT) < .001, 
                           .001, 
                           round(pnorm(zGT), 8) ) )
    # need to keep 8 decimals so that the transformations are stable
    
    PCASS2ZScoreGT <- qnorm(pGT) 
    # this should equal zGT
    
    # check it again
    PCASS2PScoreGT <- ifelse( pnorm(PCASS2ZScoreGT) > .999, 
                             .999, 
                             ifelse( pnorm(PCASS2ZScoreGT) < .001, 
                                     .001, 
                                     round(pnorm(PCASS2ZScoreGT), 8) ) )
    # this should equal pGT
      
    # set the PCASS2PScore to the upper tail
    PCASS2PScoreGTUp <-  ( (PCASS2PScoreGT / (1-PCASS2PScoreGT))^-sign(zGT) + 1 )^-1
    
    # # PCASS2Statistic is the posterior odds to 1 under an equal prior
    # PCASS2StatisticGT <- round(PCASS2PScoreGTUp, 8) / (1-round(PCASS2PScoreGTUp, 8))
    
    ## use the odds form of Bayes theorem with the total score ##
    
    # use the upper-tail p-value
    # PCASS2Prior is a decimal probability of deception
    postOddsGT <- (PCASS2PScoreGTUp / (1-PCASS2PScoreGTUp)) * (PCASS2Prior / (1-PCASS2Prior))
    # postOddsGT will be the same as BayesFactorGT when the prior odds are 1
    
    # PCCASS posterior odds are the prior odds conditioned on the test data
    
    # convert the posterior odds to posterior probability
    postPGT <- round(postOddsGT / (1+postOddsGT), 8)
    # postPGT will be the same as PCASS2PScoreGT when the prior odds are 1
    
    # transform the posterior probabilty to a posterior z score
    postZGT <- round(qnorm(postPGT) * sign(zGT), 8)
    # submit this value to the decision rules
    
    ## calculate the Bayes Factor for the total score ##
    
    BayesFactorGT <- (PCASS2PScoreGTUp / (1-PCASS2PScoreGTUp)) * (.5 /  (1-.5))
    # BF is the ratio of posterior odds and prior odds 
    # and will be equivalent to the posterior odds  under the equal prior
    
  }
  
  #### calculate z values and Bayes Factors for RQ subtotals ####
  
  {    
    
    # negative PCASS2 z scores are associated with deception
    # positive PCASS2 z scores are associated with truth-telling
    
    # select the lowest subtotal z score to calculate the SSR result
    
    zST <- (CQRQDiffScoresST - bootstrapMeans['RQDiffMean']) / bootstrapMeans['RQDiffSD']
    
    # names(zST) <- RNames
    #  names(zST) <- uniqueRQs
    
    # use only to invert the result for testing
    # zST <- zST * -1
    
    # the min subtotal will drive the test result
    zSTMin <- zST[which.min(zST)]
    
    # use only to invert the result for testing
    # zSTMin <- zSTMin * -1
    
    # names(zSTMin) <- uniqueRQs[which.min(zST)]
    
    # transform to decimal probability and round
    pST <- rep(NA, times=length(zST))
    o=1
    for(o in 1:length(zST)) {
      pST[o] <- ifelse( pnorm(zST[o]) > .999, 
                     .999, 
                     ifelse( pnorm(zST[o]) < .001, 
                             .001, 
                             round(pnorm(zST[o]), 8) ) )
    }
    # need to keep 8 decimals so that the transformations are stable
    
    # get the probability score for the lowest subtotal
    pSTMin <- ifelse( pnorm(zSTMin) > .999, 
                      .999, 
                      ifelse( pnorm(zSTMin) < .001, 
                              .001, 
                              round(pnorm(zSTMin), 8) ) )
    
    PCASS2ZScoresST <- qnorm(pST)
    # this should equal zST
    # except that extreme values will be truncated
    # to .99 or 2.326 standard deviations
    
    PCASS2ZScoreSTMin <- qnorm(pSTMin)
    # will be equal to zSTMin
    # except that extreme values will be truncated
    # to .99 or 2.326 standard deviations
    
    # check it again
    PCASS2PScoresST <- rep(NA, times=length(zST))
    o=1
    for(o in 1:length(PCASS2PScoresST)) {
      PCASS2PScoresST[o] <- ifelse( pnorm(PCASS2ZScoresST[o]) > .999, 
                                   .999, 
                                   ifelse( pnorm(PCASS2ZScoresST[o]) < .001, 
                                           .001, 
                                           round(pnorm(PCASS2ZScoresST[o]), 8) ) )
      # keep 8 decimals
    }
    # PCASS2PScoresST should equal pST
    
    PCASS2PScoreSTMin <- round(pnorm(PCASS2ZScoreSTMin), 8)
    # will be equal to pSTMin
    
    # set the min PCASS2PScore to the upper tail
    PCASS2PScoreSTMinUp <-  
      ( (PCASS2PScoreSTMin / (1-PCASS2PScoreSTMin))^-sign(zSTMin) + 1 )^-1
    
    # # PCASS2Statistic is the posterior odds to 1 under an equal prior
    # PCASS2StatisticST <- round(PCASS2PScoreSTMinUp, 8) / (1-round(PCASS2PScoreSTMinUp, 8))
    
    ## use the odds form of Bayes theorem with the min subtotal ##
    
    # use the upper-tail p-value to make the posterior odds  
    # PCASS2Prior is a decimal probability of deception
    postOddsSTMinUp <- 
      (PCASS2PScoreSTMinUp / (1-PCASS2PScoreSTMinUp)) * (PCASS2Prior / (1-PCASS2Prior))
    # postOddsGT will be the same as BayesFactorGT when the prior odds are 1
    
    # PCCASS posterior odds are the prior odds conditioned on the test data
    
    # convert the posterior odds to posterior probability
    postPSTMinUp <- postOddsSTMinUp / (1+postOddsSTMinUp)
    # postPST will be the same as PCASS2PScoreSTMinUp when the prior odds are 1
    
    # transform the posterior subtotal probabilty to a posterior z score
    postZSTMin <- qnorm(postPSTMinUp) * sign(zSTMin)
    # this will be the same  as zSTMin  and PCASS2ZScoreSTMin
    # when the prior is 1 to 1
    
    # calculate the posterior odds for all subtotals 
    # without statistical correction
    # use the odds form of Bayes Theorem as described by Murphy 1987
    postOddsST <- (PCASS2PScoresST / (1-PCASS2PScoresST)) * (PCASS2Prior / (1-PCASS2Prior))
    # prior will make no difference when the  prior = .5 or 1 to 1
    postPST <- round(postOddsST / (1+postOddsST), 8)
    postZST <- round(abs(qnorm(postPST)) * sign(zST), 8)
    # sign() is vectorized
    # submit the postZST to the decision rules
    
    ## calculate the Bayes Factor for the min subtotal ##
    
    # the upper tail min posterior probability to get the
    # BF for the min subtotal here is with no statistical correction
    BayesFactorST <- (PCASS2PScoreSTMinUp / (1-PCASS2PScoreSTMinUp)) * (.5 /  (1-.5))
    # BF is the ratio of posterior odds and prior odds 
    # and will be equivalent to the posterior odds  under the equal prior
    
    # calculate a statistically corrected (Sidak) Bayes Factor
    # different inversions of Sidak 
    # depending on the sign of the min subtotal z score
    # call a function to get the correction
    BayesFactorSTc <- BFOddsSidakFn(BF=BayesFactorST, 
                                    signVal=sign(PCASS2ZScoreSTMin),
                                    n=length(uniqueRQs) )
    
    # reduce the BF to 99 if necessary
    if(BayesFactorSTc > 99) BayesFactorSTc <- 99
    
  }
  
  ###### calculate the cut odds using the Clopper-Pearon method ######
  
  {
    
    # Jan 23, 2021
    
    CPCutProb <- 
      (PCASS2Prior / (1-PCASS2Prior))/ (1 + PCASS2Prior / (1-PCASS2Prior))
    
    # PCAT n = 8 for subtotals
    # PCAT n = 16 for grand totals
    
    CPCutProb <- 2 / 3
    
    llodds <- 1
    
    while(llodds <= 1) {
      
      llodds <- clopperPearsonFn(p=CPCutProb, 
                      n=8, 
                      a=.2, 
                      odds=TRUE )[1]
      CPCutProb <- CPCutProb + .01
      
    }
    
    CPCutOdds <- (CPCutProb / (1-CPCutProb))
    
    print(CPCutProb)
    print(CPCutOdds)
    
  }
  
  ######## transform the cut odds into decimal and z values ########
  
  {
    
    # p-values for deceptive classifications are lower tail
    # p-values for truthful classifications are upper tail
    
    cutProbT <- round(PCASS2CutOddsT / (1 + PCASS2CutOddsT), 3)
    cutProbD <- round(1-(PCASS2CutOddsD / (1 + PCASS2CutOddsD)), 3)
    
    cutZT <- round(qnorm(cutProbT), 3)
    cutZD <- round(qnorm(cutProbD), 3)
    
    # # change the cutProbD to lower tail
    # # so that pGT test scores can be >= for truth and <= for deception
    # cutProbD  <- ( (cutProbD / (1-cutProbD))^sign(zGT) + 1 )^-1
    # 
    # # cutZT <- qnorm(cutProbT)
    # cutZD <- qnorm(cutProbD)
    
    print("critical z values")
    print(cutZT)
    print(cutZD)
  
  }
  
  ####### PCASS-2 score sheet ##########
  
  {
    
    ## PCASS2 scores ##
    
    PCASS2ScoresheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                       useSensors=PCASS2Sensors2,
                                       scoreType="PCASS2Score",
                                       decimals=2,
                                       DLSTType=DLSTType,
                                       outputName="PCASS2ScoresheetDF",
                                       makeDF=FALSE,
                                       saveCSV=FALSE )
    
  }
  
  #################  KW-ANOVA ################
  
  {

    ## get the KWANOVA result ##

    # already sourced by the decisionRules.R script
    # source('~/Dropbox/R/NCCA_ASCII_Parse/KWANOVA.R', echo=FALSE)

    # requires the within-chart RQ means from sensorDiffScoreMtx
  
    withinChartRQMeans <- 
      chartTotalsFn(scoreSheetDF=PCASS2ScoresheetDF,
                    outputName="PCASS2SchartTotalsDF",
                    aggType="mean",
                    weightingCoefs=sensorContributions,
                    minSensorScores=1,
                    makeDF=FALSE,
                    saveCSV=FALSE)
    
    KWResult <- 
      KWANOVAFn(withinChartRQMeans=withinChartRQMeans, a=.05)

    # print(KWResult)
    
    # KWResult$KWResult is either "sig" or "ns"

    # use the KW result to select the SSR or TSR

  }
  
  ######### select the PCASS-2 decision rule ###########
  
  outputRule <- PCASS2DecisionRule
  
  if(PCASS2DecisionRule == "auto" || grepl("auto",  outputRule)) {

    # PCASS2DecisionRule <- "auto"
    outputRule <- PCASS2DecisionRule

    if(PCASS2DecisionRule == "auto") {
      PCASS2DecisionRule <-  ifelse(KWResult$KWResult == "sig",
                                  "SSR",
                                  "TSR" )
      outputRule <- paste0(PCASS2DecisionRule, " (auto-selected)")
    }

    print(PCASS2DecisionRule)
    print(outputRule)
    
  } else {
    outputRule <- PCASS2DecisionRule
    print(PCASS2DecisionRule)
    print(outputRule)
  }
  
  ######## call the decision rules ##########
  
  {
    
    # load the PCASS2_GTRFn() PCASS2_SSRFn() and PCASS2_TSRFn()
    # source('~/Dropbox/R/NCCA_ASCII_Parse/PCASS2_helper_functions.R', echo=FALSE)
    
    
    GTRResult <- PCASS2_GTRFn(totalScore=postZGT, 
                             RQNames=uniqueRQs, 
                             cutScores=c(GTDI=cutZD, GTNDI=cutZT) )
    
    SSRResult <- PCASS2_SSRFn(subtotalScores=postZST, 
                             RQNames=uniqueRQs, 
                             cutScores=c(STDISR=cutZD, STNDINSR=cutZT) )
    
    TSRResult <- PCASS2_TSRFn(totalScore=postZGT, 
                             subtotalScores=postZST, 
                             RQNames=uniqueRQs, 
                             cutScores=c(GTDI=cutZD, GTNDI=cutZT) )
    
    # july 16 need to check the TSR again
    # the statistical correction for DI subtotal postOdds
    # <>
    
    ## select the result ##

    PCASS2Result <- switch(PCASS2DecisionRule,
                          GTR=GTRResult,
                          SSR=SSRResult,
                          TSR=TSRResult )
    
  }
  
  ####### parse the output ########
  
  {
    
    ## parse the output values ##
    
    PCASS2CategoricalResult <- PCASS2Result$testResult
    
    PCASS2QuestionResults <- PCASS2Result$subtotalResults
    
    PCASS2ResultUsing <- PCASS2Result$resultUsing 
    
    # lowestSubtotalScore <- round(PCASS2Result$lowestSubtotalScore, 3)
    lowestSubtotalScore <- round(min(postZST), 3)
    
    # lowestSubtotalRQName <- PCASS2Result$lowestRQName
    lowestSubtotalRQName <- uniqueRQs[which.min(postZST)]
      
    zCutScore <- round(PCASS2Result$cutZ, 3)

    zScore <- round(PCASS2Result$zScore, 3)
    
    PCASS2PostOdds <- round(PCASS2Result$postOdds, 3)
    
    # upper tail probability for deception or truth
    PCASS2PostProb <- 
      round(PCASS2Result$postOdds / (1+PCASS2Result$postOdds), 3)
    
  }
  
  ##### select the output Bayes Factor for the decision rule #####
  
  {
    
    PCASS2BayesFactor <- 
      ifelse(PCASS2ResultUsing == "grand total",
             round(BayesFactorGT, 3), 
             # no correction needed for GTR
             # for SSR and TSR the correction is used in different ways
             ifelse(PCASS2DecisionRule=="SSR",
                    ifelse(sign(postZSTMin) == -1,
                           # no correction for SSR on negative z subtotals
                           BayesFactorST,
                           # with correction for + min subtotal z
                           BayesFactorSTc),
                    ifelse(sign(postZSTMin) == -1,
                           # with correction for - z scores TSR,
                           BayesFactorSTc,
                           # no correction for TSR on  + z subtotal
                           BayesFactorST) ) )
    
    # july 16 2020 <>
    # bayes factor for subtotal needs to be with statistical correction
    
    # round the BF
    PCASS2BayesFactor <- ifelse(PCASS2BayesFactor > 10,
                                round(PCASS2BayesFactor, 0),
                                round(PCASS2BayesFactor, 1) )
    
    names(PCASS2BayesFactor) <- PCASS2ResultUsing
    
  }
  
  #### reset the RqCqDFSeries Labels ####
  
  if(isTRUE(DLSTType)) {
    
    ## restore the PCASS2 question labels ##
    
    RqCqDFSeries$Label <- saveQuestionLabels
    RqCqDFSeries$chartName <- saveRqCQDFSeriesChartName
    # RqCqDFSeries$eventLabel <- saveEventLabels
    
    # View(RqCqDFSeries)
    
  }
  
  ####################### output tables ##########################
  
  {
    
    ## PCASS2 measurements ##
    
    # source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
    
    PCASS2MeasurementSensors <- 
      PCASS2Sensors[PCASS2Sensors %in% c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE")]
    
    # PCASS2Sensors2
    
    PCASS2MeasurementsDF <- 
      measurementTableFn(RqCqDFSeries=RqCqDFSeries, 
                         useSensors=PCASS2MeasurementSensors,
                         decimals=2,
                         makeDF=makeDF, 
                         saveCSV=saveCSV )
    # View(PCASS2MeasurementsDF)
    
  }
  
  ######### construct a list to hold the PCASS2 result ##########
  
  {
    
    outputListName <- 
      paste(examName, seriesName, "PCASS22OutputList", sep="_")
    
    PCASS2OutputList <- list(PCASS2="Polygraph Credibility Assessment Scoring System - ver.2",
                             examName=examName,
                             seriesName=seriesName,
                             PCASS2Result=PCASS2CategoricalResult,
                             PCASS2QuestionResults=PCASS2QuestionResults,
                             
                             PCASS2PostOdds=PCASS2PostOdds,
                             PCASS2PostProb=PCASS2PostProb,
                             PCASS2PostZ=zScore,
                             PCASS2BayesFactor=PCASS2BayesFactor,
                             PCASS2DecisionRule=outputRule,
                             PCASS2ResultUsing=PCASS2ResultUsing,
                             PCASS2Questions=uniqueQuestions,
                             PCASS2RQNames=uniqueRQs,
                             PCASS2MinRQName=lowestSubtotalRQName,
                             PCASS2MinRQZScore=lowestSubtotalScore,
                             
                             PCASS2PriorP=PCASS2Prior,
                             PCASS2PriorOdds=(PCASS2Prior / (1-PCASS2Prior)),
                             PCASS2CutOddsD=PCASS2CutOddsD,
                             PCASS2CutOddsT=PCASS2CutOddsT,
                             PCASS2CutZD=cutZD,
                             PCASS2CutZT=cutZT,
                             PCASS2CutProbD=cutProbD,
                             PCASS2CutProbT=cutProbT,
                             
                             PCASS2PostProbGT=postPGT,
                             PCASS2PostOddsGT=postOddsGT,
                             PCASS2PostZGT=postZGT,
                             PCASS2PostProbST=postPST,
                             PCASS2PostOddsST=postOddsST,
                             PCASS2PostZST=postZST,
                             
                             PCASS2DiffScoreGT=CQRQDiffScoreGT,
                             PCASS2DiffScoreST=CQRQDiffScoresST,
                             PCASS2CQRQMean=bootstrapMeans['RQDiffMean'],
                             PCASS2CQRQStDev=bootstrapMeans['RQDiffSD'],
                             
                             PCASS2Sensors=PCASS2MeasurementSensors,
                             PCASS2SensorContribution=sensorContributions,
                             KWResult=KWResult,
                             PCASS2Measurements=PCASS2MeasurementsDF,
                             PCASS2ZScoreSheet=PCASS2ScoresheetDF
    )
    
  }
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, PCASS2OutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "PCASS2Output"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      PCASS2OutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(RqCqDFSeries)
  
} # end PCASSScoresFn()


# PCASSScoresFn(RqCqDFSeries=RqCqDFSeries,
#               PCASSSensors=c("AutoEDA", "PLE"),
#               PCASSDecisionRule="SSR",
#               PCASSPrior=.5,
#               PCASSCutOddsT=2,
#               PCASSCutOddsD=2,
#               numberReSamples=3000,
#               makeDF=TRUE,
#               saveCSV=FALSE,
#               analysisListName="analysisResultList")


