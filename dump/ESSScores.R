# R function to compute the ESS scores from the RC score in the measurement DF
# called by the RCScoreFn function in the RCScore.R script




# RqCqDFSeries=RqCqDFSeries,
# ESSMDecisionRule=ESSMDecisionRule,
# makeScoreSheetDF=makeDF, 
# writeScoreSheetCSV=saveCSV,
# makeDF=makeDF,
# writeCSV=saveCSV




ESSScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                        ESSMDecisionRule=ESSMDecisionRule,
                        makeScoreSheetDF=makeDF, 
                        writeScoreSheetCSV=writeCSV,
                        makeDF=makeDF,
                        writeCSV=writeCSV) {
  # R function to compute the ESS scores from the measurements data frame
  # Raymond Nelson
  # 11-27-2017
  # called by the getScoresFn() in the scores.R script
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
    
    RqCqDFSeries$ESSScore <- ""
    
    if(!exists("makeScoreSheetDF")) { makeScoreSheetDF <- TRUE }
    if(!exists("writeScoreSheetCSV")) { writeScoreSheetCSV <- FALSE }
    
    uniqueSensors <- unique(as.character(RqCqDFSeries$sensorName))
    
    ESSMSensors <- c("UPneumo", 
                     "LPneumo", 
                     "AutoEDA", 
                     "ManualEDA",
                     "Cardio", 
                     "PLE")
    
    # ESSSensors <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
    
    # exit if ESSM Sensors are missing
    if(length(which(!(ESSMSensors %in% uniqueSensors))) > 1) {
      return(RqCqDFSeries)
    }
    
    # keep only the extant sensors
    ESSMSensors <- ESSMSensors[ESSMSensors %in% uniqueSensors]
    
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
    
    # if( length(uniqueCharts) < 3 ) { 
    #   "ESS-M reference distributions are for 3 to 5 charts"
    #   return(RqCqDFSeries) 
    # }
    
  }
  
  ################# constraints for ESS-M R/C ratios #################
  
  {
    posPneumoLow <- log(1.284) # 0.2499802
    posPneumoHigh <- log(1.649) # 0.500169
    negPneumoLow <- -log(1.105) # -0.04879016 -.0953 -.09984533 -.04993237
    negPneumoHigh <- -log(1.649) # -0.4054651
    
    posEDAHigh <- log(1000) # 6.907755
    posEDALow <- log(1.0512) # 0.09531018 -.04993237
    negEDAHigh <- -log(1000) # -6.907755
    negEDALow <- -log(1.0512) # -0.09531018
    
    posCardioHigh <- log(1000) # 6.214608
    posCardioLow <- log(1.105) # 0.09531018
    negCardioHigh <- -log(1000) # -6.214608
    negCardioLow <- -log(1.105) # -0.09531018
    
    posPLEHigh <- log(100) # 4.60517
    # posPLELow <- log(1.1) # 0.09531018 # was reported as .0993 for unknown reason
    posPLELow <- log(1.0512) # 0.04879016
    negPLEHigh <- -log(100) # -4.60517
    # negPLELow <- -log(1.1) # 0.09531018 # was reported as -0.0993 for unknown reason
    negPLELow <- -log(1.0512) # -0.04879016 
  }
  
  ####################### iterate over the ESS-M charts ####################
  
  i=1
  for (i in 1:length(uniqueCharts)) {
    
    ######### first get the RqCq data frame for this chart #########
    
    thisChart <- uniqueCharts[i]
    thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
    
    RqCqDFChart <- RqCqDFSeries[thisChartRows,]
    
    # increment the loop to the next chart if no events
    if(nrow(RqCqDFChart) < 2) { 
      next()
    }
    
    ######### then get the data for the RQs and CQs #########
    
    rqRows <- grep("R", RqCqDFChart$eventLabel)
    cqRows <- grep("C", RqCqDFChart$eventLabel)
    
    # exit if there are less than 2 RQs or less than 2 CQs
    if( length(rqRows) < 2 || length(cqRows) < 2 ) { 
      next() 
    }
    
    # get the RQs and CQs for this chart
    uniqueRQsChart <- unique(RqCqDFChart[rqRows,'eventLabel'])
    uniqueCQsChart <- unique(RqCqDFChart[cqRows,'eventLabel'])
    
    if(length(uniqueRQsChart) < 2 || length(uniqueCQsChart) < 2) {
      next()
    }
    
    rqDF <- RqCqDFChart[rqRows,]
    cqDF <- RqCqDFChart[cqRows,]
    # View(rqDF)
    # View(cqDF)
  
    assign("rqDF", rqDF, pos=1)
    assign("cqDF", cqDF, pos=1)
    
    # set the combined pneumo measurement to ""
    # pneumoRows <- which(cqDF$sensorName=="Pneumo")
    # cqDF$sensorMeasurement[pneumoRows] <- NULL
    # RqCqDFChart[cqRows,] <- cqDF
    
    ####### RC scores were already calculated for each chart  #######
    
    # set all integer score rows to NA if the R/C score is NA
    # rqDF[is.na(as.numeric(rqDF$RCScore)),'ESSScore'] <- NA
    # View(rqDF)
    
    # select rows for which the R/C score and integer score are not NA
    sensorRows <- rqDF$sensorName %in% ESSMSensors
    # theseRows <- !is.na(as.numeric(rqDF$RCScore))
    # selectRows <- which(theseRows & sensorRows)
    selectRows <- which(sensorRows)
    
    # initialize the ESSScore vector to NA to avoid errors if there is no score
    ESSScore <- NA
    
    # iterate on the selected sensor rows for for the rqDF
    j=14
    for (j in 1:length(selectRows)) {
      # first get the stimulus name, sensor, and score
      thisStimulusName <- rqDF$eventLabel[selectRows[j]]
      thisSensor <- rqDF$sensorName[selectRows[j]]
      # thisScore is the R/C score
      thisScore <- as.numeric(rqDF$RCScore[selectRows[j]])
      # print the info to the console
      print(paste(thisStimulusName, thisSensor, thisScore))
      
      if(is.na(thisScore)) {
        rqDF$ESSScore[selectRows[j]] <- NA
        next() # next j sensor row
      }
      
      # then compute the ESS scores
      
      # there is probably a different way to do this
      # perhaps using a switch instead of a daisy-chain of ifelse statements
      
      if(any(thisSensor=="UPneumo", 
             thisSensor=="LPneumo", 
             thisSensor=="Pneumo") ) {
        # pneumo
        ESSScore <- if(thisScore >= posPneumoLow && 
                           thisScore <= posPneumoHigh ) { 
          as.character("+1")
        } else if(thisScore <= negPneumoLow && 
                  thisScore >= negPneumoHigh ) {
          as.character("-1")
        } else 0
      } else if(any(thisSensor=="EDA", 
                    thisSensor=="AutoEDA", 
                    thisSensor=="ManualEDA") ) {
        # EDA
        ESSScore <- if(thisScore >= posEDALow &&
                       thisScore <= posEDAHigh ) {
          as.character("+2")
        } else if(thisScore <= negEDALow &&
                  thisScore >= negEDAHigh) {
          as.character("-2")
        } else 0
      } else if(any(thisSensor=="Cardio",
                    thisSensor=="eCardio", 
                    thisSensor=="FC") ) {
        # cardio
        ESSScore <- if(thisScore >= posCardioLow &&
                       thisScore <= posCardioHigh ) {
          as.character("+1")
        } else if(thisScore <= negCardioLow &&
                  thisScore >= negCardioHigh) {
          as.character("-1")
        } else 0
      } else if(thisSensor=="PLE") {
        # vasomotor
        ESSScore <- if(thisScore >= posPLELow &&
                       thisScore <= posPLEHigh) {
          as.character("+1")
        } else if(thisScore <= negPLELow &&
                  thisScore >= negPLEHigh) {
          as.character("-1")
        } else 0
      }
      
      ######## assign the ESS integer score to the rqDF data frame ########
      
      rqDF$ESSScore[selectRows[j]] <- ESSScore
      print(paste("integer score", ESSScore))
      
    } # end loop j over sensor rows
    
    #### combine the upper and lower pneumo scores ####
    
    for (k in 1:length(uniqueRQsChart)) {
      UPneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                            (rqDF$sensorName == "UPneumo"))
      P2 <- rqDF$ESSScore[UPneumoRow]
      P2 <- as.numeric(P2)
      names(P2) <- rqDF$CQName[UPneumoRow]
      LPneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                            (rqDF$sensorName == "LPneumo"))
      P1 <- rqDF$ESSScore[LPneumoRow]
      P1 <- as.numeric(P1)
      names(P1) <- rqDF$CQName[LPneumoRow]
      
      pneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                           (rqDF$sensorName == "Pneumo"))
      
      # 3-2-2017 this code should work for 3-position, ESS and 7-position
      # combine the upper and lower pneumo scores 
      ifelse(P1 * P2 < 0, 
             {
               # if the upper and lower pneumo signs are opposite
               rqDF$ESSScore[pneumoRow] <- 0
               # set the combined measurement and CQName to NA
               rqDF$sensorMeasurement[pneumoRow] <- NA
               rqDF$CQName[pneumoRow] <- NA 
             }, 
             ifelse(P1 + P2 >= 0, 
                    {
                      # both pneumo signs are +
                      # choose the upper P2 if sum is 0
                      # which.max() takes the first of equal values
                      thisPneumo <- which.max(c(P2, P1))
                      rqDF$ESSScore[pneumoRow] <- c(P2, P1)[thisPneumo]
                      rqDF$CQName[pneumoRow] <- 
                        c(names(P2), names(P1))[thisPneumo]
                    }, 
                    {
                      # both pneumo signs are -
                      thisPneumo <- which.min(c(P2, P1))
                      rqDF$ESSScore[pneumoRow] <- c(P2, P1)[thisPneumo]
                      rqDF$CQName[pneumoRow] <- 
                        c(names(P2), names(P1))[thisPneumo]
                    } ) )
      
    } # end loop k for unique RQs
    
    # pass the rqDF back to the RqCqDFChart
    RqCqDFChart[rqRows,] <- rqDF
    
    assign("RqCqDFChart", RqCqDFChart, pos=1)

    # pass the chart data frame back to the series
    RqCqDFSeries[thisChartRows,] <- RqCqDFChart
    # View(RqCqDFSeries)
    
    assign("RqCqDFSeries", RqCqDFSeries, env=.GlobalEnv)
    
  } # end loop i over charts in the series
  
  #######################  ESS-M numerical results  ########################
  
  {
    # use the combined abdominal and thoracic ESS scores
    ESSMSensors2 <- c("Pneumo", "AutoEDA", "Cardio", "PLE")
    
    if(!isTRUE(includePLEScores)) {
      ESSMSensors2 <- c("Pneumo", "AutoEDA", "Cardio")
    }

    ESSMSensors2 <- ESSMSensors2[ESSMSensors2 %in% uniqueSensors]

    # calculate the grand total score
    theseRows <- which(RqCqDFSeries$sensorName %in% ESSMSensors2)
    grandTotal <- sum(as.integer(RqCqDFSeries$ESSScore[theseRows]),
                      na.rm=TRUE)

    print(paste("ESS-M grand total:", grandTotal))

    subtotalScores <- rep(NA, length=length(uniqueRQs))
    names(subtotalScores) <- uniqueRQs

    # iterate over the RQs
    i=1
    for(i in 1:length(uniqueRQs)) {
      thisRQ <- uniqueRQs[i]
      RQRows <- RqCqDFSeries$eventLabel == thisRQ
      ESSMSensorRows <- RqCqDFSeries$sensorName %in% ESSMSensors2
      theseRows <- which(RQRows & ESSMSensorRows)
      subtotalScores[i] <- sum(as.numeric(RqCqDFSeries$ESSScore[theseRows]),
                               na.rm=TRUE)
    }

    print("ESS-M subtotals: ")
    # print(toString(subtotalScores))
    print((subtotalScores))

    minSubtotalScore <- subtotalScores[which.min(subtotalScores)]

    print(paste("min subtotal:", names(minSubtotalScore), minSubtotalScore)) 
  }
  
  ################## ESS-Multinomial Reference Model ###################
  
  {
    ESSMRefDir <- "~/Dropbox/R/NCCA_ASCII_Parse/"

    ESSM_simple_GT <- read.csv(paste0(ESSMRefDir, "ESSM_simple_GT.csv"),
                               stringsAsFactors=FALSE)

    ESSM_simple_ST <- read.csv(paste0(ESSMRefDir,
                                      "ESSM_simple_ST.csv"),
                               stringsAsFactors=FALSE)
  }
  
  ################ ESS-M cutScores and decision rules ##############
  
  {
    # hard-coded simple ESS-M cutscores
    # load these in the same env where the decision rules Fn are defined
    cutScores <- c(GTDI=-3, GTNDI=3, STDI=-3, STNDI=3, STDIc=-7, STNDIc=1)

    # source('~/Dropbox/R/NCCA_ASCII_Parse/decisionRules.R', echo=FALSE)
   }
  
  ################ ESS-M Classification (result) #################
  
  {
    
    TSRResult <- TSRFn(grandTotal=grandTotal, 
                       subtotalScores=subtotalScores, 
                       cutScores=cutScores)

    SSRResult <- SSRFn(subtotalScores=subtotalScores, 
                       cutScores=cutScores)
    
    GTRResult <- GTRFn(totalScore=grandTotal, 
                       RQNames=uniqueRQs,
                       cutScores=cutScores)
    
    if(!exists("ESSMDecisionRule")) ESSMDecisionRule <- "TSR"
    
    ESSMTestResult <- switch(ESSMDecisionRule,
                         "TSR"=TSRResult$testResult,
                         "SSR"=SSRResult$testResult,
                         "GTR"=GTRResult$testResult)
    
    ESSMQuestionResults <- switch(ESSMDecisionRule,
                                  "TSR"=TSRResult$subtotalResults,
                                  "SSR"=SSRResult$subtotalResults,
                                  "GTR"=GTRResult$subtotalResults)

  }
  
  
  stop()
  
  ########################### ESS-M output section ########################
  
  # output table functions
  # source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
  
  ######## ESS-M measurements data frame 
  
  measurementsDF <-
    measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                       useSensors=ESSMSensors2,
                       makeScoreSheetDF=makeScoreSheetDF,
                       writeScoreSheetCSV=writeScoreSheetCSV )
  
  ############# ESS-M score sheet for the series
  
  scoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=ESSMSensors2,
                               scoreType="ESSScore",
                               outputName="ESSMScoresheetDF",
                               makeScoreSheetDF=makeScoreSheetDF,
                               writeScoreSheetCSV=writeScoreSheetCSV)
  
  ################ ESS-M series totals
  
  seriesTotalsDF <- seriesTotalsFn(scoreSheetDF=scoreSheetDF,
                                   outputName="ESSMSeriesTotalsDF",
                                   aggType="sum",
                                   weightingCoefs=NULL,
                                   aggMethod="between",
                                   makeDF=makeDF,
                                   saveCSV=writeCSV)
  
  ########## ESS-M chart totals, including RQ subtotals and chart subtotals
  
  chartTotalsDF <- chartTotalsFn(scoreSheetDF=scoreSheetDF,
                                 outputName="ESSMChartTotalsDF",
                                 aggType="sum",
                                 weightingCoefs=NULL,
                                 makeDF=makeDF,
                                 saveCSV=writeCSV)
  
  ################ ESS-M sensor subtotals for the series
  
  sensorTotalsDF <- sensorSubtotalsFn(scoreSheetDF=scoreSheetDF,
                                      outputName="ESSMSensorTotalsDF",
                                      aggType="sum",
                                      makeDF=makeDF,
                                      saveCSV=writeCSV)
  
  ############### construct a list to hold the ESS-M result #################
  
  outputListName <- paste(examName, seriesName, "ESSMOutputList", sep="_")
  
  ESSMOutputList <- list(ESSMGrandTotal=grandTotal,
                         ESSMRQNames=uniqueRQs,
                         ESSMSubtotalScores=subtotalScores,
                         ESSMMinSubtotal=minSubtotalScore,
                         ESSMResult=ESSMTestResult,
                         ESSMQuestionResults=ESSMQuestionResults,
                         ESSMDecisionRule=ESSMDecisionRule,
                         ESSMCutscores=cutScores,
                         ESSMMeasurementDF=measurementDF,
                         ESSMScoreSheetDF=scoreSheetDF,
                         ESSMChartTotalsDF=chartTotalsDF,
                         ESSMSeriesTotalsDF=seriesTotalsDF,
                         ESSMSensorTotalsDF=sensorTotalsDF )
  
  # save the list to the globad env as a side effect
  assign(outputListName, ESSMOutputList, env=.GlobalEnv)
  
  #### visible output
  
  return(RqCqDFSeries)

} # end ESSScoresFn



