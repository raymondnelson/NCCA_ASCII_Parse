# R function to calculate OSS-2 scores and results
# Feb 1, 2019
# Raymond Nelson
#
####



# RqCqDFSeries=RqCqDFSeries, 
# forced=TRUE,
# oss2AlphaT=.05,
# oss2AlphaD=.05, 
# makeDF=makeDF,
# saveCSV=saveCSV



# requires the OSS-2 reference model
source(paste0(RPath, 'OSS2Model.R'), echo=FALSE)

# requires the grand total decision rule
# source(paste0(RPath, 'decisionRules.R'), echo=FALSE)

# requires some other functions to make the score sheet and measurement tables
# source(paste0(RPath, 'outputScores.R'), echo=FALSE)



OSS2ScoresFn <- function(RqCqDFSeries=RqCqDFSeries, 
                         forced=TRUE,
                         OSS2DecisionRule=OSS2DecisionRule,
                         oss2AlphaT=.07,
                         oss2AlphaD=.06, 
                         makeDF=FALSE,
                         saveCSV=FALSE,
                         analysisListName="analysisResultList" ) {
  # R function to compute the OSS-2 scores from the measurements data frame
  # sourced by the getScoresFn in the scores.R script
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  #
  # requires 6 tables for the OSS-2 reference tables
  # source(paste0(RPath, 'OSS2Model.R'))
  # 
  # x input is a data frame of measurements for the RQs and CQs 
  # for a test series, including all test charts
  #
  # forced input parameter will permit the use of OSS-2
  # with exams other than 3 Chart ZCT with 3 RQs and 3 CQs
  #
  # output is the RqCqDFSeries data frame 
  # with the OSS-2 column populated with OSS-2 scores
  # 
  # other output occurs as a side effect 
  #
  # OSS-2 scores are calculated using the preceding CQ for each RQ
  #
  ####
  
  {
    
    ## set up ##
    
    options(warn = 2) # escalate warnings to errors
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    print("calculate the OSS-2 scores")
    
    OSS2Sensors <- c("UPneumo", 
                     "LPneumo", 
                     "Pneumo",
                     "AutoEDA",
                     "ManualEDA",
                     "Cardio" ) # ,
                     # "FC" )
    
    # input parameters
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    # November 19, 2022 moved this before the next statement for LXCAT exams
    # exit if OSS-2 Sensors are missing
    if(length(which(!(OSS2Sensors %in% uniqueSensors))) > 1) {
      return(RqCqDFSeries)
    }
    
    OSS2Sensors <- OSS2Sensors[OSS2Sensors %in% uniqueSensors]
    
    uniqueQuestions <- unique(RqCqDFSeries$Label)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      return(RqCqDFSeries) 
    }
    
    uniqueRQs <- 
      unique(RqCqDFSeries$eventLabel[grep("R", RqCqDFSeries$eventLabel)])
    uniqueCQs <- 
      unique(RqCqDFSeries$eventLabel[grep("C", RqCqDFSeries$eventLabel)])
    
    # these indices give the question order for the first chart
    RQIndices <- which(uniqueQuestions %in% uniqueRQs)
    CQIndices <- which(uniqueQuestions %in% uniqueCQs)
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    {
      if(!exists("forced")) forced <- TRUE
      if(!exists("oss2AlphaT")) oss2AlphaT <- .07
      if(!exists("oss2AlphaD")) oss2AlphaD <- .06
      if(!exists("makeDF")) makeDF <- FALSE
      if(!exists("saveCSV")) saveCSV <- FALSE
    }
    
    saveSensorMeasurements <- RqCqDFSeries$sensorMeasurement
    
    RqCqDFSeries$OSS2Score <- ""
    
    # initialize a data frame and vector for the OSS-2 RC scores
    RCScoresDF <- NULL
    RCScoresVc <- NULL
    
    # initialize a variable to hold the chart row
    allChartRows <- NULL
    
    # initialize a data frame with the question sequence for each chart

    questionSequenceDF <- questionSequenceFn(measurementDF=RqCqDFSeries,
                                             outputName="OSS2QuestionSequence",
                                             makeDF=FALSE,
                                             saveCSV=FALSE)
    # this data frame is included in the output 
    # and shows the question order for each chart
    # ESS and OSS-2 scores are influenced to some degree by question order
    
  }
  
  #### check for OSS-2 compliance with 3 charts 3 RQs and 3 CQs ####
  
  {
    
    # exit if not 3 RQs
    if( length(uniqueRQs) != 3 && forced==FALSE ) { 
      "OSS-2 norms are for 3 RQs"
      return(RqCqDFSeries) 
    }
    
    # exit if not 3 CQs
    if( length(uniqueCQs) != 3 && forced==FALSE ) { 
      "OSS-2 norms are for 3 CQs"
      return(RqCqDFSeries) 
    }
    
    # exit if not 3 charts
    if( length(uniqueCharts) != 3 && forced==FALSE ) { 
      "OSS-2 norms are for 3 charts"
      return(RqCqDFSeries) 
    }
    
    # exit for unequal number of RQs and CQs
    if( length(uniqueRQs) != length(uniqueCQs) && forced==FALSE ) {
      "OSS-2 requires an equal number of RQs and CQs"
      return(RqCqDFSeries) 
    }
    
    # check if the CQs are preceding the RQs
    if( !all( CQIndices[1:length(RQIndices)] < RQIndices, na.rm=TRUE) ) {
      "OSS2 requires a ZCT format with 3 CQs preceding 3 RQs"
      if(forced==FALSE) { return(RqCqDFSeries) }
    }
    
    # exit if not at least 2RQs and 2 CQs
    if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
      "incorrect number of RQs and CQs"
      return(RqCqDFSeries) 
    }
    
  }
  
  ################# OSS-2 reference model #####################
  
  # source(paste0(RPath, 'OSS2Model.R'), echo=FALSE)
  
  ###################  OSS-2 numerical cut-scores ##################
  
  {
    
    # OSS2_alfa_truthtelling and OSS2_alfa_deception
    # are loaded with the OSS2Model.R script
    
    # OSS-2 numerical cutscores
    cutScoreT <- OSS2_alfa_truthtelling$truthful_cutscore[
      which.max(OSS2_alfa_truthtelling$alfa_truthtelling[
        OSS2_alfa_truthtelling$alfa_truthtelling <= oss2AlphaT])]
    cutScoreD <- OSS2_alfa_deception$deceptive_cutscore[
      which.max(OSS2_alfa_deception$alfa_deception[
        OSS2_alfa_deception$alfa_deception <= oss2AlphaD])]
  }
  
  #### iterate over the charts to get the RC ratios and OSS-2 scores ####
  
  i=1
  # modified the iff statement 2024Apr05 
  # to allow for scoring all available charts with the Ohio n40 sample
  for ( i in 1: ifelse(forced, length(uniqueCharts), 3) ) { 
    
    {
    
      # get the data for this chart
      
      thisChart <- uniqueCharts[i]
      # print(thisChart)
      thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
      
      RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      
    }
    
    {
      
      ## get the RcQc data frame for this chart ##
      
      # limited to three charts
      
      # View(RqCqDFChart)
      
      uniqueRQsChart <-
        unique(RqCqDFChart$eventLabel[grep("R", RqCqDFChart$eventLabel)])
      
      ## limit it to 3 RQs ##
      # commented out 6/19/2022 to work with LXCAT charts
      # if(length(uniqueRQsChart) > 3) {
      #   ignoreTheseRQs <- uniqueRQsChart[c(4:length(uniqueRQsChart))]
      #   thisChartRows <-
      #     thisChartRows[!(RqCqDFChart$eventLabel %in% ignoreTheseRQs)]
      #   RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      # }
      
      allChartRows <- c(allChartRows, thisChartRows)
      
    }
    
    {
      
      # then slice the data for the RQs and CQs
      
      rqRows <- grep("R", RqCqDFChart$eventLabel)
      cqRows <- grep("C", RqCqDFChart$eventLabel)
      
      # Nov 13, 2020
      # RCScoresVc <- NULL
      
      # exit if there are no RQs or no CQs
      if(length(rqRows) == 0 || length(cqRows) == 0) {
        # this will help to tollerate ad-hoc changes
        # such as repeated or missing questions
        RCScoresVc <- c(RCScoresVc, rep("", times=nrow(RqCqDFChart)))
        next()
      }
      
      # gives the question order for this chart
      uniqueQuestionsChart <- unique(RqCqDFChart$eventLabel)
      
      uniqueRQsChart <- unique(RqCqDFChart[rqRows,'eventLabel'])
      uniqueCQsChart <- unique(RqCqDFChart[cqRows,'eventLabel'])
      
      RQIndicesChart <- which(uniqueQuestionsChart %in% uniqueRQsChart)
      CQIndicesChart <- which(uniqueQuestionsChart %in% uniqueCQsChart)
      
    }
    
    # iterate over the unique RQs and calculate the RC ratios
    j=1
    for(j in 1:length(RQIndicesChart)) {
      
      # RQIndices always gives the question order for the first chart
      # similar to the 2007 OSS-2 replication in Excel
      # the 2007 Excel replication was part of OSS-3
      # for which the RQs need to be aligned
      
      # July 30, 2020
      # use RQIndicesChart and CQIndicesChart
      # along with uniqueRQsChart and uniqueCQsChart
      # and uniqueQuestionsChart
      # will give the order or presentation for each chart
      
      #### initialize separate data frames for the RQs an CQs ####
      
      {
        
        # get the RQ
        # use of RQIndices puts the RQs in the chart 1 order
        thisRQ <- uniqueQuestionsChart[RQIndicesChart[j]]
        thisRQRows <- which(RqCqDFChart$eventLabel==thisRQ &
                              RqCqDFChart$sensorName %in% OSS2Sensors)
        rqDF <- RqCqDFChart[thisRQRows,]
        # View(rqDF)
        
        # get the RQIndex for this chart
        thisRQIndex <- which(uniqueQuestionsChart == thisRQ)
        # to use the first chart order use
        # which(uniqueQuestions == thisRQ)
        
        
      }
      
      {
        
        # get the CQs preceding the RQ
        precedingCQs <- CQIndicesChart[which(CQIndicesChart < thisRQIndex)]

        if(length(precedingCQs) == 0 || any(is.na(precedingCQs))) {
          # use the subsequent CQ if none
          subsequentCQs <- CQIndicesChart[which(CQIndicesChart > RQIndices[j])]
          if(length(subsequentCQs) == 0) next() # no CQs under this condition
          thisCQ <- uniqueQuestionsChart[min(subsequentCQs, na.rm=TRUE)]
          # next j iteration for RQs if there is no preceding CQ
          if(is.na(thisCQ)) next()
        } else {
          thisCQ <- 
            uniqueQuestionsChart[max(CQIndicesChart[which(CQIndicesChart < thisRQIndex)])]
            # to use the actual order of presentation for this chart
        }
        
        # to use the question order from chart 1
        # uniqueQuestions[max(CQIndices[which(CQIndices < RQIndices[j])], na.rm=TRUE)]
        
        # for the chart order
        # if(is.na(thisCQ)) {
        #   # use the subsequent CQ if none
        #   subsequentCQs <- CQIndicesChart[which(CQIndicesChart > thisRQIndex)]
        #   if(length(subsequentCQs) == 0) next() # no CQs under this condition
        #   thisCQ <- uniqueQuestionsChart[min(subsequentCQs, na.rm=TRUE)]
        #   # next j iteration for RQs if there is no preceding CQ
        #   if(is.na(thisCQ)) next()
        # }
        
        thisCQRows <- which(RqCqDFChart$eventLabel==thisCQ &
                              RqCqDFChart$sensorName %in% OSS2Sensors)
        
        # 2020-07-12 fix a crash if the CQ is missing
        # pick the CQ preceding the RQ
        if(length(thisCQRows) == 0  && any(CQIndicesChart < thisRQIndex)) {
          usethisCQ <- 
            uniqueQuestionsChart[max(CQIndicesChart[which(CQIndicesChart < thisRQIndex)])]
          thisCQRows <- which(RqCqDFChart$eventLabel==usethisCQ &
                                RqCqDFChart$sensorName %in% OSS2Sensors)
        }
        cqDF <- RqCqDFChart[thisCQRows,]
        # View(cqDF)
        
      }
      
      {

        # initialize a data frame for the next CQ
        nextCqDF <- cqDF
        
        # OSS2 uses the preceding CQ by default
        # get the subsequent CQ, if available,

        # to replace missing and NA sensor values
        subsequentCQs <- CQIndicesChart[which(CQIndicesChart > thisRQIndex)]
        # Nov 23, 2022 added any() wrapper to avoid warnings
        if(length(subsequentCQs) != 0 && any(!is.na(subsequentCQs))) {
          nextCQ <-
            uniqueQuestionsChart[min(CQIndicesChart[which(CQIndicesChart > thisRQIndex)], na.rm=TRUE)]
          if(!is.na(nextCQ)) {
            thisCQRows <- which(RqCqDFChart$eventLabel==thisCQ &
                                  RqCqDFChart$sensorName %in% OSS2Sensors)
            # if missing pick the CQ subsequent to the RQ
            if(length(thisCQRows) == 0 && any(CQIndicesChart > thisRQIndex)) {
              usethisCQ <-
                uniqueQuestionsChart[min(CQIndicesChart[which(CQIndicesChart > thisRQIndex)])]
              thisCQRows <- which(RqCqDFChart$eventLabel==usethisCQ &
                                    RqCqDFChart$sensorName %in% OSS2Sensors)
            }
            nextCqDF <- RqCqDFChart[thisCQRows,]
          }
        }
        # View(nextCqDF)

      }
      
      #### need an option to use the actual question order for each chart ####
      
      # this option will more closely reflect the OSS/OSS-2 publications
      
      #### calculate the R/C sensor ratios for this RQ ####
      
      {
        
        rqVals <- rqDF$sensorMeasurement
        cqVals <- cqDF$sensorMeasurement
        
        # zero as missing
        rqVals[which(rqVals < 1)] <- NA
        cqVals[which(cqVals < 1)] <- NA
        
        # hunt for a replacment CQ value if possible
        
        replaceTheseCQs <- which(is.na(cqVals) | cqVals == "")
        # nextCqVals <- nextCqDF$sensorMeasurement
        # Feb 6, 2024 attempting to salvage cases with a lot of missing data
        nextCqVals <- mean(cqDF$sensorMeasurement, na.rm=TRUE)
        cqVals[replaceTheseCQs] <- nextCqVals[replaceTheseCQs]
        
        # zero as missing again
        rqVals[which(rqVals < 1)] <- NA
        cqVals[which(cqVals < 1)] <- NA
        
        # missing measurements will give NA
        RCRatios <- rqVals / cqVals
        # names(RCRatios) <- OSS2Sensors
        # names(RCRatios) <- rqDFChart$sensorName
        
        # Feb 8, 2024
        RCRatios[which(RCRatios == "NaN")] <- NA
        
        rqDF$OSS2Score <- RCRatios
        
      }
      
      # OSS-2 RC Ratios are transformed to OSS-2 scores in another loop
  
      # pass the rqDF scores to the RqCqDFChart ##
      RqCqDFChart[thisRQRows,] <- rqDF
      # View(RqCqDFChart)
      
      # we now have the RC ratios for the chart
    
    } # end loop j over RQs
    
    ####  trim outlier ratios  ####
    
    {
      # trim to 3.8906 standard deviations 
      
      # not included in the published OSS-2 spec
    }
    
    #### construct a table of OSS-2 R/C ratios ####
    
    {
    
      # before the ratios are lost in the transformation to OSS-2 scores
    
      # RCSensors <- c("UPneumo",   "LPneumo", "AutoEDA", "Cardio")
      # 
      # RCScoresChartDF <- scoreSheetFn(RqCqDFSeries=RqCqDFChart, 
      #                            useSensors=RCSensors,
      #                            scoreType="OSS2Score",
      #                            outputName="OSS2RCScoresDF",
      #                            makeDF=FALSE,
      #                            saveCSV=FALSE)
      # 
      # # bind the chart data frame to the series data frame
      # RCScoresDF <- rbind(RCScoresDF, RCScoresChartDF)
      
      # this will help to tolerate ad-hoc changes
      # such as repeated or missing questions
      RCScoresVc <- c(RCScoresVc, RqCqDFChart$OSS2Score)
      
    }
    
    #### use the OSS-2 tables to get the OSS-2 sensor scores ####
    
    l=1
    for(l in 1:length(OSS2Sensors)) {
      
      # iterate over the sensors to get the OSS-2 scores
      
      thisSensor <- OSS2Sensors[l]
      
      theseSensorRows <- which(RqCqDFChart$sensorName == thisSensor)
      
      # remove missing and NA rows
      theseSensorRows <- theseSensorRows[RqCqDFChart$OSS2Score[theseSensorRows] != "" & 
        !is.na(RqCqDFChart$OSS2Score[theseSensorRows])]
      
      if(length(theseSensorRows) == 0) next()
      
      sensorVals <- as.numeric(RqCqDFChart$OSS2Score[theseSensorRows])
      
      # UPneumo scores
      if(thisSensor == "UPneumo") {
        sensorScores <- rep(NA, times=length(sensorVals))
        for(m in 1:length(sensorVals)) {
          if(is.na(sensorVals[m])) next()
          sensorScores[m] <- 
            OSS2_RLL_Ratios$RLL_Score[
              max(which(OSS2_RLL_Ratios$RLL_Ratio <= sensorVals[m]))]
        }
        RqCqDFChart$OSS2Score[theseSensorRows] <- sensorScores
      }
      
      # LPneumo scores
      if(thisSensor == "LPneumo") {
        sensorScores <- rep(NA, times=length(sensorVals))
        for(m in 1:length(sensorVals)) {
          if(is.na(sensorVals[m])) next()
          sensorScores[m] <- 
            OSS2_RLL_Ratios$RLL_Score[
              max(which(OSS2_RLL_Ratios$RLL_Ratio <= sensorVals[m]))]
        }
        RqCqDFChart$OSS2Score[theseSensorRows] <- sensorScores
      }
      
      # Auto EDA scores
      if(thisSensor == "AutoEDA") {
        sensorScores <- rep(NA, times=length(sensorVals))
        for(m in 1:length(sensorVals)) {
          if(is.na(sensorVals[m])) next()
          sensorScores[m] <- 
            OSS2_EDA_Ratios$EDA_Score[
              max(which(OSS2_EDA_Ratios$EDA_Ratio <= sensorVals[m]))]
        }
        RqCqDFChart$OSS2Score[theseSensorRows] <- sensorScores
      }
      
      # Manual EDA scores
      if(thisSensor == "ManualEDA") {
        sensorScores <- rep(NA, times=length(sensorVals))
        for(m in 1:length(sensorVals)) {
          if(is.na(sensorVals[m])) next()
          sensorScores[m] <- 
            OSS2_EDA_Ratios$EDA_Score[
              max(which(OSS2_EDA_Ratios$EDA_Ratio <= sensorVals[m]))]
        }
        RqCqDFChart$OSS2Score[theseSensorRows] <- sensorScores
      }
      
      # Cardio Scores
      if(thisSensor == "Cardio") {
        sensorScores <- rep(NA, times=length(sensorVals))
        for(m in 1:length(sensorVals)) {
          if(is.na(sensorVals[m])) next()
          sensorScores[m] <- 
            OSS2_Cardio_Ratios$Cardio_Score[
              max(which(OSS2_Cardio_Ratios$Cardio_Ratio <= sensorVals[m]))]
        }
        RqCqDFChart$OSS2Score[theseSensorRows] <- sensorScores
      }
      
    } # end loop l over OSS-2 sensors
    
    # now we have the OSS-2 score for all sensors

    ######### combine the upper and lower pneumo scores #########

    {
      
      # make a new data frame with only RQ rows
      rqDFChart <- RqCqDFChart[rqRows,]
      # View(rqDFChart)
      
      # iterate over the RQs to combine the pneumoes
      k=1
      for (k in 1:length(uniqueRQsChart)) {
        # upper pneumo OSS-2 score
        P2 <- rqDFChart$OSS2Score[which(rqDFChart$eventLabel == uniqueRQsChart[k] &
                                          (rqDFChart$sensorName == "UPneumo"))]
        P2 <- as.numeric(P2)
        # lower pneumo OSS-2 scores
        P1 <- rqDFChart$OSS2Score[which(rqDFChart$eventLabel == uniqueRQsChart[k] &
                                          (rqDFChart$sensorName == "LPneumo"))]
        P1 <- as.numeric(P1)
        # combine the upper and lower pneumo scores
        # this should work for 3-position, ESS and 7-position scores
        ULPneumoScore <- ifelse(P1 * P2 < 0,
                                0,
                                ifelse(P1 + P2 > 0,
                                       max(c(P1, P2)),
                                       ifelse(P1 + P2 < 0,
                                              min(c(P1, P2)),
                                              0 ) ) )
        
        names(ULPneumoScore) <- "Pneumo"
        # assign the value to the rqDF
        pneumoRow <- which(rqDFChart$eventLabel == uniqueRQsChart[k] &
                             (rqDFChart$sensorName == "Pneumo"))
        rqDFChart$OSS2Score[pneumoRow] <- ULPneumoScore
        
      } # end loop k over RQs
      
      # pass the rqDFChart back to the RqCqDFChart
      RqCqDFChart[rqRows,] <- rqDFChart
      
      # pass the RqCqDFChart to the RqCqDFSeries
      RqCqDFSeries[thisChartRows,] <- RqCqDFChart
      
    }
    
  } # end i loop over charts
  
  # View(RqCqDFChart)
  
  # RCScoresDF
  # RCScoresVc
  
  ########################### OSS-2 result ################################
  
  {
    
    OSS2Sensors2 <- c("Pneumo", "AutoEDA", "Cardio")
    
    theseRows <- which(RqCqDFSeries$sensorName %in% OSS2Sensors2)
    
    OSS2GrandTotal <- sum(as.numeric(RqCqDFSeries[theseRows,'OSS2Score']),
                          na.rm=TRUE)
    
    cutScores <- c(GTNDI=cutScoreT, GTDI=cutScoreD)
    
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    # call the private function to use the grand total rule
    OSS2Result <- GTRFn(totalScore=OSS2GrandTotal, 
                        RQNames=uniqueRQs, 
                        cutScores=cutScores, 
                        flip=FALSE )
    
    OSS2CategoricalResult <- OSS2Result$testResult
    
    OSS2QuestionResults <- OSS2Result$subtotalResults
    
    resultUsing <- OSS2Result$resultUsing
    
    # get the OSS-2 p-value
    
    thisRow <- ifelse(OSS2GrandTotal > 0,
                      which(OSS2_reference_table$Total_Score == OSS2GrandTotal),
                      which(OSS2_reference_table$Total_Score == OSS2GrandTotal) )
    
    OSS2PVal <- ifelse(OSS2GrandTotal > 0,
                       OSS2_reference_table$p_deceptive[thisRow],
                       OSS2_reference_table$p_truthful[thisRow] )

    # fix pval==0
    if(OSS2PVal == 0) OSS2PVal <- "<.01"
    
    # pval for DI is the probability  
    # that a score was produced by a person represented by the 
    # reference distribution for the truthful population
    
    # pval for NDI is the probability that a score was produced
    # by a person represented by the 
    # reference distribution for deceptive persons
    
  } # end OSS-2 result
  
  #####################  OSS-2 output  ######################
  
  # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
  
  ### OSS-2 measurements data frame 
  
  OSS2Sensors3 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
  
  OSS2MeasurementsDF <-
    measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                       useSensors=OSS2Sensors3,
                       decimals=2,
                       makeDF=makeDF,
                       saveCSV=writeCSV )
  # View(OSS2MeasurementsDF)
  
  ### OSS-2 score sheet for the series 
  
  scoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=OSS2Sensors2,
                               scoreType="OSS2Score",
                               outputName="OSS2ScoresheetDF",
                               makeDF=makeDF,
                               saveCSV=writeCSV)
  # View(scoreSheetDF)
  
  ### construct a table of OSS-2 R/C ratios
  
  {
    
    # save this to put them back after initializing the RC Ration DF
    saveOSS2Scores <- RqCqDFSeries$OSS2Score
    
    # put the RC scores to make the RC Ratio table
    RqCqDFSeries$OSS2Score[allChartRows] <- RCScoresVc
    
    # include these sensors on the RC ratio table
    RCSensors <- c("UPneumo",   "LPneumo", "AutoEDA", "Cardio")

    RCScoresDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries,
                               useSensors=RCSensors,
                               scoreType="OSS2Score",
                               outputName="OSS2RCScoresDF",
                               makeDF=FALSE,
                               saveCSV=FALSE)

    # put thee OSS2 scores back after creating the RC table
    RqCqDFSeries$OSS2Score <- saveOSS2Scores
    
    # kludgy way of doing this but it works
    
    # View(RCScoresDF)
    
  }
  
  ### OSS-2 series totals
  
  # series totals are the RQ subtotals for all charts
  
  seriesTotalsDF <- seriesTotalsFn(scoreSheetDF=scoreSheetDF,
                                   outputName="OSS2SeriesTotalsDF",
                                   aggType="sum",
                                   weightingCoefs=NULL,
                                   aggMethod="within",
                                   makeDF=makeDF,
                                   saveCSV=saveCSV)
  
  ### OSS-2 chart subtotals, including RQ subtotals and chart subtotals
  
  chartTotalsDF <- chartTotalsFn(scoreSheetDF=scoreSheetDF,
                                 outputName="OSS2ChartTotalsDF",
                                 aggType="sum",
                                 weightingCoefs=NULL,
                                 makeDF=makeDF,
                                 saveCSV=saveCSV)
  
  ### OSS-2 sensor subtotals for the series
  
  sensorTotalsDF <- sensorSubtotalsFn(scoreSheetDF=scoreSheetDF,
                                outputName="OSS2SensorTotalsDF",
                                aggType="sum",
                                makeDF=makeDF,
                                saveCSV=saveCSV)
  
  ###########  construct a list to hold the OSS-2 result  ###########
  
  outputListName <- paste(examName, seriesName, "OSS2OutputList", sep="_")
  
  # need to output the question rotation
  
  OSS2OutputList <- list(OSS2="Objective Scoring System, version 2 (Krapohl & McManus 1999; Krapohl, 2002)",
                         examName=examName,
                         seriesName=seriesName,
                         OSS2Result=OSS2CategoricalResult,
                         OSS2QuestionResults=OSS2QuestionResults,
                         OSS2GrandTotal=OSS2GrandTotal,
                         OSS2PVal=OSS2PVal,
                         OSS2DecisionRule=OSS2DecisionRule,
                         OSS2Questions=uniqueQuestions,
                         OSS2RQNames=uniqueRQs,
                         OSS2AlphaT=oss2AlphaT,
                         OSS2AlphaD=oss2AlphaD,
                         OSS2CutScoreT=cutScoreT,
                         OSS2CutScoreD=cutScoreD,
                         OSS2ResultUsing=resultUsing,
                         OSS2Sensors=OSS2Sensors3,
                         OSS2QuestionSequence=questionSequenceDF,
                         OSS2RCRatios=RCScoresDF,
                         OSS2coreSheetDF=scoreSheetDF,
                         OSS2ChartTotalsDF=chartTotalsDF,
                         OSS2SeriesTotalsDF=seriesTotalsDF,
                         OSS2ensorTotalsDF=sensorTotalsDF,
                         OSS2Measurements=OSS2MeasurementsDF )
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, OSS2OutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "OSS2Output"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      OSS2OutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  # restore the sensor measurements
  RqCqDFSeries$sensorMeasurement <- saveSensorMeasurements
  
  return(RqCqDFSeries)
  
  # end OSS2ScoresFn
} 



