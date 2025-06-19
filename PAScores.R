# R Script to calculate Probability Analysis score and results
# Feb 1, 2019
# Raymond Nelson
#
###########################




# # some test data 2/6/2019
# library(readr)
# PATestData20190206 <- read_csv(paste0(RPath, "PATestData20190206.csv"))
# View(PATestData20190206)


# library(readr)
# PATest_RqCqSeriesDF20190206 <- read_csv(paste0(RPath, "PATest_RqCqSeriesDF20190206.csv"))
# View(PATest_RqCqSeriesDF20190206)

# RqCqDFSeriesSAVE <- RqCqDFSeries
# RqCqDFSeries <- as.data.frame(PATest_RqCqSeriesDF20190206)

# save.image()

# RqCqDFSeries <- RqCqDFSeriesSAVE
# View(RqCqDFSeries)


# requires the grand total decision rule
# source(paste0(RPath, 'decisionRules.R'), echo=FALSE)


# requires the PA reference model
source(paste0(RPath, 'PAModel.R'), echo=FALSE)



PAScoresFn <- function(RqCqDFSeries=RqCqDFSeries, 
                       forced=TRUE,
                       PADecisionRule=PADecisionRule,
                       PAPrior=.5,
                       PACutProbT=.7,
                       PACutProbD=.3,
                       EDA="auto",
                       makeDF=FALSE,
                       saveCSV=FALSE,
                       analysisListName="analysisResultList" ) {
  # R function to compute the Probability Analysis results 
  # from the measurements data frame
  # sourced by the getScoresFn in the scores.R script
  # Raymond Nelson
  # 2019
  #
  ###
  #
  # requires the PA reference model
  # 
  # input is a data frame of RQ and CQ measurements for a series (all charts)
  # including all test charts with RQs and CQs
  # and all sensors (upper and lower respiration, electrodermal, cardio)
  #
  # output is the RqCqDFSeries data frame with the PAScore column populated 
  # additional output is via side effect
  # 
  # PA scores are calculated using within-test z-scores
  #
  # PA results are calculated at one time using all measurements from all charts
  # this is different from other algorithms that evaluate one chart at a time
  # before aggregating the scores for the charts
  # 
  ####
  #
  # 1) calculate the mean and st-dev for each sensor within each chart
  # 2) average the sensor means and st dev values between charts
  # 3) calculate the z-score for each measurement for all sensors and all charts
  #    using the between chart sensor means and st dev
  # 4) reduce the 2 respiration z scores to 1 for each stimulus presentation
  # 5) calculate the between chart sensor mean z score for each CQ
  # 6) calculate the between CQ means of the between chart CQ sensor mean z scores
  # 7) calculate the between chart RQ mean z score for each sensor
  # 8) calculate the between RQ means of the between chart mean zscores for sensors
  # 9) calculate the difference between the CQ and RQ mean z score for each sensor
  # 10) calculate the product 
  #     of the discriminate coefs and the  difference z score for each sensor
  # 11) sum the products to achieve a single discriminate score for the exam
  # 12) calculate the likelihood for truthtelling
  #     EXP(-0.5 * ( (DS-TMean) / TStDeve )^2) / ( SQRT(2*pi) * TStDev )
  # 13) calculate the likelihood for deception
  #     EXP(-0.5 * ( (DS-DMean) / DStDeve )^2) / ( SQRT(2*pi) * DStDev )
  # 14) then use Bayes theorem to calculate the posterior likelihood of truth
  #     ((1-prior)*Tlikelihood)/((1-prior)*Tlikelihood+prior*Dlihood)
  # 15) use Bayes theorem again to calculate the posterior likelihood of deception
  #     ((1-prior)*Dlikelihood)/((1-prior)*Tlikelihood+prior*TLikelihood)
  # 16) select the maximum likelihood from #14 and #15
  # 17) determine the categorical results DI/INC/NDI using the cutProb
  #
  ####
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    print("calculate the Probability Analysis scores")
    
    # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
    
    # View(RqCqDFSeries)
    
    if(!exists("forced")) forced=TRUE
    if(!exists("PADecisionRule")) PADecisionRule="GTR"
    if(!exists("PAPrior")) PAPrior=.5
    if(!exists("PACutProbT")) PACutProbT=.7
    if(!exists("PACutProbD")) PACutProbD=.3
    if(!exists("EDA")) EDA="auto"
    if(!exists("makeDF")) makeDF=FALSE
    if(!exists("saveCSV")) saveCSV=FALSE
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    # PA does not work with the data at the individual chart level
    
    if( (length(uniqueCharts) < 3 || length(uniqueCharts) > 5) && 
        forced==FALSE ) { 
      "Probability Analysis requires 3 to 5 charts"
      return(RqCqDFSeries) 
    }
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    PASensors <- c("UPneumo", 
                   "LPneumo", 
                   "Pneumo", 
                   "AutoEDA", 
                   "ManualEDA", 
                   # "FC", 
                   "Cardio" )
    
    PASensors <- PASensors[PASensors %in% uniqueSensors]
    
    # exit if PA Sensors are missing
    if(length(which(!(PASensors %in% uniqueSensors))) > 2) {
      return(RqCqDFSeries)
    }
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      print("no events in this chart")
      return(RqCqDFSeries) 
    }
    
  }
  
  #### slice the RQs and CQs ####
  
  {
    
    rqRows <- grep("R", RqCqDFSeries$eventLabel)
    cqRows <- grep("C", RqCqDFSeries$eventLabel)
    
    # exit if there are no RQs or no CQs
    if(length(rqRows) == 0 || length(cqRows) == 0) { 
      return(RqCqDFSeries) 
    }
    
    uniqueRQs <- unique(RqCqDFSeries[rqRows,'eventLabel'])
    uniqueCQs <- unique(RqCqDFSeries[cqRows,'eventLabel'])
    
    rqDFSeries <- RqCqDFSeries[rqRows,]
    cqDFSeries <- RqCqDFSeries[cqRows,]
    # View(rqDFSeries)
    # View(cqDFSeries)
    
    # assign("rqDFSeries", rqDFSeries, pos=1)
    # assign("cqDFSeries", cqDFSeries, pos=1)
    
  }
  
  ### initialize a data frame with the question sequence for each chart ######

  # questionSequenceDF <- questionSequenceFn(RqCqDFSeries=RqCqDFSeries,
  #                                         outputName="PAQuestionSequence",
  #                                         makeDF=FALSE,
  #                                         saveCSV=FALSE)
  # 
  
  ##################   PA reference model   ###################
  
  # already sourced by the workFlow.R script
  
  # {
  #   
  #   # source(paste0(RPath, 'PAModel.R'), echo=FALSE)
  #   
  #   # from Nelson (2008) replication of the PA algorotihm
  #   # discriminate scores were calculated using SPSS
  #   PACoefs <- c(R=0.629, E=1.735, C=0.92)
  #   # normalized coefs
  #   # PACoefs <- c(R=0.192, E=0.528, C=0.280)
  #   
  #   # 1988 USSS field study
  #   # PANorms <- c(E=.53, C=.30, R=.17)
  #   
  #   # descriptives were calculated using the OSS-3 development sample
  #   PANorms <- c(DMean=1.913, DStDev=1.012, TMean=-1.009, TStDev=1.131)
  #   
  #   if(!exists("PACutProbT")) PACutProbT <- .7
  #   if(!exists("PACutProbD")) PACutProbD <- .3
  #   
  # }
  
  #### calculate the between chart means and StDevs for the sensor measurements ####
  
  {
    
    sensorMeans <- rep(NA, times=length(PASensors))
    names(sensorMeans) <- PASensors
    
    sensorSDs <- rep(NA, times=length(PASensors))
    names(sensorSDs) <- PASensors
    
    # iterate over the sensors to get the means and SDs
    i=1
    for(i in 1:length(PASensors)) {
      theseRows <- which(RqCqDFSeries$sensorName==PASensors[i])
      sensorMeans[i] <- mean(RqCqDFSeries$sensorMeasurement[theseRows], na.rm=TRUE)
      sensorSDs[i] <- sd(RqCqDFSeries$sensorMeasurement[theseRows], na.rm=TRUE)
    }
    
  }
  
  #### standardize all measurements to the between chart sensor parameters ####
  
  {
    
    # first get the sensor measurements 
    PAMeasurementsDF <- measurementTableFn(RqCqDFSeries=RqCqDFSeries, 
                                           useSensors=PASensors,
                                           decimals=2,
                                           makeDF=makeDF,
                                           saveCSV=saveCSV )
    # PAMeasurementsDF <- cbind(examName, seriesName, PAMeasurementsDF)
    # str(PAMeasurementsDF)
    
    # View(PAMeasurementsDF)
    
    # tben initialize another data frame for the z scores
    PAZScoresDF <- PAMeasurementsDF
    
    # assign("PAZScoresDF", PAZScoresDF, pos=1)
    
    # iterate over the data frame rows and calculate the z scores
    i=4
    for(i in 1:nrow(PAZScoresDF)) {
      thisSensor <- PAZScoresDF[i,'sensorName']
      # iterate over the columns (questions)
      j=5
      for(j in 5:ncol(PAZScoresDF)) {
        thisMeasurement <- PAMeasurementsDF[i,j]
        thisSensorMean <- sensorMeans[names(sensorMeans)==thisSensor]
        thisSensorSD <- sensorSDs[names(sensorSDs)==thisSensor]
        # calculate the z score
        thisZScore <- (thisMeasurement - thisSensorMean) / thisSensorSD
        # invert the sign of EDA and cardio measurements
        if(thisSensor %in% c("AutoEDA", "ManualEDA", "Cardio")) {
          thisZScore <- -thisZScore
        }
        PAZScoresDF[i,j] <- round(thisZScore, 2)
        # PAZScoresDF[i,]
      }
    }
    # View(PAZScoresDF)
    
    ###### extreme values ##########
    
    # mark extreme values
    for(j in 1:nrow(PAZScoresDF)) { 
      theseZVals <- PAZScoresDF[j,5:ncol(PAZScoresDF)]
      theseZVals <- which(theseZVals >= 4 | theseZVals <= -4)
      PAMeasurementsDF[j,5:ncol(PAZScoresDF)][theseZVals] <- NA
    }
    
    # then recalculate the Z Scores without the extreme values
    i=4
    for(i in 1:nrow(PAZScoresDF)) {
      thisSensor <- PAZScoresDF[i,'sensorName']
      # iterate over the columns (questions)
      j=5
      for(j in 5:ncol(PAZScoresDF)) {
        thisMeasurement <- PAMeasurementsDF[i,j]
        thisSensorMean <- sensorMeans[names(sensorMeans)==thisSensor]
        thisSensorSD <- sensorSDs[names(sensorSDs)==thisSensor]
        # calculate the z score
        thisZScore <- (thisMeasurement - thisSensorMean) / thisSensorSD
        # invert the sign of EDA and cardio measurements
        if(thisSensor %in% c("AutoEDA", "ManualEDA", "Cardio")) {
          thisZScore <- -thisZScore
        }
        PAZScoresDF[i,j] <- round(thisZScore, 2)
        # PAZScoresDF[i,]
      }
    }
    
    # View(PAZScoresDF)
  
  }
  
  ######## combine the upper and lower respiration by averaging ########
  
  {
    
    # first add the Pneumo rows by iterating over the LPneumow rows
    if(length(which(PAZScoresDF$sensorName=="Pneumo"))==0) {
      LPneumoRows <- which(PAZScoresDF$sensorName=="LPneumo")
      i=3
      # iterate backward to avoid problems with row numbers
      for(i in length(LPneumoRows):1) {
        PAZScoresDF <- rbind(PAZScoresDF[1:LPneumoRows[i],],
                             PAZScoresDF[LPneumoRows[i],],
                             PAZScoresDF[(LPneumoRows[i]+1):nrow(PAZScoresDF),]
        )
        # rename the added sensor row
        PAZScoresDF$sensorName[(LPneumoRows[i]+1)] <- "Pneumo"
      }
    }
    # View(PAZScoresDF)
    
    # iterate over the questions to combine the UPneumo and LPneumo scores
    i=3
    for(i in 5:ncol(PAZScoresDF)) {
      # exit if pneumos are already reduced
      if(all(PAZScoresDF$sensorName %in% c("UPneumo", "LPneumo"))) next()
      # select the question column
      thisquestion <- names(PAZScoresDF)[i]
      # iterate over the charts to reduce the pneumo scores
      j=1
      for(j in 1:length(uniqueCharts)) {
        # get the chart name 
        thisChart <- uniqueCharts[j]
        # get the upper and lower measurements
        UPneumoRow <- which(PAZScoresDF$chartName==thisChart &
                              PAZScoresDF$sensorName=="UPneumo")
        LPneumoRow <- which(PAZScoresDF$chartName==thisChart &
                              PAZScoresDF$sensorName=="LPneumo")
        # # exit if either pneumo is NA
        # if(any(is.na(PAZScoresDF[UPneumoRow,i]),
        #         is.na(PAZScoresDF[LPneumoRow,i]))) next()
        # calculate the mean
        PneumoMeanZ <- mean(c(PAZScoresDF[UPneumoRow,i], 
                              PAZScoresDF[LPneumoRow,i]), na.rm=TRUE)
        # sumbit the mean to the data frame
        thisPneumoRow <- which(PAZScoresDF$chartName==thisChart &
                                 PAZScoresDF$sensorName=="Pneumo")
        PAZScoresDF[thisPneumoRow,i] <- round(PneumoMeanZ, 2)
        # PAZScoresDF[LPneumoRow,i] <- NA
      }
    }
    # View(PAZScoresDF)
    
  }
  # mean(rowMeans(PAZScoresDF[3:ncol(PAZScoresDF)], na.rm=TRUE))
  
  #### submit the PA z scores to the RqCqSeries data frame ####
  
  {
    
    # iteratate over the PAZScoresDF rows
    i=1
    for(i in 1:nrow(PAZScoresDF)) {
      thisChart <- PAZScoresDF[i,3]
      thisSensor <- PAZScoresDF[i,4]
      # if(thisSensor=="Pneumo") next()
      # iterate over the questions/columns
      j=3
      for(j in 5:ncol(PAZScoresDF)) {
        thisQuestion <- names(PAZScoresDF)[j]
        # now get the measurement
        thisOne <- which(RqCqDFSeries$chartName==thisChart &
                           RqCqDFSeries$sensorName==thisSensor &
                           RqCqDFSeries$eventLabel==thisQuestion)
        if(length(thisOne) == 0 ) next()
        RqCqDFSeries$PAScore[thisOne] <- PAZScoresDF[i,j]
      }
    }
    
    # View(RqCqDFSeries)
    
  }

  #### select the PA sensors for scoring and output ####
  
  {
    
    ifelse(EDA=="auto",
           # ifelse returns only a scalar
           # assignment here give a vector
           PASensors2 <- c("Pneumo", "AutoEDA", "Cardio"),
           PASensors2 <- c("Pneumo", "ManualEDA", "Cardio") )
    
  }
  
  #### get the between chart mean RQ z-scores for each sensor ####
  
  {
    
    {
      # initialize a data frame for the RQs
      RQCols <- which(names(PAZScoresDF) %in% uniqueRQs)
      RQZScoreDF <- PAZScoresDF[,c(1:4, RQCols)]
      # View(RQZScoreDF)
      
      # initialize a vector to hold the RQ sensor means
      RQMeanSensorZScores <- data.frame(matrix(ncol=(length(uniqueRQs)), 
                                               nrow=length(PASensors2)))
      names(RQMeanSensorZScores) <- uniqueRQs
      row.names(RQMeanSensorZScores) <- PASensors2
      
      # initialize a vector to hold the sensor means
      meanSensorZ <-  rep(NA, length=length(PASensors2))
      names(meanSensorZ) <- PASensors2
      # iterate over the RQs to average the sensor means beween charts
      i=3
      for(i in 5:ncol(RQZScoreDF)) {
        meanSensorZ[1] <- 
          mean(RQZScoreDF[RQZScoreDF$sensorName==PASensors2[1],i], na.rm=TRUE)
        meanSensorZ[2] <- 
          mean(RQZScoreDF[RQZScoreDF$sensorName==PASensors2[2],i], na.rm=TRUE)
        meanSensorZ[3] <- 
          mean(RQZScoreDF[RQZScoreDF$sensorName==PASensors2[3],i], na.rm=TRUE)
        
        # submit the RQ values to the data frame
        RQMeanSensorZScores[,names(RQMeanSensorZScores)==names(RQZScoreDF)[i]] <- 
          meanSensorZ
      }
      # View(RQMeanSensorZScores)
    }
    
  }
  
  #### get the between chart mean CQ z-scores for each sensor ####
  
  {
    
    {  
      # initialize a data frame for the CQs
      CQCols <- which(names(PAZScoresDF) %in% uniqueCQs)
      CQZScoreDF <- PAZScoresDF[,c(1:4, CQCols)]
      # View(CQZScoreDF)
      
      # initialize a vector to hold the CQ sensor means
      CQMeanSensorZScores <- data.frame(matrix(ncol=(length(uniqueCQs)), 
                                               nrow=length(PASensors2)))
      names(CQMeanSensorZScores) <- uniqueCQs
      row.names(CQMeanSensorZScores) <- PASensors2
      
      # initialize a vector to hold the sensor means
      meanSensorZ <-  rep(NA, length=3)
      
      # iterate over the CQs to average the sensor means beween charts
      i=4
      for(i in 5:ncol(CQZScoreDF)) {
        meanSensorZ[1] <- 
          mean(CQZScoreDF[CQZScoreDF$sensorName==PASensors2[1],i], na.rm=TRUE)
        meanSensorZ[2] <- 
          mean(CQZScoreDF[CQZScoreDF$sensorName==PASensors2[2],i], na.rm=TRUE)
        meanSensorZ[3] <- 
          mean(CQZScoreDF[CQZScoreDF$sensorName==PASensors2[3],i], na.rm=TRUE)
        
        # submit the CQ values to the data frame
        CQMeanSensorZScores[,names(CQMeanSensorZScores)==names(CQZScoreDF)[i]] <- 
          meanSensorZ
      }
      # View(CQMeanSensorZScores)
    }
    
  }
  
  #### calculate the between question RQ and CQ means for each sensor ####
  
  {
    
    # use apply mean for each sensor row in the data frames for for CQs and RQs
    
    CQMeanZSensors <- apply(CQMeanSensorZScores, 1, mean, na.rm=TRUE)

    RQMeanZSensors <- apply(RQMeanSensorZScores, 1, mean, na.rm=TRUE)
    
    # we now have the between question means for each sensor for CQs
        
  }
  
  #### calclate the CQmeanZ - RQmeanZ difference for each sensor ####
  
  {
    
    meanZDiffSensors <- CQMeanZSensors - RQMeanZSensors
    
    # also get the difference CQ-RQ difference for each sensor within each RQ
    i=1
    RQMeanZDiffSensors <- RQMeanSensorZScores
    for(i in 1:ncol(RQMeanZDiffSensors)) {
      RQMeanZDiffSensors[,i] <- CQMeanZSensors - RQMeanSensorZScores[,i]
    }
    
    # round it for output
    CQMeanZSensors <- round(CQMeanZSensors, 3)
    RQMeanZSensors <- round(RQMeanZSensors, 3)
    
  }
  
  ######## calculate the PA discriminate score ########
  
  {
    
    # first multiply the mean Z sensor score by the discriminate function
    # PACoefs are from the 1988 Secret Service field study
    # and are sourced in the PAModel.R script
    weightedSensorScores <- meanZDiffSensors * PACoefs
    
    # then sum the weighted sensor scores
    discriminateScore <- 
      round(sum(weightedSensorScores, na.rm=TRUE), 3)
    # this is the likelihood function for Bayesian analysis
    
  }
  
  {
    
    # also get the weighted RQ scores
    
    weightedRQSensorScores <- RQMeanZDiffSensors
    i=1
    for(i in 1:ncol(RQMeanZDiffSensors)) {
      weightedRQSensorScores[,i] <- RQMeanZDiffSensors[,i] * PACoefs
    }
    
    # this will be equal to the discriminate score
    mean(colSums(weightedRQSensorScores, na.rm=TRUE))
    
  }
  
  #### calculate the maximum likelihood score for deception and truth ####
  
  {
    
    # MLE Truthful
    # EXP(-0.5 * ( (DS-TMean) / TStDeve )^2) / ( SQRT(2*pi) * TStDev )
    pTruthful <- exp(-0.5 * ( (discriminateScore-PANorms['TMean']) / 
                                PANorms['TStDev'] )^2) / 
      ( sqrt(2*pi) * PANorms['TStDev'] )
    
    # MLE Deceptive
    # EXP(-0.5 * ( (DS-DMean) / DStDeve )^2) / ( SQRT(2*pi) * DStDev )
    pDeceptive <- exp(-0.5 * ( (discriminateScore-PANorms['DMean']) / 
                                 PANorms['DStDev'] )^2) / 
      ( sqrt(2*pi) * PANorms['DStDev'] )
    
    # 
    
    # pTruthful <- round(pTruthful, 3)
    # pDeceptive <- round(pDeceptive, 3)
    
  }
  
  #### calculate the Bayesian posterior likelihood ####
  
  {
    
    # priorP <- .5
    
    # P(X|T)
    postTruthful <- ((1-PAPrior)*pTruthful) / 
      ((1-PAPrior)*pTruthful+PAPrior*pDeceptive)
    
    # P(X|D)
    postDeceptive <- ((1-PAPrior)*pDeceptive) /
      ((1-PAPrior)*pDeceptive+PAPrior*pTruthful)
    
    postTruthful <- ifelse(postTruthful > .999,
                           .999,
                           ifelse(postTruthful < .001,
                                  .001,
                                  round(postTruthful, 3) ) )
    
    postDeceptive <- ifelse(postDeceptive > .999,
                            .999,
                            ifelse(postDeceptive < .001,
                                   .001,
                                   round(postDeceptive, 3) ) )
    
    # these two values are complimentary
    # and only 1 value is used to classify the test result
    
  }
 
  #### calculate the categorical result using the probability cutpoints ####
  
  {
    
    cutScores <- c(GTDI=PACutProbD, GTNDI=PACutProbT)
    
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    # call the private function to use the grand total rule
    GTRResult <- GTRFn(totalScore=postTruthful, 
                       RQNames=uniqueRQs,
                       cutScores=cutScores, 
                       flip=FALSE )
    # using only the postTruthful value 
    # because it is the compliment of postDeceptive
    
    PACategoricalResult <- GTRResult$testResult
    
    PAQuestionResults <- GTRResult$subtotalResults
    
    resultUsing <- GTRResult$resultUsing
    
  }

  ############################### output ###############################
 
  # #### save the PA series z-scores to a data frame and csv
  # 
  # scoreSheetName <- paste(examName, seriesName, "PAZScoresheetDF", sep="_")
  # 
  # if(isTRUE(makeDF)) {
  #   assign(scoreSheetName, PAZScoresDF, pos=1)
  # }
  # 
  # if(isTRUE(saveCSV)) {
  #   write.csv(PAZScoresDF,
  #             file=paste0(str_sub(scoreSheetName, 1, -3), ".csv"),
  #             row.names=FALSE)
  # }
  # 
  # #### save the PA measurements to a data frame and csv
  # 
  # measurementTableName <- paste(examName, seriesName, "PAMeasurementsDF", sep="_")
  # 
  # if(isTRUE(makeDF)) {
  #   assign(measurementTableName, PAMeasurementsDF, pos=1)
  # }
  # 
  # if(isTRUE(saveCSV)) {
  #   write.csv(PAMeasurementsDF,
  #             file=paste0(str_sub(measurementTableName, 1, -3), ".csv"),
  #             row.names=FALSE)
  # }
  
  ################ construct a list to hold the PA result ################
  
  ifelse(EDA=="auto",
         PAMeasurementSensors <- c("UPneumo", 
                                   "LPneumo", 
                                   "AutoEDA", 
                                   "Cardio" ),
         PAMeasurementSensors <- c("UPneumo", 
                                   "LPneumo", 
                                   "ManualEDA", 
                                   "Cardio" ) )
  
  
  PAMeasurementsDF <- 
    PAMeasurementsDF[which(PAMeasurementsDF$sensorName %in% PAMeasurementSensors),]
  
  # write.csv(PAMeasurementsDF, 
  #           file=paste0(examName, "_PAMeasurements.csv"),
  #           row.names=FALSE)
  
  PAZScoresDF <- 
    PAZScoresDF[which(PAZScoresDF$sensorName %in% PASensors2),]
  
  outputListName <- paste(examName, seriesName, "PAOutputList", sep="_")
  
  PAOutputList <- list(ProbabilityAnalysis="Probability Analysis (Kircher & Raskin, 1988; Raskin, Kircher, Honts & Horowitz, 1988)",
                       examName=examName,
                       seriesName=seriesName,
                       PAResult=PACategoricalResult,
                       PAQuestionResults=PAQuestionResults,
                       PAQuestions=uniqueQuestions,
                       PARQNames=uniqueRQs,
                       PAPostProbT=postTruthful,
                       PAPostProbD=postDeceptive,
                       PAScore=discriminateScore,
                       MLETruthful=pTruthful,
                       MLEDeceptive=pDeceptive,
                       PADecisionRule=PADecisionRule,
                       PAResultUsing=resultUsing,
                       PACQMeanZ=CQMeanZSensors,
                       PARQMeanZ=RQMeanZSensors,
                       PACoefs=PACoefs,
                       PAPrior=PAPrior ,
                       PACutProbD=PACutProbD,
                       PACutProbT=PACutProbT,
                       PASensors=PAMeasurementSensors,
                       # PAQuestionSequence=questionSequencDF,
                       PAZScores=PAZScoresDF,
                       PAMeasurements=PAMeasurementsDF )
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, PAOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "PAOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      PAOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  # save the list to the globad env as a side effect
  
  # sensorMeans for the RqCqSeries
  # sensorSDs for the RqCqSeries
  # sensorMeans and StDev for RQs and CQ
  
  # sensorZScoresDF by RQ
  # RQZScores weighted 
  
  #### visible output ####
  
  return(RqCqDFSeries)
  
  # end PAScoresFn()
  
}



