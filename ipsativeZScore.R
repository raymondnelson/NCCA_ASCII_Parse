# R function to compute the ipsative Z scores from the measurements data frame
# Raymond Nelson
# 12-5-2017
#
# replication of the algorithm from 2008
#
###








ipsativeZFn <- function(RqCqDFSeries=RqCqDFSeries, 
                        makeDF=TRUE,
                        saveCSV=FALSE,
                        analysisListName="analysisResultList" ) {
  # R function to compute the ipsative Z scores from the measurements data frame
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  #
  # input is a data frame of measurements for RQs and CQs in a series
  # output is the RqCqDFSeries data frame with the ipsative Z column populated 
  # 
  # ipsative-Z scores are calculated as leave-one-out
  # compare each value to the mean and population SD of the other values
  #
  ##################
  
  {
    
    print("calculate the ipsative Z score for all charts in the series")
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # if(seriesName=="4") stop()
    
    # reset the rankScore column
    RqCqDFSeries$ipZScore <- ""
    # View(RqCqDFSeries)
    
    # initialize a vector of unique sensor names for the exam
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    ipZSensors <- c("UPneumo", 
                    "LPneumo", 
                    "Pneumo",
                    "AutoEDA", 
                    # "ManualEDA", 
                    "Cardio", 
                    # "FC",
                    "PLE")
    
    # remove ipZ sensors not in the data 
    ipZSensors <- ipZSensors[ipZSensors %in% uniqueSensors]
    
    # exclude the PLE
    if("PLE" %in% ipZSensors && !isTRUE(includePLEScores)) {
      ipZSensors <- ipZSensors[-which(ipZSensors %in% "PLE")]
    }
    
    # initialize a vector of unique RQ and CQ events
    uniqueQuestionsE <- unique(RqCqDFSeries$eventLabel)
    uniqueQuestions <- unique(RqCqDFSeries$Label)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) < 4) { 
      return(RqCqDFSeries) 
    }
    
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
  
  #### constraints for extreme ipZ values ####
  
  {
    # pneumoConstraint <- 4.5
    # EDAConstraint <- 30
    # cardioConstraint <- 30
    # vasoConstraint <- 30
    
    pneumoConstraint <- 3
    EDAConstraint <- 3
    cardioConstraint <- 3
    vasoConstraint <- 3
    
    # pneumoConstraint <- 100
    # EDAConstraint <- 100
    # cardioConstraint <- 100
    # vasoConstraint <- 100
  }
  
  #### iterate over the charts ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    {
      
      chartName <- uniqueCharts[i]
      chartRows <- which(RqCqDFSeries$chartName == chartName)
      
      # initialize the chart data frame
      RqCqDFChart <- RqCqDFSeries[chartRows,]
      # View(RqCqDFChart)
      
      uniqueEventsChart <- unique(RqCqDFChart$eventLabel)
      
      # re-initialize a vector of unique sensor names for the series
      uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
      
      # next chart if less than 4 events
      if(length(uniqueEventsChart) < 4) next()
      
    }
    
    # iterate over the sensor names to calculate the leave-one-out ipsative z score
    j=6 # 6 is PLE
    for (j in 1:length(ipZSensors)) {
      
      {
        
        thisSensor <- ipZSensors[j]
        
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
        
        # Oct 4, 2021
        # rescale the respiration measurements 
        # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
        #   measurementVector <- round((1-exp(-measurementVector)) * 100, 3)
        #   # larger values now signify greater changes in physiology
        # }
        # 
        # zero as missing
        measurementVector[measurementVector == 1] <- NA
        
        # initialize a vector of ipZ values
        ipsativeVector <- rep(NA, times=length(measurementVector))
        
      }
      
      # iterate over the events for each sensor measurements
      # to calculate the z score for each event
      k=4
      for(k in 1:length(measurementVector)) {
        
        # leave one out and calculate the mean 
        thisMean <- mean(measurementVector[-k], na.rm=TRUE)
        if(is.na(thisMean)) {
          ipsativeVector[k] <- 0
          next()
        }
        # leave one out and calculate the sd for the population
        thisSDP <- sdp(measurementVector[-k])
        # check if the SD is 0 or NA
        if(thisSDP==0 || is.na(thisSDP)) {
          ipsativeVector[k] <- 0
          next()
        }
        # calculate the z value
        ipZ <- (measurementVector[k] - thisMean) / thisSDP
        # add the ipZ value to the sensorDF
        ipsativeVector[k] <- ipZ
        # print(paste(sensorDF$eventLabel[k], uniqueSensors[j], "z =", round(ipZ, 3)))
      
      } # end loop K over event measurements
        
      {
        
        #### check for extreme ipZScores here ####
        
        thisConstraint <- switch(ipZSensors[j],
                                 "UPneumo"=pneumoConstraint, 
                                 "LPneumo"=pneumoConstraint, 
                                 "Pneumo"=pneumoConstraint,
                                 "AutoEDA"=EDAConstraint, 
                                 "ManualEDA"=EDAConstraint, 
                                 "Cardio"=cardioConstraint, 
                                 "FC"=cardioConstraint,
                                 "PLE"=vasoConstraint )
        
        fixThese <- which(ipsativeVector >= thisConstraint)
        # ipsativeVector[fixThese] <- NA
        ipsativeVector[fixThese] <- thisConstraint
        
        fixThese <- which(ipsativeVector <= -thisConstraint)
        # ipsativeVector[fixThese] <- NA
        ipsativeVector[fixThese] <- -thisConstraint
        
      }
      
      # submit the ipsativeVector to the chart DF
      RqCqDFChart$ipZScore[sensorRows] <- ipsativeVector
      
    } # end loop j over sensors
    
    #### average the 2 pneumo sensors ####
    
    {
      
      pneumoRows <- which(RqCqDFChart$sensorName == "Pneumo")
      UpPneumoRows <- which(RqCqDFChart$sensorName == "UPneumo")
      LpPneumoRows <- which(RqCqDFChart$sensorName == "LPneumo")
      
      ipZUp <- as.numeric(RqCqDFChart$ipZScore[UpPneumoRows])
      ipZLp <- as.numeric(RqCqDFChart$ipZScore[LpPneumoRows])
      
      for(k in 1:length(pneumoRows)) {
        RqCqDFChart$ipZScore[pneumoRows][k] <- 
          round(mean(c(ipZUp[k], ipZLp[k]), na.rm=TRUE), 3)
      }
      
    }
    
    # View(RqCqDFChart)
    
    # pass the sensorDF to the RqCqDFChart
    RqCqDFSeries[chartRows,] <- RqCqDFChart
    # View(RqCqDFSeries)
    
  } # end loop i over charts
  
  #### initialize and populate a data frame of (leave-one-out) Z scores ####
  
  {
    
    ## initialize a data frame for the ipsative Z scores ##
    
    ipZScoresDF <- 
      data.frame(matrix(ncol=(length(uniqueQuestionsE)), 
                        nrow=length(uniqueCharts)*length(ipZSensors)))
    names(ipZScoresDF) <- uniqueQuestionsE
    ipZScoresDF <- 
      cbind(sensorName=rep(ipZSensors, times=length(uniqueCharts)), 
            ipZScoresDF)
    ipZScoresDF$sensorName <- 
      as.character(ipZScoresDF$sensorName)
    ipZScoresDF <- 
      cbind(chartName=rep(uniqueCharts, each=length(ipZSensors)), 
            ipZScoresDF)
    ipZScoresDF$chartName <- as.character(ipZScoresDF$chartName)
    # View(ipZScoresDF)
    
  }
  
  #### populate the data frame with the measurements ####
  
  {
    
    i=1
    for(i in 1:nrow(ipZScoresDF)) {
      thisChart <- ipZScoresDF[i,1]
      thisSensor <- ipZScoresDF[i,2]
      # iterate over the questions
      j=3
      for(j in 3:ncol(ipZScoresDF)) {
        thisQuestion <- names(ipZScoresDF)[j]
        # now get the measurement
        thisOne <- which(RqCqDFSeries$chartName==thisChart &
                           RqCqDFSeries$sensorName==thisSensor &
                           RqCqDFSeries$eventLabel==thisQuestion)
        if(length(thisOne) == 0 ) next()
        ipZScoresDF[i,j] <- 
          round(as.numeric(RqCqDFSeries$ipZScore[thisOne]), 2)
      }
    }
    
    # View(ipZScoresDF)
    # str(ipZScoresDF)
    
    # fix the column type
    for(n in 3:ncol(ipZScoresDF)) {
      ipZScoresDF[,n] <- as.numeric(ipZScoresDF[,n])
    }
  
  }
  
  #### save the ipZScoresDF to the global envir and cwd ####
  
  {
    
    # outputDFName <- paste(examName, seriesName, "ipZScoresDF", sep="_")
    # 
    # if(isTRUE(makeDF)) {
    #   assign(outputDFName, ipZScoresDF, pos=1)
    # }
    # 
    # if(isTRUE(saveCSV)) {
    #   write.csv(ipZScoresDF,
    #             file=paste0(str_sub(outputDFName, 1, -3), ".csv"),
    #             row.names=FALSE)
    # }
    
  }
  
  ###### compute the measurement table used for ipZ scores ######
  
  {
    
    ipZMeasurementSensors <- c("UPneumo", 
                               "LPneumo", 
                               "AutoEDA", 
                               "Cardio", 
                               "PLE")
    
    ipZMeasurementSensors <- 
      ipZMeasurementSensors[ipZMeasurementSensors %in% 
                              RqCqDFSeries$sensorName]
    
    # exclude the PLE
    if("PLE" %in% ipZMeasurementSensors && !isTRUE(includePLEScores)) {
      ipZMeasurementSensors <- ipZMeasurementSensors[-which(ipZMeasurementSensors %in% "PLE")]
    }
    
    ipZMeasurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=ipZMeasurementSensors,
                         decimals=2,
                         makeDF=FALSE,
                         saveCSV=saveCSV )
    # View(ipZMeasurementsDF)
    # str(ipZMeasurementsDF)
    
  }
  
  #### aggregate the ip Z scores for all questions in the series ####
  
  {
    
    ipZSensors2 <- c("Pneumo",
                     "AutoEDA", 
                     "Cardio", 
                     "PLE")
    
    # exclude the PLE
    if("PLE" %in% ipZSensors2 && !isTRUE(includePLEScores)) {
      ipZSensors2 <- ipZSensors2[-which(ipZSensors2 %in% "PLE")]
    }
    
    # reduce the sensors to include only the combined pneumo
    ipZScoresDF <- ipZScoresDF[ipZScoresDF$sensorName %in% ipZSensors2,]
    # View(ipZScoresDF)
    # str(ipZScoresDF)
    
    # fix the column type
    for(n in 3:ncol(ipZScoresDF)) {
      ipZScoresDF[,n] <- as.numeric(ipZScoresDF[,n])
    }
    
    # View(ipZScoresDF)
  }
  
  #### within chart mean ipZ scores ####
  
  {
    
    # initialize a data frame to hold the within chart mean ipZ scores
    ipZChartMeansDF <- 
      data.frame(matrix(ncol=(length(uniqueQuestionsE)), 
                        nrow=length(uniqueCharts)))
    names(ipZChartMeansDF) <- uniqueQuestionsE
    row.names(ipZChartMeansDF) <- uniqueCharts
    
    # initialize a vector to hold the between chart mean ipZ scorees
    # meanIpZScores <- rep(NA, ncol(ipZScoresDF) - 2)
    # names(meanIpZScores) <- colnames(ipZScoresDF)[3:ncol(ipZScoresDF)]
    
    # weight the EDA more than the other sensors
    ifelse(length(unique(ipZScoresDF$sensorName)) == 4,
           sensorWeights <- c(1, 2, 1, 1),
           sensorWeights <- c(1, 2, 1) )
    
    # iterate over the charts to calculate the within chart means
    n=1
    for(n in 1:length(uniqueCharts)) {
      thisChart <- uniqueCharts[n]
      theseRows <- which(ipZScoresDF$chartName == thisChart)
      # iterate over the RQ columns to average the sensors
      m=3
      for(m in 3:ncol(ipZScoresDF)) {
        ipZChartMeansDF[n,(m-2)] <- 
          weighted.mean(ipZScoresDF[theseRows,m], w=sensorWeights, na.rm=TRUE)
        
      }
    }
    # View(ipZChartMeansDF)
    
  }
    
  #### mean ipZ question Scores ####
  
  {
    
    ipZMeans <- round(colMeans(ipZChartMeansDF, na.rm=TRUE), 2)
    names(ipZMeans) <- uniqueQuestionsE
    
  }
  
  #### parse the questions for DLST/DLDT/PCASS formats ####
  
  if(isTRUE(DLSTType)) {
    
    names(ipZChartMeansDF) <- str_sub(names(ipZChartMeansDF), -2, -1)
    
    uniqueQuestions <- unique(names(ipZChartMeansDF))
    
    ipZMeans <- rep(NA, length=length(uniqueQuestions))
    
    for(o in 1:length(ipZMeans)) {
      theseCols <- which(names(ipZChartMeansDF) == uniqueQuestions[o])
      ipZMeans[o] <- 
        mean(colMeans(ipZChartMeansDF, na.rm=TRUE)[theseCols], na.rm=TRUE)
    }
    ipZMeans <- round(ipZMeans, 2)
    names(ipZMeans) <- uniqueQuestions
    
  }
    
  ######## get the question with the greatest Z score ########
  
  {
    maxMeanIpZ <- ipZMeans[which.max(ipZMeans)]
    maxmeanIpZQuestion <- uniqueQuestions[which.max(ipZMeans)]
  }
    
  #######################  categorical result section ####################
  
  {
    
    # this method uses only the greatest ipZ score

    # SR if RQ is the greatest ipZ score and NSR if not

    # check for 2 or more RQs and 2 or more CQs

    RQNames <- uniqueQuestions[grep("R", uniqueQuestions)]
    CQNames <- uniqueQuestions[grep("C", uniqueQuestions)]

    # get the results for all questions
    # ipZAlphaD <- .333
    # ipZAlphaD <- .49
    cutZScore <- -qnorm(ipZAlphaD)
    # ipZAlphaD is initialized in the NCCAASCII_init.R script

    if(length(RQNames) >= 2 && length(CQNames) >= 2) {

      SRResults <- maxmeanIpZQuestion

      testResult <- ifelse(grepl("R", SRResults),
                           "DI/SR",
                           "NDI/NSR" )

    } else {

      # exams without 2 RQs and 2 CQs

      SRResults <- names(ipZMeans)[which(ipZMeans >= cutZScore)]

      testResult <- ifelse(length(SRResults) > 0,
                           "DI/SR",
                           "NDI/NSR")

    }
    
  }
  
  #######################  categorical result section ####################
  
  {
    
    # # this method uses only the greatest ipZ score
    # 
    # # SR if RQ is the greatest ipZ score and NSR if CQ
    # 
    # # check for 2 or more RQs and 2 or more CQs
    # 
    # RQNames <- uniqueQuestions[grep("R", uniqueQuestions)]
    # CQNames <- uniqueQuestions[grep("C", uniqueQuestions)]
    # 
    # # get the results for all questions
    # # ipZAlphaD <- .333
    # # ipZAlphaD <- .49
    # cutZScore <- -qnorm(ipZAlphaD)
    # # ipZAlphaD is initialized in the NCCAASCII_init.R script
    # 
    # if(length(RQNames) >= 2 && length(CQNames) >= 2) {
    #   
    #   SRResults <- maxmeanIpZQuestion
    #   
    #   testResult <- ifelse(grepl("R", SRResults),
    #          "DI/SR",
    #          ifelse(grepl("C", SRResults),
    #                 "NDI/NSR",
    #                 "INC/NO" ) )
    #   
    # } else {
    #   
    #   # exams without 2 RQs and 2 CQs
    #   
    #   SRResults <- names(ipZMeans)[which(ipZMeans >= cutZScore)]
    #   
    #   testResult <- ifelse(length(SRResults) > 0, 
    #                        "DI/SR",
    #                        "NDI/NSR")
    #   
    # }
    
  }
  
  #######################  categorical result section ####################
  
  {
    
    # # this method checks each RQ
    # 
    # # SR if any RQ is statistically significant, NSR if CQ
    # 
    # # get the results for all questions
    # # ipZAlphaD <- .333
    # # ipZAlphaD <- .49
    # cutZScore <- -qnorm(ipZAlphaD)
    # # ipZAlphaD is initialized in the NCCAASCII_init.R script
    # 
    # SRResults <- names(ipZMeans)[which(ipZMeans >= cutZScore)]
    # 
    # # check for relevant questions
    # if(length(which(grepl("R", SRResults))) > 0) {
    #   # keep only RQs if any RQs are SR
    #   SRResults <- SRResults[which(grepl("R", SRResults))]
    # } else if(length(which(grepl("C", SRResults))) > 0) {
    #   # check for CQs
    #   SRResults <- SRResults[which(grepl("C", SRResults))]
    # } else {
    #   # keep 'none' if none of the SR questions are RQ or CQ
    #   # if there are RQs and none of the SR questions are RQ or CQ
    #   if(length(which(grepl("R", names(ipZMeans)))) > 0) {
    #     SRResults <- "none"
    #   }
    #   # will keep any SR questions if there are no RQs in the series
    # }
    # 
    # # SRResults will be > 0 and not "none"
    # # there are no RQs and any question is SR
    # 
    # # default
    # testResult <- "INC/NO"
    # 
    # if(all(SRResults != "none", length(SRResults) > 0, length(which(grepl("R", SRResults))) > 0)) {
    #   # if the series includes RQs and any RQs are SR
    #   testResult <- "DI/SR"
    # } else if(all(SRResults != "none", length(SRResults) > 0, length(which(grepl("R", names(ipZMeans)))) > 0)) {
    #   # if the series includes any RQs ad none of the RQs are SR
    #   if(length(which(grepl("R", SRResults))) == 0 &&
    #      length(which(grepl("C", SRResults))) > 0) {
    #     # if no RQs are SR and at least 1 CQ is SR
    #     testResult <- "NDI/NSR"
    #   }
    # } else if(SRResults != "none") {
    #   # if there are no RQs in the series
    #   # and some questions are significant
    #   testResult <- "DI/SR"
    # } else {
    #   # test result will be inconclusive
    #   # if there are RQs and none of the SR questions are RQ or CQ
    #   testResult <- "INC/NO"
    # }

  }
  
  #### save the ipZScoresDF to the global envir and cwd ####
  
  {
    
    outputDFName <- paste(examName, seriesName, "ipZScoresDF", sep="_")
    
    if(isTRUE(makeDF)) {
      assign(outputDFName, ipZScoresDF, pos=1)
    }
    
    saveCSV <- TRUE
    
    if(isTRUE(saveCSV)) {
      write.csv(ipZScoresDF,
                file=paste0(str_sub(outputDFName, 1, -3), ".csv"),
                row.names=FALSE)
    }
    
  }
  
  ############ sensor means #############
  
  {
    
    # ipZScoresDF
    # ncol(ipZScoresDF)
    # ipZSensors2
    
    ipZSensorMeansDF <- 
      as.data.frame(matrix(nrow=length(ipZSensors2), ncol=(ncol(ipZScoresDF)-1)))
    names(ipZSensorMeansDF) <- names(ipZScoresDF)[2:ncol(ipZScoresDF)]
    
    ipZSensorMeansDF$sensorName <- ipZSensors2
    
    # str(ipZSensorMeansDF)
    for(m in 2:ncol(ipZSensorMeansDF)) {
      ipZSensorMeansDF[,m] <- as.numeric(ipZSensorMeansDF[,m])
    }
    
    # interate over the sensors and populate the data frame
    m=1
    for(m in 1:nrow(ipZSensorMeansDF)) {
      theseSensorRows <- which(ipZScoresDF$sensorName == ipZSensors2[m])
      ipZSensorMeansDF[m,2:ncol(ipZSensorMeansDF)] <- 
        colMeans(ipZScoresDF[theseSensorRows,3:ncol(ipZScoresDF)], na.rm = TRUE)
    }
    # View(ipZSensorMeansDF)
    
  }
  
  ########## RQ and CQ means ###########
  
  if( length(grep("R", names(ipZSensorMeansDF))) >= 2 &&
      length(grep("C", names(ipZSensorMeansDF))) > 2 ) {
    
    # ipZScoresDF
    # ncol(ipZScoresDF)
    # ipZSensors2
    # ipZSensorMeansDF
    
    # Dec 1, 2023 fix NA values 
    ipZRQCQMeans <- colMeans(ipZSensorMeansDF[2:ncol(ipZSensorMeansDF)], na.rm=TRUE)
    
  } else { 
    
    # for non-CQT charts
    ipZRQCQMeans <- "none"
    
  }
  
  ########################## output section ###########################
    
  #### construct a list to hold the rank score results ####
  
  {
  
    outputListName <- paste(examName, seriesName, "ipZOutputList", sep="_")
    
    ipZOutputList <- list(ipZ="Ipsative, leave-one-out, Z Analysis (Nelson, 2008)",
                          examName=examName,
                          seriesName=seriesName,
                          testResult=testResult,
                          sigQuestions=SRResults,
                          ipZQuestions=uniqueQuestions,
                          ipZMeans=ipZMeans,
                          maxMeanIpZ=maxMeanIpZ,
                          maxMeanIpZQuestion=maxmeanIpZQuestion, 
                          ipZRQCQMeans=ipZRQCQMeans,
                          ipZSensorMeansDF=ipZSensorMeansDF,
                          ipZSensors=ipZMeasurementSensors,
                          ipZScoresDF=ipZScoresDF,
                          ipZMeasurementsDF=ipZMeasurementsDF )
  
  } 
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, ipZOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "ipsativeZAnalysisOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      ipZOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(RqCqDFSeries)
  
} # end ipsativeZFn()



