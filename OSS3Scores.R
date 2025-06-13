# Objective Scoring System version 3
# Nelson Krapohl & Handler (2008)
#
# replication of the Excel prototype
####



{

  # already sourced by the decisionRules.R script
  # source(paste0(RPath, 'KWANOVA.R'), echo=FALSE)
  
  source(paste0(RPath, 'OSS3Model.R'), echo=FALSE)
  
  source(paste0(RPath, 'OSS3ChannelContributions.R'), echo=FALSE)
  
  # requires the grand total decision rule
  # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)

  # for the table output
  # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
  
  # RqCqDFSeries$examName[1]
  
}



fixDLSTDLDTFn <- function(saveQuestionLabels=saveQuestionLabels, 
                          CQRQLabels=CQRQLabels) {
  # a function to fix the DLST DLDT and PCASS question labels
  # saveQuestionLabels is a vector with the original question labels
  # RQLabels is a vector of DLST/DLDT/PCASS type RQ labels
  # output is a vector of corrected question labels
  # of the same length to replace the original question labels
  newQuestionLabels <- saveQuestionLabels
  fixThese <- which(newQuestionLabels %in% CQRQLabels)
  if(length(fixThese)==0) return(saveQuestionLabels)
  newQuestionLabels[fixThese] <- 
    # requires the stringr package
    str_sub(newQuestionLabels[fixThese], 2, -1)
  
  return(newQuestionLabels)
}



##################### main function ################################



OSS3ScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                         OSS3Alpha=OSS3Alpha,
                         OSS3DecisionRule="auto",
                         minPresentations=1,
                         # Oct 16, 2023 added minSensorScores input parameter
                         minSensorScores=1,
                         makeDF=FALSE,
                         saveCSV=FALSE,
                         analysisListName=NULL,
                         saveAnalysisTXT=FALSE ) {
  # R function to compute the OSS-3 results from the measurements data frame
  # sourced by the getScoresFn in the scores.R script
  # Raymond Nelson
  # 2019
  #
  ###
  #
  # requires the OSS-3 reference tables
  # source('OSS3Model.R')
  # 
  # input RqCqDFSeries is a data frame of measurements for the RQs and CQs for a test chart
  #
  # OSS3Alpha = c(oss3AlphaTDiag=.05, oss3AlphaDDiag=.05, oss3AlphaTScreen=.05, oss3AlphaDScreen=.05)
  # OSS3DecisionRule default is TSR but can be SSR or GTR or SCN
  # makeDF
  # saveCSV
  # analysisListName is the name of the list that holds the analytic result "_ANALYSIS"
  #
  # output is the RqCqDF data frame with the OSS-3 score column populated 
  # 
  # OSS-3 scores are calculated using the Ratio of each RQ and mean of all CQs
  #
  ####
  #
  #    combine the 2 respiration sensor to a single score 
  #    oposite signs = 0, else select signed value with the greater abs value
  # 1) calculate the logged R/Cmean ratios for each sensor within each chart
  # 2) standardize the logged R/Cmean ratios to the norm data for each sensor
  # 3) for each RQ calculate the weighted mean of all sensors within each chart
  # 4) calculate the unweighted means the weighted RQ means between all charts
  # 5) calculate the unweighted means of the between chart means of all RQ means
  # 6) calculate the z value for the grand total and each RQ using the
  #    separate normative distributions for guilty and innocent cases
  # 7) use the z value as the statistical classifier 
  # 8) parse the positive and negative classifications using the GTR TSR or SSR
  #
  ####
  
  {
    # initialize a list to hold the output
    
    if(!exists("analysisListName")) analysisListName <- NULL
    if(is.null(analysisListName)) analysisListName <- paste0(examName, "OSS3_ANALYSIS")
    
    if(length(ls(pattern=analysisListName)) == 0) { 
      # May 31, 2025 to handle things correctly when running OSS-3 alone
      assign(analysisListName, NULL, envir=.GlobalEnv)
    }
  }
  
  #### identify the charts, sensors and questions for the series ####
  
  {
    # View(RqCqDFSeries)
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # if(seriesName == "2") stop()
    
    print("calculate the OSS-3 scores")
    
    if(!exists("makeDF")) makeDF <- FALSE
    if(!exists("saveCSV")) saveCSV <- FALSE
    if(!exists("OSS3Alpha")) OSS3Alpha <- OSS3Alpha
    if(!exists("minPresentations")) minPresentations <- 1
    if(!exists("minSensorScores")) minSensorScores <- 1
    if(!exists("OSS3DecisionRule")) OSS3DecisionRule <- "TSR"
    
    RqCqDFSeries$OSS3Score <- ""
    RqCqDFSeries$CQMean <- ""
    
    OSS3Sensors <- c("UPneumo", 
                     "LPneumo", 
                     "Pneumo",
                     "AutoEDA", 
                     # "ManualEDA", 
                     # "FC",
                     "Cardio",
                     "PLE" )
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    OSS3Sensors <- OSS3Sensors[OSS3Sensors %in% uniqueSensors]
    
    # exclude the PLE using a setting and if missing
    if("PLE" %in% OSS3Sensors && !isTRUE(includePLEScores)) {
      OSS3Sensors <- OSS3Sensors[-which(OSS3Sensors %in% "PLE")]
    }
    
    # exit if OSS3 sensors are missing
    if(length(!(OSS3Sensors %in% uniqueSensors)) < 4) {
      return(RqCqDFSeries)
    }
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    # if( length(uniqueCharts) < 3 ) {
    #   "OSS-3 requires a minimum of 3 charts"
    #   return(RqCqDFSeries)
    # }
  }
  
  #### slice the RQs and CQs ####
    
  {
    saveQuestionLabels <- RqCqDFSeries$Label
    saveEventLabels <- RqCqDFSeries$eventLabel
    
    uniqueQuestions <- unique(saveEventLabels)
    
    # unique RQs and CQs for the series
    uniqueRQs <- 
      unique(uniqueQuestions[grep("R", uniqueQuestions)])
    uniqueCQs <- 
      unique(uniqueQuestions[grep("C", uniqueQuestions)])
    
    # in case these are needed for replacement with DLST/DLDT formats
    saveUniqueQuestions <- unique(saveEventLabels)
    saveUniqueRQs <- unique(uniqueQuestions[grep("R", uniqueQuestions)])
    saveUniqueCQs <- unique(uniqueQuestions[grep("C", uniqueQuestions)])
    
    # exit if not at least 2RQs and 2 CQs
    if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) { 
      return(RqCqDFSeries) 
    }
  }

  #### check for DLST DLDT and PCASS type charts ####
  
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
    
    # Aug 29 2023
    if(DLSTType == FALSE && length(uniqueCharts) < 3 ) {
      "OSS-3 requires a minimum of 3 charts"
      # return(RqCqDFSeries)
    }
  }
  
  #### initialize a working copy of the RqCq data frame for the series ####
    
  {
    # handle missing and 0 measurements in this one
    # along with very large and very small values
    RqCqDFSeries2 <- RqCqDFSeries
    # View(RqCqDFSeries2)
    
    # results will be submitted to the original RqCqDFSeries
  }
    
  #### exclude values that exceed 3.8906 standard deviations (p=.99995) ####
  
  {
    # this  was  not  part  of the Excel OSS-3 from 2008
    # May  14, 2020

    # # iterate over the sensors and calculate the z scores
    # j=10
    # for(i in 1:length(uniqueSensors)) {
    #   theseSensorRows <-
    #     which(RqCqDFSeries2$sensorName == uniqueSensors[j])
    # 
    #   thisSensorMean <-
    #     mean(RqCqDFSeries2$sensorMeasurement[theseSensorRows],na.rm=TRUE)
    #   thisSensorSD <-
    #     sd(RqCqDFSeries2$sensorMeasurement[theseSensorRows],na.rm=TRUE)
    # 
    #   sensorZScores <-
    #     (RqCqDFSeries2$sensorMeasurement[theseSensorRows] -
    #        thisSensorMean) / thisSensorSD
    # 
    #   NAThese <- which(sensorZScores >= 3.8906 | sensorZScores <= -3.8906)
    # 
    #   RqCqDFSeries2$sensorMeasurement[theseSensorRows][NAThese] <- NA
    # }
    
    # RqCqDFSeries2 has replaced extreme values with NA
    
    # View(RqCqDFSeries2)
  }
  
  #### iterate on the charts to get the logged R/Cmean ratios ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
      
    ##### first get the RqCq data frame for this chart #####
      
    {
      
      # get the RQs and CQs for this chart using 
      
      thisChart <- uniqueCharts[i]
      thisChartRows <- which(RqCqDFSeries2$chartName == thisChart)
      
      # slice the chart without the extreme values
      RqCqDFChart2 <- RqCqDFSeries2[thisChartRows,]
      # View(RqCqDFChart2)
      
      # transform the pneumo measurements
      # already negative
      # not sure if this is necessary # oct 1, 2021
      
      # also slice the chart with the extreme values
      # results will be submitted to this one
      RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      # View(RqCqDFChart)
      
      # increment the loop to the next chart if no events
      if(nrow(RqCqDFChart) < 2 || nrow(RqCqDFChart2) < 2) { 
        next()
      }
     
      uniqueSensorsChart <- unique(RqCqDFChart2$sensorName)
      
      # View(RqCqDFChart)
      # View(RqCqDFChart2)
      
    }
    
    #### invert the sign and transform the respiration scores ####
    
    {
      
      # Oct 1, 2021
      # necessary because of the change the pre/post pneumo extraction

      # use RqCqDfChart2 without extreme values
      
      # respirationRows <- 
      #   which(RqCqDFChart2$sensorName %in% c("UPneumo", "LPneumo", "Pneumo"))
      
      # save the respiration measurements to restore them later
      # saveRespirationMeasurements <- RqCqDFChart2$sensorMeasurement[respirationRows]
      
      # respiration measurements are all negative log ratios
      # values further from 0 indicate greater changes in physiological activity
      
      # values will be > 0 and bounded by 0 and 1
      # respirationMeasurements <- -log((1-exp(-saveRespirationMeasurements))) # ^.5
      
      # respirationMeasurements <- saveRespirationMeasurements

      # rescale so measurements are between 0 and 100
      # respirationMeasurements <- respirationMeasurements * 10
      # smaller numbers indicate greater changes in physiology

      # RqCqDFChart2$sensorMeasurement[respirationRows] <- respirationMeasurements
      
    }
    
    #### Check again for outliers here ####
    
    {

      # exclude values that exceed 3.5 standard deviations

      # this  was  also not  part  of the Excel OSS-3 from 2008
      # May  14, 2020

      # initialize a working copy of the RqCq data frame for the chart
      # not needed because we already have 2 data frames for the chart
      # RqCqDFChart2 <- RqCqDFChart

      # # iterate over the sensors and calculate the z scores
      # j=10
      # for(j in 1:length(uniqueSensorsChart)) {
      #   theseSensorRows <-
      #     which(RqCqDFChart2$sensorName == uniqueSensorsChart[j])
      # 
      #   thisSensorMean <-
      #     mean(RqCqDFChart2$sensorMeasurement[theseSensorRows],na.rm=TRUE)
      #   thisSensorSD <-
      #     sd(RqCqDFChart2$sensorMeasurement[theseSensorRows],na.rm=TRUE)
      # 
      #   sensorZScores <-
      #     (RqCqDFChart2$sensorMeasurement[theseSensorRows] -
      #     thisSensorMean) / thisSensorSD
      # 
      #   NAThese <- which(sensorZScores >= 3.5 | sensorZScores <= -3.5)
      # 
      #   RqCqDFChart2$sensorMeasurement[theseSensorRows][NAThese] <- NA
      # }
      
      # View(RqCqDFChart2)
      
    }
      
    #### check some constraints on missing and small reactions ####
    
    {
      
      zeroAsMissing <- FALSE
      zeroAsMissing <- TRUE
      
      # treat small measurements as zero when zeroAsMissing==FALSE
      # zeroThreshold <- .01
      # Dec 15, 2023 set to 10 or 1% of the y-axos range
      zeroThreshold <- 20
      
      sensorMeasurements <- RqCqDFChart2$sensorMeasurement
      smallResponses <- which(abs(sensorMeasurements) <= zeroThreshold)
      sensorMeasurements[smallResponses] <- 0
      
      # treat measurements of zero as missing values
      if(isTRUE(zeroAsMissing)) {
        zeroResponses <- which(sensorMeasurements == 0)
        sensorMeasurements[zeroResponses] <- NA
      }
      
      RqCqDFChart2$sensorMeasurement <- sensorMeasurements
      
      # small, zero and negative measures are now NA 
      # but only in the RqCqDFChart2
      
      # RqCqDFChart2$sensorMeasurement
      # View(RqCqDFChart2)
      
    }
    
    #### get the RQs and CQs for this chart ####

    {
      
      # RQs
      rqRows <- grep("R", RqCqDFChart2$eventLabel)
      uniqueRQsChart <- unique(RqCqDFChart2[rqRows,'eventLabel'])
      rqDFChart2 <- RqCqDFChart2[rqRows,]
      rqDFChart <- RqCqDFChart[rqRows,]
      # View(rqDFChart2)
      
      # CQs
      cqRows <- grep("C", RqCqDFChart2$eventLabel)
      uniqueCQsChart <- unique(RqCqDFChart2[cqRows,'eventLabel'])
      cqDFChart2 <- RqCqDFChart2[cqRows,]
      cqDFChart <- RqCqDFChart[cqRows,]
      # View(cqDFChart2)
      
      # use a maximum of 4 CQs Mar 20, 2021
      # if(length(uniqueCQsChart) > 4) {
      #   uniqueCQsChart <- unique(cqDFChart$eventLabel)
      #   keepCQs <- uniqueCQsChart[1:4]
      #   cqDFChart2 <- cqDFChart2[cqDFChart2$eventLabel %in% keepCQs,]
      #   cqDFChart <- cqDFChart[cqDFChart$eventLabel %in% keepCQs,]
      # } 
      
      if(length(uniqueRQsChart) < 2 || length(uniqueCQsChart) < 2) {
        # "insufficient number of RQs or CQs for OSS-3"
        next() # next i chart in the series
      }
      
      # View(rqDFChart)
      # View(cqDFChart)
      # View(rqDFChart2)
      # View(cqDFChart2)
      
      # assign("rqDFChart", rqDFChart, pos=1)
      # assign("cqDFChart", cqDFChart, pos=1)
      # assign("rqDFChart2", rqDFChart2, pos=1)
      # assign("cqDFChart2", cqDFChart2, pos=1)
      
    }
      
    #### calculate the mean CQ for each sensor in the chart ####
      
    {
      
      # require at least 2 CQ scores for each sensor
      req2CQs <- TRUE
      req2CQs <- FALSE
      
      # initialize a vector to hold the CQ means for the OSS-3 sensors
      cqMeans <- rep(NA, times=length(OSS3Sensors))
      
      # iterate over the sensors to compute the means
      m=1
      for (m in 1:length(OSS3Sensors)) {
        getRows <- which(cqDFChart2$sensorName==OSS3Sensors[m])
        theseMeasurements <- cqDFChart2$sensorMeasurement[getRows]
        # check if less than 2 CQs
        # OSS-3 should not calculate an R/C ration with a single CQ
        if(length(which(!is.na(theseMeasurements))) < 2 && req2CQs) {
          # increment the sensor without calculating the mean
          next()
        }
        cqMeans[m] <- mean(cqDFChart2$sensorMeasurement[getRows], na.rm=TRUE)
      }
      
      # this is slightly different for DLST exams
      # than the 2008 OSS-3 Excel prototype
      # which separated the data into "charts" 
      # and would therefore use only 2 CQs for DLST/DLDT format exams
      # whereas this version will use all CQs in the series
      
      # coerce to numeric
      cqMeans <- round(as.numeric(cqMeans), 3)
      names(cqMeans) <- OSS3Sensors
      # print(cqMeans)
      
    }
    
    #### iterate on RQs and sensors to get the logged RQ/CQ ratio ####
    
    # iterate over the OSS3Sensors
    j=1
    for (j in 1:length(OSS3Sensors)) {
      
      # submit results to the data frame with extreme values
      
      getRows <- which(rqDFChart2$sensorName==OSS3Sensors[j])
      rqValues <- as.numeric(rqDFChart2$sensorMeasurement[getRows])
      
      # result is NA if no CQ value
      if(is.na(cqMeans[j]) || cqMeans[j]==0) {
        rqDFChart2$OSS3Score[getRows] <- 
          rep(NA, length=length(rqValues))
        # was rqDFChart2 until 6/26/2021
        next()
      }
      
      # calculate the logged R/Cmean Ratio
      logRCRatios <- round(log(round(rqValues, 3) / cqMeans[j]), 3)
      
      # un-commented May 15, 2023
      logRCRatios[which(abs(logRCRatios) == Inf)] <- 0
      # thiswill also correct -Inf values
      
      # submit the logRCRatios back to the rqDFChart not rqDFChart2
      rqDFChart$OSS3Score[getRows] <- logRCRatios
        
      # save the CQ mean values
      rqDFChart$CQMean[getRows] <- cqMeans[j]
      
      # View(rqDFChart)
      
      # rqDFChart$OSS3Score[getRows]
      
      # now we have the log RC ratios for the sensors
      
    } # end loop j over sensors
    
    #### remove Inf values ####
    
    {
      # Oct 10, 2021 
      # PLE was producing some Inf values
      
      InfRows <- which(abs(as.numeric(rqDFChart$OSS3Score))==Inf)
      
      rqDFChart$OSS3Score[InfRows] <- ""
      
      
    }
    
    #### restore the respiration scores ####
    
    {
      # Oct 1, 2021
      # RqCqDFChart$sensorMeasurement[respirationRows] <- saveRespirationMeasurements
    }
    
    #### pass the results back to the RqCqDFChart and RqCqDFSeries ####
    
    {
      # results were calculated without extreme values using RqCqDFChart2
      
      # use the data frames with extreme values in the sensor measurement col
      # because that data frame will be returned
      RqCqDFChart[rqRows,] <- rqDFChart
      # View(RqCqDFChart)
      
      RqCqDFSeries[thisChartRows,] <- RqCqDFChart
      # View(RqCqDFSeries)
      
    }
    
    # after this 
    # we no longer use RqCqDFSeries2, RqCqDFChart2, rqDFChart2 or cqDFChart2
    
    # RqCqDFSeries$OSS3Score
    
    # we now have the logged R/C ratios for all charts, sensors and  RQs
    
  } # end loop i over uniqueCharts
  
  ####################################################################
  
  
  ######## after iterating on the charts for the logRC ratios ########
  
  
  ####################################################################
  
  ####  1 initialize a table of logged R/Cmean ratios  ####
  
  {

    # # used only for debugging
    # 
    # OSS3Sensors2 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE")
    # 
    # OSS3Sensors2 <-
    #   OSS3Sensors2[which(OSS3Sensors2 %in% RqCqDFSeries$sensorName)]
    # 
    # # OSS3Sensors2 includes both upper and lower pneumo scores
    # 
    # RCmeanScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries,
    #                                    useSensors=OSS3Sensors2,
    #                                    scoreType="OSS3Score",
    #                                    decimals=3,
    #                                    outputName="RCmeanScoresheetDF",
    #                                    makeDF=FALSE,
    #                                    saveCSV=FALSE)
    # 
    # # View(RCmeanScoreSheetDF)

  }
  
  #### check the sign of log ratios here ####
  
  {
    
    # Oct 4 2021
    # verified that logRC ratios are identical to the 2008 Excel prototype
    
  }
  
  #### trim outlier values ####
  
  {
    
    fixThese <- which(is.na(RqCqDFSeries$OSS3Score))
    if(length(fixThese) != 0) {
      RqCqDFSeries$OSS3Score[fixThese] <- ""
    }
    
    roundThese <- which(RqCqDFSeries$OSS3Score != "")
    RqCqDFSeries$OSS3Score[roundThese] <- 
      round(as.numeric(RqCqDFSeries$OSS3Score[roundThese]), 8)
    
    # trim the outliers to the ipsative distribution
    
    # max(as.numeric(RqCqDFSeries$OSS3Score), na.rm=TRUE)
    # max(as.numeric(RqCqDFSeries$OSS3Score), na.rm=TRUE)
    
    fixThese <- which(as.numeric(RqCqDFSeries$OSS3Score) == Inf)
    # use if length to avoid NA for missing values
    if(length(fixThese) != 0) {
      RqCqDFSeries$OSS3Score[fixThese] <- ""
    }
    # View(RqCqDFSeries)
    
    # initialize a new vector for OSS-3 sensor names
    # OSS3Sensors2 <- c("Pneumo", "AutoEDA", "FC")
    # OSS3Sensors2 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
    
    # exclude PLE
    OSS3Sensors2 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")

    OSS3Sensors2 <-
      OSS3Sensors2[which(OSS3Sensors2 %in% RqCqDFSeries$sensorName)]
    
    # now calculate the mean and st dev for all sensors and all charts
    
    theseRows <- which(RqCqDFSeries$sensorName %in% OSS3Sensors2)
    # RqCqDFSeries$sensorName[theseRows]
    
    meanLogRC <- 
      mean(round(as.numeric(RqCqDFSeries$OSS3Score[theseRows]), 3), na.rm=TRUE)
    stDevLnRC <- 
      sd(round(as.numeric(RqCqDFSeries$OSS3Score[theseRows]),3), na.rm=TRUE)
    
    # calculate the cutValue
    cutZ <- 3.8906 # .99995 was 5 until 6/26/2021
    # cutZ <- 3.5 # .99977
    # cutZ <- 5 # .9999997
    cutValue <- round(cutZ * stDevLnRC + meanLogRC, 3)
    
    ##  replace any value that exceeds +/- cutValue ##
    
    replaceVal <- cutValue
    
    # high side
    theseRows <- which(as.numeric(RqCqDFSeries$OSS3Score) != "")
    theseRows <- 
      theseRows[which(as.numeric(RqCqDFSeries$OSS3Score)[theseRows] >= 
                  cutValue)]
    # use if length to avoid NA for missing values
    if(length(theseRows) != 0) {
      RqCqDFSeries$OSS3Score[theseRows] <- replaceVal
    }
    
    # low side
    theseRows <- which(as.numeric(RqCqDFSeries$OSS3Score) != "")
    theseRows <- 
      theseRows[which(as.numeric(RqCqDFSeries$OSS3Score)[theseRows] <= 
                        -cutValue)]
    # use if length to avoid NA for missing values
    if(length(theseRows) != 0) {
      RqCqDFSeries$OSS3Score[theseRows] <- -replaceVal
    }
    # View(RqCqDFSeries)
    
    roundThese <- which(!is.na(RqCqDFSeries$OSS3Score) &
                          RqCqDFSeries$OSS3Score != "")
    RqCqDFSeries$OSS3Score[roundThese] <- 
      round(as.numeric(RqCqDFSeries$OSS3Score[roundThese]), 3)
    
    # View(RqCqDFSeries)
    
  } # end trim outlier values
  
  #### re-check the mean and stDev for all log RC ratios ####
  
  {
    
    # # exclude PLE
    # OSS3Sensors2 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
    # 
    # OSS3Sensors2 <-
    #   OSS3Sensors2[which(OSS3Sensors2 %in% RqCqDFSeries$sensorName)]
    # 
    # # now calculate the mean and st dev for all sensors and all charts
    # 
    # theseRows <- which(RqCqDFSeries$sensorName %in% OSS3Sensors2)
    # # RqCqDFSeries$sensorName[theseRows]
    # 
    # newMeanLogRC <- 
    #   mean(round(as.numeric(RqCqDFSeries$OSS3Score[theseRows]), 3), na.rm=TRUE)
    # newStDevLnRC <- 
    #   sd(round(as.numeric(RqCqDFSeries$OSS3Score[theseRows]),3), na.rm=TRUE)
    
  }
  
  ####  re-initialize a table of logged R/Cmean ratios  ####
  
  {

    # # used only for debugging
    # 
    # # OSS3Sensors2 includes both upper and lower pneumo scores
    # 
    # RCmeanScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries,
    #                                    useSensors=OSS3Sensors2,
    #                                    scoreType="OSS3Score",
    #                                    decimals=3,
    #                                    outputName="RCmeanScoresheetDF",
    #                                    makeDF=FALSE,
    #                                    saveCSV=FALSE)
    # 
    # # View(RCmeanScoreSheetDF)

  }
  
  ##########################  OSS-3 sensor norms  #########################
  
  {
    # mean and stdev of OSS-3 sensors - combined guilty and innocent cases
    # parametric bootstrap 3/31/2007 10,000 iterations of N=300
    # 0.019340637	-0.017902163	-0.038517204
    # 0.498659508	0.18984364	0.107080669

    OSS3SensorMeans <- c(RMean=-0.039, EMean=0.019, CMean=-0.018, VMean=0)
    OSS3SensorStDevs <- c(RStDev=0.107, EStDev=0.499, CStDev=0.190, VStDev=1)
    
    # OSS3SensorMeans <- c(RMean=0, EMean=0, CMean=0, VMean=0)
    # OSS3SensorStDevs <- c(RStDev=1, EStDev=1, CStDev=1, VStDev=1)
    
  }
  
  ###############  standardize the logged R/C ratios ################
  
  {
    
    # initialize a data frame for RQs
    RQRowsSeries <- grep("R", RqCqDFSeries$eventLabel)
    RqDFSeries <- RqCqDFSeries[RQRowsSeries,]
    # View(RqDFSeries)
    
    # oct 1, 2021 invert the sign of the pneumos w new feature extraction
    # oct 9, 21 remove invert the sign for pneumos
    
    # Pnuemo
    pneumoRows <- which(RqDFSeries$sensorName %in% c("UPneumo", "LPneumo"))
    RqDFSeries[pneumoRows,'OSS3Score'] <- 
      ( as.numeric(RqDFSeries[pneumoRows,'OSS3Score']) - 
          OSS3SensorMeans['RMean'] ) / OSS3SensorStDevs['RStDev']
    
    # EDA (sign value inverted)
    EDARows <- which(RqDFSeries$sensorName %in% c("AutoEDA", "ManualEDA"))
    RqDFSeries[EDARows,'OSS3Score'] <- 
      -( ( as.numeric(RqDFSeries[EDARows,'OSS3Score']) - 
          OSS3SensorMeans['EMean'] ) / OSS3SensorStDevs['EStDev'] )
    
    # Cardio (sign value inverted)
    CardioRows <- which(RqDFSeries$sensorName %in% c("Cardio", "FC"))
    RqDFSeries[CardioRows,'OSS3Score'] <- 
      -( ( as.numeric(RqDFSeries[CardioRows,'OSS3Score']) - 
          OSS3SensorMeans['CMean'] ) / OSS3SensorStDevs['CStDev'] )
    
    # PLE # not included in OSS-3 in 2008
    PLERows <- which(RqDFSeries$sensorName == "PLE")
    RqDFSeries[PLERows,'OSS3Score'] <- 
      -( ( as.numeric(RqDFSeries[PLERows,'OSS3Score']) - 
             OSS3SensorMeans['VMean'] ) / OSS3SensorStDevs['VStDev'] )
    
    # round to 3 decimals
    RqDFSeries$OSS3Score <- 
      round(as.numeric(RqDFSeries$OSS3Score), 3)
    RqDFSeries$CQMean <- 
      round(as.numeric(RqDFSeries$CQMean), 3)
    
    # pass the RQs back to the input and output data frame
    RqCqDFSeries[RQRowsSeries,] <- RqDFSeries
    
    # View(RqDFSeries)
    # View(RqCqDFSeries)
    
  } # end standardize logged R/Cmean ratios
  
  ####### 2 re-initialize a table of logged R/Cmean ratios ######
  
  {

    # # used only for debugging
    # 
    # # OSS3Sensors2 includes both upper and lower pneumo scores
    # 
    # RCmeanScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries,
    #                                    useSensors=OSS3Sensors2,
    #                                    scoreType="OSS3Score",
    #                                    decimals=3,
    #                                    outputName="RCmeanScoresheetDF",
    #                                    makeDF=FALSE,
    #                                    saveCSV=FALSE)
    # 
    # # View(RqCqDFSeries)
    # # # View(RCmeanScoreSheetDF)

  }
  
  ############# combine the 2 respiration scores to 1 ##############
  
  {
    
    theseSensors <-   c("UPneumo", 
                        "LPneumo", 
                        "Pneumo", 
                        "AutoEDA", 
                        # "FC",
                        "Cardio",
                        "PLE")
    
    k=1
    for(k in 1:length(uniqueCharts)) {
      
      # iterate over the charts to combine the respiration scores
      
      # get thisChart because we have previously completed iteration on charts
      thisChart <- uniqueCharts[k]
      
      theseChartRows <- RqCqDFSeries$chartName == thisChart
      
      OSS3SensorRows <- RqCqDFSeries$sensorName %in% theseSensors
      
      rqRows <- grep("R", RqCqDFSeries$eventLabel)
      
      # RQ rows for this chart and these sensors
      theseChartRows <- 
        which(theseChartRows & OSS3SensorRows)[which(theseChartRows & 
                                                       OSS3SensorRows) 
                                               %in% rqRows]
      
      # initialize a data from of RQs in this chart
      rqDFChart <- RqCqDFSeries[theseChartRows,]
      # View(rqDFChart)
      
      uniqueRQsChart <- unique(rqDFChart$eventLabel)
      
      # iterate over the uniqueRQsChart vector
      l=1
      for(l in 1:length(uniqueRQsChart)) {
        
        # get the upper and lower pneumo values for this RQ
        thisRQ <- uniqueRQsChart[l]
        thisRQRows <- rqDFChart$eventLabel == thisRQ
        ULPneumoRows <-
          which(rqDFChart$sensorName %in% c("UPneumo", "LPneumo") & thisRQRows)
        ULPneumoVals <- as.numeric(rqDFChart$OSS3Score[ULPneumoRows])
        CQMeanVals <- as.numeric(rqDFChart$CQMean[ULPneumoRows])
        
        # oct 4, 2021
        # fix NAs
        ULPneumoVals[which(is.na(ULPneumoVals))] <- 0
        CQMeanVals[which(is.na(CQMeanVals))] <- 0
        
        # calculate the pneumo value and submit it to the data frame
        PneumoVal <- ifelse(prod(ULPneumoVals) < 0,
                            0,
                            ULPneumoVals[which.max(abs(ULPneumoVals))] )
        CQMeanVal <- ifelse(prod(ULPneumoVals) < 0,
                            NA,
                            CQMeanVals[which.max(abs(ULPneumoVals))] )
        
        PneumoRow <- which(rqDFChart$sensorName == "Pneumo" & thisRQRows)
        rqDFChart$OSS3Score[PneumoRow] <- PneumoVal
        rqDFChart$CQMean[PneumoRow] <- CQMeanVal
        # View(rqDFChart)
        
      } # end loop l over RQs
      
      RqCqDFSeries[theseChartRows,] <- rqDFChart
      
    } # end loop k over charts
    
    # RqCqDFSeries$OSS3Score[which(RqCqDFSeries$sensorName == "Pneumo")]
    # View(RqCqDFSeries)
    
  } # end combine 2 pneumo sensor scores to 1
  
  ########### truncate the standardized logRC ratios to 3 SDs ############
  
  {
    
    # replace any value that exceeds +/- 3 stDev with the 3 st
    
    # do this with the RqCqDFSeries to get the UPneumo and LPneumo values
    RqCqDFSeries$OSS3Score[as.numeric(RqCqDFSeries$OSS3Score) > 3] <- "3"
    RqCqDFSeries$OSS3Score[as.numeric(RqCqDFSeries$OSS3Score) < -3] <- "-3"
    
    # View(RqCqDFSeries)

  }
  
  ################## 3 re-initialize the RCMean Score Sheet #############
  
  {

    # # used only for debugging
    # 
    # # now includes only the combined pneumo score
    # 
    # RCmeanScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries,
    #                                    useSensors=c("Pneumo", "AutoEDA", "Cardio"),
    #                                    scoreType="OSS3Score",
    #                                    decimals=3,
    #                                    outputName="RCmeanScoresheetDF",
    #                                    makeDF=FALSE,
    #                                    saveCSV=FALSE)
    # 
    # # View(RCmeanScoreSheetDF)

  }
  
  ############### boundary condition on missing data ################
  
  {
    
    # require at least 2 sensor scores for each stimulus presentation
    
    # first save the measurements for later
    saveSensorMeasurements <- RqCqDFSeries$sensorMeasurement
    
    # initialize a new vector for OSS-3 sensor names
    # keep only the combined respiration value
    # OSS3Sensors2 <- c("Pneumo", "AutoEDA", "Cardio")
    # OSS3Sensors2 <- c("Pneumo", "AutoEDA", "FC")
    
    OSS3Sensors3 <- c("Pneumo", "AutoEDA", "Cardio", "PLE")
    # OSS3Sensors3 <- c("Pneumo", "AutoEDA", "Cardio")
    OSS3Sensors3 <- OSS3Sensors3[OSS3Sensors3 %in% RqCqDFSeries$sensorName]
    
    OSS3RQRows <- which( RqCqDFSeries$sensorName %in% OSS3Sensors3 & 
                           grepl("R", RqCqDFSeries$eventLabel) )
    
    # slice a data frame for all RQs in the series
    rqDFSeries <- RqCqDFSeries[OSS3RQRows,]
    # View(rqDFSeries)
    
    # required minimum number of sensor scores
    # minSensorScores <- 2
    
    # iterate over the charts
    n=1
    for(n in 1:length(uniqueCharts)) {
      
      {
        
        # initialize a data frame for the chart
        thisChart <- uniqueCharts[n]
        theseChartRows <- rqDFSeries$chartName == thisChart
        rqDFChart <- rqDFSeries[theseChartRows,]
        # View(rqDFChart)
        
        # increment the loop if no RQs
        if(nrow(rqDFChart) == 0 ) next()
        
        # get the RQ names
        uniqueRQsChart <- unique(rqDFChart$eventLabel)
        
      }
      
      # iterate over the RQ columns
      o=1
      for(o in 1:length(uniqueRQsChart)) {
        thisRQ <- uniqueRQsChart[o]
        thisRQRows <- which(rqDFChart$eventLabel == thisRQ)
        RQVals <- rqDFChart$OSS3Score[thisRQRows]
        names(RQVals) <- c("R", "E", "C")
        # check the number of extant scores
        if(length(which(!is.na(RQVals))) < minSensorScores) {
          # set all values to NA if < 2 extant
          rqDFChart$OSS3Score[thisRQRows] <- NA
          rqDFChart$CQMean[thisRQRows] <- NA
        }
      } # end loop o over RQs
      
      rqDFSeries[theseChartRows,] <- rqDFChart
      
    } # end loop n over charts
    
    # View(rqDFSeries)
    RqCqDFSeries[OSS3RQRows,] <- rqDFSeries
    # View(RqCqDFSeries)
    
  } # end boundary condition on missing data
  
  #########  4 re-initialize a table of logged R/Cmean ratios #######
  
  {
    
    # # used only for debugging
    # 
    # # OSS3Sensors3 uses the combined pneumo score
    # 
    # RCmeanScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries,
    #                                    useSensors=OSS3Sensors3,
    #                                    scoreType="OSS3Score",
    #                                    decimals=3,
    #                                    outputName="RCmeanScoresheetDF",
    #                                    makeDF=FALSE,
    #                                    saveCSV=FALSE)
    # 
    # # View(RCmeanScoreSheetDF)

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
    
    ## reformat the OSS3ScoreSheetDF2 for DLST/DLDT exams ##
    
    # may need a function to do this
    
  }
  
  ########### initialize the OSS-3 score sheet ############
  
  {
    
    OSS3Sensors3a <- c("Pneumo",  "AutoEDA", "Cardio", "PLE")
    OSS3Sensors3a <- OSS3Sensors3a[OSS3Sensors3a %in% uniqueSensors]
    
    # remove the PLE going forward
    
    OSS3Sensors2 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
    
    OSS3Sensors3 <- c("Pneumo",  "AutoEDA", "Cardio")
    
    # need the OSS-3 score sheet to calculate channel contributions
    
    # includes upper and lower pnuemo
    OSS3ScoreSheetDF1 <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                      useSensors=OSS3Sensors2,
                                      scoreType="OSS3Score",
                                      decimals=3,
                                      DLSTType=DLSTType,
                                      outputName="OSS3ScoresheetDF",
                                      makeDF=makeDF,
                                      saveCSV=saveCSV)
    # OSS3ScoreSheetDF1 is not used for anything
    
    # OSS3ScoreSheetDF2 includes only the combined pneumo without PLE
    # use this one to calculate the result
    OSS3ScoreSheetDF2 <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                      useSensors=OSS3Sensors3,
                                      scoreType="OSS3Score",
                                      decimals=3,
                                      DLSTType=DLSTType,
                                      outputName="OSS3ScoresheetDF",
                                      makeDF=makeDF,
                                      saveCSV=saveCSV)
    # OSS3ScoreSheetDF2 is used for subsequent operations
    
    # includes combined pneumo and PLE
    OSS3ScoreSheetDF3 <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                      useSensors=OSS3Sensors3a,
                                      scoreType="OSS3Score",
                                      decimals=3,
                                      DLSTType=DLSTType,
                                      outputName="OSS3ScoresheetDF",
                                      makeDF=makeDF,
                                      saveCSV=saveCSV)
    
    # OSS3ScoreSheetDF2 is used for subsequent operations
    # rowMeans(OSS3ScoreSheetDF2[,5:ncol(OSS3ScoreSheetDF2)])
    
    # colMeans(OSS3ScoreSheetDF2[,5:ncol(OSS3ScoreSheetDF2)])
    
    # mean(colMeans(OSS3ScoreSheetDF2[,5:ncol(OSS3ScoreSheetDF2)]))
    
    # mean(as.matrix(OSS3ScoreSheetDF2[,5:ncol(OSS3ScoreSheetDF2)]))
    
    # View(OSS3ScoreSheetDF1)
    # View(OSS3ScoreSheetDF2)
    # View(RqCqDFSeries)
    
  } # end initialize OSS-3 scoresheet
  
  ############  OSS-3 weighting function  ############
  
  {
    
    # OSS-3 discriminate function
    OSS3WeightingCoefs <- c(Respiration=.192, EDA=.528, Cardio=.28)
    
    # normalize the weighting function so that it always sums to 1
    OSS3WeightingCoefs <- OSS3WeightingCoefs / sum(OSS3WeightingCoefs)
    
  }
  
  ############# within Chart weighted means for RQs ##############
  
  {
    
    withinChartRQMeans <- 
      chartTotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                    outputName="OSS3ChartTotalsDF",
                    aggType="mean",
                    weightingCoefs=OSS3WeightingCoefs,
                    minSensorScores=minSensorScores, 
                    makeDF=makeDF,
                    saveCSV=saveCSV) 
    
    # mean(as.matrix(withinChartRQMeans[,4:ncol(withinChartRQMeans)]))
    
  }
  
  ######### constrain the number of scored presentations ##########
  
  {
    
    # DLST exams traditionally require exactly 3 usable presentations
    # for each RQ
    
    # usable presentation requires at least 2 usable sensor scores
    # usable sensor scores == non-artifacted and non-missing
    
    # most test formats will give a result using a single RQ presentation
    # if others are not usable
    
    # there may be little or no value in doing this with OSS-3 
    # because the data are averaged not summed
    
    # the chartTotalsFn will check the number of sensor scores
    # for each prentation of each RQ
    
    # if(!exists("minPresentations")) minPresentations <- 2
    
    # check that RQ cols are numeric
    # to avoid problems when there are missing values
    l=5
    for(l in 4:ncol(withinChartRQMeans)) {
      withinChartRQMeans[,l] <- as.numeric(withinChartRQMeans[,l])
    }
    
    # str(withinChartRQMeans)
    
    # mean of this data frame will equal the OSS-3 grand mean
    # mean(as.matrix(withinChartRQMeans[,4:ncol(withinChartRQMeans)]), na.rm=TRUE)
    
    ## withinChartRQMeans is also used for the KWANOVA ##
    
    ## withinChartRQMeans is also used for channel contributions ##
    
    # initialize a matrix for the chart subtotals
    chartSubtotalsMtx <- as.matrix(withinChartRQMeans[,4:ncol(withinChartRQMeans)])
    row.names(chartSubtotalsMtx) <- withinChartRQMeans$chartName
    colnames(chartSubtotalsMtx)
    # str(chartSubtotalsMtx)
    
    # clear the scoreSheetDF2 for question presentations
    # with insufficient usable sensor scores
    s=3
    for(s in 1:nrow(chartSubtotalsMtx)) {
      # iterate over the chart rows 
      # get the chart rows from the OSS3ScoreSheetDF2
      thisChart <- row.names(chartSubtotalsMtx)[s]
      theseRows <- which(OSS3ScoreSheetDF2$chartName == thisChart)
      u=3
      for(u in 1:length(colnames(chartSubtotalsMtx))){
        # iterate over the RQ columns
        if(is.na(chartSubtotalsMtx[s,u]) || chartSubtotalsMtx[s,u] == "") {
          thisRQ <- colnames(chartSubtotalsMtx)[u]
          # clear the info from the score sheet
          thisCol <- which(names(OSS3ScoreSheetDF2) == thisRQ)
          OSS3ScoreSheetDF2[theseRows,thisCol] <- NA # was "" 20200618
        } else {
          next() # next chart
        }
      }
    }
    
    # mean(as.matrix(OSS3ScoreSheetDF2[,5:ncol(OSS3ScoreSheetDF2)]))
    
    # View(OSS3ScoreSheetDF2)
    
  } # end constraint on the number of stimulus presentations
  
  ############## recalculate the within chart RQ means ###############
  
  {
    
    # recalculate after constraining for excess missing and artifacted data
    
    withinChartRQMeans <- 
      chartTotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                    outputName="OSS3ChartTotalsDF",
                    aggType="mean",
                    weightingCoefs=OSS3WeightingCoefs,
                    minSensorScores=minSensorScores, 
                    makeDF=makeDF,
                    saveCSV=saveCSV) 
    
    # mean(as.matrix(withinChartRQMeans[,4:ncol(withinChartRQMeans)]))
    
    # View(withinChartRQMeans)
    
  }
  
  ################### between chart Sensor Means ####################
  
  {
    
    # between chart means for standardized logged ratios for each sensor
    # Can be aggregated for a sample of exams
    # and used for discriminate analysis or logistic regression
    
    # requires the OSS3ScoreSheetDF2 score sheet for combined pneumo
    # use OSS3ScoreSheetDF2 to include the PLE
    OSS3SensorMeansDF <- 
      sensorMeansFn(scoreSheetDF=OSS3ScoreSheetDF3,
                    outputName="OSS3SensorMeansDF",
                    makeDF=makeDF,
                    saveCSV=saveCSV)
    # use makeDF=TRUE and aggregate the cases for discriminate analysis
    
    # round it
    OSS3SensorMeansDF[,4:ncol(OSS3SensorMeansDF)] <- 
      round(OSS3SensorMeansDF[,4:ncol(OSS3SensorMeansDF)], 3)
    
    # calculate the means for question and sensors
    meanQuestionScores <-
      apply(OSS3SensorMeansDF[1:3,4:ncol(OSS3SensorMeansDF)], 2, weighted.mean, OSS3WeightingCoefs)
    
    # mean(meanQuestionScores)
    
    # output this and aggregate for all sample cases
    meanSensorScores <- 
      rowMeans((OSS3SensorMeansDF[,4:ncol(OSS3SensorMeansDF)]), na.rm=TRUE)
    names(meanSensorScores) <- OSS3SensorMeansDF$sensorName
    
    # proportions
    # meanSensorScores[1:3] / sum(meanSensorScores[1:3])
    
    # population standard deviations
    SDSensorScores <- 
      apply(OSS3SensorMeansDF[,4:ncol(OSS3SensorMeansDF)], 1, sdp)
    names(SDSensorScores) <- OSS3SensorMeansDF$sensorName
    
    # normalizedMeanSensorScores <- 
    #   meanSensorScores / sum(meanSensorScores, na.rm=TRUE)
    
  } # end OSS-3 sensor means
  
  ######### OSS-3 series subtotals ########
  
  {
    
    # requires the OSS3ScoreSheetDF2 for combined pneumo
    
    # give the same result as the previous step
    
    # OSS-3 series subtotals are the between chart means of within chart
    # weighted means of standardized logged R/C ratios for the sensors
    
    # source('outputScores.R', echo=FALSE)
    
    # between chart means of within chart weighted means for RQs
    OSS3SeriesTotalsDF <- seriesTotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                         outputName="OSS3SeriesTotalsDF",
                                         aggType="mean",
                                         weightingCoefs=OSS3WeightingCoefs,
                                         aggMethod="within",
                                         missingVals=0,
                                         NAVals=0,
                                         makeDF=FALSE,
                                         saveCSV=saveCSV)
    
    # str(withinChartRQMeans)
    
    for(j in 4:ncol(withinChartRQMeans)) {
      withinChartRQMeans[,j] <- as.numeric(withinChartRQMeans[,j])
    }
    
        # gives the same result
    betweenChartRQMeans <- 
      colMeans(withinChartRQMeans[,4:ncol(withinChartRQMeans)], na.rm=TRUE)
    
    # str(withinChartRQMeans)
    
    OSS3SeriesTotalsDF <- cbind.data.frame(examName=examName, 
                            seriesName=seriesName,
                            rbind(betweenChartRQMeans))
    
    # withinChartRQMeans can be used to evaluate within chart habituation 
    
    # within chart between RQ sub means are not used for anything
    # withinChartSubMeans <- 
    #   rowMeans(withinChartRQMeans[,4:ncol(withinChartRQMeans)], na.rm=TRUE)
    
  } # end OSS-3 series totals
  
  ########## calculate the between-chart OSS-3 grand mean  ##########
  
  {
    
    # gives the same result as the previous step
    # betweenChartRQMeans <- as.numeric(OSS3SeriesTotalsDF[3:ncol(OSS3SeriesTotalsDF)])
    # names(betweenChartRQMeans) <- names(OSS3SeriesTotalsDF)[3:ncol(OSS3SeriesTotalsDF)]
    
    betweenChartRQMeans <- round(betweenChartRQMeans, 3)
    
    # mean of the between chart mean weighted means for RQs
    OSS3MeanZ <- round(mean(betweenChartRQMeans, na.rm=TRUE), 3)
    
  }
  
  ########## channel contributions ##########
  
  {
    
    # source('OSS3ChannelContributions.R', echo=FALSE)
    
    ### this may need work - differs from the Excel ###
    
    channelVals <- OSS3ChannelContributionsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                              withinChartRQMeans=withinChartRQMeans,
                                              weightingCoefs=OSS3WeightingCoefs)
    
  }
  
  ########## OSS-3 reference distribution parameters  ########## 
  
  {
    # mean and stDev for weighted sensor scores of guilty and innocent cases
    OSS3RefMean <- c(SRMean=-.581, NSRMean=.586)
    OSS3RefStDev <- c(SRStDev=.453, NSRStDev=.454)
  }
  
  ########## calculate the OSS-3 grand mean z-score and p-value  ########## 
  
  {
    
    zTruthful <- (OSS3MeanZ - OSS3RefMean['NSRMean']) / 
      OSS3RefStDev['NSRStDev']
    
    zDeceptive <- (OSS3MeanZ - OSS3RefMean['SRMean']) / 
      OSS3RefStDev['SRStDev']
    
    # p-value for deceptive grand mean scores
    pTruthful <- round(pnorm(zTruthful), 3)
    # p-value for truthful grand mean scores
    # keep this on the lower tail and adjust it as needed
    pDeceptive <- round(pnorm(zDeceptive), 3)
    # use lower.tail=FALSE so that all p value is high side
    
    names(pTruthful) <- "NSRMeanP"
    names(pDeceptive) <- "SRMeanP"
    
  }
  
  ####### calculate the z-scores and p-values for RQs #######
  
  {
    
    zRQsTruthful <- (betweenChartRQMeans - OSS3RefMean['NSRMean']) / 
      OSS3RefStDev['NSRStDev']
    
    zRQsDeceptive <- (betweenChartRQMeans - OSS3RefMean['SRMean']) / 
      OSS3RefStDev['SRStDev']
    
    zRQsTruthful <- round(zRQsTruthful, 3)
    zRQsDeceptive <- round(zRQsDeceptive, 3)
    
    # use this for deceptive classifications
    pRQsTruthful <- round(pnorm(zRQsTruthful), 3)
    
    # use this for truthful classifications
    # adjust the tail region later as needed
    pRQsDeceptive <- round(pnorm(zRQsDeceptive), 3)
    # use lower.tail=FALSE so that all p values are low side
    
    names(pRQsTruthful) <- names(betweenChartRQMeans)
    names(pRQsDeceptive) <- names(betweenChartRQMeans)
    # names(pRQsTruthful) <- paste0(names(pRQsTruthful), "NSRMeanP")
    # names(pRQsDeceptive) <- paste0(names(pRQsDeceptive), "SRMeanP")
    
  }
  
  ###### select the OSS-3 decision rule ######
  
  {
    
    # we need the decicion rule before we select the p-values to report
    
    outputRule <- OSS3DecisionRule
    
    if(OSS3DecisionRule == "auto") {
      OSS3DecisionRule <-  ifelse(KWResult$KWResult == "sig",
                                  "SSR",
                                  "TSR" )
      outputRule <- paste0(OSS3DecisionRule, " (auto-selected)")
      
    } else {
      
      outputRule <- OSS3DecisionRule
      
    }
    
  }
  
  ###### select the z-scores and p-vals for classification ######
  
  {
    
    # grand mean p-value
    
    # select the p-value from the most sig. at the lower tail
    thisPVal <- which.min(c(pTruthful, 1-pDeceptive))
    # set the p-value on the upper tail for truthful results
    # truthful results are using the deceptive distribution
    # OSS3PVal <- c(pTruthful, (abs(pDeceptive - .5) + .5))[thisPVal]
    OSS3PVal <- c(pTruthful, pDeceptive)[thisPVal]
    # set the limits
    OSS3PVal <- ifelse(OSS3PVal <= .001, 
                       .001,
                       ifelse(OSS3PVal >= .999,
                              .999,
                              OSS3PVal))
    # adjust the tail region later as needed
    
  }
  
  {
    
    # select the p-values for subtotals
    
    # select pRQsDeceptive only when using the SSR 
    # and only when the min zRQsDeceptive >= 0 
    if( outputRule == "SSR" && min(zRQsDeceptive) >= 0 ) {
      # don't use the upper tail for 
      RQPVals <- pRQsDeceptive
    } else {
      # scr and TSR rules use only the truthful p-values
      RQPVals <- pRQsTruthful
    }
    # GTR does not use subtotals and so it does not matter which
    
    RQPVals <- round(RQPVals, 3)
    
    names(RQPVals) <- names(betweenChartRQMeans)
    
    
    
    # thisOne <- which.min(c(min(pRQsTruthful), min(1-pRQsDeceptive)))
    # # thisOne <- pRQsTruthful
    # # use the upper tail for pRQsDeceptive for NDI/NSR classifications
    # RQPVals <- list(pRQsTruthful, 1-pRQsDeceptive)[[thisOne]]
    # # pRQsTruthful is always used for TSR and SCR rules
    # # because subtotals are only used to make deceptive classifications
    # # for SCR and SSR pRQsTruthful is used when the result is SR
    # # and pRQsDeceptive is used when the result is NSR
    
    thisRQPVal <- which.min(RQPVals)
    minRQPVal <- RQPVals[thisRQPVal]
    minRQName <- names(betweenChartRQMeans)[thisRQPVal]
    
    # Nov 28, 2023
    minRQPVal <- 1-(abs(minRQPVal -.5) + .5)
    
    thisRQPVal <- ifelse(minRQPVal <= .001, 
                         ".001",
                         ifelse(minRQPVal >= .999,
                                ".999",
                                minRQPVal))
    
    
  }
  
  ############ chart sub-weighted-means ############
  
  {
    
    # call the chartTotalsFn here 
    # so that chartTotalsDF is extant for decision rules
    
    chartTotalsDF <- chartTotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                   outputName="OSS3ChartTotalsDF",
                                   aggType="mean",
                                   weightingCoefs=OSS3WeightingCoefs,
                                   makeDF=makeDF,
                                   saveCSV=saveCSV)
    # View(chartTotalsDF)
    
    # colMeans(chartTotalsDF[,4:ncol(chartTotalsDF)])
    
  }
  
  ###############  Alpha Boundaries  ##################
  
  {
    
    # alpha boundaries are set in the NCCAASCII_init.R  script
    # load these in the same env where the decision rules Fn are defined
    
    if(!exists("OSS3Alpha")) OSS3Alpha <- OSS3Alpha
    
    oss3AlphaTDiag <- OSS3Alpha[1]
    oss3AlphaDDiag <- OSS3Alpha[2] 
    oss3AlphaTScreen <- OSS3Alpha[3]
    oss3AlphaDScreen <- OSS3Alpha[4]
    
    ## calculate the statistically corrected alphas for subtotals ##
    
    # Bonferonni for DI/SR classifications with subtotal scores
    screenSRCorrection <- round(oss3AlphaDScreen/length(uniqueRQs), 4)
    
    # inverse Sidak for NDI/NSR classifications with subtotal scores
    screenNSRCorrection <- round(1-(1-oss3AlphaTScreen)^length(uniqueRQs), 4)
    # check it for 3 RQs
    # 1-(1-0.142625)^(1/3)
    
    OSS3Alpha3 <- c(GTDI=oss3AlphaDDiag, 
                   GTNDI=oss3AlphaTDiag, 
                   STDI=oss3AlphaDScreen, 
                   STNDI=oss3AlphaTScreen,
                   STDIc=screenSRCorrection,
                   STNDIc=screenNSRCorrection )
    names(OSS3Alpha3) <- c("GTDI", 
                           "GTNDI", 
                           "STDI", 
                           "STNDI",
                           "STDIc",
                           "STNDIc" )
    
    # correct the tail region to parse truthful results
    # permits the same decision rule functions with integer or .p input
    OSS3CutScores <- OSS3Alpha3
    OSS3CutScores['GTNDI'] <- 1 - OSS3CutScores['GTNDI']
    OSS3CutScores['STNDI'] <- 1 - OSS3CutScores['STNDI']
    OSS3CutScores['STNDIc'] <- 1 - OSS3CutScores['STNDIc']
     
    # OSS3CutScores now has all the needed cutscore for decision rules
    
  }
  
  #################  KW-ANOVA ################
  
  {
    
    ## get the KWANOVA result ##
    
    # used by OSS-3 screening rule SCNFn()
    
    # source('KWANOVA.R', echo=FALSE)
    # already sourced by the OSS3Score.R script
    
    KWResult <- KWANOVAFn(withinChartRQMeans=withinChartRQMeans, a=.1)
    
    # print(KWResult)
    
    # stop()
    
    # KWResult$KWResult is either "sig" or "ns"
    
    # can also use the KW result to select the SSR or TSR
    
  }
  
  ######### flip the p-value so that NDI results are upper tail ##########
  
  {
    
    # truthful results use the SRMean and deceptive result use NSRMean
    inputPVal <- ifelse(names(OSS3PVal) == "SRMeanP",
                        (abs(OSS3PVal - .5) + .5),
                        1-(abs(OSS3PVal - .5) + .5) )
    
    inputPVal <- round(inputPVal, 3)
    # DI/SR pvals are lower tail and 
    # NDI/NSR pvals are upper tail
    # so that the same decision rules can take decimal or inger input
    
  }
  
  ########## call the decision rules to parse the OSS-3 result ###########
  
  {
    
    # source('decisionRules.R', echo=FALSE)
    
    # use flip=false because NDI/NSR results are upper tail 
    
    # GTR uses only one input p-value for the total scoree
    GTRResult <- GTRFn(totalScore=inputPVal, 
                       RQNames=uniqueRQs,
                       cutScores=OSS3CutScores, 
                       flip=FALSE )
    
    # SSR uses only one input set of p-values for RQs
    # DI/SR pvals are lower tail and NDI/NSR pvals are upper tail
    SSRResult <- SSRFn(subtotalScores=RQPVals,
                       cutScores=OSS3CutScores,
                       flip=FALSE )
    
    # TSR alwayss uses pRQsTruthful
    TSRResult <- TSRFn(totalScore=inputPVal, 
                       subtotalScores=pRQsTruthful,
                       cutScores=OSS3CutScores,
                       flip=FALSE )
    
    # need to input both sets of p-values for RQs 20200522
    SCNResult <- SCNFn(totalScore=inputPVal, 
                       subtotalScores=list(pRQsTruthful, pRQsDeceptive),  
                       withinChartRQMeans=withinChartRQMeans, 
                       cutScores=OSS3CutScores, 
                       flip=FALSE )
    # print(SCNResult)
    
    # stop()
    
  }
  
  ########### select the result for output ###########
  
  {
    
    # use the "OSS3DecisionRule" parameter to select the decision rule
    # the rule parameter must be in the env where the OSS3ScoresFn is defined
    if(!exists("OSS3DecisionRule")) OSS3DecisionRule <- "TSR"
    
    OSS3TestResult <- switch(OSS3DecisionRule,
                             "TSR"=TSRResult$testResult,
                             "SSR"=SSRResult$testResult,
                             "SCR"=SCNResult$testResult,
                             "GTR"=GTRResult$testResult )
    
    OSS3QuestionResults <- switch(OSS3DecisionRule,
                                  "TSR"=TSRResult$subtotalResults,
                                  "SSR"=SSRResult$subtotalResults,
                                  "SCR"=SCNResult$subtotalResults,
                                  "GTR"=GTRResult$subtotalResults )
    
    resultUsing <- switch(OSS3DecisionRule,
                          "TSR"=TSRResult$resultUsing,
                          "SSR"=SSRResult$resultUsing,
                          "SCR"=SCNResult$resultUsing,
                          "GTR"=GTRResult$resultUsing )
    
  }
  
  ##################### select the output values ######################
  
  {
    
    # ifelse is vectorized internally but the output of ifelse is not 
    GTVal <- ifelse(OSS3TestResult == "NDI/NSR",
                    pTruthful,
                    pDeceptive)
    
    GTVal <- ifelse(GTVal <= .001, 
                    .001,
                    ifelse(GTVal >= .999,
                           .999,
                           GTVal))
    
    if(OSS3DecisionRule != "GTR" ) {
      
      # not for the GTR
      
      ifelse(OSS3TestResult == "NDI/NSR",
             STVals <- pRQsDeceptive,
             STVals <- pRQsTruthful )
      
      thisSubtotalScore <- which.min(STVals)
      minSubtotalScore <- STVals[thisSubtotalScore]
      
      # correct the limits of the min subtotal score
      minSubtotalScore <- ifelse(minSubtotalScore <= .001, 
                                 .001,
                                 ifelse(minSubtotalScore >= .999,
                                        .999,
                                        minSubtotalScore))
      
      # correct the limits of all subtotal scores
      ifelse(minSubtotalScore <= .001, 
             STVals[which(STVals < .001)] <- .001,
             ifelse(minSubtotalScore >= .999,
                    STVals[which(STVals > .999)] <- .999,
                    STVals))
      
    } else {
      
      # for GTR
      STVals <- "NA"
      
    }
    
  }
  
  ####################### OUTPUT section ######################
  
  ############## measurement tables 
  
  {
    
    outputSensors <- c("UPneumo", 
                       "LPneumo", 
                       "AutoEDA",
                       "Cardio" )
    
    measurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=outputSensors,
                         decimals=2,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    
  }
  
  ################ OSS-3 score sheet (standardized logged R/C ratios)
  
  OSS3ScoreSheetDF2 <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                    useSensors=c("Pneumo", "AutoEDA", "Cardio"),
                                    scoreType="OSS3Score",
                                    decimals=3,
                                    DLSTType=DLSTType,
                                    outputName="OSS3ScoresheetDF",
                                    makeDF=makeDF,
                                    saveCSV=TRUE)
  
  ############### sensor totals 
    
  sensorTotalsDF <- sensorSubtotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                      outputName="OSS3SensorTotalsDF",
                                      aggType="mean",
                                      makeDF=makeDF,
                                      saveCSV=TRUE)
    

  ############ within chart weighted means 
  
  chartTotalsDF <- chartTotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                 outputName="OSS3ChartTotalsDF",
                                 aggType="mean",
                                 weightingCoefs=OSS3WeightingCoefs,
                                 makeDF=makeDF,
                                 saveCSV=TRUE)
  
 
  
  
  ################### channel contribution
  
  # channelVals <- OSS3ChannelContributionsFn(scoreSheetDF=OSS3ScoreSheetDF2,
  #                                           withinChartRQMeans=withinChartRQMeans,
  #                                           weightingCoefs=OSS3WeightingCoefs)
  
  
  ################ series totals (means)
  
  OSS3SeriesTotalsDF <- seriesTotalsFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                       outputName="OSS3SeriesTotalsDF",
                                       aggType="mean",
                                       weightingCoefs=OSS3WeightingCoefs,
                                       aggMethod="within",
                                       missingVals=0,
                                       NAVals=0,
                                       makeDF=makeDF,
                                       saveCSV=TRUE)
  
  ############## between chart sensor means
  
  # between chart means for standardized logged ratios for each sensor
  # Can be aggregated for a sample of exams
  # and used for discriminate analysis or logistic regression
  
  # requires the OSS3ScoreSheetDF2 score sheet for combined pneumo
  OSS3SensorMeansDF <- sensorMeansFn(scoreSheetDF=OSS3ScoreSheetDF2,
                                     outputName="OSS3SensorMeansDF",
                                     makeDF=makeDF,
                                     saveCSV=TRUE)
  
  ################ between chart RQ means 
  
  {
    
    betweenChartRQMeans <- rbind.data.frame(betweenChartRQMeans)
    names(betweenChartRQMeans) <- paste0("R", 1:ncol(betweenChartRQMeans))
    
    outputName <- paste(examName,
                        seriesName,
                        "OSS3BetweenChartRQMeans",
                        sep="_")
    
    # if(isTRUE(makeDF)) {
      assign(outputName, betweenChartRQMeans, env=.GlobalEnv)
    # }
    
    # if(isTRUE(saveCSV)) {
      write.csv(betweenChartRQMeans,
                file=paste0(outputName, ".csv"),
                row.names=FALSE )
    # }
    
  }  
  
  ############ construct a list to hold the OSS=3 result ##############
  
  {
    
    outputListName <- paste(examName, seriesName, "OSS3OutputList", sep="_")
    
    OSS3OutputZTruthful <- round(zTruthful,3)
    OSS3OutputZDeceptive <- round(zDeceptive,3)
    
    # fix the p-values
    OSS3OutputPVal <- ifelse(1-(abs(OSS3PVal - .5) + .5) <= .001,
                       "<.001",
                       round(1-(abs(OSS3PVal - .5) + .5), 3) )
    
    # OSS3OutputRQPVals <- round(1-(abs(RQPVals - .5) + .5), 3)
    OSS3OutputRQPVals <- RQPVals
    OSS3OutputRQPVals[which(OSS3OutputRQPVals <= .001)] <- "<.001"
    OSS3OutputRQPVals[which(OSS3OutputRQPVals >= .999)] <- ">.999"

    # construct the output list                     
    OSS3OutputList <- list(OSS3="Objective Scoring System, version 3 (Nelson, Krapohl & Handler, 2008)",
                           examName=examName,
                           seriesName=seriesName,
                           OSS3Result=OSS3TestResult,
                           OSS3QuestionResults=OSS3QuestionResults,
                           OSS3DecisionRule=outputRule,
                           OSS3ResultUsing=resultUsing,
                           OSS3GrandMeanZ=OSS3MeanZ,
                           OSS3ZTruthful=OSS3OutputZTruthful,
                           OSS3ZDeceptive=OSS3OutputZDeceptive,
                           OSS3PVal=OSS3OutputPVal,
                           OSS3PTruthful=pTruthful,
                           OSS3PDeceptive=1-pDeceptive,
                           OSS3Questions=uniqueQuestions,
                           OSS3RQNames=uniqueRQs,
                           OSS3BetweenChartRQMeans=betweenChartRQMeans,
                           OSS3MinRQPVal=thisRQPVal,
                           OSS3MinRQName=minRQName,
                           OSS3RQPVals=OSS3OutputRQPVals,
                           OSS3ZTruthfulRQs=zRQsTruthful,
                           OSS3ZDeceptiveRQs=zRQsDeceptive,
                           OSS3RQPValsTruthful=pRQsTruthful,
                           OSS3RQPValsDeceptive=pRQsDeceptive,
                           KWResult=KWResult, 
                           OSS3Alphas=OSS3Alpha3,
                           OSS3Sensors=outputSensors,
                           OSS3WeightingCoefs=OSS3WeightingCoefs,
                           OSS3ChannelVals=channelVals,
                           OSS3SensorMeansDF=OSS3SensorMeansDF,
                           OSS3ChartTotalsDF=withinChartRQMeans,
                           OSS3ScoreSheet=OSS3ScoreSheetDF2, 
                           OSS3Measurements=measurementsDF )
    
  }

  ###### save the analysis output list to the global env as a .txt as as side effect ######
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, OSS3OutputList, env=.GlobalEnv)
  
  if(saveAnalysisTXT) {
    
    # June 5, 2025
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "OSS3Output"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      OSS3OutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
    # May 31, 2025 write the OSS-3 output information to a text file
    outputFileName <- paste0(examName,
                             "_",
                             seriesName,
                             "_",
                             "OSS3OutputList",
                             ".txt" )
    
    # use capture.output to create the text file
    capture.output(OSS3OutputList, file = outputFileName )
    
  }
  
  ###### visible output ######
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  # restore the sensor measurements
  RqCqDFSeries$sensorMeasurement <- saveSensorMeasurements
  
  return(RqCqDFSeries)

  # end OSS3ScoresFn
}  


