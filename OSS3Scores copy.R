
source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Model.R', echo=FALSE)



OSS3ScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                         decisionRule=OSS3DecisionRule ) {
  # R function to compute the OSS-3 results from the measurements data frame
  # sourced by the getScoresFn in the scores.R script
  # Raymond Nelson
  # 2019
  #
  ###
  #
  # requires the OSS-3 reference tables
  # source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Model.R')
  # 
  # x input is a data frame of measurements for the RQs and CQs for a test chart
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
    
    RqCqDFSeries$OSS3Score <- ""
    
    OSS3Sensors <- c("UPneumo", 
                     "LPneumo", 
                     "Pneumo",
                     "AutoEDA", 
                     "ManualEDA", 
                     "Cardio")
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    OSS3Sensors <- OSS3Sensors[OSS3Sensors %in% uniqueSensors]
    
    # exit if OSS3 sensors are missing
    if(length(!(OSS3Sensors %in% uniqueSensors)) < 4) {
      return(RqCqDFSeries)
    }
    
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
    if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) { 
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    # if( length(uniqueCharts) < 3 ) { 
    #   "OSS-3 requires a minimum of 3 charts"
    #   return(RqCqDFSeries) 
    # }
    
  }
  
  ######################## iterate on the charts #########################
  
  # iterate on the charts
  i=1
  for(i in 1:length(uniqueCharts)) {
      
    ##### first get the RqCq data frame for this chart #####
    
    thisChart <- uniqueCharts[i]
    thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
    
    RqCqDFChart <- RqCqDFSeries[thisChartRows,]
    # View(RqCqDFChart)
    
    # increment the loop to the next chart if no events
    if(nrow(RqCqDFChart) < 2) { 
      next()
    }
    
    ##### then get the data for the RQs and CQs #####
    
    rqRows <- grep("R", RqCqDFChart$eventLabel)
    cqRows <- grep("C", RqCqDFChart$eventLabel)
    
    # get the RQs and CQs for this chart
    uniqueRQsChart <- unique(RqCqDFChart[rqRows,'eventLabel'])
    uniqueCQsChart <- unique(RqCqDFChart[cqRows,'eventLabel'])
    
    if(length(uniqueRQsChart) < 2 || length(uniqueCQsChart) < 2) {
      next() # next chart in the series
    }
    
    rqDF <- RqCqDFChart[rqRows,]
    cqDF <- RqCqDFChart[cqRows,]
    # View(rqDF)
    # View(cqDF)
    
    assign("rqDF", rqDF, pos=1)
    assign("cqDF", cqDF, pos=1)
    
    #### calculate the mean CQ for each sensor in the chart ####
    
    {
      # initialize a vector to hold the CQ means for the OSS-3 sensors
      cqMeans <- rep("", times=length(OSS3Sensors))
      # then iterate over the sensors to compute the means
      m=3
      for (m in 1:length(OSS3Sensors)) {
        getRows <- which(cqDF$sensorName==OSS3Sensors[m])
        cqMeans[m] <- mean(cqDF$sensorMeasurement[getRows], na.rm=TRUE)
      }
      # coerce to numeric
      cqMeans <- as.numeric(cqMeans)
      names(cqMeans) <- OSS3Sensors
      # print(cqMeans)
    }
    
    #### iterate on RQs and sensors to calculate the logged RQ/CQ ratio ####
    
    # iterate over the OSS3Sensors
    j=1
    for (j in 1:length(OSS3Sensors)) {
      getRows <- which(rqDF$sensorName==OSS3Sensors[j])
      rqValues <- as.numeric(rqDF$sensorMeasurement[getRows])
      if(is.na(cqMeans[j]) || cqMeans[j]==0) {
        rqDF$OSS3Score[getRows] <- rep(NA, length=length(rqValues))
        next()
      }
      # calculate the logged R/Cmean Ratio
      rqDF$OSS3Score[getRows] <- log(rqValues / as.numeric(cqMeans[j]))
    }
    # View(rqDF)
    
    #### pass the results back to the RqCqDFChart and RqCqDFSeries ####
    
    RqCqDFChart[rqRows,] <- rqDF
    # View(RqCqDFChart)
    
    RqCqDFSeries[thisChartRows,] <- RqCqDFChart
    
    # now we have the logged R/C ratios for all charts, sensors and  RQs
    
  } # end loop i over uniqueCharts
  
  ########## invert the logged R/C ratios for EDA and cardio ##############
  
  {
    # invert these so the sign vals match for all sensors
    # - signs are associated with deception and + signs assoc with truth
    
    EDARows <- which(RqCqDFSeries$sensorName=="AutoEDA" | 
                       RqCqDFSeries$sensorName=="ManualEDA")
    EDARows <- EDARows[RqCqDFSeries$OSS3Score[EDARows] != ""]
    RqCqDFSeries$OSS3Score[EDARows] <- 
      -as.numeric(RqCqDFSeries$OSS3Score[EDARows])
    
    CardioRows <- which(RqCqDFSeries$sensorName=="Cardio")
    CardioRows <- CardioRows[RqCqDFSeries$OSS3Score[CardioRows] != ""]
    RqCqDFSeries$OSS3Score[CardioRows] <- 
      -as.numeric(RqCqDFSeries$OSS3Score[CardioRows])
    # View(RqCqDFSeries)
  }
  
  ##################### ipsative trim outlier #######################
  
  {
    
    # trim the outliers to the ipsative distribution
    
    max(as.numeric(RqCqDFSeries$OSS3Score), na.rm=TRUE)
    max(as.numeric(RqCqDFSeries$OSS3Score), na.rm=TRUE)
    meanLogRC <- mean(as.numeric(RqCqDFSeries$OSS3Score), na.rm=TRUE)
    stDevLnRC <- sd(as.numeric(RqCqDFSeries$OSS3Score), na.rm=TRUE)
    
    # calculate the cutValue
    cutValue <- 3.8906 * stDevLnRC + meanLogRC
    
    # replace any value that exceeds +/- cutValue 
    RqCqDFSeries$OSS3Score[as.numeric(RqCqDFSeries$OSS3Score) > cutValue] <- 
      cutValue
    RqCqDFSeries$OSS3Score[as.numeric(RqCqDFSeries$OSS3Score) < -cutValue] <- 
      -cutValue
    
  }
  
  ##########################  OSS-3 sensor norms  #########################
  
  {
    
    # mean and stdev of sensors - combined guilty and innocent cases
    # not sure where these came frome
    # check the OSS3Norms.R script
    # OSS3SensorMeans <- c(RMean=.175, EMean=.495, CMean=.338)
    # OSS3SensorStDevs <- c(RStDev=.078, EStDev=.128, CStDev=.096)
    
    OSS3SensorMeans <- c(RMean=-0.039, EMean=0.019, CMean=-0.018)
    OSS3SensorStDevs <- c(RStDev=0.107, EStDev=0.499, CStDev=0.190)
    
  }
  
  ###############  standardize the logged R/C ratios ################
  
  {
  
    RQRowsSeries <- grep("R", RqCqDFSeries$eventLabel)
    RqDFSeries <- RqCqDFSeries[RQRowsSeries,]
    # View(RqDFSeries)
  
    pneumoRows <- which(RqDFSeries$sensorName == "UPneumo" | 
                          RqDFSeries$sensorName == "LPneumo" | 
                          RqDFSeries$sensorName == "Pneumo" )
    RqDFSeries[pneumoRows,'OSS3Score'] <- 
      ( as.numeric(RqDFSeries[pneumoRows,'OSS3Score']) - 
          OSS3SensorMeans['RMean'] ) / OSS3SensorStDevs['RStDev']
    
    EDARows <- which(RqDFSeries$sensorName == "AutoEDA" |
                       RqDFSeries$sensorName == "ManualEDA")
    RqDFSeries[EDARows,'OSS3Score'] <- 
      ( as.numeric(RqDFSeries[EDARows,'OSS3Score']) - 
          OSS3SensorMeans['EMean'] ) / OSS3SensorStDevs['EStDev']
    
    CardioRows <- which(RqDFSeries$sensorName == "Cardio")
    RqDFSeries[CardioRows,'OSS3Score'] <- 
      ( as.numeric(RqDFSeries[CardioRows,'OSS3Score']) - 
          OSS3SensorMeans['CMean'] ) / OSS3SensorStDevs['CStDev']
  
    RqCqDFSeries[RQRowsSeries,] <- RqDFSeries
    # View(RqCqDFSeries)
    
  }
  
  ################# combine the 2 respirations scores to 1 ################
  
  # iterate over the charts to combine the respiration scores
  i=1
  for(i in 1:length(uniqueCharts)) {
    # get thisChart because we have previously completed iteration on charts
    thisChart <- uniqueCharts[i]
    theseChartRows <- RqCqDFSeries$chartName == thisChart
    theseSensors <-   c("UPneumo", "LPneumo", "Pneumo", "AutoEDA", "Cardio")
    OSS3SensorRows <- RqCqDFSeries$sensorName %in% theseSensors
    rqRows <- grep("R", RqCqDFSeries$eventLabel)
    theseRows <- 
      which(theseChartRows & OSS3SensorRows)[which(theseChartRows & 
                                                     OSS3SensorRows) 
                                             %in% rqRows]
    # initialize a data from of RQs in this chart
    rqDFChart <- RqCqDFSeries[theseRows,]
    # View(rqDFChart)
    uniqueRQsChart <- unique(rqDFChart$eventLabel)
    # iterate over the uniqueRQs
    j=1
    for(j in 1:length(uniqueRQsChart)) {
      thisRQ <- uniqueRQsChart[j]
      thisRQRows <- rqDFChart$eventLabel == thisRQ
      ULPneumoRows <-
        which(rqDFChart$sensorName %in% c("UPneumo", "LPneumo") & thisRQRows)
      ULPneumoVals <- as.numeric(rqDFChart$OSS3Score[ULPneumoRows])
      PneumoRow <- which(rqDFChart$sensorName == "Pneumo" & thisRQRows)
      PneumoVal <- ifelse(prod(ULPneumoVals) < 0,
                          0,
                          ULPneumoVals[which.max(abs(ULPneumoVals))]
      )
      rqDFChart$OSS3Score[PneumoRow] <- PneumoVal
    }
    RqCqDFSeries[theseRows,] <- rqDFChart
  }
  
  ################## boundary condition on missing data ###################
  
  {
    
    # exclude question presentation for which there are < 2 sensor scores
    
    # initialize a new vector for OSS-3 sensor names
    OSS3Sensors2 <- c("Pneumo", "AutoEDA", "Cardio")
    
    OSS3RQRows <- which( RqCqDFSeries$sensorName %in% OSS3Sensors2 & 
                           grepl("R", RqCqDFSeries$eventLabel) )
    
    # keep only the combined respiration value
    rqDFSeries <- RqCqDFSeries[OSS3RQRows,]
    # View(rqDFSeries)
    
    # iterate over the charts
    i=1
    for(i in 1:length(uniqueCharts)) {
      thisChart <- uniqueCharts[i]
      theseChartRows <- rqDFSeries$chartName == thisChart
      # initialize a data frame for the chart
      rqDFChart <- rqDFSeries[theseChartRows,]
      # View(rqDFChart)
      # get the RQ names
      uniqueRQsChart <- unique(rqDFChart$eventLabel)
      # iterate over the RQ columns
      # could maybe use weighted.mean() instead
      j=1
      for(j in 1:length(uniqueRQsChart)) {
        thisRQ <- uniqueRQsChart[j]
        # thisRQ <- colnames(RCRatioDF3Chart)[j]
        thisRQRows <- which(rqDFChart$eventLabel == thisRQ)
        RQVals <- rqDFChart$sensorMeasurement[thisRQRows]
        names(RQVals) <- c("R", "E", "C")
        # check for at least 2 extant values
        if(length(which(!is.na(RQVals) & RQVals != 0)) < 2) {
          # set all values to NA if < 2 extant
          rqDFChart$sensorMeasurement[thisRQRows] <- NA
        }
      }
      rqDFSeries[theseChartRows,] <- rqDFChart
    } 
    
    RqCqDFSeries[OSS3RQRows,] <- rqDFSeries
    # View(RqCqDFSeries)
    
  }
  
  ###################### trim to 3 standard deviations ####################
  
  {
    # replace any value that exceeds +/- 3 stDev with the 3 st
    
    rqDFSeries$OSS3Score[as.numeric(rqDFSeries$OSS3Score) > 3] <- "3"
    rqDFSeries$OSS3Score[as.numeric(rqDFSeries$OSS3Score) < -3] <- "-3"
    
    RqCqDFSeries[OSS3RQRows,] <- rqDFSeries
    # View(RqCqDFSeries)
  }
  
  #######################  OSS-3 weighting function  ######################
  
  {
    # OSS-3 discriminate function
    OSS3WeightingCoefs <- c(R=.192, E=.528, C=.28)
    
    # normalize the weighting function so that it always sums to 1
    OSS3WeightingCoefs <- OSS3WeightingCoefs / sum(OSS3WeightingCoefs)
  }
  
  ###############  weighted means sensor scores for each RQ ################
  
  {
    # initialize a data frame to hold the within-chart weighted mean RQ values
    WMChartDF <- matrix(ncol=length(uniqueRQs),
                        nrow=length(uniqueCharts) )
    colnames(WMChartDF) <- uniqueRQs
    row.names(WMChartDF) <- uniqueCharts
    
    # initialize a vector for the weighted mean RQ scores
    WMeanRQsVals <- rep(NA, times=length(uniqueRQs))
    names(WMeanRQsVals) <- uniqueRQs
    
    # iterate over the charts
    i=1
    for(i in 1:length(uniqueCharts)) {
      thisChart <- uniqueCharts[i]
      theseChartRows <- rqDFSeries$chartName == thisChart
      rqDFChart <- rqDFSeries[theseChartRows,]
      # iterate over the RQs
      j=1
      for(j in 1:length(uniqueRQs)) {
        thisRQ <- uniqueRQs[j]
        # thisRQ <- colnames(RCRatioDF3Chart)[j]
        RQVals <- rqDFChart$OSS3Score[rqDFChart$eventLabel == thisRQ]
        RQVals <- as.numeric(RQVals)
        names(RQVals) <- c("R", "E", "C")
        # could maybe use weighted.mean() instead
        thisRQWMean <- sum(RQVals * OSS3WeightingCoefs) / sum(OSS3WeightingCoefs)
        names(thisRQWMean) <- thisRQ
        # this is to ensure that RQs are correctly aligned
        WMeanRQsVals[names(WMeanRQsVals) == thisRQ] <- thisRQWMean
      }
      # submit the weighted mean RQ values to the data frame
      WMChartDF[i,] <- WMeanRQsVals
      # View(WMChartDF)
    } 
  }
  
  ############### calculate the between-chart means for RQs ##############
  
  # use the WMChartDF from the previous step
  
  RQWMeans <- colMeans(WMChartDF)
  
  ############## calculate the OSS-3 between RQ grand mean ################
  
  OSS3Mean <- mean(RQWMeans)
  
  ###################  OSS-3 reference distributions  #####################
  
  {
    # mean and stDev for weighted sensor scores of guilty and innocent cases
    OSS3RefMean <- c(SRMean=-.581, NSRMean=.586)
    OSS3RefStDev <- c(SRStDev=.453, NSRStDev=.454)
  }
  
  ################  calculate the OSS-3 z-score and p-value ###############
  
  {
    zTruthful <- (OSS3Mean - OSS3RefMean['NSRMean']) / 
      OSS3RefStDev['NSRStDev']
    
    zDeceptive <- (OSS3Mean - OSS3RefMean['SRMean']) / 
      OSS3RefStDev['SRStDev']
    
    pTruthful <- pnorm(zTruthful)
    pDeceptive <- pnorm(zDeceptive)
  }
  
  ############# calculate the z-scores and p-values for RQs ###############
  
  {
    zRQsTruthful <- (RQWMeans - OSS3RefMean['NSRMean']) / 
      OSS3RefStDev['NSRStDev']
    
    zRQsDeceptive <- zDeceptive <- (RQWMeans - OSS3RefMean['SRMean']) / 
      OSS3RefStDev['SRStDev']
    
    pRQsTruthful <- pnorm(zRQsTruthful)
    PRQsDeceptive <- pnorm(zRQsDeceptive)
  }
  
  ###### select the z-scores and p-vals for classification and output ######
  
  {
    thisPVal <- which.min(c(pTruthful, pDeceptive))
    OSS3PVal <- c(pTruthful, pDeceptive)[thisPVal]
    
    thisDistr <- which.min(c(min(pRQsTruthful), min(PRQsDeceptive)))
    RQPVals <- list(pRQsTruthful, PRQsDeceptive)[[thisDistr]]
    thisRQPVal <- which.min(RQPVals)
    minRQPVal <- RQPVals[thisRQPVal]
    minRQName <- names(RQPVals)[thisRQPVal]
  }
  
  ###############  Alphas Boundaries and Decision rules  ##################
  
  {
    # hard-coded alpha boundaries
    # load these in the same env where the decision rules Fn are defined
    screenSR <- round(.05/length(uniqueRQs), 3)
    screenNSR <- round(1-(1-.05)^length(uniqueRQs), 3)
    # 1-(1-0.142625)^(1/length(uniqueRQs))
    OSS3Alpha <- c(GTDI=.05, 
                   GTNDI=.05, 
                   STDI=.05, 
                   STNDI=.05,
                   STDIc=screenSR,
                   STNDIc=screenNSR )
    
    cutScores <- OSS3Alpha
    
    # correct the tail region to parse truthful results
    # permits the same decision rule functions with integer or .p input
    if(which.min(c(pTruthful, pDeceptive)) == 1) {
      cutScores['GTNDI'] <- 1 - cutScores['GTNDI']
      cutScores['STNDI'] <- 1 - cutScores['STNDI']
      cutScores['STNDIc'] <- 1 - cutScores['STNDIc']
      # GTVal <- 1 - GTVal
      # STVals <- 1 - STVals
    }
    
    # source('~/Dropbox/R/NCCA_ASCII_Parse/decisionRules.R', echo=FALSE)
  }
  
  ########## call the decision rules to parse the OSS-3 result ###########
  
  {
    # use the "rule" parameter to select the decision rule
    # the rule parameter must be in the env where the OSS3ScoresFn is defined
    
    TSRResult <- TSRFn(grandTotal=.018, 
                       subtotalScores=c(.01, .015, .03),
                       cutScores=cutScores )
    
    SSRResult <- SSRFn(subtotalScores=c(.01, .015, .03),
                       cutScores=cutScores )
    
    GTRResult <- GTRFn(totalScore=.018, 
                       RQNames=uniqueRQs,
                       cutScores=cutScores )
    
    if(!exists("decisionRule")) decisionRule <- "TSR"
    
    OSS3TestResult <- switch(decisionRule,
                             "TSR"=TSRResult$testResult,
                             "SSR"=SSRResult$testResult,
                             "GTR"=GTRResult$testResult)
    
    OSS3QuestionResults <- switch(decisionRule,
                                  "TSR"=TSRResult$subtotalResults,
                                  "SSR"=SSRResult$subtotalResults,
                                  "GTR"=GTRResult$subtotalResults)
  }
  
  ##################### select the output values ######################
  
  {
    # ifelse is vectorized internally but the the output of ifelse is not 
    GTVal <- ifelse(OSS3TestResult == "NDI/NSR",
                    pTruthful,
                    pDeceptive)
    
    if(decisionRule == "SSR") {
      GTVal <- thisSubtotalScore <- which.min(subtotalScores)
      minSubtotalScore <- subtotalScores[thisSubtotalScore]
      
      STVals <- ifelse(OSS3TestResult == "NDI/NSR",
                       pRQsTruthful,
                       PRQsDeceptive)
    }
  }
  
  ############################### output ###############################
  
  useSensors <- c("UPneumo", 
                  "LPneumo", 
                  "AutoEDA",
                  "Cardio" )
  
  ############## measurement table ##############
  
  measurementsDF <-
    measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                       useSensors=useSensors,
                       makeScoreSheetDF=makeScoreSheetDF,
                       writeScoreSheetCSV=writeScoreSheetCSV )
  
  
  
  # {
  #   
  #   # initialize a data frame for the OSS-3 measurements
  #   measurementsDF <- 
  #     data.frame(matrix(ncol=(length(uniqueQuestions)), 
  #                       nrow=length(uniqueCharts)*length(useSensors)))
  #   names(measurementsDF) <- uniqueQuestions
  #   measurementsDF <- 
  #     cbind(sensorName=rep(useSensors, times=length(uniqueCharts)), 
  #           measurementsDF)
  #   measurementsDF$sensorName <- as.character(measurementsDF$sensorName)
  #   measurementsDF <- 
  #     cbind(chartName=rep(uniqueCharts, each=length(useSensors)), 
  #           measurementsDF)
  #   measurementsDF$chartName <- as.character(measurementsDF$chartName)
  #   
  #   # populate the data frame with the measurements
  #   i=1
  #   for(i in 1:nrow(measurementsDF)) {
  #     thisChart <- measurementsDF[i,1]
  #     thisSensor <- measurementsDF[i,2]
  #     # iterate over the questions
  #     j=3
  #     for(j in 3:ncol(measurementsDF)) {
  #       thisQuestion <- names(measurementsDF)[j]
  #       # now get the measurement
  #       thisOne <- which(RqCqDFSeries$chartName==thisChart &
  #                          RqCqDFSeries$sensorName==thisSensor &
  #                          RqCqDFSeries$eventLabel==thisQuestion)
  #       if(length(thisOne) == 0 ) next()
  #       thisCol <- which(names(measurementsDF) == thisQuestion)
  #       measurementsDF[i,thisCol] <- RqCqDFSeries$sensorMeasurement[thisOne]
  #     }
  #   }
  #   # View(measurementsDF)
  #   
  #   # save the OSS-3 measurements to a data frame and csv 
  #   
  #   measurementTableName <- paste(examName, 
  #                                 seriesName, 
  #                                 "measurementsDF", 
  #                                 sep="_")
  #   
  #   if(!exists("makeScoreSheetDF")) makeScoreSheetDF <- TRUE
  #   
  #   if(isTRUE(makeScoreSheetDF)) {
  #     assign(measurementTableName, measurementsDF, pos=1)
  #   }
  #   
  #   if(!exists("writeScoreSheetCSV")) writeScoreSheetCSV <- FALSE
  #   
  #   if(isTRUE(writeScoreSheetCSV)) {
  #     write.csv(measurementsDF,
  #               file=paste0(str_sub(measurementTableName, 1, -3), ".csv"),
  #               row.names=FALSE)
  #   }
  #   
  # }
  
  ################ score sheet ################
  
  scoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=OSS3Sensors2,
                               scoreType="OSS3Score",
                               outputName="OSS3ScoresheetDF",
                               makeScoreSheetDF=makeScoreSheetDF,
                               writeScoreSheetCSV=writeScoreSheetCSV)
  
  # {
  #   
  #   scoreSheetDF <- data.frame(matrix(ncol=(length(uniqueRQs)),
  #                                     nrow=length(uniqueCharts) *
  #                                       length(useSensors) ) )
  #   names(scoreSheetDF) <- uniqueRQs
  #   scoreSheetDF <- cbind(examName=examName,
  #                         seriesName=seriesName,
  #                         chartName=rep(uniqueCharts, each=length(useSensors)),
  #                         sensorName=rep(useSensors, times=length(uniqueCharts)),
  #                         scoreSheetDF)
  #   
  #   # iterate over the RQs
  #   # ensures that RQs are correctly aligned for rotated charts
  #   i=1
  #   for(i in 1:length(uniqueRQs)) {
  #     thisRQ <- uniqueRQs[i]
  #     RQRows <- RqCqDFSeries$eventLabel == thisRQ
  #     OSS3SensorRows <- RqCqDFSeries$sensorName %in% useSensors
  #     theseRows <- which(RQRows & OSS3SensorRows)
  #     scoreSheetDF[,thisRQ] <- RqCqDFSeries$OSS3Score[theseRows]
  #   }
  #   
  #   # fix the columns data types
  #   scoreSheetDF$examName <- as.character(scoreSheetDF$examName)
  #   scoreSheetDF$seriesName <- as.character(scoreSheetDF$seriesName)
  #   scoreSheetDF$chartName <- as.character(scoreSheetDF$chartName)
  #   scoreSheetDF$sensorName <- as.character(scoreSheetDF$sensorName)
  #   i=5
  #   for(i in 5:ncol(scoreSheetDF)) {
  #     thisCol <- names(scoreSheetDF)[i]
  #     scoreSheetDF[,thisCol] <- as.numeric(scoreSheetDF[,thisCol])
  #   }
  #   # View(scoreSheetDF)
  #   
  #   scoreSheetName <- paste(examName, seriesName, "OSS3ScoresheetDF", sep="_")
  #   
  #   if(isTRUE(makeScoreSheetDF)) {
  #     assign(scoreSheetName, scoreSheetDF, pos=1)
  #   }
  #   
  #   if(isTRUE(writeScoreSheetCSV)) {
  #     write.csv(scoreSheetDF,
  #               file=paste0(str_sub(scoreSheetName, 1, -3), ".csv"),
  #               row.names=FALSE)
  #   }
  #   
  # }
    
  ############### series subtotals ###############
  
  
  
  {
    
    # series subtotals are the RQ subtotals for all charts
    
    seriesTotalsDF <- data.frame(matrix(ncol=(length(uniqueRQs)), 
                                        nrow=1 ) )
    names(seriesTotalsDF) <- uniqueRQs
    seriesTotalsDF <- cbind(examName=examName,
                            seriesName=seriesName,
                            subTotal="subTotal",
                            seriesTotalsDF )
    
    # uses the scoreSheetDF
    seriesTotalsDF[1,c(4:ncol(seriesTotalsDF))] <-
      colSums(scoreSheetDF[,c(5:ncol(scoreSheetDF))], na.rm=TRUE)
    
    seriesTotalsDF$grandMean <- rowMeans(seriesTotalsDF[,c(4:ncol(seriesTotalsDF))], 
                                         na.rm=TRUE)
    
    # write the score sheet to a data frame and .csv
    
    seriesTotalsDFName <- paste(examName,
                                seriesName,
                                "OSS3SeriesTotalsDF",
                                sep="_")
    
    if(isTRUE(makeDF)) {
      assign(seriesTotalsDFName, seriesTotalsDF, env=.GlobalEnv)
    }
    
    if(isTRUE(saveCSV)) {
      write.csv(seriesTotalsDF,
                file=paste0(str_sub(seriesTotalsDFName, 1, -3), ".csv"),
                row.names=FALSE )
    }
    
  }
  
  ############ chart subtotals ############
  
  
  
  {
    
    # including RQ subtotals and chart subtotals
    
    # chartTotalsDF <- data.frame(matrix(ncol=(length(uniqueRQs)), 
    #                                    nrow=length(uniqueCharts) ) )
    chartTotalsDF <- WMChartDF
    row.names(chartTotalsDF) <- NULL
    # View(chartTotalsDF)
    
    names(chartTotalsDF) <- uniqueRQs
    chartTotalsDF <- cbind(examName=examName,
                           seriesName=seriesName,
                           chartName=uniqueCharts,
                           subTotal="subTotal",
                           chartTotalsDF)
    # View(chartTotalsDF)
    
    # write the subtotals to a data frame and .csv
    
    chartTotalsDFName <- paste(examName,
                               seriesName,
                               "OSS3ChartTotalsDF",
                               sep="_")
    
    if(isTRUE(makeDF)) {
      assign(chartTotalsDFName, chartTotalsDF, env=.GlobalEnv)
    }
    
    if(isTRUE(saveCSV)) {
      write.csv(chartTotalsDF,
                file=paste0(str_sub(chartTotalsDFName, 1, -3), ".csv"),
                row.names=FALSE )
    }
    
  }
  
  ######## sensor scores for the series ########
  
  
  
  
  {
    
    sensorTotalsDF <- data.frame(matrix(ncol=(length(useSensors)), 
                                        nrow=length(uniqueCharts) ) )
    names(sensorTotalsDF) <- useSensors
    sensorTotalsDF <- cbind(examName=examName,
                            seriesName=seriesName,
                            chartName=uniqueCharts,
                            subTotal="subTotal",
                            sensorTotalsDF)
    
    # iterate over the charts and useSensors vectors
    # uses the scoreSheetDF
    i=1
    for(i in 1:length(uniqueCharts)) {
      thisChart <- uniqueCharts[i]
      theseChartRows <- scoreSheetDF$chartName == thisChart
      # iterate over the sensors
      j=1
      for(j in 1:length(useSensors)) {
        thisSensor <- useSensors[j]
        theseSensorRows <- scoreSheetDF$sensorName == thisSensor
        thisRow <- which(theseChartRows & theseSensorRows)
        thisSensorMean <- 
          mean(as.numeric(scoreSheetDF[thisRow,c(5:ncol(scoreSheetDF))]), 
               na.rm=TRUE )
        sensorTotalsDF[sensorTotalsDF$chartName==thisChart,
                       names(sensorTotalsDF)==thisSensor] <- thisSensorMean
      }
    }
    # View(sensorTotalsDF)
    
    # write the subtotals to a data frame and .csv
    
    sensorTotalsDFName <- paste(examName,
                                seriesName,
                                "OSS3SensorSubtotalsDF",
                                sep="_")
    
    if(isTRUE(makeDF)) {
      assign(sensorTotalsDFName, sensorTotalsDF, env=.GlobalEnv)
    }
    
    if(isTRUE(saveCSV)) {
      write.csv(sensorTotalsDF,
                file=paste0(str_sub(sensorTotalsDFName, 1, -3), ".csv"),
                row.names=FALSE )
    }
    
  }
  
  ############ construct a list to hold the OSS=3 result ##############
  
  outputListName <- paste(examName, seriesName, "OSS3OutputList", sep="_")

  OSS3OutputList <- list(OSS3Score=OSS3PVal,
                         OSS3Result=OSS3TestResult,
                         OSS3QuestionResults=OSS3QuestionResults,
                         OSS3MinRQPVal=minRQPVal,
                         OSS3MinRQName=minRQName,
                         OSS3RQPVals=RQPVals,
                         OSS3Alphas=OSS3Alpha,
                         OSS3DecisionRule=decisionRule,
                         OSS3Measurements=measurementsDF,
                         OSS3ScoreSheet=scoreSheetDF,
                         OSS3PTruthful=pTruthful,
                         OSS3PDecetive=pDeceptive,
                         OSS3ZTruthful=zRQsTruthful,
                         OSS3ZDeceptive=zRQsDeceptive )

  #### save the list to the globad env as a side effect

  assign(outputListName, OSS3OutputList, env=.GlobalEnv)
  
  #### return the RqCqDFSeries to the getScoresFn() ####
  
  return(RqCqDFSeries)

  # end OSS3ScoresFn
}  


