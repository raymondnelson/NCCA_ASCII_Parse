# R function to compute scores and results for various algorithms
# Raymond Nelson
# 10-25-2016

####

# source a script to load the selectCQFn to choose CQs for each RQ
# source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)

# source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)

# source the amplitudeExtractHelperFunctions.R script to load the spd function
# for the population st dev
# source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=FALSE)

# source some scripts to calclate various types of numerical scores
# source('~/Dropbox/R/NCCA_ASCII_Parse/rankScores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/RRMScore.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/miritelloRank.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/ipsativeZScore.R', echo=FALSE)
  
# for ESS scores
# source('~/Dropbox/R/NCCA_ASCII_Parse/RCScores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/ESSScores.R', echo=FALSE)

# OSS-3
# source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Scores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Model.R', echo=FALSE)

# OSS-2
# source('~/Dropbox/R/NCCA_ASCII_Parse/OSS2Scores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/OSS2Model.R', echo=FALSE)

# Probability Analysis
# source('~/Dropbox/R/NCCA_ASCII_Parse/PAScores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/PAModel.R', echo=FALSE)

# Rank Order Scoring System
# source('~/Dropbox/R/NCCA_ASCII_Parse/ROSSScores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/ROSSModel.R', echo=FALSE)

# MacLaren Krapohl Permutation Scoring System
# source('~/Dropbox/R/NCCA_ASCII_Parse/PSSScores.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/PSSModel.R', echo=FALSE)


# RCScoreArgs <- list(x="examName", y="seriesName", z="chartName", makeDF=TRUE, output=FALSE)

# examName <- "DX199457YouPhase"
# seriesName <- "2"
# chartName <- "01A"
# x <- "DX199457YouPhase"
# x <- "DPEMSER120419RODRIGOALEXANDERMORENOPATARROYOPRE"
# y <- "2"
# z <- "ALL"


getScoresFn <- function() {
  # R function to compute the exam scores from the measurement data frome
  # Raymond Nelson
	# 10-25-2016
	# 2-2-2019
	#
  # examName input is a scalar with a unique exam name
  # obtained from the global envir
  #
  # this function does not iterate over a vector of exam names
  # instead, it will get the measurement data frame from the global environment
  # seriesName input is a scalar with the series name
  # chartName input is a scalar with the chart name
  # makeDF=TRUE will assign the function output to a data frame in the global env as a side effect
  # output=TRUE will output the measurement data frame in the normal manner
  #
  ####
  
  # called by the getExamFn function in the getSegment.R script
  # getExamFn() is called in the workFlow.R script
  
  # this script will call other functions from other scripts
	
	# source('~/Dropbox/R/NCCA_ASCII_Parse/rankScores.R', echo=TRUE)
	# source('~/Dropbox/R/NCCA_ASCII_Parse/RRMScore.R', echo=TRUE)
	# source('~/Dropbox/R/NCCA_ASCII_Parse/miritelloRank.R', echo=TRUE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/ipsativeZScore.R', echo=TRUE)
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/OSS2Scores.R', echo=TRUE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/ESSScores.R', echo=TRUE)
  # OSS3
  # Probability Analysis
  # ROSS
  # Bootstrap Test
  # Permutation Test
  
  # PCASS test
  
  ####
  
  # examName=examName, 
  # seriesName=seriesName, 
  # makeDF=TRUE, 
  # output=FALSE 
  
  # print a message
  # myChartFUN()

  # examName <- x
  # seriesName <- y
  # chartName <- z
  
  # seriesName <- "2"
  # chartName <- "04A"
  
  # chartName <- "03A"
  
  # assign("examName", examName, pos=1)
  # assign("seriesName", seriesName, pos=1)
  # assign("chartName", chartName, pos=1)

  ############## get the measurements data frame for this exam ##############
  
  {
    searchString <- paste0("*", examName, "_Measurements", "*")
    if(!exists("searchString")) return()
    
    # initialize the measurementDF for the exam
    measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # View(measurementDF)
    if(is.null(measurementDF)) return()
    
  }
  
  ########## reset the scores ###########
  
  {
    
    # all series
    # measurementDF$rankScore <- ""
    # measurementDF$RRMScore <- ""
    # measurementDF$miritelloRankScore <- ""
    # measurementDF$ipZScore <- ""
    
    # measurementDF$RCScore <- ""
    # measurementDF$CQName <- ""
    # measurementDF$CQMean <- ""
    
    # CQT series
    # measurementDF$ESSScore <- ""
    # measurementDF$OSS2Score <- ""
    # measurementDF$OSS3Score <- ""
    # measurementDF$PAScore <- ""
    # measurementDF$ROSSScore <- ""
    # measurementDF$PSSScore <- ""
    # measurementDF$bootstrapScore <- ""
    
    # measurementDF$PCASSScore <- ""
    
  }
  
  ###### coerce these from factor variables to character ######
  
  # {
  #   measurementDF$rankScore <- as.character(measurementDF$rankScore)
  #   measurementDF$RRMScore <- as.character(measurementDF$RRMScore)
  #   measurementDF$miritelloRankScore <- 
  #     as.character(measurementDF$miritelloRankScore)
  #   measurementDF$ipZScore <- as.character(measurementDF$ipZScore)
  #   
  #   measurementDF$RCScore <- as.character(measurementDF$RCScore)
  #   measurementDF$CQName <- as.character(measurementDF$CQName)
  #   measurementDF$CQMean <- as.character(measurementDF$CQMean)
  #   
  #   measurementDF$ESSScore <- as.character(measurementDF$ESSScore)
  #   measurementDF$OSS3Score <- as.character(measurementDF$OSS3Score)
  #   measurementDF$OSS2Score <- as.character(measurementDF$OSS2Score)
  #   measurementDF$PAScore <- as.character(measurementDF$PAScore)
  #   measurementDF$ROSSScore <- as.character(measurementDF$ROSSScore)
  #   measurementDF$PSSScore <- as.character(measurementDF$PSSScore)
  #   measurementDF$bootstrapScore <- 
  #     as.character(measurementDF$bootstrapScore)
  #   
  #   measurementDF$PCASSScore <- as.character(measurementDF$PCASSScore)
  #   
  #   assign("measurementDF", measurementDF, pos=1)
  #   # assign("measurementDF", measurementDF, env=.GlobalEnv)
  #   # View(measurementDF)
  # }
  
  ######### slice the measurement data frame for the series ##########
  
  {
    # keep only the measurements for the selected series
    seriesRows <- which(measurementDF$seriesName==seriesName)
    
    seriesMeasurementDF <- measurementDF[seriesRows,]
    # View(seriesMeasurementDF)
    
    # assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
    
        # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(seriesMeasurementDF$sensorName))
    
    # initialize a vector of min values for the sensor measurements
    # minSensorVal <- c(15,15,15,30,30,30,30,30,30,30,30,30,30,.0953)
    # minSensorVal <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,.01)
    # names(minSensorVal) <- uniqueSensors
    
    # initialize a vector of unique events
    # measurementDF does not include X and XX announcements
    uniqueEventsSeries <- unique(seriesMeasurementDF$eventLabel)
    
    if(length(uniqueEventsSeries) == 0) {
      print("no events")
      return()
    }
  }
  
  ########## average the upper and lower pneumo measurements ##########

  if(nrow(seriesMeasurementDF) != 0) {

    uniqueCharts <- unique(seriesMeasurementDF$chartName)
    
    # iterate over the exam charts
    i=1
    for(i in 1:length(uniqueCharts)) {
      
      {
        thisChartName <- uniqueCharts[i]
        chartRows <- which(seriesMeasurementDF$chartName == thisChartName)
        
        # isolate the measurement data frame for the chart
        chartMeasurementDF <- seriesMeasurementDF[chartRows,]
        # View(chartMeasurementDF)
        
        # assign("chartMeasurementDF", chartMeasurementDF, pos=1)
        
        if(nrow(chartMeasurementDF) == 0) next()
        
        uniqueEventsChart <- unique(chartMeasurementDF$eventLabel)
      }
      
      # iterate to combine the upper and lower pneumo measurements 
      j=1
      for (j in 1:length(uniqueEventsChart)) {
        # get the rows for each unique event
        eventRows <- which(chartMeasurementDF$eventLabel==uniqueEventsChart[j])
        # make a data frame for the event
        eventDF <- chartMeasurementDF[eventRows,]
        # get the upper and lower pneumo measurements
        upperPn <- eventDF$sensorMeasurement[which(eventDF$sensorName=="UPneumo")]
        lowerPn <- eventDF$sensorMeasurement[which(eventDF$sensorName=="LPneumo")]
        # calculate the mean, ignoring NA values
        combinedPn <- mean(c(upperPn, lowerPn), na.rm=TRUE)
        # add the combined mean to the eventDF
        eventDF$sensorMeasurement[which(eventDF$sensorName=="Pneumo")] <- combinedPn
        # add the eventDF to the chartMeasurementDF
        chartMeasurementDF[eventRows,] <- eventDF
      } # end for loop j over uniqueEventsChart
      
      seriesMeasurementDF[chartRows,] <- chartMeasurementDF
      
    } # end loop i over charts
    
    # submit the series measurement data frame to the exam measurement data frame
    measurementDF[seriesRows,] <- seriesMeasurementDF

    # save to the global environment for inspection
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    # View(measurementDF)
    
  }
  
  #########  initialize the RqCqDF data frame for the series  ##########
  
  # the RqCqDF contains only RQs and CQs
  
  if(length(uniqueEventsSeries) != 0) {
    
    # make a vector of RQs and CQs for the series 
    
    RqCqSeriesRows <- grep("[CR]+", seriesMeasurementDF$eventLabel)
    
    if(length(RqCqSeriesRows) != 0) {
      # exclude sacrifice relevant questions
      SRRows <- grep("SR", seriesMeasurementDF$eventLabel)
      SRRows <- c(SRRows, grep("RS", seriesMeasurementDF$eventLabel))
      # make a vector of working rows for CQs and RQs
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% SRRows)]
      # exclude "CT" (cleared throat) annotations
      CTRows <- grep("CT", seriesMeasurementDF$eventLabel)
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% CTRows)]
    }    
    
    # keep all events except the first event if no RQ and CQ events
    if(length(RqCqSeriesRows) == 0) {
      # get all indices for events not in excludedEvents
      RqCqSeriesRows <- which(!(seriesMeasurementDF$Label %in% excludeEvents))
      # get the event lables from the eventLabels column
      workingEvents <- seriesMeasurementDF$eventLabel[RqCqSeriesRows]
      workingEvents <- unique(workingEvents)
      # replace the working events with the Labels column
      workingEvents <- 
        seriesMeasurementDF$Label[seriesMeasurementDF$eventLabel %in% 
                                    workingEvents]
      # complete the vector of working rows
      RqCqSeriesRows <- which(seriesMeasurementDF$Label %in% workingEvents)
      # remove the rows for the first event
      RqCqSeriesRows <- seriesMeasurementDF$eventLabel %in% 
        (unique(seriesMeasurementDF$eventLabel[RqCqSeriesRows])[-1])
    }
    
    # return the measurementDF and exit if no stimulus events
    if(length(RqCqSeriesRows) == 0) {
      if(output == TRUE) {
        return(measurementDF)
      } else return()
    }
    
    # initialize the RqCQDF for the series
    RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("RqCqSeriesRows", RqCqSeriesRows, pos=1)
    
    # View(RqCqDFSeries)
    
  } 
  
  #### get the RQs and CQs and stop if uniqueRQs != uniqueCQs #####
  
  {
    uniqueRQs <- 
      unique(RqCqDFSeries$eventLabel[grep("R", RqCqDFSeries$eventLabel)])
    uniqueCQs <- 
      unique(RqCqDFSeries$eventLabel[grep("C", RqCqDFSeries$eventLabel)])
    
    # check if equal number of RQs and CQs
    if(isTRUE(checkRQCQs)) {
      if( (length(uniqueRQs) != 3) || (length(uniqueRQs) != length(uniqueCQs)) ) {
        print(examName)
        stop("unequal RQs and CQs")
      }
    }
    
    
  }
  
  ######### initialize a named vector of measurement values #########
  
  { 
    measurementsVc <- RqCqDFSeries$sensorMeasurement
    
    names(measurementsVc) <- 
      paste(RqCqDFSeries$chartName, RqCqDFSeries$eventLabel, sep="_")
    
    # this does not seem to be used for anything 2019-07-10
  } 
  
  ##########    calculate the rank scores for the chart    ###########
  
  if(isTRUE(getRankScores)) {
    
    # use a function to get the rank scores for each chart in the series
    # source('~/Dropbox/R/NCCA_ASCII_Parse/rankScores.R', echo=TRUE)
    RqCqDFSeries <- rankScoreFn(RqCqDFSeries=RqCqDFSeries, 
                                makeDF=makeDF,
                                saveCSV=saveCSV)
    
    # RqCqDFSeries$rankScore <- rankScores
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # View(RqCqDFSeries)
    
    # pass the RqCqDFSeries to the series measurement data frame
    # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
   #  measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # save to the global environment for inspection
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    
    ### 11-9-2016 need to find out how ranks are computed for combined pneumo
    
  }
  
  ######  cacluate the RRM relative response magnitude scores  ######
  
  if(isTRUE(getRRMScores)) {
    
    # use a function to get the RRM scores
    # source('~/Dropbox/R/NCCA_ASCII_Parse/RRMScore.R', echo=TRUE)
    RqCqDFSeries <- RRMScoreFn(RqCqDFSeries=RqCqDFSeries, 
                               makeDF=TRUE,
                               saveCSV=FALSE)
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # View(RqCqDFSeries)
    
    # pass the RqCqDFSeries to the series measurement data frame
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # save to the global environment for inspection
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    
    ### 11/6/2016 need to combine the RRM scores for upper and lower pneumo into a single score
    
  }
  
  ########   calculate the Miritello rank scores for the chart    ########
  
  if(isTRUE(getMiritelloRankScores)) {
    
    # use a function to get the Miritello rank scores
    # source('~/Dropbox/R/NCCA_ASCII_Parse/miritelloRank.R', echo=TRUE)
    RqCqDFSeries <- miritelloRankFn(RqCqDFSeries=RqCqDFSeries, 
                                    makeDF=TRUE,
                                    saveCSV=FALSE)
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # View(RqCqDFSeries)
    
    # pass the RqCqDFSeries to the series measurement data frame
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # save to the global environment for inspection
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    
  }
  
  #########   calculate the ipsative Z scores for the chart   #########
    
  if(isTRUE(getIpsativeZScores)) {
    
    # use a function to get the Miritello rank scores
    # source('~/Dropbox/R/NCCA_ASCII_Parse/ipsativeZScore.R', echo=TRUE)
    RqCqDFSeries <- ipsativeZFn(RqCqDFSeries=RqCqDFSeries, 
                                makeDF=TRUE,
                                saveCSV=FALSE)
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # View(RqCqDFSeries)
    
    # pass the RqCqDFSeries to the series measurement data frame
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # save to the global environment for inspection
    # assign("chartMeasurementDF", chartMeasurementDF, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    
  }
  
  ############  exit if not 2 RQs and 2 CQs in this series  ############
  
  {
    
    rqRows <- grep("R", seriesMeasurementDF$Label)
    cqRows <- grep("C", seriesMeasurementDF$Label)
    
    uniqueRQs <- unique(seriesMeasurementDF$Label[rqRows])
    uniqueCQs <- unique(seriesMeasurementDF$Label[cqRows])
    
    if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
      
      # assign the measurement DF to the original object name in the global env
      assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)

      if(makeDF==TRUE) {
        assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)
      }
      if(output == TRUE) {
        return(measurementDF)
      } else {
        return()
      }
      
    }
    
  }
  
  ######  calculate the Probability Analysis scores for the series #######
  
  # probability analysis requires the entire series including all charts 
  # unlike other algorithms that work with one chart at a time
  
  # there is always a first chart, even when not questions 
  
  if(isTRUE(getPAScores)) {
    
    RqCqDFSeries <- PAScoresFn(RqCqDFSeries=RqCqDFSeries,
                               forced=TRUE,
                               PADecisionRule=PADecisionRule,
                               priorProb=.5,
                               PACutProbT=.7,
                               PACutProbD=.3,
                               makeDF=makeDF,
                               saveCSV=saveCSV )
    
    # View(RqCqDFSeries)

    # pass the RqCQDFSeries bac to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries

    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF

    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    
  }
  
  ############  calculate the OSS-2 scores for the series  ############
  
  if(isTRUE(getOSS2Scores)) {
    
    # OSS-2 is for 3 questions single issue exams with 3 CQs and 3 charts
    # OSS-2 compares each RQ to the preceding CQ
    # OSS-2 differs only slightly from OSS-1
    # for which Fed ZCT R5 is compared to the bracketing CQ with the greater
    # change in physiological activity

    RqCqDFSeries <- OSS2ScoresFn(RqCqDFSeries=RqCqDFSeries,
                                 forced=TRUE,
                                 OSS2DecisionRule=OSS2DecisionRule,
                                 oss2AlphaT=oss2AlphaT,
                                 oss2AlphaD=oss2AlphaD,
                                 makeDF=TRUE,
                                 saveCSV=FALSE )

    # View(RqCqDFSeries)

    # pass the RqCQDFSeries back to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries

    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF

    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    # View(measurementDF)
    
  }
  
  ####################   calculate the OSS-3 scores   ###################
  
  # OSS- 3 is for single or multiple issue exams with 2-4 CQs and 3-5 charts
  # OSS-3 compares each RQ to the mean CQ
  
  if(isTRUE(getOSS3Scores)) { 
    
    # source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Scores.R', echo=TRUE)
    
    if(!exists("OSS3DecisionRule")) OSS3DecisionRule <- "TSR"
    
    RqCqDFSeries <- OSS3ScoresFn(RqCqDFSeries=RqCqDFSeries,
                                 OSS3DecisionRule=OSS3DecisionRule, 
                                 makeDF=makeDF,
                                 saveCSV=saveCSV)
    
    # View(RqCqDFSeries)
    
    # pass the RqCQDFSeries back to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    # View(measurementDF)
    
  }
  
  ##########  calculate the Honts & Driscoll (1988) ROSS Score  ##########
  
  {
    
    if(!exists("ROSSDecisionRule")) ROSSDecisionRule <- "GTR"
    
    RqCqDFSeries <- ROSSScoresFn(RqCqDFSeries=RqCqDFSeries,
                                 ROSSDecisionRule=ROSSDecisionRule,
                                 forced=TRUE,
                                 makeDF=makeDF,
                                 saveCSV=saveCSV)
    
    # View(RqCqDFSeries)
    
    # pass the RqCQDFSeries back to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    # View(measurementDF)
    
  }
  
  ####################  calculate the R/C scores   #####################
  
  if(isTRUE(getOSS3Scores) && !isTRUE(getRCScores)) { getRCScores <- TRUE }
  
  if(isTRUE(getRCScores)) {
    
    # use a function to get the R/C scores for the chart
    # source('~/Dropbox/R/NCCA_ASCII_Parse/RCScores.R', echo=TRUE)
    # will call the selectCQFn function from the selectCQ.R script
    # source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)

    # need the R/C scores for all charts
    # before calculating ESS for all charts
    # so that the selectCQFn can evaluate all events in the question sequence

    seriesMeasurementDF <- RCScoresFn(seriesMeasurementDF=seriesMeasurementDF,
                                      useMean=FALSE)
    # View(seriesMeasurementDF)

    # re-acquire the RqCqDFSeries
    RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
    # View(RqCqDFSeries)

    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF

    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(measurementDF)
    
  }
  
  ################  calculate the ESS-M integer scores  #################
  
  if(isTRUE(getESSScores)) {
    
    # use a function to get the ESS integer scores
    # source('~/Dropbox/R/NCCA_ASCII_Parse/ESSScores.R', echo=TRUE)

    # use the RqCqSeriesDF

    RqCqDFSeries <- ESSScoresFn(RqCqDFSeries=RqCqDFSeries,
                                ESSMDecisionRule=ESSMDecisionRule,
                                makeDF=makeDF,
                                saveCSV=saveCSV)

    # View(RqCqDFSeries)

    # pass the RqCQDFSeries back to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries

    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF

    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    # View(measurementDF)
    
  }
  
  ##########  calculate the MacLaren Krapoh 2003 Permutation Score  ##########
  
  if(isTRUE(getPSSScores)) {
    
    if(!exists("PSSDecisionRule")) PSSDecisionRule <- "GTR"
    
    RqCqDFSeries <- PermutationTestScoresFn(RqCqDFSeries=RqCqDFSeries,
                                            PSSDecisionRule=PSSDecisionRule,
                                            forced=TRUE,
                                            priorProb=.5,
                                            PSSCutProbT=.1,
                                            PSSCutProbD=.9,
                                            makeDF=makeDF,
                                            saveCSV=saveCSV)
    
    # View(RqCqDFSeries)
    
    # pass the RqCQDFSeries bac to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
  }

  #########  calculate the Honts & Dewitt 1992 Bootstrap Score  ##########
  
  if(isTRUE(getBootstrapScores)) {
    
    if(!exists("BootstrapTestDecisionRule")) BootstrapTestDecisionRule <- "GTR"
    
    RqCqDFSeries <- bootstrapScoresFn(RqCqDFSeries=RqCqDFSeries,
                                      bootstrapDecisionRule=bootstrapDecisionRule,
                                      bootstrapCutProbT=.3,
                                      bootstrapCutProbD=.7,
                                      forced=TRUE,
                                      makeDF=makeDF,
                                      saveCSV=saveCSV)
    
    # View(RqCqDFSeries)
    
    # pass the RqCQDFSeries bac to the seriesMeasurementDF
    seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
    
    # pass the seriesMeasurementDf back to the measurementDF
    measurementDF[seriesRows,] <- seriesMeasurementDF
    
    # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
    # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
    assign("measurementDF", measurementDF, pos=1)
    # View(seriesMeasurementDF)
    
  }
  
  ########################    output    #######################
  
  # assign the measurement DF to the original object name in the global env
  assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)
  
  # function output
  if(output == TRUE) return(measurementDF)

} # end getScoresFn function



####



# getScoresFn(x=examName, y=seriesName, z=chartName)



# myExamFUN <- function() {print("ray")}
# mySeriesFUN <- function() {print("irv")}
# # myChartFUN <- function() {print("nel")}
# mySegmentFUN <- function() {print("rin")}



# this is called using do.call() 
# do.call() can take a function or character string as the first arg
# chartFUN <- RCScoreFn
# 
# getExamFn(x=uniqueExams)


# calculate the sum
# sum(as.numeric(a09N1214_Measurements$integerScore), na.rm=TRUE)

# rqRows <- grep("R", a09N1214_Measurements$eventLabel)

# RQs <- unique(a09N1214_Measurements$eventLabel[rqRows])

# names(RQs) <- RQs

# length(RQs)

# loop over the RQs to caculate the the RQ scores
# for (i in 1:length(RQs)) {
#   selectRows <- which(a09N1214_Measurements$eventLabel == RQs[i])
#   # assign(RQs[i], sum(as.numeric(a09N1214_Measurements$integerScore[selectRows]), na.rm = TRUE))
#   RQs[i] <- sum(as.numeric(a09N1214_Measurements$integerScore[selectRows]), na.rm = TRUE)
# }



# RQs


