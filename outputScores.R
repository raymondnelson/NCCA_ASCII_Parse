# R Functions to create output tables for polygraph scoring algorithms
# Mar 1, 2019
# Raymond Nelson
###
#
# this script includes several functions
#
# measurementTableFn() measurement data frame - all CQs and RQs for all charts
# rankTableFn() rank score sheet data frame - rank vals for all CQs and RQs all charts
# scoreSheetFn() score sheet data frame - all RQ scores for all charts
# seriesTotalsFn() series subtotals - all RQ subtotal scores for the series
# chartTotalsFn() chart subtotals - all RQ subtotals x all charts
# checkScoresFn() private function in chartTotalsFn
# sensorSubtotalsFn() sensor subtotals - all sensor subtotals x all charts
# questionSequenceFn() construct a table of question sequences for all charts

# Oct 14, 2024
# artifactTableFn() a 2x3 table of artifacts for CQs and RQs  (questions x charts x sensors) 

#
# May 11, 2020 need to add a table for CQ selection
# can use the scoreSheetFn with "CQName" parameter
#
####



############## measurement table ##############

measurementTableFn <- function(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=useSensors,
                               decimals=2,
                               makeDF=TRUE,
                               saveCSV=FALSE,
                               transpose=FALSE ) {
  # make a data frame for all measurements in the series
  # all charts
  # all RQs
  # all sensors
  #
  # input is a data from of all RQs and CQs
  # output is a data frame of measurements 
  #
  # makeDF
  # saveCSV
  ####
  
  options(warn=2)
  
  {
    # View(RqCqDFSeries)
    ## reduce the input data frame to RQs and CQs ##
    RqCqSeriesRows <- grep("[CR]+", RqCqDFSeries$eventLabel)
    if(length(RqCqSeriesRows) != 0) {
      # exclude sacrifice relevant questions
      SRRows <- grep("SR", RqCqDFSeries$eventLabel)
      SRRows <- c(SRRows, grep("RS", RqCqDFSeries$eventLabel))
      # make a vector of working rows for CQs and RQs
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% SRRows)]
      # exclude "CT" (cleared throat) annotations
      CTRows <- grep("CT", RqCqDFSeries$eventLabel)
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% CTRows)]
    }    
    # keep all events except the first event if no RQ and CQ events
    if(length(RqCqSeriesRows) == 0) {
      # get all indices for events not in excludedEvents
      RqCqSeriesRows <- which(!(RqCqDFSeries$Label %in% excludeEvents))
      # get the event lables from the eventLabels column
      workingEvents <- RqCqDFSeries$eventLabel[RqCqSeriesRows]
      workingEvents <- unique(workingEvents)
      if(length(workingEvents) <= 1) return("insufficient number of events")
      # replace the working events with the Labels column
      workingEvents <- 
        RqCqDFSeries$Label[RqCqDFSeries$eventLabel %in% 
                                    workingEvents]
      # complete the vector of working rows
      RqCqSeriesRows <- which(RqCqDFSeries$Label %in% workingEvents)
      # remove the rows for the first event
      # RqCqSeriesRows <- RqCqDFSeries$eventLabel %in% 
      #   (unique(RqCqDFSeries$eventLabel[RqCqSeriesRows])[-1])
    }
    # return the measurementDF and exit if no stimulus events
    if(length(RqCqSeriesRows) == 0) {
      if(output == TRUE) {
        return("no events")
      } else return()
    }
    # initialize the RqCQDF for the series
    RqCqDFSeries <- RqCqDFSeries[RqCqSeriesRows,]
    # assign("RqCqSeriesRows", RqCqSeriesRows, pos=1)
  }
  
  ### initial set up ###
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    uniqueSensors <- unique(RqCqDFSeries$sensorName)
    
    if(!exists("decimals")) decimals <- 2
    
    if(!exists("useSensors")) useSensors <- c("UPneumo",
                                              "LPneumo",
                                              "AutoEDA",
                                              "Cardio",
                                              "PLE")
    
    useSensors <- useSensors[useSensors %in% uniqueSensors]
    
    if(!exists("makeDF")) makeDF <- TRUE
    
    if(!exists("saveCSV")) saveCSV <- TRUE
    
  }
  
  ### initialize a data frame for the measurements ###
  
  {
    measurementsDF <- 
      data.frame(matrix(ncol=(length(uniqueQuestions)), 
                        nrow=length(uniqueCharts)*length(useSensors)))
    names(measurementsDF) <- uniqueQuestions
    measurementsDF <- 
      cbind(sensorName=rep(useSensors, times=length(uniqueCharts)), 
            measurementsDF)
    measurementsDF$sensorName <- as.character(measurementsDF$sensorName)
    measurementsDF <- 
      cbind(chartName=rep(uniqueCharts, each=length(useSensors)), 
            measurementsDF)
    measurementsDF$chartName <- as.character(measurementsDF$chartName)
	# May 13, 20201
	# measurementDF <- cbind(seriesName=as.character(seriesName), measurementDF)
    # measurementDF <- cbind(examName=as.character(examName), measurementDF)
  }
  
  # populate the data frame with the measurements
  i=1
  for(i in 1:nrow(measurementsDF)) {
    thisChart <- measurementsDF[i,1]
    thisSensor <- measurementsDF[i,2]
    # iterate over the questions
    j=3 # column 3 is the first question
    for(j in 3:ncol(measurementsDF)) {
      thisQuestion <- names(measurementsDF)[j]
      # get the RqCQDFSeries row
      thisOne <- which(RqCqDFSeries$chartName==thisChart &
                         RqCqDFSeries$sensorName==thisSensor &
                         RqCqDFSeries$eventLabel==thisQuestion)
      if(length(thisOne) == 0 ) next()
      thisCol <- which(names(measurementsDF) == thisQuestion)
      # now get the measurement
      measurementsDF[i,thisCol] <- 
        round(RqCqDFSeries$sensorMeasurement[thisOne], decimals)
    }
  } # end i loop ofver measurementsDF rows
  # View(measurementsDF)
  
  # May 13, 2021
  measurementsDF <- cbind(seriesName=as.character(seriesName), measurementsDF)
  measurementsDF <- cbind(examName=as.character(examName), measurementsDF)
  
  
  # save the  measurement data frame to a data frame and csv 
  
  measurementTableName <- paste(examName, 
                                seriesName, 
                                "measurementTableDF", 
                                sep="_" )
  
  # if(!exists("makeDF")) makeDF <- TRUE
  
  if(isTRUE(makeDF)) {
    assign(measurementTableName, measurementsDF, pos=1)
  }
  
  # if(!exists("saveCSV")) saveCSV <- FALSE
  
  if(isTRUE(transpose)) {
    measurementsDF <- t(rbind(colnames(measurementsDF), measurementsDF))
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(measurementsDF,
              file=paste0(str_sub(measurementTableName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  return(measurementsDF)
  
} # end measurementTableFn()

############## Event Indices  table ##############

eventIndicesTableFn <- function(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=useSensors,
                               makeDF=TRUE,
                               saveCSV=FALSE ) {
  # make a data frame for all event indices in the series
  # all charts
  # all RQs
  # all sensors
  #
  # response indices  include
  # question onset
  # question offset
  # verbal answer
  # response onset
  # response End
  #
  # input is a data from of all RQs and CQs
  # output is a data frame of measurements 
  #
  # makeDF
  # saveCSV
  ####
  
  {
    ## reduce the input data frame to RQs and CQs ##
    RqCqSeriesRows <- grep("[CR]+", RqCqDFSeries$eventLabel)
    if(length(RqCqSeriesRows) != 0) {
      # exclude sacrifice relevant questions
      SRRows <- grep("SR", RqCqDFSeries$eventLabel)
      SRRows <- c(SRRows, grep("RS", RqCqDFSeries$eventLabel))
      # make a vector of working rows for CQs and RQs
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% SRRows)]
      # exclude "CT" (cleared throat) annotations
      CTRows <- grep("CT", RqCqDFSeries$eventLabel)
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% CTRows)]
    }    
    # keep all events except the first event if no RQ and CQ events
    if(length(RqCqSeriesRows) == 0) {
      # get all indices for events not in excludedEvents
      RqCqSeriesRows <- which(!(RqCqDFSeries$Label %in% excludeEvents))
      # get the event lables from the eventLabels column
      workingEvents <- RqCqDFSeries$eventLabel[RqCqSeriesRows]
      workingEvents <- unique(workingEvents)
      # replace the working events with the Labels column
      workingEvents <- 
        RqCqDFSeries$Label[RqCqDFSeries$eventLabel %in% 
                             workingEvents]
      # complete the vector of working rows
      RqCqSeriesRows <- which(RqCqDFSeries$Label %in% workingEvents)
      # remove the rows for the first event
      # RqCqSeriesRows <- RqCqDFSeries$eventLabel %in% 
      #   (unique(RqCqDFSeries$eventLabel[RqCqSeriesRows])[-1])
    }
    # return the measurementDF and exit if no stimulus events
    if(length(RqCqSeriesRows) == 0) {
      if(output == TRUE) {
        return("no events")
      } else return()
    }
    # initialize the RqCQDF for the series
    RqCqDFSeries <- RqCqDFSeries[RqCqSeriesRows,]
    # assign("RqCqSeriesRows", RqCqSeriesRows, pos=1)
  }
  
  ### initial set up ###
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    uniqueSensors <- unique(RqCqDFSeries$sensorName)
    
    if(!exists("decimals")) decimals <- 2
    
    if(!exists("useSensors")) useSensors <- c("UPneumo",
                                              "LPneumo",
                                              "AutoEDA",
                                              "Cardio",
                                              "PLE")
    
    useSensors <- useSensors[useSensors %in% uniqueSensors]
    
    if(!exists("makeDF")) makeDF <- TRUE
    
    if(!exists("saveCSV")) saveCSV <- TRUE
    
  }
  
  ### initialize a data frame for the measurements ###
  
  {
    measurementsDF <- 
      data.frame(matrix(ncol=(length(uniqueQuestions)), 
                        nrow=length(uniqueCharts)*length(useSensors)))
    names(measurementsDF) <- uniqueQuestions
    measurementsDF <- 
      cbind(sensorName=rep(useSensors, times=length(uniqueCharts)), 
            measurementsDF)
    measurementsDF$sensorName <- as.character(measurementsDF$sensorName)
    measurementsDF <- 
      cbind(chartName=rep(uniqueCharts, each=length(useSensors)), 
            measurementsDF)
    measurementsDF$chartName <- as.character(measurementsDF$chartName)
    # May 13, 20201
    # measurementDF <- cbind(seriesName=as.character(seriesName), measurementDF)
    # measurementDF <- cbind(examName=as.character(examName), measurementDF)
  }
  
  # populate the data frame with the measurements
  i=1
  # iterate over the charts
  for(i in 1:nrow(measurementsDF)) {
    thisChart <- measurementsDF[i,1]
    thisSensor <- measurementsDF[i,2]
    j=3 # colum 3 is the first question
    # iterate over the questions
    for(j in 3:ncol(measurementsDF)) {
      thisQuestion <- names(measurementsDF)[j]
      # get the RqCQDFSeries row
      thisOne <- which(RqCqDFSeries$chartName==thisChart &
                         RqCqDFSeries$sensorName==thisSensor &
                         RqCqDFSeries$eventLabel==thisQuestion)
      if(length(thisOne) == 0 ) next()
      thisCol <- which(names(measurementsDF) == thisQuestion)
      # now get the measurement
      measurementsDF[i,thisCol] <- 
        round(RqCqDFSeries$sensorMeasurement[thisOne], decimals)
    }
  } # end i loop ofver measurementsDF rows
  # View(measurementsDF)
  
  # May 13, 20201
  measurementsDF <- cbind(seriesName=as.character(seriesName), measurementsDF)
  measurementsDF <- cbind(examName=as.character(examName), measurementsDF)
  
  
  # save the  measurement data frame to a data frame and csv 
  
  measurementTableName <- paste(examName, 
                                seriesName, 
                                "measurementTableDF", 
                                sep="_" )
  
  # if(!exists("makeDF")) makeDF <- TRUE
  
  if(isTRUE(makeDF)) {
    assign(measurementTableName, measurementsDF, pos=1)
  }
  
  # if(!exists("saveCSV")) saveCSV <- FALSE
  
  if(isTRUE(transpose)) {
    measurementsDF <- t(rbind(colnames(measurementsDF), measurementsDF))
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(measurementsDF,
              file=paste0(str_sub(measurementTableName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  return(measurementsDF)
  
} # end measurementTableFn()

################ rank score table ##############

rankTableFn <- function(RqCqDFSeries=RqCqDFSeries, 
                        useRank="rank",
                        useSensors=useSensors,
                        makeDF=TRUE,
                        saveCSV=FALSE ) {
  # make a data frame for all rank values in the series
  # all charts
  # all RQs and CQs
  # all sensors
  #
  # input is a data from of all RQs and CQs
  #
  # useSensors is a vector of sensor names
  # useRank = c("rank", "ROSS") 
  # rank scores are largest response = 1
  # ROSS scores are smallest response = 1
  # makeDF
  # saveCSV
  
  # output is a data frame of measurements 
  ####
  
  examName <- RqCqDFSeries$examName[1]
  seriesName <- RqCqDFSeries$seriesName[1]
  
  uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
  uniqueCharts <- unique(RqCqDFSeries$chartName)
  uniqueSensors <- unique(RqCqDFSeries$sensorName)
  
  if(!exists("useSensors")) useSensors <- c("UPneumo",
                                            "LPneumo",
                                            "AutoEDA",
                                            "Cardio",
                                            "PLE")
  
  useSensors <- useSensors[useSensors %in% uniqueSensors]
  
  if(!exists("useRanks")) useRank <- "rank"
  
  if(!exists("makeDF")) makeDF <- TRUE
  
  if(!exists("saveCSV")) saveCSV <- TRUE
  
  # initialize a data frame for the measurements
  
  {
    measurementsDF <- 
      data.frame(matrix(ncol=(length(uniqueQuestions)), 
                        nrow=length(uniqueCharts)*length(useSensors)))
    names(measurementsDF) <- uniqueQuestions
    # add the sensor names
    measurementsDF <- 
      cbind(sensorName=rep(useSensors, times=length(uniqueCharts)), 
            measurementsDF)
    measurementsDF$sensorName <- as.character(measurementsDF$sensorName)
    # add the chart name
    measurementsDF <- 
      cbind(chartName=rep(uniqueCharts, each=length(useSensors)), 
            measurementsDF)
    measurementsDF$chartName <- as.character(measurementsDF$chartName)
  }
  
  # populate the data frame with the measurements
  i=1
  for(i in 1:nrow(measurementsDF)) {
    thisChart <- measurementsDF[i,1]
    thisSensor <- measurementsDF[i,2]
    # iterate over the questions
    j=3 # colum 3 is the first question
    for(j in 3:ncol(measurementsDF)) {
      thisQuestion <- names(measurementsDF)[j]
      # get the RqCQDFSeries row
      thisOne <- which(RqCqDFSeries$chartName==thisChart &
                         RqCqDFSeries$sensorName==thisSensor &
                         RqCqDFSeries$eventLabel==thisQuestion)
      if(length(thisOne) == 0 ) next()
      thisCol <- which(names(measurementsDF) == thisQuestion)
      # now get the rank score
      measurementsDF[i,thisCol] <- 
        ifelse(useRank=="ROSS",
               round(as.numeric(RqCqDFSeries$ROSSScore[thisOne]), 2),
               round(as.numeric(RqCqDFSeries$rankScore[thisOne]), 2) )
    }
  }
  # View(measurementsDF)
  
  # save the  measurement data frame to a data frame and csv 
  
  rankTableName <- paste(examName, 
                         seriesName, 
                         "rankScoresDF", 
                         sep="_" )
  
  if(!exists("makeDF")) makeDF <- TRUE
  
  if(isTRUE(makeDF)) {
    assign(rankTableName, measurementsDF, pos=1)
  }
  
  if(!exists("saveCSV")) saveCSV <- FALSE
    
  if(isTRUE(saveCSV)) {
    write.csv(measurementsDF,
              file=paste0(str_sub(rankTableName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  return(measurementsDF)
  
} # end rankTableFn()


################ score sheet ################

scoreSheetFn <- function(RqCqDFSeries=RqCqDFSeries, 
                         useSensors=useSensors,
                         scoreType="OSS3Score",
                         decimals=2,
                         DLSTType=FALSE,
                         outputName="OSS3ScoresheetDF",
                         makeDF=FALSE,
                         saveCSV=FALSE ) {
  # make a data frame for scores
  # all charts
  # all sensors
  # called by the ESSScoresFn, OSS3ScoresFn and RCScoresFn
  #
  # input is a data from of all RQs and CQs
  # RqCqDFSeries is the measurementDF for all charts in a series
  # useSensors is a vector of sensor names
  # scoreType is a scalar with the name of the column to obtain the scores
  # decimals use used to set the rounding level
  # use decimals="non-numeric" for text
  # outputName is a scalar with the name to be appended to the output data
  # makeDF will put a data frame in the global envir
  # saveCSV will save a .csv file to the current working directory
  #
  # output is a data frame with the score sheet info
  #
  # for a table of CQs selected for each RQ
  # use the scoreSheetFn with scoreType="CQName" parameter
  # 
  ####
  
  {
    
    if(!exists("decimals")) decimals <- 2
    
    # View(RqCqDFSeries)
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    if(!exists("useSensors")) useSensors <- c("UPneumo", 
                                              "LPneumo",
                                              "AutoEDA", 
                                              "Cardio", 
                                              "PLE")
    if(!exists("scoreType")) scoreType <- "RCScore"
    if(!exists("outputName")) outputName <- "RCScoreSheetDF"
    
  }
  
  {
  
    # exclude charts without 2 CQs and 2 RQs
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    keepCharts <- NULL
    i=2
    for(i in 1:length(uniqueCharts)) {
      thisChartDF <- RqCqDFSeries[RqCqDFSeries$chartName==uniqueCharts[i],]
      rqRows <- grep("R", thisChartDF$eventLabel)
      uniqueRQs <- unique(thisChartDF[rqRows,'Label'])
      cqRows <- grep("C", thisChartDF$eventLabel)
      uniqueCQs <- unique(thisChartDF[cqRows,'Label'])
      if(length(uniqueRQs) >= 2 && length(uniqueCQs) >= 1) { 
      # TES/DLST charts will appear to have 1 CQ for charts 2 and 3
        keepCharts <- c(keepCharts, uniqueCharts[i]) 
      }
    }
    RqCqDFSeries <- RqCqDFSeries[RqCqDFSeries$chartName %in% keepCharts,]
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    numberUniqueCharts <- length(uniqueCharts)
    
    # View(RqCqDFSeries)
  
  }
  
  {
  
    # get the unique RQ names
    
    # use eventLabel column instead of Label because of repeated questions
    # DLST/DLDT formats must use eventLabel because it has original labels
    rqRows <- grep("R", RqCqDFSeries$eventLabel)
    rqRows <- grep("R", RqCqDFSeries$Label)
    
    uniqueRQs <- unique(RqCqDFSeries$eventLabel[rqRows])
    uniqueRQsE <- unique(RqCqDFSeries$eventLabel[rqRows])
    
  }
  
  if(isTRUE(DLSTType)) {
    
    numberUniqueCharts <- 
      length(unique(str_sub(uniqueRQsE, 1, 1)))
    
    # 2020-07-25
    uniqueCharts <- 
      paste0("0", unique(str_sub(RqCqDFSeries$eventLabel, 1, 1)), "A")
    
    saveChartNames <- RqCqDFSeries$chartName
    
    RqCqDFSeries$chartName <- 
      paste0("0", str_sub(RqCqDFSeries$eventLabel, 1, 1), "A")
    
    # get the RQs using the Label column
    uniqueRQs <- unique(RqCqDFSeries$Label[rqRows])
    
  }
  
  {
    
    # initialize a score sheet data frame
    
    scoreSheetDF <- data.frame(matrix(ncol=(length(uniqueRQs)),
                                      nrow=numberUniqueCharts *
                                        length(useSensors) ) )
    names(scoreSheetDF) <- uniqueRQs
    scoreSheetDF <- cbind(examName,
                          seriesName,
                          chartName=rep(uniqueCharts, each=length(useSensors)),
                          sensorName=rep(useSensors, times=numberUniqueCharts),
                          scoreSheetDF,
                          stringsAsFactors=FALSE)
    # View(scoreSheetDF)
    # View(RqCqDFSeries)
    
  }
  
  # iterate over the RQs and populate the score sheet with the scoreType
  # ensures that RQs are correctly aligned for rotated charts
  i=2
  for(i in 1:length(uniqueRQs)) {
    thisRQ <- uniqueRQs[i]
    if(!isTRUE(DLSTType)) {
      # for common formats with multiple charts
      RQRows <- (RqCqDFSeries$eventLabel == thisRQ)
    } else {
      # DLST/DLDT/PCASS format, remove the first char from eventLabel
      RQRows <- 
        (str_sub(RqCqDFSeries$eventLabel, 2, -1) == thisRQ)
    }
    theseSensorRows <- (RqCqDFSeries$sensorName %in% useSensors)
    # get the indices in RqCqDFSeries for this sensor and this RQ
    theseRows <- which(RQRows & theseSensorRows)
    # need to identify which charts have which RQs
    # in case of repeated or inserted questions
    RQCharts <- unique(RqCqDFSeries$chartName[RQRows])
    scoreSheetRows <- which(scoreSheetDF$chartName %in% RQCharts)
    # obtain and round the score sheet values
    ifelse(decimals=="non-numeric",
           scoreSheetVals <- 
             RqCqDFSeries[theseRows,scoreType],
           scoreSheetVals <- 
             round(as.numeric(RqCqDFSeries[theseRows,scoreType]), decimals) )
    # 20200722 replace missing values with NA to avoid blank cells
    scoreSheetVals[which(scoreSheetVals == "")] <- "NA"
    # submit the values to the score sheet
    scoreSheetDF[scoreSheetRows,(i+4)] <- scoreSheetVals
  }
  
  # fix the chart names for DLST/DLDT formats
  # these need to be corrected so scores can be aggregated per chart
  if(isTRUE(DLSTType)) {
    uniqueCharts <- paste0("0", unique(str_sub(uniqueRQsE, 1, 1)), "A")
    chartNameVc <- rep(uniqueCharts, each=length(useSensors))
    scoreSheetDF$chartName <- chartNameVc
  }
  
  
  # fix the columns data types
  # scoreSheetDF$examName <- as.character(scoreSheetDF$examName)
  # scoreSheetDF$seriesName <- as.character(scoreSheetDF$seriesName)
  # scoreSheetDF$chartName <- as.character(scoreSheetDF$chartName)
  # scoreSheetDF$sensorName <- as.character(scoreSheetDF$sensorName)
  
  if(scoreType !="CQName") {
    # use a loop
    i=5
    for(i in 5:ncol(scoreSheetDF)) {
      thisCol <- names(scoreSheetDF)[i]
      scoreSheetDF[,thisCol] <- as.numeric(scoreSheetDF[,thisCol])
    }
  }
  # View(scoreSheetDF)
  
  # initialize the name for the ouput  score sheet
  scoreSheetName <- paste(examName, seriesName, outputName, sep="_")
  
  if(isTRUE(makeDF)) {
    assign(scoreSheetName, scoreSheetDF, pos=1)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(scoreSheetDF,
              file=paste0(str_sub(scoreSheetName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  return(scoreSheetDF)
  
} # scoreSheetFn()

############### series subtotals ###############

seriesTotalsFn <- function(scoreSheetDF=scoreSheetDF,
                           outputName="OSS3SseriesTotalsDF",
                           aggType="mean",
                           weightingCoefs=NULL,
                           aggMethod="within",
                           missingVals=NULL,
                           NAVals=NULL,
                           makeDF=FALSE,
                           saveCSV=FALSE) {
  # series totals are the RQ subtotals for all charts
  # input aggType is "mean" or "sum" 
  # input aggMethod is "within" or "between"
  # aggMethod makes no difference when aggType == "sum
  # PA uses between chart sensor aggregation
  # OSS-3 uses within chart sensor aggregation 
  # "between" or "within" makes no difference with summation
  ###
  
  {
    
    examName <- scoreSheetDF$examName[1]
    seriesName <- scoreSheetDF$seriesName[1]
    
    # View(scoreSheetDF)
    # assign("scoreSheetDF", scoreSheetDF, envir=.GlobalEnv )
    
    uniqueCharts <- unique(scoreSheetDF$chartName)
    
    uniqueQs <- unique(names(scoreSheetDF)[5:ncol(scoreSheetDF)])
    
    uniqueSensors <- unique(scoreSheetDF$sensorName)
    
    # weightingCoefs <- OSS3WeightingCoefs
    
    # check if the length of the weighting coefs == number of sensors
    if(length(weightingCoefs) != length(uniqueSensors)) {
      weightingCoefs <- NULL
    }
    
    if(!exists("weightingCoefs") || is.null(weightingCoefs)) {
      weightingCoefs <- rep(1, length=length(uniqueSensors))
    }
    
  }
  
  # commented out to  match the Excel result 
  # {
  #   ## fix missing and NA values ##
  #   scoreMtx <- as.matrix(scoreSheetDF[,5:ncol(scoreSheetDF)])
  #   scoreMtx[which(is.na(scoreMtx))] <- 0
  #   scoreSheetDF[,5:ncol(scoreSheetDF)] <- scoreMtx
  # }
  
  {
    ## initialize the series totals output data frame ##
    seriesTotalsDF <- data.frame(matrix(ncol=(length(uniqueQs)), 
                                        nrow=1 ) )
    names(seriesTotalsDF) <- uniqueQs
    seriesTotalsDF <- cbind(examName=examName,
                            seriesName=seriesName,
                            seriesTotalsDF )
  }
  
  # aggregate the scoreSheetDF ##
  
  if(aggType=="mean") {
    
    if(aggMethod=="within") {
      
      # within chart aggregation of sensor values
      
      # inititalize a matrix to hold the result from the aggregation
      chartMeans <- 
        matrix(ncol=(length(uniqueQs)), nrow=length(uniqueCharts))
      colnames(chartMeans) <- uniqueQs
      rownames(chartMeans) <- uniqueCharts
      
      # iterate over the charts
      i=3
      for(i in 1:length(uniqueCharts)) {
        thisChart <- uniqueCharts[i]
        chartRow <- which(rownames(chartMeans) == thisChart)
        # iterate over the questions
        j=1
        for(j in 1:length(uniqueQs)) {
          thisQuestion <- uniqueQs[j]
          # get the question column from the input scoreSheetDF
          scoreSheetCol <- which(names(scoreSheetDF) == thisQuestion)
          # now get the chart sensor values from the score sheet
          sensorVals <- 
            scoreSheetDF[scoreSheetDF$chartName==thisChart,scoreSheetCol]
          # calculate the weighted mean
          # use the  weighted.mean function to match the Excel result
          # when missing or NA values
          WMean <- weighted.mean(sensorVals, weightingCoefs, na.rm=TRUE)
          # pass the weighted mean to the matrix of chart means
          chartMeans[chartRow,which(colnames(chartMeans)==thisQuestion)] <- 
            round(WMean, 3)
        } # end j
      } # end i
      # then calculate the unweighted between chart means
      seriesTotalsDF[1,3:ncol(seriesTotalsDF)] <- colMeans(chartMeans, na.rm=TRUE)
      
    } else { # if aggMethod is not "within"
      
      # between-chart aggregation of sensors
      
      sensorMeans <- as.data.frame(
        matrix(ncol=(length(uniqueQs)), nrow=length(uniqueSensors)) )
      colnames(sensorMeans) <- uniqueQs
      rownames(sensorMeans) <- uniqueSensors
      # iterate over the sensors
      i=1
      for(i in 1:length(uniqueSensors)) {
        thisSensor <- uniqueSensors[i]
        # get the sensor rows in the score sheet
        sensorRows <- scoreSheetDF$sensorName == thisSensor
        # get the sensor values 
        sensorMeans[i,1:ncol(sensorMeans)] <- 
          scoreSheetDF[sensorRows,5:ncol(scoreSheetDF)]
      } # end i
      # initialize a vector for the weighted means
      WMeans <- rep(NA, length(uniqueQs))
      # iterate over the question columns to calculate the weighted means
      j=1
      for(j in 1:length(uniqueQs)) {
        WMeans[j] <- 
          sum(sensorMeans[,j] * weightingCoefs, na.rm=TRUE) / 
          sum(weightingCoefs, na.rm=TRUE)
      }
      # submit the WMean vector to the seriesTotalsDF
      seriesTotalsDF[1,3:ncol(seriesTotalsDF)] <- WMeans
    }
  } else { # if the aggType is not "mean"
    
    # summation instead of means
    
    seriesTotalsDF[1,c(3:ncol(seriesTotalsDF))] <-
      colSums(scoreSheetDF[,c(5:ncol(scoreSheetDF))], na.rm=TRUE)
  }

  # aggregate the values for all questions   
  # if(aggType=="mean") {
  #   seriesTotalsDF$grandMean <- rowMeans(seriesTotalsDF[,c(3:ncol(seriesTotalsDF))],
  #                                        na.rm=TRUE)
  # } else {
  #   seriesTotalsDF$grandTotal <- rowSums(seriesTotalsDF[,c(3:ncol(seriesTotalsDF))],
  #                                        na.rm=TRUE)
  # }
  
  ## output ##
  
  seriesTotalsDFName <- paste(examName,
                              seriesName,
                              outputName,
                              sep="_")
  
  if(isTRUE(makeDF)) {
    assign(seriesTotalsDFName, seriesTotalsDF, env=.GlobalEnv)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(seriesTotalsDF,
              file=paste0(str_sub(seriesTotalsDFName, 1, -3), ".csv"),
              row.names=FALSE )
  }
  
  return(seriesTotalsDF)
  
} # seriesTotalsFn()


######## chart totals, including RQ subtotals and chart subtotals ########

chartTotalsFn <- function(scoreSheetDF=scoreSheetDF,
                          outputName="OSS3SchartTotalsDF",
                          aggType="mean",
                          weightingCoefs=NULL,
                          minSensorScores=1,
                          makeDF=FALSE,
                          saveCSV=FALSE) {
  # calculate the RQ subtotals for each chart
  ####
  
  {
    
    examName <- scoreSheetDF$examName[1]
    
    seriesName <- scoreSheetDF$seriesName[1]
    
    uniqueCharts <- unique(scoreSheetDF$chartName)
    
    uniqueQs <- unique(names(scoreSheetDF)[5:ncol(scoreSheetDF)])
    
    uniqueSensors <- unique(scoreSheetDF$sensorName)
    
    # weightingCoefs <- OSS3WeightingCoefs
    
    if(length(weightingCoefs) != length(uniqueSensors)) {
      weightingCoefs <- NULL
    }
    
    if(!exists("weightingCoefs") || is.null(weightingCoefs)) {
      weightingCoefs <- rep(1, length=length(unique(scoreSheetDF$sensorName)))
    }
    
  }
  
  {
    
    # initialize the output data frame
    
    chartTotalsDF <- data.frame(matrix(ncol=(length(uniqueQs)),
                                       nrow=length(uniqueCharts) ) )
    names(chartTotalsDF) <- uniqueQs
    chartTotalsDF <- cbind(examName=examName,
                           seriesName=seriesName,
                           chartName=uniqueCharts,
                         chartTotalsDF)
    # View(chartTotalsDF)
  
  }
  
  # private function to check the number of sensor scores within chart
  # x=chartMtx[,1]
  checkScoresFn <- function(x, minSensorScores=minSensorScores) {
    outVals <- rep(NA, times=length(x))
    outVals[which(x != "" & !is.na(x))] <- x[which(x != "" & !is.na(x))]
  }
  
  # iterate over the charts
  # uses the scoreSheetDF and chartTotalsDF
  i=1
  for(i in 1:length(uniqueCharts)) {
    thisChart <- uniqueCharts[i]
    thisRow <- which(chartTotalsDF$chartName == thisChart)
    theseCols <- c(4:ncol(chartTotalsDF))
    if(aggType == "mean") {
      
      j=1
      for(j in 1:length(theseCols)) {
        
        thisScoreSheetCol <- 
          which( names(scoreSheetDF) == names(chartTotalsDF)[theseCols][j] )
        
        theseScoreSheetRows <- which((scoreSheetDF$chartName==thisChart))
        
        sensorScores <- scoreSheetDF[theseScoreSheetRows,thisScoreSheetCol]
        
        # check for min number of sensor scores
        if( length(which(sensorScores != "" & !is.na(sensorScores))) < 
            minSensorScores) {
          chartTotalsDF[thisRow,theseCols[j]] <- ""
          next()
        }
        
        chartTotalsDF[thisRow,theseCols[j]] <- 
          weighted.mean(
            x=scoreSheetDF[theseScoreSheetRows,thisScoreSheetCol],
            w=weightingCoefs,
            na.rm=TRUE ) 
      } # end j loop for RQ columns
      
    } else {
      
      # aggType != "mean 
      # aggregate by summation instead
      
      scoreSheetDF[(scoreSheetDF$chartName==thisChart),
                   (5:ncol(scoreSheetDF))]
      
      chartMtx <- as.matrix(scoreSheetDF[(scoreSheetDF$chartName==thisChart),
                                            (5:ncol(scoreSheetDF))])
      
      # check for min question scores
      apply(chartMtx, 2, checkScoresFn)
      
      chartScores <- colSums(chartMtx, na.rm=TRUE)
      
      chartTotalsDF[thisRow,theseCols] <- chartScores

    }
  }
  
  ## write the subtotals to a data frame and .csv ##
  
  chartTotalsDFName <- paste(examName,
                             seriesName,
                             outputName,
                             sep="_")
  
  if(!exists("makeDF")) makeDF <- FALSE
  if(isTRUE(makeDF)) {
    assign(chartTotalsDFName, chartTotalsDF, env=.GlobalEnv)
  }
  
  if(!exists("saveCSV")) saveCSV <- FALSE
  if(isTRUE(saveCSV)) {
    write.csv(chartTotalsDF,
              file=paste0(str_sub(chartTotalsDFName, 1, -3), ".csv"),
              row.names=FALSE )
  }
  
  ## output ##
  
  return(chartTotalsDF)
  
} # end chartTotalsFn()

######## sensor subtotal scores for the series ########

sensorSubtotalsFn <- function(scoreSheetDF=scoreSheetDF,
                              outputName="OSS3SsensorTotalsDF",
                              aggType="mean",
                              makeDF=TRUE,
                              saveCSV=FALSE) {
  # aggregate the sensor scores between charts
  
  {
    
    examName <- scoreSheetDF$examName[1]
    seriesName <- scoreSheetDF$seriesName[1]
    
    uniqueSensors <- unique(scoreSheetDF$sensorName)
    
    uniqueCharts <- unique(scoreSheetDF$chartName)
    
  }
  
  {
    
    # initialize the output data frame
    
    sensorTotalsDF <- data.frame(matrix(ncol=(length(uniqueSensors)), 
                                        nrow=length(uniqueCharts) ) )
    names(sensorTotalsDF) <- uniqueSensors
    sensorTotalsDF <- cbind(examName=examName,
                            seriesName=seriesName,
                            chartName=uniqueCharts,
                            sensorTotalsDF)
    
    sensorTotalsDF$examName <- as.character(sensorTotalsDF$examName)
    sensorTotalsDF$seriesName <- as.character(sensorTotalsDF$seriesName)
    sensorTotalsDF$chartName <- as.character(sensorTotalsDF$chartName)
    
    # str(sensorTotalsDF)
    
  }
  
  # iterate over the charts and uniqueSensors vectors
  # uses the scoreSheetDF
  i=1
  for(i in 1:length(uniqueCharts)) {
    thisChart <- uniqueCharts[i]
    theseChartRows <- scoreSheetDF$chartName == thisChart
    # iterate over the sensors
    j=1
    for(j in 1:length(uniqueSensors)) {
      thisSensor <- uniqueSensors[j]
      theseSensorRows <- scoreSheetDF$sensorName == thisSensor
      thisRow <- which(theseChartRows & theseSensorRows)
      if(aggType=="mean") {
        thisSensorMean <- 
          mean(as.numeric(scoreSheetDF[thisRow,c(5:ncol(scoreSheetDF))]), 
               na.rm=TRUE )
      } else {
        thisSensorMean <- 
          sum(as.numeric(scoreSheetDF[thisRow,c(5:ncol(scoreSheetDF))]), 
               na.rm=TRUE )
      }
      sensorTotalsDF[sensorTotalsDF$chartName==thisChart,
                     names(sensorTotalsDF)==thisSensor] <- thisSensorMean
    }
  }
  # View(sensorTotalsDF)
  
  # write the subtotals to a data frame and .csv
  
  sensorTotalsDFName <- paste(examName,
                              seriesName,
                              outputName,
                              sep="_")
  
  if(isTRUE(makeDF)) {
    assign(sensorTotalsDFName, sensorTotalsDF, env=.GlobalEnv)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(sensorTotalsDF,
              file=paste0(str_sub(sensorTotalsDFName, 1, -3), ".csv"),
              row.names=FALSE )
  }
  
  return(sensorTotalsDF)

} # end sensorSubtotalsFn()

######## sensor means for the series ########

sensorMeansFn <- function(scoreSheetDF=scoreSheetDF,
                          outputName="OSS3SsensorMeansDF",
                          makeDF=TRUE,
                          saveCSV=FALSE) {
  # aggregate the sensor scores between charts
  # for discriminate analysis or logistic regression
  
  {
    
    examName <- scoreSheetDF$examName[1]
    seriesName <- scoreSheetDF$seriesName[1]
    
    uniqueSensors <- unique(scoreSheetDF$sensorName)
    
    uniqueCharts <- unique(scoreSheetDF$chartName)
    
    uniqueRQs <- names(scoreSheetDF[5:ncol(scoreSheetDF)])
    
  }
  
  {
    
    # initialize the output data frame
    
    sensorMeansDF <- data.frame(matrix(ncol=(length(uniqueRQs)), 
                                       nrow=length(uniqueSensors) ) )
    names(sensorMeansDF) <- uniqueRQs
    sensorMeansDF <- cbind(examName=examName,
                           seriesName=seriesName,
                           sensorName=uniqueSensors,
                           sensorMeansDF)
    
    sensorMeansDF$examName <- as.character(sensorMeansDF$examName)
    sensorMeansDF$seriesName <- as.character(sensorMeansDF$seriesName)
    sensorMeansDF$sensorName <- as.character(sensorMeansDF$sensorName)
    
    # str(sensorMeansDF)
    
  }
  
  # iterate over the charts and uniqueSensors vectors
  # uses the scoreSheetDF
  i=1
  for(i in 1:length(uniqueRQs)) {
    thisRQ <- uniqueRQs[i]
    thisRQCol <- which(names(scoreSheetDF) == thisRQ)
    # iterate over the sensors
    j=1
    for(j in 1:length(uniqueSensors)) {
      thisSensor <- uniqueSensors[j]
      theseSensorRows <- which(scoreSheetDF$sensorName == thisSensor)
      thisSensorMean <- 
        mean(as.numeric(scoreSheetDF[theseSensorRows,thisRQCol]), 
             na.rm=TRUE )
      sensorMeansDF[j,(i+3)] <- thisSensorMean
      
    } # end j loop over sensors
  } # end i loop over RQs
  # View(sensorMeansDF)
  
  # standardize the means because they are standardized logged values
  sensorMeansDF[,4:ncol(sensorMeansDF)] <- 
    pnorm(as.matrix(sensorMeansDF[,4:ncol(sensorMeansDF)]))
  
  ## write the subtotals to a data frame and .csv ##
  
  # construct the data frame name
  sensorMeansDFName <- paste(examName,
                              seriesName,
                              outputName,
                              sep="_")
  
  if(isTRUE(makeDF)) {
    assign(sensorMeansDFName, sensorMeansDF, env=.GlobalEnv)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(sensorMeansDF,
              file=paste0(str_sub(sensorMeansDFName, 1, -3), ".csv"),
              row.names=FALSE )
  }
  
  return(sensorMeansDF)
  
} # end sensorMeansFn()

######## question sequence data frame ########

questionSequenceFn <- function(measurementDF=RqCqDFSeries,
                               outputName="questionSequenceDF",
                               makeDF=FALSE,
                               saveCSV=FALSE) {
  # construct a table of question sequences for all charts in a series
  ####
  
  {
    examName <- measurementDF$examName[1]
    seriesName <- measurementDF$seriesName[1]
    uniqueCharts <- unique(measurementDF$chartName)
    uniqueQs <- unique(measurementDF$eventLabel)
    uniqueSensors <- unique(measurementDF$sensorName)
  }
  
  {
    # initialize the output data frame
    questionSequenceDF <- data.frame(matrix(ncol=(length(uniqueQs)),
                                            nrow=length(uniqueCharts) ) )
    names(questionSequenceDF) <- paste0("event",1:length(uniqueQs))
    questionSequenceDF <- cbind(examName=examName,
                                seriesName=seriesName,
                                chartName=uniqueCharts,
                                questionSequenceDF)
    
    questionSequenceDF$examName <- 
      as.character(questionSequenceDF$examName)
    questionSequenceDF$seriesName <- 
      as.character(questionSequenceDF$seriesName)
    questionSequenceDF$chartName <- 
      as.character(questionSequenceDF$chartName)
      
    # View(questionSequenceDF)
  }
  
  # iterate over the charts
  i=1
  for(i in 1:length(uniqueCharts)) {
    thisChart <- uniqueCharts[i]
    thisRow <- which(questionSequenceDF$chartName == thisChart)
    theseCols <- c(4:ncol(questionSequenceDF))
    measurementDFChart <- measurementDF[measurementDF$chartName == uniqueCharts[i],]
    # get the questions from the measurementDFChart
    chartQs <- unique(measurementDFChart$eventLabel)
    questionSequenceDF[thisRow,c(4:(length(chartQs)+3))] <- chartQs
  }
  
  ## write the questions to a data frame and .csv ##
  
  {
    
    questionSequenceDFName <- paste(examName,
                                    seriesName,
                                    outputName,
                                    sep="_")
    
    if(!exists("makeDF")) makeDF <- FALSE
    if(isTRUE(makeDF)) {
      assign(questionSequenceDFName, questionSequenceDF, env=.GlobalEnv)
    }
    
    if(!exists("saveCSV")) saveCSV <- FALSE
    if(isTRUE(saveCSV)) {
      write.csv(questionSequenceDF,
                file=paste0(str_sub(questionSequenceDFName, 1, -1), ".csv"),
                row.names=FALSE )
    }
    
  }
  
  ## output ##
  return(questionSequenceDF)
  
} # end questionSequenceFn()


######## artifact table ########

artifactTableFn <- function(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=useSensors,
                               decimals=2,
                               makeDF=TRUE,
                               saveCSV=FALSE,
                               transpose=FALSE ) {
  # R function to aggregate data facts to a table for all charts and all sensors in a series
  # Oct 14, 2024
  # Raymond Nelson
  # make a data frame for all measurements in the series
  # all charts
  # all RQs
  # all sensors
  #
  # input is a data from of all RQs and CQs
  # output is a data frame of measurements 
  #
  # makeDF
  # saveCSV
  ####
  
  {
    ## reduce the input data frame to RQs and CQs ##
    RqCqSeriesRows <- grep("[CR]+", RqCqDFSeries$eventLabel)
    if(length(RqCqSeriesRows) != 0) {
      # exclude sacrifice relevant questions
      SRRows <- grep("SR", RqCqDFSeries$eventLabel)
      SRRows <- c(SRRows, grep("RS", RqCqDFSeries$eventLabel))
      # make a vector of working rows for CQs and RQs
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% SRRows)]
      # exclude "CT" (cleared throat) annotations
      CTRows <- grep("CT", RqCqDFSeries$eventLabel)
      RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% CTRows)]
    }    
    # keep all events except the first event if no RQ and CQ events
    if(length(RqCqSeriesRows) == 0) {
      # get all indices for events not in excludedEvents
      RqCqSeriesRows <- which(!(RqCqDFSeries$Label %in% excludeEvents))
      # get the event lables from the eventLabels column
      workingEvents <- RqCqDFSeries$eventLabel[RqCqSeriesRows]
      workingEvents <- unique(workingEvents)
      # replace the working events with the Labels column
      workingEvents <- 
        RqCqDFSeries$Label[RqCqDFSeries$eventLabel %in% 
                             workingEvents]
      # complete the vector of working rows
      RqCqSeriesRows <- which(RqCqDFSeries$Label %in% workingEvents)
      # remove the rows for the first event
      # RqCqSeriesRows <- RqCqDFSeries$eventLabel %in% 
      #   (unique(RqCqDFSeries$eventLabel[RqCqSeriesRows])[-1])
    }
    # return the measurementDF and exit if no stimulus events
    if(length(RqCqSeriesRows) == 0) {
      if(output == TRUE) {
        return("no events")
      } else return()
    }
    # initialize the RqCQDF for the series
    RqCqDFSeries <- RqCqDFSeries[RqCqSeriesRows,]
    # assign("RqCqSeriesRows", RqCqSeriesRows, pos=1)
  }
  
  ### initial set up ###
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    uniqueSensors <- unique(RqCqDFSeries$sensorName)
    
    if(!exists("decimals")) decimals <- 2
    
    if(!exists("useSensors")) useSensors <- c("UPneumo",
                                              "LPneumo",
                                              "AutoEDA",
                                              "Cardio",
                                              "PLE")
    
    useSensors <- useSensors[useSensors %in% uniqueSensors]
    
    if(!exists("makeDF")) makeDF <- TRUE
    
    if(!exists("saveCSV")) saveCSV <- TRUE
    
  }
  
  ### initialize a data frame for the measurements ###
  
  {
    measurementsDF <- 
      data.frame(matrix(ncol=(length(uniqueQuestions)), 
                        nrow=length(uniqueCharts)*length(useSensors)))
    names(measurementsDF) <- uniqueQuestions
    measurementsDF <- 
      cbind(sensorName=rep(useSensors, times=length(uniqueCharts)), 
            measurementsDF)
    measurementsDF$sensorName <- as.character(measurementsDF$sensorName)
    measurementsDF <- 
      cbind(chartName=rep(uniqueCharts, each=length(useSensors)), 
            measurementsDF)
    measurementsDF$chartName <- as.character(measurementsDF$chartName)
    # May 13, 20201
    # measurementDF <- cbind(seriesName=as.character(seriesName), measurementDF)
    # measurementDF <- cbind(examName=as.character(examName), measurementDF)
  }
  
  # populate the data frame with the measurements
  i=1
  for(i in 1:nrow(measurementsDF)) {
    thisChart <- measurementsDF[i,1]
    thisSensor <- measurementsDF[i,2]
    # iterate over the questions
    j=3 # colum 3 is the first question
    for(j in 3:ncol(measurementsDF)) {
      thisQuestion <- names(measurementsDF)[j]
      # get the RqCQDFSeries row
      thisOne <- which(RqCqDFSeries$chartName==thisChart &
                         RqCqDFSeries$sensorName==thisSensor &
                         RqCqDFSeries$eventLabel==thisQuestion)
      if(length(thisOne) == 0 ) next()
      thisCol <- which(names(measurementsDF) == thisQuestion)
      # now get the measurement
      measurementsDF[i,thisCol] <- 
        round(RqCqDFSeries$sensorMeasurement[thisOne], decimals)
    }
  } # end i loop ofver measurementsDF rows
  # View(measurementsDF)
  
  # May 13, 20201
  measurementsDF <- cbind(seriesName=as.character(seriesName), measurementsDF)
  measurementsDF <- cbind(examName=as.character(examName), measurementsDF)
  
  
  # save the  measurement data frame to a data frame and csv 
  
  measurementTableName <- paste(examName, 
                                seriesName, 
                                "measurementTableDF", 
                                sep="_" )
  
  # if(!exists("makeDF")) makeDF <- TRUE
  
  if(isTRUE(makeDF)) {
    assign(measurementTableName, measurementsDF, pos=1)
  }
  
  # if(!exists("saveCSV")) saveCSV <- FALSE
  
  if(isTRUE(transpose)) {
    measurementsDF <- t(rbind(colnames(measurementsDF), measurementsDF))
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(measurementsDF,
              file=paste0(str_sub(measurementTableName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  return(measurementsDF)
  
} # end measurementTableFn()
