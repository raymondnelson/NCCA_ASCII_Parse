

############################### output ###############################

############## OSS-3 measurement table ##############

{
  
  # initialize a data frame for the OSS-3 measurements
  OSS3MeasurementsDF <- 
    data.frame(matrix(ncol=(length(uniqueQuestions)), 
                      nrow=length(uniqueCharts)*length(OSS3Sensors2)))
  names(OSS3MeasurementsDF) <- uniqueQuestions
  OSS3MeasurementsDF <- 
    cbind(sensorName=rep(OSS3Sensors2, times=length(uniqueCharts)), 
          OSS3MeasurementsDF)
  OSS3MeasurementsDF$sensorName <- as.character(OSS3MeasurementsDF$sensorName)
  OSS3MeasurementsDF <- 
    cbind(chartName=rep(uniqueCharts, each=length(OSS3Sensors2)), 
          OSS3MeasurementsDF)
  OSS3MeasurementsDF$chartName <- as.character(OSS3MeasurementsDF$chartName)
  
  # populate the data frame with the measurements
  i=1
  for(i in 1:nrow(OSS3MeasurementsDF)) {
    thisChart <- OSS3MeasurementsDF[i,1]
    thisSensor <- OSS3MeasurementsDF[i,2]
    # iterate over the questions
    j=3
    for(j in 3:ncol(OSS3MeasurementsDF)) {
      thisQuestion <- names(OSS3MeasurementsDF)[j]
      # now get the measurement
      thisOne <- which(RqCqDFSeries$chartName==thisChart &
                         RqCqDFSeries$sensorName==thisSensor &
                         RqCqDFSeries$eventLabel==thisQuestion)
      if(length(thisOne) == 0 ) next()
      thisCol <- which(names(OSS3MeasurementsDF) == thisQuestion)
      OSS3MeasurementsDF[i,thisCol] <- RqCqDFSeries$sensorMeasurement[thisOne]
    }
  }
  # View(OSS3MeasurementsDF)
  
  # save the OSS-3 measurements to a data frame and csv 
  
  measurementTableName <- paste(examName, 
                                seriesName, 
                                "OSS3MeasurementsDF", 
                                sep="_")
  
  if(!exists("makeScoreSheetDF")) makeScoreSheetDF <- TRUE
  
  if(isTRUE(makeScoreSheetDF)) {
    assign(measurementTableName, OSS3MeasurementsDF, pos=1)
  }
  
  if(!exists("writeScoreSheetCSV")) writeScoreSheetCSV <- FALSE
  
  if(isTRUE(writeScoreSheetCSV)) {
    write.csv(OSS3MeasurementsDF,
              file=paste0(str_sub(measurementTableName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
}

######## OSS-3 score sheet standardized logged ratios ########

{
  
  scoreSheetDF <- data.frame(matrix(ncol=(length(uniqueRQs)),
                                    nrow=length(uniqueCharts) *
                                      length(OSS3Sensors2) ) )
  names(scoreSheetDF) <- uniqueRQs
  scoreSheetDF <- cbind(examName=examName,
                        seriesName=seriesName,
                        chartName=rep(uniqueCharts, each=length(OSS3Sensors2)),
                        sensorName=rep(OSS3Sensors2, times=length(uniqueCharts)),
                        scoreSheetDF)
  
  # iterate over the RQs
  # ensures that RQs are correctly aligned for rotated charts
  i=1
  for(i in 1:length(uniqueRQs)) {
    thisRQ <- uniqueRQs[i]
    RQRows <- RqCqDFSeries$eventLabel == thisRQ
    OSS3SensorRows <- RqCqDFSeries$sensorName %in% OSS3Sensors2
    theseRows <- which(RQRows & OSS3SensorRows)
    scoreSheetDF[,thisRQ] <- RqCqDFSeries$OSS3Score[theseRows]
  }
  
  # fix the columns data types
  scoreSheetDF$examName <- as.character(scoreSheetDF$examName)
  scoreSheetDF$seriesName <- as.character(scoreSheetDF$seriesName)
  scoreSheetDF$chartName <- as.character(scoreSheetDF$chartName)
  scoreSheetDF$sensorName <- as.character(scoreSheetDF$sensorName)
  i=5
  for(i in 5:ncol(scoreSheetDF)) {
    thisCol <- names(scoreSheetDF)[i]
    scoreSheetDF[,thisCol] <- as.numeric(scoreSheetDF[,thisCol])
  }
  # View(scoreSheetDF)
  
  scoreSheetName <- paste(examName, seriesName, "OSS3ScoresheetDF", sep="_")
  
  if(isTRUE(makeScoreSheetDF)) {
    assign(scoreSheetName, scoreSheetDF, pos=1)
  }
  
  if(isTRUE(writeScoreSheetCSV)) {
    write.csv(scoreSheetDF,
              file=paste0(str_sub(scoreSheetName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
}

############### OSS-3 series totals ###############

{
  
  # series totals are the RQ subtotals for all charts
  
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

############ OSS-3 chart subtotals ############

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

######## OSS-3 sensor scores for the series ########

{
  
  sensorTotalsDF <- data.frame(matrix(ncol=(length(OSS3Sensors2)), 
                                      nrow=length(uniqueCharts) ) )
  names(sensorTotalsDF) <- OSS3Sensors2
  sensorTotalsDF <- cbind(examName=examName,
                          seriesName=seriesName,
                          chartName=uniqueCharts,
                          subTotal="subTotal",
                          sensorTotalsDF)
  
  # iterate over the charts and OSS3Sensors2 vectors
  # uses the scoreSheetDF
  i=1
  for(i in 1:length(uniqueCharts)) {
    thisChart <- uniqueCharts[i]
    theseChartRows <- scoreSheetDF$chartName == thisChart
    # iterate over the sensors
    j=1
    for(j in 1:length(OSS3Sensors2)) {
      thisSensor <- OSS3Sensors2[j]
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



