




RRMScoreFn <- function(RqCqDFSeries=RqCqDFSeries, 
                       makeDF=TRUE,
                       writeCSV=FALSE) {
  # R function to compute the Relative Response Magnitudes from the measurements data frame
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  # x input is a data frame of measurements for the RQs and CQs for a test chart
  # output is the RqCqDF data frame with the RRM column populated 
  # 
  # RRM is (x-min)/(max-min)
  # largest response = 1
  # smallest response = 0
  ##################
  
  # reset the rankScore column
  RqCqDFSeries$RRMScore <- ""
  # View(RqCqDFSeries)
  
  # initialize a vector of unique sensor names
  uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
  
  RRMSensors <- c("UPneumo", 
                  "LPneumo", 
                  "Pneumo",
                  "AutoEDA", 
                  "ManualEDA", 
                  "Cardio", 
                  "PLE")
  
  # remove RRM sensors not in the data 
  RRMSensors <- RRMSensors[RRMSensors %in% uniqueSensors]
  
  # initialize a vector of unique RQ and CQ events
  uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
  
  # exit if there are no unique events
  if(length(uniqueQuestions) < 4) { 
    return(RqCqDFSeries) 
  }
  
  uniqueCharts <- unique(RqCqDFSeries$chartName)
  
  #### iterate over the charts ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    chartRows <- which(RqCqDFSeries$chartName == uniqueCharts[i])
    
    # initialize the chart data frame
    RqCqDFChart <- RqCqDFSeries[chartRows,]
    
    uniqueEventsChart <- unique(RqCqDFChart$eventLabel)
    
    if(length(uniqueEventsChart) < 4) next()
    
    # iterate over the sensor names to calculate the RRM score
    j=1
    for (j in 1:length(RRMSensors)) {
      sensorRows <- which(RqCqDFChart$sensorName==RRMSensors[j])
      
      # initialize a vector of measurements for the sensor
      measurementVector <- RqCqDFChart[sensorRows,'sensorMeasurement']
      
      # next iteration if there are no measurements
      if(length(which(!is.na(measurementVector))) == 0) {
        next()
      }
      
      # fix negative measurement values
      measurementVector[measurementVector < 0] <- NA
      
      # get the max min and range for the sensor scores
      if(length(which(!is.na(measurementVector)))) {
        maxResponse <- max(measurementVector, na.rm=TRUE) 
        minResponse <- min(measurementVector, na.rm=TRUE)
      } else {
        # submit the measurementVector to the RqCqDFChart
        RqCqDFChart$RRMScore[sensorRows] <- measurementVector
        next() # next j
      }
      
      
      # calculate the range of response magnitudes
      responseRange <- maxResponse - minResponse
      
      # iterate over the stimulus events to calculate the RRM for each event
      # k=1
      for(k in 1:length(measurementVector)) {
        thisResponse <- measurementVector[k]
        # next measurement if this one is NA
        if(is.na(thisResponse)) next()
        # calculate the RRM
        if(responseRange == 0) {
          measurementVector[k] <- NA
          next()
        }
        thisRRM <- round((thisResponse - minResponse) / responseRange, 2)
        
        # add the RRM to the measurementVector
        measurementVector[k] <- thisRRM
        
      } # end iteration over k events for each sensor
      
      # submit the measurementVector to the RqCqDFChart
      RqCqDFChart$RRMScore[sensorRows] <- measurementVector
      
    } # end iteration over j uniqueSensors
    
    # pass the chart data frame back to the series data frame
    RqCqDFSeries[chartRows,] <- RqCqDFChart
    
  } # end loop i over charts
  
  ################ output #################
  
  #### initialize a data frame of RRM scores
  
  {
    
    # initialize a data frame for the rank scores
    RRMScoresDF <- data.frame(matrix(ncol=(length(uniqueQuestions)), 
                                     nrow=length(uniqueCharts)*length(RRMSensors)))
    names(RRMScoresDF) <- uniqueQuestions
    RRMScoresDF <- cbind(sensorName=rep(RRMSensors, times=length(uniqueCharts)), 
                         RRMScoresDF)
    RRMScoresDF$sensorName <- as.character(RRMScoresDF$sensorName)
    RRMScoresDF <- cbind(chartName=rep(uniqueCharts, each=length(RRMSensors)), 
                         RRMScoresDF)
    RRMScoresDF$chartName <- as.character(RRMScoresDF$chartName)
    # View(RRMScoresDF)
    
    # populate the data frame with the measurements
    i=1
    for(i in 1:nrow(RRMScoresDF)) {
      thisChart <- RRMScoresDF[i,1]
      thisSensor <- RRMScoresDF[i,2]
      # iterate over the questions
      j=3
      for(j in 3:ncol(RRMScoresDF)) {
        thisQuestion <- names(RRMScoresDF)[j]
        # now get the measurement
        thisOne <- which(RqCqDFSeries$chartName==thisChart &
                           RqCqDFSeries$sensorName==thisSensor &
                           RqCqDFSeries$eventLabel==thisQuestion)
        if(length(thisOne) == 0 ) next()
        RRMScoresDF[i,j] <- RqCqDFSeries$RRMScore[thisOne]
      }
    }
    
  }
  
  #### save it 
  
  outputDFName <- paste(examName, seriesName, "RRMScoreDF", sep="_")
  
  if(isTRUE(makeDF)) {
    assign(outputDFName, RRMScoresDF, pos=1)
  }
  
  if(isTRUE(writeCSV)) {
    write.csv(RRMScoresDF,
              file=paste0(str_sub(outputDFName, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  #### output the RqCqDFSeries data frame 
  
  return(RqCqDFSeries)
  
  # end RRMScoreFn()
} 


