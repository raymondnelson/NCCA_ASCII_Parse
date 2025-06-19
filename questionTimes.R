# R script to extract the question lengtb
# July 21, 2023
# Raymond Nelson
# 
# called by a function in the newScores.R scrip
# Will also answer distance, response onset time, response length,

# response onset
# response peak

# stimInterval
# answerLat
# responseLength
# edaOnsetPhase
# edaPeakPhase
# eCardioPhase

# UPneumo
# LPneumo

# AutoEDA
# ManEDA
# Cardio
# ECardio
# FC



######### main function #############



extractResponseTimeFn <- function(RqCqDFSeries=RqCqDFSeries, 
                                  makeDF=FALSE,
                                  saveCSV=FALSE,
                                  analysisListName="analysisResultList" ) {
  # R script to extract the question lengtb
  # July 21, 2023
  # Raymond Nelson
  # 
  # called by the newScoresFn() function in the newScores.R script
  # after feature extraction
  # get the sample indices for response onset and response peak or end (EDA and cardio)
  # 
  # these columns are in the NCCA ASCII output
  # question onset
  # question offset
  # verbal answer
  
  # these columns are initialized in the _Measurements data frame
  # response onset - for EDA and cardio, the onset of a + slope or sig increase in + slope
  # response end - peak of reaction 
  # prestimulus index
  # recovery index
  #
  # can be used to calculate other parameters
  # stimInterval - length of time between the onset of each test stimulus
  # answerLat - latency time from stim offset to verbal answer
  # responseLength - time from response onset to response peak/end
  # edaOnsetPhase - time phase for response onset using Auto and Manual EDA
  # edaPeakPhase - time Phase for response peak using Auto and Manual EDA
  # eCardioPhase - phasing differences for the electronic cardio sensor
  #
  ####
  # x input is a data frame of measurements for the RQs and CQs for a test chart
  # output is the RqCqDF data frame with the response columns populated 
  ####
  
  {
    
    print("obtain the response onset and peak indices for all event and charts in the series")
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # reset the rankScore column
    RqCqDFSeries$RRMScore <- ""
    # View(RqCqDFSeries)
    
  }
  
  {
    
    # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    useSensors <- c("UPneumo", 
                    "LPneumo", 
                    "Pneumo",
                    "AutoEDA", 
                    "ManualEDA", 
                    "Cardio", 
                    # "FC",
                    "PLE")
    
    # remove sensors not in the data 
    useSensors <- useSensors[useSensors %in% uniqueSensors]
    
    # exclude the PLE using a setting and if missing
    if("PLE" %in% useSensors && !isTRUE(includePLEScores)) {
      useSensors <- useSensors[-which(useSensors %in% "PLE")]
    }
    
    # initialize a vector of unique RQ and CQ events
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) < 4) { 
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
  }
  
  #### iterate over the charts to get the response indices ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    {
      
      chartRows <- which(RqCqDFSeries$chartName == uniqueCharts[i])
      
      # initialize the chart data frame
      RqCqDFChart <- RqCqDFSeries[chartRows,]
      
      uniqueEventsChart <- unique(RqCqDFChart$eventLabel)
      
      if(length(uniqueEventsChart) < 4) next()
      
    }
    
    #### iterate over the sensors ####
    
    j=1
    for(j in 1:length(uniqueEventsChart)) {
      
      thisSensor <- useSensors[j]
      
      # get the rows for the sensor
      sensorRows <- which(RqCqDFChart$sensorName==thisSensor)
      
      
      
      # initialize a vector of measurements for the sensor
      measurementVector <- RqCqDFChart[sensorRows,'sensorMeasurement']
      
      # oct 1, 2021
      # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
      #   measurementVector <- exp(measurementVector)
      #   # pneumo R/C ratios are now decimals < 1 
      #   # and smaller values are larger responses 
      # }
      
      # next j iteration if there are no measurements
      if(length(which(!is.na(measurementVector))) == 0) {
        next()
      }
      
      # zero as missing
      measurementVector[measurementVector == 0] <- NA
      
      # Oct 4, 2021
      # rescale the respiration measurements 
      # if(thisSensor %in% c("UPneumo", "LPneumo", "Pneumo")) {
      #   measurementVector <- round(exp(-measurementVector) * 100, 3)
      #   # larger values now signify greater changes in physiology
      # }
      
      # get the max min and range for the sensor scores
      if( length(which(!is.na(measurementVector))) > 2 ) {
        maxResponse <- max(measurementVector, na.rm=TRUE) 
        minResponse <- min(measurementVector, na.rm=TRUE)
      } else {
        # submit the measurementVector to the RqCqDFChart
        RqCqDFChart$RRMScore[sensorRows] <- measurementVector
        next() # next j
      }
      
      # calculate the range of response magnitudes for this sensor
      responseRange <- maxResponse - minResponse
      
      # initialize a vector
      RRMVector <- rep(NA, times=length(measurementVector))
      
    } # end loop j over sensors
    
    
    
  }
  
  
  
  
  # measurementDF <- cbind(measurementDF[1:10],
  #                        measurementDF$responseOnset <- "",
  #                        measurementDF$responsePeak <- "",
  #                        measurementDF[11:ncol(measurementDF)] )
  
  
}
  





