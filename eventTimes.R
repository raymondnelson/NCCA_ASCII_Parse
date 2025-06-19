# R script to make an aggregated data frame of stimulus event indices for all exams in a sample
# August 4, 2023
# Raymond Nelson
# 
# input is a vector of exam names
# ouput is a skiny data frame of events for all exams in a sample
#
# Case ID
# series name
# chart name
# Event label
# Senor Name 

# stimulus onset 
# stimulus offset
# verbal answer
# prestim start
# latency
# ROW End
# WOE End
# response onset
# response peak
# response recovery # a value at an arbirary point after the stimulus offset
# post stim # a value at the end of the stimulus segment

# output is a data frame

####

# library(stringr)

# source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)


# need the getFirstLastEventFn() 
# source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)





# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]




aggreateEventIndicesFn<- function(x=uniqueExams, 
                                  sensors=c("UPneumo", "LPneumo", "AutoEDA", "ManualEDA", "Cardio", "PLE"),
                                  # onlyCQT=TRUE,
                                  writeCSV=TRUE, 
                                  showNames=TRUE, 
                                  output=FALSE ) {
  # R script to make an aggregated skiny data frame of stimulus event indices for all exams in a sample
  # August 4, 2023
  # Raymond Nelson
  # 
  # input is a vector of exam names
  # ouput is a skiny data frame of events for all exams in a sample
  #
  # Case ID
  # series name
  # chart name
  # Event label
  # Senor Name 
  
  # stimulus onset 
  # stimulus offset
  # verbal answer
  # prestim start
  # latency
  # ROW End
  # WOE End
  # response onset
  # response peak
  # response recovery # a value at an arbirary point after the stimulus offset
  # post stim # a value at the end of the stimulus segment
  
  # output is a skinny data frame
  
  ####
  
  uniqueExams <- x
  
  print("aggregate the event indices for all cases in a sample (in the global envir)")
  
  # initialize the output data frame
  outputDF <- NULL
  
  #### iterate over each exam in the list ####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    #### get the data for this exam ####
    
    {
      
      examName <- uniqueExams[i]
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Measurements", "*")
      examMeasurementsDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      examMeasurementsDF$examName <- as.character(examMeasurementsDF$examName)
      examMeasurementsDF$seriesName <- as.character(examMeasurementsDF$seriesName)
      examMeasurementsDF$chartName <- as.character(examMeasurementsDF$chartName)
      
      # examStartRow <- 1
      # examEndRow <- nrow(examMeasurementsDF)
      
      # assign("examMeasurementsDF", examMeasurementsDF, pos=1)
      # assign("examName", examName, pos=1)
      
      if(showNames==TRUE) print(examName)
      
      # get the names of all unique series in the exam
      uniqueSeries <- as.character(unique(examMeasurementsDF$seriesName))
      
    }
    
    #### iterate over each unique series ####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        # get the list of time series data for the charts in the exam
        seriesMeasurementsDF <- examMeasurementsDF[examMeasurementsDF$seriesName==seriesName,]
        
        # seriesOnsetRow <- range(which(examMeasurementsDF$seriesName==seriesName))[1]
        # seriesEndRow <- range(which(examMeasurementsDF$seriesName==seriesName))[2]
        seriesOnsetRow <- which(examMeasurementsDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesMeasurementsDF) - 1
        
        # assign("seriesMeasurementsDF", seriesMeasurementsDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        if(showNames==TRUE) print(paste("series", seriesName))
        
        # uniqueCharts <- names(seriesMeasurementsDF)
        uniqueCharts <- as.character(unique(seriesMeasurementsDF$chartName))
        
      }
      
      #### iterate over each unique chart in the series ####
      
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          chartName <- uniqueCharts[k]
          # get the time series data frame for each chart in the series
          chartMeasurementsDF <- seriesMeasurementsDF[seriesMeasurementsDF$chartName==chartName,]
          # View(chartDF)
          
          chartOnsetRow <- which(seriesMeasurementsDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartMeasurementsDF) - 1
          
          # assign("chartMeasurementsDF", chartMeasurementsDF, pos=1)
          # assign("chartName", chartName, pos=1)
          
          print(paste("Chart:", chartName))
          
          # skip short charts less than 5 events
          if(nrow(chartMeasurementsDF)<70) next()
          
          # a vector of event onset rows
          Labels <- chartMeasurementsDF$Label[chartMeasurementsDF$eventLabel!=""]
          eventLabels <- chartMeasurementsDF$eventLabel[chartMeasurementsDF$eventLabel!=""]
          
          # advance to the next chart if no events
          if(length(eventLabels)==0) {
            print("no stimulus events")
            next()
          } 
          
          uniqueSensors <- unique(chartMeasurementsDF$sensorName)
          
          if(!exists("sensors")) sensors <- c("UPneumo", "LPneumo", "AutoEDA", "ManualEDA", "Cardio", "PLE")
          useSensors <- uniqueSensors[which(uniqueSensors %in% sensors)]
          
        }
        
        #### intialize a data frame to hold the output ####
        
        {  
          eventIndicesDF <- NULL
        }
        
        #### slice and retain the events for this chart ####
        
        {
          
          keepRows <- which(chartMeasurementsDF$sensorName %in% useSensors)
          
          keepChartMeasurementsDF <- chartMeasurementsDF[keepRows,c(1:3,9,5,6:8,11:18)]
            
          # View(keepChartMeasurementsDF)
        }
    
        outputDF <- rbind.data.frame(outputDF, keepChartMeasurementsDF)
        
        row.names(outputDF) <- NULL
        
        # View(outputDF)
        # View(keepChartMeasurementsDF)
        
      } # end iteration over k chart data frames 
      
          } # end iteration over j series data frames
    
    #### main output is a side effect ####
    
  } # end iteration over i exams
  
  row.names(outputDF) <- NULL
  
  DFName <- paste0("aggregatedEventIndices", "N", length(uniqueExams), "DF")
  assign(DFName, outputDF, envir=.GlobalEnv)
  
  #### .csv output ####
  
  # .csv files are created separately for each series 
  
  if(writeCSV==TRUE) {
    # construct the .csv filename
    outputFileName <- paste0("aggregatedEventIndices", "N", length(uniqueExams), ".csv")
    
    write.csv(outputDF, file=outputFileName, row.names=FALSE)
  }

  #### output ####
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(outputDF)
  
} # end aggreateEventIndicesFn()

# aggreateEventIndicesFn(x=uniqueExams,
#                        sensors=c("UPneumo", "LPneumo", "AutoEDA", "ManualEDA", "Cardio", "PLE"),
#                        writeCSV=FALSE, 
#                        showNames=TRUE, 
#                        output=FALSE )


