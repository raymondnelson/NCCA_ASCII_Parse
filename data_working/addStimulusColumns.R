# 7-28-2015
# add a column to all time series data frames
# to indicate the onset row for all stimulus events
#
# this column can then be used to select segments for processing and plotting
#
# also add a column to all time series data frames
# to include the stimulus text statement on every row during the stim presentation
#
##########################################



library(stringr)



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# uniqueExams <- uniqueExams[1:2]



addStimulusColumns <- function(x=uniqueExams, 
                               output=FALSE, 
                               showNames=FALSE) {
  # function to apply add the events and stimulus text columns to the time series data
  #
  # x input is a list of unique exams
  # output=TRUE will output the list for the last exam series in the nput
  # showNames=TRUE will print the exam series names and chart names to the console
  #
  # this function will select each data frame in input vector of data frame names
  # then select unique series and unique charts for each exam
  #
  # output is a data frame with the same name
  # with additional columns for
  #
  # eventLabel indicating the onset
  # Events
  # stimText
  # Answer
  
  ###
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    # get the exam name
    examName <- uniqueExams[i]
    
    # get the time series data  
    timeSeriesDF <- get(paste0(examName, "_Data"), pos=1)
    eventDF <- get(paste0(examName, "_Stimuli"), pos=1)
    
    # remove extra characters from the lables column
    library(stringr)
    timeSeriesDF$Label <- as.character(timeSeriesDF$Label)
    timeSeriesDF$Label <- str_replace_all(timeSeriesDF$Label, "[- ]", "")
    
    #add the columns
    timeSeriesDF$eventLabel <- rep("", nrow(timeSeriesDF))
    timeSeriesDF$Events <- rep("", nrow(timeSeriesDF))
    timeSeriesDF$stimText <- rep("", nrow(timeSeriesDF))
    timeSeriesDF$Answer <- rep("", nrow(timeSeriesDF))

    # get the names of all unique series for the exam
    seriesNames <- unique(as.character(timeSeriesDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(seriesNames)) {
      
      if(showNames==TRUE) print(seriesNames[j])
      
      seriesDF <- timeSeriesDF[timeSeriesDF$seriesName==seriesNames[j],]
      
      # make a vector of names for all charts in the series
      chartNames <- unique(as.character(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(chartNames)) {
        # get the data frame with the time series data for each chart in the series
        
        chartName <- chartNames[k]
        
        chartOnsetRow <- which(timeSeriesDF$chartName==chartNames[k])[1]
        
        chartData <- seriesDF[seriesDF$chartName==chartNames[k],]
        chartEvents <- eventDF[eventDF$chartName==chartNames[k],]
        
        if(showNames==TRUE) print(chartName)
        
#         ### add the columns
#         
#         chartData$eventLabel <- rep("", nrow(chartData))
#         chartData$Events <- rep("", nrow(chartData))
#         chartData$stimText <- rep("", nrow(chartData))
#         chartData$Answer <- rep("", nrow(chartData))
        
        ### add the data to the new columns
        
        chartData$eventLabel[as.numeric(chartEvents$Begin)] <- chartEvents$Label
        chartData$Events[as.numeric(chartEvents$Begin)] <- "onset"
        chartData$Events[as.numeric(chartEvents$End)] <- "offset"
        chartData$Events[as.numeric(chartEvents$Answer)] <- "answer"
        chartData$Answer[as.numeric(chartEvents$Answer)] <- chartData$Label[as.numeric(chartEvents$Answer)]
        chartData$stimText[as.numeric(chartEvents$Begin)] <- chartEvents$Statement
        
      } # end loop over each K chart
      
      # save the chart data to the time series data frame
      timeSeriesDF[chartOnsetRow:nrow(chartData),] <- chartData
      
    } # end loop over each j series
    
  } # end loop over each i exam
  
} # end addStimulusColumns function
  
  