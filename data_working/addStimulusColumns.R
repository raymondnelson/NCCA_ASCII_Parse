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
    
    if(showNames==TRUE) print(examName)
    
    # get the time series data  
    examDF <- get(paste0(examName, "_Data"), pos=1)
    eventDF <- get(paste0(examName, "_Stimuli"), pos=1)
    
    # remove extra characters from the lables column
    library(stringr)
    examDF$Label <- as.character(examDF$Label)
    examDF$Label <- str_replace_all(examDF$Label, "[- ]", "")
    
    #add the 4 new columns
    examDF$eventLabel <- rep("", nrow(examDF))
    examDF$Events <- rep("", nrow(examDF))
    examDF$stimText <- rep("", nrow(examDF))
    examDF$Answer <- rep("", nrow(examDF))
    
    examDF <- examDF[,c(1:6,(ncol(examDF)-3):ncol(examDF),7:(ncol(examDF)-4))]

    # get the names of all unique series for the exam
    uniqueSeries <- unique(as.character(examDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series ", uniqueSeries[j]))
      
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      # make a vector of names for all charts in the series
      uniqueCharts <- unique(as.character(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        
        chartName <- uniqueCharts[k]
        
        chartOnsetRow <- which(examDF$chartName==uniqueCharts[k])[1]
        
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        chartEvents <- eventDF[eventDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(chartName)
        
#         ### add the columns
#         
#         chartDF$eventLabel <- rep("", nrow(chartDF))
#         chartDF$Events <- rep("", nrow(chartDF))
#         chartDF$stimText <- rep("", nrow(chartDF))
#         chartDF$Answer <- rep("", nrow(chartDF))
        
        ### add the data to the new columns
        
        chartDF$eventLabel[as.numeric(chartEvents$Begin)] <- chartEvents$Label
        chartDF$Events[as.numeric(chartEvents$Begin)] <- "onset"
        chartDF$Events[as.numeric(chartEvents$End)] <- "offset"
        chartDF$Events[as.numeric(chartEvents$Answer)] <- "answer"
        chartDF$Answer[as.numeric(chartEvents$Answer)] <- chartDF$Label[as.numeric(chartEvents$Answer)]
        chartDF$stimText[as.numeric(chartEvents$Begin)] <- chartEvents$Statement
        
        # save the chart data to the time series data frame
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over each K chart
      
    } # end loop over each j series
    
    # save the time series data frame to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
  
  } # end loop over each i exam
  
  # return the output from the last unique exam
  if(output==TRUE) return(examDF)

} # end addStimulusColumns function
  
addStimulusColumns(x=uniqueExams, output=FALSE, showNames=TRUE)
  