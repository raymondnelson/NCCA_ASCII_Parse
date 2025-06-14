# 7-28-2015
# add columns to all time series data frames
# eventLabel
# events
# stimText
# answer
# 
#
# these column can then be used to select segments for processing and plotting
#
#
##########################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# uniqueExams <- uniqueExams[1:2]



addStimulusColumns <- function(x=uniqueExams, 
                               output=FALSE, 
                               showNames=TRUE) {
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
  # eventLabel indicating the onset of the stimulus text
  # Events including Begin, End and Answer rows
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
    examDF <- get(paste0(examName, "_Data"), pos=1)
    eventDF <- get(paste0(examName, "_Stimuli"), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # remove extra characters from the lables column
    library(stringr)
    examDF$Label <- as.character(examDF$Label)
    examDF$Label <- str_replace_all(examDF$Label, "[- ]", "")
    
    #add the 4 new columns
    examDF$eventLabel <- rep("", nrow(examDF))
    examDF$Events <- rep("", nrow(examDF))
    examDF$stimText <- rep("", nrow(examDF))
    examDF$Answer <- rep("", nrow(examDF))
    
    # re-order the data frame columns
    examDF <- examDF[,c(1:6,(ncol(examDF)-3):ncol(examDF),7:(ncol(examDF)-4))]

    # get the names of all unique series for the exam
    uniqueSeries <- unique(as.character(examDF$seriesName))
    
    # loop over each unique series
    # j=2
    for(j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series ", seriesName))
      
      # make a vector of names for all charts in the series
      uniqueCharts <- unique(as.character(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        
        # set this row to the examDF not the seriesDF so we can save the data directly to the ExamDF
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(showNames==TRUE) print(chartName)
        
        chartEvents <- eventDF[eventDF$chartName==chartName,]
        
        # increment the loop if there are no events
        if(nrow(chartEvents)==0) next()
        
        ### add the data to the new columns
        
        chartDF$Events[as.numeric(chartEvents$Begin)] <- "onsetRow"
        
        # correct for situation where End and Begin have the same row
        # l=1
        for(l in 1:length(chartEvents$End)) {
          if(as.numeric(chartEvents$End[l])<=as.numeric(chartEvents$Begin[l])) {
            chartEvents$End[l] <- as.numeric(chartEvents$Begin[l])+1 }
          } # end for loop
        
#         ifelse((as.numeric(chartEvents$End) == as.numeric(chartEvents$Begin)),
#                chartEvents$End <- as.numeric(chartEvents$Begin)+1,
#                chartEvents$End <- chartEvents$End)
#                # chartDF$Events[as.numeric(chartEvents$Begin)+1] <- "offsetRow",
#                # chartDF$Events[as.numeric(chartEvents$End)] <- "offsetRow")
        
        chartDF$Events[as.numeric(chartEvents$End)] <- "offsetRow"
        
        # correct for situation where Answer and End have the same row
        for(m in 1:length(chartEvents$Answer)) {
         if(as.numeric(chartEvents$Answer[m])<=as.numeric(chartEvents$End[m])) {
           chartEvents$Answer[m] <- as.numeric(chartEvents$End[m])+1 }
         } # end for loop
        

#                  ifelse((as.numeric(chartEvents$Answer) <= as.numeric(chartEvents$End)),
#                    
#                }
#                
#                chartEvents$Answer <- as.numeric(chartEvents$End)+1,
#                
#                chartEvents$Answer <- chartEvents$Answer
#                )
               # ifelse((as.numeric(chartEvents$Answer) == as.numeric(chartEvents$Begin)),
                   # chartDF$Events[as.numeric(chartEvents$Begin)+2] <- "answerRow",
                   # chartDF$Events[as.numeric(chartEvents$End)+1] <- "answerRow"),
            # chartDF$Events[as.numeric(chartEvents$Answer)] <- "answerRow")
        
        chartDF$Events[as.numeric(chartEvents$Answer)] <- "answerRow"  
        
        # add the event label to the onset row for each stimulus event
        chartDF$eventLabel[as.numeric(chartEvents$Begin)] <- chartEvents$Label
        # add the stimulus text statement to the onset row for each stimulus event
        chartDF$stimText[as.numeric(chartEvents$Begin)] <- chartEvents$Statement
        # add the answer to the answer row for each stimulus event
        # chartDF$Answer[as.numeric(chartEvents$Answer)] <- chartDF$Label[as.numeric(chartEvents$Answer)]
        # chartDF$Answer[chartDF$Events=="answerRow"] <- chartDF$Label[as.numeric(chartEvents$Answer)]
        chartDF$Answer[as.numeric(chartEvents$Answer)] <- chartDF$Label[as.numeric(chartEvents$Answer)]
        
        # save the chart data to the time series data frame
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over each K chart
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF
      
    } # end loop over each j series
    
    # save the time series data frame to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
  
  } # end loop over each i exam
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the output from the last unique exam
  if(output==TRUE) return(examDF)

} # end addStimulusColumns() function
  
# addStimulusColumns(x=uniqueExams, output=FALSE, showNames=TRUE)
  