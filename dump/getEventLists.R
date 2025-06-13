# make a list of all onset offset and answer events
#
# for each chart for each series for each exam in the cwd
# 
#
##################################


# get the stimulus segment using the values from the segEvents list
# stimSegment <- chartData[segEvents$onsetRow-30*50:segEvents$prestimRow+5+25,]

# can use na.omit(match(vecA, vecB))

library(stringr)

# # get exam names from the _Stimuli data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Stimuli$", pos=1),1, -9))

###

getEventLists <- function(x=uniqueExams, makeList=TRUE, returnOut=FALSE) {
  # function to make a list of vectors for onset offset and answer for each event
  # will save a list for each chart of each series and each exam
  # input is a vector of unique exam names in the global environment
  # output is a list from the last chart
  # each output list will have separate data frame for each stimulus question
  # including the sample row numbers for the Label Begin End and Answer
  
####
  
  # loop over the exams
  # i <- 1
  for (i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    examStimuli <- get(paste0(examName, "_Stimuli"), pos=1)
    # uniqueSeries <- unique(strtrim(examStimuli$chartName, 1))
    uniqueSeries <- unique(examStimuli$seriesName)
    
    # loop over each series
    # j <- 1
    for (j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      searchString <- paste0(seriesName, ".")
      uniqueCharts <- unique(grep(searchString, examStimuli$chartName, value=TRUE))
      
      # loop over unique charts for each series for each exam
      # k <- 1
      for (k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        chartStimuli <- examStimuli[examStimuli$chartName==chartName,]   
        
        # make an object to hold the results of each iteration of the the next loop
        eventList <- NULL
        
        # loop over the rows and make a list of onset, offset and answer rows in the data
        # l <- 1
        for (l in 1:nrow(chartStimuli)) {
          myList <- cbind.data.frame(chartStimuli[l,5],
                                     as.numeric(chartStimuli[l,6]),
                                     as.numeric(chartStimuli[l,7]),
                                     as.numeric(chartStimuli[l,8]))
          colnames(myList) <- c("Label", "Begin", "End", "Answer")
          eventList[[l]] <- myList
          # eventList[[l]] <- as.numeric(chartStimuli[l,6:8])
        }
        # using a loop allows a named list output
        names(eventList) <- chartStimuli$Label
        
#         # make a data frame of the onset offset and answer columns for each chart
#         # not using vectorization in order to use a list output
#         eventList <- cbind.data.frame(chartStimuli$Label,
#                                       as.numeric(chartStimuli$Begin), 
#                                       as.numeric(chartStimuli$End), 
#                                       as.numeric(chartStimuli$Answer), 
#                                       stringsAsFactors=FALSE)
#         # eventList <- chartStimuli[,6:8]
#         # row.names(eventList) <- chartStimuli$Label
#         colnames(eventList) <- c("Label", "Begin", "End", "Answer")

        eventListName <- paste0(examName, "_", chartName, "_eventList")
        
        #save each eventList
        if(makeList==TRUE) assign(eventListName, eventList, pos=1)
        
      } # end loop over unique charts
      
    } # end loop over unique series
    
  } # end loop over unique exam
  
  # return the last eventList
  if(returnOut==TRUE) return(eventList)
  
} # end function to get event lists      

getEventLists(x=uniqueExams, makeList=TRUE, returnOut=FALSE)

#ls(pattern="eventList")

