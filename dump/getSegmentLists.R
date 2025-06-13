# get all stimulus segments for feature extraction or plotting
#
# for each chart for each series for each exam in the cwd
# 
#
##################################



library(stringr)

# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))


cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15


###

getDataSegmentLists <- function(x=uniqueExams, makeList=TRUE) {
  # function to get a list of time series data frame segments 
  # for plotting and feature extraction
  #
  # requires eventLists from the getEventLists function
  #
  # will save a list containing the time series segements 
  # for each chart of each series and each exam
  # output is a list from the last chart
  
  ####
  
  # loop over the exams
  # i=1
  for (i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    
    # make a vector of the names of eventLists for each exam
    # uses glob2rx to make the regex searchstring
    eventLists <- ls(pattern=glob2rx(paste0(examName, "*_eventList$")), pos=1) 
    
    # use the data frame of all exam data to get the names of unique series
    examDF <- get(paste0(examName, "_Data"), pos=1)
    uniqueSeries <- unique(strtrim(examDF$chartName, 1))



    # get the list names for the data for each unique series
    # uniqueSeries <- ls(pattern=glob2rx(paste0("*", examName, "*_eventList*"), trim.head=TRUE, trim.tail=TRUE), pos=1)
    # trim the series names
    # uniqueSeries <- str_sub(uniqueSeries, -8, -8)
    
    # loop over each unique series in each exam
    # j=1
    for (j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      
#       searchString <- paste0(seriesName, ".")
#       uniqueCharts <- unique(grep(searchString, examDF$chartName, value=TRUE))
      
      # get the series data
      # seriesDF <- get(seriesName, pos=1)
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]

      # uniqueCharts <- names(seriesDF)      
      uniqueCharts <- as.character(unique(seriesDF$chartName))

      # loop over unique chart for each series for each exam
      # k=1
      for (k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        
        # chartDF <- seriesDF[[k]]
        chartDF <- examDF[examDF$chartName==chartName,]      
        
        # make a variable to hold the results
        dataSegmentList <- NULL
        
        # get the eventList for the chart
        chartEventList <- get(paste0(examName, "_", chartName, "_eventList"), pos=1)
        
        # loop over the list of vectors
        # and save the result as a list of stimulus segments
        # for each unique chart
        # with the same name as the event labels
        # l=1
        for (l in 1:length(chartEventList)) {
          
          startRow <- unlist(chartEventList[l])[2]
          # startRow <- chartEventList[l,2]
          ifelse(startRow>=prestimSeg*cps, 
                 prestimRow<-startRow-prestimSeg*cps, 
                 prestimRow<-1)
          endRow <- startRow + 25*cps
          mySegment <- chartDF[prestimRow:endRow,]
          dataSegmentList[[l]] <- mySegment
        }
        print(names(chartEventList))
        # names(dataSegmentList) <- chartEventList$Label
        
        # add the names after the loop is complete
        names(dataSegmentList) <- names(chartEventList)
        
        #save each eventList
        dataSegmentListName <- paste0(examName, "_", chartName, "_dataSegmentList")
        if(makeList==TRUE) {
          assign(dataSegmentListName, dataSegmentList, pos=1)
        } # end if
        
      } # end loop over unique charts
      
    } # end loop over unique series
    
  } # end loop over unique exam
  
  # return the last eventList
  # return(dataSegmentList)
  
} # end function to get data segments

getDataSegmentLists(x=uniqueExams, makeList=TRUE)


# ls(pattern="dataSegmentList")
# head(PF090316_1.01A_dataSegmentList[[2]], 160)
# str(PF090316_1.01A_dataSegmentList[[2]])
# str(PF090316_1.01A_dataSegmentList[[1]])
# length(PF090316_1.01A_dataSegmentList[[1]])

# rm(list=ls(pattern="dataSegmentList"))
