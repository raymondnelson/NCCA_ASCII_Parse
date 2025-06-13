# fix duplicate event Names
# 10-1-2015 Raymond Nelson
#
# function to ensure that all scored events have unique names
#
####



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[6]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15
# addSeg <- 5



##############

# x=uniqueExams
# showNames=TRUE
# output=FALSE


# source(paste0(RPath, 'NCCAASCIIParseHelperFunctions.R'), echo=FALSE)


fixDuplicatesFn<- function(x=uniqueExams, 
                           fixAnnotations=FALSE,
                           makeDF=TRUE, 
                           showNames=TRUE, 
                           output=false) {
  # function to iterate over a vector of data frame names 
  # and fix duplicate question tags
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  # this script is sourced and the function is called in the NCCAASCIIParse.R script
  # and calls the fixDupFn() function in the NCCAASCIIParseHelperFunctions.R script
  #
  ##########################
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      # assign("examDF", examDF, pos=1)
      
      if(showNames==TRUE) print(examName)
      
      examStartRow <- 1
      examEndRow <- nrow(examDF)
      
      # get the names of all unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
    }
    
    # loop over each unique series
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        # assign("seriesDF", seriesDF, pos=1)
        
        if(showNames==TRUE) print(paste("series", seriesName))
        
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        # uniqueCharts <- names(seriesDF)
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
      
      # loop over each chart in the series 
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          chartName <- uniqueCharts[k]
          
          if(showNames==TRUE) print(uniqueCharts[k])
          
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          # assign("chartDF", chartDF, pos=1)
          
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
        }
        
        ### process the stimulus segments
        
        {
          
          # first get the event labels and event onset indices
          
          # make a vector of row indices for each event onset
          eventIndices <- which(chartDF$eventLabel!="")
          
          # a vector of event labels
          eventNames <- chartDF$Label[eventIndices]
          
          # make a vector of answers and announcements to remove from the eventNames vector
          removeEvents <- c("YES", "NO", "Yes", "No", "yes", "no", "ANS", "ans", "x", "X", "xx", "XX", "xxx", "xxxx", "XXX", "XXXX")
          
          # remove excluded events using the global variable excludedEvents
          if(!fixAnnotations) {
            removeEvents <- sort(unique(c(removeEvents, excludeEvents)))
          }
          # eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          # changed this to keep and fix repeated annotations 11/30/2016
          
          eventNames <- eventNames[!(eventNames %in% removeEvents)]
          
          # remove event indices for events excluded by removeEvents
          eventIndices <- eventIndices[!(chartDF$Label[eventIndices] %in% removeEvents)]

          # reset
          chartDF$eventLabel[eventIndices] <- chartDF$Label[eventIndices]
                    
        }
        
        #### correct for repeated stimulus events by appending to the name of the repetition
        
        # proceed only if there are any events
        if(length(eventNames) > 0) {
          # call the fixDupFn() function in the NCCAASCIIParseHelperFunctions.R script
          fixedEventNames <- fixDupFn(x=eventNames)  
          # add the corrected event names to the chartDF
          # this is vectorized and needs no loop
          chartDF$eventLabel[eventIndices] <- fixedEventNames
          # save the chartDF to the seriesDF
          # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
          # instead save the chartDF to the examDF
          examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF
        } # end if for events > 0
        
      } # end iteration over k chart data frames 
      
      # save the seriesDF to the examDF
      # not needed because the chartDF is previously saved directly to the examDF
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end iteration over j series data frames
    
    # save the examDF to the global environment 
    if(makeDF==TRUE) assign(paste0(examName, "_Data"), examDF, pos=1) 
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) { 
    return(examDF) } else {
      return( paste0(uniqueExams, "_Data") )
    }
  
} # end fixDuplicatesFn()

# fixDuplicatesFn(x=uniqueExams, showNames=TRUE, output=FALSE)


