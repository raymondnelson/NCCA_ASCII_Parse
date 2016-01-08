# fix duplicate event Names
# 10-1-2015 Raymond Nelson
#
# function to ensure that all scored events have unique names
#
################################



library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[6]



cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15
addSeg <- 5



##############

x=uniqueExams
showNames=TRUE
output=FALSE


fixDuplicatesFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # function to iterate over a vector of data frame names 
  # and add UPneumoArtifacts and LPneumoArtifacts column to the time series data frame
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  ##########################
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    ### add additional columns here
    
    ###
    
    # get the names of all unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=3
      for(k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        eventIndices <- which(chartDF$eventLabel!="")
        excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN")
        eventIndices <- eventIndices[-which(eventNames %in% excludeEvents)]
        eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        
         ####### correct for repeated segments by appending the name of the repetition
        
        #private function to fix duplicate/repeated event names
        fixDup <- function (x) {
          dups <- which(duplicated(x, fromLast = TRUE))
          if(length(dups)==0) return(x)
          for (m in 1:length(dups)) {
            x[dups[m]] <- paste0(x[dups[m]], "a")
            fixDup(x) # added 10-3-2015 to check for multple repetitions of an event
          }
          fixDup(x)
          return(x)
        }
        eventNames <- fixDup(x=eventNames)  
        
        # add the corrected event names to the chartDF
        for (n in 1:length(eventNames)){
          chartDF$eventLabel[eventIndices[n]] <- eventNames[n]
        }
        
        
        
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end iteration over j series data frames
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1) 
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end fixDuplicatesFn()

# fixDuplicatesFn(x=uniqueExams, showNames=TRUE, output=FALSE)


