# get segment
# 
# sampling rate is 30cps
# x input is a vector of time series data
# x length is from stimulus onset to 15 seconds after stimulus onset
# measurement is the sum of absolute differences between successive samples 
#
################################



library(stringr)

# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]



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



# get a segment instead of iterating through all exams 
getSegment <- TRUE
examNum <- 1
seriesNum <- 1
chartNum <- 1
segmentNum <- 4



getSegmentFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE, getSegment=getSegment) {
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
  
  print(paste0("get segment: ", getSegment))
  
  if(getSegment == TRUE) {
    assign("examName", uniqueExams[examNum], pos=1)
    uniqueExams <- uniqueExams[examNum] 
  } 
  
  # source the list of excluded events so that measurements are not plotted for these
  source('~/Documents/R_programming/NCCA_ASCII_Parse/excludedEvents.R')

    # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(getSegment==TRUE) {
      assign("examDF", examDF, pos=1)
      # break
    }
    
    if(showNames==TRUE) print(examName)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    ### add additional columns here
    
    ###
    
    # get the names of all unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    if(getSegment == TRUE) { 
      uniqueSeries <- uniqueSeries[seriesNum] 
      assign("uniqueSeries", uniqueSeries, pos=1)
      assign("seriesName", uniqueSeries, pos=1) 
    }
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(getSegment==TRUE) {
        assign("seriesDF", seriesDF, pos=1)
        # break
      }
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      if(getSegment == TRUE) {
        uniqueCharts <- uniqueCharts[chartNum]
        assign("uniqueCharts", uniqueCharts, pos=1)
        assign("chartName", uniqueCharts, pos=1) 
      }
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        chartName <- uniqueCharts[k]

        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(getSegment==TRUE) {
          assign("chartDF", chartDF, pos=1)
          # break
        }
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB")
        # eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        
        if(getSegment == TRUE) { 
          eventNames <- eventNames[segmentNum] 
        }
        
        # loop over all the events in the chart data frame
        for (l in 1:length(eventNames)) {
          # l=1
          segmentName <- eventNames[l]
          # get the onset row  for the chart DF so that events during the prestimSegment are ignored
          segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          # set the end row so that the data frame
          segEndRow <- segOnsetRow + measuredSeg*cps
          if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
          # set the row number for the end of the stimulus segment
          endRow <- segEndRow + addSeg*cps-1
          if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)

          # get the segment start row
          startRow <- prestimRow
          
          # get the segment data frame
          segmentDF <- chartDF[startRow:endRow,]
          
          if(showNames==TRUE) print(segmentName)
          
          # adjust the rows for the segmentDF
          prestimRow <- prestimRow - startRow + 1
          segOnsetRow <- segOnsetRow - startRow + 1
          segEndRow <- segEndRow - startRow
          
          if(getSegment==TRUE) {
            assign("segmentName", eventNames[l], pos=1) 
            assign("segmentDF", segmentDF, pos=1)
            # break
          }
          
        } # end loop over l events 
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
  } # end iteration over i exams
  
  if(output==TRUE) return(examDF)
  
} # end getSegmentFn()

##########################################



x=uniqueExams
showNames=TRUE
output=FALSE



# get a segment instead of iterating through all exams 
getSegment <- TRUE
examNum <- 1
seriesNum <- 1
chartNum <- 1
segmentNum <-4



getSegmentFn(x=uniqueExams, showNames=TRUE, output=FALSE, getSegment=getSegment)



View(segmentDF)


