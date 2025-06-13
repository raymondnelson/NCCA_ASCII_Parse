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



cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15



##############



source('~/Documents/R_programming/NCCA_ASCII_Parse/addCols.R')
newColNames <- readLines('~/Documents/R_programming/NCCA_ASCII_Parse/TukeyFences.R')



artifactColsFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
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
    
    examDF <- addCols(x=examDF, y=newColNames)
    
    # get the names of all unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        
        # loop over all the events in the chart data frame
        # l=11
        for (l in 1:length(eventNames)) {
          segmentName <- eventNames[l]
          # get the onset row  for the chart DF so that events during the prestimSegment are ignored
          segOnsetRow <- which(chartDF$Label==segmentName)[1]
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          # set the end row so that the data frame
          segEndRow <- segOnsetRow + measuredSeg*cps
          if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
          # set the row number for the end of the stimulus segment
          endRow <- segEndRow + 10*cps -1
          if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
          # get the segment data frame
          segmentDF <- chartDF[prestimRow:endRow,]
          
          if(showNames==TRUE) print(segmentName)
          
          # get the segment start row
          startRow <- prestimRow
          
          # adjust the rows for the segmentDF
          # prestimRow <- prestimRow - startRow + 1
          # segEndRow <- segEndRow - startRow + 1
          # endRow <- endRow <- endRow - startRow + 1
          
          #####
          
          # need a function to process the activity sensor data 
          
          #####
          
          # save the segmentDF to the chartDF
          chartDF[startRow:endRow,] <- segmentDF
          
        } # end loop over l events 
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(chartOnsetRow+nrow(chartDF)-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end iteration over j series data frames
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1) 
    
  } # end iteration over i exams
  
  if(output==TRUE) return(examDF)
  
} # end artifactColsFn()

artifactColsFn(x=uniqueExams, showNames=TRUE, output=FALSE)

# examDF2 <- PF090316_Data[which(PF090316_Data$chartName=="1_02A"),]



