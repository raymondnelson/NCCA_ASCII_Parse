# get charts
# 
# sampling rate is 30cps
# x input is a vector of time series data
# x length is from stimulus onset to 15 seconds after stimulus onset
# measurement is the sum of absolute differences between successive samples 
#
################################



library(stringr)



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15



##############



featureExtractFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
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
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    ### add additional columns here
    
    # get the names of all unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=2
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartDF <- examDF[examDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(examDF$chartName==uniqueCharts[k])[1]
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        
        # loop over all the events in the chart data frame
        # l=4
        for (l in 1:length(eventNames)) {
          segmentName <- eventNames[l]
          
          # get the onset row  for the chart DF so that events during the prestimSegment are ignored
          segOnsetRow <- which(chartDF$Label==segmentName)[1]
          
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          
          # set the end row so that the data frame
          endRow <- segOnsetRow + measuredSeg*cps-1
          if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
          
          # set the row number for the end of the stimulus segment
          segEndRow <- endRow + 10*cps
          
          # get the segment data frame
          segmentDF <- chartDF[prestimRow:segEndRow,]
          
          # get the segment start row
          startRow <- segmentDF$Sample[1]
          
          if(showNames==TRUE) print(segmentName)
          
          ###
          
          # adjust the segOnsetRow for the segmentDF
          segOnsetRow <- segOnsetRow - startRow + 1
          endRow <- endRow <- endRow - endRow + 1
          segEndRow <- segEndRow - startRow + 1
          
          #####
          
          
          
          #####
          
          # save the segmentDF to the chartDF
          chartDF[prestimRow:segEndRow,] <- segmentDF
          
        } # end loop over l events 
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end iteration over j series data frames
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(output==TRUE) return(examDF)
  
} # end featureExtractFn()

featureExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE)


