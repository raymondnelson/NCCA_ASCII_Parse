# new script for cardio feature extraction

# copied from the cardio extract script 9-19-2015


# 
#
#
################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[8]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15



###########




PLEExtractFn <- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # function to iterate over a vector of data frame names 
  # and add UPneumoArtifacts and LPneumoArtifacts column to the time series data frame
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  ########
  
  source('~/dropbox/R_programming/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  # source the helper function
  source('~/dropbox/R_programming/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
  ########
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # skip the rest and go to the next chart if there is no PLE column in the data frame
    if(sum(pmatch(names(examDF), "c_PL", nomatch=0))==0) next
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    ### add additional columns here
    examDF$PLEExtract <- rep("", times=nrow(examDF))
    examDF$PLEMeans <- rep("", times=nrow(examDF))
    examDF$PLEMeasure <- rep("", times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=3
        chartName <- uniqueCharts[k]
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB")
        eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        
        # stop if pulse rate is outside the normal range
        if(round(60 / (mean(diff(maxPeak(chartDF$c_Cardio1, y=8))) / 30), 0) < 60) { 
          chartDF$PLEExtract <- "ONR"
          seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
          next }
        if(round(60 / (mean(diff(maxPeak(chartDF$c_Cardio1, y=8))) / 30), 0) > 100) { 
          chartDF$PLEExtract <- "ONR"
          seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
          break }
        
        # loop over all the events in the chart data frame
        for (l in 1:length(eventNames)) {
          # l=1
          segmentName <- eventNames[l]
          # get the onset row  for the chartDF so that events during the prestimSegment are ignored
          segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          # set the end row so that the data frame
          segEndRow <- segOnsetRow + measuredSeg*cps - 1
          if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
          # set the row number for the end of the stimulus segment
          endRow <- segEndRow + addSeg*cps
          if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
          
          # get the segment start row
          startRow <- prestimRow
          
          # get the segment data frame
          segmentDF <- chartDF[startRow:endRow,]
          
          if(showNames==TRUE) print(segmentName)
          
          #####
          
          # adjust the onsetRow for the segmentDF
          segOnsetRow <- segOnsetRow - startRow + 1
          segEndRow <- segEndRow - startRow + 1 # added 9-21-2015
          
          # onsetRow <- which(segmentDF$Events=="onsetRow")[1]
          # get the first offset after the onset row
          offsetRow <- which(segmentDF$Events[segOnsetRow:nrow(segmentDF)]=="offsetRow")[1] + segOnsetRow - 1
          answerRow <- which(segmentDF$Events[segOnsetRow:nrow(segmentDF)]=="answerRow")[1] + segOnsetRow - 1
          
          # remove NA rows
          # segmentDF <- na.omit(segmentDF)
          
          # fix problem when answerRow == offsetRow
          if(offsetRow == segOnsetRow) offsetRow <- segOnsetRow + 1
          if(answerRow <= offsetRow) answerRow <- offsetRow + 1
          if(offsetRow >= nrow(segmentDF)) offsetRow <- nrow(segmentDF) - 2
          if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
          
          ##################################
          
          prestimSegOnset <- segOnsetRow - ((3*cps)+1)
          if(prestimSegOnset < 1) prestimSegOnset <- 1
          prestimSegOffset <- segOnsetRow - 1
          if(prestimSegOffset < 1) prestimSegOffset <- 1
          poststimSegOnset <- segOnsetRow + ((5*cps))
          if(poststimSegOnset > nrow(segmentDF)) poststimSegOnset <- nrow(segmentDF)
          poststimSegOffset <- segOnsetRow + ((10*cps-1))
          if(poststimSegOffset > nrow(segmentDF)) poststimSegOffset <- nrow(segmentDF)
          
          segmentDF$PLEExtract[prestimSegOnset] <- "prestimSegOnset"
          segmentDF$PLEExtract[prestimSegOffset] <- "prestimSegOffset"
              
          segmentDF$PLEExtract[segOnsetRow] <- "onsetRow"
          segmentDF$PLEExtract[offsetRow] <- "offsetRow"
          segmentDF$PLEExtract[answerRow] <- "answerRow"
          
          segmentDF$PLEExtract[poststimSegOnset] <- "poststimSegOnset"
          segmentDF$PLEExtract[poststimSegOffset] <- "poststimSegOffset"    
          
          ###### get the PLE response measurement
          
          PLEList <- PLEMeasurement(dataVector=segmentDF$c_PL, stimulusOnset=segOnsetRow)
          
          segmentDF$PLEMeans[(prestimSegOnset:prestimSegOffset)] <- PLEList$prestimMeanAmp
          segmentDF$PLEMeans[(poststimSegOnset:poststimSegOffset)] <- PLEList$poststimMeanAmp          
          
          # and finally get the PLE change measurement and add it to the data frame
          ifelse(PLEList$prePostRatio < 1,
            segmentDF$PLEMeasure[(segOnsetRow:segEndRow)] <- PLEList$prePostRatio,
            segmentDF$PLEMeasure[(segOnsetRow:segEndRow)] <- NA
          )
                    
          ##################################
          
          # save the segmentDF to the chartDF
          chartDF[startRow:endRow,] <- segmentDF
          
        } # end loop over l events 
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over K charts
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over J series
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end PLEExtractFn() function

# PLEExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE)


