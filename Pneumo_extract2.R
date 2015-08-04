# new script for pneumo data measurement
# 
#
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



###########



# function to compute the sum of absolute differences in y axis exursion
pneumoExtract <- function(x, y) {
  # function to extract the excursion length measurement from pneumo data
  # x = vector of time series data for the measured stimulus segment
  # y = time series vector of events for the measured stimulus segment 
  # including "resonseOnsetRow" "responseEndRow" "aBuffOn" and "aBuffOff"
  responseOnsetRow <- x[which(x=="responseOnsetRow")]
  responseEndRow <- x[which(x=="responseEndRow")]
  aBuffOn <- x[which(x=="aBuffOn")]
  aBuffOf <- x[which(x=="aBuffOff")]
  x <- x[c(responseOnsetRow:aBuffOn, aBuffOff:responseEndRow)]
  return(sum(abs(diff(x))))  
} # end pneumoExtract function

# pneumoExtract()



###########



pneumoExtractFn2 <- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
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
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # add the UPneumoExtract and LPneumoExtract columns to the data frame
    examDF$UPneumoExtract <- rep("", times=nrow(examDF))
    examDF$LPneumoExtract <- rep("", times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
            
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartDF <- examDF[examDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(examDF$chartName==uniqueCharts[k])[1]
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventRows <- chartDF$eventLabel[chartDF$eventLabel!=""]
        
        # loop over all the events in the chart data frame
        # l=1
        for (l in 1:length(eventRows)) {
          segmentName <- eventRows[l]
          prestimRow <- which(chartDF$Label==segmentName)-5*cps
          endRow <- prestimRow+30*cps-1

          segmentDF <- chartDF[prestimiRow:endRow,]
          
          if(showNames==TRUE) print(segmentName)
          
          onsetRow <- which(segmentDF$Events[segmentDF$Events=="onsetRow"][1])
          offsetRow <- which(segmentDF$Events[segmentDF$Events=="offsetRow"][1])
          AnswerRow <- which(segmentDF$Events[segmentDF$Events=="answerRow"][1])
#           responseOnsetRow <- onsetRow+1
#           responseEndRow <- responseOnsetRow+measuredSeg*cps
          
#           aBuffOn
#           aBuffOff
          
          # remove NA rows
          # segmentDF <- na.omit(segmentDF)
          
          # fix problem when answerRow == offsetRow
          if(offsetRow == onsetRow) offsetRow <- onsetRow + 1
          if(answerRow <= endRow) answerRow <- endRow + 1
          
          # get the segment start row
          startRow <- segmentDF$Sample[1]
          
          begin <- onsetRow - startRow + 1
          end <- offsetRow - startRow + 1
          answer <- answerRow - startRow + 1
          if(answer == end) answer <- answer + 1
          
          # get the responseOnsetRow and responseEndRow
          responseOnsetRow <- begin + 1 
#           endRow <- begin + (measuredSeg * cps)
          responseEndRow <- responseOnsetRow + (measuredSeg * cps) 
          
          aBuffOn <- answer - (1 * cps)
          aBuffOff <- answer + (1 * cps)
          
          # correct for segments shorter than the measurement segment
          if(responseEndRow >= nrow(segmentDF)) {
            responseEndRow <- nrow(segmentDF) -1
          }
          
#           # add the UPneumoExtract and LPneumoExtract columns to the data frame
#           segmentDF$UPneumoExtract <- rep("", times=nrow(segmentDF))
#           segmentDF$LPneumoExtract <- rep("", times=nrow(segmentDF))
          
          # add the prestim and end rows to the data frame
          segmentDF$UPneumoExtract[1] <- "prestimRow"
          segmentDF$LPneumoExtract[1] <- "prestimRow"
          segmentDF$UPneumoExtract[endRow] <- "endRow"
          segmentDF$LPneumoExtract[endRow] <- "endRow"

          # add the response onset and response end data to the 2 pneumo columns
          segmentDF$UPneumoExtract[begin] <- "onsetRow"
          segmentDF$LPneumoExtract[begin] <- "onsetRow"
          segmentDF$UPneumoExtract[end] <- "offsetRow"
          segmentDF$LPneumoExtract[end] <- "offsetRow"
          segmentDF$UPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
          segmentDF$LPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
          segmentDF$UPneumoExtract[responseEndRow] <- "responseEndRow"
          segmentDF$LPneumoExtract[responseEndRow] <- "responseEndRow"
          
          # add the answer distortion buffer 
          segmentDF$UPneumoExtract[answer] <- "answerRow"
          segmentDF$LPneumoExtract[answer] <- "answerRow"
          segmentDF$UPneumoExtract[aBuffOn] <- "aBuffOn"
          segmentDF$UPneumoExtract[aBuffOff] <- "aBuffOff"
          segmentDF$LPneumoExtract[aBuffOn] <- "aBuffOn"
          segmentDF$LPneumoExtract[aBuffOff] <- "aBuffOff"  
          
          #
#           segmentList[[j]] <- segmentDF
          
          # save the segmentDF to the chartDF
          chartDF[prestimiRow:endRow,] <- segmentDF

        } # end loop over l events 
        
      } # end loop over K charts

      # save the chartDF to the examDF
      examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
      
    } # end loop over J series
    
  } # end loop over i exams 

  # save the examDF to the global environment 
  assign(paste0(examName, "_Data"), examDF, pos=1)

  if(output==TRUE) return(examDF)
  
} # end pneumoExtractFn2

# pneumoExtractFn2(x=uniqueExams, showNames=TRUE, output=FALSE)

