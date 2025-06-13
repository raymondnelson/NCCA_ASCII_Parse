# new script for pneumo feature extraction
# 
#
#
################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15




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
  
  source('~/Documents/R_programming/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  # source the robustPneumoMeasurement.R script to load 2 needed functions
  # maxPeak()
  # robustPneumoMeasurement()
  source('~/Documents/R_programming/NCCA_ASCII_Parse/robustPneumoMeasurement.R', echo=FALSE)
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    ### add additional columns here
    
    # add the UPneumoExtract and LPneumoExtract columns to the data frame
    examDF$UPneumoExtract <- rep("", times=nrow(examDF))
    examDF$LPneumoExtract <- rep("", times=nrow(examDF))
    examDF$UPneumoMeasure <- rep("", times=nrow(examDF))
    examDF$LPneumoMeasure <- rep("", times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      # get the list of time series data for the charts in the exam
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      seriesOnsetRow <- range(which(examDF$seriesName==seriesName))[1]
      seriesEndRow <- range(which(examDF$seriesName==seriesName))[2]
      
      # seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      # seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      # seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      # uniqueCharts <- as.character(unique(seriesDF$chartName))
      uniqueCharts <- as.character(unique(examDF[seriesOnsetRow:seriesEndRow,'chartName']))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        chartName <- uniqueCharts[k]
        # get the data frame with the time series data for each chart in the series
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- range(which(examDF$seriesName==seriesName & examDF$chartName==chartName))[1]
        # chartOnsetRow <- chartOnsetrow - seriesOnsetRow + 1
        chartEndRow <- range(which(examDF$seriesName==seriesName & examDF$chartName==chartName))[2]
        chartEndRow <- chartEndRow - seriesOnsetRow + 1
        # chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        # chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        # chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        # eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        eventNames <- examDF[chartOnsetRow:(chartEndRow+seriesOnsetRow-1),'eventLabel']
        eventNames <- eventNames[eventNames!=""]

        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB")
        eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        
#         if(round(60 / (mean(diff(maxPeak(chartDF$c_UPneumo, y=40))) / 30), 2) < 12) { 
#           chartDF$UPneumoExtract <- "ONR"
#           chartDF$LPneumoExtract <- "ONR"
#           seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
#           next }
#         if(round(60 / (mean(diff(maxPeak(chartDF$c_UPneumo, y=40))) / 30), 2) > 20) { 
#           chartDF$LPneumoExtract <- "ONR"
#           chartDF$LPneumoExtract <- "ONR"
#           seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
#           next }
        
        # loop over all the events in the chart data frame
        for (l in 1:length(eventNames)) {
          # l=1
          segmentName <- eventNames[l]
          # get the onset row  for the chart DF so that events during the prestimSegment are ignored
          # segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
          segOnsetRow <- which(examDF$seriesName==seriesName & examDF$chartName==chartName & examDF$eventLabel==segmentName)[1]
          # segOnsetRow <- segOnsetRow - chartOnsetRow + 1
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          # set the end row so that the data frame
          segEndRow <- segOnsetRow + measuredSeg * cps - 1
          # if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF) - 1
          if(segEndRow > (chartEndRow+seriesOnsetRow-1)) segEndRow <- chartEndRow - 2
          
          # set the row number for the end of the stimulus segment
          endRow <- segEndRow + addSeg*cps
          # if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
          if(endRow > (chartEndRow+seriesOnsetRow-1)) endRow <- chartEndRow - 1  
            
          # get the segment start row
          startRow <- prestimRow
          
          # get the segment data frame
          #segmentDF <- chartDF[startRow:endRow,]
          startRowE <- startRow+chartOnsetRow+seriesOnsetRow-2
          endRowE <- endRow+chartOnsetRow+seriesOnsetRow-2
          
          segmentDF <- examDF[startRow:endRow,]
          
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
          
          ############################
          
#           begin <- onsetRow
#           end <- offsetRow
#           answer <- answerRow
#           if(answer <= end) answer <- end + 1
          
          # get the responseOnsetRow and responseEndRow
          responseOnsetRow <- segOnsetRow + 1 
          # responseEndRow <- responseOnsetRow + (measuredSeg * cps) - 1 - 2
          responseEndRow <- segEndRow + 1
          
          aBuffOn <- answerRow - (1 * cps) - 1
          if(aBuffOn <= segOnsetRow) aBuffOn <- segOnsetRow + 1
          aBuffOff <- answerRow + (1 * cps) + 1
          if(aBuffOff >= nrow(segmentDF)) aBuffOff <- nrow(segmentDF)
          
          # correct for segments shorter than the measurement segment
          if(responseEndRow > nrow(segmentDF)) {
            responseEndRow <- nrow(segmentDF) - 1
          } 
          
          # add the prestim and end rows to the data frame
#           segmentDF$UPneumoExtract[1] <- "prestimRow"
#           segmentDF$LPneumoExtract[1] <- "prestimRow"
#           segmentDF$UPneumoExtract[endRow] <- "endRow"
#           segmentDF$LPneumoExtract[endRow] <- "endRow"

          # add the stimulus and answer data to the pneumo extract columns
#           chartDF$UPneumoExtract[segOnsetRow+startRow-1] <- "onsetRow"
#           chartDF$LPneumoExtract[segOnsetRow+startRow-1] <- "onsetRow"
#           chartDF$UPneumoExtract[offsetRow+startRow-1] <- "offsetRow"
#           chartDF$LPneumoExtract[offsetRow+startRow-1] <- "offsetRow"
#           chartDF$UPneumoExtract[answerRow+startRow-1] <- "answerRow"
#           chartDF$LPneumoExtract[answerRow+startRow-1] <- "answerRow"
          segmentDF$UPneumoExtract[segOnsetRow] <- "onsetRow"
          segmentDF$LPneumoExtract[segOnsetRow] <- "onsetRow"
          segmentDF$UPneumoExtract[offsetRow] <- "offsetRow"
          segmentDF$LPneumoExtract[offsetRow] <- "offsetRow"
          segmentDF$UPneumoExtract[answerRow] <- "answerRow"
          segmentDF$LPneumoExtract[answerRow] <- "answerRow"
          
          # add the response onset and response end data to the 2 pneumo extract columns
#           chartDF$UPneumoExtract[responseOnsetRow+startRow-1] <- "responseOnsetRow"
#           chartDF$LPneumoExtract[responseOnsetRow+startRow-1] <- "responseOnsetRow"
#           chartDF$UPneumoExtract[responseEndRow+startRow-1] <- "responseEndRow"
#           chartDF$LPneumoExtract[responseEndRow+startRow-1] <- "responseEndRow"
          segmentDF$UPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
          segmentDF$LPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
          segmentDF$UPneumoExtract[responseEndRow] <- "responseEndRow"
          segmentDF$LPneumoExtract[responseEndRow] <- "responseEndRow"
          
          # add the answer distortion buffer 
#           chartDF$UPneumoExtract[aBuffOn+startRow-1] <- "aBuffOn"
#           chartDF$UPneumoExtract[aBuffOff+startRow-1] <- "aBuffOff"
#           chartDF$LPneumoExtract[aBuffOn+startRow-1] <- "aBuffOn"
#           chartDF$LPneumoExtract[aBuffOff+startRow-1] <- "aBuffOff"  
          segmentDF$UPneumoExtract[aBuffOn] <- "aBuffOn"
          segmentDF$UPneumoExtract[aBuffOff] <- "aBuffOff"
          segmentDF$LPneumoExtract[aBuffOn] <- "aBuffOn"
          segmentDF$LPneumoExtract[aBuffOff] <- "aBuffOff"  
          
          ### get the pneumo measurement
          
          # first use the maxPeakPn function in in the robustPenumoMeasurement.R script 
          # to determine the respiration rate
          UPMeasurement <- NA
          if(round(60 / (mean(diff(maxPeakPn(segmentDF$c_UPneumo, y=40))) / 30), 2) < 12) { 
            UPMeasurement <- "ONR_slow"
          }
          if(round(60 / (mean(diff(maxPeakPn(segmentDF$c_UPneumo, y=40))) / 30), 2) > 20) { 
            UPMeasurement <- "ONR_fast"
            # "ONR" signifies "outside the normal range"
          }
          # 
          LPMeasurement <- NA
          if(round(60 / (mean(diff(maxPeakPn(segmentDF$c_LPneumo, y=40))) / 30), 2) < 12) { 
            LPMeasurement <- "ONR_slow"
          }
          if(round(60 / (mean(diff(maxPeakPn(segmentDF$c_LPneumo, y=40))) / 30), 2) > 20) { 
            LPMeasurement <- "ONR_fast"
            # "ONR" signifies "outside the normal range"
          }
          
#           if(str_sub(UPMeasurement,1,3) != "ONR" | str_sub(LPMeasurement,1,3) != "ONR") {
#             UPMeasurement <- "ONR"
#             LPMeasurement <- "ONR"
#           }
          
          if(is.na(UPMeasurement) & is.na(LPMeasurement)) {
            # if(str_sub(UPMeasurement,1,3) != "ONR" & str_sub(LPMeasurement,1,3) != "ONR") {
              UPMeasurement <- robustPneumoMeasurement(dataVector=segmentDF$c_UPneumo[responseOnsetRow:responseEndRow], 
                                                       verbalAnswer=answerRow-responseOnsetRow+1)
              LPMeasurement <- robustPneumoMeasurement(dataVector=segmentDF$c_LPneumo[responseOnsetRow:responseEndRow], 
                                                       verbalAnswer=answerRow-responseOnsetRow+1)
            # }
          }
          
#           if(str_sub(UPMeasurement,1,3) != "ONR") {          
#           }
          
          # add the excursion measurement
          # first make a vector of the measurement rows
#          measureRows <- c((responseOnsetRow:aBuffOn), (aBuffOff:responseEndRow))
# 11-1-2015 commented out these next lines to see if the simpler version works          
#           responseOnsetRow <- responseOnsetRow+startRow-2
#           responseEndRow <- responseOnsetRow+(cps*measuredSeg)-1
#         if(responseEndRow > nrow(chartDF)) responseEndRow <- nrow(chartDF)
#           chartDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             rep((sum(abs(chartDF$c_UPneumoDiff[(measureRows+startRow-1)]))/(measuredSeg-2)), times=(responseEndRow-responseOnsetRow+1))
#           chartDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             rep((sum(abs(chartDF$c_LPneumoDiff[(measureRows+startRow-1)]))/(measuredSeg-2)), times=(responseEndRow-responseOnsetRow+1))

#           chartDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(chartDF$c_UPneumoDiff[(measureRows+startRow-1)])) / (measuredSeg-2) )
#           chartDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(chartDF$c_LPneumoDiff[(measureRows+startRow-1)])) / (measuredSeg-2) )

#           segmentDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(segmentDF$c_UPneumoDiff[(measureRows)])) / (measuredSeg-2) )
#           segmentDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(segmentDF$c_LPneumoDiff[(measureRows)])) / (measuredSeg-2) )
          
#           segmentDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- UPMeasurement
#           segmentDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- LPMeasurement
          segmentDF$UPneumoMeasure[segOnsetRow:segEndRow] <- UPMeasurement
          segmentDF$LPneumoMeasure[segOnsetRow:segEndRow] <- LPMeasurement
          
          
          ###
          
          # save the segmentDF to the chartDF
          # chartDF[startRow:endRow,] <- segmentDF 
          examDF[startRow:endRow,] <- segmentDF 
          
        } # end loop over l events 
        
        # save the chartDF to the seriesDF
        # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        # examDF[(chartOnsetRow+seriesOnsetRow-1):(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over K charts
      
      # save the seriesDF to the examDF
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 

    } # end loop over J series
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i exams 

  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end pneumoExtractFn2

# pneumoExtractFn2(x=uniqueExams, showNames=TRUE, output=FALSE)

