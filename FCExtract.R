# Cardio feature extraction
# 4/27/2016
# Raymond Nelson


newFCExtractFn <- function(x=segmentDF, onset, offset, answer, end, segName, chart, series, exam) {
  # cardio feature extraction function is called from the feature extraction function
  # x input is a data frame for a stimulus segment
  # other parameters are obtained from the parent and global environment
  # output is the segmentDF 
  # after extracting the features and adding the extraction data to the extraction and measurement columns 
  ###
  
  segmentDF <- x
  stimOnsetRow <- onset
  stimOffsetRow <- offset
  answerRow <- answer
  stimEndRow <- end
  segEndRow <- end
  segmentName <- segName
  chartName <- chart
  seriesName <- series
  examName <- exam
  
  dataVector <- switch(FCLine,
                       "ma" = segmentDF$c_FCMA,
                       "diastolic" = segmentDF$c_FCDiastolic,
                       "systolic" = segmentDF$c_FCSystolic,
                       "mid" = segmentDF$c_FCMid,
                       "otherwise: last" )
  
  extractList <- as.list(c(stimOnsetRow,
                           stimOffsetRow,
                           answerRow,
                           segmentName,
                           paste(examName, seriesName, chartName, segmentName, sep="_")) )
  
  extractList[[6]] <- dataVector
  
  names(extractList) <- c("begin",
                          "end",
                          "answer",
                          "segmentName",
                          "segmentTitle",
                          "dataVector" )
  
  ############
  
  ### use the amplitude extractFunction to compute the cardio response
  extractResult <- amplitudeExtractFn(extractList=extractList,
                                      dataRate=cps,
                                      Lat=CardioLat,
                                      ROWEnd=ROWEnd,
                                      nSmooth=ignore,
                                      strictWindow=EW,
                                      strictROW=ROW,
                                      descentStop=descentRule,
                                      slopeChange=slopeChangeRule)
  
  # extractResult is a named character vector with 7 items:
  # responseOnsetRow, responsePeakRow, responseOnsetValue, responsePeakValue,
  # responseChangeValue, stopRow, segmentTitle
  
  # print(extractResult)
  
  # process the ouput of the amplitudeExctract function
  responseOnsetRow <- as.numeric(extractResult["responseOnsetRow"])
  responseEndRow <- as.numeric(extractResult["responsePeakRow"])
  stopRow <- as.numeric(extractResult["stopRow"])
  responseChange <- as.numeric(extractResult["responseChangeValue"])
  
  # make sure that events are on distinct rows
  # onsetRow <- segOnsetRow
  latencyRow <- stimOnsetRow + (CardioLat * cps)
  ROWEndRow <- answerRow + (ROWEnd * cps)
  if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3
  
  # use the stimEndRow to remove responses at the end of the segment
  if(responseOnsetRow == (stimEndRow - 1 )) {
    responseOnsetRow <- NULL
    responseEndRow <- NULL
    stopRow <- NULL
    responseChange <- NULL
  }
  
  # construct a named vector to hold the results
  events <- as.numeric(c(stimOnsetRow,
                         stimOffsetRow,
                         answerRow,
                         latencyRow,
                         ROWEndRow,
                         segEndRow,
                         responseOnsetRow,
                         responseEndRow,
                         stopRow,
                         responseChange))
  
  # name items in the events vector
  ifelse(length(events)==10,
         names(events) <- c("onsetRow",
                            "offsetRow",
                            "answerRow",
                            "latencyRow",
                            "ROWEndRow",
                            "segEndRow",
                            "responseOnsetRow",
                            "responseEndRow",
                            "stopRow",
                            "responseChange"),
         names(events) <- c("onsetRow",
                            "offsetRow",
                            "answerRow",
                            "latencyRow",
                            "ROWEndRow",
                            "segEndRow"))
  
  segmentDF$FCExtract[events["onsetRow"]] <- "onsetRow"
  segmentDF$FCExtract[events["offsetRow"]] <- "offsetRow"
  segmentDF$FCExtract[events["answerRow"]] <- "answerRow"
  if(!is.null(responseOnsetRow)) {
    segmentDF$FCExtract[events["responseOnsetRow"]] <- "responseOnsetRow"
    segmentDF$FCExtract[events["responseEndRow"]] <- "responseEndRow"
  }
  if(!is.null(responseChange)) {
    segmentDF$FCMeasure[stimOnsetRow:stimOffsetRow] <- responseChange
  }
  
  # check the result
  # segmentDF$FCExtract[segmentDF$CardioExtract!=""]
  
  print(events)
  
  return(segmentDF)
  
} # end newFCExtractFn()



