# pneumo feature extraction
# 4-28-2016
# Raymond Nelson



pneumoExtractFn <- function(x=segmentDF, y=extract.params, tRate=16, aRate=16) {
  # pneumo feature extraction function 
  # is called from the feature extraction function
  # x input is a data frame for a stimulus segment
  # other parameters are obtained from the parent and global environment
  # output is the segmentDF 
  # after extracting the features 
  # and adding the extraction data to the extraction and measurement columns 
  ###
  
  segmentDF <- x
  
  extract.params <- y
  
  stimOnsetRow <- extract.params$onset
  stimOffsetRow <- extract.params$offset
  answerRow <- extract.params$answer
  segEndRow <- extract.params$end
  segmentName <- extract.params$segName
  chartName <- extract.params$chart
  seriesName <- extract.params$series
  examName <- extract.params$exam
  
  UPMeasurement <- tRate
  LPMeasurement <- aRate
  
  ###
  
  # reset the extracted values 
  segmentDF$UPneumoExtract[stimOnsetRow] <- ""
  segmentDF$LPneumoExtract[stimOnsetRow] <- ""
  
  # get the responseOnsetRow and responseEndRow
  responseOnsetRow <- stimOnsetRow + 1 
  responseEndRow <- segEndRow + 1
  
  # buffer around the verbal answer
  aBuffOn <- answerRow - (pneumoAnsBuff * cps) - 1
  if(aBuffOn <= stimOnsetRow) aBuffOn <- stimOnsetRow + 2
  aBuffOff <- answerRow + (pneumoAnsBuff * cps) + 1
  if(aBuffOff >= nrow(segmentDF)) aBuffOff <- nrow(segmentDF)
  
  # correct for segments shorter than the measurement segment
  if(responseEndRow > nrow(segmentDF)) {
    responseEndRow <- nrow(segmentDF) - 1
  } 
  
  # add the stimulus and answer data to the 2 pneumo extract columns
  segmentDF$UPneumoExtract[stimOnsetRow] <- "onsetRow"
  segmentDF$LPneumoExtract[stimOnsetRow] <- "onsetRow"
  segmentDF$UPneumoExtract[stimOffsetRow] <- "offsetRow"
  segmentDF$LPneumoExtract[stimOffsetRow] <- "offsetRow"
  segmentDF$UPneumoExtract[answerRow] <- "answerRow"
  segmentDF$LPneumoExtract[answerRow] <- "answerRow"
  
  # add the response onset and response end data to the 2 pneumo extract columns
  segmentDF$UPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
  segmentDF$LPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
  segmentDF$UPneumoExtract[responseEndRow] <- "responseEndRow"
  segmentDF$LPneumoExtract[responseEndRow] <- "responseEndRow"
  
  # add the answer distortion buffer 
  segmentDF$UPneumoExtract[aBuffOn] <- "aBuffOn"
  segmentDF$UPneumoExtract[aBuffOff] <- "aBuffOff"
  segmentDF$LPneumoExtract[aBuffOn] <- "aBuffOn"
  segmentDF$LPneumoExtract[aBuffOff] <- "aBuffOff"  
  
  ### get the pneumo measurement
  
  # compute the pneumo excursion measurement 
  # only if both upper and lower are not ONR
  if(UPMeasurement=="normal" & LPMeasurement=="normal") {
    UPMeasurement <- pneumoMeasurementFn(dataVector=segmentDF$c_UPneumo[responseOnsetRow:responseEndRow], 
                                         verbalAnswer=answerRow)
    LPMeasurement <- pneumoMeasurementFn(dataVector=segmentDF$c_LPneumo[responseOnsetRow:responseEndRow], 
                                         verbalAnswer=answerRow)
    
    # reduce the scale of the pneumo measurements
    # 2020-07-25
    UPMeasurement <- UPMeasurement / 20
    LPMeasurement <- LPMeasurement / 20
    
  } else {
    UPMeasurement <- "ONR"
    LPMeasurement <- "ONR"
  }

  
  # add the excursion measurement to the data frame
  if(!is.null(UPMeasurement)) {
    segmentDF$UPneumoMeasure[stimOnsetRow:stimOffsetRow] <- UPMeasurement
  }
  if(!is.null(LPMeasurement)) {
    segmentDF$LPneumoMeasure[stimOnsetRow:stimOffsetRow] <- LPMeasurement
  }
  
  # print the results
  print(paste("upper:", UPMeasurement))
  print(paste("lower:", LPMeasurement))
  
  # check the result
  # segmentDF$UPneumoExtract[segmentDF$UPneumoExtract!=""]
  # segmentDF$LPneumoExtract[segmentDF$LPneumoExtract!=""]
  
  return(segmentDF)
  
} # end newPneumoExtractFn()


