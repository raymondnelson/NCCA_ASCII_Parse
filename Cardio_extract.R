# function to extract onset and end of cardio responses

# uses the amplitudeExtract.R script with the amplitudeExtract() function

########################



source('~/Documents/R_programming/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)



cps <- 30
prestimSeg <- 5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15



library(stringr)



mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")

# mySegmentLists <- mySegmentLists[1]
# myEventLists <- myEventLists[1]



#####



cardioExtract <- function(x=mySegmentLists, y=myEventLists) {
  # function to locate the responseOnset and responseEnd
  # for the time series cardio data of each stimulus segment
  #
  # x is a vector of names of data segment lists  in the cwd
  # ending in "_dataSegmentList"
  # y is a vector of names of event lists in the cwd
  # ending in "_eventList"
  # examinations 
  #
  ####  
  
  # vectors of names
  mySegmentLists <- x
  myEventLists <- y  
  
    # iterate over the segment lists
  # i=1
  for (i in 1:length(mySegmentLists)) {
    
    # get the names
    segmentListName <- mySegmentLists[i]
    eventListName <- myEventLists[i]
    
    # get a single list of segment data frames
    segmentList <- get(segmentListName, pos=1)
    eventList <- get(eventListName, pos=1)
    
    # save the names of the stimulus segments
    segmentNames <- names(segmentList)
    
    # iterate over the data frames in each list
    # j=1
    for (j in 1:length(segmentNames)) {  
      
      segmentDF <- segmentList[[j]]
      eventDF <- eventList[[j]]
      
      # fix problem when answerRow == offsetRow
      if(eventDF$End == eventDF$Begin) eventDF$End <- eventDF$Begin + 1
      if(eventDF$Answer <= eventDF$End) eventDF$Answer <- eventDF$End + 1
      
#       # remove NA rows
#       segmentDF <- na.omit(segmentDF)
      
      # get the segment start row
#       startRow <- segmentDF$Sample[1]
#       
#       # get the responseOnsetRow and responseEndRow
#       label <- eventDF$Label
#       onsetRow <- eventDF$Begin - (startRow - 1)
#       offsetRow <- eventDF$End - (startRow - 1)
#       answerRow <- eventDF$Answer - (startRow - 1)
#       ROWEndRow <- eventDF$Answer - (startRow - 1) + (ROWEnd * cps)
#       endRow <- onsetRow + (measuredSeg * cps)
#       name <- paste(segmentDF$examName[1],
#                     segmentDF$chartName[1],
#                     eventDF$Label,
#                     sep="_")
      
#       # correct for segments shorter than the measurement segment
#       if(EndRow > nrow(segmentDF)) {
#         EndRow <- nrow(segmentDF)
#       }

      CardioData <- segmentDF$c_CardioMA
      
      ####
      # col 17 = diastolic, 18=systolic, 19=minmax, 20=moving average
      # ampExt <- amplitudeExtract(x=segmentDF, y=eventDF, column=20)
#       ampExt <- amplitudeExtract(x=CardioData, 
#                                  begin=onsetRow, 
#                                  end=offsetRow, 
#                                  answer=answerRow,
#                                  start=startRow,
#                                  lat=CardioLat,
#                                  label=label,
#                                  segmentName=name)
      
      ampExt <- amplitudeExtract(x=CardioData,
                                 begin=eventDF$Begin, 
                                 end=eventDF$End, 
                                 answer=eventDF$Answer,
                                 start=segmentDF$Sample[1],
                                 lat=EDALat,
                                 label=eventDF$Label,
                                 segmentName=paste(segmentDF$examName[1],
                                                   segmentDF$chartName[1],
                                                   eventDF$Label,
                                                   sep="_"),
                                 nSmooth=4)


      responseOnsetRow <- as.numeric(ampExt[1])
      responseEndRow <- as.numeric(ampExt[2])
      stopRow <- as.numeric(ampExt["stopRow"])
      
      # add the cardioExtract column to the data frame
      segmentDF$CardioExtract <- rep("", times=nrow(segmentDF))

      ###
      
      # make sure that events are on distinct rows
#       prestimRow <- 1
      onsetRow <- eventDF$Begin - segmentDF$Sample[1] + 1 
      offsetRow <- eventDF$End - segmentDF$Sample[1] + 1 
      answerRow <- eventDF$Answer - segmentDF$Sample[1] + 1 
      latencyRow <- eventDF$Begin - segmentDF$Sample[1] + 1 + CardioLat * cps
      # reponseOnsetRow 
      # responseEndRow
      ROWEndRow <- eventDF$Answer - segmentDF$Sample[1] + 1 + (ROWEnd * cps)
      endRow <- eventDF$Begin - segmentDF$Sample[1] + 1 + (measuredSeg * cps)
      #
      events <- as.numeric(c(onsetRow, 
                    offsetRow, 
                    answerRow, 
                    latencyRow, 
                    ROWEndRow,
                    responseOnsetRow, 
                    responseEndRow,
                    endRow,
                    stopRow))
      names(events) <- c("onsetRow", 
                           "offsetRow", 
                           "answerRow", 
                           "latencyRow", 
                           "ROWEndRow", 
                           "responseOnsetRow",
                           "responseEndRow",
                           "endRow",
                         "stopRow")
      ##
      for(i in 9:5) {if(events[i-1] == events[i]) events[i-1] <- events[i-1] - 1}
      for(i in 1:3) {if(events[i+1] == events[i]) events[i+1] <- events[i+1] + 1}
      ##
#       segmentDF$CardioExtract[events["prestimRow"]] <- "prestimRow"
      segmentDF$CardioExtract[events["onsetRow"]] <- "onsetRow"
      segmentDF$CardioExtract[events["offsetRow"]] <- "offsetRow"
      segmentDF$CardioExtract[events["answerRow"]] <- "answerRow"
      segmentDF$CardioExtract[events["ROWEndRow"]] <- "ROWEndRow"
      segmentDF$CardioExtract[events["latencyRow"]] <- "latencyRow"
      segmentDF$CardioExtract[events["responseOnsetRow"]] <- "responseOnsetRow"
      segmentDF$CardioExtract[events["responseEndRow"]] <- "responseEndRow"
      segmentDF$CardioExtract[events["endRow"]] <- "endRow"
      segmentDF$CardioExtract[events["stopRow"]] <- "stopRow"

      ####
      
      segmentList[[j]] <- segmentDF
      
    } # end for loop iteration over data frames in each list
    
    names(segmentList) <- segmentNames
    
    assign(segmentListName, segmentList, pos=1)
    
  } # end iteration over segment lists
  
} # end cardioExtract function

cardioExtract(x=mySegmentLists, y=myEventLists)



