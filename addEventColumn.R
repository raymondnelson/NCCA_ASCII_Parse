# prepare a data frame for the segmentPlot function

# uses the _eventList and _dataSegmentList

# add an events column
# also add measurement columns for each sensor 

# for PLE pulse width make a shaded box indicating the mean pulsewidth
# for the length of the prestim and measurement

###############################

cps <- 30
prestimSeg <- 5
EDALat <- .5
ROWEnd <- 5
measuredSeg <- 15


# myEventList <- PF090316_1.01A_eventList
# mySegmentList <- PF090316_1.01A_dataSegmentList
# 
# myEventList1 <- PF090316_1.01A_eventList[[1]]
# mySegmentList1 <- PF090316_1.01A_dataSegmentList[[1]]



mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")


####

# make a function to add the events to each data frame
addEvents <- function(x=segmentDF, y=eventDF) {
  # function to add an events column to the data frame for all measured segments
  # can be called recursively for the list of segments for each unique series and exam
  # x is a data frame of the time series data for a stimulus presentation
  # y is a data fram of 1 row with the question label
  # along with the stimulus begin end and answer row
  # returnOut parameter will return the last segment
  
  segmentDF <- x
  eventDF <- y
  
  # event labels
  uniqueLabels <- unique(segmentDF$Label)
  uniqueLabels <- uniqueLabels[uniqueLabels!=""]
  
  # onset sample row for the segment
  startRow <- segmentDF$Sample[1]
  
  #     # these are the same as 
  #     min(which(segmentDF$Label==eventDF$Label)) + (startRow - 1)
  #     eventDF$Begin
  #     max(which(segmentDF$Label==eventDF$Label)) + (segmentDF$Sample[1] - 1)
  #     eventDF$End
  #     which(segmentDF$Label==uniqueLabels[2])
  #     eventDF$Answer - (startRow-1)
  
  ###
  
  # add the Events column to the data frame
  segmentDF$Events <- rep("", times=nrow(segmentDF))
  
  prestimRow <- eventDF$Begin - (startRow-1) - (cps*prestimSeg)
  # correction for X announcement for which there may not be 5 seconds prestimulus
  if(prestimRow < 1) prestimRow <- 1
  onsetRow <- eventDF$Begin - (startRow-1)
  #EDALatRow <- onsetRow + cps*EDALat
  offsetRow <- eventDF$End - (startRow-1)
  # correction if offsetRow==onsetRow
  if(offsetRow==onsetRow) offsetRow <- offsetRow+1
  answerRow <- eventDF$Answer - (startRow-1)
  # correction if there is no answer (answer row will be the same as offsetRow)
  if(answerRow==offsetRow) answerRow <- answerRow+1
  if(answerRow==onsetRow) answerRow <- answerRow+2
  # ROWEndRow <- answerRow + (cps*ROWEnd)
  endRow <- onsetRow + (cps*measuredSeg)
  
  # added to correct for events longer than the last segment
  # other corrections for short segments
  if(onsetRow > nrow(segmentDF)) onsetRow <- (nrow(segmentDF)-6)
  # if(EDALatRow > nrow(segmentDF)) EDALatRow <- (nrow(segmentDF)-5)
  if(offsetRow > nrow(segmentDF)) offsetRow <- (nrow(segmentDF)-4)
  if(answerRow > nrow(segmentDF)) answerRow <- (nrow(segmentDF)-3)
  # if(ROWEndRow > nrow(segmentDF)) ROWEndRow <- (nrow(segmentDF)-2)
  if(endRow > nrow(segmentDF)) endRow <- (nrow(segmentDF)-1)
  
  segmentDF$Events[prestimRow] <- "prestimRow"
  segmentDF$Events[onsetRow] <- "onsetRow"
  # segmentDF$Events[EDALatRow] <- "EDALatRow"
  segmentDF$Events[offsetRow] <- "offsetRow"
  segmentDF$Events[answerRow] <- "answerRow"
  # segmentDF$Events[ROWEndRow] <- "ROWEndRow"
  segmentDF$Events[endRow] <- "endRow"
  
  return(segmentDF)
  
} # end addEvents function
# addEvents(x=segmentDF, y=eventDF)

########################################

  
#####  

addEventsF <- function(x=mySegmentLists, y=myEventLists) {
  # function to loop the addEvents function over all _eventLists and _dataSegmentLists
  # in the global environment
  # x is a vector of names of all _dataSegmentList lists
  # y is a vector of names of all _eventList lists
  # this function will get each list and an Events column to the measurement segments
  # the Events column can be used for plotting and measurement
  #
#   ####
#   
#   # make a function to add the events to each data frame
#   addEvents <- function(x=segmentDF, y=eventDF) {
#     # function to add an events column to the data frame for all measured segments
#     # can be called recursively for the list of segments for each unique series and exam
#     # x is a data frame of the time series data for a stimulus presentation
#     # y is a data fram of 1 row with the question label
#     # along with the stimulus begin end and answer row
#     # returnOut parameter will return the last segment
#     
#     segmentDF <- x
#     eventList <- y
#     
#     # event labels
#     uniqueLabels <- unique(segmentDF$Label)
#     uniqueLabels <- uniqueLabels[uniqueLabels!=""]
#     
#     # onset sample row for the segment
#     startRow <- segmentDF$Sample[1]
#     
# #     # these are the same as 
# #     min(which(segmentDF$Label==eventDF$Label)) + (startRow - 1)
# #     eventDF$Begin
# #     max(which(segmentDF$Label==eventDF$Label)) + (segmentDF$Sample[1] - 1)
# #     eventDF$End
# #     which(segmentDF$Label==uniqueLabels[2])
# #     eventDF$Answer - (startRow-1)
#     
#     ###
#     
#     # add the Events column to the data frame
#     segmentDF$Events <- rep("", times=nrow(segmentDF))
#     
#     prestimRow <- eventDF$Begin - (startRow-1) - (cps*prestimSeg)
#     # correction for X announcement for which there may not be 5 seconds prestimulus
#     if(prestimRow < 1) prestimRow <- 1
#     onsetRow <- eventDF$Begin - (startRow-1)
#     #EDALatRow <- onsetRow + cps*EDALat
#     offsetRow <- eventDF$End - (startRow-1)
#     # correction if offsetRow==onsetRow
#     if(offsetRow==onsetRow) offsetRow <- offsetRow+1
#     answerRow <- eventDF$Answer - (startRow-1)
#     # correction if there is no answer (answer row will be the same as offsetRow)
#     if(answerRow==offsetRow) answerRow <- answerRow+1
#     if(answerRow==onsetRow) answerRow <- answerRow+2
#     # ROWEndRow <- answerRow + (cps*ROWEnd)
#     endRow <- onsetRow + (cps*measuredSeg)
#     
#     # added to correct for events longer than the last segment
#     # other corrections for short segments
#     if(onsetRow > nrow(segmentDF)) onsetRow <- (nrow(segmentDF)-6)
#     # if(EDALatRow > nrow(segmentDF)) EDALatRow <- (nrow(segmentDF)-5)
#     if(offsetRow > nrow(segmentDF)) offsetRow <- (nrow(segmentDF)-4)
#     if(answerRow > nrow(segmentDF)) answerRow <- (nrow(segmentDF)-3)
#     # if(ROWEndRow > nrow(segmentDF)) ROWEndRow <- (nrow(segmentDF)-2)
#     if(endRow > nrow(segmentDF)) endRow <- (nrow(segmentDF)-1)
#     
#     segmentDF$Events[prestimRow] <- "prestimRow"
#     segmentDF$Events[onsetRow] <- "onsetRow"
#     # segmentDF$Events[EDALatRow] <- "EDALatRow"
#     segmentDF$Events[offsetRow] <- "offsetRow"
#     segmentDF$Events[answerRow] <- "answerRow"
#     # segmentDF$Events[ROWEndRow] <- "ROWEndRow"
#     segmentDF$Events[endRow] <- "endRow"
#     
#     return(segmentDF)
#     
#   } # end addEvents function
#   # addEvents(x=segmentDF, y=eventDF)
#   
#   ########################################
  
  segmentList1 <- x
  eventList1 <- y  
    
  ###
  
  # iterate over the segment lists  
  # i <- 1
  for (i in 1:length(segmentList1)) {
  
    segmentListName <- segmentList1[i]
    eventListName <- eventList1[i]
    
    segmentList <- get(segmentListName, pos=1)
    eventList <- get(eventListName, pos=1)
    
    segmentNames <- names(segmentList)
    
    # iterate over the data frames in each list
    # j <- 2
    for (j in 1:length(segmentList)) {  
      
      segmentDF <- segmentList[[j]]
      eventDF <- eventList[[j]]
      
#       # remove NA Rows
#       segmentDF <- na.omit(segmentDF)
      
      z <- addEvents(x=segmentDF, y=eventDF)

      segmentList[[j]] <- z

    } # end iteration over data frames in each list

    names(segmentList) <- segmentNames
    
    assign(segmentListName, segmentList, pos=1)
    
  } # end iteration over the segment lists

} # end addEventsF()

###

addEventsF(x=mySegmentLists, y=myEventLists)

