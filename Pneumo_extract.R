# Pneumo data measurement
# 
# Y-axis excursion
# sum of absolute differences of each successive sample
# in the measured stimulus segment
#
# sampling rate is 30cps
# x input is a vector of time series data
# x length is from stimulus onset to 15 seconds after stimulus onset
# measurement is the sum of absolute differences between successive samples 
#
################################


cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15


###########


# function to compute the sum of absolute diffs in y axis exursion
pneumoExtract <- function(x, y) {
  # function to extract the excursion length measurement from pneumo data
  # x = vector of time series data for the measured stimulus segment
  # y = time series vector of events for the measured stimulus segment 
  # including "resonseOnsetRow" "responseEndRow" "aBuffOn" and "aBuffOff"
  responseOnsetRow <- x[which(x=="responseOnsetRow")]
  responseEndRow <- x[which(x=="responseEndRow")]
  aBuffOn <- x[which(x=="aBuffOn")]
  aBuffOf <- x[which(x=="aBuffOff")]
  x <- x[c(responseOnsetRow:aBuffOn, responseEndRow:aBuffOff)]
  #
  return(sum(abs(diff(x))))  
} # end pneumoExtract function

# pneumoExtract()


#############

# # Pneumo measurement
# P1 <- sum(diff(stimSegment$LPneumo[150:600]))
# P2 <- sum(diff(stimSegment$UPneumo[150:600]))
# print(c("P1", P1))
# print(c("P2", P2))


##############


mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")

# mySegmentLists <- mySegmentLists[6]
# myEventLists <- myEventLists[6]


###############



# function to iterate over a list of exams and add the measurement columns
pneumoExtractFn <- function(x=mySegmentLists, y=myEventLists) {
  # function to iterate over a list of exams, series, charts and stimulus segments
  # and add UPneumoExtract and LPneumoExtract column to the time series data frame
  #
  # x is a vector of names of lists that contain the data frames
  # for the time series data for the stimulus segments
  #
  # y is a vector of names of lists that contain the data frames of
  # event row numbers for the stimulus segments
  # 
  # output is the saved lists that include the UPneumoExtract and LPneumoExtract columns
  #
  ########
  
  # vectors of names
  mySegmentLists <- x
  myEventLists <- y  
  
  ###
  
  # iterate over the segment lists
  # i <- 1
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
    # j <- 1
    for (j in 1:length(segmentNames)) {  
      
      segmentDF <- segmentList[[j]]
      eventDF <- eventList[[j]]
      
      # remove NA rows
      # segmentDF <- na.omit(segmentDF)
      
      # get the segment start row
      startRow <- segmentDF$Sample[1]
      
      begin <- eventDF$Begin[1]
      end <- eventDF$End[1]
      answer <- eventDF$Answer[1]
      
      
      # get the responseOnsetRow and responseEndRow
      responseOnsetRow <- begin - (startRow - 1)
      responseEndRow <- responseOnsetRow + (measuredSeg * cps)
      
      aBuffOn <- eventDF$Answer - (startRow - 1) - (1 * cps)
      aBuffOff <- eventDF$Answer - (startRow - 1) + (1 * cps)
      
      # correct for segments shorter than the measurement segment
      if(responseEndRow > nrow(segmentDF)) {
        responseEndRow <- nrow(segmentDF)
      }
      
      # add the UPneumoExtract and LPneumoExtract columns to the data frame
      segmentDF$UPneumoExtract <- rep("", times=nrow(segmentDF))
      segmentDF$LPneumoExtract <- rep("", times=nrow(segmentDF))
      
      # add the response onset and response end data to the 2 pneumo columns
      segmentDF$UPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
      segmentDF$LPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
      segmentDF$UPneumoExtract[responseEndRow] <- "responseEndRow"
      segmentDF$LPneumoExtract[responseEndRow] <- "responseEndRow"
      
      # add the answer distortion buffer 
      segmentDF$UPneumoExtract[aBuffOn] <- "aBuffOn"
      segmentDF$UPneumoExtract[aBuffOff] <- "aBuffOff"
      segmentDF$LPneumoExtract[aBuffOn] <- "aBuffOn"
      segmentDF$LPneumoExtract[aBuffOff] <- "aBuffOff"        
      
      segmentList[[j]] <- segmentDF
      
    } # end iteration over data frames in each list
    
    names(segmentList) <- segmentNames
    
    assign(segmentListName, segmentList, pos=1)
    
  } # end iteration over the segment lists
  
} # end pneumoExtractFn()

pneumoExtractFn(x=mySegmentLists, y=myEventLists)
  
  

