# EDA extract
#
# function to recursively apply the amplitudeExtract function
# to all lists of stimulus segment data frames int he cwd
#
# input is a list of time series data frames 
# and a list of event data frames
#
# event data frames include the event label
# along with begin, end and anser row
#
# time series data frames include a zero centered vector
# for each sensor
#
# output is a data frame of the time series vectors
# along with an additional column indicating the 
# response onset and response peak row for each stmulus event 
#
# difference between onset and peak are 
# indicative of a realtive increase in conductance
# or relative decrease in resistance in response to the stimulus
#
# EDA measurement
# ROW is from stim onset to 5 seconds after verbal response
# EDA latency is .5 seconds after stimulus onset
# measurement periodis 15 seconds
# sampling rate is 30cps
#
# This script includes 2 functions
#
# EDAExtract() to compute the response onset and peak
# and add the measurement column to the stimulus segment data frame 
#
# EDAExtractFn() to iterate the EDAExtract function
# over a list of exams, series, charts, and stimulus segments
#
########################################


cps <- 30
prestimSeg <- 5
EDALat <- .5
ROWEnd <- 5
measuredSeg <- 15



source('~/Documents/R_programming/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)



mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")

# mySegmentLists <- mySegmentLists[1:3]
# myEventLists <- myEventLists[1:3]


#################### 

# make a function to iterate the amplitudeExtract or EDAExtract function 
# over a list of exams, series, charts, and stimulus segments 

EDAExtractFn <- function(x=mySegmentLists, y=myEventLists) {
  # function to iterate the EDAExtract function over a list of measurement segments
  # using a list of Begin End and Answer events to take a response measurement
  #
  # x is a vector of names of all _dataSegmentList lists
  # y is a vector of names of all _eventList lists
  #
  # this function will get each list and add an AutoEDAExtract column 
  # to the measurement segments
  # the Events column can be used for plotting and measurement
  #
  #############
  
  
  
  ############# 
  
  # first make a function to add the AutoEDAExtract column to the data frame
  
  # source('~/Documents/R_programming/NCCA_ASCII_Parse/EDAExtract.R', echo=TRUE)

  ##################################
  
  # vectors of names
  mySegmentLists <- x
  myEventLists <- y  
  
  ###
  
  # iterate over the segment lists
  # i<-1
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
    # j<-13
    for (j in 1:length(segmentNames)) {  
      
      #### section using amplitudeExtract()
      
      segmentDF <- segmentList[[j]]
      eventDF <- eventList[[j]]
      
      # fix problem when answerRow == offsetRow
      if(eventDF$End == eventDF$Begin) eventDF$End <- eventDF$Begin + 1
      if(eventDF$Answer <= eventDF$End) eventDF$Answer <- eventDF$End + 1
      
      EDAData <- segmentDF$c_AutoEDA
      
      # columns 9=manualEDA, column 16=AutoEDA
      # z <- amplitudeExtract(x=segmentDF[,9], y=eventDF, column=16)
      z <- amplitudeExtract(x=EDAData,
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
      
      responseOnsetRow <- as.numeric(z[1])
      responseEndRow <- as.numeric(z[2])
      stopRow <- as.numeric(z["stopRow"])
      
      # add the AutoEDAExtract column to the data frame
      segmentDF$AutoEDAExtract <- rep("", times=nrow(segmentDF))
      
      # make sure that events are on distinct rows
#       prestimRow <- 1
      onsetRow <- eventDF$Begin - segmentDF$Sample[1] + 1 
      offsetRow <- eventDF$End - segmentDF$Sample[1] + 1 
      answerRow <- eventDF$Answer - segmentDF$Sample[1] + 1 
      latencyRow <- eventDF$Begin - segmentDF$Sample[1] + 1 + EDALat * cps
      ROWEndRow <- eventDF$Answer - segmentDF$Sample[1] + 1 + (ROWEnd * cps)
      # reponseOnsetRow 
      # responseEndRow
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
#       segmentDF$AutoEDAExtract[events["prestimRow"]] <- "prestimRow"
      segmentDF$AutoEDAExtract[events["onsetRow"]] <- "onsetRow"
      segmentDF$AutoEDAExtract[events["offsetRow"]] <- "offsetRow"
      segmentDF$AutoEDAExtract[events["answerRow"]] <- "answerRow"
      segmentDF$AutoEDAExtract[events["latencyRow"]] <- "latencyRow"
      segmentDF$AutoEDAExtract[events["ROWEndRow"]] <- "ROWEndRow"
      segmentDF$AutoEDAExtract[events["responseOnsetRow"]] <- "responseOnsetRow"
      segmentDF$AutoEDAExtract[events["responseEndRow"]] <- "responseEndRow"
      segmentDF$AutoEDAExtract[events["endRow"]] <- "endRow"
      segmentDF$AutoEDAExtract[events["stopRow"]] <- "stopRow"
      
      ###
      
      segmentList[[j]] <- segmentDF
      
      ### section using EDAExtract()
      
      # segmentDF <- segmentList[[j]]
      # eventDF <- eventList[[j]]
      # z <- EDAExtract(x=segmentDF, y=eventDF)
      # segmentList[[j]] <- z
      
      ###
      
    } # end iteration over data frames in each list
    
    names(segmentList) <- segmentNames
    
    assign(segmentListName, segmentList, pos=1)
    
  } # end iteration over the segment lists
  
} # end EDAExtractFn()

EDAExtractFn(x=mySegmentLists, y=myEventLists)



