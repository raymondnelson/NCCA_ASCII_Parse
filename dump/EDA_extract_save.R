# EDA extract
#
# function to extract response data from EDA stimulus segment
#
# EDA measurement
# ROW is from stim onset to 5 seconds after verbal response
# EDA latency is .5 seconds after stimulus onset
# input is a vector 15 seconds of time series data for each stimulus
# sampling rate is 30cps
# output is a numerical value representing the max amplitude of increase in conductance
# the magnitude of the increase in conductance in response to the stimulus
#
# 1. locate the ROW after latency
# 2. locate  all positive slope segments
# 3. locate the onset value and rowNumb for all positive slopes
# 4. locate the max value and rowNumb for all positive slope segments that begin in the ROW
# 5. locate the difference between each onset and all subsquent max 
# 6. select the measurement for max diff between each onset and all subsequent max vals 
# 
# This procedure is necessary in case the EDA data 
# show the largest positive slope response,
# followed by a subsequent lower onset point and a smaller positive slope response
#
########################################


cps <- 30
prestimSeg <- 5
EDAlat <- .5
ROWEnd <- 5
measuredSeg <- 15


# myEventList <- PF090316_1.01A_eventList
# mySegmentList <- PF090316_1.01A_dataSegmentList
# 
# myEventList1 <- PF090316_1.01A_eventList[[1]]
# mySegmentList1 <- PF090316_1.01A_dataSegmentList[[1]]



# myEventList <- PF090316_1.01A_eventList
# mySegmentList <- PF090316_1.01A_dataSegmentList
# 
# myEventList1 <- PF090316_1.01A_eventList[[1]]
# mySegmentList1 <- PF090316_1.01A_dataSegmentList[[1]]


mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")


#####  

EDAExtractFn <- function(x=mySegmentLists, y=myEventLists, returnOutput=FALSE) {
  # function to iterate the EDAExtract function over a list of measurement segments
  # x is a vector of names of all _dataSegmentList lists
  # y is a vector of names of all _eventList lists
  # this function will get each list and an AutoEDAExtract column to the measurement segments
  # the Events column can be used for plotting and measurement
  #
  ####
  
  # first make a function to add the AutoEDAExtract column to the data frame
  EDAExtract <- function(x=segmentDF, y=eventDF) {
    # function to extract the amplitude of EDA increase in response to a stimulus
    #
    # x is a data frame of time series data for a single stimulus segment
    #
    # y is a data frame of Label, Begin, End, and Answer events for the stimulus segment, 
    # including prestimRow, onsetRow, latencyRow, offsetRow, answerRow, ROWEndRow, endRow
    # ouput is a named numerical vector
    # including responseOnsetRow, respeonsePeakRow, responseOnsetValue, responsePeakValue, and responseChangeValue
    #
    ####
    
    myEventList1 <- y
    mySegmentList1 <- x
    
    startRow <- mySegmentList1$Sample[1]
    
    # correction for X announcement for which there may not be 5 seconds prestimulus
    ifelse(myEventList1$Begin <= (cps*prestimSeg),
           prestimRow <- 1,
           prestimRow <- myEventList1$Begin - (startRow-1) - (cps*prestimSeg))
    onsetRow <- myEventList1$Begin - (startRow-1)
    EDALatRow <- onsetRow + cps*lat
    offsetRow <- myEventList1$End - (startRow-1)
    answerRow <- myEventList1$Answer - (startRow-1)
    #correcting if there is no answer (answer row will be the same as offsetRow)
    if(answerRow==offsetRow) answerRow <- answerRow+1
    ROWEndRow <- answerRow + (cps*ROWEnd)
    endRow <- onsetRow + (cps*measuredSeg)
    
    # a named numerical vector of 7 items, inluding:
    # prestimRow, onsetRow, EDALatRow, offsetRow, answerRow, ROWEndRow, endRow
    myEvents <- c(prestimRow, onsetRow, EDALatRow, offsetRow, answerRow, ROWEndRow, endRow)
    names(myEvents) <- c("prestimRow", "onsetRow", "EDALatRow", "offsetRow", "answerRow", "ROWEndRow", "endRow")
    
    # a time series vector including 
    # 3 seconds before and 15 seconds after stimulus onset
    myData <- mySegmentList1$AutoEDA[prestimRow:endRow]
    
    ###
    
    # make a function to determine the slope
    slopeDir <- function(z=myData) {
      z1 <- z
      diff1 <- diff(z)
      z1 <- ifelse(diff1==0,
                   z1 <- 0,
                   # ifelse is vectorized and does not require a control loop
                   ifelse(diff1>=0,
                          z1 <- 1,
                          z1 <- -1)
      )
      return(z1)
    } # slopeDir
    mySlope <-  c(0, slopeDir(z=myData))
    # added a 0 at the onset to account for the shortened vector length
    ###
    
    # make a function to find the onset of positive slope segments
    positiveChange <- function(x=mySlope) {
      # x is a vector of slope valences for each sample in the time series data
      x1 <- x
      # if else is vectorized
      x1 <- ifelse(x==1,
                   x1 <- 1,
                   x1 <- 0)
      # onset of positive slope
      xOnset <- ifelse(x1==0,
                       ifelse((x1[1:(length(x1)-1)] + x1[2:length(x1)]) > 0,
                              1,
                              0),
                       0)
      # phase correction
      xOnset <- c(0, xOnset[1:(length(xOnset)-1)])
      return(xOnset)
    } # positive Change
    xOnset <- which(positiveChange()[1:ROWEndRow]==1)
    
    ###
    
    # function to locate the peak of all positive slope sections 
    slopePeak <- function(x=mySlope) {
      # x is a vector of 
      x1 <- x
      xPeak <- ifelse(x1==0,
                      0,
                      ifelse((x1[1:(length(x1)-1)] * x1[2:length(x1)]) == 1,
                             0,
                             1))
    } # end slopePeak function
    xPeak <- which(slopePeak()==1)
    xPeak <- xPeak[xPeak<=ROWEndRow]
    
    # keep one additional peak if the slope is positive at the end of the ROWEnd
    ifelse(mySlope[ROWEndRow]==1, 
           xPeak <- xPeak[1:(length(xPeak[xPeak<=ROWEndRow])+1)],
           xPeak <- xPeak[xPeak <= ROWEndRow])
    
    ###
    
    # remove the prestimulus segment from the data
    xOnset <- xOnset[xOnset>EDALatRow]
    xPeak <- xPeak[xPeak > xOnset[1]]
    
    ###
    
    # compute a vector of max amplitudes for each onset and all subsequent Peaks
    yChange <- xOnset
    yChangeMax <- 0
    # i <- 1
    for (i in 1:length(xOnset)) {
      # yChange is a vector of xPeak rows for the max increase following each xOnset
      yChangeOnset <- xOnset[i]
      yChangePeak <-
        
        yChange[i] <- max(myData[xPeak[i:length(xPeak)]] - myData[xOnset[i]])
      
    } # end for loop
    
    # get the max change value from the vector 
    yChangeMaxIndex <- which.max(yChange)
    
    # get the onset row by indexing the max change value
    yChangeOnset <- xOnset[which.max(yChange)]
    yChangeOnsetValue <- myData[yChangeOnset]
    
    #   # get the peak row by indexing the xPeak vector
    #   yChangePeak <- xPeak[which.max(yChange):length(xPeak)][which.max(myData[xPeak[which.max(yChange):length(xPeak)]] - 
    #                                                                      myData[xOnset[which.max(yChange)]])]
    #   
    #   #same
    #   yChangePeak <- xPeak[which.max(yChange):length(xPeak)][which.max(abs(myData[xPeak[yChangeMaxIndex:length(xPeak)]] - 
    #                                                                          yChangeOnsetValue  ))]
    
    ### and a simpler version to index the peak row
    
    # make a short vector of the xPeak vector starting at the xOnset
    # xPeakShort <- xPeak[yChangeMaxIndex:length(xPeak)]
    xPeakShort <- xPeak[which.max(yChange):length(xPeak)]
    
    # make a short vector of the peak values
    xPeakShortValues <- myData[xPeakShort] 
    
    # make a vector of the differences between xPeakShortValues and yChangeOnsetValue
    xPeakShortDifferences <- xPeakShortValues - yChangeOnsetValue
    
    yChangePeak <- xPeakShort[which.max(xPeakShortDifferences)]
    
    ###
    
    # get the peak value 
    yChangePeakValue <- myData[yChangePeak]
    
    ###
    
    # compute the change from response onset to response peak
    yChangeValue <- yChangePeakValue - yChangeOnsetValue
    
    # or this way
    yChangePeakValue <- xPeakShortDifferences[which.max(xPeakShortDifferences)] + 
      yChangeOnsetValue
    
    ###
    
    # construct the output vector
    outputVector <- c(yChangeOnset, 
                      yChangePeak, 
                      yChangeOnsetValue, 
                      yChangePeakValue, 
                      yChangeValue)
    names(outputVector) <- c("responseOnsetRow",
                             "responsePeakRow",
                             "responseOnsetValue",
                             "responsePeakValue",
                             "responseChangevalue")
    
    # return(outputVector)
    
    ###
    
    # add the AutoEDAExtract column to the data frame
    segmentDF$AutoEDAExtract <- rep("", times=nrow(segmentDF))
    
    # add the data to the column
    segmentDF$AutoEDAExtract[yChangeOnset] <- "responseOnsetRow"
    segmentDF$AutoEDAExtract[yChangePeak] <- "responsePeakRow"
    
    return(segmentDF)
    
  } # end EDAExtract function
  # EDAExtract(x=segmentDF, y=eventDF)
  
  ####
  
  segmentList1 <- x
  eventList1 <- y  
  returnOut <- returnOutput
  
  ###
  
  # iterate over the segment lists
  for (i in 1:length(segmentList1)) {
    
    segmentListName <- segmentList1[i]
    eventListName <- eventList1[i]
    
    segmentList <- get(segmentListName, pos=1)
    eventList <- get(eventListName, pos=1)
    
    segmentNames <- names(segmentList)
    
    # iterate over the data frames in each list
    for (j in 1:length(segmentList)) {  
      
      segmentDF <- segmentList[[j]]
      eventDF <- eventList[[j]]
      
      z <- EDAExtract(x=segmentDF, y=eventDF)
      
      segmentList[[j]] <- z
    } # end iteration over data frames in each list
    
    names(segmentList) <- segmentNames
    
    assign(segmentListName, segmentList, pos=1)
    
  } # end iteration over the segment lists
  
} # end EDAExtractFn()
# EDAExtractFn(x=mySegmentLists, y=myEventLists, returnOutput=FALSE)


