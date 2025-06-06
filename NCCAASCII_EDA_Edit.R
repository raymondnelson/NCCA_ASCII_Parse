# R script to edit the NCCA EDA data
# November 29, 2024
# Raymond Nelson 

####


######## LOCATE THE NCCA ASCII FILES IN THE CWD ##########


{
  
  library(stringr)
  
  library(readr)
  
  if(!exists("RPath")) {
    # mac
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  NCCAASCIIChartNames <- getCharts((x="D\\$"))
  
  uniqueExamNames <- uniqueNames(getCharts((x="D\\$")))
  
  
  uniqueSeriesNames <- str_sub(getCharts((x="D\\$")), -5, -5)
  
  
  uniqueChartNames <- str_sub(getCharts((x="D\\$")), -3, -1)
  
  
  print(uniqueExamNames)
  
  print(NCCAASCIIChartNames)
  
}



########### initialize a function to locate the stimulus events in the NCCA ASCCII data #############


locateEventsFn <- function(NCCAASCIIName) {
  # R function to locate the stimulus event within the NCCA ASCII data
  # November 30, 2024
  # Raymond Nelson
  ####
  # NCCAASCIIName is the name of a single NCCA ASCCII chart
  ##
  # output is a named vector of event index rows
  # the name of each item is the question label
  ####
  
  library(stringr)
  
  {
    thisNCCAASCIIName <- NCCAASCIIName[1]
      
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    # read the NCCA ASCII text file
    # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
    textLines <- readLines(thisNCCAASCIIName)
  }
  
  ## locate the time series data rows ##
  
  {
    # nchar("Sample     Time    Label")
    tsHeaderTextFragment <- "Sample     Time    Label"
    tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == tsHeaderTextFragment) + 1
    tsEndRow <- length(textLines)
    tsHeaderRow <- tsStartRow - 1
  }
  
  ## locate the events table ##
  
  {
    # nchar("Event    Label      Begin        End     Answer")
    eventsHeaderTextFragment <- "Event    Label      Begin        End     Answer"
    eventsStartRow <- which(str_sub(textLines[1:150], 1, 47) == eventsHeaderTextFragment) + 1
    eventsHeaderRow <- eventsStartRow - 1
    # slice the events table
    eventsTableText <- textLines[c(eventsHeaderRow:(tsHeaderRow-2))]
    numberOfEvents <- length(eventsTableText) - 1
  }
  
  ## get the question labels and stimulus onset indices ##
  
  {
    eventLabelsVc <- NULL
    for(i in 2:length(eventsTableText)) {
      # 7:14 character columns for the event labels
      eventLabelsVc <- c(eventLabelsVc, str_trim(str_sub(eventsTableText[i], 7, 14)))
    }
    eventBeginVc <- NULL
    for(i in 2:length(eventsTableText)) {
      # 16:25 character columns for stimulus onset indices
      eventBeginVc <- c(eventBeginVc, as.numeric(str_trim(str_sub(eventsTableText[i], 16, 25))))
    }
  }
  
  names(eventBeginVc) <- eventLabelsVc
  
  eventBeginVc
  
} # end locateEventsFn


# locateEventsFn(NCCAASCIIName)


########### initialize a function to plot an EDA stimulus segment ##################



EDASegPlotFn <- function(NCCAASCIIName, startIdx, len, output=FALSE) {
  # R function to plot an EDA segment
  # used to locate an EDA segment for interpolation
  # November 29, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file for a single chart
  # NCCAASCIIName is not vectorized
  # Label is the question ID for the inserted event
  # startIdx is the sample row where interpolation begins
  # len is the number of sample rows to interpolate 
  ##
  # output is the name of the NCCA ASCII text file for the chart
  # an output side effect is a time series plot for the EDA segement
  ####
  
  {
    thisNCCAASCIIName <- NCCAASCIIName[1]
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
    textLines <- readLines(thisNCCAASCIIName)
  }
  
  ## locate the time series data rows ##
  
  {
    # nchar("Sample     Time    Label")
    tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Sample     Time    Label") +  1
    tsEndRow <- length(textLines)
    # slice the time series data
    timeSeriesData <- textLines[tsStartRow:tsEndRow]
    # isolate the Label column
    LabelColumn <- str_sub(timeSeriesData, 17, 24)
    # length(LabelColumn)
    # get the time series header row to use later
    tsHeaderRow <- textLines[(tsStartRow-1)]
    # timeSeriesData
  }
  
  ## locate the text rows for the EDA column ##
  
  {
    headerRowText <- textLines[(tsStartRow-1)]
    
    grep("EDA1", headerRowText)
    
    # get the character positions for the EDA column within the NCCA ASCII text file
    colEnd <- str_locate(headerRowText, "EDA1")[2]
    colStart <- colEnd - 9
    # 10 text characters
  }
  
  ## slice the EDA values for the segment 
  
  {
    startSlice <- startIdx
    endSlice <- startSlice + len - 1
    
    theseRows <- c(startSlice:endSlice)
    
    thisSegment <- timeSeriesData[theseRows]
    
    edaValues <- NULL
    
    for(i in 1:length(theseRows)) {
      edaValues <- c(edaValues, str_sub(thisSegment[i], colStart, colEnd))
    }
    
    edaValues <- as.numeric(edaValues)
    
    
    
    startVal <- edaValues[1]
    endVal <- edaValues[length(edaValues)]
  }
  
  plot.ts(edaValues)
  
  print(NCCAASCIIName)
  
  if(output) return(edaValues)
  
} # end EDASegPlotFn() 



# EDASegPlotFn("D&-TedTestOrlando-1-1.03A", 7775, 60)
# EDASegPlotFn("D&-TedTestOrlando-1-1.03A", 8514, 60)
# EDASegPlotFn("D&-TedTestOrlando-1-1.04A", 6103, 120)


########### initialize a function to interpolate an EDA segment ###########




EDAInterpolateFn <- function(NCCAASCIIName, startIdx, len, fact=3, augment=FALSE) {
  # R function to interpolate EDA values between two index rows in the NCCA ASCII data
  # November 29, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file for a single chart
  # NCCAASCIIName is not vectorized
  # startIdx is the sample row where interpolation begins
  # len is the number of sample rows to interpolate 
  # fact is the magnitude factor of change from the data to the interpolated straight line,
  # augment = TRUE will increase the EDA activity
  ##
  # output is the name of the NCCA ASCII text file for the chart
  # an output side effect is to overwrite the NCCA ASCII chart 
  ####
  
  {
    thisNCCAASCIIName <- NCCAASCIIName[1]
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
    textLines <- readLines(thisNCCAASCIIName)
  }
  
  ## locate the time series data rows ##
  
  {
    # nchar("Sample     Time    Label")
    tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Sample     Time    Label") +  1
    tsEndRow <- length(textLines)
    # slice the time series data
    timeSeriesData <- textLines[tsStartRow:tsEndRow]
    # isolate the Label column
    LabelColumn <- str_sub(timeSeriesData, 17, 24)
    # length(LabelColumn)
    # get the time series header row to use later
    tsHeaderRow <- textLines[(tsStartRow-1)]
    # timeSeriesData
  }
  
  ## locate the text rows for the EDA column ##
  
  {
    headerRowText <- textLines[(tsStartRow-1)]
    
    grep("EDA1", headerRowText)
    
    # get the character positions for the EDA column within the NCCA ASCII text file
    colEnd <- str_locate(headerRowText, "EDA1")[2]
    colStart <- colEnd - 9
    # 10 text characters
  }
  
  ## slice the EDA values for the segment 
  
  {
    startSlice <- startIdx
    endSlice <- startSlice + len - 1
    
    theseRows <- c(startSlice:endSlice)
    
    thisSegment <- timeSeriesData[theseRows]
    
    edaValues <- NULL
    
    for(i in 1:length(theseRows)) {
      edaValues <- c(edaValues, str_sub(thisSegment[i], colStart, colEnd))
    }
    
    edaValues <- as.numeric(edaValues)
    
    # plot.ts(edaValues)
    
    startVal <- edaValues[1]
    endVal <- edaValues[length(edaValues)]
  }
  
  ## save the difference values to add some noise later ##
  
  {
    medVal <- median(edaValues)
    
    diffVals <-  diff(edaValues)
    
    medDiff <- median(diffVals)  
    # mean(diffVals)  
    # quantile(diffVals, .75)
    
    # subtrat the median 
    # plot.ts(edaValues - medVal)
    # simply moves the segment so the origen is 0 
  }
  
  ## interpolate between the start and end values ##
  
  {
    # distance from start to end value
    SEDistance <- endVal - startVal
    
    interpVal <- SEDistance / (len - 1)
    
    interpLine <- trunc(cumsum(c(startVal, rep(interpVal, (len - 1)))))
    # plot.ts(interpLine)
    
    if(!exists("augment")) augment <- FALSE
    
    factA <- 1 / fact
    # factB <- fact - 1
    
    distanceVals <- (edaValues - interpLine)
    # plot.ts(edaValues)
    
    if(isTRUE(augment)) {
      # compute the partial distance between the segment and the interpolation line
      # distanceVals <- (edaValues - interpLine) * factA
      # distanceVals <- ((edaValues * factB) - interpLine) /  factA
      # compute the new augemented segment
      newLine <- round((interpLine + (distanceVals * fact)))
    } else {
      # compute the partial distance values
      # distanceVals <- (edaValues - interpLine) / factA
      # compute the new reduced segment
      newLine <- round(interpLine + (distanceVals * factA)) 
    }
    # 
    
    # plot.ts(newLine)
  }
  
  ## format the new EDA segment as text and insert it into the data segment ## 
  
  {
    newTextCol <- str_pad(paste0(as.character(newLine), ".0"), width=10, side="left")
    
    for(i in 1:length(newTextCol)) {
      thisLine <- thisSegment[i]
      newLine <- paste0(str_sub(thisLine, 1, (colStart-1)), 
                        newTextCol[i], 
                        str_sub(thisLine, (colEnd+1), -1) )
      thisSegment[i] <- newLine
    }
    
    # put the segement in the time series data
    timeSeriesData[theseRows] <- thisSegment
    
    # put the time series data into the ncca ascii text data
    textLines[tsStartRow:tsEndRow] <- timeSeriesData
  }
  
  ## write the NCCA ASCII text file ##
  
  {
    # write_lines(textLines, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
    write_lines(textLines, file=thisNCCAASCIIName)
  }
  
  print(NCCAASCIIName)
  
} # end function




#################### EDIT AN EDA SEGMENT #################

stop()

######### STOP ##########

{
  NCCAASCIIChartNames
  # NCCAASCIIName <- "D&-X30EL8XX1000FZC3-X.02A"
  NCCAASCIIName <- NCCAASCIIChartNames[1]
  eventLocs <- locateEventsFn(NCCAASCIIName=NCCAASCIIName)
  print(eventLocs)
}

{
  stimOnsetLoc <- eventLocs['R5']
  WOE <- 450
  preStim <- 150
  postStim <- 150
  preSeg <- 0
  postSeg <- 0
  
  # plot the stimulus segment for 15 seconds
  EDASegPlotFn(NCCAASCIIName=NCCAASCIIName, startIdx=stimOnsetLoc, len=WOE)
}


{
  segLoc <- 90
  segLen <- 300
  
  # plot only the response segment  
  EDASegPlotFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+segLoc), len=segLen)
}




# augment an EDA reaction
{
  # EDAInterpolateFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+segLoc), len=segLen, fact=1.25, augment=TRUE)
  EDASegPlotFn(NCCAASCIIName=NCCAASCIIName, (stimOnsetLoc+segLoc), len=segLen)
}

# reduce an EDA reaction
{
  # EDAInterpolateFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+segLoc), len=segLen, fact=1.25, augment=FALSE)
  EDASegPlotFn(NCCAASCIIName=NCCAASCIIName, (stimOnsetLoc+segLoc), len=segLen)
}

# check the plot
# EDASegPlotFn(NCCAASCIIName, 7775, len=60)
# EDASegPlotFn(NCCAASCIIName, 8514, len=60)
# EDASegPlotFn(NCCAASCIIName, 6118, len=75)

# and plot the 

#######################

########################


