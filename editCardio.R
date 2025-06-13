# R Script to fix cardio and EDA data after signal processing
# February 23, 2025
# Raymond Nelson
####



# export thhe data to NCCA ASCII after editingg



####



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
  
  NCCAASCIIChartNames <- getCharts((x="D\\&"))
  
  uniqueExamNames <- uniqueNames(getCharts((x="D\\&")))
  
  
  uniqueSeriesNames <- str_sub(getCharts((x="D\\&")), -5, -5)
  
  
  uniqueChartNames <- str_sub(getCharts((x="D\\&")), -3, -1)
  
  
  # print(uniqueExamNames)
  
  print(NCCAASCIIChartNames)
  
}



########### initialize a function to locate the events #############



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
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    # read the NCCA ASCII text file
    textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
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



############# initialize a function to plot a cardio data segment ############



cardioSegPlotFn <- function(NCCAASCIIName, startIdx=1, len=NULL, output=FALSE) {
  # R function to plot a cardio data segment
  # used to locate a cardio segment for interpolation
  # Dec 7, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file for a single chart
  # NCCAASCIIName is not vectorized
  # startIdx is the sample row where interpolation begins
  # len is the number of sample rows to interpolate 
  ##
  # output is the name of the NCCA ASCII text file for the chart
  # an output side effect is a time series plot for the EDA segement
  ####
  
  {
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
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
  
  ## length ##
  
  if(is.null(len)) {
    len <- length(timeSeriesData)
  }
  
  ## locate the text rows for the cardio column ##
  
  {
    headerRowText <- textLines[(tsStartRow-1)]
    
    grep("Cardio", headerRowText)
    
    # get the character positions for the EDA column within the NCCA ASCII text file
    colEnd <- str_locate(headerRowText, "Cardio1")[2]
    colStart <- colEnd - 9
    # 10 text characters
  }
  
  ## slice the Cardio values for the segment 
  
  {
    startSlice <- startIdx
    endSlice <- startSlice + len - 1
    
    theseRows <- c(startSlice:endSlice)
    
    thisSegment <- timeSeriesData[theseRows]
    
    cardioValues <- NULL
    
    for(i in 1:length(theseRows)) {
      cardioValues <- c(cardioValues, str_sub(thisSegment[i], colStart, colEnd))
    }
    
    cardioValues <- as.numeric(cardioValues)
    
    startVal <- cardioValues[1]
    endVal <- cardioValues[length(cardioValues)]
  }
  
  plot.ts(cardioValues)
  
  print(NCCAASCIIName)
  
  if(output) return(cardioValues)
  
} # end cardioSegPlotFn() 



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
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
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



######################



########## initialize a function to get the pulse amplitude ###########



getPulseAmplitudeFn <- function(cardioDAT, startLoc) {
  # R function to get a cardio pulse amplitude from the pulse prior to a response segment
  # cardioDAT is the time series cardio data for a single stimulus presentation
  # including prestimulus segment, stimulus segment and poststimulus
  # startLoc is the systolic peak where a physiological response begins
  ###
  maxVal <- cardioDAT[startLoc]
  begin <- (startLoc-60)
  end <- startLoc
  if(begin <= 0) {
    begin <- 1
    minLoc <- which.min(cardioDAT[c(begin:end)]) + begin - 1
    minVal <- cardioDAT[minLoc]
    end <- which.max(cardioDAT[c(begin:(begin+60-1))])
    maxVal <- cardioDAT[end]
  } else {
    minLoc <- which.min(cardioDAT[c(begin:end)]) + begin - 1
    minVal <- cardioDAT[minLoc]
  }
  # pulseAmp <- cardioDAT[startLoc] - minVal
  
  # startVal - cardioDAT[which.min(cardioDAT[c(startLoc:(startLoc+45))]) + startLoc - 1
  
  pulseAmp <- maxVal - minVal
  pulseAmp
}



#################



stop()

{
  NCCAASCIIChartNames
  NCCAASCIIName <- "D&-6EFAI-X.02A"
  # cardioSegPlotFn(NCCAASCIIName, 300, 450)
  eventLocs <- locateEventsFn(NCCAASCIIName=NCCAASCIIName)
  print(eventLocs)
}

{
  ## locate and plot the stimulus segment
  stimOnsetLoc <- eventLocs['E3'] - 70
  WOE <- 120
  
  # plot the segment
  cardioDAT <- cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, stimOnsetLoc, len=WOE, output=TRUE)
}


cardioDAT <- cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, output=TRUE)


mean(cardioDAT)


{
  # locate and plot the response segment
  segLoc <- 50
  segLen <- 300
  
  # plot the physiological response
  cardioDAT0 <- cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, (stimOnsetLoc+segLoc), len=segLen, output=TRUE)
}

{
  # get the pulse amplitude
  pulseAmp <- getPulseAmplitudeFn(cardioDAT, stimOnsetLoc)
  # print(pulseAmp)
}

getPulseAmplitudeFn()



## edit the cardio data 

# replace the artifact with the mean 


