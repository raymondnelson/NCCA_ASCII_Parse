# R script to edit the NCCA Cardio data
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



############# initialize a function to plot a cardio data segment ############



cardioSegPlotFn <- function(NCCAASCIIName, startIdx, len, output=FALSE) {
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



###########  initialize a function to make a straight line from response onset to response end. ############



straightLineFn <- function(x) {
  # R function to make a straight line for the response segment
  # x input is the cardio response segment from onset systolic peak to end systolic peak
  xLen <- length(x)
  xStartVal <- x[1]
  xEndVal <- x[length(x)]
  xDiff <- xEndVal - xStartVal
  xIncrement <- xDiff / (xLen-1)
  xOut <- rep(xStartVal, times=length(x))
  for(i in 2:length(xOut)) {
    xOut[i] <- xOut[i-1] + xIncrement
  }
  # plot.ts(xOut)
  round(xOut)
}



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



####### initialize a function to draw BEZIER CURVE #######



bezier_curve_7 <- function(start=1, end=1, num_points=100, x=c(0, 1, 8, 75, 8, 1, 0)) {
  # Function to compute a degree-6 (7-point) Bézier curve
  # start is the onset value for the response segement
  # end is the end value for the response segement
  
  y=c(0:6)
  
  # Initialize empty vectors to store the curve points
  x_points <- numeric(num_points)
  y_points <- numeric(num_points)
  
  yChange <- end - start
  
  P0 <- round(x[1] + 0)
  P1 <- round(x[2] + ((yChange / 6) * 1))
  P2 <- round(x[3] + ((yChange / 6) * 2))
  P3 <- round(x[4] + yChange / 2)
  P4 <- round(x[5] + ((yChange / 6) * 4))
  P5 <- round(x[6] + ((yChange / 6) * 5))
  P6 <- round(x[7] + end)
  
  # Generate points along the curve
  i=1
  for (i in 1:num_points) {
    t <- (i - 1) / (num_points - 1)  # Parameter t in [0, 1]
    
    # Bézier formula for x and y coordinates
    x_points[i] <- (1-t)^6 * y[1] + 
      6 * (1-t)^5 * t * y[2] + 
      15 * (1-t)^4 * t^2 * y[3] + 
      20 * (1-t)^3 * t^3 * y[4] + 
      15 * (1-t)^2 * t^4 * y[5] + 
      6 * (1-t) * t^5 * y[6] + 
      t^6 * y[7]
    
    y_points[i] <- (1-t)^6 * P0[1] + 
      6 * (1-t)^5 * t * P1[1] + 
      15 * (1-t)^4 * t^2 * P2[1] + 
      20 * (1-t)^3 * t^3 * P3[1] + 
      15 * (1-t)^2 * t^4 * P4[1] + 
      6 * (1-t) * t^5 * P5[1] + 
      t^6 * P6[1]
  }
  
  # Return the points as a data frame
  data.frame(x = x_points, y = y_points)
}



######## PLOT A BEZIER #########



{
  
  # if(!exists("pulseAmp")) pulseAmp <- 12000
  # if(!exists("slopeDistance")) slopeDistance <- 0
  # 
  # # curve_points <- bezier_curve_7(start=startVal, end=endVal, x=c(0, 1, 35, 15, 10, 1, 0))
  # 
  # plotPointVals <- round(c(0, pulseAmp*.01, pulseAmp*.67, pulseAmp*.5, pulseAmp*.12,  slopeDistance*.01, 0))
  # plotPointVals <- round(c(0, pulseAmp*.01, pulseAmp*.5, pulseAmp*.67, pulseAmp*.025,  slopeDistance*.01, 0))
  # 
  # curve_points <- bezier_curve_7(start=1, end=1, x=plotPointVals)
  # curve_points <- bezier_curve_7(start=1, end=slopeDistance, x=plotPointVals)
  # # curve_points <- bezier_curve_7(start=1, end=0, x=plotPointVals)
  # 
  # plot(curve_points$x, curve_points$y, type = "l", col = "blue", lwd = 2,
  #      xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")
  # 
  # # Plot the default curve
  # curve_points <- bezier_curve_7(x=c(0, 1, 10000, 12316, 1000, 500, 0))
  # plot(curve_points$x, curve_points$y, type = "l", col = "blue", lwd = 2,
  #      xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")
  # 
  # # Example usage
  # P0 <- c(0, 0)
  # P1 <- c(1, 1)
  # P2 <- c(2, 8)
  # P3 <- c(3, 75)
  # P4 <- c(4, 8)
  # P5 <- c(5, 1)
  # P6 <- c(6, 0)
  # 
  # points(rbind(P0, P1, P2, P3, P4, P5, P6), col = "red", pch = 16)  # Plot control points
  
}



############ initialize a function to edit a cardio response segment ###############



cardioInterpolateFn <- function(NCCAASCIIName, startIdx=301, cardioAmp=12000, len=450, fact=.5, augment=TRUE, output=FALSE) {
  # R function to interpolate cardio values between two index rows in the NCCA ASCII data
  # Dec 11, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file for a single chart
  # NCCAASCIIName is not vectorized
  # startIdx is the sample row where interpolation begins
  # cardioAmp is the pulse amplitude of the cardio data prior to the response segment
  # len is the number of sample rows to interpolate 
  # fact is the magnitude factor of change from the data to the interpolated straight line,
  # augment = TRUE will increase the cardio activity
  ##
  # output is the name of the NCCA ASCII text file for the chart
  # an output side effect is to overwrite the NCCA ASCII chart 
  ####
  
  {
    # import the NCCA ASCII data from the text file
    thisNCCAASCIIName <- NCCAASCIIName[1]
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
    textLines <- readLines(thisNCCAASCIIName)
  }
  
  ## locate the time series data rows ##
  
  {
    # slide the time series data and locate the Label column
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
  
  ## locate the text rows for the cardio data column ##
  
  {
    headerRowText <- textLines[(tsStartRow-1)]
    grep("Cardio1", headerRowText)
    # get the character positions for the cardio column within the NCCA ASCII text file
    colEnd <- str_locate(headerRowText, "Cardio1")[2]
    colStart <- colEnd - 9
    # 10 text characters
  }
  
  ## slice the cardio response segment ##
  
  {
    startSlice <- startIdx
    endSlice <- startSlice + len - 1
    theseRows <- c(startSlice:endSlice)
    thisSegment <- timeSeriesData[theseRows] 
  }
  
  ## slice the cardio data column for the segment ##
  
  {
    cardioValues <- NULL
    i=1
    for(i in 1:length(thisSegment)) {
      # build up the cardio time series data via concatenation
      cardioValues <- c(cardioValues, str_sub(thisSegment[i], colStart, colEnd))
    }
    cardioValues <- as.numeric(cardioValues)
    # plot.ts(cardioValues)
    startVal <- cardioValues[1]
    endVal <- cardioValues[length(cardioValues)]
  }
  
  ## interpolate a straight line between the start and end values ##
  
  {
    # distance from start to end value
    SEDistance <- endVal - startVal
    interpVal <- SEDistance / (len - 1)
    interpLine <- trunc(cumsum(c(startVal, rep(interpVal, (len - 1)))))
    # plot.ts(interpLine)
  }
  
  ## use a function to compute the bezier response curve ##
  
  {
    # use the cardioAmp input value to shape the new response curve
    plotPointVals <- c(0, cardioAmp*.01, cardioAmp*.9, cardioAmp, cardioAmp*.25, cardioAmp*.1, 0)
    curve_points <- bezier_curve_7(start=1, end=(SEDistance+1), num_points=len, x=plotPointVals)
    # plot.ts(curve_points$y)
    # plot(curve_points$x, curve_points$y, type = "l", col = "blue", lwd = 2,
    #      xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")
  }
  
  ## get the distance from the straight line to the curve ##
  
  {
    # these values will be subtracted from the cardio data
    diffLine <- round(curve_points$y - (interpLine - interpLine[1]))
  }
  
  ## augment or dimminish the cardio response ##
  
  {
    if(!exists("augment")) augment <- TRUE
    if(isTRUE(augment)) {
      # newLine is the data that are inserted into the time series 
      newLine <- cardioValues + round(diffLine * fact)
    } else {
      newLine <- cardioValues - round(diffLine * fact)
    }
    # plot.ts(cardioValues)
    plot.ts(newLine)
  }
  
  ## format the new cardio response segment as text ##
  
  {
    # format the newLine column data 
    newTextCol <- str_pad(paste0(as.character(newLine), ".0"), width=10, side="left")
  }
  
  ## insert the new response into the cardio data segment ## 
  
  {
    # use a loop to insert the new text lines into the cardio response segment
    for(i in 1:length(newTextCol)) {
      thisLine <- thisSegment[i]
      newLine0 <- paste0(str_sub(thisLine, 1, (colStart-1)), 
                         newTextCol[i], 
                         str_sub(thisLine, (colEnd+1), -1) )
      thisSegment[i] <- newLine0
    }
    # put the segement in the time series data
    timeSeriesData[theseRows] <- thisSegment
    # put the time series data into the ncca ascii text data
    textLines[tsStartRow:tsEndRow] <- timeSeriesData
  }
  
  ## write the NCCA ASCII text file ##
  
  {
    # write_lines(textLines, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
    # thisNCCAASCIIName
    write_lines(textLines, file=thisNCCAASCIIName)
  }
  
  print(NCCAASCIIName)

  if(isTRUE(output)) newLine
    
} # end cardioInterpolateFn() function



##########  SELECT AND EDIT A CARDIO RESPONSE SEGMENT #############

stop()

#########   STOP  ###########

{
  NCCAASCIIChartNames
  # NCCAASCIIName <- "D&-XDDV3OXX1000ARM3-X.01A"
  NCCAASCIIName <- NCCAASCIIChartNames[5]
  # cardioSegPlotFn(NCCAASCIIName, 300, 450)
  eventLocs <- locateEventsFn(NCCAASCIIName=NCCAASCIIName)
  print(NCCAASCIIName)
  print(eventLocs)
}
  
{
  ## locate and plot the stimulus segment
  stimOnsetLoc <- eventLocs['R5'] - 300
  # stimOnsetLoc <- 30600
  WOE <- (15*120) - 0
  preStim <- (10*120)
  postStim <- (10*120)
  preSeg <- 0
  postSeg <- 0
  
  # plot the 15 second evaluation window
  cardioDAT <- cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, stimOnsetLoc, len=WOE, output=TRUE)
}

{
  # locate and plot the response segment
  segLoc <- 60
  segLen <- 480
  
  # plot the physiological response
  cardioDAT0 <- cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, (stimOnsetLoc+segLoc), len=segLen, output=TRUE)
}

{
  # locate the response onset and end at the systolic peak
  
  # use the entire stimulus segment for this
  segStart <- ifelse(segLoc-23 <= 0, 1, segLoc-23)
  startLoc <- which.max(cardioDAT[c(segStart:(segLoc+23))]) + (segLoc-23)
  endLoc <- which.max(cardioDAT[c((segLoc+segLen-23):(segLoc+segLen+23))])  + (segLen+segLoc-23-1) 
  
  if(startLoc <= 0) startLoc <- 1
  if(length(endLoc)==0) endLoc <- length(cardioDAT)
  
  startVal <- cardioDAT[startLoc]
  endVal <- cardioDAT[endLoc]
  
  cardioDAT1 <- cardioDAT[c(startLoc:endLoc)]
  
  # get the response length from onset to end
  # responseLen <- endLoc - startLoc + 1
  responseLen <- length(cardioDAT1)
  # print(responseLen)

  # plot the physiological response from the max peak indices
  # plot.ts(cardioDAT1)
  
  # plot the physiological response
  cardioDAT1 <- cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+startLoc-1), len=segLen, output=TRUE)
  
}

{
  # get the pulse amplitude
  pulseAmp <- getPulseAmplitudeFn(cardioDAT, startLoc)
  print(pulseAmp)
  
  # get the distance from the response onset value to response end value
  slopeDistance <- cardioDAT1[length(cardioDAT1)] - cardioDAT1[1]
  # responseDiff <- endVal - startVal
  # print(slopeDistance)
  
  # make a straightLine from response onset to response end
  straightLineDAT <- straightLineFn(cardioDAT1)
  # plot.ts(straightLineDAT)
}
  
  
## augment the cardio repsonse 
{
  # cardioInterpolateFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+startLoc-1), cardioAmp=pulseAmp, len=segLen, fact=.25, augment=TRUE, output=FALSE)
  cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+startLoc-1), len=segLen, output=FALSE)
}

## diminish the cardio response
{
  # cardioInterpolateFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+startLoc-1), cardioAmp=pulseAmp, len=segLen, fact=.25, augment=FALSE, output=FALSE)
  cardioSegPlotFn(NCCAASCIIName=NCCAASCIIName, startIdx=(stimOnsetLoc+startLoc-1), len=segLen, output=FALSE)
}

