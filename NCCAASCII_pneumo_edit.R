# R script to edit the NCCA Pneumograph data
# January 2, 2025
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
  
  source("~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R", echo=FALSE)
  
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



############# initialize a function to plot a pneumo data segment ############



pneumoSegPlotFn <- function(NCCAASCIIName, colName="UPneumo", startIdx, len, output=FALSE) {
  # R function to plot a respiration data segment
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
  
  ## locate the text rows for the respiration column ##
  
  {
    headerRowText <- textLines[(tsStartRow-1)]
    # grep(colName, headerRowText)
    # get the character positions for the EDA column within the NCCA ASCII text file
    colEnd <- str_locate(headerRowText, colName)[2]
    colStart <- colEnd - 9
    # 10 text characters
  }
  
  ## slice the respiration values for the segment 
  
  {
    startSlice <- startIdx
    endSlice <- startSlice + len - 1
    
    theseRows <- c(startSlice:endSlice)
    
    thisSegment <- timeSeriesData[theseRows]
    
    dataValues <- NULL
    
    for(i in 1:length(theseRows)) {
      dataValues <- c(dataValues, str_sub(thisSegment[i], colStart, colEnd))
    }
    
    dataValues <- as.numeric(dataValues)
    
    startVal <- dataValues[1]
    endVal <- dataValues[length(dataValues)]
  }
  
  plot.ts(dataValues)
  
  print(NCCAASCIIName)
  
  if(output) return(dataValues)
  
} # end pneumoSegPlotFn() 



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
  
  if(!exists("pulseAmp")) respAmp <- 6000
  if(!exists("endVal")) endVal <- 1
  if(!exists("startVal")) startVal <- 1
  if(!exists("slopeDistance")) slopeDistance <- 0

  curve_points <- bezier_curve_7(start=startVal, end=endVal, x=c(0, 1, 35, 15, 10, 1, 0))

  plotPointVals <- round(c(0, respAmp*.01, respAmp*.15, respAmp*.2, respAmp*.15,  slopeDistance*.01, 0))
  # plotPointVals <- round(c(0, respAmp*.01, respAmp*.5, respAmp*.67, respAmp*.025,  slopeDistance*.01, 0))

  curve_points <- bezier_curve_7(start=1, end=1, x=plotPointVals)
  # curve_points <- bezier_curve_7(start=1, end=slopeDistance, x=plotPointVals)
  # curve_points <- bezier_curve_7(start=1, end=0, x=plotPointVals)

  plot(curve_points$x, curve_points$y, type = "l", col = "blue", lwd = 2,
       xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")

  # Plot the default curve
  # curve_points <- bezier_curve_7(x=c(0, 5, 100, 500, 100, 5, 0))
  # plot(curve_points$x, curve_points$y, type = "l", col = "blue", lwd = 2,
  #      xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")

  # invert the curve
  curve_points$y0 <- -curve_points$y
  plot(curve_points$x, curve_points$y0, type = "l", col = "blue", lwd = 2,
       xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")
  
  # Example usage
  P0 <- c(0, 0)
  P1 <- c(1, 1)
  P2 <- c(2, 8)
  P3 <- c(3, 75)
  P4 <- c(4, 8)
  P5 <- c(5, 1)
  P6 <- c(6, 0)

  # points(rbind(P0, P1, P2, P3, P4, P5, P6), col = "red", pch = 16)  # Plot control points
  
}



##########  initialize a function to edit a pneumograph response segment ##############



NCCAASCIIName <- NCCAASCIIChartNames[1]

eventLocs <- locateEventsFn(NCCAASCIIName=NCCAASCIIChartNames[1])

eventName <- "C1"


NCCAASCIIName 

eventLocs

startIdx <- eventLocs[eventName]



pneumoInterpolateFn <- function(NCCAASCIIName, respCol="UPneumo", startIdx, len=450, fact=.67, augment=TRUE, output=FALSE) {
  # R function to interpolate cardio values between two index rows in the NCCA ASCII data
  # Dec 11, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file for a single chart
  # NCCAASCIIName is not vectorized
  # startIdx is the sample row where interpolation begins
  # respCol is the column name for the respiration channel
  # len is the number of sample rows to interpolate 
  # fact is the magnitude factor of change from the data to the interpolated straight line,
  # augment = TRUE will increase the respiratory suppression response activity
  # augement = FALSE will decrease the respiratory suppression response 
  ##
  # output is the name of the NCCA ASCII text file for the chart
  # an output side effect is to overwrite the NCCA ASCII chart 
  ####
  
  ## read the NCCA ASCII file ##
  
  {
    # import the NCCA ASCII data from the text file
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    
    # read the ncca ascii text file
    textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
    
    # cps <- 30
    if(!exists("len")) len <- 450
    if(!exists("fact")) fact <- .67
    if(!exists("respCol")) respCol <- "UPneumo"
    if(!exists("augment")) augment <- TRUE
    if(!exists("output")) output <- FALSE
    
    respCol <- ifelse(respCol=="upper", "UPneumo",
                      ifelse(respCol=="lower", "LPneumo",
                             respCol))
  }
  
  ## locate the time series data rows and slice the stimulus segment ##
  
  {
    # slice the time series data and locate the Label column
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
  
  ## locate the column character positions for the pneumo data ##
  
  {
    if(!exists("respCol")) respCol <- "UPneumo"
    headerRowText <- textLines[(tsStartRow-1)]
    grep(respCol, headerRowText)
    # get the character positions for the EDA column within the NCCA ASCII text file
    colEnd <- str_locate(headerRowText, respCol)[2]
    colStart <- colEnd - 9
    # 10 text characters
  }
  
  ## abstract the respiration column for the entire chart ##
  
  {
    respDAT <- NULL
    i=1
    for(i in 1:length(timeSeriesData)) {
      respDAT <- c(respDAT, str_sub(timeSeriesData[i], colStart, colEnd))
    }
    respDAT <- as.numeric(respDAT)
    # length(respDAT)
    # plot.ts(respDAT)
  }
  
  ## compute the inhalation, exhalation, and mid-line for the entire chart ##
  
  {
    ## requires helper functions from the sigProcHelper.R script
    
    cps=30
    
    # the mid line
    respMidDAT <- MASmooth(x=respDAT, y=round(25*cps), times=4)
    # values > mid are reduced to augment a response
    # values < mid are increased to augment a response
    # plot.ts(respMidDAT)
    
    # inhalation line
    respInhDAT <- interpolatePeaks(x=maxPeak(x=respDAT, y=round(.25*cps)),
                                             y=respDAT[maxPeak(x=respDAT, y=round(.25*cps))])
    # plot.ts(respInhDAT)
    
    # exhalation line
    respExhDAT <- interpolatePeaks(x=minPeak(x=respDAT, y=round(.25*cps)),
                                             y=respDAT[minPeak(x=respDAT, y=round(.25*cps))])
    # plot.ts(respExhDAT)
    
    # # compute the respiration amplitude
    # respAmp <- mean(respInhDAT) - mean(respExhDAT)
  }
  
  ## slice the stimulus segment from the time series data ##
  
  {
    startSlice <- startIdx
    endSlice <- startSlice + len - 1
    theseRows <- c(startSlice:endSlice)
    thisSegment <- timeSeriesData[theseRows] 
  }
  
  ## slice the respiration data column for the stimulus segment ##
  
  {
    respSegDAT <- NULL
    i=1
    for(i in 1:length(thisSegment)) {
      # build up the cardio time series data via concatenation
      respSegDAT <- c(respSegDAT, str_sub(thisSegment[i], colStart, colEnd))
    }
    respSegDAT <- as.numeric(respSegDAT)
    # plot.ts(respSegDAT)
  }
  
  ## also slice the data for the mid line for the segment ##
  
  {
    midLineSegDAT <- respMidDAT[theseRows]
    # plot.ts(midLineSegDAT)
  }
  
  ## interpolate a straight line between the start and end values for the mid-line ##
  
  {
    startVal <- midLineSegDAT[1]
    endVal <- midLineSegDAT[length(midLineSegDAT)]
    # distance from start to end value
    SEDistance <- endVal - startVal
    interpVal <- SEDistance / (len - 1)
    interpLine <- trunc(cumsum(c(startVal, rep(interpVal, (len - 1)))))
    # plot.ts(interpLine)
  }
  
  ## compute straight lines for the inhalation and exhalation lines ## 
  
  {
    # len is an input parameter
    respInhSegDAT <- respInhDAT[c(startSlice:endSlice)]
    # plot.ts(respInhSegDAT)
    inhLineStartVal <- respInhDAT[startSlice]
    inhLineEndVal <- respInhDAT[endSlice]
    inhSEDistance <- inhLineEndVal - inhLineStartVal
    inhInterpVal <- inhSEDistance / (len - 1)
    inhInterpLine <- trunc(cumsum(c(inhLineStartVal, rep(inhInterpVal, (length(c(startSlice:endSlice)) - 1)))))
    # plot.ts(inhInterpLine)
    
    respExhSegDAT <- respExhDAT[c(startSlice:endSlice)]
    # plot.ts(respExhSegDAT)
    exhLineStartVal <- respExhDAT[startSlice]
    exhLineEndVal <- respExhDAT[endSlice]
    exhSEDistance <- exhLineEndVal - exhLineStartVal
    exhInterpVal <- exhSEDistance / (len - 1)
    exhInterpLine <- trunc(cumsum(c(exhLineStartVal, rep(exhInterpVal, (length(c(startSlice:endSlice)) - 1)))))
    # plot.ts(exhInterpLine)
  }
  
  ## compute the respiration amplitude for this segment ##
  
  {
    respAmp <- mean(inhInterpLine - exhInterpLine)
  }
  
  ## use a function to compute the bezier response curve for the inhalation line ##
  
  {
    # use the cardioAmp input value to shape the new response curve
    plotPointVals <- c(0, respAmp*.01, respAmp*.9, respAmp, respAmp*.25, respAmp*.1, 0)
    
    # inhalation curve
    curve_pointsInh <- bezier_curve_7(start=1, end=(inhSEDistance+1), num_points=len, x=plotPointVals)
    # plot.ts(curve_pointsInh$y)
    # plot(curve_points$x, curve_points$y, type = "l", col = "blue", lwd = 2,
    #      xlab = "x", ylab = "y", main = "Degree-6 Bézier Curve (7 Points)")
    
    # plot.ts(inhInterpLine)
    
    # invert the curve
    curve_pointsInh$y0 <- inhInterpLine - (curve_pointsInh$y - inhInterpLine)
    # plot.ts(curve_pointsInh$y0)
  }
  
  ## compute the bezier curve for the exhalation line ##
  
  {
    # exhalation curve
    curve_pointsExh <- bezier_curve_7(start=1, end=(exhSEDistance+1), num_points=len, x=plotPointVals)
    # plot.ts(curve_pointsExh$y)
    
    # plot.ts(exhInterpLine)
    
    # invert the curve
    curve_pointsExh$y0 <- exhInterpLine - (curve_pointsExh$y - exhInterpLine)
    # plot.ts(curve_pointsExh$y0)
  }
  
  ## get the distance from the straight lines to the bezier curves ##
  
  {
    ## these values will be subtracted from and added to the respiration data
    
    if(augment) {
      inhDiffLine <- -(curve_pointsInh$y - (inhInterpLine - inhInterpLine[1]))
      # plot.ts(inhDiffLine)
      exhDiffLine <- curve_pointsExh$y - (exhInterpLine - exhInterpLine[1])
      # plot.ts(exhDiffLine)
    } else {
      inhDiffLine <- (curve_pointsInh$y - (inhInterpLine - inhInterpLine[1]))
      # plot.ts(inhDiffLine)
      exhDiffLine <- -(curve_pointsExh$y - (exhInterpLine - exhInterpLine[1]))
      # plot.ts(exhDiffLine)
    }
    
  }
  
  ## construct a data frame for the segment ##
  
  {
    workingDF <- cbind.data.frame(respSegDAT, 
                                  midLineSegDAT, 
                                  respInhSegDAT, 
                                  inhInterpLine, 
                                  respExhSegDAT,
                                  exhInterpLine)
    
    # compute the distance of the respiration data to the mid line
    workingDF$midDistance <- respSegDAT - midLineSegDAT
    
    # sparate the resiration data above and below the mid line 
    workingDF$tidalMidDistance <- workingDF$midDistance
    workingDF$exhalMidDistance <- workingDF$midDistance
    
    # zero the inhalation distance below the mid line
    workingDF$tidalMidDistance[which(workingDF$tidalMidDistance <= 0)] <- 0
    
    # zerio the exhalation distance above the mid line
    workingDF$exhalMidDistance[which(workingDF$exhalMidDistance >= 0)] <- 0
    
    # compute the relative or proportional distance to the mid line
    workingDF$tidalProp <- workingDF$tidalMidDistance / max(workingDF$tidalMidDistance)
    workingDF$exhalProp <- workingDF$exhalMidDistance / min(workingDF$exhalMidDistance)
    
    tidalProp <- workingDF$tidalProp
    exhalProp <- workingDF$exhalProp
  }
  
  ## augment or diminish the respiration response ##
  
  {
    # plot.ts(inhDiffLine)
    # plot.ts(exhDiffLine)
    # plot.ts(midLineSegDAT)
    # plot.ts(respSegDAT)
    
    # initialize the new respiration segment data
    newRespSegDAT <- respSegDAT
    i=1
    for(i in 1:length(respSegDAT)) {
      if(respSegDAT[i] > midLineSegDAT[i]) {
        # data values above the mid line
        newRespSegDAT[i] <- round( respSegDAT[i] + ((inhDiffLine[i] * fact) * tidalProp[i]) )
      } else {
        # data values below the midline
        newRespSegDAT[i] <- round( respSegDAT[i] + ((exhDiffLine[i] * (fact/2)) * exhalProp[i]) )
      }
    }
    
    newRespSegDAT <- round(newRespSegDAT, 0)
    # plot.ts(newRespSegDAT)
    # plot.ts(respDAT)
    
    workingDF$newRespSegDAT <- newRespSegDAT
  }
  
  ## format the new respiration response segment as text ##
  
  {
    # format the newLine column data 
    newTextCol <- str_pad(paste0(as.character(newRespSegDAT), ".0"), width=10, side="left")
  }
  
  ## insert the new response into the respiration data segment ## 
  
  {
    # use a loop to insert the new text lines into the respiration response segment
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
    write_lines(textLines, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
  }
  
  print(NCCAASCIIName)
  
  if(isTRUE(output)) newLine
  
} # end pneumoInterpolateFn()



######### SELECT AND EDIT A PNEUMO RESPONSE SEGMENT ##########



stop()


#########.  STOP   ###########



{
  NCCAASCIIChartNames
  # NCCAASCIIName <- "D&-DaveTestJoe-1-1.05A"
  NCCAASCIIName <- NCCAASCIIChartNames[4]
  # cardioSegPlotFn(NCCAASCIIName, 300, 450)
  eventLocs <- locateEventsFn(NCCAASCIIName=NCCAASCIIName)
  print(eventLocs)
  respCol <- "UPneumo"
}

{
  ## locate and plot the stimulus segment
  eventName <- "R7"
  eventLoc <- eventLocs[eventName]
  WOE <- 500
  preStim <- 150
  postStim <- 150
  # preSeg <- 0
  # postSeg <- 0
  
  stimOnsetLoc <- ( (eventLoc - preStim) + 1 )
  len <- ( (WOE + preStim + postStim) - 2 )
  
  # plot the 15 second evaluation window with the prestim and poststim segments
  respDAT <- pneumoSegPlotFn(NCCAASCIIName=NCCAASCIIName, colName=respCol, startIdx=stimOnsetLoc, len=len, output=TRUE)
}

{
  # locate and plot the response segment
  segLoc <- ( (stimOnsetLoc + preStim) - 1 ) + 0
  segLen <- ( WOE ) + 0 
   
  # plot the physiological response
  respDAT0 <- pneumoSegPlotFn(NCCAASCIIName=NCCAASCIIName, colName=respCol, startIdx=segLoc, len=segLen, output=TRUE)
}

## augment the respiration response (decreased amplitude)
{
  # pneumoInterpolateFn(NCCAASCIIName=NCCAASCIIName, respCol=respCol, startIdx=segLoc, len=segLen, fact=.4, augment=TRUE, output=FALSE)
  pneumoSegPlotFn(NCCAASCIIName=NCCAASCIIName, colName=respCol, startIdx=stimOnsetLoc, len=len, output=FALSE)
}


## diminish the respiration response (increased amplitude)
{
  # pneumoInterpolateFn(NCCAASCIIName=NCCAASCIIName, respCol=respCol, startIdx=segLoc, len=segLen, fact=.4, augment=FALSE, output=FALSE)
  pneumoSegPlotFn(NCCAASCIIName=NCCAASCIIName, colName=respCol, startIdx=segLoc, len=segLen, output=FALSE)
}







