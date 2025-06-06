# function to complete preprocessing tasks on the NCCA ASCII ouput
# after parsing the header, stimulus, and time series data
# 4/23/2016
# Raymond Nelson

# contains 3 helper fuctions 

# fixTagsFn()
# setColRangeFn()
# getFirstLastEventFn()

# and the main function

# preProc()


#########################################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[3]


# x=uniqueExams
# y=100
# showNames=TRUE
# output=FALSE


# {
#   
#   # source this first
#   # source this for the fixTagsFn()
#   source(paste0(RPath, 'NCCAASCIIParseHelperFunctions.R'), echo=FALSE)
#   
#   # source this last
#   # source the sigProcHelper.R script to load the getFirstLastEventFn()
#   # also the setColRange() function to replace the one from NCCAASCIIParseHelperFunction.R
#   source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
#   
# }

fixTagsFn <- function(x=chartDF$Label) {
  # R function to fix problem question tags 
  # and ensure consistency in question labels
  # called by the preProc function
  # input x is a vector or data frame column containing the question tags
  # chartDF$Label
  # chartDF$eventLabel
  # output is a vector of the same length as the input
  ###
  
  # called by preproc function
  
  x <- toupper(x)
  
  x <- gsub("X_X", "X", x)
  x <- gsub("XX_XX", "XX", x)
  x <- gsub("XXX", "XX", x)
  x <- gsub("XXXX", "XX", x)
  
  x <- gsub("RELEVANT", "R", x)
  x <- gsub("RELEVA", "R", x)
  
  x <- gsub("COMPARISON", "C", x)
  x <- gsub("COMPAR", "C", x)
  
  x <- gsub("NEUTRAL", "N", x)
  x <- gsub("NEUTRA", "N", x)
  
  x <- gsub("IRRELEVANT", "I", x)
  x <- gsub("IRRELE", "I", x)
  x <- gsub("IR", "I", x)
  
  x <- gsub("SACRIFICE", "SA", x)
  x <- gsub("SACRIF", "SA", x)
  x <- gsub("SACRIFICE RELEVANT", "SA", x)
  
  x <- gsub("SYMPTOMATIC", "SY", x)
  x <- gsub("SYM", "SY", x)
  x <- gsub("SYMPTO", "SY", x)
  
  x <- gsub("INTRODUCTORY", "INT", x)
  x <- gsub("INTROD", "INT", x)
  
  x <- gsub(" +", "", x)
  
  # all punctuation characters
  x <- gsub("[[:punct:]]", "", x)
  
  x <- gsub("RR", "R", x)
  x <- gsub("R R", "R", x)
  x <- gsub("CC", "C", x)
  # x <- gsub("C C", "C", x)
  x <- gsub("NN", "N", x)
  x <- gsub("N N", "N", x)
  x <- gsub("I", "I", x)
  x <- gsub("II", "I", x)
  x <- gsub("I I", "I", x)
  x <- gsub("SR", "SA", x)
  x <- gsub("SRSR", "SA", x)
  x <- gsub("SR SR", "SA", x)
  x <- gsub("RS RS", "SA", x)
  x <- gsub("RSRS", "SA", x)
  x <- gsub("RSR", "SA", x)
  x <- gsub("S S", "SA", x)
  x <- gsub("SASA", "SA", x)
  x <- gsub("SS", "SA", x)
  x <- gsub("SY SY", "SY", x)
  x <- gsub("SYSY", "SY", x)
  x <- gsub("SYMSYM", "SY", x)
  x <- gsub("SYM SYM", "SY", x)
  
  x <- gsub("IS3", "SY", x)
  x <- gsub("IS2", "SY", x)
  x <- gsub("2IS", "SY", x)
  x <- gsub("3IS", "SY", x)
  x <- gsub("3SY", "SY", x)
  x <- gsub("2SY", "SY", x)
  # x <- gsub("3S", "SY", x)
  # x <- gsub("S3", "SY", x)
  
  x <- gsub("3SA", "SA", x)
  x <- gsub("3SA", "SA", x)
  x <- gsub("2SA", "SA", x)
  x <- gsub("2S", "SA", x)
  x <- gsub("S2", "SA", x)
  
  # SPACES AND COMMAS 
  x <- gsub(" ", "", x)
  x <- gsub("\\.", "", x)
  
  # AXCITON SERIES ANNOTATIONS
  x <- gsub("MIXEDSER", "EI", x)
  x <- gsub("SERIES1", "EI", x)
  x <- gsub("MGQT", "EI", x)
  x <- gsub("ZCT", "EI", x)
  
  # SOME UNUSUAL QUESTION TAGS
  x <- gsub("3(C)", "C3", x)
  x <- gsub("4(C)", "C4", x)
  x <- gsub("5(C)", "C5", x)
  x <- gsub("6(C)", "C6", x)
  x <- gsub("7(C)", "C7", x)
  x <- gsub("9(C)", "C9", x)
  x <- gsub("4(R)", "R4", x)
  x <- gsub("5(R)", "R5", x)
  x <- gsub("6(R)", "R6", x)
  x <- gsub("7(R)", "R7", x)
  x <- gsub("8(R)", "R8", x)
  x <- gsub("10(R)", "R10", x)
  
  # SOME MORE UNUSUAL QUESTION TAGS
  x <- gsub("3C", "C3", x)
  x <- gsub("C3C", "C3", x)
  x <- gsub("4C", "C4", x)
  x <- gsub("C4C", "C4", x)
  x <- gsub("5C", "C5", x)
  x <- gsub("C5C", "C5", x)
  x <- gsub("6C", "C6", x)
  x <- gsub("C6C", "C6", x)
  x <- gsub("7C", "C7", x)
  x <- gsub("C7C", "C7", x)
  x <- gsub("9C", "C9", x)
  x <- gsub("C9C", "C9", x)
  x <- gsub("4R", "R4", x)
  x <- gsub("R4R", "R4", x)
  x <- gsub("5R", "R5", x)
  x <- gsub("R5R", "R5", x)
  x <- gsub("6R", "R6", x)
  x <- gsub("R6R", "R6", x)
  x <- gsub("7R", "R7", x)
  x <- gsub("R7R", "R7", x)
  x <- gsub("8R", "R8", x)
  x <- gsub("R8R", "R8", x)
  x <- gsub("9R", "R9", x)
  x <- gsub("R9R", "R9", x)
  x <- gsub("10R", "R10", x)
  x <- gsub("R10R", "R10", x)
  
  x <- gsub("3E", "E3", x)
  x <- gsub("8E", "E8", x)
  
  # ACTQ KEY QUESTIONS
  x <- gsub("4KEY", "4K", x)
  x <- gsub("4Key", "4K", x)
  x <- gsub("4KeyR", "4K", x)
  x <- gsub("4KR", "4K", x)
  
  # ANNOTATIONS
  x <- gsub("IE", "EI", x)
  x <- gsub("OSN", "OS", x)
  x <- gsub("ISN", "OS", x)
  x <- gsub("CT", "TS", x)
  x <- gsub("CL", "TS", x)
  x <- gsub("CC", "TS", x)
  x <- gsub("TT", "T", x)
  x <- gsub("TTT", "T", x)
  x <- gsub("MM", "M", x)
  x <- gsub("MMM", "M", x)
  x <- gsub("MV", "M", x)
  x <- gsub("WR", "EI", x)
  x <- gsub("WRQ", "EI", x)
  x <- gsub("PWQ", "EE", x)
  x <- gsub("BDB", "DB", x)
  x <- gsub("BD", "DB", x)
  
  # June 30, 2023
  #  x <- gsub("CGH", "TS", x)
  # <- gsub("GH", "TS", x)  
  
  return(x)
} 




setColRangeFn <- function(DAT, y=30000, firstRow=firstEvent, lastRow=lastEventEnd) {
  # R function to set each column range
  # called by the preProc function
  # in the preProc.R script
  # DAT is a zero centered column from the data frome of recorded time series data
  # y is the max range value 
  ###
  # get the current data range
  rangeVal <- max(DAT[firstRow:lastRow]) - min(DAT[firstRow:lastRow])
  rangeCoef <- y / rangeVal
  # in case there is a dead sensor with no activity
  ifelse(rangeVal < 5,
         DAT <- DAT * 0,
         DAT <- DAT * rangeCoef
  )
  # center the data at zero
  DAT <- DAT - DAT[firstRow]
  ## Mar 7, 2025 - modification to fix data drops (Axciton charts in the 2002 DodPI archive) ##
  {
    xMed <- median(DAT)
    x25th <- quantile(DAT, .25)
    x75th <- quantile(DAT, .75)
    x99th <- quantile(DAT, .99)
    x01th <- quantile(DAT, .01)
    xIQR <- x75th - x25th
    xLimitLow <- xMed - (1 * xIQR)
    xLimitHigh <- xMed - (1 * xIQR)
    DAT[which(DAT <= x01th)] <- x01th
  }
  ###
  return(DAT)
} 




getFirstLastEventFn <- function(x=chartDF) {
  # R function to compute the row indices for the first and last events in a chart data frame
  # called by the preProc function
  # x input is a chart DF after signal processing
  # uses the excludedEvents variable from the global env - loaded by excludedEvent.R
  # output is a named vector of two items: firstEvent, lastEventEnd
  ###
  chartDF <- x
  # make a vector of event names
  # eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
  eventNames <- toupper(chartDF$Label[chartDF$Label!=""])
  # make a vector of event onset rows
  # eventRows <- which(chartDF$eventLabel!="")  
  eventRows <- which(chartDF$Label!="")  
  # get the onset of the first event and the end of the last event
  if(length(eventNames)==0) {
    print("no stimulus events. none processed.rn")
    # next()
    firstEvent=1
    lastEventEnd=nrow(chartDF)
  } else {
    # get the first event for scaling and centering
    firstEvent <- eventRows[!(eventNames %in% excludeEvents)][1]
    # fix missing firstEvent
    if(is.na(firstEvent)) { firstEvent <- 1 }
    # get the last event for scaling and centering
    lastEvent <- eventRows[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
    eventNames[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
    # fix missing lastEvent
    if(length(lastEvent)==0) { lastEvent <- nrow(chartDF) }
    # changed 10-1-2016 because some examiners rush to end the chart recording 
    # and others present more stimuli after the XX end announcement
    # lastEventEnd <- lastEvent
    # changed back 11-1-2016
    # use the measuredSeg and cps values from the global env
    lastEventEnd <- lastEvent + measuredSeg * cps
    if(length(lastEventEnd)==0) { lastEventEnd <- nrow(chartDF) }
    # fix potential problem when lastEventEnd exceeds the data frame rows for the chart
    if(lastEventEnd > nrow(chartDF)) { lastEventEnd <- nrow(chartDF) }
    if(is.na(lastEventEnd)) { lastEventEnd <- nrow(chartDF) }
  }
  outVector <- c(firstEvent, lastEventEnd)
  names(outVector) <- c("firstEvent", "lastEventEnd")
  return(outVector)
} 




############# main function ######################



preProc <- function(x=uniqueExams, makeDF=TRUE, output=FALSE) {
  # R function to pre-process NCCA ASCII output data
  # after parsing the header events and time series data
  #
  # center the onset at zero
  # set the range for each sensor 
  # add some columns for events
  # add eventLabels and event Text
  #
  # x input is a vector of exam names in the current working directory
  
  # data frames are saved to the global environment as a side effect
  # showNames parameter controls console output of the exam name
  
  # output parameter controls whether the data from the last unique exam is returned
  #
  # output of this function is a data frame, 
  # from the last exam in the input vector of exam names
  # side effect of this function is to create _Data and _Stimuli data frames for each exam
  #
  ###
  
  uniqueExams <- x
  
  # get the range from the function input y value
  # colRange <- as.numeric(y)
  
  #### iterate over each exam in the input vector of unique exam names #### 
  
  i=1
  for(i in 1:length(uniqueExams)) {
   
    {
      
      # get the exam data and stimulus events and the names of each series
      
      examName <- uniqueExams[i]
      assign("examName", examName, pos=1)
      print(examName)
      
      #### 20191231 maybe could  parse the headers and data here ####
      
      # get the names of time series lists for all unique series in each exam
      # searchString <- paste0("*", examName, "_Data", "*")
      
      #### get the exam data frame with the time series data #### 
      
      # get the time series data frame for the exam
      # examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      examDF <- get(paste0(examName, "_Data"), pos=1)
      
      # View(examDF)
      
      {
        examDF$UPneumo <- as.numeric(examDF$UPneumo)
        examDF$LPneumo <- as.numeric(examDF$LPneumo)
        examDF$EDA1 <- as.numeric(examDF$EDA1)
        examDF$Cardio1 <- as.numeric(examDF$Cardio1)
        if("Move1" %in% names(examDF[5:16])) {
          examDF$Move1 <- as.numeric(examDF$Move1)
        }
        if("PPG1" %in% names(examDF[5:16])) {
          examDF$PPG1 <- as.numeric(examDF$PPG1)
        }
      }
      
      # set the first and last rows
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      # fix question tags 
      # source(paste0(RPath, 'NCCAASCIIParseHelperFunctions.R'), echo=FALSE)
      examDF$Label <- fixTagsFn(examDF$Label)
      examDF$eventLabel <- fixTagsFn(examDF$eventLabel)
      # assign("examDF", examDF, pos=1)
      
      # print(examName)
      print(paste("exam:", i, "of", length(uniqueExams), examName))
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(examDF$seriesName))
      print(paste("unique series: ", paste(uniqueSeries, collapse = " ")))
      
      ####   get the stimulus events data frame for the exam   ####
      
      {
        
        # if(exists("eventDF")) rm("eventDF")
        eventDFName <- paste0(examName, "_Stimuli")
        if(exists(eventDFName)) {
          eventDF <- get(eventDFName, pos=1)
        } #  commented out 8-4-2017 else next()
        
        # set these to numeric
        eventDF$Begin <- as.numeric(eventDF$Begin)
        eventDF$End <- as.numeric(eventDF$End)
        eventDF$Answer <- as.numeric(eventDF$Answer)
        
        # View(eventDF)
        
        # change the data types - this should be done during parsing for efficiency
        # eventDF$seriesName <- as.character(eventDF$seriesName)
        # eventDF$Event <- as.numeric(eventDF$Event)
        # eventDF$Begin <- as.numeric(eventDF$Begin)
        # eventDF$End <- as.numeric(eventDF$End)
        # eventDF$Answer <- as.numeric(eventDF$Answer)
        
        # reassign the _Stimuli and eventDF data frames 
        # to the global envir for inspection
        # assign(paste0(examName, "_Stimuli"), eventDF, pos=1)
        # assign("eventDF", eventDF, pos=1)
        # View(eventDF)
        
        eventDF$Label <- fixTagsFn(eventDF$Label)
        eventDF$eventLabel <- fixTagsFn(eventDF$eventLabel)
        
        # initialize a newEventDF for this exam
        newEventDF <- NULL
        
      }
      
    }
    
    ###### iterate over each unique series ######
    
    j=1
    for(j in 1:length(uniqueSeries)) {
    
      {
        
        # slice the series and get the names of each chart
        
        # get the time-series data frame for this series
        seriesName <- uniqueSeries[j]
        seriesRows <- which(examDF$seriesName == seriesName)
        seriesOnsetRow <- min(seriesRows)
        seriesEndRow <- max(seriesRows)
        
        #### slice the series data frame ####
        
        seriesDF <- examDF[seriesRows,]
        # View(seriesDF)
        
        # assign("seriesDF", seriesDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        print(paste("series:", toString(uniqueSeries[j])))
        
        # get a vector of names of time series data for the charts in the exam series
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        print(paste("unique charts:", toString(uniqueCharts)))
        
      }
      
      #### iterate over each unique chart ####
      
      k=1
      for(k in 1:length(uniqueCharts)) {
       
        {
          
          # get the chart and chart details
          
          chartName <- uniqueCharts[k]
          # assign("chartName", chartName, pos=1)
          print(chartName)
          
          # get the data frame with the time series data for each chart in the series
          chartRows <- which(seriesDF$chartName==chartName)
          chartOnsetRow <- min(chartRows)
          chartEndRow <- max(chartRows)
          
          #### slice the chart data frame ####
          
          chartDF <- seriesDF[chartRows,]
          
          # assign("chartDF", chartDF, pos=1)
          # View(chartDF)
          
          # for multi-series federal exams
          # if(seriesName == "7" && chartName == "02A") {
          #   assign("chartDF", chartDF, envir=.GlobalEnv)
          #   stop()
          # }
          
          # re-initialize the eventLabel column in the chartDF data frame
          chartDF$eventLabel <- ""
          
        }
        
        #### center the data at zero ####
        
        {
          
          # data needs to be centered at onset zero for the DSP filters
        
          print("  center the data at zero")
          
          lastDataCol <- min(grep("^c_", names(chartDF))) - 1
          
          # fix some NA values that may occur when not using some sensors
          l=11
          for(l in 11:lastDataCol) {
            chartDF[grep("NaN", chartDF[,l]),l] <- 0
            # other way to do it
            # chartDF[which(is.na(as.numeric(chartDF[,11]))),l] <- 0
          }
          
          # make sure all data columns are numeric
          l=11
          for(l in 11:lastDataCol) {
            chartDF[,l] <- as.numeric(chartDF[,l])
          }
          
          # iterate over the chartDF columns and call the centerColumn function
          l=11
          # start at column 11 because that is the first data column in chartDF
          for (l in 11:lastDataCol) {
            # use a function to center the data
            chartDF[,l + lastDataCol - 10] <- centerColumn(chartDF[,l])
          } # end loop over l columns to center data
          
          # View(chartDF)
          
        }
         
        #### get the first and last events for this chart ####
        
        {
        
          # first source the sigProcHelper.R script
          # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
          
          firstLastEvents <- getFirstLastEventFn(x=chartDF)
          firstEvent <- firstLastEvents[1]
          lastEventEnd <- firstLastEvents[2] - 450
          # in case of short charts
          if(lastEventEnd < 1) lastEventEnd <- nrow(chartDF)
          # assign("firstLastEvents", firstLastEvents, pos=1)
          print(firstEvent)
          print(lastEventEnd)
          
        }
        
        #### scale the range for this chart ####
        
        {
          
          print("  set the range for the time series data")
          
          # first source the sigProcHelper.R script
          
          # start at the first data column 11
          useCols <- ((11 + (lastDataCol - 10)):((lastDataCol + 1) + (lastDataCol - 11)))
          m=17
          for (m in min(useCols):max(useCols)) {
            # use a function for each data column
            # colRange is set in the init
            if(!exists("colRange", envir=.GlobalEnv)) colRange=30000
            chartDF[,m] <- setColRangeFn(DAT=chartDF[,m], y=colRange, firstRow=firstEvent, lastRow=lastEventEnd)
            # plot.ts(chartDF[,m])
          } 
          
        }
        
        #### events ####
        
        ## select the events for the current chart ##
        
        {
          
          if(!exists("eventDF")) {
            # sumbit the chartDF and increment to the next chart if no events
            examDF[useRows,] <- chartDF
            next()
          }
          # View(eventDF)
          
          # get the rows for this series and this chart
          # eventDF was loaded in the i loop for the exam
          useRows <- which(eventDF$chartName == chartName & 
                             eventDF$seriesName == seriesName)
          
          if(length(useRows) == 0) {
            # submit the chartDF to the examDF and increment the chart
            # cannot use useRows because this is the examDF not eventDF
            examDF[examDF$chartName==chartName & examDF$seriesName==seriesName,] <-
              chartDF
            next()
          }
          
          #### slice the events data frame for this chart ####
          
          chartEventsDF <- eventDF[useRows,]
          # View(chartEventsDF)
          # nrow(chartEventsDF)
          # chartEventsDF$Label
          # View(chartDF)
          
          # increment the k loop if no events
          if(nrow(chartEventsDF) == 0) {
            examDF[useRows,] <- chartDF
            next()
          }
          
          uniqueEventsChart <- unique(chartEventsDF$Label)
          
          allEventsChart <- chartEventsDF$Label
          
          # unique(chartDF$Label)
          
        }
        
        #####   work with the events   ####
        
        if(nrow(chartEventsDF) != 0) {
          
          ## check that all events in chartEventsDF are in chartDF ##
          
          # re-order the chartEventsDF rows by the event Begin indices
          # 4-20-2017 may no longer be necessary
          # because the rows are re-ordered earlier
          # 8-24-2017
          # chartEventsDF <- chartEventsDF[(sort(chartEventsDF$Begin, index.return=TRUE)$ix),]
          # chartEventsDF <- chartEventsDF[(sort(as.numeric(chartEventsDF$Begin), index.return=TRUE)$ix),]
          # View(chartEventsDF)
          
          print("  process the stimulus event vectors")
          print("    checking for missing events")
          
          # get the labels from the chartDF
          chartLabels <- unique(chartDF$Label[chartDF$Label != ""])
          
          # check the chart data frame with the events data frame
          missingEvents <- which(!(allEventsChart %in% chartLabels))
          
          if(length(missingEvents) > 0) {
            o=1
            for (o in 1:length(missingEvents)) {
              # determine the chartDF row to add the missing event
              missingEventRow <- min(which(chartDF$Sample >= chartEventsDF$Begin[missingEvents[o]]))
              # add the missing event to the chartDF
              chartDF$Label[missingEventRow] <- chartEventsDF$Label[missingEvents[o]]
            } # end for loop over missing events in chartDF$Label
          } # end if for missing events
          
          # 3-23-2017 add the eventLabel data to the chartDF
          # 4-5-2017 fixed problem with imported charts - indices exceed nrow
          
          ## check the last event ##
          
          lastEventNumber <- nrow(chartEventsDF)
          
          lastBegin <- chartEventsDF$Begin[lastEventNumber]
          lastEnd <- chartEventsDF$End[lastEventNumber]
          lastAnswer <- chartEventsDF$Answer[lastEventNumber]
          
          # make a vector of indices for the last event
          lastEvent <- as.numeric(c(lastBegin, lastEnd, lastAnswer))
            
        } # end work with events
        
        #### check for answer prior to stimulus offset  ####
        
        # View(chartEventsDF)
        # View(chartDF)
        
        if(length(which(chartEventsDF$Answer <= chartEventsDF$End)) > 0) {
          
          # only if any of the answers are before the end of question
          
          print("    checking the location of verbal answers" )
          
          # check for answers prior to question end
          # get the event rows in the events data frame
          theseEvents <- which(chartEventsDF$Answer <= chartEventsDF$End)
          # this can happen on the Axciton
          # if the examiner taps the answer 
          # prior to tapping the end of question
          
          # get the row indices for the chart data frame
          theseAnswerRows <- chartEventsDF$Answer[theseEvents]
          theseEndRows <- chartEventsDF$End[theseEvents]
          
          #### Aug 27, 2020 ####
          #### causes a problem for imported charts ####
          #### when the answer for a question is at the question end ####
          
          ## need to fix both the eventDF and the chartDF ##
          
          # which(chartDF$Label %in% c("NO", "YES", "ANS"))
          # chartEventsDF$Answer[theseEvents]
          # chartEventsDF$Answer[!is.na(chartEventsDF$Answer)]
          
          chartDFRows <- nrow(chartDF)
          eventLabels <- chartEventsDF$Label
          chartDFLabels <- chartDF$Label
          
          # construct a data frame to compare each 3 rows in chartDF
          
          check1 <- which(chartDF$Label[1:(chartDFRows-2)] %in% eventLabels)
          check2 <- which(chartDF$Label[2:(chartDFRows-1)] %in% eventLabels)
          check3 <- which(chartDF$Label[3:chartDFRows] %in% eventLabels)
          
          checkDF <- cbind.data.frame(chartDFLabels[check1], 
                                      chartDFLabels[check2], 
                                      chartDFLabels[check3])
          
          # View(checkDF)
          
          # a private function to check each row
          # for answer or annotation inside the question
          compareFn <- function(x) {
            # output a TRUE or FALSE value for each row
            x[1] == x[3] && x[1] != x[2]
          }
          
          # call the function to get the rows from checkDF
          fixTheseRows <- which(apply(checkDF, 1, compareFn))
          
          # use the rows from checkDF to get the rows in the chartDF
          # use check2 to locate the middle of each group of 3 rows
          fixRows <- check2[fixTheseRows]
          # use the preciding row value as the replacement value
          replacementRows <- fixRows - 1
          
          # replace the rows 
          # where an answer is entered prior to the end of a question
          chartDF$Label[fixRows] <- chartDF$Label[replacementRows]
          
          ####
          
          # pass the chartEventsDF back now that it has changed
          eventDF[useRows,] <- chartEventsDF
          # assign(eventDFName, eventDF, envir=.GlobalEnv)
          
          # pass the chartDF back after changing it
          seriesDF[chartRows,] <- chartDF
          examDF[seriesRows,] <- seriesDF
          assign(paste0(examName, "_Data"), examDF, pos=1)
          
        } # end check for answer prior to question offset
        
        ######## work with the stimulus onset ########
         
        if(nrow(chartEventsDF) != 0) {
          
          {
            
            # get the stim question labels and onset rows
            
            # make a vector of event labels from the chart data frame
            # 3-23-2017
            uniqueEvents <- chartDF$Label
            # don't use chartDF$eventLabel for this
            # uniqueEvents <- chartDF$eventLabel
            
            # keep only the first index for each unique event onset
            for(l in 2:length(uniqueEvents)) {
              # use the chartDF$Label because it remains un-altered by this loop
              if(chartDF$Label[l] == chartDF$Label[l-1]) { uniqueEvents[l] <- "" }
              # 3-23-2017 
              # if(chartDF$eventLabel[l] == chartDF$eventLabel[l-1]) uniqueEvents[l] <- ""
            } 
            
            # now make a vector of questionlabels
            stimQuestionOnset <- which(uniqueEvents != "")
            # chartDF$Label[stimQuestionOnset]
            
            # Oct 2, 2023 removed , "Y", "N", "y", "n" because "Y" is an annotations
            answerWords <- c("YES", "Yes", "yes", "NO", "No", "no", "ANS", "Ans", "ans")
            
            # exclude YES, NO and ANS answers
            stimQuestionOnset <- 
              stimQuestionOnset[which(!(chartDF$Label[uniqueEvents!=""] %in% answerWords))]
                                      
            # length(stimQuestionOnset)
            # chartDF$Label[stimQuestionOnset]
            
            # get the question labels
            # 3-23-2017
            # stimQuestions <- chartDF$eventLabel[stimQuestionOnset]
            stimQuestions <- chartDF$Label[stimQuestionOnset]
            
            # stop if questions or annotations have been lost
            if(length(stimQuestions) != nrow(chartEventsDF)) {
              print(paste("stimQuestions:", paste(stimQuestions, collapse = " ")))
              print(paste("chartEvents:  ", paste(chartEventsDF$Label, collapse = " ")))
              stopMsg <- paste("missing event - ",
                               "check the event rows in the chartDF for ",
                               examName,
                               chartName)
              View(chartDF)
              stop(stopMsg)
            }
            # events may become lost during decimation
            # if a single row event is on a discarded row
            # such as answers and annotations
            
            #### we now have vectors for
            # uniqueEvents # the Label column in the chartDF
            # stimQuestionOnset # row index for qeustion onset
            # stimQuestions # this is the question label
            
          }
          
          #### remove repeated events ####
          
          {
            
            # remove the second of repeated events 
            # that occur when an annotation is entered during an event
            # terminate the event at the annotation
            
            # initialize a vector of items to remove
            removeEvents <- NULL
            # make a vector of events to remove
            if(length(stimQuestions) >= 3) {
              # use a loop to build the vector of repeated events
              p=3
              for(p in 3:length(stimQuestions)) {
                # compare each stim quesiton to the one 2 positions before
                if(stimQuestions[p] == stimQuestions[(p-2)]) {
                  
                  if(stimQuestionOnset[(p-1)] >= (stimQuestionOnset[p]-1))
                    removeEvents <- c(removeEvents, p)
                }
              }
            }
            
            # use the removeEvents vector to remove the repeated events
            # if(length(removeEvents) > 0) {
            if(!is.null(removeEvents)) {
              print(paste("annotation during event removing duplicate", removeEvents))
              stimQuestions <- stimQuestions[-removeEvents]
              stimQuestionOnset <- stimQuestionOnset[-removeEvents]
            }
            
            # print(chartDF$eventLabel[stimQuestionOnset])
            print(chartDF$Label[stimQuestionOnset])
            
            # no need to add the onset of all events to the eventLabel column in the chartDF
            # because they now are added earlier
            
          }
          
          #### check that stimulus events in chartDF == chartEventsDF ####
          
          if( length(stimQuestionOnset) != length(chartEventsDF$Label) ) {
          
            # View(chartEventsDF$Label)
            
            print("problem with question labels")
            
            # initialize a newChartEventsDF 
            newChartEventsDF <- NULL
            
            # initialize a vector of events in the chartDF
            chartDFEvents <- chartDF$Label[stimQuestionOnset]
            
            # initialize a subset the chartEventsDF to use in the loop
            subChartEventsDF <- chartEventsDF
            
            # iterate over the chartDFEvents to build up the newChartEvents
            # iterate over the chartEventsDF$Label
            o=23
            # for (o in 1:length(chartDFEvents)) {
            for (o in 1:3) {
              #### 2019 June 28 - unsure if this is work correctly ####
              # View(subChartEventsDF)
              # get the current stimulus event
              thisEvent <- chartDFEvents[o]
              # get the row number for this event
              eventRow <- which(subChartEventsDF$eventLabel == thisEvent)[1]
              # add the row to the newChartEventsDF
              newChartEventsDF <- rbind(newChartEventsDF, subChartEventsDF[eventRow,])
              # exit the loop without reducing the subset if the eventRow is already the last row
              if(eventRow == nrow(subChartEventsDF)) break()
              # reduce the events in subChartEventsDF
              # keeping only the rows after the eventRow
              subChartEventsDF <- subChartEventsDF[(eventRow+1):nrow(subChartEventsDF),]
            } # end for o loop over chartDFEvents
            
            # now the newChartEventsDF has rows == length(chartDFEvents)
            
            # replace the chartEventsDF with the newChartEventsDF
            chartEventsDF <- newChartEventsDF
            # View(chartEventsDF)
            
          } # end if events are equivalent in chartDF and chartEventsDF
          
          #### add the correct onset to the chartEventsDF ####
          
          {
            
            chartEventsDF$Begin <- stimQuestionOnset
            # View(chartEventsDF)
            
            # check for errors if these lengths are different
            # chartEventsDF$Begin
            # stimQuestionOnset
            
            # pass the chartEventsDF back now that it has changed
            eventDF[useRows,] <- chartEventsDF
            # assign(eventDFName, eventDF, envir=.GlobalEnv)
            
            # all events in the chartEventsD are now included in the chartDF
            
          }
          
        } # end work with the stimulus onset
        
        #### work with the stimulus end ####
        
        if(nrow(chartEventsDF) != 0) {
          
          {
            
            # get the end row index for each stimulus question
            
            # make a vector to work on the stimulus end from the chartDF
            uniqueEvents <- chartDF$Label
            # 3-23-2017
            # uniqueEvents <- chartDF$eventLabel
            
            # use a loop to keep a single index for each unique event end
            l=1
            for(l in 1:(length(uniqueEvents)-1)) {
              if(chartDF$Label[l] == chartDF$Label[(l+1)]) uniqueEvents[l] <- ""
              # if(chartDF$eventLabel[l] == chartDF$eventLabel[l+1]) uniqueEvents[l] <- ""
            } # end for loop over uniqueEvents
            
            stimQuestionEnd <- which(uniqueEvents != "")
            
            # Oct 2, 2023 removed "Y" and "N" because "Y" is an annotation
            answerWords <- c("YES", "NO", "ANS")
            
            # exclude verbal answers YES, NO and ANS
            stimQuestionEnd <- 
              stimQuestionEnd[which(!(chartDF$Label[uniqueEvents!=""] %in% answerWords))]
            
            missingQuestions  <- which(!(chartDF$Label[stimQuestionEnd] %in% stimQuestions))
            if(length(missingQuestions > 0)) {
              stop("problem in preProc.R - missing questions")
            }
            # stimQuestions <- chartDF$Label[stimQuestionEnd]
            
            # 3-24-2017 replace with the eventLabel column from chartEventsDF
            # we have already verified that all Labels in chartEventsDR 
            # are in the chartDF$Labels column
            
            missingEvents <- which(!(stimQuestions %in% chartEventsDF$Label))
            if(length(missingEvents) == 0) {
              stimQuestions <- chartEventsDF$eventLabel
            }
            
            # all questions in eventsDF are in the chartDF
            
          }
          
          #### remove the second of repeated event endings #### 
          
          {
            
            # remove the second of repeated events 
            # that occur when an annotation is entered during an event
            # terminate the event at the annotation
            
            # initialize a vector of items to remove
            removeEvents <- NULL
            # use a loop to build a vector of repeated events
            if(length(stimQuestions) >= 3) {
              # removeEvents <- NULL
              for(p in 3:length(stimQuestions)) {
                # compare each quesiton to the one 2 positions before
                if(stimQuestions[p] == stimQuestions[(p-2)]) {
                  
                  if(stimQuestionEnd[(p-1)] >= (stimQuestionEnd[p]-1))
                    removeEvents <- c(removeEvents, p)
                }
              }
            }
            
            # if(length(removeEvents) > 0) {
            if(!is.null(removeEvents)) {
              print(paste("annotation during event removing duplicate", removeEvents))
              stimQuestions <- stimQuestions[-removeEvents]
              stimQuestionEnd <- stimQuestionEnd[-removeEvents]
            }
            
          }
          
          {
            
            # 3-23-2017 for repeated annotations
            # remove duplicated events that occur 
            # when an annotation is entered during a question
            # fixLabels <- which(chartDF$eventLabel[stimQuestionEnd] != "")
            # 3-24-2017 need to correctly handle repeated events
            # fixLabels <- which(chartDF$Label[stimQuestionEnd] != "")
            # fixLabels <- stimQuestions
            # stimQuestions[fixLabels] <- chartDF$eventLabel[stimQuestionEnd][fixLabels]
            # stimQuestions[fixLabels] <- chartDF$Label[stimQuestionEnd][fixLabels]
            
          }
          
          {
            
            # initialize a dummy vector of event labels starting with 1
            eventLabels <- 1
            
            if(length(stimQuestions) > 1) {
              # use a loop to check for repeated eventLabels
              m=2
              for(m in 2:length(stimQuestions)) {
                # if the eventLabel is not a repeat then add it to the vector
                if(!(stimQuestions[m] %in% stimQuestions[1:(m-1)])) {
                  eventLabels <- c(eventLabels, m)
                }
              } # end for
            } # end if
            
            # keep only non-repeated questions
            stimQuestionEnd <- stimQuestionEnd[eventLabels]
            
            # fix the last event end if necessary
            if( length(stimQuestionEnd) != length(stimQuestionOnset) ) {
              stimQuestionEnd <- c(stimQuestionEnd, (nrow(chartDF)-1))
            }
            
            # check that stimQuestionEnd does not exceed the chart length
            while(length(which(stimQuestionEnd >= nrow(chartDF))) > 0) {
              # adjust the end of the last stimulus if necessary
              stimQuestionEnd[max(which(stimQuestionEnd >= 
                                          nrow(chartDF)))] <- nrow(chartDF) - 1
            }
            
          }
          
          {
            
            # add the correct end to the eventDF data frame
            # not yet added to the chartEventsDF
            # eventDF$End[(eventDF$chartName==chartName & eventDF$seriesName==seriesName)] <- stimQuestionEnd
            
            # add the correct end to the chartEventsDF instead
            chartEventsDF$End <- stimQuestionEnd
            # View(chartEventsDF)
            
            # pass the chartEventsDF back now that it has changed
            eventDF[useRows,] <- chartEventsDF
            # assign(eventDFName, eventDF, envir=.GlobalEnv)
            
          }
          
        } # end work with the stimulus end
        
        ####  work on the verbal answer  ####
        
        if(nrow(chartEventsDF) != 0) {
          
          {
            
            # get the answer indices frokm the chartDF
            
            uniqueEvents <- chartDF$Label
            
            # use a loop to keep a single index for each unique event end
            for(l in 1:(length(uniqueEvents)-1)) {
              if(chartDF$Label[l] == chartDF$Label[l+1]) uniqueEvents[l] <- ""
            }
            
            stimQuestions <- which(uniqueEvents != "")
            
            # Oct 2, 2023 removed "Y" and "N" because "Y" is an annotation
            answerWords <- c("YES", "NO", "ANS")
            
            # keep only the YES and NO answers
            verbalAnswer <- 
              stimQuestions[which((chartDF$Label[uniqueEvents!=""] %in% answerWords))]
            
          }
          
          {
            
            # get the indices for the verbal answer
            
            # get the answer indices from the chartEventsDF instead
            answerIndex <- chartEventsDF$Answer
            # answerIndex <- eventDF$Answer[(eventDF$chartName==chartName & eventDF$seriesName==seriesName)]
            
            # work backward to match the answer to the question onset and question end
            for(l in length(answerIndex):1) {
              # get the answer indices greater than the question end index
              iAnswer <- verbalAnswer[which(verbalAnswer > stimQuestionEnd[l])]
              # if none use the question end index as the answer index
              if(length(iAnswer) == 0) {
                answerIndex[l] <- stimQuestionEnd[l] 
              } else {
                # if answerIndex l is the last stimulus event
                if(l == length(stimQuestionOnset)) {
                  # maybe there is a better way but this works
                  answerIndex[l] <- iAnswer[1] 
                } else {
                  # check if the answer index is greater than the stimulus event index
                  if(iAnswer[1] > stimQuestionEnd[l]) {
                    # 10-4-2016 to avoid potential problem with last stim event when there is no XX announcement
                    if(iAnswer[1] < stimQuestionOnset[l+1]) {
                      answerIndex[l] <- iAnswer[1]
                    } else {
                      answerIndex[l] <- stimQuestionEnd[l]
                    }
                  } else {
                    answerIndex[l] <- stimQuestionEnd[l]
                  }
                }
              }
            } # end for loop 
            
            # now the answerIndex vector is complete and ready to add to the eventDF
            # answerIndex
            
          }
          
          {
            
            # add the answerIndex vector to the eventDF Answer column
            # not yet because it is added to the chartEventsDF
            # eventDF$Answer[(eventDF$chartName==chartName & eventDF$seriesName==seriesName)] <- answerIndex
            
            # chartEventsDF$Answer
            # answerIndex
            
            # add the answerIndex vector to the chartEventsDF Answer column instead
            # if(nrow(chartEventsDF) > 0) {
            if(length(answerIndex) == nrow(chartEventsDF)) {
              chartEventsDF$Answer <- answerIndex
            } else stop("problem with answers in preProc.R")
            
            # View(chartEventsDF)
            
            # the chartEventsDF is now corrected
            
            # pass the chartEventsDF back now that it has changed
            eventDF[useRows,] <- chartEventsDF
            # assign(eventDFName, eventDF, envir=.GlobalEnv)
            
          }
          
        } # end work with the verbal answer
        
        ########   build a new corrected eventDF   ########
        
        if(nrow(chartEventsDF) != 0) {
          
          # check for events that are overlapping with a previous event
          
          # newEventDF <- NULL
          
          # need to build a new eventDF
          # newEventDF was initialized earlier in the i loop
          newEventDF <- rbind(newEventDF, chartEventsDF)
          # View(newEventDF)
          
          # check that the first event onset is before the end
          if(chartEventsDF$Begin[1] >= chartEventsDF$End[1]) {
            chartEventsDF$End[1] <- chartEventsDF$Begin[1] + 1
          } 
          
          # check for NA answers
          if(length(which(is.na(chartEventsDF$Answer))) > 0) {
            theseNAAnswers <- which(is.na(chartEventsDF$Answer))
            chartEventsDF$Answer[theseNAAnswers] <- 
              chartEventsDF$End[theseNAAnswers] + 1
          }
          
          # check the first event end is before the answer
          if(chartEventsDF$End[1] >= chartEventsDF$Answer[1]) {
            chartEventsDF$Answer[1] <- chartEventsDF$End[1] + 1
          } 
          
          # then use a loop to check all other events against the previous event
          if(nrow(chartEventsDF)>=2) {
            
            for(n in 2:length(chartEventsDF$Begin)) { 
              # is the onset index less than the previous answer index
              if(chartEventsDF$Begin[n] <= chartEventsDF$Answer[(n-1)]) {
                chartEventsDF$Begin[n] <- chartEventsDF$Answer[(n-1)] + 1
              }
              # is the stimulus onset index greater than the stimulus end index
              if(chartEventsDF$Begin[n] >= chartEventsDF$End[n]){
                chartEventsDF$End[n] <- chartEventsDF$Begin[n] + 1
              } 
              # is the stimulus onset index greater than the answer index
              if(chartEventsDF$End[n] >= chartEventsDF$Answer[n]){
                chartEventsDF$Answer[n] <- chartEventsDF$End[n] + 1
              }
            } # end for loop
            
          } # end if for nrow(chartEventsDF) >= 2
            
            # add the data to the new columns in the chart data frame
          {
            chartDF$Events[chartEventsDF$Begin] <- "onsetRow"
            chartDF$Events[chartEventsDF$End] <- "offsetRow"
            chartDF$Events[chartEventsDF$Answer] <- "answerRow"  
          }
          
          # make sure that event labels are upper case
          {
            chartEventsDF$Label <- toupper(chartEventsDF$Label)
            # chartDF$eventLabel <- toupper(chartDF$eventLabel)
          }
            
          # fix question tags 
          {
            chartDF$Label <- fixTagsFn(chartDF$Label)
            chartDF$eventLabel <- fixTagsFn(chartDF$eventLabel)
          }
          
          # add the info to the eventLable stimText and Answer colums
          # in the chartDF
          {
            # add the event label to the onset row for each stimulus event
            chartDF$eventLabel[as.numeric(chartEventsDF$Begin)] <- chartEventsDF$Label
            # add the stimulus text statement to the onset row for each stimulus event
            chartDF$stimText[as.numeric(chartEventsDF$Begin)] <- chartEventsDF$Statement
            # add the answer to the answer row for each stimulus event
            chartDF$Answer[as.numeric(chartEventsDF$Answer)] <- chartDF$Label[chartEventsDF$Answer]
          }
          
          # pass the chartEventsDF back now that it has changed
          eventDF[useRows,] <- chartEventsDF
          # assign(eventDFName, eventDF, envir=.GlobalEnv)
          
          # pass the chartDF back after changing it
          seriesDF[chartRows,] <- chartDF
          examDF[seriesRows,] <- seriesDF
          assign(paste0(examName, "_Data"), examDF, pos=1)
          
          # View(eventDF)
        
        } # end build new corrected eventDF
        
        #### save the chartDF to the seriesDF ####
        
        {
          
          # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
          
          # save the chartDF directly to the examDF instead
          # useRows <- (chartOnsetRow + seriesOnsetRow - 1):(chartEndRow + seriesOnsetRow - 1)
          useRows <- which(examDF$chartName == chartName & examDF$seriesName == seriesName)
          examDF[useRows,] <- chartDF
          
          # assign(paste0(examName, "_Data"), examDF, pos=1)
          
          ### these next lines could be done in the parent loop but are safer here
          
          # save the examDF to the global environment with the centered data
          # assign(paste0(examName, "_Data"), examDF, pos=1)
          
          # save the eventDF to the global environment
          # assign(paste0(examName, "_Stimuli"), eventDF, pos=1)
          
        }
        
        ####
        
      } # end for loop over each k chart
      
    } # end for loop over each j series
    
    # save the examDF to the global environment with the centered data
    if (makeDF==TRUE) assign(paste0(examName, "_Data"), examDF, pos=1)
    
    # save the eventDF to the global environment
    if (makeDF==TRUE) assign(paste0(examName, "_Stimuli"), newEventDF, pos=1)
    
  } # end for loop over i unique exam names
  
  # print(paste(i, "exams processed"))
  
  # return the last examDF
  if(output==TRUE) { 
    return(examDF) } else { 
      return(paste0(uniqueExams, "_Data"))
    }
  # return(paste0(examName, "_Data"))
  
} # end preProc() function



# preProc(x=uniqueExams, makeDF=TRUE, output=FALSE)

