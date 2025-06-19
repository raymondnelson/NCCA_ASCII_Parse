# R Script to fix the stimulus onset, offset and answer locations in NCCA ASCII output
# March 31, 2024
# Raymond Nelson 
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
  
  # axciton
  type <- "D\\$"
  # lafayette
  # type <- "D\\&"
  
  # setwd("~/Dropbox/DATASETS_BACKUP/OSSN60Holdout_ZCT60/OSSN60_NCCAASCII/NCCAASCIIOutputLAF")
  
  if(!exists("NCCAASCIIChartNames")) {
    NCCAASCIIChartNames <- getCharts((x=type))
  }
  
  if(!exists("uniqueExamNames")) {
    uniqueExamNames <- uniqueNames(getCharts((x=type)))
  }
  
  NCCAASCIIChartNames <- getCharts((x=type))
  
  uniqueExamNames <- uniqueNames(getCharts((x=type)))
  
  uniqueSeriesNames <- str_sub(getCharts((x=type)), -5, -5)
  
  uniqueChartNames <- str_sub(getCharts((x=type)), -3, -1)
  
  # nchar("ray")
  
}



####  LOCATE EVENTS ####



{
  
  NCCAASCIIChartNames
  
  
  
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
      
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      
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
    
    print(NCCAASCIIName)
    
    names(eventBeginVc) <- eventLabelsVc
    
    eventBeginVc
    
  } # end locateEventsFn
  
  # 8 9 10 11 12 13 14 15 16 17 18 19 20 24 25 26
  # c(m, ee, ts, ei, sn, sw, bi, ai, TS, DB, tt, ttmm, mm, mmmm, tttt)
  
  # c(17, 40, 41, 42, 43, 45, 50, 53, 55, 57, 58, 59, 64, 65)
  
  NCCAASCIIName <- NCCAASCIIChartNames[1]
  
  
  
  locateEventsFn(NCCAASCIIName)
  
} 



#### STOP ####



stop()



#### CHANGE EVENT LOCATION ####



eventLocFn <- function(NCCAASCIIName, Label, offsetVal=NULL, beginOffset=NULL, endOffset=NULL, answerOffset=NULL) {
  # R function to fix the stimulus onset, offset and answer locations in NCCA ASCII output
  # March 31, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file
  # NCCAASCIIName is not vectorized
  # offsetVALL is the number of time series samples to offset the event with the name "Label"
  #
  # if offsetVall is not NULL it will override inputs for beginOffset endOffset and answerOffset
  #
  # this function will read the NCCA ASCII file
  # then offset the event as specified by the input parameters
  # including the event table and time series data
  # and finally re-write the NCCA ASCII text file
  ####
  
  if(!is.null(offsetVal)) { 
    beginOffset <- offsetVal
    endOffset <- offsetVal
    answerOffset <- offsetVal
  }

  {
    thisNCCAASCIIName <- NCCAASCIIName[1]
    examName <- str_sub(thisNCCAASCIIName, 4, -7)
    seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
    chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
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
    # pad the input search label
    thisLabelText <- str_pad(Label, width=8, pad="-", side="left")
    # locate the time series for for this event
    theseDataRows <- which(LabelColumn == thisLabelText)
    # timeSeriesData
  }
  
  ## locate the event table ##
  
  {
    # nchar("Event    Label      Begin        End     Answer")
    eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
    eventTableEndRow <- tsStartRow - 3
    # slice the evet table
    eventTable <- textLines[eventTableStartRow:eventTableEndRow]
    # isolate the Label column
    eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
    # isolate the Begin column
    BeginColumn <- str_sub(eventTable, 16, 25)
    # isolate the End column
    EndColumn <- str_sub(eventTable, 29, 36)
    # isolate the Answer column
    AnswerColumn <- str_sub(eventTable, 40, 47)
    # locate the row for the input event Label
    thisEventTableRow <- which(eventLabelColumn == Label)
    # eventTable
  }
  
  ## exit if repeated question - Oct 4, 2024 ##
  
  {
    # restrict the operation to the first event
    thisEventTableRow <- thisEventTableRow[1]
    
    if(length(thisEventTableRow) > 1) {
      # stop()
      return("error: repeated question, duplicated question label")
    }
  }
  
  ## locate the event ##
  
  {
    oldBegin <- as.numeric(str_trim(BeginColumn[thisEventTableRow], side="left"))
    oldEnd <- as.numeric(str_trim(EndColumn[thisEventTableRow], side="left"))
    oldAnswer <- as.numeric(str_trim(AnswerColumn[thisEventTableRow], side="left"))
  }
    
  ## compute the new rows ##
  
  {
    newBegin <- oldBegin + beginOffset
    newEnd <- oldEnd + endOffset
    newAnswer <- ifelse(oldAnswer == "",
                        "",
                        oldAnswer + answerOffset)
  }
  
  ## adjust the time series data ##
  
  {
    theseDataRows <- which(LabelColumn == thisLabelText)
    # clear the old Label rows
    timeSeriesData[theseDataRows] <- 
      str_replace(timeSeriesData[theseDataRows], thisLabelText, "--------")
    oldAnswerText <- str_sub(timeSeriesData[oldAnswer], 17, 24)
    # clear the old answer
    timeSeriesData[oldAnswer] <- paste0(str_sub(timeSeriesData[oldAnswer], 1, 16),
                                        "--------",
                                        str_sub(timeSeriesData[oldAnswer], 25, -1))
    # set the new Label rows
    timeSeriesData[newBegin:newEnd] <- 
      str_replace(timeSeriesData[newBegin:newEnd], "--------", thisLabelText)
    # set the new Answer row
    if(!is.na(newAnswer)) {
      timeSeriesData[newAnswer] <- paste0(str_sub(timeSeriesData[newAnswer], 1, 16),
                                          oldAnswerText,
                                          str_sub(timeSeriesData[newAnswer], 25, -1))
    }
  }
  
  ## adjust the event table ##
  
  {
    # eventTable
    # 
    # thisEventTableRow
    # 
    # str_pad(newBegin, width= pad=" ", side="left")
    # eventTable[thisEventTableRow]
    # [1] "08          R1       4181       4230       4251 "
    # nchar("08          R1")
    # [1] 14
    # nchar("05          SW       2578")
    # [1] 25
    # nchar("05          SW       2578       2578   " )
    # [1] 39
    # nchar("05          SW       2578       2578" )
    # [1] 36
    # nchar("14          R3       8556       8625       8639 ")
    # [1] 48
    answerPad <- ifelse((length(newAnswer) > 0 && !is.na(newAnswer)),
                        str_c(str_pad(newAnswer, width=11, pad=" ", side="left"), " "),
                        "   ")
    beginPad <- str_pad(newBegin, width=11, pad=" ", side="left")
    endPad <- str_pad(newEnd, width=11, pad=" ", side="left")
    str_c(beginPad, endPad, answerPad)
    newLine <- str_c(str_sub(eventTable[thisEventTableRow], 1, 14), beginPad, endPad, answerPad)
    eventTable[thisEventTableRow] <- newLine
  }
  
  ## construct the output vector
  
  {
    outputVector <- textLines
    # replace the time series rows
    outputVector[tsStartRow:tsEndRow] <- timeSeriesData
    outputVector[eventTableStartRow:eventTableEndRow] <- eventTable
  }
  
  ## write the lines to the text file ##
  
  {
    # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
    write_lines(outputVector, file=thisNCCAASCIIName)
  }
  paste(examName, seriesName, chartName, Label, "adjusted by", beginOffset, endOffset, answerOffset)
}  # end eventLocFn()



NCCAASCIIChartNames



# NCCAASCIIName <- checkNCCA[2]

# NCCAASCIIName <- repeatEventChartNames[4]

NCCAASCIIName <- NCCAASCIIChartNames[11]



locateEventsFn(NCCAASCIIName)



# eventLocFn(NCCAASCIIName = NCCAASCIIName,
#            Label = "R7",
#            offsetVal = -150,
#            beginOffset = 0,
#            endOffset = (120*2),
#            answerOffset = 60 )



####  RENAME EVENT LABEL ####



eventLabelFn <- function(NCCAASCIIChartNames, Label, newLabel, allQs=FALSE, answer="keep") {
  # R function to change a question label in the NCCA ASCII data
  # March 31, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName can be a vector
  # Label
  # newLabel
  # allQs = FALSE will change only the first presentation of a repeated question
  # answer = "Yes" or "No" will use this answer text regardless of the current answer
  # answer = "keep" will keep the existing answer text
  # answer = "none" will remove the answer
  ####
  
  # initialize a counter
  chartCounter <- 0
  
  # iterate over the vector of NCCA ASCII chart names
  
  # the answer parameter to "none if it is null or missing
  if(any(answer == "", is.null(answer))) answer <- "none"
  
  if(newLabel == "X" || newLabel == "XX") answer <- "none"
  
  i=1
  for(i in 1:length(NCCAASCIIChartNames)) {
    
    print(paste("chart:", i))
    
    rewrite <- FALSE
    
    {
      thisNCCAASCIIName <- NCCAASCIIChartNames[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      print(paste(i, examName))
    }
    
    ## locate and slice the time series data rows ##
    
    {
      # nchar("Sample     Time    Label")
      tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Sample     Time    Label") +  1
      tsEndRow <- length(textLines)
      # slice the time series data
      timeSeriesData <- textLines[tsStartRow:tsEndRow]
      # isolate the Label column
      LabelColumn <- str_sub(timeSeriesData, 17, 24)
      # length(LabelColumn)
      # timeSeriesData
    }
    
    ## locate and slide the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # eventTable
    }
    
    ## locate and slice the question list ##
    
    {
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # questionTable
    }
    
    ## next chart if the old Label is not extant ##
    
    {
      if(length(which(questionLabelColumn %in% Label)) == 0) next()
    }
    
    ## adjust the Label ##
    
    {
      
      ## adjust the question list ##
      
      {
        # locate the Label row in the question list
        ifelse(allQs,
               questionListRow <- which(questionLabelColumn == Label),
               questionListRow <- which(questionLabelColumn == Label)[1] )
        questionLabelColumn[questionListRow] <- newLabel
        # pad the new label for the event table 
        newLabelPad <- str_pad(newLabel, width=8, side="left", pad=" ")
        # newQuestionTableRowText <- paste0(str_sub(questionTable[questionListRow], 1, 6),
        #                                   newLabelPad,
        #                                   str_sub(questionTable[questionListRow], 15, -1) )
        for(j in 1:length(questionListRow)) {
          newQuestionTableRowText <- paste0(str_sub(questionTable[questionListRow[j]], 1, 6),
                                            newLabelPad,
                                            paste0(" ", newLabel) )
          questionTable[questionListRow[j]] <- newQuestionTableRowText
        }
      }
      
      ## adjust the event table ##
      
      {
        # locate the row for the input event Label
        ifelse(allQs,
               eventTableRow <- which(eventLabelColumn == Label),
               eventTableRow <- which(eventLabelColumn == Label)[1] )
        oldBegin <- as.numeric(str_trim(BeginColumn[eventTableRow], side="left"))
        oldEnd <- as.numeric(str_trim(EndColumn[eventTableRow], side="left"))
        oldAnswer <- as.numeric(str_trim(AnswerColumn[eventTableRow], side="left"))
        
        for(j in 1:length(eventTableRow)) {
          # use the newLabelPad from earlier
          eventTable[eventTableRow[j]] <- 
            paste0(str_sub(eventTable[eventTableRow[j]], 1, 6),
                   newLabelPad,
                   str_sub(eventTable[eventTableRow[j]], 15, -1) )
        }
        
        if(answer == "keep") {
          # "keep" will keep the existing answer
          # any answer text of length == 0 charcters will result in removal of the answer
          for(j in 1:length(eventTableRow)) {
            eventTable[eventTableRow[j]] <- str_sub(eventTable[eventTableRow[j]], 1, 49)
          }
        } else if(answer == "none") {
          # "none" will remove the answer
          for(j in 1:length(eventTableRow)) {
            eventTable[eventTableRow[j]] <- str_sub(eventTable[eventTableRow[j]], 1, 39)
          }
        }
      }
      
      ## adjust the event Label in the time series data rows ##
      
      {
        # locate the time series for for this event
        theseDataRows <- NULL
        # theseDataRows <- which(LabelColumn == thisLabelText)
        # do it this way to avoid problems when a question is repeated
        ifelse(allQs,
               {
                 for(j in 1:length(oldBegin)) {
                   theseDataRows <- c(theseDataRows, c(oldBegin[j]:oldEnd[j]))
                 }
                 theseDataRows 
               },
               theseDataRows <- c(oldBegin[1]:oldEnd[1]) )
        # pad the input search label
        newLablPad2 <- str_pad(newLabel, width=8, pad="-", side="left")
        for(j in 1:length(theseDataRows)) {
          timeSeriesData[theseDataRows[j]] <- 
            str_c(str_sub(timeSeriesData[theseDataRows[j]], 1, 16),
                  newLablPad2,
                  str_sub(timeSeriesData[theseDataRows[j]], 25, -1) )
        }
        
        # head(timeSeriesData[theseDataRows])
        
        # head(str_c(str_sub(timeSeriesData[theseDataRows], 1, 16),
        #            newLablPad2,
        #            str_sub(timeSeriesData[theseDataRows], 25, -1) ))
        # 
        
      }
      
      ## adjust the answer text in the time series data ##
      
      {
        if(answer != "keep" && answer != "none") {
          # change the answer
          # pad the new answer if the answer is not "keep"
          # NULL will keep the existing answer
          newAnswerPad <- str_pad(answer, width=8, side="left", pad="-")
          ifelse(allQs,
                 tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow])),
                 tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow]))[1] )
          # set the new answer rows
          for(j in 1:length(tsAnswerRow)) {
            timeSeriesData[tsAnswerRow[j]] <- 
              paste0(str_sub(timeSeriesData[tsAnswerRow[j]], 1, 16),
                     newAnswerPad,
                     str_sub(timeSeriesData[tsAnswerRow[j]], 25, -1) )
          }
        } else if(answer != "keep" && answer == "none") {
          # "none" will remove the answer
          newAnswerPad <- "--------"
          ifelse(allQs,
                 tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow])),
                 tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow]))[1] )
          # set the new answer rows
          for(j in 1:length(tsAnswerRow)) {
            timeSeriesData[tsAnswerRow[j]] <- 
              paste0(str_sub(timeSeriesData[tsAnswerRow[j]], 1, 16),
                     newAnswerPad,
                     str_sub(timeSeriesData[tsAnswerRow[j]], 25, -1) )
          }
        } else {
          # keep the old verbal answer
          ifelse(allQs,
                 tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow])),
                 tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow]))[1] )
          # no need to do anything
          # timeSeriesData[tsAnswerRow]
        }
      }
      
    }
    
  ## construct the output vector ##
  
  {
    outputVector <- textLines
    # replace the time series rows
    outputVector[tsStartRow:tsEndRow] <- timeSeriesData
    # replace the event table
    outputVector[eventTableStartRow:eventTableEndRow] <- eventTable
    # replace the question list
    outputVector[questionListStartRow:questionListEndRow] <- questionTable
    
  }
  
  ## write the data to the NCCA ASCII file
  
  {
    # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
    write_lines(outputVector, file=thisNCCAASCIIName)
  }
  
  print(paste(examName, seriesName, chartName, "question label", Label, "changed to", newLabel))
  
  chartCounter <- chartCounter + 1
  
  } # end i loop
  
  return (paste("Question labels changed in", chartCounter, "charts:", Label, "changed to", newLabel))
  
} # end eventLabelFn()



NCCAASCIIChartNames



# locateEventsFn(NCCAASCIIChartNames[3])



# NCCAASCIIName <- repeatEventChartNames[18]



NCCAASCIIName <- NCCAASCIIChartNames[1]



locateEventsFn(NCCAASCIIName)



# locateEventsFn(checkNCCA[1])



# eventLabelFn(NCCAASCIIChartNames = NCCAASCIIChartNames,
#            Label = "1OR",
#            newLabel = "R10",
#            allQs = TRUE,
#            answer = "No" )


# March 15 fixed I1 labels for the Marin Sample
# March 15 fixed KK labesl for 2 charts "D$-X$$5FUNUC-X.01A" "D$-X$$5FUNUC-X.02A"
# March 15 fixed YY labesl for 2 charts "D$-X$$5FUNUC-X.01A" "D$-X$$5FUNUC-X.02A"
# March 15 fixed SS labesl for 2 charts "D$-X$$5FUNUC-X.01A" "D$-X$$5FUNUC-X.02A"
# fixed ot, os, db, wr, wrq, m, sn, c, ct, C4C, C6C, C9C tt mm z zz ei pw pwq



### REMOVE EVENT ####



eventRemoveFn <- function(NCCAASCIIName, Label) {
  # R function to remove a stimulus onset, offset and answer locations in NCCA ASCII output
  # March 31, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file
  # NCCAASCIIName can be vectorized
  #
  # this function will read the NCCA ASCII file
  # then offset the event as specified by the input parameters
  # including the event table and time series data
  # and finally re-write the NCCA ASCII text file
  #
  # removes only the first of repeated events
  ####
  
  chartCounter <- 0
  
  i=1
  for(i in 1:length(NCCAASCIIName)) {
    
    {
      thisNCCASCIIName <- NCCAASCIIName[i]
      examName <- str_sub(thisNCCASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCASCIIName, -3, -1))
      
      print(paste(i, thisNCCASCIIName))
    }
    
    ## read the NCCA ASCII data ##
    
    # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
    textLines <- readLines(thisNCCASCIIName)
    
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
      # pad the input search label
      thisLabelText <- str_pad(Label, width=8, pad="-", side="left")
      # locate the time series for for this event
      theseDataRows <- which(LabelColumn == thisLabelText)
      tsHeaderRow <- textLines[(tsStartRow-1)]
      # timeSeriesData
    }
    
    ## locate the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the evet table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # locate the row for the input event Label
      thisEventTableRow <- which(eventLabelColumn == Label)[1]
      oldBegin <- as.numeric(str_trim(BeginColumn[thisEventTableRow], side="left"))[1]
      oldEnd <- as.numeric(str_trim(EndColumn[thisEventTableRow], side="left"))[1]
      oldAnswer <- as.numeric(str_trim(AnswerColumn[thisEventTableRow], side="left"))[1]
      # eventTable
    }
    
    ## exit if repeated question - Oct 4, 2024 ##
    
    if(length(thisEventTableRow) > 1) {
      # stop()
      # return(paste("error: repeated question, duplicated question label", i, examName))
      print(paste("repeated event", i, examName))
      thisEventTableRow <- thisEventTableRow[1]
      
    }
    
    ## missing event ##
    
    if(length(thisEventTableRow) == 0) {
      print("event not found")
      next()
    }
    
    ## locate the question list ##
    
    {
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # locate the table row for the event 
      thisQuestionTableRow <- which(questionLabelColumn == Label)[1]
      # questionTable
    }
    
    ## adjust the time series data ##
    
    {
      theseDataRows <- c(oldBegin:oldEnd)
      # clear the old Label rows
      timeSeriesData[theseDataRows] <- 
        str_replace(timeSeriesData[theseDataRows], thisLabelText, "--------")
      if(!is.na(oldAnswer)) {
        oldAnswerText <- str_sub(timeSeriesData[oldAnswer], 17, 24)
        # clear the old answer
        timeSeriesData[oldAnswer] <- paste0(str_sub(timeSeriesData[oldAnswer], 1, 16),
                                            "--------",
                                            str_sub(timeSeriesData[oldAnswer], 25, -1))
      }
    }
    
    ## adjust the event table ##
    
    {
      ## delete the line from the table
      eventTable <- eventTable[-thisEventTableRow]
    }
    
    ## adjust the question list ##
    
    {
      # locate the Label row in the question list
      questionListRow <- which(questionLabelColumn == Label)[1]
      questionTable <- questionTable[-questionListRow]
    }
    
    ## construct the output vector
    
    {
      outputVector <- textLines[1:(questionListStartRow-1)] 
      outputVector <- c(outputVector, questionTable, " ", "Event    Label      Begin        End     Answer" )
      outputVector <- c(outputVector, eventTable, " ", tsHeaderRow)
      outputVector <- c(outputVector, timeSeriesData) 
    }
    
    ## adjust the number of events ##
    
    {
      # row 9
      # nchar("Number of questions: ")
      # [1] 21
      # length(eventTable)
      outputVector[9] <- str_c("Number of questions: ", length(eventTable))
    }
    
    ## write the lines to the text file ##
    
    {
      # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
      write_lines(outputVector, file=thisNCCASCIIName)
    }
    
    print(paste(Label, "event now removed from", examName, seriesName, chartName))
    
    chartCounter <- chartCounter + 1
    
  } # end i loop
  
  return(paste("Event", Label, "removed from", chartCounter, "charts."))
  
} # end eventRemoveFn()



NCCAASCIIChartNames



# NCCAASCIIName <- repeatEventChartNames[14]



# NCCAASCIIName <- checkXXX[1]



# names( locateEventsFn("D$-X$$46CUN4-X.01A") )



NCCAASCIIName <- NCCAASCIIChartNames[1]



locateEventsFn(NCCAASCIIName)



# eventRemoveFn(NCCAASCIIName = NCCAASCIIName,
#               Label = "I1a")


# [1] "D$-X$$87I$XF-X.02A" .03A
# X   I1a   SA2    E3    C4    R5    C6     m    R7    E8    C9   R10    SS    KK    YY    XX 
# 3961  7201 10201 13201 16321 19441 22561 25081 26161 29401 32401 35521 38641 41641 44881 47881 


# "CCC" 
# [1] "D$-X$$4%WA#P-X.02A"

# [1] "D$-X$$48JV$M-X.01A"
# X   I1a    I1    I1    I1    R5    C6    R7    E8    C9   R10    XX 
# 2178  4092  7484 10965 14524 17894 21419 24828 28172 31576 34892 38130 



#### CHECK FOR REPEATED EVENTS ####



fixRepeatedEventsFn <- function(NCCAASCIIName) {
  # R function to fix the Labels for repeated events in the NCCA ASCII data
  # October 5, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName can be a vector
  ####
  
  # initialize a counter
  chartCounter <- 0
  
  repeatedEventsVc <- NULL
  
  # iterate over the vector of NCCA ASCII chart names
  
  i=1
  for(i in 1:length(NCCAASCIIName)) {
    
    {
      thisNCCAASCIIName <- NCCAASCIIName[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      print(paste(i, examName))
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
      # timeSeriesData
    }
    
    ## locate the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # eventTable
    }
    
    ## locate the question list ##
    
    {
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # questionTable
    }
    
    ## get the unique events for this chart ##
    
    {
      allEvents <- questionLabelColumn[questionLabelColumn != ""]
      uniqueEvents <- unique(allEvents)
    }
    
    ## check for repeated events ##
    
    {

      # repeatedEventsVc <- NULL

      for(j in 1:length(uniqueEvents)) {
        repeatedQs <- allEvents[which(allEvents %in% uniqueEvents[j])]
        if(length(repeatedQs) > 1) {
          # repeatedEventsVc <- c(repeatedEventsVc, repeatedQs)
          repeatedEventsVc <- c(repeatedEventsVc, thisNCCAASCIIName)
        }
      }

      # add the exam name to an output vector
      repeatedEventsVc <- unique(repeatedEventsVc)

      # next chart if no repeated events
      # if(is.null(repeatedEventsVc)) next()

    }
    
    ## use the allEvents vector to change the question labels ###
    
    {
      
      # # initialize a new vector of all events for the new labels
      # allEventsNew <- allEvents
      # 
      # # iterate backward over the all events vector to adjust the labels
      # l=1
      # for(l in c((length(allEvents)-1):1)) {
      #   fixThese <- which((allEvents[l:length(allEvents)] %in% allEvents[l]))
      #   if(length(fixThese) < 2) next()
      #   fixThese0 <- fixThese[c(1:(length(fixThese)-1))]
      #   allEventsNew[l:length(allEvents)][fixThese0] <- 
      #     paste0(allEventsNew[l:length(allEvents)][fixThese0], "a")
      # }
      # # allEventsNew
      # 
      # # use the allEventsNew vector to handle multiple repetitions 
      
    }
    
    ## rename the repeated events ##
    
    {
      
      # # length(allEvents)
      # # length(allEventsNew)
      # 
      # for(k in 1:length(allEvents)) {
      #   if(allEvents[k] == allEventsNew[k]) next()
      #   # call a function to do this
      #   eventLabelFn(NCCAASCIIName = NCCAASCIIName[i],
      #                Label = allEvents[k],
      #                newLabel=allEventsNew[k],
      #                allQs=FALSE,
      #                answer="keep")
      #   # data are written to the NCCA ASCII text file by the eventLabelFn
      #   chartCounter <- chartCounter + 1
      #   
      # }
      
    }
    
    # ## write the data to the NCCA ASCII file
    # 
    # {
    #   # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
    # }
    # 
    # # print(paste(examName, seriesName, chartName, "question label", Label, "changed to", newLabel))
    # 
    # # chartCounter <- chartCounter + 1
    
  } # end i loop
  
  print(paste("Question labels change in", chartCounter, "charts"))
  
  unique(repeatedEventsVc)
  
} # end fix repeated events 



NCCAASCIIChartNames



# repeatEventChartNames <- fixRepeatedEventsFn(NCCAASCIIName=NCCAASCIIChartNames)



# locateEventsFn(repeatEventChartNames[16])



# c(5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 19, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
# 38, 45, 46, 47, 48, 49, 50, 54, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72,
# 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90) 

# c(14, 17, 18, 20, 21, 37, 39, 40, 41, 42, 43, 44, 52, 53, 55, 56, 92)


# file.copy(paste0("../", repeatEventChartNames), ".")

  
# SKY 40 

# ot, ee ct, wr, db, m, mv, mvt


#### CHECK FOR X and XX ANNOUNCEMENTS ####



checkXXXFn <- function(NCCAASCIIName, check="both") {
  # R function to check for an input question Label in the NCCA ASCII data
  # March 4, 2025
  # Raymond Nelson 
  ####
  # NCCAASCIIName can be "X" or "XX" or "both"
  # check 
  ####
  
  # initialize a counter
  chartCounter <- 0
  
  outputVc <- NULL
  
  # iterate over the vector of NCCA ASCII chart names
  
  i=1
  for(i in 1:length(NCCAASCIIName)) {
    
    {
      thisNCCAASCIIName <- NCCAASCIIName[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      print(paste(i, examName))
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
      # timeSeriesData
    }
    
    ## locate the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # eventTable
    }
    
    ## locate the question list ##
    
    {
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # questionTable
    }
    
    ## get the unique events for this chart ##
    
    {
      allEvents <- questionLabelColumn[questionLabelColumn != ""]
      uniqueEvents <- unique(allEvents)
    }
    
    ## check for X and XX annoucements ##
    
    {
      
      if(check == "X") {
        if(allEvents[1] != "X") {
          chartCounter <- chartCounter + 1
          outputVc <- c(outputVc, thisNCCAASCIIName)
        }
      } else if(check == "XX") {
        if(allEvents[length(allEvents)] != "XX") {
          chartCounter <- chartCounter + 1
          outputVc <- c(outputVc, thisNCCAASCIIName)
        }
      } else if(check == "both") {
        if(allEvents[1] != "X" || allEvents[length(allEvents)] != "XX") {
          chartCounter <- chartCounter + 1
          outputVc <- c(outputVc, thisNCCAASCIIName)
        }
      }
      
    }
    
  } # end i loop
  
  print(paste("Problem announcements found in", chartCounter, "charts"))
  
  unique(outputVc)
  
} # end check X and XX announcements 



NCCAASCIIChartNames



# checkXXX <- checkXXXFn(NCCAASCIIName=NCCAASCIIChartNames, check="both")



# locateEventsFn(checkXXX[4])



#### EVENT INSERT  ####



eventInsertFn <- function(NCCAASCIIName, Label, beginIdx=NULL, endDx=NULL, answerDx=.5, answerTxt="Yes") {
  # R function to insert an event into the NCAA ASCII time series data
  # March 31, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName is the name of an NCCA ASCII text file
  # NCCAASCIIName is not vectorized
  # Label is the question ID for the inserted event
  # beginIdx is the sample row where the new event begins
  # endDx is the distance in seconds to the end of the new event
  # endDx=NULL will randomiz the stimulus length
  # use endDx=0 for annotations
  # answerDx is the distance or time length in seconds to the verbal answer
  # answer distance will be subject to some randomization
  # use answerDx=NULL for annotations
  # answerText is the character string in the time series data
  # 
  # this function will read the NCCA ASCII file
  # then insert the event as specified by the input parameters
  # including the event table and time series data
  # and finally re-write the NCCA ASCII text file
  ####
  
  {
    examName <- str_sub(NCCAASCIIName[1], 4, -7)
    seriesName <- as.character(str_sub(NCCAASCIIName[1], -5, -5))
    chartName <- as.character(str_sub(NCCAASCIIName[1], -3, -1))
    textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
  }
  
  ## get the event location ##
  
  {
    
    # randomize the length of the event a little
    endDx0 <- ifelse(!exists("endDx") || is.null(endDx), 
                     sample(c(75:95), 1),
                     sample(c(((endDx*cps)-5):((endDx*cps)+5)), 1) )
    
    if(endDx0 < 1) endDx0 <- 1
    
    endIdx <- ifelse(endDx==0, beginIdx, (beginIdx + endDx0) - 1)
    
    # randomize the distance to answer a little
    answerDx0 <- ifelse((!exists("answerDx") || is.null(answerDx)),
                        1,
                        sample(c(((answerDx * cps)-4):((answerDx * cps)+4)), 1) )
    
    answerIdx <- (endIdx + answerDx0) - 1 
    
    answerTxt0 <- ifelse(is.null(answerDx), 
                         "", 
                         str_pad(answerTxt, width=8, pad="-", side="left") )
      
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
  
  ## locate the event table ##
  
  {
    # nchar("Event    Label      Begin        End     Answer")
    eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
    eventTableEndRow <- tsStartRow - 3
    # slice the evet table
    eventTable <- textLines[eventTableStartRow:eventTableEndRow]
    # isolate the Label column
    eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
    # isolate the Begin column
    BeginColumn <- str_sub(eventTable, 16, 25)
    # isolate the End column
    EndColumn <- str_sub(eventTable, 29, 36)
    # isolate the Answer column
    AnswerColumn <- str_sub(eventTable, 40, 47)
    # eventTable
    
    # # locate the row for the input event Label
    # thisEventTableRow <- which(eventLabelColumn == Label)[1]
    # oldBegin <- as.numeric(str_trim(BeginColumn[thisEventTableRow], side="left"))[1]
    # oldEnd <- as.numeric(str_trim(EndColumn[thisEventTableRow], side="left"))[1]
    # oldAnswer <- as.numeric(str_trim(AnswerColumn[thisEventTableRow], side="left"))[1]
  }
  
  ## locate the question list ##
  
  {
    # nchar("Event    Label Statement")
    # [1] 24
    questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
    questionListEndRow <- eventTableStartRow - 3
    # slice the evet table
    questionTable <- textLines[questionListStartRow:questionListEndRow]
    # isolate the Label column
    questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
    # this will include blank rows when the question text is wrapped
    # questionTable
  }
  
  ## set the new event label ##
  
  {
    # pad the input search label
    thisLabelText <- str_pad(Label, width=8, pad="-", side="left")
    # locate the time series data rows for for this event
    theseDataRows <- which(LabelColumn == thisLabelText)
    # stop if error
    if(length(theseDataRows) > 0) {
      return("this event already exists")
    }
  }
  
  ## add the new event to the time series data ##
  
  {
    # initialize a vector of event rows
    theseDataRows <- c(beginIdx:endIdx)
    # add the event
    timeSeriesData[theseDataRows] <- 
      str_replace(timeSeriesData[theseDataRows], "--------", thisLabelText)
    # add the verbal answer if it exists
    if(!is.null(answerDx)) {
      # oldAnswerText <- str_sub(timeSeriesData[oldAnswer], 17, 24)
      # set the answer
      timeSeriesData[answerIdx] <- 
        str_replace(timeSeriesData[answerIdx], "--------", answerTxt0)
    }
  }
  
  ## adjust the event table ##
  
  {
    # the event table is place after the question list in the output file
    eventOnsetRows <- as.numeric(str_trim(BeginColumn, side="left"))
    # locate the event row following the new inserted event index
    thisRow <- which(eventOnsetRows >= beginIdx)[1]
    # construct the new event table row
    newEventRowTxt <- 
      paste0(str_pad(str_pad(thisRow, 2, side="left", pad=0), 6, side="right", pad=" "),
             str_pad(Label, 8, side="left", pad=" "),
             str_pad(beginIdx, 11, side="left", pad=" "),
             str_pad(endIdx, 11, side="left", pad=" "),
             ifelse(is.null(answerDx), "   ", str_pad(answerIdx, 11, side="left", pad=" ")),
             " " )
    # add the row to the event table
    eventTable <- c(eventTable[c(1:(thisRow-1))],
                    newEventRowTxt,
                    eventTable[c(thisRow:length(eventTable))])
    # fix the event numbers
    for(i in 1:length(eventTable)) {
      eventTable[i] <- 
        paste0(str_pad(i, 2, side="left", pad="0"),
          str_sub(eventTable[i], 3, -1) )
    }
  }
  
  
  ## adjust the question list ##
  
  {
    # the question list is before the event table in the output file
    # use the same row as the even table
    # first construct the question row text
    newStatementRowTxt <- paste0(str_pad(str_pad(thisRow, 2, side="left", pad=0), 6, side="right", pad=" "),
                             str_pad(Label, 8, side="left", pad=" "),
                             " ",
                             Label )
    questionTableRows <- as.numeric(str_sub(questionTable, 1, 2))
    # now use the row from the question table
    thisRow0 <- which(as.numeric(str_sub(questionTable, 1, 2)) >= thisRow)[1]
    # make the new table
    questionTable <- c(questionTable[c(1:(thisRow0-1))],
                       newStatementRowTxt,
                       questionTable[c(thisRow0:length(questionTable))] )
    # initialize a scalar for the event numbers
    j=1
    # fix the event numbers 
    for(i in 1:length(questionTable)) {
      if(str_sub(questionTable[i], 1, 2) == "  ") next()
      questionTable[i] <- 
        paste0(str_pad(j, 2, side="left", pad="0"),
               str_sub(questionTable[i], 3, -1) )
      j <- (j+1)
    }
  }
  
  ## construct the output vector
  
  {
    outputVector <- textLines[1:(questionListStartRow-1)] 
    outputVector <- c(outputVector, questionTable, " ", "Event    Label      Begin        End     Answer" )
    outputVector <- c(outputVector, eventTable, " ", tsHeaderRow)
    outputVector <- c(outputVector, timeSeriesData) 
  }
  
  ## adjust the number of events ##
  
  {
    outputVector[9] <- str_c("Number of questions: ", length(eventTable))
  }
  
  ## write the lines to the text file ##
  
  {
    write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
  }
  
  paste(Label, "event ", Label, "now added to from", examName, seriesName, chartName)
  
} # end eventInsertFn()



NCCAASCIIChartNames



NCCAASCIIName <- NCCAASCIIChartNames[1]



locateEventsFn(NCCAASCIIName)



# eventInsertFn(NCCAASCIIName = "D&-MikeATestOrlando-1-1.02A",
#               Label="N5",
#               beginIdx=(3470+900),
#               endDx=NULL,
#               answerDx=.5,
#               answerTxt="Yes" )



## for annotations ##

NCCAASCIIChartNames

# locateEventsFn(NCCAASCIIName=NCCAASCIIChartNames[1])

# eventInsertFn(NCCAASCIIName = "D&-GregTestKaren-1-1.04A",
#               Label="EI",
#               beginIdx=(1246+900),
#               endDx=0,
#               answerDx=NULL,
#               answerTxt=NULL )



#### FIX ANSWER  ####



fixAnswerFn <- function(NCCAASCIIChartNames, Label="E3", answerDx="fix", answerTxt="Yes", maxDx=480, minDx=15) {
  # R function to insert or remove the answers to stimulus events in NCCA ASCII files
  # Oct 3, 2024
  # modified Mar 13, 2025 to fix the min and max answer distance
  # Raymond Nelson
  ####
  # NCCAASCIIChartNames is the name of an NCCA ASCII text file
  # NCCAASCIIChartNames can be a vector
  # Label is the question ID where the answer will be inserted or removed
  # answerDx is the distance (number of samples) from the question offset to the verbal answer
  # answerDx can be a single value or a vector for randomization of the answer distance 
  # answerDx = "keep" will retain the old answer distance
  # answerDX = NULL will keep the old answer distance
  # answerDx = "fix" will replace a missing answer or limit an existing answer to the maxDx-minDix range
  # minDx will adjust the old answer distance if it is not NULL
  # maxDx will adjust the old answer distance if it is not NULL
  # answerTxt is the character string in the time series data,
  # answerTxt = NULL will remove the verbal answer
  # answerTxt = "keep" will retain the current verbal answer at the new time series row
  # answerTxt = "Yes" or "No" or "Ans" will coerce the answer to these words 
  # 
  # default input parameters will do nothing
  #
  # this function will read the NCCA ASCII file,
  # then adjust the verbal answer specified by the input parameters, 
  # including the event table and time series data,
  # Then re-write the NCCA ASCII text file.
  # 
  # this function will stop if an event is repeated
  ####
  
  if(getOption("warn") !=2) {
    # set the warn level to suppress warnings
    if(!exists("oldw")) oldw <- getOption("warn")
    # -1 to suppress warnings
    # 0 is normal, warnings are display at end
    # 1 warnings are display at the time
    # 2 warnings are escalated to errors
    options(warn = 2)
    # reset to default
    # options(warn = 0)
    # rm(oldw)
    print(paste("warn level:", oldw))
    # getOption("warn")
  }
  
  counter <- 0
  
  answerDistanceDAT <- NULL
  
  # a kludgy solution
  answerDxSave <- answerDx
  
  i=1
  for(i in 1:length(NCCAASCIIChartNames)) {
    
    # print(paste("chart:", i))
    
    rewrite <- FALSE
    
    # kludgy fix because answerDx is modified in this function
    answerDx <- answerDxSave
    
    if(is.null(answerTxt)) answerDx <- NULL
    
    ## read the data from theh NCCA ASCII text file ##
    
    {
      thisNCCAASCIIName <- NCCAASCIIChartNames[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      # answerDx <- "fix"
      # answerTxt <- "Yes"
      # # answerTxt <- "No"
      # # answerTxt <- NULL
      # answerDx <- 30
      # Label <- "E3"
      print(paste(i, thisNCCAASCIIName))
    }
    
    ## locate and slice the time series data rows ##
    
    {
      # nchar("Sample     Time    Label")
      tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Sample     Time    Label") +  1
      tsEndRow <- length(textLines)
      # slice the time series data
      timeSeriesData <- textLines[tsStartRow:tsEndRow]
      # isolate the Label column
      LabelColumn <- str_sub(timeSeriesData, 17, 24)
      # length(LabelColumn)
      # pad the input search label
      thisLabelText <- str_pad(Label, width=8, pad="-", side="left")
      # locate the time series for for this event
      theseDataRows <- which(LabelColumn == thisLabelText)
      # timeSeriesData
    }
    
    ## locate and slice the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # locate the row for the input event Label
      thisEventTableRow <- which(eventLabelColumn == Label)
      
      # if the event is not present in the charts
      if(length(thisEventTableRow) == 0) next()
      
      # get the question onset offset and answer indices
      oldBegin <- as.numeric(str_trim(BeginColumn[thisEventTableRow], side="left"))
      oldEnd <- as.numeric(str_trim(EndColumn[thisEventTableRow], side="left"))
      oldAnswer <- as.numeric(str_trim(AnswerColumn[thisEventTableRow], side="left"))
      # oldAnswer wil be NA if the answer is missing
      # eventTable
      
      # if(length(!is.na(oldAnswer)) == 0) stop(i)
    }
    
    ## exit if repeated question - Oct 4, 2024 ##
    
    if(length(thisEventTableRow) > 1) {
      # stop()
      return("error: repeated question, duplicated question label")
    }
    
    ## exit if the answer is blank and is intended to be removed ##
    
    {
      # if(is.na(oldAnswer)) {
      #   next()
      # } else {
      #  # stop()
      # }
    }
    
    ## check the old answer distance ##
    
    {
      
      oldAnswerDx <- oldAnswer - oldEnd



      ## Mar 13, 2025 aggregate the answer distances for the marin sample
      # answerDistanceDAT <- NULL
      answerDistanceDAT <- c(answerDistanceDAT, oldAnswerDx)
      # next()
      
      
      
      if( all( answerDx=="fix", !is.null(minDx), !is.null(maxDx) ) ) {
        
        # only if there is an existing answer 
        # and the input answerDx parameter indicates a goal to keep the existing answer distance
        
        if( !is.na(oldAnswer) ) {
          # if the verbal answer is not missing
          
          if( oldAnswerDx < minDx )  {
            # fix the new answer distance only if it is out of range
            # answerDx0 <- minDx
            # keep it
            answerDx0 <- oldAnswerDx
          } else if( oldAnswerDx > maxDx ) {
          
            # random new distance
            answerDx0 <- sample(c(minDx:maxDx), 1)
            
            
            # answerDx0 <- maxDx
            
            
            # assign("i", i, envir=.GlobalEnv)
            # stop()
            
            
            # remove out of range values
            answerDx0 <- NULL
          } else {
            # keep the old answer distance if it is not out of range
            answerDx0 <- oldAnswerDx
          }
          
        } else {
          # if the verbal answer is missing
          # answerDx0 <- sample(c(minDx:maxDx), 1)
          # keep it NA
          answerDX0 <- NA
        }
        
      } else {
        
        # no answer distance if the answer is missing 
        answerDx0 <- NULL
        
      }
    
    }
    
    ## compute the new answer row ##
    
    {
      
      if(is.null(answerDx0)) {
        newAnswerIdx <- NA
        answerDx0 <- NA
      } else {
        # randomize the distance to the new answer a little
        answerDx0 <- ifelse(is.null(answerDx),
                            NA,
                            # sample(c(((answerDx * cps)-4):((answerDx * cps)+4)), 1)
                            ifelse(length(answerDx) > 1,
                                   # some random variantion if answerDx is a vector
                                   sample(answerDx, 1),
                                   # if answerDx is a single value
                                   ifelse(is.numeric(answerDx),
                                          # because answerDx might be "fix" or NULL
                                          answerDx,
                                          answerDx0 )
                                   # this allows setting the answer distance to a fixed input value
                            )
        )
        
        # initialize the new answer index for the time series data
        newAnswerIdx <- ifelse(is.null(answerDx),
                               NA,
                               (oldEnd + answerDx0) - 0 )
      }
      
    }
    
    ## select the new answer text ##
    
    {
      # save the old answer text
      oldAnswerText <- str_sub(timeSeriesData[oldAnswer], 17, 24)
      # no new answer if answerDx0 is NA when there is no new answer distance
      answerTxt0 <- ifelse(any(is.na(newAnswerIdx), is.null(answerTxt)),
                           NA,
                           answerTxt )
      
      if(is.null(answerTxt)) {
        # answerTxt0 = "--------"
        answerTxt0 <-  NA
        newAnswerIdx <- NA
      } else if(answerTxt=="keep" && !is.na(newAnswerIdx)) {
        # keep the old answer text if extant and if the new answer index is not NA
        # answerTxt0 is already initialized to the new answer text if answerTxt is not NULL
        # answerTxt0 is already inititalized to NA if answerTxt is NULL
        answerTxt0 <- oldAnswerText
        # old answer is either Yes or No
      } else if(all(!is.null(answerTxt), !is.na(newAnswerIdx), answerTxt!="keep")) {
        # use the new answer
        # new answer can be "Yes", "No", or "Ans"
        answerTxt0 <- str_pad(answerTxt, width=8, pad="-", side="left")
        # new answer is padded for the Label column in the time series data
      } else if(is.na(answerTxt0) || is.na(newAnswerIdx)) {
        # no verbal answer if the answerTxt0 is NA at this point
        # answerTxt0 <- "--------"
        answerTxt0 <-  NA
        newAnswerIdx <- NA
      }
    }
    
    ## next chart if there is no change ##
    
    {
      if( !is.na(oldAnswerText) && !is.na(oldAnswer) ) {
        # old answer exists
        if( is.na(newAnswerIdx) || is.na(answerDx0) ) {
          rewrite <- TRUE
        } else if( oldAnswerText == answerTxt0 && oldAnswer == newAnswerIdx ) {
          # no need to rewrite if there is no change
          rewrite <- FALSE
          next()
        }
      } else {
        rewrite <- TRUE
        # no change if the old answer is missin
        next()
      }
    }
    
    ## adjust the time series data ##
    
    if(rewrite) {
      
      newTimeSeriesData <- timeSeriesData
      
      # clear the old answer 
      if( !is.na(oldAnswer) || is.na(answerTxt0) ) {
        newTimeSeriesData[oldAnswer] <- paste0(str_sub(newTimeSeriesData[oldAnswer], 1, 16),
                                            "--------",
                                            str_sub(newTimeSeriesData[oldAnswer], 25, -1))
      }
      
      # no need to clear the old answer text because there is none
      if( !is.na(newAnswerIdx) && !is.na(answerTxt0) ) {
        # set the new Answer at the new answer index
        newTimeSeriesData[newAnswerIdx] <- paste0(str_sub(newTimeSeriesData[newAnswerIdx], 1, 16),
                                            answerTxt0,
                                            str_sub(newTimeSeriesData[newAnswerIdx], 25, -1))
      }
      
    }
    
    ## adjust the event table ##
    
    if(rewrite) {
      newEventTable <- eventTable
      
      # pad the answer onset and offset columns 
      answerPad <- ifelse(!is.na(newAnswerIdx),
                          str_c(str_pad(newAnswerIdx, width=11, pad=" ", side="left"), " "),
                          "   ")
      beginPad <- str_pad(oldBegin, width=11, pad=" ", side="left")
      endPad <- str_pad(oldEnd, width=11, pad=" ", side="left")
      # str_c(beginPad, endPad, answerPad)
      # initialize the event line
      newLine <- str_c(str_sub(newEventTable[thisEventTableRow], 1, 14), beginPad, endPad, answerPad)
      # insert the line into the event table, overwriting the old event line
      newEventTable[thisEventTableRow] <- newLine
    }
    
    ## construct the output vector ##
    
    if(rewrite) {
      
      outputVector <- textLines
      # replace the time series rows
      outputVector[tsStartRow:tsEndRow] <- newTimeSeriesData
      outputVector[eventTableStartRow:eventTableEndRow] <- newEventTable
      
    }
    
    ## write the lines to the text file ##
    
    if(rewrite) {
      
      write_lines(outputVector, file=thisNCCAASCIIName)
      
      print(paste(examName, seriesName, chartName, Label, "Answer was modified"))
      
      counter <- counter + 1
      
    }
    
    # counter <- 0
    
    # answerDistanceDAT <- NULL
    # Label="C6"
    
    # maxDx <- 480
    # minDx <- 12
    
    # answerDx <- "fix"
    # answerDxSave <- answerDx
    
  } # end i loop 
  
  
  # I1
  # > summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.00   39.00   82.00   88.49  120.00  360.00      31 
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.00   44.75   93.00  109.35  120.00  587.00 
  
  # SA2
  # > summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.00   37.00   84.00   87.63  120.00  480.00 
  
  # E3
  # > summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  1.00   87.00   87.00   88.92   89.00  361.00       2 
  
  # E8
  # > summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #   0.0    90.0   100.0   100.4   110.0   329.0       1 

  # c4
  # > summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.00   53.00   98.00   99.26  120.00  480.00      23  
  
  # r5
  # > summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.00   48.00   86.00   89.04  120.00  480.00      27 
  
  # c6
  # summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #   0.0    59.5   113.0    99.6   120.0   360.0      33 
  
  # r7
  # summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.00   30.75   72.00   77.33  120.00  456.00      32 
  
  # c9
  # summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.00   55.00   96.00   95.02  120.00  360.00      27 
  
  # r10
  # summary(answerDistanceDAT)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.00   34.75   81.00   82.96  120.00  358.00      24 
  
  
  
  return(paste("Answers corrected in", counter, "charts."))
  
} # end fixAnswer() function



NCCAASCIIChartNames

NCCAASCIIName <- NCCAASCIIChartNames[1]

locateEventsFn(NCCAASCIIName)



# fixAnswerFn(NCCAASCIIChartNames=NCCAASCIIChartNames,
#             Label="C6",
#             answerDx="fix",
#             answerTxt="No",
#             maxDx=480,
#             minDx=12 ) 



#### MISSING ANSWER  ####



missingAnswerFn <- function(NCCAASCIIChartNames, Label="I1", answerDx=c(12:480), answerTxt="Yes") {
  # R function to fix missing answers to stimulus events in NCCA ASCII files
  # March 3, 2024
  # Raymond Nelson
  ####
  # NCCAASCIIChartNames is the name of an NCCA ASCII text file
  # NCCAASCIIChartNames can be a vector
  # Label is the question ID where the answer will be inserted or removed
  # answerDx is the distance, number of samples, from the question offset to the verbal answer
  # answerDx can be "rand" or a vector to be sampled from
  # answerDx can be a single value
  # answerTxt is the character string in the time series data,
  # answerTxt = NULL will remove the verbal answer
  # answerTxt = "keep" will retain the current verbal answer at the new time series row
  # answerTxt = "Yes" or "No" or "Ans" will coerce the answer to these words 
  ####
  # this function will read the NCCA ASCII file
  # then fix a missing verbal answer specified by the input parameters, 
  # including the event table and time series data.
  # Then re-write the NCCA ASCII text file
  #
  # if an answer is present but differs from the answerTxt input param it will be replaced
  #
  # This function will stop if an event is repeated, 
  # and will not modify an extant answer text or answer distance if answerTxt="keep".
  # Also, it will not modify an existing answer that is the same as the answerTxt inpu
  ####
  
  counter <- 0
  
  
  i=1
  for(i in 1:length(NCCAASCIIChartNames)) {
    
    # print(paste("chart:", i))
    
    {
      thisNCCAASCIIName <- NCCAASCIIChartNames[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      # answerTxt <- "Yes"
      # # answerTxt <- "No"
      # # answerTxt <- NULL
      # answerDx <- 30
      # Label <- "E3"
      
      print(paste(i, examName, seriesName, chartName))
    }
    
    ## locate and slice the time series data rows ##
    
    {
      # nchar("Sample     Time    Label")
      tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Sample     Time    Label") +  1
      tsEndRow <- length(textLines)
      # slice the time series data
      timeSeriesData <- textLines[tsStartRow:tsEndRow]
      # isolate the Label column
      LabelColumn <- str_sub(timeSeriesData, 17, 24)
      # length(LabelColumn)
      # pad the input search label
      thisLabelText <- str_pad(Label, width=8, pad="-", side="left")
      # locate the time series for for this event
      theseDataRows <- which(LabelColumn == thisLabelText)
      # timeSeriesData
    }
    
    ## locate and slice the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # locate the row for the input event Label
      thisEventTableRow <- which(eventLabelColumn == Label)
      # if the event is not present in the charts
      if(length(thisEventTableRow) == 0) next()
      # get the question onset offset and answer indices
      oldBegin <- as.numeric(str_trim(BeginColumn[thisEventTableRow], side="left"))
      oldEnd <- as.numeric(str_trim(EndColumn[thisEventTableRow], side="left"))
      oldAnswer <- as.numeric(str_trim(AnswerColumn[thisEventTableRow], side="left"))
      # oldAnswer wil be NA if the answer is missing
      # eventTable
    }
    
    ## exit if repeated question - Oct 4, 2024 ##
    
    if(length(thisEventTableRow) > 1) {
      # stop()
      print(paste("repeated question:", i, examName))
      return("error: repeated question, duplicated question label")
      
      # thisEventTableRow <- thisEventTableRow[1]
    }
    
    ## pad the new verbal answer ##
    
    {
      answerTxtPad <- str_pad(answerTxt, width=8, side="left", pad="-")
      oldAnswerDx <- oldAnswer - oldEnd
    }
    
    ## if the verbal answer already exists ##
    
    {
      if(!is.na(oldAnswer)) {
        
        # next chart without re-writing this chart
        next()
        
        # if( LabelColumn[oldAnswer] == answerTxtPad ) 
        #   if( oldAnswerDx >= minDx && oldAnswerDx <= maxDx ) {
        #     # no need to fix the answer
        #     next()
        #   } else {
        #     # remove the old answer
        #     LabelColumn[oldAnswer] <- LabelColumn[(oldAnswer-1)]
        #   }
        
      } else {
        oldAnswerDx <- NULL
      } 
      
      # stop for inspection
      # if(is.na(oldAnswer)) stop()
    }
    
    ## set the new answer distance ##
    
    {
      if(answerDx[1]=="rand") {
        answerDx0 <- sample(c(10:90), 1)
      } else if(length(answerDx) > 1) {
        answerDx0 <- sample(answerDx, 1)
      } else if(length(answerDx) == 1) {
        answerDx0 <- answerDx
      } else if(is.null(answerTxt)) {
        answerDx0 <- NULL
      }
    }
    
    ## compute the new answer row ##
    
    {
      # # randomize the distance to answer a little
      # answerDx0 <- ifelse(is.null(answerDx),
      #                     NA,
      #                     # sample(c(((answerDx * cps)-4):((answerDx * cps)+4)), 1)
      #                     answerDx )
      
      # initialize the new answer index in the time series data
      newAnswer <- ifelse(is.null(answerDx0),
                          NA,
                          (oldEnd + answerDx0) - 0 )
    }
    
    ## select the new answer text ##
    
    {
      # save the old answer text
      oldAnswerText <- str_sub(timeSeriesData[oldAnswer], 17, 24)
      
      # # no new answer if answerDX0 is NA when there is no new answer distance
      # answerTxt0 <- ifelse( any(is.na(newAnswer), is.null(answerTxt) ),
      #                      NA,
      #                      answerTxt )
      
      if( any( is.na(newAnswer), is.null(answerTxt) ) ) {
        answerTxt0 <-  NA
        newAnswer <- NA
      } else if( answerTxt=="keep" && !is.na(newAnswer) ) {
        # answerTxt0 is already initialized to the new answer text if answerTxt is not NULL
        # answerTxt0 is already inititalized to NA if answerTxt is NULL
        # keep the old answer text if extant and if the new answer index is not NA
        answerTxt0 <- oldAnswerText
        # old answer is either Yes or No
      } else if( all( answerTxt!="keep", !is.na(newAnswer), !is.null(answerTxt) ) ) {
        # use the new answer
        answerTxt0 <- answerTxtPad
      } 
        
      if(is.na(answerTxt0)) {
        # no verbal answer if the answerTxt0 is NA at this point
        # answerTxt0 <- "--------"
        answerTxt0 <-  NA
        newAnswer <- NA
        # oldAnswer
      }
    }
    
    ## adjust the time series data ##
    
    {
      newTimeSeriesData <- timeSeriesData
      
      if(!is.na(oldAnswer)) {
        # clear the old answer 
        newTimeSeriesData[oldAnswer] <- paste0(str_sub(newTimeSeriesData[oldAnswer], 1, 16),
                                            "--------",
                                            str_sub(newTimeSeriesData[oldAnswer], 25, -1))
      }
      
      if(!is.na(newAnswer) && !is.na(answerTxt0)) {
        # set the new Answer at the newAnswer index
        newTimeSeriesData[newAnswer] <- paste0(str_sub(newTimeSeriesData[newAnswer], 1, 16),
                                            answerTxt0,
                                            str_sub(newTimeSeriesData[newAnswer], 25, -1))
      }
      
    }
    
    ## adjust the event table ##
    
    {
      newEventTable <- eventTable
      # pad the answer onset and offset columns 
      answerPad <- ifelse(!is.na(newAnswer),
                          str_c(str_pad(newAnswer, width=11, pad=" ", side="left"), " "),
                          "   ")
      beginPad <- str_pad(oldBegin, width=11, pad=" ", side="left")
      endPad <- str_pad(oldEnd, width=11, pad=" ", side="left")
      # str_c(beginPad, endPad, answerPad)
      # initialize the event line
      newLine <- str_c(str_sub(eventTable[thisEventTableRow], 1, 14), beginPad, endPad, answerPad)
      # insert the line into the event table, overwriting the old event line
      newEventTable[thisEventTableRow] <- newLine
    }
    
    ## construct the output vector
    
    {
      outputVector <- textLines
      # replace the time series rows
      outputVector[tsStartRow:tsEndRow] <- newTimeSeriesData
      outputVector[eventTableStartRow:eventTableEndRow] <- newEventTable
    }
    
    ## write the lines to the text file ##
    
    {
      # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
      write_lines(outputVector, file=thisNCCAASCIIName)
    }
    
    print(paste(examName, seriesName, chartName, Label, "Answer was modified"))
    
    counter <- counter + 1
    
  } # end i loop
  
  return(paste("Answers corrected in", counter, "charts."))
  
} # end missingAnswer function



NCCAASCIIChartNames



NCCAASCIIName <- NCCAASCIIChartNames[1]



locateEventsFn(NCCAASCIIName)



# missingAnswerFn(NCCAASCIIChartNames=NCCAASCIIChartNames,
#             Label="C4",
#             answerDx=c(12:480),
#             answerTxt="No" ) 



#### CHECK ALL CHARTS FOR A SPECIFIED EVENT LABEL ####



checkEventsFn <- function(NCCAASCIIName, check, inverse=FALSE, position=NULL) {
  # R function to check for an input question Label in the NCCA ASCII data
  # March 4, 2025
  # Raymond Nelson 
  ####
  # NCCAASCIIName can be a vector
  # check is the name of an event
  # check can be a vector
  # inverse = TRUE will output NCCA file names where all events in "check" are not found
  # position can be NULL to check all event positions
  # position can be 1, 2, 3, to check these numbered positions
  # position can be -1, -2, -3, to check backward from the end or last event
  # 
  # output will be a vector of NCCA ASCII file names where the event was found
  ####
  
  # initialize a counter
  chartCounter <- 0
  
  outputVc <- NULL
  
  # iterate over the vector of NCCA ASCII chart names
  
  i=1
  for(i in 1:length(NCCAASCIIName)) {
    
    {
      thisNCCAASCIIName <- NCCAASCIIName[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      print(paste(i, examName))
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
      # timeSeriesData
    }
    
    ## locate the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # eventTable
    }
    
    ## locate the question list ##
    
    {
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # questionTable
    }
    
    ## get the unique events for this chart ##
    
    {
      allEvents <- questionLabelColumn[questionLabelColumn != ""]
      uniqueEvents <- unique(allEvents)
    }
    
    ## check for the specified event ##
    
    {
      
      if(!inverse) {
    
        ## search for an event ##
        
        if(is.null(position)) {
          # search all event positions
          if(any(allEvents %in% check)){
            chartCounter <- chartCounter + 1
            outputVc <- c(outputVc, thisNCCAASCIIName)
            print(thisNCCAASCIIName)
          }
        } else {
          # searching for an event in a specified position 
          if(position < 0) {
            # start at the last event
            thisPosition <- length(allEvents) + (position + 1)
          } else {
            # start at the first event
            thisPosition <- position 
          }
          # check only the first item when searching for specified positions
          if(allEvents[thisPosition] == check[1]) {
            chartCounter <- chartCounter + 1
            outputVc <- c(outputVc, thisNCCAASCIIName)
            print(thisNCCAASCIIName)
          }
        }
        
      } else {
      
        ## search for a missing event ##
        
        if(is.null(position)) {
          # search all positions
          if(any(!(check %in% allEvents))){
            chartCounter <- chartCounter + 1
            outputVc <- c(outputVc, thisNCCAASCIIName)
            print(thisNCCAASCIIName)
          } 
        } else {
          # search for a missing event in a specified position
          if(position < 0) {
            # start at the last event
            thisPosition <- length(allEvents)  +(position + 1)
          } else {
            # start at the first event
            thisPosition <- position 
          }
          # check only the first item when searching for specified positions
          if(allEvents[thisPosition] != check[1]) {
            chartCounter <- chartCounter + 1
            outputVc <- c(outputVc, thisNCCAASCIIName)
            print(thisNCCAASCIIName)
          }
        }
      }
      
    }
      
  } # end i loop
  
  if(inverse) {
    print(paste("Missing events found in", chartCounter, "charts"))
  } else {
    print(paste("Events found in", chartCounter, "charts"))
  }
  unique(outputVc)
  
} # end checkEventsFn()



NCCAASCIIChartNames



# checkNCCA <- checkEventsFn(NCCAASCIIName=NCCAASCIIChartNames,
#                            check = c("K"),
#                            inverse = FALSE,
#                            position=NULL)



# c("C4", "R5", "C6", "R7", "C9", "R10")
# c("C4", "C6", "C9")
# c("R5", "R7", "R10")
# c("I1", "SA2", "E3", "C4", "R5", "C6", "R7", "E8", "C9", "R10")
# c("X", "XX")



# locateEventsFn(checkNCCA[1])



# repeated I1
# [1] "D$-X$$3MIZCR-X.03A" "D$-X$$3NMFG4-X.02A" "D$-X$$3PGWAD-X.02A" "D$-X$$3YUYYE-X.02A"
# [5] "D$-X$$3Z5Q9P-X.01A" "D$-X$$3Z5Q9P-X.03A" "D$-X$$4&Z%ME-X.03A" "D$-X$$4%EECJ-X.01A"
# [9] "D$-X$$4%EECJ-X.02A" "D$-X$$4%WA#P-X.02A" "D$-X$$46CUN4-X.01A" "D$-X$$48JV$M-X.01A"
# [13] "D$-X$$4L72C1-X.02A" "D$-X$$4RY18J-X.02A" "D$-X$$51NZ75-X.02A" "D$-X$$8MFRW6-X.01A"
# [17] "D$-X$$ALSL73-X.03A" "D$-X$$BC9FG9-X.01A" "D$-X$$BC9FG9-X.03A" "D$-X$$BXN1FF-X.02A"
#  "D$-X$$CS#JHI-X.01A" "D$-X$$CS#JHI-X.02A" "D$-X$$CS#JHI-X.03A"
# [25] "D$-X$$CT9QFL-X.02A" "D$-X$$CYXJKC-X.01A" "D$-X$$CYXJKC-X.02A" "D$-X$$CYXJKC-X.03A"
# [29] "D$-X$$D6CH%0-X.01A"



# missing E8
# [1] "D$-X$$CJ90Y#-X.01A" "D$-X$$CJ90Y#-X.02A" "D$-X$$CJ90Y#-X.03A" "D$-X$$CS#JHI-X.01A"
# [5] "D$-X$$CS#JHI-X.02A" "D$-X$$CS#JHI-X.03A" "D$-X$$CYXJKC-X.01A" "D$-X$$CYXJKC-X.02A"
# [9] "D$-X$$CYXJKC-X.03A"



# file.copy(from=paste0("../", checkNCCA), to=".")



# missing R10
# [1] "D$-X$$CJ90Y#-X.01A" .02A .03A
# X   I1a   SA2    E3    C4    R5    C6    R7    C8    R9    10    XX 
# 1921  5881  9001 12121 15601 18841 21961 25081 28321 31561 34681 37921 



# locateEventsFn(checkXXX[3])



########  CHANGE TO ANNOTATION  ########



annotationFn <- function(NCCAASCIIName, Label, newLabel, allQs=FALSE) {
  # R function to change a question label in the NCCA ASCII data
  # March 31, 2024
  # Raymond Nelson 
  ####
  # NCCAASCIIName can be a vector of NCCA file names
  # Label
  # newLabel
  # allQs = FALSE will change only the first presentation of a repeated question
  ####
  
  # initialize a counter
  chartCounter <- 0
  
  # iterate over the vector of NCCA ASCII chart names
  
  i=1
  for(i in 1:length(NCCAASCIIName)) {
    
    ## import the NCCA ASCII text lines
    
    {
      thisNCCAASCIIName <- NCCAASCIIName[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(NCCAASCIIName[i], -3, -1))
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      print(paste(i, examName))
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
      # timeSeriesData
    }
    
    ## locate the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # eventTable
    }
    
    ## locate the question list ##
    
    {
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # questionTable
    }
    
    ## next chart if the Label is not extant ##
    
    {
      if(length(which(questionLabelColumn %in% Label)) == 0) next()
    }
    
    ## adjust the question list ##
    
    {
      # locate the Label row in the question list
      ifelse(allQs,
             questionListRow <- which(questionLabelColumn == Label),
             questionListRow <- which(questionLabelColumn == Label)[1] )
      # insert the new label into the question label column
      questionLabelColumn[questionListRow] <- newLabel
      # pad the new label for the event table 
      newLabelPad <- str_pad(newLabel, width=8, side="left", pad=" ")
      for(j in 1:length(questionListRow)) {
        # construct a new text row with the new label
        # newQuestionTableRowText <- paste0(str_sub(questionTable[questionListRow], 1, 6),
        #                                   newLabelPad,
        #                                   str_sub(questionTable[questionListRow], 15, -1) )
        newQuestionTableRowText <- paste0(str_sub(questionTable[questionListRow[j]], 1, 6),
                                          newLabelPad,
                                          paste0(" ", newLabel) )
        questionTable[questionListRow[j]] <- newQuestionTableRowText
      }
    } 
    
    ## adjust the event table ##
    
    {
      # locate the row for the input event Label
      ifelse(allQs,
             eventTableRow <- which(eventLabelColumn == Label),
             eventTableRow <- which(eventLabelColumn == Label)[1] )
      oldBegin <- as.numeric(str_trim(BeginColumn[eventTableRow], side="left"))
      oldEnd <- as.numeric(str_trim(EndColumn[eventTableRow], side="left"))
      oldAnswer <- as.numeric(str_trim(AnswerColumn[eventTableRow], side="left"))
      for(j in 1:length(eventTableRow)) {
        # use the newLabelPad from earlier to construct a new text row for the annotation
        newEventTableRow <- 
          paste0(str_sub(eventTable[eventTableRow[j]], 1, 6),
                 newLabelPad,
                 str_pad(oldBegin[j], width=11, side="left", pad=" "),
                 # use the oldBegin to set the annotation on a single data row
                 str_pad(oldBegin[j], width=11, side="left", pad=" "), 
                 "   " )
        # # use the newLabelPad from earlier
        # eventTable[eventTableRow] <- 
        #   paste0(str_sub(eventTable[eventTableRow], 1, 6),
        #          newLabelPad,
        #          str_sub(eventTable[eventTableRow], 15, -1) )
        # eventTable[eventTableRow] <- str_sub(eventTable[eventTableRow], 1, 39)
        eventTable[eventTableRow[j]] <- newEventTableRow
      }
    }
    
    ## adjust the event Label in the time series data rows ##
    
    {
      # locate the time series for for this event
      # theseDataRows <- which(LabelColumn == thisLabelText)
      # do it this way to avoid problems when a question is repeated
      theseDataRows <- NULL
      
      if(allQs) {
        for(j in 1:length(oldBegin)){
          theseDataRows <- c(theseDataRows, c(oldBegin[j]:oldEnd[j]))
        }
      } else {
        theseDataRows <- c(oldBegin[1]:oldEnd[1]) 
      }
      
      # pad the input search label
      newLablPad2 <- str_pad(newLabel, width=8, pad="-", side="left")
      
      # first remove the old events
      if(length(theseDataRows) > 1) {
        # only if the old event was more than one data row
        theseDataRows2 <- theseDataRows[c(1:length(theseDataRows))]
        # str_c() and str_sub() are vectorized, and this does not need a loop
        timeSeriesData[theseDataRows2] <- 
          str_c(str_sub(timeSeriesData[theseDataRows2], 1, 16),
                "--------",
                str_sub(timeSeriesData[theseDataRows2], 25, -1) )
      }
      
      # oldBegin instead of theseDataRows
      
      # then set the new annotation and the first data row for each old event
      for(j in 1:length(oldBegin)) {
        # the newLabelPad2 will be recycled and inserted into the text
        timeSeriesData[oldBegin[j]] <- 
          str_c(str_sub(timeSeriesData[oldBegin[j]], 1, 16),
                newLablPad2,
                str_sub(timeSeriesData[oldBegin[j]], 25, -1) )
      } # end loop j for data rows
      
    }
    
    ## adjust the answer text in the time series data ##
    
    {
      answerTxtPad <- "--------"
      ifelse(allQs,
             tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow])),
             tsAnswerRow <- as.numeric(str_trim(AnswerColumn[eventTableRow]))[1] )
      tsAnswerRow <- tsAnswerRow[!is.na(tsAnswerRow)]
      # set the new answer rows
      if(length(tsAnswerRow) > 0) {
        for(j in 1:length(tsAnswerRow)) {
          timeSeriesData[tsAnswerRow[j]] <- 
            paste0(str_sub(timeSeriesData[tsAnswerRow[j]], 1, 16),
                   answerTxtPad,
                   str_sub(timeSeriesData[tsAnswerRow[j]], 25, -1) )
        }
      }
    }
    
    ## construct the output vector ##
    
    {
      outputVector <- textLines
      # replace the time series rows
      outputVector[tsStartRow:tsEndRow] <- timeSeriesData
      # replace the event table
      outputVector[eventTableStartRow:eventTableEndRow] <- eventTable
      # replace the question list
      outputVector[questionListStartRow:questionListEndRow] <- questionTable
      
    }
    
    ## write the data to the NCCA ASCII file
    
    {
      # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
      write_lines(outputVector, file=thisNCCAASCIIName)
    }
    
    print(paste(examName, seriesName, chartName, "question label", Label, "changed to", newLabel))
    
    chartCounter <- chartCounter + 1
    
  } # end i loop
  
  return (paste("Question labels changed in", chartCounter, "charts:", Label, "changed to", newLabel))
  
} # end annotationFn()



NCCAASCIIChartNames



NCCAASCIIName <- NCCAASCIIChartNames[1]



locateEventsFn(NCCAASCIIName)



# annotationFn(NCCAASCIIName = NCCAASCIIName,
#            Label = "C9",
#            newLabel = "EI",
#            allQs = FALSE)



########## FIX QUESTION LABELS AND ANSWERS ############



fixQuestionsAndAnswersFn <- function(NCCAASCIIChartNames) {
  # R function to fix quesion labels and answers
  # March 12, 2025
  # Raymond Nelson
  ####
  # NCCAASCIIName can be a vector of NCCAASCII text file names in the cwd
  # output is a messagge
  ##
  # ensure that verbal answers are not recorded late, during the next stimulus event
  # also ensure that question labels are consistent 
  # for all data rows from question Begin to question End,
  # with no data rows for which the event label is missing or equal to "--------"
  ####
  
  if(getOption("warn") !=2) {
    # set the warn level to suppress warnings
    if(!exists("oldw")) oldw <- getOption("warn")
    # -1 to suppress warnings
    # 0 is normal, warnings are display at end
    # 1 warnings are display at the time
    # 2 warnings are escalated to errors
    options(warn = 2)
    # reset to default
    # options(warn = 0)
    # rm(oldw)
    print(paste("warn level:", oldw))
    # getOption("warn")
  }
  
  # initialize a counter
  chartCounter <- 0
  
  # iterate over the vector of NCCA ASCII chart names
  
  i=1
  for(i in 1:length(NCCAASCIIChartNames)) {
    
    print(paste("chart:", i))
    
    rewrite <- FALSE
    
    {
      thisNCCAASCIIName <- NCCAASCIIChartNames[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      # textLines <- readLines(paste0("D&-", examName, "-", seriesName, ".", chartName))
      textLines <- readLines(thisNCCAASCIIName)
      
      print(paste(i, examName, seriesName, chartName))
    }
    
    ## locate and slice the time series data rows ##
    
    {
      # nchar("Sample     Time    Label")
      tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Sample     Time    Label") +  1
      tsEndRow <- length(textLines)
      # slice the time series data
      timeSeriesData <- textLines[tsStartRow:tsEndRow]
      # isolate the Label column
      LabelColumn <- str_sub(timeSeriesData, 17, 24)
      # length(LabelColumn)
      # timeSeriesData
    }
    
    ## locate and slice the event table ##
    
    {
      # nchar("Event    Label      Begin        End     Answer")
      eventTableStartRow <- which(str_sub(textLines[1:150], 1, 47) == "Event    Label      Begin        End     Answer") + 1
      eventTableEndRow <- tsStartRow - 3
      # slice the event table
      eventTable <- textLines[eventTableStartRow:eventTableEndRow]
      # isolate the Label column
      eventLabelColumn <- str_trim(str_sub(eventTable, 7, 14), side="left")
      # isolate the Begin column
      BeginColumn <- str_sub(eventTable, 16, 25)
      # isolate the End column
      EndColumn <- str_sub(eventTable, 29, 36)
      # isolate the Answer column
      AnswerColumn <- str_sub(eventTable, 40, 47)
      # eventTable
      
      # initialize numeric versions of the End and Answer columns
      BeginColumnNm <- as.numeric(BeginColumn)
      EndColumnNm <- as.numeric(EndColumn)
      AnswerColumnNm <- as.numeric(AnswerColumn)
      
      AnswerColumnChr <- str_trim(AnswerColumn, side="both")
      
      eventLabelColumnPad <- str_pad(eventLabelColumn, side="left", pad="-", width=8)
      
      if(length(EndColumnNm) != length(AnswerColumnNm)) stop("check answer column length")
      if(length(EndColumnNm) != length(eventLabelColumn)) stop("check event label column length")
    }
    
    ## locate and slice the question list ##
    
    {
      ## need to get the question list after the event table 
      
      # nchar("Event    Label Statement")
      # [1] 24
      questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == "Event    Label Statement") + 1
      questionListEndRow <- eventTableStartRow - 3
      # slice the evet table
      questionTable <- textLines[questionListStartRow:questionListEndRow]
      # isolate the Label column
      questionLabelColumn <- str_trim(str_sub(questionTable, 7, 14), side="left")
      # this will include blank rows when the question text is wrapped
      # questionTable
      
      if(!all.equal(questionLabelColumn, eventLabelColumn)) stop("check Labels")
    }
    
    ## fix the answer column in the event table and answer row in the time series data ##
    
    {
      
      ## event table ##
      
      newAnswerColumn <- AnswerColumn
      
      # initialize a new event table
      newEventTable <- eventTable
      
      j=1
      for(j in 1:(length(AnswerColumnNm)-1)) {
        # make sure that each anser is recorded before the onset of the next event
        
        if(is.na(AnswerColumnNm[j])) next()
        
        if(AnswerColumnNm[j] >= BeginColumnNm[(j+1)]) {
          
          # remove the answer from the time series data
          LabelColumn[AnswerColumnNm[j]] <- "--------"
          
          # remove the answer index from the event table
          newAnswerColumn[j] <- "  "
          
          j=1
          for(j in 1:length(eventTable)) {
            newEventTable[j] <- paste0(str_sub(eventTable[j], 1, 39), newAnswerColumn[j])
          }
          
          rewrite <- TRUE
          
        }
        
      }
      
      ## time series data ##
      
      # BeginColumnNm
      # EndColumnNm
      # eventLabelColumn
      
      if(length(BeginColumnNm) != length(eventLabelColumn)) stop("bad event column?")
        
        # initialize a new Label Column
        newLabelColumn <- LabelColumn
        
        j=1
        for(j in 1:length(BeginColumnNm)) {
          
          thisEvent <- LabelColumn[c(BeginColumnNm[j]:EndColumnNm[j])]
          
          if( any(thisEvent != eventLabelColumnPad[j]) ) {
            
            newLabelColumn[c(BeginColumnNm[j]:EndColumnNm[j])] <- eventLabelColumnPad[j]
            
            rewrite <- TRUE
            
          }
        
        }
      
    }
    
    ## rebuild the time series data rows ##
    
    {
      
      newTimeSeriesData <- timeSeriesData
      
      if(rewrite) {
        
        newTimeSeriesData <- str_sub(timeSeriesData, 1, 16)
        # use the new Label column
        newTimeSeriesData <- str_c(newTimeSeriesData, newLabelColumn)
        # add the columns to the right of the Label Column
        newTimeSeriesData <- str_c(newTimeSeriesData, str_sub(timeSeriesData, 25, -1))
        
      }
      
    }
    
    ## next chart if no changes have been made ##
    
    if(!rewrite) next()
    
    ## construct the output vector ##
    
    {
      
      # initialize the output vector
      outputVector <- textLines
      
      # replace the time series rows
      outputVector[tsStartRow:tsEndRow] <- newTimeSeriesData
      # replace the event table
      outputVector[eventTableStartRow:eventTableEndRow] <- newEventTable
      # replace the question list
      outputVector[questionListStartRow:questionListEndRow] <- questionTable
      
    }
    
    ## write the data to the NCCA ASCII file
    
    print(paste0("corrected answer locations in ", thisNCCAASCIIName))
    
    {
      # write_lines(outputVector, file=paste0("D&-", examName, "-", seriesName, ".", chartName))
      write_lines(outputVector, file=thisNCCAASCIIName)
    }
    
    chartCounter <- chartCounter + 1
    
  } # end i loop
  
  
  
  
  return (paste("Question labels fixed in", chartCounter, "charts."))
  
  
  
} # end fixQuestionsAndAnswersFn()



NCCAASCIIChartNames



NCCAASCIIName <- NCCAASCIIChartNames[1]



# fixQuestionsAndAnswersFn(NCCAASCIIChartNames=NCCAASCIIChartNames)



# March 15, 2025 fixed labels and answers for the Marin sample



