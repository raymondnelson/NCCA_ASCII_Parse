# R function to remove punctuation characters from question labels in the NCCA ASCII data
# February 19, 2025
# Raymond Nelson 
####


library(stringr)
library(readr)



fixPunctuatedLabelsFn <- function(NCCAASCIIName) {
  # R function to remove punctuation characters from question labels in the NCCA ASCII data
  # February 19, 2025
  # Raymond Nelson 
  ####
  # NCCAASCIIName can be a vector of NCCA ASCII file names
  #
  # type is used to denote the instrument type for the NCCA ASCII file name
  #   "D$-" is for Axciton
  #   "D#-" is Stoelting
  #   "D&-" is Lafayette
  #   D%-" is Limestone 
  ####
  # output is a message
  # side effect is that NCCA ASCII data files are modified
  ####
  
  # initialize a counter
  chartCounter <- 0
  
  # iterate over the vector of NCCA ASCII chart names
  
  i=1
  for(i in 1:length(NCCAASCIIName)) {
    
    {
      thisNCCAASCIIName <- NCCAASCIIName[i]
      examName <- str_sub(thisNCCAASCIIName, 4, -7)
      seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
      chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
      
      textLines <- readLines(thisNCCAASCIIName)
      
      print(thisNCCAASCIIName)
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
    
    if(all(!grepl(pattern="[[:punct:]]", x=questionLabelColumn))) next()
    
    ## inititalize the output tables ##
    
    {
      newQuestionTable <- questionTable
      newEventTable <- eventTable
    }
    
    ## adjust the quesitons Label ##
    
    {
      
      ## adjust the question list if there are punctution characters ##
      
      {
        # remove the punctuation
        newQuestionLabelColumn <- 
          gsub(pattern="[[:punct:]]", "", questionLabelColumn)
        # pad the label strings
        newQuestionLabelPad <- str_pad(newQuestionLabelColumn, width=8, side="left", pad=" ")
        for(j in 1:length(newQuestionTable)) {
          newQuestionTableRowText <- paste0(str_sub(questionTable[j], 1, 6),
                                            newQuestionLabelPad[j],
                                            paste0(" ", newQuestionLabelColumn[j]) )
          newQuestionTable[j] <- newQuestionTableRowText
        }
      }
      
      ## adjust the event table ##
      
      {
        # use the newQuestionLabelPad vector from the previous step
        for(j in 1:length(newEventTable)) {
          newEventTabelRowText <- paste0(str_sub(eventTable[j], 1, 6),
                                         newQuestionLabelPad[j],
                                         paste0(" ", BeginColumn[j]),
                                         paste0("   ", EndColumn[j]),
                                         paste0("   ", AnswerColumn[j], " " ))
          newEventTable[j] <- newEventTabelRowText
        }
      }
      
      ## adjust the event Label in the time series data rows ##
      
      {
        # new time series data
        newTimeSeriesData <- timeSeriesData
        # new text for the label column in the time series data
        newQuestionLabelPad2 <- 
          str_pad(newQuestionLabelColumn, width=8, pad="-", side="left")
        # use the old label to locate the rows for each event
        oldQuestionLablePad <- 
          str_pad(questionLabelColumn, width=8, pad="-", side="left")
        # initialize a new column for the labels in the time series data
        newLabelColumn <- LabelColumn
        j=1
        for(j in 1:length(newQuestionLabelPad2)) {
          theseDataRows <- NULL
          thisEvent <- newQuestionLabelPad2[j]
          thisOldEvent <- oldQuestionLablePad[j]
          theseDataRows <- which(LabelColumn == thisOldEvent)
          # iterate to replace the label in the time series data rows
          for(k in 1:length(theseDataRows)) {
            newTimeSeriesData[theseDataRows[k]] <-
              str_c(str_sub(newTimeSeriesData[theseDataRows[k]], 1, 16),
                    thisEvent,
                    str_sub(newTimeSeriesData[theseDataRows[k]], 25, -1) )
          }
          
          

          
          
          # theseDataRows <- which(LabelColumn == thisLabelText)
          # # do it this way to avoid problems when a question is repeated
          # ifelse(allQs,
          #        {
          #          for(i in 1:length(oldBegin)) {
          #            theseDataRows <- c(theseDataRows, c(oldBegin[i]:oldEnd[i]))
          #          }
          #          theseDataRows 
          #        },
          #        theseDataRows <- c(oldBegin[1]:oldEnd[1]) )
          # # pad the input search label
          # newLablPad2 <- str_pad(newLabel, width=8, pad="-", side="left")
          # for(i in 1:length(theseDataRows)) {
          #   timeSeriesData[theseDataRows[i]] <- 
          #     str_c(str_sub(timeSeriesData[theseDataRows[i]], 1, 16),
          #           newLablPad2,
          #           str_sub(timeSeriesData[theseDataRows[i]], 25, -1) )
          # }
          
        }
        
        # head(timeSeriesData[theseDataRows])
        # 
        # head(str_c(str_sub(timeSeriesData[theseDataRows], 1, 16),
        #            newLablPad2,
        #            str_sub(timeSeriesData[theseDataRows], 25, -1) ))
        
      }
      
    }
    
    ## construct the output vector ##
    
    {
      outputVector <- textLines
      # replace the time series rows
      outputVector[tsStartRow:tsEndRow] <- newTimeSeriesData
      # replace the event table
      outputVector[eventTableStartRow:eventTableEndRow] <- newEventTable
      # replace the question list
      outputVector[questionListStartRow:questionListEndRow] <- newQuestionTable
      
    }
    
    ## write the data to the NCCA ASCII file
    
    write_lines(outputVector, file=thisNCCAASCIIName)
    
    print(paste("Puntuation characters removed from question labels:", thisNCCAASCIIName))
    
    chartCounter <- chartCounter + 1
    
  } # end i loop
  
  return (paste("Question labels fixed in", chartCounter, "charts"))
  
} # end fixPunctuatedLabelsFn()



NCCAASCIINames <- list.files(pattern="D\\$-")

# fixPunctuatedLabelsFn(NCCAASCIIName=NCCAASCIINames)




# copyThese <- list.files()



# file.copy(from=paste0("../", copyThese), to=paste0("./", copyThese), overwrite=TRUE)





