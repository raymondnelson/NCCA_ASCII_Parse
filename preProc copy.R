# function to complete preprocessing tasks on the NCCA ASCII ouput
# after parsing the header, stimulus, and time series data
# 4/23/2016
# Raymond Nelson

#########################################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[3]


# x=uniqueExams
# y=100
# showNames=TRUE
# output=FALSE


# source this first
# source this for the fixTagsFn()
source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCIIParseHelperFunctions.R', echo=FALSE)

# source this last
# source the sigProcHelrper.R script to load the getFirstLastEventFn()
# also the setColRange() function to replace the one from NCCAASCIIParseHelperFunction.R
source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)


preProc <- function(x=uniqueExams, makeDF=TRUE, output=FALSE) {
  # function to pre-process NCCA ASCII output data
  # center the onset at zero
  # set the range for each sensor as 1 to 1000
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
      
      examName <- uniqueExams[i]
      # assign("examName", examName, pos=1)
      print(examName)
      
      #### 20191231 maybe could  parse the headers and data here ####
      
      # get the names of time series lists for all unique series in each exam
      # searchString <- paste0("*", examName, "_Data", "*")
      
      #### get the exam data frame with the time series data #### 
      
      # get the time series data frame for the exam
      # examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      examDF <- get(paste0(examName, "_Data"), pos=1)
      
      # set the first and last rows
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      # fix question tags 
      examDF$Label <- fixTagsFn(examDF$Label)
      # assign("examDF", examDF, pos=1)
      
      print(examName)
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(examDF$seriesName))
      print(paste("unique series: ", paste(uniqueSeries, collapse = " ")))
      
      ####   get the stimulus events data frame for the exam   ####
      
      # if(exists("eventDF")) rm("eventDF")
      eventDFName <- paste0(examName, "_Stimuli")
      if(exists(eventDFName)) {
        eventDF <- get(eventDFName, pos=1)
      } #  commented out 8-4-2017 else next()
      
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
      # assign(paste0(examName, "_Stimuli"), eventDF, pos=1)
      # assign("eventDF", eventDF, pos=1)
      # View(eventDF)
      
      # initialize a newEventDF for this exam
      newEventDF <- NULL
      
    }
    
    #### iterate over each unique series ####
    
    j=2
    for(j in 1:length(uniqueSeries)) {
    
      {
        
        # get the time-series data frame for this series
        seriesName <- uniqueSeries[j]
        seriesRows <- which(examDF$seriesName == seriesName)
        seriesOnsetRow <- min(seriesRows)
        seriesEndRow <- max(seriesRows)
        
        #### slice the series data frame ####
        
        seriesDF <- examDF[seriesRows,]
        # View(seriesDF)
        
        # seriesOnsetRow <- which(examDF$seriesName == seriesName)[1]
        # seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        # assign("seriesDF", seriesDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        print(paste("series:", toString(seriesName)))
        
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
          
          # if(seriesName == "7" && chartName == "02A") {
          #   assign("chartDF", chartDF, envir=.GlobalEnv)
          #   stop()
          # }
          
          # chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          # chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          # chartEndRow <- max(which(seriesDF$chartName==chartName))
          
          # re-initialize the eventLabel column in the chartDF data frame
          chartDF$eventLabel <- ""
          
        }
        
        # center the data at zero
        
        {
          
          # data needs to be centered at onset zero for the DSP filters
        
          print("  center the data at zero")
          
          lastDataCol <- min(grep("^c_", names(chartDF))) - 1
          
          # fix some NA values that may occur when not using some sensors
          l=11
          for(l in 11:lastDataCol) {
            chartDF[grep("NaN", chartDF[,l]),l] <- 0
            # chartDF[which(is.na(as.numeric(chartDF[,11]))),l] <- 0
          }
          
          # make sure all data columns are numeric
          # l=11
          # for(l in 11:lastDataCol) {
          #   chartDF[,l] <- as.numeric(chartDF[,l])
          # }
          
          # iterate over the chartDF columns and call the centerColumn function
          l=11
          # start at column 11 because that is the first data column in chartDF
          for (l in 11:lastDataCol) {
            # use a function to center the data
            chartDF[,l + lastDataCol - 10] <- centerColumn(chartDF[,l])
          } # end loop over l columns to center data
          
          # View(chartDF)
          
        }
        
        # get the first and last events
        
        {
        
          # first source the sigProcHelper.R script
          # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
          
          firstLastEvents <- getFirstLastEventFn(x=chartDF)
          firstEvent <- firstLastEvents[1]
          lastEventEnd <- firstLastEvents[2]
          # assign("firstLastEvents", firstLastEvents, pos=1)
          
        }
        
        # set the range
        
        {
          
          print("  set the range for the time series data")
          
          # first call the sigProcHelper.R script
          
          # start at the first data column 11
          useCols <- ((11 + (lastDataCol - 10)):((lastDataCol + 1) + (lastDataCol - 11)))
          m=16
          for (m in min(useCols):max(useCols)) {
            # use a function for each data column
            # colRange is set in the init
            chartDF[,m] <- setColRange(x=chartDF[,m], y=colRange, firstRow=firstEvent, lastRow=lastEventEnd)
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
          
          ## slice the events data frame for this chart ##
          
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
          
        }
        
        #####   work with the events   ####
        
        if(nrow(chartEventsDF) != 0) {
          
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
          
          # chartEventDFLabels <- chartEventsDF$Label
          
          ## check that all events in chartEventsDF are in chartDF ##
          
          # check the chart data frame with the events data frame
          missingEvents <- which(!(allEventsChart %in% chartLabels))
          # chartEventsDF$Label[missingEvents]
          
          # 3-23-2017 changed to handle repeated annotations and anotation during stim events
          # missingEvents <- which(!(chartEventsDF$eventLabel %in% chartDF$Label))
          if(length(missingEvents) > 0) {
            o=2
            for (o in 1:length(missingEvents)) {
              # determine the chartDF row to add the missing event
              missingEventRow <- min(which(chartDF$Sample >= chartEventsDF$Begin[missingEvents[o]]))
              # add the missing event to the chartDF
              chartDF$Label[missingEventRow] <- chartEventsDF$Label[missingEvents[o]]
              # 3-23-2017 to handle repeated annotations and anotation during stim events
              # chartDF$eventLabel[missingEventRow] <- chartEventsDF$eventLabel[missingEvents[o]]
            } # end for loop over missing events in chartDF$Label
          } # end if for missing events
          
          # 3-23-2017 add the eventLabel data to the chartDF
          # 4-5-2017 fixed problem with imported charts - indices exceed nrow
          # chartDF$eventLabel[chartEventsDF$Begin] <- chartEventsDF$eventLabel
          
          ## check the last event ##
          
          lastEventNumber <- nrow(chartEventsDF)
          
          lastBegin <- chartEventsDF$Begin[lastEventNumber]
          lastEnd <- chartEventsDF$End[lastEventNumber]
          lastAnswer <- chartEventsDF$Answer[lastEventNumber]
          
          # vector of indices for the last event
          lastEvent <- as.numeric(c(lastBegin, lastEnd, lastAnswer))
          
          # fix na values
          lastEvent[which(is.na(lastEvent))] <- nrow(chartDF)
          
          # check if any are at the end of the chartDF
          if(any(lastEvent > (nrow(chartDF)-3))) {
            # reset them
            lastEvent0 <- lastEvent
            lastEvent0[which(lastEvent > nrow(chartDF)-3)] <- 
              nrow(chartDF) - 3
            # adjust the chartEventsDF
            chartEventsDF$Begin[lastEventNumber] <- lastEvent0[1]
            chartEventsDF$End[lastEventNumber] <- lastEvent0[2]
            chartEventsDF$Answer[lastEventNumber] <- lastEvent0[3]
            # adjust the chartDF answer
            chartDF$Label[lastEvent0[3]] <- chartDF$Label[lastEvent[3]]
            if(lastEvent[3] != lastEvent0[3]) {
              chartDF$Label[lastEvent[3]] <- ""
            }
            # then reset the event label in the chartDF
            # cannot do this with imported charts
            # because the chartEventsDF indices are incorrect
            # chartDF$Label[lastEvent[1]:lastEvent[2]] <- ""
            # chartDF$Label[lastEvent0[1]:lastEvent0[2]] <- 
            #   chartEventsDF$Label[lastEventNumber]
          }
            
        } # end work with events
        
        #### check for answer prior to stimulus offset  ####
        
        # View(chartEventsDF)
        # View(chartDF)
        
        if(length(which(chartEventsDF$Answer <= chartEventsDF$End)) > 0) {
          
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
        
        #### work with the stimulus onset ####
         
        if(nrow(chartEventsDF) != 0) {
          
          {
            
            # get the stim question labels and onset rows
            
            # make a vector of event labels from the chart data frame
            # 3-23-2017
            # uniqueEvents <- chartDF$eventLabel
            uniqueEvents <- chartDF$Label
            
            # keep only the first index for each unique event onset
            for(l in 2:length(uniqueEvents)) {
              # use the chartDF$Label because it remains un-altered by this loop
              if(chartDF$Label[l] == chartDF$Label[l-1]) { uniqueEvents[l] <- "" }
              # 3-23-2017 
              # if(chartDF$eventLabel[l] == chartDF$eventLabel[l-1]) uniqueEvents[l] <- ""
            } 
            
            # now make a vector of questionlabels
            stimQuestionOnset <- which(uniqueEvents != "")
            # chartDF$Label[stimQuestions]
            
            answerWords <- c("YES", "NO", "ANS")
            
            # exclude YES, NO and ANS answers
            stimQuestionOnset <- 
              stimQuestionOnset[which(!(chartDF$Label[uniqueEvents!=""] %in% answerWords))]
                                      
                                      # != "YES" & 
                                      #   chartDF$Label[uniqueEvents!=""] != "NO")]
            
            # length(stimQuestionOnset)
            # chartDF$Label[stimQuestionOnset]
            
            # get the question labels
            # 3-23-2017
            # stimQuestions <- chartDF$eventLabel[stimQuestionOnset]
            stimQuestions <- chartDF$Label[stimQuestionOnset]
            
            #### we now have vectors for
            # uniqueEvents # the Label column in the chartDF
            # stimQuestionOnset # row index for qeustion onset
            # stimQuestions # this is the question label
            
          }
          
          {
            
            # remove the second of repeated events 
            # that occur when an annotation is entered during an event
            # terminate the event at the annotation
            
            removeEvents <- NULL
            # make a vector of events to remove
            if(length(stimQuestions) >= 3) {
              # initialize a vector of items to remove
              # removeEvents <- NULL
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
          
          if( length(stimQuestionOnset) != length(chartEventsDF$Label) ) {
          
            # check to ensure that stimulus events in chartDF == chartEventsDF
          
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
            
            # now newChartEventsDF has rows == length(chartDFEvents)
            
            # replace the chartEventsDF with the newChartEventsDF
            chartEventsDF <- newChartEventsDF
            # View(chartEventsDF)
            
          } # end if
          
          # add the correct onset to the chartEventsDF instead
          chartEventsDF$Begin <- stimQuestionOnset
          # View(chartEventsDF)
          
          # pass the chartEventsDF back now that it has changed
          eventDF[useRows,] <- chartEventsDF
          # assign(eventDFName, eventDF, envir=.GlobalEnv)
          
          # all events in the chartEventsD are now included in the chartDF
          
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
          
          {
            
            # remove the second of repeated events 
            # that occur when an annotation is entered during an event
            # terminate the event at the annotation
            
            removeEvents <- NULL
            if(length(stimQuestions) >= 3) {
              # initialize a vector of items to remove
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
          
          # 3-23-2017 for repeated annotations
          # remove duplicated events that occur 
          # when an annotation is entered during a question
          # fixLabels <- which(chartDF$eventLabel[stimQuestionEnd] != "")
          # 3-24-2017 need to correctly handle repeated events
          # fixLabels <- which(chartDF$Label[stimQuestionEnd] != "")
          # fixLabels <- stimQuestions
          # stimQuestions[fixLabels] <- chartDF$eventLabel[stimQuestionEnd][fixLabels]
          # stimQuestions[fixLabels] <- chartDF$Label[stimQuestionEnd][fixLabels]
          
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
          
          # add the correct end to the eventDF data frame
          # not yet added to the chartEventsDF
          # eventDF$End[(eventDF$chartName==chartName & eventDF$seriesName==seriesName)] <- stimQuestionEnd
          
          # add the correct end to the chartEventsDF instead
          chartEventsDF$End <- stimQuestionEnd
          # View(chartEventsDF)
          
          # pass the chartEventsDF back now that it has changed
          eventDF[useRows,] <- chartEventsDF
          # assign(eventDFName, eventDF, envir=.GlobalEnv)
          
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
          
        } # end work with the verbal answer
        
        ####   build a new corrected eventDF   ####
        
        if(nrow(chartEventsDF) != 0) {
          
          # check for events that are overlapping with a previous event
          
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

