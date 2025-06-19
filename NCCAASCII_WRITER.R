# R function to export a single test chart data to NCCA ASCII format
# 14 Dec 2018
# Raymond Nelson
#
# called in a loop in the NSCAASCII_Output.R script
#
##########



# library(stringr)


{
  
  # source(paste0(RPath, 'NCCAASCII_PseudoNamer.R'))
  
  # # source the NCCA ASCII parse helper function to load the fromMinSec function
  # source(paste0(RPath, 'toMinSec.R'), echo=FALSE)
  
  # # initialize the setColRange() function and the getFirstLastEventFn()
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
}


# writeNCCAASCII <- TRUE


# examName <- "TES1"
# seriesName <- "1"
# chartName <- "01A"



####### main function #######



writeNCCAASCIIFn <- function(thisChartDF=thisChartDF, 
                             thisChartEventsDF=thisChartEventsDF,
                             newChartName=newChartName,
                             thisOutputName=thisOutputName,
                             outputSensors2=NULL,
                             outputSeriesName2=NULL,
                             inclPPG=TRUE,
                             inclAutoEDA=TRUE,
                             inclFC=TRUE, 
                             inclMove1=TRUE,
                             fixMissingAnswers=FALSE,
                             fixAnswerDistance=FALSE,
                             fixLabels=FALSE,
                             fixRQRotation=FALSE,
                             fixCQRotation=FALSE,
                             fixRotation=FALSE,
                             fixQuestionLength=FALSE,
                             fixDuplicates=FALSE,
                             RQLengths2=RQLengths2,
                             CQLengths2=CQLengths2,
                             SALength2=SALength2,
                             SYLengths2=SYLengths2,
                             NLengths2=NLengths2,
                             XXXLengths2=XXXLengths2,
                             ditherEvents=FALSE,
                             outputFolderName="NCCAASCIIOutputLAF" ) {
  # R function to write the NCCA ASCII file for each chart
  # 2020-02-08
  # Raymond Nelson
  #
  # called by the NCCAASCIIOutputFn() in the NCCAASCCIIOutput.R script
  #
  # thisChartDF is the data frame for a single chart
  # thisChartEventsDF is the events data frame for a single chart
  #
  # thisOutputName is the output file name in NCCA ASCII format
  #
  # ouput sensors2 is a vector of NCCA ASCII sensor names
  # outputSeriesName2 is the output series name to use in the file names
  # inclPPG will include or exclude the PPG sensor
  #
  # fixRQRotation can be used to set the RQ rotation for each chart
  # fixCQRotation can be used to set the CQ rotation for each chart
  # fixLabels can be use to set the CQ and RQ labels when data are problematic
  #
  # visible output is the name of the exam 
  # for which the NCCA ASCII files are written
  #
  # NCCA ASCII output is to the CWD
  #
  ####
  
  {
    assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
    assign("thisChartEventsDF", thisChartEventsDF, envir=.GlobalEnv)
  }
  
  #### set up the output series name, chart name, and sensor names ####
    
  {
    
    # stop("writing NCCA ASCII")
    
    if(!exists("outputSeriesName2")) outputSeriesName2 <- NULL
    outputSeriesName2 <- as.character(outputSeriesName2)
    if(length(outputSeriesName2) == 0) outputSeriesName2 <-
      as.character(thisChartDF$seriesName[1])
    
    if(!exists("outputSensors2")) outputSensors2 <-c("UPneumo", "LPneumo", "EDA1", "Cardio1", "Move1")
    if(is.null(outputSensors2)) outputSensors2 <- c("UPneumo", "LPneumo", "EDA1", "Cardio1", "Move1")
    
    # check if activity sensor data are available
    if("Move1" %in% names(thisChartDF) || inclMove1) {
      inclMove1 <- TRUE
      outputSensors2 <- unique(c(outputSensors2, "Move1"))
    } else {
      inclMove1 <- FALSE
      outputSensors2 <- outputSensors2[outputSensors2 != "Move1"]
    }
    
    if(!exists("inclAutoEDA")) inclAutoEDA <- FALSE
    
    # if(!exists("inclFC")) inclFC <- FALSE
    
    # if(!exists("inclPPG")) inclPPG <- FALSE
    
    # # # check if PLE data are available
    # if(("PPG1" %in% names(thisChartDF)) || inclPPG) {
    #   inclPPG2 <- TRUE
    # } else {
    #   inclPPG2 <- FALSE
    # }
    
    # if(inclPPG) inclPPG2 <- TRUE
    
    # # include PPG based on an environment parameter
    # if(isTRUE(inclPPG)) outputSensors2 <- c(outputSensors2, "PPG1")
    
    # exclude the PPG column if PPG data is not present
    if("PPG1" %in% names(thisChartDF) || inclPPG) {
      inclPPG2 <- TRUE
      outputSensors2 <- unique(c(outputSensors2, "PPG1"))
    } else {
      inclPPG2 <- FALSE
      outputSensors2 <- outputSensors2[outputSensors2 != "PPG1"]
    }
    
    if(isTRUE(inclAutoEDA)) outputSensors2 <- c(outputSensors2, "EDA2")
    
    # if(isTRUE(inclFC)) outputSensors2 <- c(outputSensors2, "FC")
    
    {
      # these parameters are set in the NCCAASCIIOutput.R script and are 
      if(!exists("fixRQRotation")) fixRQRotation <- FALSE
      if(!exists("fixCQRotation")) fixCQRotation <- FALSE
      if(!exists("fixLabels")) fixLabels <- FALSE
      if(!exists("fixAnswerDistance")) fixAnswerDistance <- FALSE
      if(!exists("fixQuestionLength")) fixQuestionLength <- FALSE
      
      if(!exists("outputFolderName")) outputFolderName <- "NCCAASCIIOutputLAF"
      
      if(!exists("newChartName")) newChartName <- thisChartDF$chartName[1] # "03A"
    }
    
    print(paste("exam:", thisChartDF$examName[1]))
    
  }
  
  #### Construct the output file name ####
  
  {
    
    # thisOutputName is an input parameter
    
    # outputNames[i]
    
    # if(!exists("thisOutputName")) {
    #   thisOutputName <- paste0(str_sub(thisChartDF$examName[1], 2, -1),
    #                            "-",
    #                            # "2", # series number
    #                            outputSeriesName2,
    #                            ".",
    #                            # chart name in NCCA form ex: "01A"
    #                            newChartName )
    # }
    
    # thisOutputName <- paste0(outputNames[i], 
    #                          "-",
    #                          # "2", # series number
    #                          outputSeriesName,
    #                          ".", 
    #                          # chart name in NCCA form ex: "01A"
    #                          newChartName )
    
    # show the NCCAASCII text file name on the console
    print(paste("new NCCA ASCII file:", thisOutputName))
    
  }
  
  #### set the chart time - the time at which the chart is recorded ####
  
  {
    
    thisSeriesNumber <- (str_sub(thisOutputName, -5, -5))
    thisChartNumber <- as.numeric(str_sub(thisOutputName, -2, -2))
    # thisChartTime <- c("01:01", "02:01", "03:01", "04:01", "05:01", "06:01", "07:01", "08:01", "09:01")[thisChartNumber]
    # thisChartTime <- c("12:01", "12:16", "12:31", "12:46", "12:01", "12:16", "02:31", "02:46", "03:01")[thisChartNumber]
    
    seriesNumbers <- c("01", "02", "03", "04", "05")
    chartTimes <- c(":10", ":20", ":30", ":40", ":50")
    
    ifelse(thisSeriesNumber == "X",
           thisHour <- 1,
           thisHour <- as.numeric(thisSeriesNumber) )
    
    thisChartTime <- paste0(seriesNumbers[thisHour], chartTimes[thisChartNumber])  
    
    print(paste("chart time:", thisChartTime))
    
  }
  
  #### scale and offset the data for output to the new NCCA ASCII format ####
  
  {
    
    {
      ### get the first and last event locations ###
      
      # source the sigProcHelper.R script for the getFirstLastEventFn
      # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
      
      firstLastEvents <- getFirstLastEventFn(thisChartDF)
      firstEvent <- firstLastEvents[1]
      lastEventEnd <- firstLastEvents[2]
    }
    
    # use a function to scale the data
    for(l in 11:ncol(thisChartDF)) {
      thisChartDF[,l] <- 
        setColRange(DAT=thisChartDF[,l], 
                    y=10000, 
                    firstRow=firstEvent, 
                    lastRow=lastEventEnd)
    }
    
    # offset the data and round to .0 decimals
    for(l in 11:ncol(thisChartDF)) {
      # offsetVal <- 25000 - median((thisChartDF[,l]))
      # offsetVal <- 10000 - min(thisChartDF[,l] )
      # offsetVal <- 25000 - min(thisChartDF[,l] )
      offsetVal <- 10000 - min(thisChartDF[,l][firstEvent:lastEventEnd] )
      thisChartDF[,l] <- round(thisChartDF[,l] + offsetVal, 0)
    }
    
    # adjust values less than 1 or greater than 999999
    for(l in 11:ncol(thisChartDF)) {
      thisChartDF[,l][which(thisChartDF[,l] > 999999)] <- 999999
      thisChartDF[,l][which(thisChartDF[,l] < 1)] <- 1
    }
    
    # fix the sample numbers
    thisChartDF$Sample <- 1:nrow(thisChartDF)
    
    {
      # for inspection only, not used for output
      
      assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
      # stop()
      # View(thisChartDF)
    }
    
  }
  
  ########## check the X and XX announcements #############
  
  {
    # the X and XX announcement must be the first and last events
    # and must not exceed the chart data frame rows
    
    theseXXXRows <- which(thisChartEventsDF$Label %in% c("X", "XX"))
    
    if(length(theseXXXRows) > 2) {
      stop("problem with X or XX announcement")
    }
    
    # is XX the last event?
    if(theseXXXRows[2] != nrow(thisChartEventsDF)) {
      assign("thisChartEventsDF", thisChartEventsDF, envir=.GlobalEnv)
      assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
      stop("problem with XX announcement")
      thisChartDF$Label[(thisChartEventsDF$End[theseXXXRows[2]]+1):nrow(thisChartDF)] <- ""
      thisChartEventsDF <- thisChartEventsDF[c(1:theseXXXRows[2]),]
    }
    
    # is X the first event?
    if(theseXXXRows[1] != 1) {
      assign("thisChartEventsDF", thisChartEventsDF, envir=.GlobalEnv)
      assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
      stop("problem with X announcement")
      thisChartDF$Label[1:(thisChartEventsDF$Begin[theseXXXRows[1]]-1)] <- ""
      thisChartEventsDF <- thisChartEventsDF[c(theseXXXRows[1]:nrow(thisChartEventsDF)),]
    }
    
    # do the X or XX exceed the chart data frame
    # if(theseXXXRows[2] >= nrow(thisChartDF)) {
    #   stop("X or XX exceeds the chart data frame")
    #   thisChartEventsDF$End[theseXXXRows[2]] <- nrow(thisChartDF) - 3
    #   thisChartDF$Label[(thisChartEventsDF$End[theseXXXRows[2]]+1):nrow(thisChartDF)] <- ""
    # }

    
    # does the XX exceed the time series data frame
    if(as.numeric(thisChartEventsDF$End[theseXXXRows[2]]) >= (nrow(thisChartDF)-16)) {
      thisChartDF$Label[(nrow(thisChartDF)-15):nrow(thisChartDF)] <- ""
      thisChartEventsDF$End[theseXXXRows[2]] <- nrow(thisChartDF) - 16
    }
    
    
  }
  
  ######################################################
  
  #### check and fix some problems with the ANSWERS ####
  
  {
    
    {
      ## annotation answers ##
      
      # need to first check that annotations occur on a single row 
      # then make sure that annotations have no answer
      # remove verbal answers for annotation in the events data frame
      
      # annotationRows <- which(thisChartEventsDF$Begin == thisChartEventsDF$End)
      # Feb 27, 2024 to format the NCCA output correctly 
      annotationRows <- which(thisChartEventsDF$Begin == thisChartEventsDF$End)
      # annotationRows <- sort(unique(which(thisChartEventsDF$Begin == (thisChartEventsDF$End - 1))))
      
      # delete the answer row for annotations
      # modified this loop on Feb 27, 2024
      if(length(annotationRows > 0)) {
        # delete the answers from the time series data
        o=1
        for(o in 1:length(annotationRows)) {
          # delete the answer from  the time series data first
          # if the answer location is the event End and the event Begin
          # fixed Mar 4, 2024
          if(thisChartEventsDF$Answer[annotationRows[o]] == thisChartEventsDF$End[annotationRows[o]]) {
            thisChartEventsDF$Answer[annotationRows[o]] <- ""
            next()
          }
          # proceed when the Answer was not equal to the End 
          # get time series data frame row fo the answer
          thisAnswerRow <- as.numeric(thisChartEventsDF$Answer[annotationRows[o]])
          if(is.null(thisAnswerRow)) next()
          # remove the verbal answer from the time series data frame Label column
          thisChartDF$Label[thisAnswerRow] <- ""
          thisChartDF$eventLabel[thisAnswerRow] <- ""
          {
            # then ensure the annotation exists on a single row
            # annotations are already extant on a single time series row
            thisDataRow <- as.numeric(thisChartEventsDF$Begin[annotationRows[o]]) + 1
            thisChartDF$Label[thisDataRow] <- ""
            thisChartDF$eventLabel[thisDataRow] <- ""
            # Feb 27, 2024
            # in case there is an answer in the time series data
            thisDataRow <- as.numeric(thisChartEventsDF$Begin[annotationRows[o]]) + 2
            thisChartDF$Label[thisDataRow] <- ""
            thisChartDF$eventLabel[thisDataRow] <- ""
          }
        } # end for loop o over annotationRows
        # redundant delete of the annotation answers in the events data frame
        thisChartEventsDF$Answer[annotationRows] <- ""
        # make sure the annotation exists on a single data frame sample row
        thisChartEventsDF$End[annotationRows] <- thisChartEventsDF$Begin[annotationRows]
      }
    }
    
    {
      ## X and XX answers ##
      
      # check for and remove answers to the X and XX announcements
      # some X XX announcements may be on a single row 
      # but they will already have no verbal answer
      theseXXXRows <- which(thisChartEventsDF$Label %in% c("X", "XX"))
      # keep only those rows for which the answer is not already blank
      fixTheseXXXAnswerRows <- theseXXXRows[theseXXXRows %in% which(thisChartEventsDF$Answer != "")]
      # get the end index rows for X and XX
      checkTheseXXXRows <- thisChartEventsDF$End[fixTheseXXXAnswerRows]
      {
        # fix the X and XX in the time series data and events data frames
        theseDataAnswerRows <- as.numeric(thisChartEventsDF$Answer[fixTheseXXXAnswerRows])
        theseDataEndRows <- as.numeric(thisChartEventsDF$Answer[fixTheseXXXAnswerRows])
        # check if the Label is the same for End and Answer
        XXXEndTxt <- thisChartDF$Label[theseDataEndRows]
        XXXAnswerTxt <- thisChartDF$Label[theseDataAnswerRows]
        fixThese <- which(XXXEndTxt != XXXAnswerTxt)
        if(length(fixThese > 0)) {
          # fix the time series data
          # set the Label to the preceding data row
          thisChartDF$Label[theseDataEndRows[fixThese]] <- thisChartDF$Label[(theseDataEndRows - 1)]
          # adjust this
          fixTheseXXXAnswerRows <- fixTheseXXXAnswerRows[-fixThese]
        }
        # fix the events data frame
        thisChartEventsDF$Answer[fixTheseXXXAnswerRows] <- ""
      }
    }
    
    {
      ## verbal Answers that exist on the same row as the End of an event ##
      
      fixEnds <- which(thisChartEventsDF$Answer == thisChartEventsDF$End)
      if(length(fixEnds) > 0) {
        # set the data Label column to the info in the preceeding row
        fixTheseDataRows <- as.numeric(thisChartEventsDF$Answer[fixEnds])
        useTheseDataRows <- fixTheseDataRows - 1
        thisChartDF$Label[fixTheseDataRows] <- thisChartDF$Label[useTheseDataRows]
        # also fix the eventLabel column
        thisChartDF$eventLabel[fixTheseDataRows] <- thisChartDF$eventLabel[useTheseDataRows]
        # fix the events data frame by removing the verbal answer index
        thisChartEventsDF$Answer[fixEnds] <- ""
      }
    }
    
    {
      # commented out Feb 23, 2025
      # because the problem is already corrected
      # but now there is a missing anser
      
      # ## check for answers that are equal to the End or question offset ##
      # 
      # # not for annotations
      # 
      # fixThese <- which(thisChartEventsDF$Answer == thisChartEventsDF$End)
      # if(length(fixThese) > 0) {
      #   fixChartDFRows <- as.numeric(thisChartEventsDF$Answer[fixThese])
      #   # first work with the data
      #   # set the Label to the preceding row before fixing the Label
      #   thisChartDF$Label[fixChartDFRows] <- thisChartDF$Label[(fixChartDFRows - 1)]
      #   # move the answer to the sample subseqeunt to stimulus end
      #   thisChartDF$Label[(fixChartDFRows + 1)] <- "Ans"
      #   # then fix the events data frame
      #   thisChartEventsDF$Answer[fixThese] <- (fixChartDFRows + 1)
      # }
      # # View(thisChartEventsDF)
    }
    
    if(fixMissingAnswers) {
      
      # check for and fix missing answers to test questions
      
      fixTheseAnswers <- NULL
      checkThese <- which(thisChartEventsDF$Answer[1:nrow(thisChartEventsDF)] == "")
      # includes X and XX and annotations for now, 
      fixTheseAnswers <- checkThese
      # remove annotations on a single row using the annotationRows vector
      annotationRows <- which(thisChartEventsDF$Begin == thisChartEventsDF$End)
      if(length(annotationRows > 0)) {
        # this will also exclude X XX announcements that occur on a single row
        fixTheseAnswers <- checkThese[-which(checkThese %in% annotationRows)]
      }
      # ignore the last event XX
      fixTheseAnswers <- fixTheseAnswers[which(fixTheseAnswers < nrow(thisChartEventsDF))]
      # ignore the first event X
      fixTheseAnswers <- fixTheseAnswers[which(fixTheseAnswers > 1)]
      # fix the missing answers
      if(length(fixTheseAnswers) > 0) {
        # randomize the answer distance from .5 to 1 seconds after stim end
        spoofAns <- sample(c(30,30), size=length(fixTheseAnswers), replace=TRUE)
        {
          # get the onset of the next event
          nextEvents <- fixTheseAnswers + 1
          # do not add another event
          nextEvents <- nextEvents[which(nextEvents <= nrow(thisChartEventsDF))]
          nextEventsRows <- as.numeric(thisChartEventsDF$Begin[nextEvents])
        }
        # set the new/missing answer rows
        newAnswerRows <- as.numeric(thisChartEventsDF$End[fixTheseAnswers]) + spoofAns
        # check that new answers do not overlap the next event
        omitThese <- which(newAnswerRows >= as.numeric(nextEventsRows))
        if(length(omitThese) > 0) {
          fixTheseAnswers <- fixTheseAnswers[-omitThese]
          newAnswerRows <- newAnswerRows[-omitThese]
        }
        # add the missing answer to the events DF
        thisChartEventsDF$Answer[fixTheseAnswers] <- newAnswerRows
        # add the missing answer to the data 
        thisChartDF$Label[newAnswerRows] <- "Ans"
        # use "Ans" to avoid problems with non-sensical answers
      }
      
    } # end if check for missing answers
    
  }
  
  #### check the ANNOTATIONS ####
  
  {
    #   # need to first check that annotations occur on a single row 
    #   # them make sure that annotations have no answer
    #   # remove verbal answers for annotation in the events data frame
    #   
    #   # annotationRows <- which(thisChartEventsDF$Begin == thisChartEventsDF$End)
    #   # Feb 27, 2024 to format the NCCA output correctly 
    #   annotationRows <- which(thisChartEventsDF$Begin == thisChartEventsDF$End)
    #   # annotationRows <- sort(unique(which(thisChartEventsDF$Begin == (thisChartEventsDF$End - 1))))
    #   
    #   # modified this loop on Feb 27, 2024
    #   # delete the answer row
    #   if(length(annotationRows > 0)) {
    #     # delete the answers from the time series data
    #     o=1
    #     for(o in 1:length(annotationRows)) {
    #       # delete the answer from  the time series data first
    #       thisAnswerRow <- as.numeric(thisChartEventsDF$Answer[annotationRows[o]])
    #       if(is.null(thisAnswerRow)) next()
    #       thisChartDF$Label[thisAnswerRow] <- ""
    #       thisChartDF$eventLabel[thisAnswerRow] <- ""
    #       # then ensure the annotation exists on a single row
    #       thisDataRow <- as.numeric(thisChartEventsDF$Begin[annotationRows[o]]) + 1
    #       thisChartDF$Label[thisDataRow] <- ""
    #       thisChartDF$eventLabel[thisDataRow] <- ""
    #       # Feb 27, 2024
    #       # in case there is an answer in the time series data
    #       thisDataRow <- as.numeric(thisChartEventsDF$Begin[annotationRows[o]]) + 2
    #       thisChartDF$Label[thisDataRow] <- ""
    #       thisChartDF$eventLabel[thisDataRow] <- ""
    #       
    #     }
    #     # delete the answers in the events data frame
    #     thisChartEventsDF$Answer[annotationRows] <- ""
    #     thisChartEventsDF$End[annotationRows] <- thisChartEventsDF$Begin[annotationRows]
    #   }
    # 
    # # Feb 27, 2024 - this does not seem to work for imported charts
    # # because annotations are already adjusted 
    # # so that end and answer are on rows subsequent to the Begin
  }
  
  #### check the answer distance for RQs and CQs and other questions ####
  
  if(isTRUE(fixAnswerDistance)) {
    
    # check the answer distance for all events except X and XX
    # annotations will give distance == NA because of no answers
    
    # get the question offset or end rows
    chartDFOffsetRows <- 
      as.numeric(thisChartEventsDF$End[1:nrow(thisChartEventsDF)])
    chartAnswerRows <- 
      as.numeric(thisChartEventsDF$Answer[1:nrow(thisChartEventsDF)])
    answerDistance <- chartAnswerRows - chartDFOffsetRows
    fixThese <- which(answerDistance < 20 | answerDistance > 38)
    # exclude X and XX answers
    fixThese <- fixThese[!(fixThese %in% c(1, nrow(thisChartEventsDF)))]
    # fixThese is offset by 1 row because started at 2
    if(length(fixThese) > 0) {
      # get the old answer rows in the data
      oldAnswerRows <- as.numeric(thisChartEventsDF$Answer[fixThese])
      # Clear the answer in the events data frame
      thisChartEventsDF$Answer[fixThese] <- ""
      # save the old answer text
      oldAnswerTxt <- thisChartDF$Label[oldAnswerRows]
      # sample a new distance from .67 to 1.25 seconds.
      answerDistance[fixThese] <- 
        sample(c(30,30), size=length(fixThese), replace=TRUE)
      # get the new answer rows
      newAnswerRows <- chartDFOffsetRows[fixThese] + answerDistance[fixThese]
      # set the new answer location in the events data frame
      thisChartEventsDF$Answer[fixThese] <- newAnswerRows
      # clear the answer in the times series data frame for this chart
      thisChartDF$Label[oldAnswerRows] <- ""
      # set the new answer in the time series data
      thisChartDF$Label[newAnswerRows] <- oldAnswerTxt
      
    }
    
    # View(thisChartEventsDF)
    # View(thisChartDF)
    # check other questions 
    
  }
  
  #### check again and remove answers for annotations ####
  
  {
    # commented out Feb 27, 2024
    
    # # annotations have end == begin, so they have a single row
    # # annotations have no answer in the NCCA ASCII output
    # 
    # # locate the events for which the end == begin and delete the answer
    # fixThese <- which(thisChartEventsDF$Begin == thisChartEventsDF$End)
    # 
    # # include events on 2 rows
    # # fixThese2 <- which(thisChartEventsDF$Begin == thisChartEventsDF$End - 1)
    # # fixThese <- sort(c(fixThese, fixThese2))
    # 
    # if(length(fixThese) > 0) {
    #   o=1
    #   for(o in 1:length(fixThese)) {
    #     # fix the answer in the time series data first
    #     thisDataRow <- as.numeric(thisChartEventsDF$Answer[fixThese[o]])
    #     thisChartDF$Label[thisDataRow] <- ""
    #     # then fix the answer in the events data frame
    #     thisChartEventsDF$Answer[fixThese[o]] <- ""
    #     # fix the end in the data and events data frame
    #     thisDataRow <- as.numeric(thisChartEventsDF$Begin[fixThese[o]]) + 1
    #     thisChartDF$Label[thisDataRow]
    #     thisChartEventsDF$End[fixThese[o]] <- thisChartEventsDF$Begin[fixThese[o]]
    #     
    #   }
    # }
  }
  
  #### check the label for R10 because some people use 0 and "O" carelessly ####
  
  {
    # # check the label for question 10 - some people use "O" instead of 0
    # fixThese <- which(thisChartEventsDF$Label %in% c("1OR", "R1O"))
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese] 
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       correctLabel <- ifelse(thisChartEventsDF$Label[fixThese[o]] == "1OR", 
    #                              "10R", 
    #                              "R10" )
    #       thisChartEventsDF$Label[fixThese[o]] <- correctLabel
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- correctLabel
    #     }
    #   }
    # }
  }
  
  #####################################################################
  
  #### Sept 21, 2021 Check and fix the Sacrifice Question Labels ####
  
  {
    # # for all exams
    # fixThese <- grep(pattern="2", x=thisChartEventsDF$Label)
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "SA2"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "SA2"
    #     }
    #   }
    # }
  }
  
  #### Sept 21, 2021 Check and fix the Introductory Question Labels ####
  
  {
    # # for Utah exams
    # fixThese <- 2 # Int question is always 2nd after the X announcement
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "Int1"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "Int1"
    #     }
    #   }
    # }
  }
  
  #### Sept 21, 2021 Check and fix the question Label for symptomatic 3 ####
  
  {
    # # for all FZCT and BiZone
    # fixThese <- grep(pattern="3", x=thisChartEventsDF$Label)
    # fixThese2 <- grep(pattern="C", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "E3"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "E3"
    #     }
    #   }
    # }
  }
  
  #### Sept 21, 2021 Check and fix the question Label for symptomatic 8 ####
  
  {
    # # for all FZCT and BiZone
    # fixThese <- grep(pattern="8", x=thisChartEventsDF$Label)
    # fixThese2 <- grep(pattern="C", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # fixThese2 <- grep(pattern="R", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "E8"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "E8"
    #       # needs to be E9 for BiZone
    #     }
    #   }
    # }
  }
  
  #### June 12, 2023 Check and fix the question Label for Neutral 8 questions ####
  
  {
    # # for all FZCT and BiZone
    # fixThese <- grep(pattern=c("8"), x=thisChartEventsDF$Label)
    # fixThese2 <- grep(pattern="C", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # fixThese2 <- grep(pattern="R", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "N8"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "N8"
    #     }
    #   }
    # }
  }
  
  #### Sept 21, 2021 Check and fix the question Label for Neutral 1 questions ####
  
  {
    # # for all FZCT and BiZone
    # fixThese <- grep(pattern=c("1"), x=thisChartEventsDF$Label)
    # fixThese2 <- grep(pattern="C", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # fixThese2 <- grep(pattern="R", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "N1"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "N1"
    #     }
    #   }
    # }
  }
  
  #### Sept 21, 2021 Check and fix the question Label for Neutral 2 questions ####
  
  {
    # # for all DLST and PCAT
    # fixThese <- grep(pattern=c("2"), x=thisChartEventsDF$Label)
    # fixThese2 <- grep(pattern="C", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # fixThese2 <- grep(pattern="R", x=thisChartEventsDF$Label)
    # fixThese <- fixThese[which(!(fixThese %in% fixThese2))]
    # if(length(fixThese) > 0) {
    #   theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    #   theseRowsEnd <- thisChartEventsDF$End[fixThese]
    #   o=1
    #   if( !is.na(fixThese[1] == "") ) {
    #     for(o in 1:length(fixThese)) {
    #       if(fixThese[o] == "") next()
    #       thisChartEventsDF$Label[fixThese[o]] <- "N2"
    #       thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- "N2"
    #     }
    #   }
    # }
  }
  
  #### June 12, 2023 Check and fix the question Label for all procedural questions ####
  
  fixPLabels <- FALSE
  # fixPlabels <- TRUE
  if( (fixRotation && fixPlabels) && seriesName == "2" ) {
    # for all Canadian A series exams
    
    questionTags <- switch(newChartName,
                           "01A"=c("N1", "SA2", "SY3", "N8"),
                           "02A"=c("N8", "SA2", "SY3", "N1"),
                           "03A"=c("N1", "SA2", "SY3", "N8") )

    fixThese <- c(2, 3, 4, 9)
    
    if(length(fixThese) > 0) {
      theseRowsStart <- thisChartEventsDF$Begin[fixThese]
      theseRowsEnd <- thisChartEventsDF$End[fixThese]
      o=1
      for(o in 1:length(fixThese)) {
        if(fixThese[o] == "") next()
        # fix the events data frame
        thisChartEventsDF$Label[fixThese[o]] <- questionTags[o]
        # fix the time series data 
        thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- questionTags[o]
      }
    }
  }
  
  ###############################################################
  
  #### sept 20, 2021 check the length of CQs and RQs ####
  
  fixRQCQLength <- FALSE
  # fixRQCQLength <- TRUE
  if( (fixQuestionLength || fixRQCQLength) && seriesName!=1 ) {
    
    RQRows <- grep("R", thisChartEventsDF$Label)
    CQRows <- grep("C", thisChartEventsDF$Label)
    
    # View(thisChartEventsDF)
    
    RQLengths2 <- RQLengths2[1:length(RQRows)]
    CQLengths2 <- CQLengths2[1:length(CQRows)]
    
    RQStart <- as.numeric(thisChartEventsDF$Begin[RQRows])
    RQEnd <- as.numeric(thisChartEventsDF$End[RQRows])
    
    # (RQEnd - RQStart) / cps
    
    CQStart <- as.numeric(thisChartEventsDF$Begin[CQRows])
    CQEnd <- as.numeric(thisChartEventsDF$End[CQRows])
    
    # (CQEnd - CQStart) / cps
    
    oldRQEnd <- as.numeric(thisChartEventsDF$End[RQRows])
    oldCQEnd <- as.numeric(thisChartEventsDF$End[CQRows])
    
    oldRQAnswer <- as.numeric(thisChartEventsDF$Answer[RQRows])
    oldCQAnswer <- as.numeric(thisChartEventsDF$Answer[CQRows])
    
    newRQEnd <- RQStart + RQLengths2 - 1
    newCQEnd <- CQStart + CQLengths2 - 1
    
    # new RQ Answers from .5 to 1.25 seconds
    newRQAnswer <- newRQEnd + sample(c(15:37), size=length(newRQEnd), replace=TRUE)
    # new CQ Answer from .67 to 1.5 seconds
    newCQAnswer <- newCQEnd + sample(c(20:45), size=length(newCQEnd), replace=TRUE)
    
    # adjust the rows and answers in the events data frame
    
    thisChartEventsDF$Answer[RQRows] <-  newRQAnswer
    thisChartEventsDF$Answer[CQRows] <-  newCQAnswer
    
    thisChartEventsDF$End[RQRows] <-  newRQEnd
    thisChartEventsDF$End[CQRows] <-  newCQEnd
    
    # remove the old events and  answers in the time series data
    
    thisChartDF$Label[oldRQAnswer] <- ""
    thisChartDF$Label[oldCQAnswer] <- ""
    
    # thisChartDF$Label
    
    # iterate over the RQs and CQs to fix the questions
    
    o=1
    for(o in 1:length(RQStart)) {
      thisChartDF$Label[RQStart[o]:oldRQEnd[o]] <- ""
      thisChartDF$Label[RQStart[o]:newRQEnd[o]] <-  thisChartEventsDF$Label[RQRows][o]
    }
    
    o=1
    for(o in 1:length(CQStart)) {
      thisChartDF$Label[CQStart[o]:oldCQEnd[o]] <- ""
      thisChartDF$Label[CQStart[o]:newCQEnd[o]] <-  thisChartEventsDF$Label[CQRows][o]
    }
    
    # thisChartDF$Label[newRQAnswer] <- "Ans"
    # thisChartDF$Label[newCQAnswer] <- "Ans"
    
    thisChartDF$Label[newRQAnswer] <- "No"
    thisChartDF$Label[newCQAnswer] <- "No"
    
    # add the adjusted events and answer to the time series data
    
    RQLabels <- thisChartEventsDF$Label[RQRows]
    CQLabels <- thisChartEventsDF$Label[CQRows]
    
    # View(thisChartEventsDF)
    
    # eventLabel column is not used for output to NCCA ASCII
    
  } 
  
  #### check the length of the Sacrifice Question ####
  
  fixSALength <- FALSE
  # fixSALength <- TRUE
  if( (fixQuestionLength || fixSALength) && seriesName!=1 ) {
    
    # View(thisChartEventsDF)
    SAQuestions <- c("SA2", "2SA", "SA", "SA3", "3SA")
    SARows <- which(thisChartEventsDF$Label %in% SAQuestions)
    
    SALength2 <- SALength2[1:length(SARows)]
    
    SAStart <- as.numeric(thisChartEventsDF$Begin[SARows])
    SAEnd <- as.numeric(thisChartEventsDF$End[SARows])
    
    # (SAEnd - SAStart) / cps
    
    oldSAEnd <- as.numeric(thisChartEventsDF$End[SARows])
    
    oldSAAnswer <- as.numeric(thisChartEventsDF$Answer[SARows])
    
    newSAEnd <- SAStart + SALength2 - 1
    
    # new Answers from .67 to 1.25 seconds
    newSAAnswer <- newSAEnd + sample(c(30,30), size=length(newSAEnd), replace=TRUE)
    # adjust the rows and answers in the events data frame
    
    thisChartEventsDF$Answer[SARows] <-  newSAAnswer
    
    thisChartEventsDF$End[SARows] <-  newSAEnd
    
    # remove the old events and  answers in the time series data
    
    thisChartDF$Label[oldSAAnswer] <- ""
    
    # thisChartDF$Label
    
    # iterate over the SA questions to fix the questions
    
    o=1
    for(o in 1:length(SAStart)) {
      thisChartDF$Label[SAStart[o]:oldSAEnd[o]] <- ""
      thisChartDF$Label[SAStart[o]:newSAEnd[o]] <-  thisChartEventsDF$Label[SARows][o]
    }
    
    # thisChartDF$Label[newSAAnswer] <- "Ans"
    thisChartDF$Label[newSAAnswer] <- "Yes"
    
    # add the adjusted events and answer to the time series data
    
    SALabels <- thisChartEventsDF$Label[SARows]
    # View(thisChartEventsDF)
    
    # eventLabel column is not used for output to NCCA ASCII
    
  }
  
  #### check the length of the Introductory and Symptomatic Questions ####
  
  fixOILength <- FALSE
  # fixOILength <- TRUE
  if( (fixQuestionLength || fixOILength) && seriesName!=1 ) {
    all()
    # View(thisChartEventsDF)
    SYQuestions <- c("Int", "Int1", "1Int", "3E", "E3", "8E", "E8", "I1", "SY", "SY3", "SY8")
    OQRows <- which(thisChartEventsDF$Label %in% SYQuestions)
    
    SYLengths2 <- SYLengths2[1:length(OQRows)]
    
    OQStart <- as.numeric(thisChartEventsDF$Begin[OQRows])
    OQEnd <- as.numeric(thisChartEventsDF$End[OQRows])
    
    # (OQEnd - OQStart) / cps
    
    oldOQEnd <- as.numeric(thisChartEventsDF$End[OQRows])
    
    oldOQAnswer <- as.numeric(thisChartEventsDF$Answer[OQRows])
    
    newOQEnd <- OQStart + SYLengths2 - 1
    
    # new Answers from .67 to 1.25 seconds
    newOQAnswer <- newOQEnd + sample(c(20:38), size=length(newOQEnd), replace=TRUE)
    # adjust the rows and answers in the events data frame
    
    thisChartEventsDF$Answer[OQRows] <-  newOQAnswer
    
    thisChartEventsDF$End[OQRows] <-  newOQEnd
    
    # remove the old events and  answers in the time series data
    
    thisChartDF$Label[oldOQAnswer] <- ""
    
    # thisChartDF$Label
    
    # iterate over the Int nd SY questions to fix the questions
    
    o=1
    for(o in 1:length(OQStart)) {
      thisChartDF$Label[OQStart[o]:oldOQEnd[o]] <- ""
      thisChartDF$Label[OQStart[o]:newOQEnd[o]] <-  thisChartEventsDF$Label[OQRows][o]
    }
    
    # thisChartDF$Label[newOQAnswer] <- "Ans"
    
    thisChartDF$Label[newOQAnswer] <- "Yes"
    
    # Need to set Symptomatic 8 answer to "No" in the time series data
    thisChartDF$Label[which( thisChartDF$Label %in% c("E8", "8E", "SY8"))] <- "No"
    # does not need to be set in the events data fram
    
    # add the adjusted events and answer to the time series data
    
    OQLabels <- thisChartEventsDF$Label[OQRows]
    # View(thisChartEventsDF)
    
    # eventLabel column is not used for output to NCCA ASCII
    
  }
  
  #### check the length of Neutral Questions ####
  
  fixNeutralLength <- FALSE
  # fixNeutralLength <- TRUE
  if(fixQuestionLength || fixNeutralLength) {
    
    # View(thisChartEventsDF)
    
    {
      NQuestions <- c("N1", "1N", "N2", "2N", "7N", "N7", "N8", "8N", "N11", "11N")
      NQuestions <- c(NQuestions, c("1", "2", "3", "4", "5", "6", "7"))
      NQuestions <- c(NQuestions, c("4K", "K4", "4KA", "K4A"))
      NQuestions <- c(NQuestions, c("1N1", "1N2", "1N3", "1N4"))
      NQuestions <- c(NQuestions, c("2N1", "2N2", "2N3", "2N4"))
      NQuestions <- c(NQuestions, c("3N1", "3N2", "3N3", "3N4"))
      NQuestions <- c(NQuestions, c("4N1", "4N2", "4N3", "4N4"))
    }
    
    NQRows <- which(thisChartEventsDF$Label %in% NQuestions)
    
    NLengths2 <- NLengths2[1:length(NQRows)]
    
    NQStart <- as.numeric(thisChartEventsDF$Begin[NQRows])
    NQEnd <- as.numeric(thisChartEventsDF$End[NQRows])
    
    # (NQEnd - NQStart) / cps
    
    oldNQEnd <- as.numeric(thisChartEventsDF$End[NQRows])
    
    oldNQAnswer <- as.numeric(thisChartEventsDF$Answer[NQRows])
    
    newNQEnd <- NQStart + NLengths2 - 1
    
    # new Answers from .67 to 1.25 seconds
    newNQAnswer <- newNQEnd + sample(c(20:38), size=length(newNQEnd), replace=TRUE)
    # adjust the rows and answers in the events data frame
    
    thisChartEventsDF$Answer[NQRows] <-  newNQAnswer
    
    thisChartEventsDF$End[NQRows] <-  newNQEnd
    
    # remove the old events and  answers in the time series data
    
    thisChartDF$Label[oldNQAnswer] <- ""
    
    # thisChartDF$Label
    
    # iterate over the neutral questions to fix the questions
    
    o=1
    for(o in 1:length(NQStart)) {
      thisChartDF$Label[NQStart[o]:oldNQEnd[o]] <- ""
      thisChartDF$Label[NQStart[o]:newNQEnd[o]] <-  thisChartEventsDF$Label[NQRows][o]
    }
    
    # thisChartDF$Label[newNQAnswer] <- "Ans"
    thisChartDF$Label[newNQAnswer] <- "No"
    
    # add the adjusted events and answer to the time series data
    
    NQLabels <- thisChartEventsDF$Label[NQRows]
    
    # View(thisChartEventsDF)
    
    # eventLabel column is not used for output to NCCA ASCII
    
  }
  
  #### check the location of the XX ###
  
  {
    
    # # June 13, 2023
    # 
    # XXEventIdx <- which(thisChartEventsDF$Label == "XX")
    # 
    # if(length(XXEventIdx) > 0) {
    #   
    #   lastEventIdx <- XXEventIdx - 1
    #   
    #   lastEventBegin <- thisChartEventsDF$Begin[lastEventIdx] 
    #   lasteEventEnd <- thisChartEventsDF$End[lastEventIdx]
    #   XXBegin <- thisChartEventsDF$Begin[XXEventIdx]
    #   XXEnd <- thisChartEventsDF$End[XXEventIdx]
    #   
    #   XXDistance <- (XXBegin - lastEventBegin) / 30
    #   
    #   if(length(lastEventBegin) != 0) {
    #     if(abs(XXDistance) > 20.5) {
    #       newXXBegin <- thisChartEventsDF$Begin[lastEventIdx] + 780 + sample(c(30,30), 1, replace=TRUE)
    #       newXXEnd <- thisChartEventsDF$Begin[XXEventIdx] + sample(c(60:60), 1, replace=TRUE)
    #       # check if the XX exceeds the length of the time series data
    #       if(newXXEnd > nrow(thisChartDF)) {
    #         newXXBegin <- nrow(thisChartDF) - 75
    #         newXXend <- nrow(thisChartDF) - 45
    #       }
    #       # set the new XX in the events data frame
    #       thisChartEventsDF$Begin[XXEventIdx] <- newXXBegin
    #       thisChartEventsDF$End[XXEventIdx] <- newXXEnd
    #       # remove the old XX from the time series data
    #       thisChartDF$Label[XXBegin:nrow(thisChartDF)] <- ""
    #       # set the new XX in the time series data
    #       # thisChartDF$Label[thisChartEventsDF$Begin[XXEventIdx]:thisChartEventsDF$End[XXEventIdx]] <- "XX"
    #       thisChartDF$Label[newXXBegin:newXXEnd] <- "XX"
    #     }
    #   }
    #   
    # }
    
  }
  
  #### check the length of the X and XX announcements  ####
  
  fixXXXLength <- FALSE
  # fixXXXLength <- TRUE
  if(fixQuestionLength || fixXXXLength) {
    
    # View(thisChartEventsDF)
    XXXQuestions <- c("X", "XX")
    XXXRows <- which(thisChartEventsDF$Label %in% XXXQuestions)
    
    XXXLengths2 <- XXXLengths2[1:length(XXXRows)]
    
    XXXStart <- as.numeric(thisChartEventsDF$Begin[XXXRows])
    XXXEnd <- as.numeric(thisChartEventsDF$End[XXXRows])
    
    # (XXXEnd - XXXStart) / cps
    
    oldXXXEnd <- as.numeric(thisChartEventsDF$End[XXXRows])
    
    oldXXXAnswer <- as.numeric(thisChartEventsDF$Answer[XXXRows])
    
    newXXXEnd <- XXXStart + XXXLengths2 - 1
    
    if(newXXXEnd[2] >= nrow(thisChartDF)) {
      diffValue <- newXXXEnd[2] - nrow(thisChartDF)
      newXXXEnd[2] <- newXXXEnd[2] - (diffValue + 60)
      # fix the events data frame
      thisChartEventsDF$End[XXXRows[2]] <- newXXXEnd[2]
      oldXXXEnd[2] <- nrow(thisChartDF)
    }
    
    {
      # NO ANSWERS FOR X AND XX ANNOUNCEMENTS 
      
      # new Answers from .67 to 1.25 seconds
      newXXXAnswer <- newXXXEnd + sample(c(30, 30), size=length(XXXQuestions), replace=TRUE)
      # adjust the rows and answers in the events data frame
      
      thisChartEventsDF$Answer[XXXRows] <-  ""
      
      # thisChartEventsDF$End[XXXRows] <-  newXXXEnd
    }
    
    # remove the old events and  answers in the time series data
    
    for(o in 1:length(oldXXXAnswer)) {
      if(is.na(oldXXXAnswer[o])) next()
      if(oldXXXAnswer[o] > nrow(thisChartDF)) next()
      thisChartDF$Label[oldXXXAnswer[o]] <- ""
    }
    
    # thisChartDF$Label
    
    # iterate over the Int X AND XX announcements to fix the questions
    
    o=1
    for(o in 1:length(XXXStart)) {
      thisChartDF$Label[XXXStart[o]:oldXXXEnd[o]] <- ""
      thisChartDF$Label[XXXStart[o]:newXXXEnd[o]] <-  thisChartEventsDF$Label[XXXRows][o]
    }
    
    # NO ANSWER FOR X AND XX announcements
    # thisChartDF$Label[newOQAnswer] <- "Ans"
    
    # add the adjusted events and answer to the time series data
    
    XXXLabels <- thisChartEventsDF$Label[XXXRows]
    # View(thisChartEventsDF)
    
    # eventLabel column is not used for output to NCCA ASCII
    
  }
  
  ########### dithering response onset, end, and answer ###################
  
  if(isTRUE(ditherEvents)) {
    
    # ditherEvents is an input parameter
    
    # introduce small variation to the begin, end and answer locations
    
    oldBeginIdx <- as.numeric(thisChartEventsDF$Begin)
    oldEndIdx <- as.numeric(thisChartEventsDF$End)
    oldAnswerIdx <- as.numeric(thisChartEventsDF$Answer)
    
    eventLabel <- thisChartEventsDF$Label
    
    # # exclude annotations (for which there is no verbal answer)
    # oldBeginIdx <- oldBeginIdx[which(oldAnswerIdx != "")]
    # oldEndIdx <- oldEndIdx[which(oldAnswerIdx != "")]
    # eventLabel <- eventLabel[which(oldAnswerIdx != "")]
    # 
    # oldAnswerIdx <- oldAnswerIdx[which(oldAnswerIdx != "")]
    
    # answers
    o=1
    for(o in 1:length(eventLabel)) {
      # next if no answer
      if(oldAnswerIdx[o] == "" || is.na(oldAnswerIdx[o])) next()
      if(eventLabel[o] == "") next()
      answerWord <- thisChartDF$Label[as.numeric(oldAnswerIdx[o])]
      newIdx <- as.numeric(oldAnswerIdx[o]) + sample(c(-3:3), 1, replace=TRUE)
      if(as.numeric(oldAnswerIdx[o]) <= (as.numeric(oldEndIdx[o])+1)) next()
      if(newIdx < (oldEndIdx[o]+20)) { newIdx <- oldEndIdx[o]+20 }
      thisChartDF$Label[as.numeric(oldAnswerIdx[o])] <- ""
      thisChartDF$Label[newIdx] <- answerWord
      thisChartEventsDF$Answer[o] <- newIdx
    }
    
    # Begin
    o=1
    for(o in 1:length(eventLabel)) {
      # next if no answer
      if(oldAnswerIdx[o] == "" || is.na(oldAnswerIdx[o])) next()
      oldBegin <- oldBeginIdx[o]
      newBegin <- oldBegin + sample(c(-4:4), 1, replace=TRUE)
      if(newBegin <= oldBegin) {
        thisChartDF$Label[newBegin:oldBegin] <- thisChartDF$Label[oldBegin]
      } else {
        thisChartDF$Label[(oldBegin:(newBegin-1))] <- ""
      }
      thisChartEventsDF$Begin[o] <- newBegin
    }
    
    # end
    for(o in 1:length(eventLabel)) {
      # next if no answer
      if(oldAnswerIdx[o] == "" || is.na(oldAnswerIdx[o])) next()
      oldEnd <- oldEndIdx[o]
      newEnd <- oldEnd + sample(c(-3:3), 1, replace=TRUE)
      if(as.numeric(oldAnswerIdx[o]) <= (oldEndIdx[o]+1)) next()
      if(newEnd >= oldEnd) {
        thisChartDF$Label[oldEnd:newEnd] <- thisChartDF$Label[oldEnd]
      } else {
        thisChartDF$Label[((newEnd+1):oldEnd)] <- ""
      }
      thisChartEventsDF$End[o] <- newEnd
    }
    
  }
  
  #######################################################
  
  #### ensure that RQ and CQ answers are "NO" ####
  
  {  
    # check the answers for RQ, CQ and other questions 
    RQRows <- grep("R", thisChartEventsDF$Label)
    CQRows <- grep("C", thisChartEventsDF$Label)
    RQCQRows <- sort(c(RQRows, CQRows))
    # get the answer rows in the chart time series data frame
    RQCQChartDFAnswerRows <- as.numeric(thisChartEventsDF$Answer[RQCQRows])
    # fix RQ and CQ answers that are not NO
    fixThese <- which(thisChartDF$Label[RQCQChartDFAnswerRows] != "No")
    
    # <> need to check the answer distance to ensure that the answer is not embedded in the next question
    
    if(length(fixThese) > 0) {
      # does not fix missing answers
      thisChartDF$Label[RQCQChartDFAnswerRows[fixThese]] <- "No"
    }
  }
  
  #### check the questions with YES answers ####
  
  {    
    theseQuestions <- c("1", "1A", "1B", "1D", "1E", "2", "3")
    theseQuestions <- c(theseQuestions, c("3", "3E", "E3", "SY", "3SY", "SY3", "SA", "SA2", "2SA", "Int", "I1", "I4", "I7", "1I", "4I", "7I"))
    theseQuestions <- c(theseQuestions, c("N1", "N2", "N3", "N4", "N7", "N8", "N11", "1N", "2N", "3N", "4N", "7N", "8N", "11N"))
    
    # question E8 has a NO answer for Federal ZCT exams 
    # question 8 is neutral and has YES answer for Canadian A Series ZCT
    # theseQuestions <- c(theseQuestions, c("8E", "E8"))
    
    checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    
    # get the rows to fix in the time series data
    fixThese <-  theseRows[which(thisChartDF$Label[theseRows] != "Yes")]
    if(length(fixThese) > 0) {
      thisChartDF$Label[fixThese] <- "Yes"
    }
  }
  
  #### check for No answers for ACQT charts ####
  
  {
    # theseQuestions <- c("1", "2", "3", "4", "4K", "4Key", "5", "6", "7", "4KA", "4A", "4KeyA", "Key", "Key4", "Key4A")
    # checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)
    # 
    # # set the answer for ACQT questions
    # if(length(checkThese>0)) {
    #   theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    #   thisChartDF$Label[theseRows] <- "No"
    # }
  }
  
  #### question SY8 or E8 has a NO answer for Federal ZCT exams ####
  
  {
    # question 8 is Symptomatic and has a NO answer for Federal ZCT exams
    # question 8 is a Neutral for the Canadian ZCT (RCMP/CPC A Series)
    theseQuestions <- c("8", "8E", "E8", "8SY", "SY8")
    checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)
    # set the answer for these question
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    thisChartDF$Label[theseRows] <- "No"
    # thisChartDF$Label[checkThese] <- "No"
  }
  
  ###########################################
  
  #### initialize the question labels for different formats ####
  
  {

    {
      # initialize a long vector of all RQs and CQs for all test formats
      # events that are not in this list are annotations without answers

      # yes answered questions
      theseQuestions <- c("1", "1A", "1B", "1D", "2", "3", "3E", "E3", "4", "7", "8", "11")
      theseQuestions <- c(theseQuestions, c("N1", "1N", "N2", "2N", "N3", "3N", "7N", "N7", "N8", "N11", "11N"))
      theseQuestions <- c(theseQuestions, c("NI1", "1NI", "NI2", "2NI", "NI3", "3NI", "7NI", "NI7", "NI8", "NI11", "11NI"))  
      theseQuestions <- c(theseQuestions, c("N4", "4N", "N4", "4N", "N5", "5N", "N6", "6N")) 
      theseQuestions <- c(theseQuestions, c("NI4", "4NI", "NI4", "4NI", "NI5", "5NI", "NI6", "6NI"))
      theseQuestions <- c(theseQuestions, c("1N1", "1N2", "1N3", "1N4"))
      theseQuestions <- c(theseQuestions, c("2N1", "2N2", "2N3", "2N4"))
      theseQuestions <- c(theseQuestions, c("3N1", "3N2", "3N3", "3N4"))
      theseQuestions <- c(theseQuestions, c("4N1", "4N2", "4N3", "4N4"))
      
      # other questions for ACQT charts
      theseQuestions <- c(theseQuestions, c("4", "4K", "4KA", "4KEY", "KEY4", "R4KEY", "4KEYR", "5", "6", "7"))
      theseQuestions <- c(theseQuestions, c("3K", "3KA", "3KEY", "KEY3", "R3KEY", "3KEYR"))
      theseQuestions <- c(theseQuestions, c("5K", "5KA", "5KEY", "KEY5", "R5KEY", "5KEYR"))
      theseQuestions <- c(theseQuestions, c("N4", "4N", "N4", "4N", "N5", "5N", "N6", "6N")) 
      theseQuestions <- c(theseQuestions, c("NI4", "4NI", "NI4", "4NI", "NI5", "5NI", "NI6", "6NI"))
      
      # ZCT
      theseQuestions <- c(theseQuestions, c("C4", "4C", "C6", "6C", "C9", "9C"))
      theseQuestions <- c(theseQuestions, c("R5", "5R", "7R", "R7", "10R", "R10"))
      #  procedural questions
      theseQuestions <- c(theseQuestions, c("Int", "Int1", "1Int", "I1", "SA", "SA2", "SA3", "2SA", "3SA", "SY", "SYM", "3E", "E3", "8E", "E8", "SY3", "SY8"))
      # AFMGQT and LEPET
      theseQuestions <- c(theseQuestions, c("C3", "3C", "C5", "5C", "C7", "7C", "C8", "8C", "C9", "9C", "C10", "10C", "C11", "11C", "C12", "12C"))
      theseQuestions <- c(theseQuestions, c("R4", "4R", "R6", "6R", "R8", "8R", "R9", "9R", "R10", "10R", "R11", "11R"))
      # LEPET series 2
      theseQuestions <- c(theseQuestions, c("C23", "23C", "C25", "25C", "C27", "27C", "C28", "28C", "C29", "29C", "C30", "30C", "C31", "31C", "C32", "32C"))
      theseQuestions <- c(theseQuestions, c("R24", "24R", "R26", "26R", "R28", "28R", "R29", "29R", "R30", "30R", "R31", "31R"))
      # UTAH-3 and Utah-4 RQs and CQs
      theseQuestions <- c(theseQuestions, c("1C", "C1", "2C", "C2", "3C", "C3"))
      theseQuestions <- c(theseQuestions, c("R1", "1R", "R2", "2R", "R3", "3R", "R4", "4R"))
      # Canadian-Utah (RCMP/CPC) A Series RQs and CQs
      theseQuestions <- c(theseQuestions, c("C4", "4C", "C6", "6C", "C9", "9C"))
      theseQuestions <- c(theseQuestions, c("R5", "5R", "7R", "R7", "10R", "R10"))
      # TES/DLST and PCASS/PCAT
      theseQuestions <- c(theseQuestions, c("1R1", "1R2", "1R3", "2R1", "2R2", "2R3", "3R1", "3R2", "3R3", "4R1", "4R2", "4R3"))
      theseQuestions <- c(theseQuestions, c("1C1", "1C2", "1C3", "2C1", "2C2", "2C3", "3C1", "3C2", "3C3", "4C1", "4C2", "4C3"))
      
      # Army MGQT
      theseQuestions <- c(theseQuestions, c("I1", "1I", "I2", "2I", "I4", "4I", "7I", "I7"))
      theseQuestions <- c(theseQuestions, c("R3", "3R", "5R", "R5", "R8", "8R", "R9", "9R"))
      theseQuestions <- c(theseQuestions, c("C6", "6C", "C10", "10C"))
      
      # additional question labels
      theseQuestions <- c(theseQuestions, c("C4C", "C6C", "C9C", "C8C", "C10C", "C3C", "C5C", "C7C", "C1C", "C2C", "C3", "C10"))
      theseQuestions <- c(theseQuestions, c("R5R", "R7R", "R10R", "R4R", "R6R", "R8R", "R9", "R1R", "R2R", "R3R"))
      theseQuestions <- c(theseQuestions, c("S", "K", "U"))
    }

    theseQuestions <- unique(theseQuestions)

  }
  
  ########################################################################
  
  #### check for events that are not questions and remove the answers ####
  
  {
    # questions not in theseQuestions are annotations
    checkThese <- which(!(thisChartEventsDF$Label %in% theseQuestions)) 
    # locate the answers
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    theseRows <- theseRows[which(!is.na(theseRows))]
    
    # View the labels for events that have no verbal answer
    # thisChartEventsDF$Label[checkThese]
    
    # get the non-blank answer rows to fix in the time series data
    fixThese <-  theseRows[which(thisChartDF$Label[theseRows] != "")]
    if(length(fixThese) > 0) {
      # first remove the answer in the time series data
      thisChartDF$Label[fixThese] <- ""
      # then remove the answer in the events table
      thisChartEventsDF$Answer[checkThese] <- ""
    }
    
    # thisChartDF$Label[theseRows] <- ""
    # thisChartEventsDF$Answer[checkThese] <- ""
    
  }
  
  #### remove answers to X and XX events ####
  
  {
    # get the X and XX answer rows
    XXXRows <- 
      thisChartEventsDF$Answer[thisChartEventsDF$Label=="X" |
                                 thisChartEventsDF$Label=="XX"]
    XXXRows <- as.numeric(XXXRows[XXXRows != ""])
    
    # remove XX answers after the end of the time series data
    XXXRows <- XXXRows[which(XXXRows <= nrow(thisChartDF))]
    
    # check if there are answer labels that are not X or XX
    XXXRows <- XXXRows[!(thisChartDF$Label[XXXRows] %in% c("X", "XX"))]
    if(length(XXXRows) > 0) {
      thisChartDF$Label[XXXRows] <- ""
    }
    
    # set all of the X and XX answers to ""
    thisChartEventsDF$Answer[thisChartEventsDF$Label=="X" |
                               thisChartEventsDF$Label=="XX"] <- ""
    # View(thisChartEventsDF)
  }
  
  #### end check answers ####
  
  ######################################################
  
  #### select the RQ AND CQ rotation schedule ####
  
  {
    
    # Re-acquire the RQ and CQ rows
    
    # get the RQ and CQ rows in the events data frame
    RQRows <- grep("R", thisChartEventsDF$Label)
    CQRows <- grep("C", thisChartEventsDF$Label)
    
    # newChartName is the new name of the chart for the output data
    if(!exists("newChartName")) newChartName <- "03A"
    
  }
  
  if(isTRUE(fixCQRotation)) {
    
    ## CQ Rotation ##
    
    # 3 CQs Federal You-Phase / Bi-Zone
    CQ3RotationBZ <- switch(newChartName,
                            "01A"=c("C4", "C6", "C8"),
                            "02A"=c("C6", "C8", "C4"),
                            "03A"=c("C8", "C4", "C6") )
    # 3 CQs Federal ZCT
    CQ3RotationFZCT <- switch(newChartName,
                              "01A"=c("C4", "C6", "C9"),
                              "02A"=c("C6", "C9", "C4"),
                              "03A"=c("C9", "C4", "C6") ) 
    # 3 CQs Utah 3 Question
    CQ3RotationUT3 <- switch(newChartName,
                             "01A"=c("C4", "C7", "C10"),
                             "02A"=c("C7", "C10", "C4"),
                             "03A"=c("C10", "C4", "C7") ) 
    # 3 CQs Canadian RCMP/CPC A Series 3 Question
    CQ3RotationASer <- switch(newChartName,
                              "01A"=c("C4", "C6", "C9"),
                              "02A"=c("C9", "C4", "C6"),
                              "03A"=c("C4", "C6", "C9") ) 
    # 3 CQs Utah 4 Question
    CQ4RotationUT4 <- switch(newChartName,
                             "01A"=c("C4", "C7", "C10"),
                             "02A"=c("C7", "C10", "C4"),
                             "03A"=c("C10", "C4", "C7") ) 
    # 3 CQs AFMGQTV2
    CQ3RotationAF2 <- switch(newChartName,
                             "01A"=c("C3", "C6", "C9"),
                             "02A"=c("C6", "C9", "C3"),
                             "03A"=c("C9", "C3", "C6") ) 
    # 3 CQs AFMGQTV1
    CQ3RotationAF1 <- switch(newChartName,
                             "01A"=c("C3", "C5", "C7"),
                             "02A"=c("C5", "C7", "C3"),
                             "03A"=c("C7", "C3", "C5") )
    # 4 CQs AFMGQTV1
    CQ4RotationAF1 <- switch(newChartName,
                             "01A"=c("C3", "C5", "C7", "C9"),
                             "02A"=c("C5", "C7", "C9", "C3"),
                             "03A"=c("C7", "C9", "C3", "C5") )
    # 5 CQs AFMGQTV1 - with an extract CQ at the end of 4 RQs
    CQ5RotationAF1 <- switch(newChartName,
                             "01A"=c("C3", "C5", "C7", "C9", "C11"),
                             "02A"=c("C5", "C7", "C9", "C11", "C3"),
                             "03A"=c("C7", "C9", "C11", "C3", "C5") )
    # 4 CQs LEPET with inserted 7N
    CQ4RotationLPT <- switch(newChartName,
                             "01A"=c("C3", "C5", "C8", "C10"),
                             "02A"=c("C5", "C10", "C8", "C3"),
                             "03A"=c("C10", "C3", "C5", "C8") )
    # 5 CQs LEPET with inserted 7N
    CQ5RotationLPT <- switch(newChartName,
                             "01A"=c("C3", "C5", "C8", "C10", "C12"),
                             "02A"=c("C5", "C10", "C12", "C8", "C3"),
                             "03A"=c("C10", "C3", "C5", "C12", "C8") )
    
    ## DLST/TES ##
    
    # # TES/DLST with 2 RQs repeated 4 times
    # RQRotation <- c("1R1", "1R2", "2R1", "2R2", "3R1", "3R2", "4R1", "4R2")
    # # TES/DLST with 2 CQs repeated
    CQ2RotationDLST <- c("1C1", "1C2", "2C1", "2C2", "3C1", "3C2", "4C1", "4C2")
    
    ## PCASS/PCAT
    
    # # PCASS/PCAT with 3 RQs repeated 4 times
    # RQRotation <- c("1R1", "1R2", "1R3", "2R1", "2R2", "2R3", "3R1", "3R2", "3R3", "4R1", "4R2", "4R3")
    # # PCASS/PCAT with 2 RQs repeated 4 times
    # RQRotation <- c("1R1", "1R2", "2R1", "2R2", "3R1", "3R2", "4R1", "4R2")
    # # PCASS/PCAT with 3 CQs repeated 4 times
    CQ3RotationPCAT <- c("1C1", "1C2", "1C3", "2C1", "2C2", "2C3", "3C1", "3C2", "3C3", "4C1", "4C2", "4C3")
    CQ2RotationPCAT <- c("1C1", "1C2", "2C1", "2C2", "3C1", "3C2", "4C1", "4C2")
    
    {
      
      # rotation is based on the number of CQs and the format
      
      # CQRotation <- switch(as.character(length(CQRows)),
      #                      "3"=CQ3RotationAF2,
      #                      # "4"=CQ4RotationAF1,
      #                      "4"=CQ4RotationLPT,
      #                      "5"=CQ5RotationLPT ) 
      
      CQRotation <- CQ3RotationFZCT
      # CQRotation <- CQ3RotationBZ
      # CQRotation <- CQ3RotationASer
      
    }
    
  }
  
  if(isTRUE(fixRQRotation)) {
    
    ## RQ Rotation ##
    
    ## Federal ZCT ##
    
    # RQ rotation for Federal ZCT exams
    RQRotationFZCT <- c("R5", "R7", "R10")
    # RQs are traditionally not rotated for this format
    
    ## Federal You-Phase Bi-Zone ##
    
    # RQ rotation for Federal ZCT exams
    RQRotationFBZ <- c("R5", "R7")
    # RQs are traditionally not rotated for this format
    
    ## Canadian RCMP/CPC A Series ##
    
    # RQ rotation for Canadian RCMP/CPC A Series exams
    RQRotationASer <- switch(newChartName,
                             "01A"=c("R5", "R7", "R10"),
                             "02A"=c("R5", "R7", "R10"),
                             "03A"=c("R10", "R5", "R7") )
    # same tags as the FZCT but the RQs are rotated to the right
    # CQs are rotated to the left 
    
    ## Utah 3-Question format ##
    
    # RQ rotation for Utah 3 question exams
    RQRotationUT3 <- switch(newChartName,
                            "01A"=c("R5", "R8", "R11"),
                            "02A"=c("R11", "R5", "R8"),
                            "03A"=c("R", "R11", "R5") )
    # RQs are rotated to the right
    # CQs are rotated to the left 
    
    ## Utah 4-Question format ##
    
    # RQ rotation for Utah 3 question exams
    RQRotationUT4 <- switch(newChartName,
                            "01A"=c("R5", "R6", "R8", "R9"),
                            "02A"=c("R8", "R6", "R9", "R6"),
                            "03A"=c("R6", "R9", "R5", "R8") )
    # RQs are rotated to the right
    # CQs are rotated to the left 
    
    ## AFMGQTv2 ##
    
    # RQ rotation for AFMGQTv2 exams with 2 RQs
    # RQRotation <- c("R4", "R6")
    RQ2RotationAF2 <- switch(newChartName,
                             "01A"=c("R4", "R6"),
                             "02A"=c("R6", "R4"),
                             "03A"=c("R4", "R6") )
    
    # RQ rotation for AFMGQTv2 exams with 3 RQs
    # RQRotation <- c("R4", "R5", "R7")
    RQ3RotationAF2 <- switch(newChartName,
                             "01A"=c("R4", "R5", "R7"),
                             "02A"=c("R7", "R4", "R5"),
                             "03A"=c("R5", "R7", "R4") )
    
    # RQ rotation for AFMGQTv2 exams with 4 RQs
    # RQRotation <- c("R4", "R5", "R7", "R8")
    RQ4RotationAF2 <- switch(newChartName,
                             "01A"=c("R4", "R5", "R7", "R8"),
                             "02A"=c("R7", "R4", "R8", "R5"),
                             # "03A"=c("R5", "R8", "R4", "R7") )
                             "03A"=c("R4", "R5", "R7", "R8") ) 
    
    ## AFMGQT-LEPET ##
    
    # # RQ rotation for AFMGQT-LEPET exams 4 RQs with I7 and 5CQs
    # # RQRotation <- c("R4", "R6", "R9", "R11")
    RQ4RotationLPT <- switch(newChartName,
                             "01A"=c("R4", "R6", "R9", "R11"),
                             "02A"=c("R6", "R9", "R4", "R11"),
                             "03A"=c("R9", "R4", "R6", "R11") )
    
    {
      # RQ rotation is based on the test format and number of RQs
      
      # RQRotation <- switch(as.character(length(RQRows)),
      #                      "2"=RQ2RotationAF2,
      #                      "3"=RQ3RotationAF2,
      #                      # "4"=RQ4RotationLPT,
      #                      "4"=RQ4RotationAF2,
      #                      "5"=RQ5RotationAF2 )
      
      RQRotation <- RQRotationFZCT
      # RQRotation <- RQRotationASer
      # RQRotation <- RQRotationFBZ
      
    }
    
  }
  
  #### set the RQ and CQ rotation ####
  
  {
    
    if(isTRUE(fixRQRotation) && length(RQRows)>1) {
      
      ## RQ Rotation ##
      
      # fixRQRotation is a parameter that is set int he NCCAASCIIOutput.R script
      
      # set the new RQ rotation in the events data frame
      thisChartEventsDF$Label[RQRows] <- RQRotation
      
      # use a loop to set the RQ rotation in the time series data frame
      RQRowsStart <- thisChartEventsDF$Begin[RQRows]
      RQRowsEnd <- thisChartEventsDF$End[RQRows]
      o=1
      for(o in 1:length(RQRowsStart)) {
        thisChartDF$Label[RQRowsStart[o]:RQRowsEnd[o]] <- RQRotation[o]
      }
    }
    
    if(isTRUE(fixCQRotation) && length(CQRows)>0) {
      
      ## CQ Rotation ##
      
      # fixCQRotation is a parameter that is set in the NCCAASCIIOutput.R script
      
      # set the new CQ rotation in the events data frame
      thisChartEventsDF$Label[CQRows] <- CQRotation
      
      # use a loop to set the CQ rotation in the time series data frame
      CQRowsStart <- thisChartEventsDF$Begin[CQRows]
      CQRowsEnd <- thisChartEventsDF$End[CQRows]
      o=1
      for(o in 1:length(CQRowsStart)) {
        thisChartDF$Label[CQRowsStart[o]:CQRowsEnd[o]] <- CQRotation[o]
      }
    }
    
  }
  
  #### fix the RQ and CQ labels ####
  
  fixRQCQLabels <- FALSE
  # fixRQCQLabels <- TRUE
  if(isTRUE(fixLabels) && fixRQCQLabels) {
    
    # fixLabels is an input parameter for this function
    
    ## fix some odd problematic question labels for some datasets ###
    
    # RQ and CQ labels can also be fixed while setting the RQ and CQ rotation
    
    {
      
      # RQ rows in the events data frame
      RQRows <- grep("R", thisChartEventsDF$Label)
      
      {
        oldRQLabels <- c("R24", "R26", "R28", "R30", "R29", "R31")
        # oldRQLabels <- c(oldRQLabels, c("R8", "R10")) # for LEPET
        newRQLabels <- c("R4", "R6", "R9", "R11", "R9", "R11")
        # newRQLabels <- c(newRQLabels, c("R9", "R11")) # for LEPET
        
        fixTheseRQs <- RQRows[which(thisChartEventsDF$Label[RQRows] %in% oldRQLabels)]
      }
      
      RQRowsBegin <- as.numeric(thisChartEventsDF$Begin[fixTheseRQs])
      RQRowsEnd <- as.numeric(thisChartEventsDF$End[fixTheseRQs])
      
      # loop over the RQ labels
      if(length(fixTheseRQs) > 0) {
        o=1
        for(o in 1:length(fixTheseRQs)) {
          oldRQ <- thisChartEventsDF$Label[fixTheseRQs[o]]
          newRQ <- newRQLabels[which(oldRQLabels == oldRQ)]
          # fix the time series data frame first
          thisChartDF$Label[RQRowsBegin[o]:RQRowsEnd[o]] <- newRQ
          # then fix the chart events data frame
          thisChartEventsDF$Label[fixTheseRQs[o]] <- newRQ
        }
      } 
      
    }
    
    {
      
      # CQ Rows in the events data frame
      CQRows <- grep("C", thisChartEventsDF$Label)
      
      {
        oldCQLabels <- c("C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31", "C32")
        newCQLabels <- c("C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12")
        
        fixTheseCQs <- CQRows[which(thisChartEventsDF$Label[CQRows] %in% oldCQLabels)]
      }
      
      CQRowsBegin <- as.numeric(thisChartEventsDF$Begin[fixTheseCQs])
      CQRowsEnd <- as.numeric(thisChartEventsDF$End[fixTheseCQs])
      
      # loop over the CQ labels
      if(length(fixTheseCQs) > 0) {
        # fix the time series data frame first
        o=1
        for(o in 1:length(fixTheseCQs)) {
          oldCQ <- thisChartEventsDF$Label[fixTheseCQs[o]]
          newCQ <- newCQLabels[which(oldCQLabels == oldCQ)]
          # fix the time series data frame first
          thisChartDF$Label[CQRowsBegin[o]:CQRowsEnd[o]] <- newCQ
          # then fix the chart events data frame
          thisChartEventsDF$Label[fixTheseCQs[o]] <- newCQ
        }
      } 
      
    }
    
    # does not fix the eventLabel column
    # because it is not included in the NCCA ASCII output
    # and these changes are not saved
    
    # View(thisChartEventsDF)
    
  }
  
  #### fix duplicate events ####
  
  # override the input parameter with these
  # fixDuplicates <- FALSE
  # fixDuplicates <- TRUE
  # TRUE restores default labels (so that repeated events will have identical labels)
  if(!exists("fixDuplicates")) fixDuplicates <- TRUE
  
  if(isTRUE(fixDuplicates)) {
    
    # fixDuplicates is an input parameter for this function
    # restores duplicate labels that were adjusted at input
    
    o=1
    for(o in 1:nrow(thisChartEventsDF)) {
      # first make sure the Label and eventLabel are identical
      # non-identical values indicate a duplicate question that was re-named on import
      thisChartDF$eventLabel[thisChartEventsDF$Begin[o]:thisChartEventsDF$End[o]] <- 
        thisChartDF$Label[thisChartEventsDF$Begin[o]]
    }
    
    # then fix the answers in the time series data 
    # first select events with missing answers in the eventLabels column
    theseAnswers <- as.numeric(thisChartEventsDF$Answer[which(thisChartEventsDF$Answer != "")])
    # then get the answer from the Labels column
    thisChartDF$eventLabel[theseAnswers] <- thisChartDF$Label[theseAnswers]
    
    # ansText <- c("No", "NO", "no", "Yes", "YES", "yes", "Ans", "ANS", "ans")
    
  }
  
  ######################################
  
  #### construct the output names ####
  
  {
    examName <- unique(thisChartDF$examName)[1]
    
    # seriesName <- unique(thisChartDF$seriesName)[1]
    # seriesName <- "2"
    # seriesName <- outputSeriesName2
    
    # chartName <- unique(thisChartDF$chartName)[1]
    # chartName <- newChartName
    
    # initialize the name of the NCCA ASCII Outtput file
    NCCAASCIIName <- paste0("D&-", thisOutputName)
    
    print(paste0("format and write NCCA ASCII name: ", NCCAASCIIName) )
  }
  
  #### initialize the header info ####
  
  headerInfo <- c( # paste0("Name of this file: ", "NA"),
    paste0("Name of this file: ", NCCAASCIIName),
    # paste0("Source file: ", "NA"),
    paste0("Source file: ", str_sub(thisOutputName, 1, -7)),
    # paste0("Instrument: ", "Lafayette Windows"),
    paste0("Instrument: ", "Instrument Not Available"),
    paste0("Software Version: ", "NA"),
    paste0("Chart Date: ", "01JAN70"),
    paste0("Time: ", thisChartTime),
    paste0("Examination Number: ", outputSeriesName2),
    # paste0("Examination Number: 2"),
    paste0("Chart Number: ", newChartName),
    paste0("Number of questions: ", nrow(thisChartEventsDF)),
    paste0("Fastest Sample Rate (Hz): ", "30"),
    paste0("Number of samples: ", nrow(thisChartDF)),
    paste0("Number of channels: ", length(outputSensors2)),
    paste0("Sample Rate (Hz) - UPneumo: ", "30"),
    paste0("Sample Rate (Hz) - LPneumo: ", "30"),
    paste0("Sample Rate (Hz) - EDA1: ", "30"),
    paste0("Sample Rate (Hz) - Cardio1: ", "30"),
    if(isTRUE(inclMove1)) paste0("Sample Rate (Hz) - Move1: ", "30"),
    if(isTRUE(inclPPG2)) paste0("Sample Rate (Hz) - PPG1: ", "30"),
    if(isTRUE(inclAutoEDA)) paste0("Sample Rate (Hz) - EDA2: ", "30"),
    if(isTRUE(inclFC)) paste0("Sample Rate (Hz) - FC: ", "30")
    
  )
  
  #### initialize a vector for the list of stimulus events ####
  
  {
    stimText <- rep("", times=(nrow(thisChartEventsDF)+1))
    
    stimText[1] <- "Event    Label Statement"
    n=1
    for(n in 1:(length(stimText)-1)) {
      stimText[(n+1)] <- paste( str_pad(str_pad(n, 2, side="left", pad="0"), 5, side="right"),
                                # str_pad(str_pad(thisChartEventsDF$Event[n], 2, side="left", pad="0"), 5, side="right"),
                                
                                ifelse(fixDuplicates, 
                                       str_pad(thisChartEventsDF$Label[n], 8, side="left"),
                                       str_pad(thisChartEventsDF$eventLabel[n], 8, side="left")),
                                ifelse(fixDuplicates,
                                       str_pad(thisChartEventsDF$Label[n], 9, side="right"),
                                       str_pad(thisChartEventsDF$eventLabel[n], 9, side="right")) 
                                
      ) 
    }
    
    # print(stimText)
  }
  
  #### initialize a vector for the event locations ####
  
  {
    # initialize a vector
    eventInfo <- rep("", times=(nrow(thisChartEventsDF)+1))
    
    eventInfo[1] <- "Event    Label      Begin        End     Answer"
    
    n=1
    for(n in 1:(length(eventInfo)-1)) {
      eventInfo[(n+1)] <- paste( str_pad(str_pad(n, 2, side="left", pad="0"), 5, side="right"),
                                 # str_pad(str_pad(thisChartEventsDF$Event[n], 2, side="left", pad="0"), 5, side="right"),
                                 
                                 ifelse(fixDuplicates, 
                                        str_pad(thisChartEventsDF$Label[n], 8, side="left"),
                                        str_pad(thisChartEventsDF$eventLabel[n], 8, side="left")),
                                 # str_pad(thisChartEventsDF$Label[n], 8, side="left"),
                                 
                                 str_pad(thisChartEventsDF$Begin[n], 10, side="left"),
                                 str_pad(thisChartEventsDF$End[n], 10, side="left"),
                                 str_pad(thisChartEventsDF$Answer[n], 10, side="left"),
                                 " "  # add a space at the end of the line
                                 
      )
    }
    
    # View(thisChartEventsDF)
    # print(eventInfo)
  }
  
  #### initialize a data frame for the time series data ####
  
  {
    
    assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
    # stop()
    
    # View(thisChartDF)
    
    {
      # select the data columns for output in the NCCA ASCII file
      
      SampleNUmber <- round(as.integer(thisChartDF$Sample), 0)
      # Time <- thisChartDF$Time
      
      if(fixDuplicates) {
        Label <- thisChartDF$Label
      } else {
        Label <- thisChartDF$eventLabel
      }
      
      # unsmoothed
      UPneumo <- round(as.integer(thisChartDF$UPneumo), 0)
      LPneumo <- round(as.integer(thisChartDF$LPneumo), 0)
      EDA1 <- round(as.integer(thisChartDF$EDA1), 0)
      
      Cardio1 <- round(as.integer(thisChartDF$Cardio1), 0)
      
      if(isTRUE(inclMove1)) Move1 <- round(as.integer(thisChartDF$Move1), 0)
      if(isTRUE(inclPPG2)) PPG1 <- round(as.integer(thisChartDF$PPG1), 0)
      if(isTRUE(inclAutoEDA)) EDA2 <- round(as.integer(thisChartDF$c_AutoEDA), 0)
      if(isTRUE(inclFC)) FC <- round(as.integer(thisChartDF$FC), 0)
      
      if(all(Move1 == Move1[1])) Move1 <- NULL
      if(all(PPG1 == PPG1[1])) PPG1 <- NULL
     
      # if(isTRUE(inclMove1) && length(Move1) == 0) Move1 <- rep(-9.9, times=length(EDA1))
      #  
      # if(isTRUE(inclPPG2) && length(PPG1) == 0) PPG1 <- rep(-9.9, times=length(EDA1))
    }
    
    {
      ### Mar 8, 2021 calculate a new time scale
      
      chartTime <- fromMinSecFn(x=thisChartDF$Time, vct=FALSE)
      
      dataLength <- nrow(thisChartDF)
      
      # dataRate <- round(1 / (chartTime / dataLength))
      
      ## new time vector and new data rate ##
      
      # make a new time vector at the new data rate (adding 1 second)
      secondsVc <- round(seq(from=0.0, to=chartTime+1, by = .0333333333), 3)
      # head(secondsVc)
      # tail(secondsVc)
      
      # trim the time vector to the old data length
      secondsVc <- secondsVc[1:dataLength]
      
      newDataLength  <- length(secondsVc)
      # max(secondsVc)
      
      if(dataLength != newDataLength) stop("problem with output time scale")
      
      #  make a new time scale at the new data rate
      #  call the toMinSecFn() from the  toMinSec.R script
      Time <- toMinSecFn(secondsVc)
      
      # use the old time scale instead
      # Time <- thisChartDF$Time
    }
    
    {
      # initialize a vector for the time series data frame
      outputSensors3 <- c("UPneumo", "LPneumo", "EDA1", "Cardio1", "Move1", "PPG1")
      examDAT <- rep("", times=(nrow(thisChartDF)+1))
      examDAT[1] <- 
        paste0("Sample     Time    Label", paste(str_pad(outputSensors3, width=11, side="left"), collapse=""))
    }
    
    {
      # use a loop to add the text columns for the structured file output
      n=1
      for(n in 1:(length(examDAT)-1)) {
        examDAT[(n+1)] <- paste( str_pad(n, 6, side="left"),
                                 # str_pad(SampleNUmber[n], 6, side="left"),
                                 str_pad(str_sub(Time[n], 1, 8), 8, side="left"),
                                 str_pad(Label[n], 8, side="left", pad="-"),
                                 # now the time series data
                                 # each data column is 11 characters wide
                                 paste0(str_pad(as.integer(UPneumo[n]), 8, side="left"), ".0"),
                                 paste0(str_pad(as.integer(LPneumo[n]), 8, side="left"), ".0"),
                                 paste0(str_pad(as.integer(EDA1[n]), 8, side="left"), ".0"),
                                 paste0(str_pad(as.integer(Cardio1[n]), 8, side="left"), ".0"),
                                 if(isTRUE(inclMove1)) {
                                   ifelse(length(Move1)==0,
                                          str_pad(as.character(-9.9), 10, side="left"),
                                          paste0(str_pad(as.integer(Move1[n]), 8, side="left"), ".0") )
                                 },
                                 if(isTRUE(inclPPG2)) {
                                   ifelse(length(PPG1)==0,
                                          str_pad(as.character(-9.9), 10, side="left"),
                                          paste0(str_pad(as.integer(PPG1[n]), 8, side="left"), ".0") )
                                 },
                                 # does not work to use isTRUE here
                                 # if(isTRUE(inclAutoEDA)) paste0(str_pad(as.integer(EDA2[n]), 8, side="left"), ".0"),
                                 if(isTRUE(inclFC)) paste0(str_pad(as.integer(FC[n]), 8, side="left"), ".0")  
        )
      }
      
      # if(isTRUE(inclMove1) && length(Move1) == 0) Move1 <- rep(-9.9, times=length(EDA1))
      #  
      # if(isTRUE(inclPPG2) && length(PPG1) == 0) PPG1 <- rep(-9.9, times=length(EDA1))
      
      
    }
    
    # View(examDAT)
    # head(examDAT, 10)
    # tail(examDAT, 40)
    
  }
  
  #### write the NCCA ASCII output in the Lafayette format ####
  
  {
    # if(!exists("outputFolderName")) outputFolderName <- "NCCAASCIIOutput"
    
    # folderName <- paste0("FZCT_", str_sub(thisOutputName, -9, -7))
    
    writePFPathName <- paste0(outputFolderName,
                              "/",
                              NCCAASCIIName )
    
    # if(!dir.exists(folderName)) dir.create(folderName)
    
    writeLines(text=c(headerInfo, " ", stimText, " ", eventInfo, " ", examDAT), 
               con=writePFPathName)
  }
  
  #### output the name of the NCCA ASCII file ####
  
  # outputName <- paste0("D&-", 
  #                      examName, 
  #                      "_", 
  #                      chartFormat, 
  #                      "-", 
  #                      seriesName, 
  #                      ".", 
  #                      chartName)
  
  return(NCCAASCIIName)
  
} # end writeNCCAASCIIFn()




