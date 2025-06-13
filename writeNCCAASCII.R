# R function to export the test chart data to NCCA ASCII format
# 14 Dec 2018
# Raymond Nelson
#
##########



library(stringr)


# writeNCCAASCII <- TRUE


# examName <- "TES1"
# seriesName <- "1"
# chartName <- "01A"

writeNCCAASCIIFn <- function(thisChartDF=thisChartDF, 
                             thisChartEventsDF=thisChartEventsDF,
                             thisOutputName=thisOutputName,
                             outputSensors=outputSensors ) {
  # R function to write the NCCA ASCII file for each chart
  # 2020-02-08
  # Raymond Nelson
  #
  # called by the NCCAASCIIOutputFn() in the NCCAASCCIIOutput.R script
  #
  # visible output is the name of the exam 
  # for which the NCCA ASCII files are written
  #
  # NCCA ASCII output is to the CWD
  #
  ####
  
  {
    # assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
    # assign("thisChartEventsDF", thisChartEventsDF, envir=.GlobalEnv)

    # stop("writing NCCA ASCII")
  }
  
  {
    # show the NCCAASCII text file name on the console
    print(paste("write the NCCA ASCII file:", thisOutputName))
    print(paste("exam:", thisChartDF$examName[1]))
  }
  
  #### check and fix the answers ####
  
  {
    
    # set the X and XX answers to ""
    thisChartEventsDF$Answer[thisChartEventsDF$Label=="X" |
                               thisChartEventsDF$Label=="XX"] <- ""
    # View(thisChartEventsDF)
    
    # check for answers that are equal to the End
    fixThese <- which(thisChartEventsDF$Answer == thisChartEventsDF$End)
    fixChartDFRows <- as.numeric(thisChartEventsDF$Answer[fixThese])
    # set the Label to the preceding row before fixing the Label
    thisChartDF$Label[fixChartDFRows] <- thisChartDF$Label[(fixChartDFRows - 1)]
    thisChartDF$Label[(fixChartDFRows + 1)] <- "Ans"
    thisChartEventsDF$Answer[fixThese] <- (fixChartDFRows + 1)
    
    # check and fix missing answers
    # fixThese <- 
    #   which(thisChartEventsDF$Answer[2:(nrow(thisChartEventsDF)-1)] == "")
    # if(length(fixThese) > 0) {
    #   spoofAns <- sample(c(20:37), size=length(fixThese))
    #   theseChartDFRows <- as.numeric(thisChartEventsDF$End[fixThese]) + spoofAns
    #   thisChartEventsDF$Answer[fixThese] <- theseChartDFRows
    #   thisChartDF$Label[theseChartDFRows] <- "Ans"
    # }
    
    # check the label for question 10 - some people use "O" instead of 0
    fixThese <- which(thisChartEventsDF$Label %in% c("1OR", "R1O"))
    theseRowsStart <- thisChartEventsDF$Begin[fixThese]
    theseRowsEnd <- thisChartEventsDF$End[fixThese] 
    o=1
    if( !is.na(fixThese[1] == "") ) {
      for(o in 1:length(fixThese)) {
        if(fixThese[o] == "") next()
        correctLabel <- ifelse(thisChartEventsDF$Label[fixThese[o]] == "1OR", 
                               "10R", 
                               "R10" )
        thisChartEventsDF$Label[fixThese[o]] <- correctLabel
        thisChartDF$Label[theseRowsStart[o]:theseRowsEnd[o]] <- correctLabel
      }
    }
    
    # check the answers for RQ, CQ and other questions 
    RQRows <- grep("R", thisChartEventsDF$Label)
    CQRows <- grep("C", thisChartEventsDF$Label)
    RQCQRows <- sort(c(RQRows, CQRows))
    
    # end of the stimulus questions in the time series data
    RQCQChartDFOffsetRows <- thisChartEventsDF$End[RQCQRows]
    RQCQChartDFOffsetRows <- as.numeric(RQCQChartDFOffsetRows)
    
    # answer rows in the chart time series data frame
    RQCQChartDFAnswerRows <- thisChartEventsDF$Answer[RQCQRows]
    RQCQChartDFAnswerRows <- as.numeric(RQCQChartDFAnswerRows)
    
    # fix answers that are not NO
    fixThese <- which(thisChartDF$Label[RQCQChartDFAnswerRows] != "NO")
    thisChartDF$Label[RQCQChartDFAnswerRows[fixThese]] <- "NO"
    
    # check the answer distance
    answerDistance <- RQCQChartDFAnswerRows - RQCQChartDFOffsetRows
    fixThese <- which(answerDistance < 15 | answerDistance > 60)

    # # check for zero answer distance and correct the events data frame
    # fixTheseA <- 
    #   which(RQCQChartDFAnswerRows[fixThese] == 
    #           RQCQChartDFOffsetRows[fixThese])
    # thisChartEventsDF$End[RQCQRows][fixThese][fixTheseA] <- 
    #   thisChartEventsDF$End[RQCQRows][fixThese][fixTheseA] - 1
    
    # clear the answer in the chart data frame and events data frame
    thisChartDF$Label[RQCQChartDFAnswerRows[fixThese]] <- ""
    thisChartEventsDF$Answer[RQCQRows[fixThese]] <- ""
    
    # sample a new distance from .67 to 1.67 seconds.
    answerDistance[fixThese] <- 
      sample(c(20:50), size=length(fixThese), replace=TRUE)
    newAnswerRows <- 
      RQCQChartDFOffsetRows[fixThese] + answerDistance[fixThese]
    
    # set the new answer to NO
    thisChartDF$Label[newAnswerRows] <- "NO"
    thisChartEventsDF$Answer[RQCQRows[fixThese]] <- newAnswerRows
    
    # View(thisChartEventsDF)
    # View(thisChartDF)
    # check other questions 
    
    # questions with YES answer
    theseQuestions <- c("1", "1A", "1B", "2", "3", "3E", "E3")
    checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)

    # set the answer for YES answer questions
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    thisChartDF$Label[theseRows] <- "YES"
    
    # get the answer and end rows
    answerRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    endRows <- as.numeric(thisChartEventsDF$End[checkThese])

    # check the time distance to answer
    answerDistance <- answerRows - endRows
    fixThese <- which(answerDistance < 20 | answerDistance > 50)
    
    # check and for zero distance and correct the events data frame
    # fixTheseA <- 
    #   fixThese[which(answerRows[fixThese] == endRows[fixThese])]
    # thisChartEventsDF$End[checkThese][fixTheseA] <- 
    #   thisChartEventsDF$End[checkThese][fixTheseA] - 1
    
    thisChartDF$Label[answerRows[fixThese]] <- ""
    thisChartEventsDF$Answer[checkThese[fixThese]] <- ""
    
    # sample a new distance from .67 to 1.67 seconds.
    answerDistance[fixThese] <- 
      sample(c(20:50), size=length(fixThese), replace=TRUE)
    newAnswerRows <- 
      endRows[fixThese] + answerDistance[fixThese]
    
    # set the new answer
    thisChartDF$Label[newAnswerRows] <- "YES"
    thisChartEventsDF$Answer[checkThese[fixThese]] <- newAnswerRows
    
    #### question 8 has a NO answer
    
    theseQuestions <- c("8", "8E", "E8")
    checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)
    
    # set the answer for these question  
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    thisChartDF$Label[theseRows] <- "NO"
    # thisChartDF$Label[checkThese] <- "NO"
    
    # get the answer and end rows
    answerRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    endRows <- as.numeric(thisChartEventsDF$End[checkThese])
    
    # check the time distance to answer
    answerDistance <- answerRows - endRows
    fixThese <- which(answerDistance < 20 | answerDistance > 50)
    
    # check for zero distance and correct the events data frame
    # fixTheseA <- 
    #   fixThese[which(answerRows[fixThese] == endRows[fixThese])]
    # thisChartEventsDF$End[checkThese][fixTheseA] <- 
    #   thisChartEventsDF$End[checkThese][fixTheseA] - 1
    
    thisChartDF$Label[answerRows[fixThese]] <- ""
    thisChartEventsDF$Answer[checkThese[fixThese]] <- ""
    
    answerDistance[fixThese] <- 
      sample(c(20:50), size=length(fixThese), replace=TRUE)
    newAnswerRows <- 
      endRows[fixThese] + answerDistance[fixThese]
    
    # set the new answer
    thisChartDF$Label[newAnswerRows] <- "NO"
    thisChartEventsDF$Answer[checkThese[fixThese]] <- newAnswerRows
    
    # check that announcements, annotations and instructions have no answer
    theseQuestions <- c("1", "1A", "1B", "2", "3", "3E", "E3", "8", "8E", "E8")
    theseQuestions <- c(theseQuestions, c("C4", "4C", "C6", "6C", "C9", "9C"))
    theseQuestions <- c(theseQuestions, c("R5", "5R", "7R", "R7", "10R", "R10"))
    
    checkThese <- which(!(thisChartEventsDF$Label %in% theseQuestions))
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    
    thisChartDF$Label[theseRows] <- ""
    thisChartEventsDF$Answer[checkThese] <- ""
    
    # end check answers
    
  }
  
  #### exam details ####
  
  {
    
    examName <- unique(thisChartDF$examName)[1]
    
    # seriesName <- unique(thisChartDF$seriesName)[1]
    seriesName <- "2"
    
    chartName <- unique(thisChartDF$chartName)[1]
    
  }
  
  #### check and fix the RQ AND CQ rotation ####
  
  {
    
    CQRotation <- switch(chartName,
                         "01A"=c("C4", "C6", "C9"),
                         "02A"=c("C6", "C9", "C4"),
                         "03A"=c("C9", "C4", "C6"),
                         "Otherwise: last" )
    RQRotation <- c("R5", "R7", "R10")
    
    # RQ and RQ rows in the events data frame
    RQRows <- grep("R", thisChartEventsDF$Label)
    CQRows <- grep("C", thisChartEventsDF$Label)
    
    thisChartEventsDF$Label[RQRows] <- RQRotation
    thisChartEventsDF$Label[CQRows] <- CQRotation

    RQRowsStart <- thisChartEventsDF$Begin[RQRows]
    RQRowsEnd <- thisChartEventsDF$End[RQRows] 
    o=1
    for(o in 1:length(RQRowsStart)) {
      thisChartDF$Label[RQRowsStart[o]:RQRowsEnd[o]] <- RQRotation[o]
    }
    
    CQRowsStart <- thisChartEventsDF$Begin[CQRows]
    CQRowsEnd <- thisChartEventsDF$End[CQRows]
    o=1
    for(o in 1:length(CQRowsStart)) {
      thisChartDF$Label[CQRowsStart[o]:CQRowsEnd[o]] <- CQRotation[o]
    }
    
  }
  
  #### initialize the name of the NCCA ASCII output file ####
  
  NCCAASCIIName <- paste0("D&-", thisOutputName)
  
  #### initialize the header info ####
  
  headerInfo <- c( paste0("Name of this file: ", NCCAASCIIName),
                   paste0("Source file: ", "NA"),
                   paste0("Instrument: ", "Lafayette Windows"),
                   paste0("Software Version: ", "Software Version Not Available"),
                   paste0("Chart Date: ", "01Jan20"),
                   paste0("Time: ", "00:01"),
                   paste0("Examination Number: ", seriesName),
                   # paste0("Examination Number: 2"),
                   paste0("Chart Number: ", chartName),
                   paste0("Number of questions: ", nrow(thisChartEventsDF)),
                   paste0("Fastest Sample Rate (Hz): ", "30"),
                   paste0("Number of samples: ", nrow(thisChartDF)),
                   paste0("Number of channels: ", length(outputSensors)),
                   paste0("Sample Rate (Hz)- UPneumo: ", "30"),
                   paste0("Sample Rate (Hz)- LPneumo: ", "30"),
                   paste0("Sample Rate (Hz)- EDA1: ", "30"),
                   paste0("Sample Rate (Hz)- Cardio1: ", "30"),
                   paste0("Sample Rate (Hz)- Move1: ", "30"),
           #        paste0("Sample Rate (Hz)- PPG11: ",                   ""
     ""
  )
  
  #### initialize a vector for the list of stimulus events ####
  
  {
    
    stimText <- rep("", times=(nrow(thisChartEventsDF)+1))
    
    stimText[1] <- "Event    Label Statement"
    n=1
    for(n in 1:(length(stimText)-1)) {
      stimText[(n+1)] <- paste( str_pad(str_pad(thisChartEventsDF$Event[n], 2, side="left", pad="0"), 5, side="right"),
                                str_pad(thisChartEventsDF$Label[n], 8, side="left"),
                                str_pad(thisChartEventsDF$Label[n], 9, side="right") 
      )
    }
    
  }
  
  #### initialize a vector for the event locations ####
  
  {
    
    # check and fix any answers are equal to the end
    # fixThese <- 
    #   thisChartEventsDF$Answer[thisChartEventsDF$Answer == thisChartEventsDF$End]
    # 
    # thisChartEventsDF$Answer[thisChartEventsDF$Answer == thisChartEventsDF$End] <- 
    #   as.numeric(fixThese) + 1  
    
    eventInfo <- rep("", times=(nrow(thisChartEventsDF)+1))
    
    eventInfo[1] <- "Event    Label      Begin        End     Answer"
    
    n=1
    for(n in 1:(length(eventInfo)-1)) {
      eventInfo[(n+1)] <- paste( str_pad(str_pad(thisChartEventsDF$Event[n], 2, side="left", pad="0"), 5, side="right"),
                                 str_pad(thisChartEventsDF$Label[n], 8, side="left"),
                                 str_pad(thisChartEventsDF$Begin[n], 10, side="left"),
                                 str_pad(thisChartEventsDF$End[n], 10, side="left"),
                                 str_pad(thisChartEventsDF$Answer[n], 10, side="left") 
      )
    }
    
  }
  
  #### initialize a vector for the time series data ####
  
  {
    
    # first round the time series data
    # thisChartDF$UPneumo <- round(thisChartDF$UPneumo, 0)
    # thisChartDF$LPneumo <- round(thisChartDF$LPneumo, 0)
    # thisChartDF$EDA1 <- round(thisChartDF$EDA1, 0)
    # thisChartDF$Cardio1 <- round(thisChartDF$Cardio1, 0)
    # thisChartDF$Move1 <- round(thisChartDF$Move1, 0)
    # thisChartDF$PPG11<- round(thisChartDF$PPG1, 0)
    
    # initialize a vector for the data
    examDAT <- rep("", times=(nrow(thisChartDF)+1))
    
    if(length(outputSensors)==6) {
      examDAT[1] <- "Sample     Time    Label    UPneumo    LPneumo       EDA1    Cardio1      Move1       PPG1"
    } else {
      examDAT[1] <- "Sample     Time    Label    UPneumo    LPneumo       EDA1    Cardio1      Move1"
    }
    
    n=1
    for(n in 1:(length(examDAT)-1)) {
      examDAT[(n+1)] <- paste( str_pad(n, 6, side="left"),
                               # str_pad(thisChartDF$Sample[n], 6, side="left"),
                               str_pad(str_sub(thisChartDF$Time[n], 1, 8), 8, side="left"),
                               str_pad(thisChartDF$Label[n], 8, side="left", pad="-"),
                               paste0(str_pad(as.integer(thisChartDF$UPneumo[n]), 8, side="left"), ".0"),
                               paste0(str_pad(as.integer(thisChartDF$LPneumo[n]), 8, side="left"), ".0"),
                               paste0(str_pad(as.integer(thisChartDF$EDA1[n]), 8, side="left"), ".0"),
                               paste0(str_pad(as.integer(thisChartDF$Cardio1[n]), 8, side="left"), ".0"),
                               paste0(str_pad(as.integer(thisChartDF$Move1[n]), 8, side="left"), ".0")
                               # paste0(str_pad(as.integer(thisChartDF$PPG11[n]), 8, side="left"), ".0")
      )
    }
    
    # head(examDAT, 100)
    
  }
  
  #### write the NCCA ASCII output in the Lafayette format ####
  
  {
    
    folderName <- paste0("FZCT_", str_sub(thisOutputName, -9, -7))
    
    writePFPathName <- paste0(folderName,
                              "/",
                              NCCAASCIIName )
    
    if(!dir.exists(folderName)) dir.create(folderName)
    
    writeLines(text=c(headerInfo, stimText, " ", eventInfo, " ", examDAT), 
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
  
  # end writeNCCAASCIIFn()
} 




