# export the simulated chart data to NCCA ASCII format
# 14 Dec 2018
# Raymond Nelson
#
##########



# library(stringr)


# writeNCCAASCII <- TRUE


# examName <- "TES1"
# seriesName <- "1"
# chartName <- "01A"

writeNCCAASCIIFn <- function(thisChartDF=thisChartDF, 
                             thisChartEventsDF=thisChartEventsDF,
                             thisOutputName=thisOutputName,
                             outputSensors=outputSensors ) {
  #
  # R function to write the NCCA ASCII file for each chart
  #
  # called by the NCCAASCIIOutputFn() in the NCCAASCCIIOutput.R script
  #
  ####
  
  {
    
    # assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
    # assign("thisChartEventsDF", thisChartEventsDF, envir=.GlobalEnv)

    # stop("writing NCCA ASCII")
    
  }
  
  # show the NCCAASCII text file name on the console
  print(paste("write the NCCA ASCII file:", thisOutputName))
  print(paste("exam:", thisChartDF$examName[1]))
  
  # set the X and XX answers to ""
  
  thisChartEventsDF$Answer[thisChartEventsDF$Label=="X" |
                             thisChartEventsDF$Label=="XX"] <- ""
  # View(thisChartEventsDF)
  # initialize some names
  
  
  
  {
    
    # check the answers for RQ, CQ and other questions 
    
    # rows in the events data frame
    RQAnswerRows <- grep("R", thisChartEventsDF$Label)
    CQAnswerRows <- grep("C", thisChartEventsDF$Label)
    RQCQAnswerRows <- sort(c(RQAnswerRows, CQAnswerRows))
    
    # rows in the time series data frame
    RQCQChartDFAnswerRows <- 
      thisChartEventsDF$Answer[RQCQAnswerRows]
    RQCQChartDFAnswerRows <- as.numeric(RQCQChartDFAnswerRows)
    
    # which answers are not NO
    fixThese <- which(thisChartDF$Label[RQCQChartDFAnswerRows] != "NO")
    thisChartDF$Label[RQCQChartDFAnswerRows[fixThese]] <- "NO"
    
    # check the answer distance
    RQCQChartDFOffsetRows <- thisChartEventsDF$End[RQCQAnswerRows]
    RQCQChartDFOffsetRows <- as.numeric(RQCQChartDFOffsetRows)
    
    # bad answer timing
    answerDistance <- RQCQChartDFAnswerRows - RQCQChartDFOffsetRows
    fixThese <- which(answerDistance < 20 | answerDistance > 37)

    # check for zero distance and correct the events data frame
    fixTheseA <- 
      which(RQCQChartDFAnswerRows[fixThese] == 
              RQCQChartDFOffsetRows[fixThese])
    thisChartEventsDF$End[RQCQAnswerRows][fixThese][fixTheseA] <- 
      thisChartEventsDF$End[RQCQAnswerRows][fixThese][fixTheseA] - 1
    
    # clear the answer in the chart data frame and events data frame
    thisChartDF$Label[RQCQChartDFAnswerRows[fixThese]] <- ""
    thisChartEventsDF$Answer[RQCQAnswerRows[fixThese]] <- ""
    
    # sample a new distance from .5 to 1.3 seconds.
    answerDistance[fixThese] <- 
      sample(c(20:37), size=length(fixThese), replace=TRUE)
    newAnswerRows <- 
      RQCQChartDFOffsetRows[fixThese] + answerDistance[fixThese]
    
    # set the new answer to NO
    thisChartDF$Label[newAnswerRows] <- "NO"
    thisChartEventsDF$Answer[RQCQAnswerRows[fixThese]] <- newAnswerRows
    
    # View(thisChartEventsDF)
    # View(thisChartDF)
    # check other questions 
    
    ####
    
    theseQuestions <- c("1", "1A", "1B", "2", "3", "3E", "E3")
    checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)
    
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    thisChartDF$Label[theseRows] <- "YES"
    
    answerRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    endRows <- as.numeric(thisChartEventsDF$End[checkThese])
    
    answerDistance <- answerRows - endRows
    fixThese <- which(answerDistance < 20 | answerDistance > 37)
    
    # check for zero distance and correct the events data frame
    fixTheseA <- 
      fixThese[which(answerRows[fixThese] == endRows[fixThese])]
    thisChartEventsDF$End[checkThese][fixTheseA] <- 
      thisChartEventsDF$End[checkThese][fixTheseA] - 1
    
    thisChartDF$Label[answerRows[fixThese]] <- ""
    thisChartEventsDF$Answer[checkThese[fixThese]] <- ""
    
    answerDistance[fixThese] <- 
      sample(c(20:37), size=length(fixThese), replace=TRUE)
    newAnswerRows <- 
      endRows[fixThese] + answerDistance[fixThese]
    
    thisChartDF$Label[newAnswerRows] <- "YES"
    thisChartEventsDF$Answer[checkThese[fixThese]] <- newAnswerRows
    
    # question 8 has a NO answer
    
    theseQuestions <- c("8", "8E", "E8")
    checkThese <- which(thisChartEventsDF$Label %in% theseQuestions)
    
    theseRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    thisChartDF$Label[theseRows] <- "NO"
    # thisChartDF$Label[checkThese] <- "NO"
    
    answerRows <- as.numeric(thisChartEventsDF$Answer[checkThese])
    endRows <- as.numeric(thisChartEventsDF$End[checkThese])
    
    answerDistance <- answerRows - endRows
    fixThese <- which(answerDistance < 20 | answerDistance > 37)
    
    # check for zero distance and correct the events data frame
    fixTheseA <- 
      fixThese[which(answerRows[fixThese] == endRows[fixThese])]
    thisChartEventsDF$End[checkThese][fixTheseA] <- 
      thisChartEventsDF$End[checkThese][fixTheseA] - 1
    
    thisChartDF$Label[answerRows[fixThese]] <- ""
    thisChartEventsDF$Answer[checkThese[fixThese]] <- ""
    
    answerDistance[fixThese] <- 
      sample(c(20:37), size=length(fixThese), replace=TRUE)
    newAnswerRows <- 
      endRows[fixThese] + answerDistance[fixThese]
    
    thisChartDF$Label[newAnswerRows] <- "NO"
    thisChartEventsDF$Answer[checkThese[fixThese]] <- newAnswerRows
    
    ####
    
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
  
  # exam details
  
  {
    
    examName <- unique(thisChartDF$examName)[1]
    
    seriesName <- unique(thisChartDF$seriesName)[1]
    
    chartName <- unique(thisChartDF$chartName)[1]
    
  }
  
  # initialize the name of the NCCA ASCII output file
  
  NCCAASCIIName <- paste0("D&-", thisOutputName)
  
  #### initialize the header info
  
  headerInfo <- c( paste0("Name of this file: ", NCCAASCIIName),
                   paste0("Source file: ", "NA"),
                   paste0("Instrument: ", "Lafayette Windows"),
                   paste0("Software Version: ", "Software Version Not Available"),
                   paste0("Chart Date: ", "01Jan20"),
                   paste0("Time: ", "00:01"),
                   paste0("Examination Number: ", seriesName),
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
           #        paste0("Sample Rate (Hz)- PLE1: ",                   ""
     ""
  )
  
  #### initialize a vector for the list of stimulus events 
  
  stimText <- rep("", times=(nrow(thisChartEventsDF)+1))
  
  stimText[1] <- "Event    Label Statement"
  n=1
  for(n in 1:(length(stimText)-1)) {
    stimText[(n+1)] <- paste( str_pad(str_pad(thisChartEventsDF$Event[n], 2, side="left", pad="0"), 5, side="right"),
                              str_pad(thisChartEventsDF$Label[n], 8, side="left"),
                              str_pad(thisChartEventsDF$Label[n], 9, side="right") 
    )
  }
  
  #### initialize a vector for the event locations
  
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
  
  #### initialize a vector for the time series data
  
  # first round the time series data
  # thisChartDF$UPneumo <- round(thisChartDF$UPneumo, 0)
  # thisChartDF$LPneumo <- round(thisChartDF$LPneumo, 0)
  # thisChartDF$EDA1 <- round(thisChartDF$EDA1, 0)
  # thisChartDF$Cardio1 <- round(thisChartDF$Cardio1, 0)
  # thisChartDF$Move1 <- round(thisChartDF$Move1, 0)
  # thisChartDF$PLE1<- round(thisChartDF$PLE1, 0)
  
  # initialize a vector for the data
  examDAT <- rep("", times=(nrow(thisChartDF)+1))
 
  if(length(outputSensors)==6) {
    examDAT[1] <- "Sample     Time    Label    UPneumo    LPneumo       EDA1    Cardio1      Move1       PLE1"
  } else {
    examDAT[1] <- "Sample     Time    Label    UPneumo    LPneumo       EDA1    Cardio1      Move1"
  }
  
  n=1
  for(n in 1:(length(examDAT)-1)) {
    examDAT[(n+1)] <- paste( str_pad(n, 6, side="left"),
                             # str_pad(thisChartDF$Sample[n], 6, side="left"),
                             str_pad(str_sub(thisChartDF$Time[n], 1, 8), 8, side="left"),
                             str_pad(thisChartDF$Label[n], 8, side="left", pad="-"),
                             paste0(str_pad(thisChartDF$UPneumo[n], 8, side="left"), ".0"),
                             paste0(str_pad(thisChartDF$LPneumo[n], 8, side="left"), ".0"),
                             paste0(str_pad(thisChartDF$EDA1[n], 8, side="left"), ".0"),
                             paste0(str_pad(thisChartDF$Cardio1[n], 8, side="left"), ".0"),
                             paste0(str_pad(thisChartDF$Move1[n], 8, side="left"), ".0")
                             # paste0(str_pad(thisChartDF$PLE1[n], 8, side="left"), ".0")
    )
  }
  
  # head(examDAT, 100)
  
  #### write the 
  
  writeLines(text=c(headerInfo, stimText, " ", eventInfo, " ", examDAT), 
             con=NCCAASCIIName)
  
  # output the name of the NCCA ASCII file
  
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




