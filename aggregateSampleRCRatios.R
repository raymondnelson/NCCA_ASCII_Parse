# R Script to aggregate the skiny _Measurements.csv" data to a single .csv for all cases in a sample
# January 19, 2025
# Raymond Nelson
####



# rm(list=ls())



# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")

# setwd("~/Dropbox/R/NCCAASCII_data/Ohio_PPG_data/Ohio2015_n40_nccascii_2024Mar")



# library("readr")

# library("stringi")

# library("stringr")



#### get the _Measurements.csv file names ####



measurementFileNames <- list.files(path = ".",
                                   pattern = "._Measurements.csv",
                                   all.files = FALSE,
                                   full.names = FALSE,
                                   recursive = FALSE,
                                   ignore.case = FALSE,
                                   include.dirs = FALSE,
                                   no.. = FALSE)

# N <- length(measurementFileNames)



#### initialize the sequence of question labels for for the examination format ####



{
  # federal ZCT
  theseQuestions <- c("1", "SA", "E3", "C4", "R5", "C6", "R7", "E8", "C9", "R10")
  
  # utah 3 question format
  theseQuestions <- c("I1", "SA", "N1", "C1", "R1", "N2", "C2", "R2", "N3", "C3", "R3")
  
  # utah 3 question format
  theseQuestionsUT3 <- c("I1", "SA", "N1", "C1", "R1", "N2", "C2", "R2", "N3", "C3", "R3")
  
  # federal You-PHase
  theseQuestionsBZC2 <- c("1", "SA", "E3", "C4", "R5", "C6", "R7", "C8", "E9")
  
  # federal ZCT
  theseQuestionsFZC3 <- c("1", "SA", "E3", "C4", "R5", "C6", "R7", "E8", "C9", "R10")
  
  # AFMGQTV1-4RQ
  theseQuestionsAF14 <- c("1", "SA", "C3", "R4", "C5", "R6", "C7", "R8", "C9", "R10")
  
  # AFMGQTV1-3RQ
  theseQuestionsAF13 <- c("1", "SA", "C3", "R4", "C5", "R6", "C7", "R8", "C9")
  
  # AFMGQTV1-2RQ
  theseQuestionsAF12 <- c("1", "SA", "C3", "R4", "C5", "R6", "C7")
  
  # AFMGQTV2-4RQ
  theseQuestionsAF24 <- c("1", "SA", "C3", "R4", "R5", "C6", "R7", "R8", "C9")
  
  # AFMGQTV2-3RQ
  theseQuestionsAF23 <- c("1", "SA", "C3", "R4", "R5", "C6", "R7", "C8")
  
  # AFMGQTV2-2RQ
  theseQuestionsAF22 <- c("1", "SA", "C3", "R4", "R5", "C6")
  
  # ARMY MGQT-3RQ
  theseQuestionsARM3 <- c("1", "3", "4", "R5", "C6", "7", "C7", "R8", "R9", "C10", "C11")
  
  # ARMY MGQT-4RQ
  theseQuestionsARM4 <- c("1", "R3", "4", "R5", "C6", "7", "C7", "R8", "R9", "C10", "C11")
  
}



#### initialize an output data frame for the aggregation ####



{
  
  numberEvents <- length(theseQuestionsFZC3)
  
  sensorNames <- c("TR", "AR", "E", "C", "F")
  sensorNames0 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE")
  numberSensors <- length(sensorNames)
  
  questionCols <- paste0(rep(paste0("m", rep(c(1:numberSensors), each=numberEvents), "_", rep(1:numberEvents), "_"), each=numberSensors), sensorNames)
  columnNames <- c("examName", "technique", "criterionState", paste0("Q", c(1:numberEvents)), questionCols)
  
  aggRCRatiosDF <- as.data.frame(matrix(nrow=length(measurementFileNames), ncol=length(columnNames)))
  names(aggRCRatiosDF) <- columnNames
  
  # also initialize a data frame for ESSM Scores
  aggESSMScoresDF <- aggRCRatiosDF
  
  # outputDF <- aggRCRatiosDF
  # 
  # # 11 output data frame for the event indices
  # preStimOutputDF <- outputDF
  # eventBeginOutputDF <- outputDF
  # latencyOutputDF <- outputDF
  # eventEndOutputDF <- outputDF
  # answerOutputDF <- outputDF
  # responseOnsetOutputDF <- outputDF
  # rowEndOutputDF <- outputDF
  # responseEndOutputDF <- outputDF
  # woeEndOutputDF <- outputDF
  # recRowOutputDF <- outputDF
  # postStimOutputDF <- outputDF

  # rm(outputDF)
}



#### initialize an output row ####



{
  outputRow <- rep(NA, times = length(columnNames))
  names(outputRow) <- columnNames
  
  outputRow["technique"] <- "1"
  outputRow["criterionState"] <- ""
  
  ## map the event labels to the output row ##
  
  # for(i in 1:length(theseQuestions)) {
  #   colmnName <- paste0("Q", i)
  #   outputRow[colmnName] <- theseQuestions[i]
  # }
  
}



#### import the criterion state ####



{
  criterionStateDF <- read_csv(file="criterionState.csv")
  
  criterionStateDF$examName <- criterionStateDF$shortName
}



#### iterate over the _Measurements.csv files and aggregate the data values ####



i=1
for(i in 1:length(measurementFileNames)) {
  
  {
    thisExamMeasurementsDF <- read_csv(measurementFileNames[i])
    
    thisExamName <- str_sub(thisExamMeasurementsDF[1,1], 2, -1)
    
    uniqueSeries <- unique(thisExamMeasurementsDF$seriesName)
    
    # thisTestFormat <- str_sub(thisExamName, -4, -1)
    thisTestFormat <- "FZC3"
  }
  
  ## limit the data frame to one series ##
  
  thisExamMeasurementsDF2 <- thisExamMeasurementsDF[thisExamMeasurementsDF$seriesName==uniqueSeries[1],]
  
  ## initialize an output vector (row) for this exam ##
  
  {
    # output row for logRC Ratios
    outputRow0 <- outputRow
    outputRow0['examName'] <- thisExamName
    # outputRow0["technique"] <- "1"
    outputRow0["criterionState"] <- ""
    
    # output row for ESSM scores
    outputRow1 <- outputRow0
  }
  
  ## map the question sequence to the outputRow0 ##
  
  {
    
    theseQuestions <- switch(thisTestFormat,
                             BZC2=theseQuestionsBZC2,
                             FZC3=theseQuestionsFZC3,
                             AF14=theseQuestionsAF14,
                             AF13=theseQuestionsAF13,
                             AF12=theseQuestionsAF12,
                             AF24=theseQuestionsAF24,
                             AF23=theseQuestionsAF23,
                             AF22=theseQuestionsAF22,
                             ARM3=theseQuestionsARM3,
                             ARM4=theseQuestionsARM4, 
                             UT3=theseQuestionsUT3 )
    
    for(m in 1:length(theseQuestions)) {
      colmnName <- paste0("Q", m)
      outputRow0[colmnName] <- theseQuestions[m]
      outputRow1[colmnName] <- theseQuestions[m]
    }
    
  }
  
  ## add the criterion state to the output row ##
  
  {
    outputRow0["criterionState"] <- 
      criterionStateDF$criterionState[which(str_sub(thisExamName, 1, -1) == criterionStateDF$examName)]
    outputRow1["criterionState"] <- 
      criterionStateDF$criterionState[which(str_sub(thisExamName, 1, -1) == criterionStateDF$examName)]
  }
  
  ## exclude the X and XX and limit the data frame to the selected questions ##
  
  {
    # do this after selecting theseQuestions
    
    thisExamMeasurementsDF3 <- thisExamMeasurementsDF2[(thisExamMeasurementsDF2$eventLabel %in% theseQuestions),]
  }
  
  ## iterate over the charts ##
  
  uniqueCharts <- unique(thisExamMeasurementsDF3$chartName)
  
  ## map the event labels to the question columns
  
  j=1
  for(j in 1:length(uniqueCharts)) {
    
    {
      thisChartName <- uniqueCharts[j]
      
      thisChartDF <- thisExamMeasurementsDF3[(thisExamMeasurementsDF3$chartName==thisChartName),]
      
      uniqueEvents <- unique(thisChartDF$eventLabel)
    }
    
    ## iterate over the events ##
    
    k=1
    for(k in 1:length(uniqueEvents)) {
      
      {
        thisEventLabel <- uniqueEvents[k]
        
        thisEventDF <- thisChartDF[(thisChartDF$eventLabel==thisEventLabel), ]
        
        uniqueSensors <- unique((thisEventDF$sensorName))
      }
      
      ## map the event label to the correct column ##
      
      {
        # this ensure that questions are correctly aligned when there is a departure from the expected sequence
        
        thisEventNumber <- which(thisEventLabel == outputRow0[4:14])

        selectColumn <- paste0(paste0("m", j), "_", thisEventNumber, "_")
        # the sensor name will be concatenated with selectColumn later 
      }
      
      ## iterate over the sensors ## 
      
      l=1
      for(l in 1:length(sensorNames0)) {
        
        thisSensorName <- sensorNames0[l]
        
        if(!(thisSensorName %in% uniqueSensors)) next()
        
        thisRCScore <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'RCScore']
        
        thisESSMScore <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'ESSScore']
        
        ## assign the value to the outputRow vector
        
        # names(outputRow)
        
        # construct the column name for the event and sensor
        # selectColumn <- paste0(paste0("m", j), "_", k, "_")
        sensorCol <- switch(thisSensorName,
                            UPneumo="TR",
                            LPneumo="AR",
                            AutoEDA="E",
                            Cardio="C",
                            PLE="F" )
        selectColumn0 <- paste0(selectColumn, sensorCol)
        
        # insert the value into the output row for logRC ratios
        outputRow0[which(names(outputRow) == selectColumn0)] <- round(thisRCScore, 3)
        
        # insert the ESSM score into the output row for ESSM scores
        outputRow1[which(names(outputRow) == selectColumn0)] <- thisESSMScore
        
      } # end l loop over sensors 
      
    } # end k loop over events
    
  } # end iteration j over charts
  
  ## insert the outputRow into the aggRCRatiosDF
  
  aggRCRatiosDF[i,] <- outputRow0
  
  aggESSMScoresDF[i,] <- outputRow1
  
} # end i loop over _Measurements.csv file names



#### change 0 and NA values to missing values #####


for(i in (numberEvents+4):ncol(aggRCRatiosDF)) {
  aggRCRatiosDF[which(aggRCRatiosDF[,i] == 0),i] <- ""
  aggRCRatiosDF[is.na(aggRCRatiosDF[,i]),i] <- ""
  
  aggESSMScoresDF[is.na(aggESSMScoresDF)] <- ""
}




  
#### write the output data frame to a .csv text file ####


# initialize the name of the output csv file
# outputFileNameRCRatios <- paste0("aggRCRatios_N", N, "_latency05.csv")
# outputFileNameESSMScores <- paste0("aggESSMScores_N", N, "latency05.csv")

outputFileNameRCRatios <- get("outputFileNameRCRatios", envir=.GlobalEnv)
outputFileNameESSMScores <- get("outputFileNameESSMScores", envir=.GlobalEnv)

write_csv(aggRCRatiosDF, outputFileNameRCRatios)
write_csv(aggESSMScoresDF, outputFileNameESSMScores)



