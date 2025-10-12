# R Script to aggregate the semi-skiny _Measurements.csv" data to a single .csv for all cases in a sample
# January 19, 2025
# Raymond Nelson
####
#
# produce a .csv table similar to input requirements for the 2008 OSS-3 Excel prototype



rm(list=ls())



# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")

setwd("~/Dropbox/R/NCCAASCII_data/Ohio_PPG_data/Ohio2015_n40_nccascii_2024Mar")
setwd("~/Dropbox/DATASETS/Axciton_confirmed_casesN44/AxcitonN44_NCCAASCIIOutputLAF_2025Jan")


library("readr")

library("stringi")
library("stringr")

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

N <- length(measurementFileNames)



#### initialize the sequence of question labels for for the examination format ####



{
  # federal ZCT
  fzctQuestions <- c("N1", "SA", "E3", "C4", "R5", "C6", "R7", "E8", "C9", "R10")
  
  # utah 3 question format
  ut3Questions <- c("I1", "SA", "N1", "C1", "R1", "N2", "C2", "R2", "N3", "C3", "R3")
  
  # utah 4 question format
  ut4Questions <- c("I1", "SA", "N1", "C1", "R1", "R2", "C2", "R3", "R4", "C3", "N2")
  
  # USAMFGQT
  af1mgqtQuestions <- c("N1", "SA", "C3", "R4", "C5", "R6", "C7", "R8", "C9", "R10")
  
  af2mgqtQuestions <- c("N1", "SA", "C3", "R4", "R5", "C6", "R7", "R8", "C9")
                      
  # Army
  armyQuestions <- c("I1", "I2", "R3", "I4", "R5", "C6", "I7", "R8", "R9", "C10")
  
  # YouPhase
  theseQuestions <- 
    unique(c(fzctQuestions, ut3Questions, ut4Questions, af1mgqtQuestions, af2mgqtQuestions, armyQuestions))
  
  
}



#### initialize an output data frame for the aggregation ####



{
  
  numberEvents <- length(theseQuestions)
  
  sensorNames <- c("TR", "AR", "E", "C", "F")
  sensorNames0 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE")
  numberSensors <- length(sensorNames)
  
  questionCols <- paste0(rep(paste0("m", rep(c(1:numberSensors), each=numberEvents), "_", rep(1:numberEvents), "_"), each=numberSensors), sensorNames)
  columnNames <- c("examName", "technique", "criterionState", paste0("Q", c(1:numberEvents)), questionCols)
  
  # initialize a data frame
  aggMeasurementsDF <- as.data.frame(matrix(nrow=length(measurementFileNames), ncol=length(columnNames)))
  names(aggMeasurementsDF) <- columnNames
  
  outputDF <- aggMeasurementsDF
  
  # 11 output data frames for the event indices
  preStimOutputDF <- outputDF
  eventBeginOutputDF <- outputDF
  latencyOutputDF <- outputDF
  eventEndOutputDF <- outputDF
  answerOutputDF <- outputDF
  responseOnsetOutputDF <- outputDF
  rowEndOutputDF <- outputDF
  responseEndOutputDF <- outputDF
  woeEndOutputDF <- outputDF
  recRowOutputDF <- outputDF
  postStimOutputDF <- outputDF
  
  # rm(outputDF)
}



#### initialize an output row ####



{
  outputRow <- rep(NA, times = length(columnNames))
  names(outputRow) <- columnNames
  
  outputRow["technique"] <- "1"
  outputRow["criterionState"] <- ""
  
  ## map the event labels to the output row ##
  
  for(i in 1:length(theseQuestions)) {
    colmnName <- paste0("Q", i)
    outputRow[colmnName] <- theseQuestions[i]
  }
  
}



#### import the criterion state ####



{
  criterionStateDF <- read_csv(file="criterionState.csv")
}



#### iterate over the _Measurements.csv files and aggregate the measurements ####



i=1
for(i in 1:length(measurementFileNames)) {
  
  {
    thisExamMeasurementsDF <- read_csv(measurementFileNames[i])
    
    thisExamName <- str_sub(thisExamMeasurementsDF[1,1], 2, -1)
    
    uniqueSeries <- unique(thisExamMeasurementsDF$seriesName)
  }
  
  ## limit the data frame to one series ##
  
  # thisExamMeasurementsDF2 <- thisExamMeasurementsDF[thisExamMeasurementsDF$seriesName=="1",]
  thisExamMeasurementsDF2 <- thisExamMeasurementsDF[thisExamMeasurementsDF$seriesName=="X",]
  
  ## exclude the X and XX and limit the data frame to the selected questions ##
  
  thisExamMeasurementsDF3 <- thisExamMeasurementsDF2[(thisExamMeasurementsDF2$eventLabel %in% theseQuestions),]

  ## initialize an output vector (row) ##
  
  {
    outputRow0 <- outputRow
    outputRow0['examName'] <- thisExamName
    # outputRow0["technique"] <- "1"
    outputRow0["criterionState"] <- ""
  }
  
  ## add the criterion state to the output row ##
  
  {
    outputRow0["criterionState"] <- 
      criterionStateDF$criterionState[which(str_sub(thisExamName, 1, -1) == criterionStateDF$examName)]
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
      }
      
      ## map the event label to the correct column ##
      
      {
        # this ensure that questions are correctly aligned when there is a departure from the expected sequence
        
        thisEventNumber <- which(thisEventLabel == outputRow0[4:14])

        selectColumn <- paste0(paste0("m", j), "_", thisEventNumber, "_")
      }
      
      ## iterate over the sensors ## 
      
      l=1
      for(l in 1:length(sensorNames0)) {
        
        thisSensorName <- sensorNames0[l]
        
        thisMeasurementVal <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'sensorMeasurement']
        
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
        
        # insert the value into the output row
        outputRow0[which(names(outputRow) == selectColumn0)] <- round(thisMeasurementVal, 3)
        
      } # end l loop over sensors 
      
    } # end k loop over events
    
  } # end iteration j over charts
  
  ## insert the outputRow into the aggMeasurementsDF
  
  # str(unlist(outputRow0))
  
  aggMeasurementsDF[i,] <- outputRow0
  
} # end i loop over _Measurements.csv file names



#### change 0 and NA values to missing values #####


for(i in (numberEvents+4):ncol(aggMeasurementsDF)) {
  aggMeasurementsDF[which(aggMeasurementsDF[,i] == 0),i] <- ""
  aggMeasurementsDF[is.na(aggMeasurementsDF[,i]),i] <- ""
}




  
#### write the output data frame to a .csv text file ####


# initialize the name of the output csv file
outputFileName <- paste0("aggMeasurements_N", N, ".csv")

# write_csv(aggMeasurementsDF, outputFileName)



