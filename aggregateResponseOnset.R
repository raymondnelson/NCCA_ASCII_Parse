# R script to aggregate the repsonse onset and response end
# January 19, 2025
# Raymond Nelson
####




rm(list=ls())



# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")

# setwd("~/Dropbox/R/NCCAASCII_data/Ohio_PPG_data/Ohio2015_n40_nccascii_2024Mar")

# setwd("~/Dropbox/R/NCCAASCII_data/AxcitonN44_NCCAASCIIOutputLAF")

setwd("~/Dropbox/DATASETS_BACKUP/OSSN60Holdout_ZCT60/OSSN60_NCCAASCII/NCCAASCIIOutputLAF/NCCAASCIIOutputLAF")


library("readr")

library("stringi")

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



#### initialize the sequence of question labels for for the examination formats ####


{
  # federal ZCT
  theseQuestionsFZCT <- c("1", "SA", "E3", "C4", "R5", "C6", "R7", "E8", "C9", "R10")
  
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
  
  numberEvents <- length(theseQuestions)
  
  # numberEvents <- 11
  
  # sensor names to construct the column names in the output data frame
  sensorNames <- c("TR", "AR", "E", "C", "F")
  # actual sensor names
  sensorNames0 <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE")
  numberSensors <- length(sensorNames)
  
  questionCols <- paste0(rep(paste0("m", rep(c(1:numberSensors), each=numberEvents), "_", rep(1:numberEvents), "_"), each=numberSensors), sensorNames)
  
  # column names for the output data frame
  columnNames <- c("examName", "technique", "criterionState", paste0("Q", c(1:numberEvents)), questionCols)
  
  ## initialize a data frame
  aggMeasurementsDF <- as.data.frame(matrix(nrow=length(measurementFileNames), ncol=length(columnNames)))
  names(aggMeasurementsDF) <- columnNames
  
  outputDF <- aggMeasurementsDF
  # View(outputDF)
  
  {
    # 11 output data frame for the event indices
    preStimIdxDF <- outputDF
    eventBeginIdxDF <- outputDF
    latencyIdxDF <- outputDF
    eventEndIdxDF <- outputDF
    answerIdxDF <- outputDF
    responseOnsetIdxDF <- outputDF
    rowEndIdxDF <- outputDF
    responseEndIdxDF <- outputDF
    woeEndIdxDF <- outputDF
    recRowIdxDF <- outputDF
    postStimIdxDF <- outputDF
    
    # 11 more output data frames for the event Values
    preStimYvalsDF <- outputDF
    eventBeginYvalsDF <- outputDF
    latencyYvalsDF <- outputDF
    eventEndYvalsDF <- outputDF
    answerYvalsDF <- outputDF
    responseOnsetYvalsDF <- outputDF
    rowEndYvalsDF <- outputDF
    responseEndYvalsDF <- outputDF
    woeEndYvalsDF <- outputDF
    recRowYvalsDF <- outputDF
    postStimYvalsDF <- outputDF
  }
  
  # rm(outputDF)
}



#### initialize an output row ####



{
  outputRow <- rep(NA, times = length(columnNames))
  names(outputRow) <- columnNames
  
  outputRow["technique"] <- "1"
  outputRow["criterionState"] <- ""
}



#### import the criterion state ####



{
  criterionStateDF <- read_csv(file="criterionState.csv")
  
  # short names are the Axciton names without punctuation characters
  criterionStateDF$examName <- criterionStateDF$shortName
  
  # View(criterionStateDF)
}



#### iterate over the _Measurements.csv files and aggregate the measurements ####



i=1
for(i in 1:length(measurementFileNames)) {
  
  {
    thisExamMeasurementsDF <- read_csv(measurementFileNames[i])
    # View(thisExamMeasurementsDF)
    
    # include or exclude the last character
    # depending on whether the character contains the criterion state
    thisExamName <- str_sub(thisExamMeasurementsDF[1,1], 2, -1)
    
    uniqueSeries <- unique(thisExamMeasurementsDF$seriesName)
    
    ## use this for the Axciton n=44 mixed format sample  
    # thisTestFormat <- str_sub(thisExamName, -4, -1)
    
    ## for other formats
    thisTestFormat <- "FZCT"
  }
  
  ## select the question sequence ##
  
  {
    
    # ## for the Axciton n=44 mixed format sample
    # theseQuestions <- switch(thisTestFormat,
    #                          BZC2=theseQuestionsBZC2,
    #                          FZC3=theseQuestionsFZC3,
    #                          AF14=theseQuestionsAF14,
    #                          AF13=theseQuestionsAF13,
    #                          AF12=theseQuestionsAF12,
    #                          AF24=theseQuestionsAF24,
    #                          AF23=theseQuestionsAF23,
    #                          AF22=theseQuestionsAF22,
    #                          ARM3=theseQuestionsARM3,
    #                          ARM4=theseQuestionsARM4, 
    #                          UT3=theseQuestionsUT3 )
    
    # for the Ohio cases
    # theseQuestions <- theseQuestionsUT3
    
    ##
    
    theseQuestions <- theseQuestionsFZCT
    
  }
  
  ## map the event labels to the output row ##
  
  {
    # reset the question names
    outputRow[c(4:14)] <- NA
    # then map the names for this test format
    for(j in 1:length(theseQuestions)) {
      colmnName <- paste0("Q", j)
      outputRow[colmnName] <- theseQuestions[j]
    }
  }
  
  ## add the criterion state to the output row ##
  
  {
    outputRow['examName'] <- thisExamName
    
    outputRow["criterionState"] <- 
      criterionStateDF$criterionState[which(str_sub(thisExamName, 1, -1) == criterionStateDF$examName)]
  }
  
  ## initialize an output vector (row) for each output data frame ##
  
  {
    
    # output rows for the sample indices 
    preStimIdxRow <- outputRow
    beginIdxRow <- outputRow
    latencyIdxRow <- outputRow
    endIdxRow <- outputRow
    answerIdxRow <- outputRow
    responseOnsetIdxRow <- outputRow
    rowEndIdxRow <- outputRow
    responseEndIdxRow <- outputRow
    woeEndIdxRow <- outputRow
    recRowIdxRow <- outputRow
    postStimIdxRow <- outputRow
    
    # output rows for sample data values
    preStimYvalsRow <- outputRow
    beginYvalsRow <- outputRow
    latencyYvalsRow <- outputRow
    endYvalsRow <- outputRow
    answerYvalsRow <- outputRow
    responseOnsetYvalsRow <- outputRow
    rowEndYvalsRow <- outputRow
    responseEndYvalsRow <- outputRow
    woeEndYvalsRow <- outputRow
    recRowYvalsRow <- outputRow
    postStimYvalsRow <- outputRow
    
  }
  
  ## limit the data frame to one series ##
  
  {
    
    # View(thisExamMeasurementsDF)
    thisExamMeasurementsDF2 <- thisExamMeasurementsDF
    # thisExamMeasurementsDF2 <- thisExamMeasurementsDF[thisExamMeasurementsDF$seriesName==uniqueSeries[1],]
    # View(thisExamMeasurementsDF2)
    
    ## limit the data frame to the selected questions by excluding the X, XX and annotations ##
    
    thisExamMeasurementsDF3 <- thisExamMeasurementsDF2[(thisExamMeasurementsDF2$eventLabel %in% theseQuestions),]
    # View(thisExamMeasurementsDF3)
  }
  
  ## iterate over the charts ##
  
  uniqueCharts <- unique(thisExamMeasurementsDF3$chartName)
  
  ## map the event labels to the question columns
  
  j=1
  for(j in 1:length(uniqueCharts)) {
    
    {
      thisChartName <- uniqueCharts[j]
      
      # View(thisExamMeasurementsDF3)
      thisChartDF <- thisExamMeasurementsDF3[(thisExamMeasurementsDF3$chartName==thisChartName),]
      # View(thisChartDF)
      
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
        # theseQuestions
        
        # this ensure that questions are correctly aligned when there is a departure from the expected sequence
        
        thisEventNumber <- which(thisEventLabel == outputRow[4:14])
        
        selectColumn <- paste0(paste0("m", j), "_", thisEventNumber, "_")
      }
      
      ## iterate over the sensors ## 
      
      l=1
      for(l in 1:length(sensorNames)) {
        
        ## construct the column name for the event and sensor ##
        
        {
          thisSensorName <- sensorNames0[l]
          
          # selectColumn <- paste0(paste0("m", j), "_", k, "_")
          sensorCol <- switch(thisSensorName,
                              UPneumo="TR",
                              LPneumo="AR",
                              AutoEDA="E",
                              Cardio="C",
                              PLE="F" )
          
          selectColumn0 <- paste0(selectColumn, sensorCol)
          
          # get the column index for this chart, question, and sensor
          thisColumn <- which(names(beginIdxRow) == selectColumn0)
        }
        
        ## get the response indices ##
        
        {
          # View(thisEventDF)
          
          # names(thisEventDF) 
          # View(thisEventDF)
          
          # [6] "Begin"              "End"                "Answer"  
          # "preStimX"           "latencyEndX"        "responseOnsetX"     "responseEndX"      
          # [26] "ROWEndX"            "WOEEndX"            "responseRecX"       "postStimX"          
          
          thisPreStimIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'preStimX']
          thisEventBeginIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'Begin']
          
          thisLatencyIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'latencyEndX']
          
          thisEventEndIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'End']
          thisAnswerIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'Answer']
          thisResponseOnsetIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'responseOnsetX']
          
          thisRowEndIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'ROWEndX']
          
          thisResponseEndIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'responseEndX']
          
          thisWoeEndIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'WOEEndX']
          
          # recovery row and poststim row 
          thisRecRowIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'responseRecX']
          thisPostStimIdx <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'postStimX']
        }
        
        ## assign the sample indices to the output vectors ##
        
        {
          preStimIdxRow[thisColumn] <- thisPreStimIdx
          beginIdxRow[thisColumn] <- thisEventBeginIdx
          latencyIdxRow[thisColumn] <- thisLatencyIdx
          endIdxRow[thisColumn] <- thisEventEndIdx
          answerIdxRow[thisColumn] <- thisAnswerIdx
          responseOnsetIdxRow[thisColumn] <- thisResponseOnsetIdx
          rowEndIdxRow[thisColumn] <- thisRowEndIdx
          responseEndIdxRow[thisColumn] <- thisResponseEndIdx
          woeEndIdxRow[thisColumn] <- thisWoeEndIdx
          recRowIdxRow[thisColumn] <- thisRecRowIdx
          postStimIdxRow[thisColumn] <- thisPostStimIdx
        }
        
        ## get the Y-axis data values for all indices for this sensor ##
        
        {
          # View(thisEventDF)
          
          # [11] "preStimY"           "beginY"             "latY"               "endY"               "answerY"           
          # [16] "responseOnsetY"     "responseEndY"       "rowEndY"            "woeEndY"            "responseRecY"      
          # [21] "postStimY"          
          
          thisPreStimYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'preStimY']
          thisEventBeginYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'beginY']
          thisLatencyYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'latY']
          thisEventEndYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'endY']
          thisAnswerYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'answerY']
          thisResponseOnsetYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'responseOnsetY']
          thisRowEndYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'rowEndY']
          thisResponseEndYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'responseEndY']
          thisWoeEndYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'woeEndY']
          thisRecRowYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'responseRecY']
          thisPostStimYval <- thisEventDF[(thisEventDF$sensorName==thisSensorName),'postStimY']
        }
        
        ## assign the Y-axis data values to output vectors ##
        
        {
          # use the thisColumn scalar from earlier 
          
          preStimYvalsRow[thisColumn] <- thisPreStimYval
          beginYvalsRow[thisColumn] <- thisEventBeginYval
          latencyYvalsRow[thisColumn] <- thisLatencyYval
          endYvalsRow[thisColumn] <- thisEventEndYval
          answerYvalsRow[thisColumn] <- thisAnswerYval
          responseOnsetYvalsRow[thisColumn] <- thisResponseOnsetYval
          rowEndYvalsRow[thisColumn] <- thisRowEndYval
          responseEndYvalsRow[thisColumn] <- thisResponseEndYval
          woeEndYvalsRow[thisColumn] <- thisWoeEndYval
          recRowYvalsRow[thisColumn] <- thisRecRowYval
          postStimYvalsRow[thisColumn] <- thisPostStimYval
        }
        
      } # end l loop over sensors 
      
    } # end k loop over unique events
    
    # 
    
  } # end iteration j over charts
  
  ## fix this Feb 5, 2025 - columns are lost when unlisting if one of the sensors (PLE) is not present ##
  
  {
    # locate missing items due to missing sensors
    # which(!(names(outputRow) %in% names(unlist(preStimIdxRow))))
    # [1]  24  79 134
    # set missing values to NA to avoid loosing the column when unlisting
    
    idxVectorNames <- ls(pattern="IdxRow")
    
    m=1
    for(m in 1:length(idxVectorNames)) {
      # which( !( names(outputRow) %in% names(unlist(thisIdxVector)) ) )
      
      thisIdxVector <- get(idxVectorNames[m])
      
      # set the value to NA for missing sensors
      thisIdxVector[which( !( names(outputRow) %in% names(unlist(thisIdxVector)) ) )] <- NA
      
      # which( !( names(outputRow) %in% names(unlist(thisIdxVector)) ) )
      
      assign(idxVectorNames[m], thisIdxVector)
    }
    
    # preStimIdxRow 
    # beginIdxRow 
    # latencyIdxRow 
    # endIdxRow 
    # answerIdxRow 
    # responseOnsetIdxRow 
    # rowEndIdxRow 
    # responseEndIdxRow 
    # woeEndIdxRow 
    # recRowIdxRow 
    # postStimIdxRow 
    
    YValsVectorNames <- ls(pattern="YvalsRow")
    
    m=1
    for(m in 1:length(YValsVectorNames)) {
      # which( !( names(outputRow) %in% names(unlist(thisYvalsVector)) ) )
      
      thisYvalsVector <- get(YValsVectorNames[m])
      
      # set the value to NA for missing sensors
      thisYvalsVector[which( !( names(outputRow) %in% names(unlist(thisYvalsVector)) ) )] <- NA
      
      # which( !( names(outputRow) %in% names(unlist(thisYvalsVector)) ) )
      
      assign(YValsVectorNames[m], thisYvalsVector)
    }
    
  }
  
  ## transform the output lists to vectors ##
  
  {
    # locate missing items due to missing sensors
    # which(!(names(outputRow) %in% names(unlist(preStimIdxRow))))
    # [1]  24  79 134
    
    preStimIdxRow <- unlist(preStimIdxRow, use.names=FALSE)
    beginIdxRow <- unlist(beginIdxRow, use.names=FALSE)
    latencyIdxRow <- unlist(latencyIdxRow, use.names=FALSE)
    endIdxRow <- unlist(endIdxRow, use.names=FALSE)
    answerIdxRow <- unlist(answerIdxRow, use.names=FALSE)
    responseOnsetIdxRow <- unlist(responseOnsetIdxRow, use.names=FALSE)
    rowEndIdxRow <- unlist(rowEndIdxRow, use.names=FALSE)
    responseEndIdxRow <- unlist(responseEndIdxRow, use.names=FALSE)
    woeEndIdxRow <- unlist(woeEndIdxRow, use.names=FALSE)
    recRowIdxRow <- unlist(recRowIdxRow, use.names=FALSE)
    postStimIdxRow <- unlist(postStimIdxRow, use.names=FALSE)
    
    preStimYvalsRow <- unlist(preStimYvalsRow, use.names=FALSE)
    beginYvalsRow <- unlist(beginYvalsRow, use.names=FALSE)
    latencyYvalsRow <- unlist(latencyYvalsRow, use.names=FALSE)
    endYvalsRow <- unlist(endYvalsRow, use.names=FALSE)
    answerYvalsRow <- unlist(answerYvalsRow, use.names=FALSE)
    responseOnsetYvalsRow <- unlist(responseOnsetYvalsRow, use.names=FALSE)
    rowEndYvalsRow <- unlist(rowEndYvalsRow, use.names=FALSE)
    responseEndYvalsRow <- unlist(responseEndYvalsRow, use.names=FALSE)
    woeEndYvalsRow <- unlist(woeEndYvalsRow, use.names=FALSE)
    recRowYvalsRow <- unlist(recRowYvalsRow, use.names=FALSE)
    postStimYvalsRow <- unlist(postStimYvalsRow, use.names=FALSE)
  }
  
  ## insert the outputRow into the output data frames
  
  {
    ## row indices for events in the time series data
    preStimIdxDF[i,] <- preStimIdxRow
    eventBeginIdxDF[i,] <- beginIdxRow
    latencyIdxDF[i,] <- latencyIdxRow
    eventEndIdxDF[i,] <- endIdxRow
    answerIdxDF[i,] <- answerIdxRow
    responseOnsetIdxDF[i,] <- responseOnsetIdxRow
    rowEndIdxDF[i,] <- rowEndIdxRow
    responseEndIdxDF[i,] <- responseEndIdxRow
    woeEndIdxDF[i,] <- woeEndIdxRow
    recRowIdxDF[i,] <- recRowIdxRow
    postStimIdxDF[i,] <- postStimIdxRow
  }
  
  {
    ## y-values for events in the time series data
    preStimYvalsDF[i,] <- preStimYvalsRow
    eventBeginYvalsDF[i,] <- beginYvalsRow
    latencyYvalsDF[i,] <- latencyYvalsRow
    eventEndYvalsDF[i,] <- endYvalsRow
    answerYvalsDF[i,] <- answerYvalsRow
    responseOnsetYvalsDF[i,] <- responseOnsetYvalsRow
    rowEndYvalsDF[i,] <- rowEndYvalsRow
    responseEndYvalsDF[i,] <- responseEndYvalsRow
    woeEndYvalsDF[i,] <- woeEndYvalsRow
    recRowYvalsDF[i,] <- recRowYvalsRow
    postStimYvalsDF[i,] <- postStimYvalsRow
  }
  
} # end i loop over _Measurements.csv file names



#### change 0 and NA values to missing values #####



for(i in 15:ncol(eventBeginIdxDF)) {
  
  # replace scores of 0
  preStimIdxDF[which(preStimIdxDF[,i] == 0),i] <- ""
  eventBeginIdxDF[which(eventBeginIdxDF[,i] == 0),i] <- ""
  latencyIdxDF[which(latencyIdxDF[,i] == 0),i] <- ""
  eventEndIdxDF[which(eventEndIdxDF[,i] == 0),i] <- ""
  answerIdxDF[which(answerIdxDF[,i] == 0),i] <- ""
  responseOnsetIdxDF[which(responseOnsetIdxDF[,i] == 0),i] <- ""
  rowEndIdxDF[which(rowEndIdxDF[,i] == 0),i] <- ""
  responseEndIdxDF[which(responseEndIdxDF[,i] == 0),i] <- ""
  woeEndIdxDF[which(woeEndIdxDF[,i] == 0),i] <- ""
  recRowIdxDF[which(recRowIdxDF[,i] == 0),i] <- ""
  postStimIdxDF[which(postStimIdxDF[,i] == 0),i] <- ""
  
  # replace NA scores
  preStimIdxDF[is.na(preStimIdxDF[,i]),i] <- ""
  eventBeginIdxDF[is.na(eventBeginIdxDF[,i]),i] <- ""
  latencyIdxDF[is.na(latencyIdxDF[,i]),i] <- ""
  eventEndIdxDF[is.na(eventEndIdxDF[,i]),i] <- ""
  answerIdxDF[is.na(answerIdxDF[,i]),i] <- ""
  responseOnsetIdxDF[is.na(responseOnsetIdxDF[,i]),i] <- ""
  rowEndIdxDF[is.na(rowEndIdxDF[,i]),i] <- ""
  responseEndIdxDF[is.na(responseEndIdxDF[,i]),i] <- ""
  woeEndIdxDF[is.na(woeEndIdxDF[,i]),i] <- ""
  recRowIdxDF[is.na(recRowIdxDF[,i]),i] <- ""
  postStimIdxDF[is.na(postStimIdxDF[,i]),i] <- ""
  
  # replace y-axis values of 0
  preStimYvalsDF[which(preStimYvalsDF[,i] == 0),i] <- ""
  eventBeginYvalsDF[which(eventBeginYvalsDF[,i] == 0),i] <- ""
  latencyYvalsDF[which(latencyYvalsDF[,i] == 0),i] <- ""
  eventEndYvalsDF[which(eventEndYvalsDF[,i] == 0),i] <- ""
  answerYvalsDF[which(answerYvalsDF[,i] == 0),i] <- ""
  responseOnsetYvalsDF[which(responseOnsetYvalsDF[,i] == 0),i] <- ""
  rowEndYvalsDF[which(rowEndYvalsDF[,i] == 0),i] <- ""
  responseEndYvalsDF[which(responseEndYvalsDF[,i] == 0),i] <- ""
  woeEndYvalsDF[which(woeEndYvalsDF[,i] == 0),i] <- ""
  recRowYvalsDF[which(recRowYvalsDF[,i] == 0),i] <- ""
  postStimYvalsDF[which(postStimYvalsDF[,i] == 0),i] <- ""
  
  # replace y-Axis values that are NA
  preStimYvalsDF[is.na(preStimYvalsDF[,i]),i] <- ""
  eventBeginYvalsDF[is.na(eventBeginYvalsDF[,i]),i] <- ""
  latencyYvalsDF[is.na(latencyYvalsDF[,i]),i] <- ""
  eventEndYvalsDF[is.na(eventEndYvalsDF[,i]),i] <- ""
  answerYvalsDF[is.na(answerYvalsDF[,i]),i] <- ""
  responseOnsetYvalsDF[is.na(responseOnsetYvalsDF[,i]),i] <- ""
  rowEndYvalsDF[is.na(rowEndYvalsDF[,i]),i] <- ""
  responseEndYvalsDF[is.na(responseEndYvalsDF[,i]),i] <- ""
  woeEndYvalsDF[is.na(woeEndYvalsDF[,i]),i] <- ""
  recRowYvalsDF[is.na(recRowYvalsDF[,i]),i] <- ""
  postStimYvalsDF[is.na(postStimYvalsDF[,i]),i] <- ""
  
} # end i loop to correct 0 and NA values



#### write the output data frame to a .csv text file ####


# setwd("~/Dropbox/R/NCCAASCII_data/AxcitonN44_NCCAASCIIOutputLAF/responseLatency")

{
  
  # dir.create("./responseLatency")
  # 
  # {
  #   write_csv(preStimIdxDF, paste0("aggPreStimIdx_N", N, ".csv"))
  #   write_csv(eventBeginIdxDF, paste0("aggEventBeginIdx_N", N, ".csv"))
  #   write_csv(latencyIdxDF, paste0("aggLatencyIdx_N", N, ".csv"))
  #   write_csv(eventEndIdxDF, paste0("aggEventEndIdx_N", N, ".csv"))
  #   write_csv(answerIdxDF, paste0("aggAnswerIdx_N", N, ".csv"))
  #   write_csv(responseOnsetIdxDF, paste0("aggResponseOnsetIdx_N", N, ".csv"))
  #   write_csv(rowEndIdxDF, paste0("aggRowEndIdx_N", N, ".csv"))
  #   write_csv(responseEndIdxDF, paste0("aggResponseEndIdx_N", N, ".csv"))
  #   write_csv(woeEndIdxDF, paste0("aggWoeEndIdx_N", N, ".csv"))
  #   write_csv(recRowIdxDF, paste0("aggRecoveryRowIdx_N", N, ".csv"))
  #   write_csv(postStimIdxDF, paste0("aggPostStimIdx_N", N, ".csv"))
  # }
  # 
  # {
  #   write_csv(preStimYvalsDF, paste0("aggPreStimYvals_N", N, ".csv"))
  #   write_csv(eventBeginYvalsDF, paste0("aggEventBeginYvals_N", N, ".csv"))
  #   write_csv(latencyYvalsDF, paste0("aggLatencyYvals_N", N, ".csv"))
  #   write_csv(eventEndYvalsDF, paste0("aggEventEndYvals_N", N, ".csv"))
  #   write_csv(answerYvalsDF, paste0("aggAnswerYvals_N", N, ".csv"))
  #   write_csv(responseOnsetYvalsDF, paste0("aggResponseOnsetYvals_N", N, ".csv"))
  #   write_csv(rowEndYvalsDF, paste0("aggRowEndYvals_N", N, ".csv"))
  #   write_csv(responseEndYvalsDF, paste0("aggResponseEndYvals_N", N, ".csv"))
  #   write_csv(woeEndYvalsDF, paste0("aggWoeEndYvals_N", N, ".csv"))
  #   write_csv(recRowYvalsDF, paste0("aggRecoveryRowYvals_N", N, ".csv"))
  #   write_csv(postStimYvalsDF, paste0("aggPostStimYvals_N", N, ".csv"))
  # }
  
}

