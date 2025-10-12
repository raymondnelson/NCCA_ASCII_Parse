# R function to write the exams from the global env to NCCA ASCII format
# 2020-02-08
# Raymond Nelson
#
# calls a function in the NCCAASCII_Writer.R script
#
####



# rm(list=ls())


# setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII")


# requires each exam saved as .Rda by the NCCA ASCII part script



#### required libraries ####



{
  library(readr)
  library(stringr)
}



#### set the R path for the project ####



{
  
  if(!exists("RPath")) {
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
}


#### source the NCCA ASCII output scripts with the required functions and parameters ####


{
  
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  # initialize the setColRange() function and the getFirstLastEventFn()
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  # correct the header details in NCCA ASCII text files
  source(paste0(RPath, 'NCCAASCII_FixHeaders.R'), echo=FALSE)
  
  # a list of events that are not scored
  source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  
  # a function to list the exams in the global environment
  source(paste0(RPath, 'getExamNames.R'), echo=TRUE)
  
  # function to generate random names
  source(paste0(RPath, 'NCCAASCII_PseudoNamer.R'), echo=FALSE)
  
  # source the NCCA ASCII parse helper function to load the fromMinSec function
  source(paste0(RPath, 'toMinSec.R'), echo=FALSE)
  
  # initialize the function to write the NCCA ASCII files
  source(paste0(RPath, 'NCCAASCII_WRITER.R'), echo=FALSE)
  
}


#### warning level ####


if(getOption("warn") !=2) {
  # set the warn level to suppress warnings
  if(!exists("oldw")) oldw <- getOption("warn")
  # -1 to suppress warnings
  # 0 is normal, warnings are display at end
  # 1 warnings are display at the time
  # 2 warnings are escalated to errors
  options(warn = 2)
  # options(warn = 0)
  # rm(oldw)
  print(paste("old warn level:", oldw))
  print(paste("current warn level: ", getOption("warn")))
}

#### set the output directory ####


{
  # dirName <- paste0("NCCAASCIIOutputLAF", setNumber)
  dirName <- "NCCAASCIIOutputLAF"
  if( basename(getwd()) != dirName ) {
    if( !dir.exists(dirName) ) dir.create(dirName)
    # setwd(dirName)
  }
  print(paste("output directory:", dirName))

  print(paste("current directory:", basename(getwd())))
  # outputNames <- "FZCT_001"
  
  # outputNames <- paste0("FZCT_2020SD_", str_pad(examNumbers, 3, "left", pad="0"))
  
  # print(outputNames)
  
  
  # private function to located the _Data data frame objects
  # getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  # uniqueExams <- getUniqueExams(x="*_Data$")
  
  
  # # get exam names from the _Data data frames
  # print("make a list of unique exams in the global environment")
  # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  # print(uniqueExams)
}


######## set up some parameters for the NCCA ASCII output ########


{
  
  # if(!exists("uniqueExams")) uniqueExams <- x
  
  # if(!exists("outputFolderName")) outputFolderName <- "NCCAASCIIOutputLAF"
  # dirName was initialized earlier
  if(!exists("outputFolderName")) outputFolderName <- dirName
  
  # check that the output folder exists
  if(!dir.exists(outputFolderName)) dir.create(outputFolderName)
  
  # fix (replace) RQ and CQ labels for the different test formats
  fixLabels <- TRUE
  fixLabels <- FALSE
  
  # standardize the RQ and CQ rotations for different test formats
  fixRQRotation <- TRUE
  fixRQRotation <- FALSE
  fixCQRotation <- TRUE
  fixCQRotation <- FALSE
  
  # fix the labels and rotation for all procedural questions (not RQ or CQ)
  fixQuestionLength <- TRUE
  fixQuestionLength <- FALSE
  # RQCQ rotation is also controlled by a setting within the NCCAASC_writerFn()
  
  # adjust the question length and answer distance 
  fixAnswerDistance <- TRUE
  fixAnswerDistance <- FALSE
  
  fixMissingAnswers <- FALSE
  fixMissingAnswers <- TRUE
  
  {
    # set missing parameters
    if(!exists("fixLabels")) fixLabels <- FALSE
    if(!exists("fixRQRotation")) fixRQRotation <- FALSE
    if(!exists("fixCQRotation")) fixCQRotation <- FALSE
    if(!exists("fixRotation")) fixRotation <- FALSE
    if(!exists("fixQuestionLength")) fixQuestionLength <- FALSE
    if(!exists("fixAnswerDistance")) fixAnswerDistance <- FALSE
    if(!exists("fixMissingAnswers")) fixMissingAnswers <- FALSE
  }
  
  # introduce small variation into question onset, end, and answer locations
  ditherEvents <- FALSE
  
  if(isTRUE(ditherEvents)) {
    fixLabels <- FALSE
    fixRQRotation <- FALSE
    fixCQRotation <- FALSE
    fixRotation <- FALSE
    fixQuestionLength <- FALSE
    fixAnswerDistance <- FALSE
    fixMissingAnswers <- FALSE
  }
  
  {
    # June 20, 2025 re-write Stoelting DLST exams
    fixLabels <- FALSE
    fixRQRotation <- FALSE
    fixCQRotation <- FALSE
    fixRotation <- FALSE
    fixQuestionLength <- FALSE
    fixAnswerDistance <- FALSE
    fixMissingAnswers <- FALSE
    
    fixDuplicates <- TRUE
    
  }
  
}


#### include additional sensors ####


{
  
  # forearm cuff
  inclFC <- FALSE
  
  inclAutoEDA <- FALSE
  
  # sensor is not present in older Axciton and Lafayette exams
  inclPPG <- TRUE
  
  # does not exist in old Axciton cases
  inclMove1 <- TRUE
  
}


#### initialize a function to reduce the measurements data frame to a stimuli data frame ####


uniqueEventsFn <- function(x) {
  # R function to reduce the measurements data frame to a Stimuli data frame
  # Called in a loop in this script
  # x input is the eventDF which is actually the _Measurements data frame
  # output is the eventDF with reduced columns
  ##
  # restrict to the first 8 columns for exam, series, chart, label, eventLabel, Begin, End, Answer
  eventDF <- x[,1:8]
  # colapse the data for each event 
  theseEvents <- apply(eventDF, 1, paste, collapse="")
  # compare each even to the previous and keep only 1 row per event
  theseEventRows <- 
    which(theseEvents[2:length(theseEvents)] != theseEvents[1:(length(theseEvents)-1)])
  theseEventRows <- sort(unique(c(1, theseEventRows+1)))
  eventDF <- eventDF[theseEventRows,]
  return(eventDF)
}


#### load the exams from the cwd ####


loadRDA <- FALSE
# loadRDA <- TRUE

if(loadRDA) {
  # load the 2.Rda files from the cwd
  theseRDAs <- list.files(pattern="2.Rda$")
  print(theseRDAs)
  i=1
  for(i in 1:length(theseRDAs)) {
    load(theseRDAs[i])
  }
}


#### initialize a function to get the exams from the global envir ####


{
  
  # make a function to make a list of unique exams in the global environment
  # getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, envir=.GlobalEnv),1, -6)) }
  
  # get exam names from the _Data data frames
  print("make a list of unique exams in the global environment")
  uniqueExams <- getUniqueExams(x="*_Data$")
  print(uniqueExams)
  
}


#### initialize the case OUTPUT NAMES ####


{
  
  {
    
    # a function to generate random case output names 
    
    # source(paste0(RPath, 'NCCAASCII_PseudoNamer.R'), echo=FALSE)
    
    # set.seed(20210218)
    # set.seed(20211208)
    # set.seed(20220225) # forearm cuff study Feb 2023
    # set.seed(20240317)
    # set.seed(20240320)
    set.seed(20250317)
    
    # commmend out the lines below to re-use the existing names
    
    # outputNames <- NCCAASCCINamerFn(60)
    # outputNames <- NCCAASCCINamerFn(100)
    # outputNames <- outputNames[1:15]
    # outputNames <- outputNames[9]
    
  }
  
  if(!exists("outputNames")) {
    # re-write the NCCA ASCII files to the same exam names
    
    # may include the $$ character for axciton
    # outputNames <- uniqueExamNames
    
    # does not include $$ character
    # outputNames <- uniqueExams
    
    # use the name without the "D" in the first character
    outputNames <- str_sub(uniqueExams, 2, -1)
    
    # embed the criterion state in the file name
    # outputNames <- paste0(str_sub(outputNames, 1, 8),
    #                       ifelse(str_sub(outputNames, 10, 10)=="N", 1, 0),
    #                       str_sub(outputNames, 9, 9))
    
    # embed the criterion state in the file name
    # outputNames <- paste0(str_sub(outputNames, 1, 9),
    #                       ifelse(str_sub(outputNames, 10, 10)=="N", 1, 0),
    #                       str_sub(outputNames, 11, -1) )
    
    
    
    # uniqueExams2 <- str_sub(uniqueExams, 2, -1)
    
    # outputNames <- uniqueExamNames
    
    # print(outputNames)
  }
  
  {
    # set.seed(20210218)
    
    # # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_PseudoNamer.R'), echo=TRUE)
    
    # outputNames <- NCCAASCCINamerFn(length(uniqueExams))
    
    # outputNames <- outputNames[16:25]
    # outputNames <- outputNames[2]
    # print(outputNames)
     
    # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
    # print(uniqueExams)
    
    # x=uniqueExams
    # y=100
    # showNames=TRUE
    # output=FALSE
    
    # outputNames <- paste0("FZCT_N60_", str_pad(1:10, 3, "left", pad="0"))
    # outputNames <- paste0("SPOT_", str_pad(1:1, 3, "left", pad="0"))
    # outputNames <- "FZCT_001"
    # outputNames <- "FZCT2020D076A"
  }
  
  {
    # # pad with $ for axciton files
    # i=1
    # for (i in 1:length(outputNames)) {
    #   outputNames[i] <- 
    #     paste0(paste(rep("$", times=(8 - nchar(outputNames[i]))), collapse=""),
    #            outputNames[i])
    # }
    # print(outputNames)
  }
  
  print(unique(outputNames))
  
}


######## fix the criterion state data frame ########


useCriterionDF <- FALSE
# useCriterionDF <- TRUE

if(useCriterionDF) {
  
  if(!exists("criterionStateDF")) {
    if(file.exists("criterionState.csv")) {
      criterionStateDF <- read_csv("./criterionState.csv")
    }
  }
  
  if(exists("criterionStateDF")) {
    
    criterionStateDFNew <- criterionStateDF
    
    i=1
    for (i in 1:length(uniqueExams)) {
      # uniqueExams[i]  
      thisRow <- which(criterionStateDF$examName ==  str_sub(uniqueExams[i], 2, -1))
      criterionStateDFNew$examName[thisRow] <- outputNames[i]
    }
    
    # View(criterionStateDFNew)
    
    criterionStateDFNew$newExamName <- criterionStateDFNew$examName
    
    write_csv(criterionStateDFNew, 
              paste0("./", outputFolderName, "/", "criterionState.csv"))
    
  }
  
}


########################################################################

##########  write the NCCA ASCII output to Lafayette format ############


# set to false so this script does not run unintentionally
writeNCAAASCII_LAF <- FALSE
writeNCAAASCII_LAF <- TRUE

if(!exists("writeNCAAASCII_LAF")) writeNCAAASCII_LAF <- FALSE



if(isTRUE(writeNCAAASCII_LAF)) {
  
  #### get the exams from the global env ####
  
  {
    # get exam names from the _Data data frames in the global envir
    print("make a list of unique exams in the global environment")
    
    # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
    # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", envir=.GlobalEnv),1, -6))
    
    # done earlier
    # uniqueExams <- getUniqueExams(x="*_Data$")
    
    print(uniqueExams)
    
    # source the getExamNames.R script to load and call the getCharts() function
    # source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=TRUE)
    
    # uniqueExamNames <- getCharts(x="D&-", uniqueTests=TRUE)
    # uniqueExamNames <- getCharts(x="D%-", uniqueTests=TRUE)
    # uniqueExamNames <- getCharts(x="D\\$-", uniqueTests=TRUE)
    # print(uniqueExamNames)
  }
  
  #### set the output names ####
  
  {
    
    # done earlier 
    
    # different method to get the output names
    # if(file.exists("criterionState.csv")) {
    #   criterionState <- read_csv("criterionState.csv")
    #   # View(criterionState)
    #   outputNames <- criterionState$examName
    #   
    #   print(outputNames)
    # }
    
    # write the .csv with the old names and new names
    # oldNameNewNameDF <- 
    #   cbind.data.frame(oldName=uniqueExams, newName=outputNames, examName=uniqueExamNames)
    # write_csv(oldNameNewNameDF, paste0("oldNameNewName_", length(outputNames), ".csv"))
    
    # dirName <- "testDir"
    # if(!dir.exists("./testDir")) dir.create("testDir")
    # setwd(dirName)
    
    if(!exists("outputNames")) {
      outputNames <- uniqueExams
      print(outputNames)
    }
    
    print(outputNames)
    
    # if(!exists("setNumber")) setNumber <- ""
    
    if(length(uniqueExams) != length(outputNames)) {
      stop("unequal lengths: check the length of the outputNames vector ")
    }
    
  }
  
  ######## set the series name ########
  
  {
    
    # series name is normally obtained from the data
    
    # if(!exists("outputSeriesName")) outputSeriesName <- "2"
    # 
    # outputSeriesName <- "2"
    # # outputSeriesName <- "X"
    # 
    # print(paste("output series name:", outputSeriesName))
    
  }
  
  ######## set the output sensors ########
  
  {
    if(!exists("outputSensors")) outputSensors <- c("c_UPneumoSm", "c_LPneumoSm", "c_AutoEDA", "c_Cardio1", "c_Move1", "c_PPG1")
    if(is.null(outputSensors)) outputSensors <- c("c_UPneumoSm", "c_LPneumoSm", "c_AutoEDA", "c_Cardio1", "c_Move1", "c_PPG1")
    
    # inclPPG <- FALSE
    if(!exists("inclPPG")) inclPPG <- TRUE
    
    if(inclFC && !("FC" %in% outputSensors)) {
      outputSensors <- c(outputSensors, "c_FC")
    }
    
    if(!inclPPG) {
      outputSensors <- outputSensors[outputSensors != "c_PPG1"]
    }
    
    if(!inclMove1) {
      outputSensors <- outputSensors[outputSensors != "c_Move1"]
    }
    
    print(paste("output sensors:", paste(outputSensors, collapse=" ")))
  }
  
  #### fix or manage repeated events ####
  
  {
    # TRUE will restore duplicate event labels that were adjusted at data import
    # TRUE ensures that duplicate event labels are output as recorded
    fixDuplicates <- TRUE
    # fixDuplicates <- FALSE
    # FALSE will allow for exporting adjusted labels
    # so that each even has a unique label 
  }
  
  ###############################################################################
  
  #### ITERATE over the exams and write the NCCA ASCII output ####
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_writer.R')
  # this function will call another function from the NCCAASCII_writer.R script
  
  # stop()
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      # get the data frame with the times series data
      
      if(!exists("i")) i=1
      
      examName <- uniqueExams[i]
      assign("examName", examName, pos=1)
      print(examName)
      print(paste(i, "of", length(uniqueExams)))
      
      # get the names of time series lists for all unique series in each exam
      # searchString <- paste0("*", examName, "_Data", "*")
      
      # get the time series data frame for the exam
      # examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      # instead get the data from the input vector of exam names
      examDF <- get(paste0(examName, "_Data"), pos=1)
      
      # set the first and last rows
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      # print(examName)
      
      # get the stimulus events data frame for the exam
      
      # trying this method Feb 27, 2024
      eventDFName <- paste0(examName, "_Stimuli")
      eventDF <- get(eventDFName, pos=1)[,c(1:3, 5:9)]
      # the _Measurements data frame is no handling repeated annotations correctly
      
      # commented out the method using _Measurements Feb 27, 2024
      # eventDFName <- paste0(examName, "_Measurements")
      # if(exists(eventDFName)) {
      #   eventDF <- get(eventDFName, pos=1)
      #   eventDF <- eventDF[,1:8]
      #   # reduce the measurements data frame to a Stimuli data frame
      #   eventDF <- uniqueEventsFn(eventDF)
      # } 
      
      # View(eventDF)
      
      {
        # change the data types - this should be done during parsing for efficiency
        # eventDF$seriesName <- as.character(eventDF$seriesName)
        # eventDF$Event <- as.numeric(eventDF$Event)
        # eventDF$Begin <- as.numeric(eventDF$Begin)
        # eventDF$End <- as.numeric(eventDF$End)
        # eventDF$Answer <- as.numeric(eventDF$Answer)
      }
      
      # reassign the _Stimuli and eventDF data frames
      # assign(paste0(examName, "_Stimuli"), eventDF, pos=1)
      # assign("eventDF", eventDF, pos=1)
      # View(eventDF)
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(examDF$seriesName))
      print(paste("unique series:", paste(uniqueSeries, collapse=" ")))
            
      # initialize a newEventDF for this exam
      newEventDF <- NULL
      
    }
    
    ##### set the base question lengths for the exam #####
    
    {
      
      if(!exists("cps")) cps <- 30
      
      # these will be varied later
      
      # these lengths are used only if fixRQLengths == TRUE
      
      # initialize the stimulus lengths here so that they are similar for each chart
      # initialize the new RQ and CQ lengths - this can be 4 RQs regardless of 
      RQLengths <- round(sample(c((4.3*cps):(5.3*cps)), 5, replace=TRUE))
      CQLengths <- round(sample(c((4.7*cps):(5.7*cps)), 5, replace=TRUE))
      
      # RQLengths 
      # CQLengths
      
      SALength <- round(sample(c((4.5*cps):(5.5*cps)), 1))
      
      # symptomatic lengths - also introductory question
      SYLengths <- round(sample(c((4.5*cps):(4.75*cps)), 3, replace=TRUE))
      
      # neutral question lengths (need a lot of these for the ACQT)
      NLengths <- round(sample(c((1.75*cps):(2.25*cps)), 8, replace=TRUE))
      
      # X and XX announcement lengths
      XXXLengths <- round(c(sample(c((4.5*cps):(5*cps)), 1), sample(c((3.5*cps):(4*cps)), 1)))
    
    }
    
    #### iterate over each unique series ####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        # get the time-series data frame for this series
        seriesName <- uniqueSeries[j]
        seriesRows <- which(examDF$seriesName == seriesName)
        # seriesOnsetRow <- which(examDF$seriesName == seriesName)[1]
        # seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        seriesOnsetRow <- min(seriesRows)
        seriesEndRow <- max(seriesRows)
        seriesDF <- examDF[seriesRows,]
        # View(seriesDF)
        
        assign("seriesDF", seriesDF, pos=1)
        assign("seriesName", seriesName, pos=1)
        
        print(paste("series:", toString(seriesName)))
        
        # get a vector of names of time series data for the charts in the exam series
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        print(paste("unique charts:", toString(uniqueCharts)))
        
        # outputSeriesName <- "2"
        outputSeriesName <- seriesName
        
        # outputSeriesName <- NULL
        if(is.null(outputSeriesName)) {
          # use the series number if the outputSeriesName is NULL
          outputSeriesName2 <- j
        } else {
          # use the input param if not NULL
          outputSeriesName2 <- outputSeriesName
        }
      }
      
      ##### iterate over each chart #####
      
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          # get the chart and chart details
          
          chartName <- uniqueCharts[k]
          assign("chartName", chartName, pos=1)
          print(paste("chart name:", chartName))
          
          # get the data frame with the time series data for each chart in the series
          chartRows <- which(seriesDF$chartName==chartName)
          # chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          # chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          # chartEndRow <- max(which(seriesDF$chartName==chartName))
          chartOnsetRow <- min(chartRows)
          chartEndRow <- max(chartRows)
          
          chartDF <- seriesDF[chartRows,]
          assign("chartDF", chartDF, pos=1)
          # View(chartDF)
        }
        
        # get the first and last events
        
        {
          # # source the sigProcHelper.R script for the getFirstLastEventFn
          # # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
          # 
          # firstLastEvents <- getFirstLastEventFn(x=chartDF)
          # firstEvent <- firstLastEvents[1]
          # lastEventEnd <- firstLastEvents[2] - 450
          # lastEvent <- firstLastEvents[2] - 450
          # assign("firstLastEvents", firstLastEvents, pos=1)
        }
        
        ####### planned action here #######
        
        {
          
          
        }
        
        #####  slice a time series data frame for this chart #####
        
        {
          
          {
            
            # a vector of sensors to be included in the NCCA ASCII output
            # outputSensors2 <- c("UPneumo", "LPneumo", "EDA1", "Cardio1", "Move1", "PPG1")
            outputSensors2 <- outputSensors
            
            if(isTRUE(inclMove1)) {
              outputSensors2 <- unique(c(outputSensors2, "c_Move1"))
            }
            
            if(isTRUE(inclPPG)) {
              outputSensors2 <- unique(c(outputSensors2, "c_PPG1"))
            }
            
            outputSensors2 <- unique(outputSensors2)
            
          }
          
          {
            
            ## check for PPG and Activity sensors ##
            
            # remove the Activity sensor using an input parameter
            # if(!isTRUE(inclMove1)) {
            #   # outputSensors2 <- outputSensors2[ -(which(outputSensors2 == "Move1")) ]
            #   outputSensors2 <- outputSensors[outputSensors != "Move1"]
            # }
            
            # remove the Activity sensor if not present
            if(!("Move1" %in% names(chartDF))) {
              # outputSensors2 <- outputSensors2[ -(which(outputSensors2 == "Move1")) ]
              outputSensors2 <- outputSensors[outputSensors != "c_Move1"]
            }
            
            # use the input parameter to exclude the PPG
            # if(!isTRUE(inclPPG)) {
            #   # outputSensors2 <- outputSensors[c(1, 2, 3, 4, 5)]
            #   outputSensors2 <- outputSensors[outputSensors != "PPG1"]
            # }
            
            # remove the PLE sensor if not present
            if(!("PPG1" %in% names(chartDF))) {
              # outputSensors2 <- outputSensors[c(1, 2, 3, 4, 5)]
              outputSensors2 <- outputSensors[outputSensors != "c_PPG1"]
            }
            
          }
          
          # View(chartDF)
          
          # column numbers for the output sensors
          theseSensorCols <- which(names(chartDF) %in% outputSensors2)
          # names(chartDF)[theseSensorCols]
          
          # names(chartDF)[theseSensorCols]
          
          {
            
            ## rearrange the sensor columns ##
            
            theseSensorCols2 <- theseSensorCols
            n=1
            for(n in 1:length(theseSensorCols2)) {
              theseSensorCols2[n] <- 
                theseSensorCols[which(names(chartDF)[theseSensorCols] == outputSensors[n])]
                # this will put the sensors in order
            }
            
          }
          
          # slice the chart data frame to work with
          # the first 10 cols are exam, series, chart and event info
          thisChartDF <- chartDF[,c(1:10, theseSensorCols2)]
          # View(thisChartDF)
          
          thisChartDFNames <- c(names(thisChartDF[1:10]), 
                                c("UPneumo", "LPneumo", "EDA1", "Cardio1") )
          if(inclMove1) {
            thisChartDFNames <- c(thisChartDFNames, "Move1")
          }
          
          if(inclPPG) {
            thisChartDFNames <- c(thisChartDFNames, "PPG1")
          }
          
          # names(thisChartDF) <- c(names(thisChartDF[1:10]), 
          #                               c("UPneumo", "LPneumo", "EDA1", "Cardio1", "Move1", "PPG1") )
          names(thisChartDF) <- thisChartDFNames
          
          assign("thisChartDF", thisChartDF, envir=.GlobalEnv)
          # View(thisChartDF)
          
        }
        
        ##### slice a chart events data frame for the events in this chart #####
        
        {
          
          theseEventRows <- which(eventDF$seriesName == seriesName & 
                                    eventDF$chartName == chartName)
          thisChartEventsDF <- eventDF[theseEventRows,]
          
          # if(thisChartEventsDF$seriesName[1] == 1 && nrow(thisChartEventsDF) != 9) {
          #   # print("incorrect number of ACQT events")
          #   # View(thisChartEventsDF)
          #   stop(paste0(thisChartEventsDF$examName[1], ": incorrect number of ACQT events"))
          # }
          # if(thisChartEventsDF$seriesName[1] == 2 && nrow(thisChartEventsDF) != 12) {
          #   print(paste0("chart: ", thisChartEventsDF$chartName[1]))
          #   # View(thisChartEventsDF) 
          #   stop(paste0(thisChartEventsDF$examName[1], ": incorrect number of CQT events"))
          # }
          
          assign("thisChartEventsDF", thisChartEventsDF, envir=.GlobalEnv)
          # View(thisChartEventsDF)
          
        }
        
        ##### vary the lengths of the questions for this chart #####
        
        {
          # base lengths were set earlier and are varied here for each chart
          
          # these are used only if fixRQLengths==TRUE
          
          RQLengths2 <- RQLengths + sample(c(-4:4), size=length(RQLengths), replace=TRUE)
          CQLengths2 <- CQLengths + sample(c(-4:4), size=length(CQLengths), replace=TRUE)
          SALength2 <- SALength + sample(c(-4:4), size=length(SALength), replace=TRUE)
          SYLengths2 <- SYLengths + sample(c(-4:4), size=length(SYLengths), replace=TRUE)
          NLengths2 <- NLengths + sample(c(-4:4), size=length(NLengths), replace=TRUE)
          XXXLengths2 <- XXXLengths + sample(c(-4:4), size=length(XXXLengths), replace=TRUE)
        }
        
        ##### construct the new chart name #####
        
        {
          newChartName <- paste0(str_pad(k, 2, "left", pad="0"), "A" )
          print(paste("new chart name:", newChartName))
        }
        
        ##### construct the NCCA ASCII output file name #####
        
        {
          
          outputNameI <- outputNames[i]
          
          # if(exists("criterionState")) {
          #   outputNameI <- 
          #     criterionState$examName[which(criterionState$AxExamName==uniqueExams2[i])]
          # }  
          
          thisOutputName <- paste0(str_sub(outputNameI, 1, -1),
                                   "-",
                                   # "2", # series number
                                   outputSeriesName2,
                                   ".", 
                                   # chart name is in NCCA form ex: "01A"
                                   newChartName )
          assign("thisOutputName", thisOutputName, envir=.GlobalEnv)
          
          print(paste("output file name:", thisOutputName))
          
          # "D&-" will be added by the NCCAASCII_writer function
          
          thisChartDF_SAVE <- thisChartDF
          thisChartEventsDF_SAVE <- thisChartEventsDF
          
        }
        
        ##### call a function to write the NCCA ASCII file #####
        
        # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_writer.R')
        
        writeNCCAASCIIFn(thisChartDF=thisChartDF, 
                         thisChartEventsDF=thisChartEventsDF,
                         newChartName=newChartName,
                         thisOutputName=thisOutputName,
                         outputSensors2=outputSensors2,
                         outputSeriesName2=outputSeriesName2,
                         inclPPG=inclPPG,
                         inclAutoEDA=inclAutoEDA,
                         inclFC=inclFC,
                         inclMove1=inclMove1,
                         fixMissingAnswers=fixMissingAnswers,
                         fixAnswerDistance=fixAnswerDistance,
                         fixLabels=fixLabels,
                         fixRQRotation=fixRQRotation,
                         fixCQRotation=fixCQRotation,
                         fixQuestionLength=fixQuestionLength,
                         fixDuplicates=fixDuplicates,
                         RQLengths2=RQLengths2,
                         CQLengths2=CQLengths2,
                         SALength2=SALength2,
                         SYLengths2=SYLengths2,
                         NLengths2=NLengths2,
                         XXXLengths2=XXXLengths2,
                         ditherEvents=ditherEvents,
                         outputFolderName=outputFolderName )
        
      } # end for loop over each k chart
      
      # chartDF was submitted to the examDF in the inner loop
      
    } # end for loop over each j series
    
    # no need to submit the seriesDF to the examDF
    
  } # end for loop over i unique exam names
  
  print(paste(i, "exams processed"))
  
  print(outputNames)
  
  # setwd("./NCCAASCIIOutputLAF")
   
  
  ####
  
  # reset the warn level
  if(exists("oldw")) options(warn = oldw)
  
  
  # initialize the name of the csv file to hold the old and new names
  # setNumber <- "_set9"
  # setNumber <- ""
  # csvName <- paste0("FZCT_SD2020", setNumber, ".csv")
  # csvName <- paste0("AFMGQT_LEPET", setNumber, ".csv")
  
  
  # examNumbers <- c(87:87)
  
  
  # write the csv with old name and new exam name
  # write_csv(cbind.data.frame(uniqueExams, outputNames), csvName)
  
  
  # set to false so this script does not run unintentionally
  writeNCAAASCII_LAF <- FALSE
  # writeNCAAASCII_LAF <- TRUE
  
  
} # end isTRUE(reWriteNCAAASCII_LAF)



# reWriteNCAAASCII_LAF <- TRUE
# setNumber <- "_set2"
# csvName <- paste0("FZCT_SD2020", "_set2", ".csv")
# examNumbers <- c(11:20)



# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF")

