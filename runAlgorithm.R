# R script to run an analysis algorithm on 
# May 11, 2021
# Raymond Nelson
#
####
# 
# three functions in this script
#
# toLongMeasurementTableFn() to transform measurement score sheets 
# to long format from Excel or LXSoftware and LXEdge OSS-3 report
# 
# toWideMeasurementTableFn() to transform measurement score sheets
# to wide formate for input to the Excel OSS-3 prototype
#
# 




# *_Measurements.csv is the _Measurements data frame
# with all feature extraction values and algorithm results 
#
# _measurementsTable.csv is a scoresheet table of feature extraction values
#
# *_2.Rda is the serialized exam data
#
# _ANALYSIS 


library(stringr)
library(readr)


# need to check that the _ANALYSIS list exists in the global envir

# need to transform the feature extraction scoresheet to long format
# so that data can be transferred from the Lafayette and Excel OSS-3
# 

toLongMeasurementTableFn <- function(x="*_measurementTable.csv",
                                             saveCSV=TRUE,
                                             fileName="newRqCqDFSeries") {
  # R function to transform a wide measurement score sheet
  # to a long measurement data frame for analysis
  # May 11, 2021
  # Raymond Nelson 
  #
  # x input is the name of a .csv file with the wide format measurement table
  #
  # output is the long format RqCqDFSeries 
  # 
  # output can be saved as a CSV 
  # and also as a data frame that can be submitted to a scoring algorithm
  #
  # can be used to submit LxSoftware and LXEdge measurments to R OSS-3
  #
  ####
  
  if(!exists("x")) x <- "*_measurementTable.csv"
  if(!exists("fileName")) fileName <- "newRqCqDFSeries"
  
  theseCSVFiles <- list.files(pattern = x)
  
  i=1
  for(i in 1:length(theseCSVFiles)) {
    
    thisCSV <- read.csv(theseCSVFiles[i], header=TRUE)  
    
    {
      # fix question labels
      theseDFNames <- names(thisCSV)
      
      theseDFNames[which(theseDFNames == "X2R")] <- "R2"
      theseDFNames[which(theseDFNames == "X3R")] <- "R3"
      theseDFNames[which(theseDFNames == "X4R")] <- "R4"
      theseDFNames[which(theseDFNames == "X5R")] <- "R5"
      theseDFNames[which(theseDFNames == "X6R")] <- "R6"
      theseDFNames[which(theseDFNames == "X7R")] <- "R7"
      theseDFNames[which(theseDFNames == "X8R")] <- "R8"
      theseDFNames[which(theseDFNames == "X9R")] <- "R9"
      theseDFNames[which(theseDFNames == "X10R")] <- "R10"
      theseDFNames[which(theseDFNames == "X11R")] <- "R11"
      
      theseDFNames[which(theseDFNames == "X3C")] <- "C3"
      theseDFNames[which(theseDFNames == "X4C")] <- "C4"
      theseDFNames[which(theseDFNames == "X5C")] <- "C5"
      theseDFNames[which(theseDFNames == "X6C")] <- "C6"
      theseDFNames[which(theseDFNames == "X7C")] <- "C7"
      theseDFNames[which(theseDFNames == "X8C")] <- "C8"
      theseDFNames[which(theseDFNames == "X9C")] <- "C9"
      theseDFNames[which(theseDFNames == "X10C")] <- "C10"
      
      theseDFNames[which(theseDFNames == "X1N")] <- "N1"
      theseDFNames[which(theseDFNames == "X2N")] <- "N2"
      theseDFNames[which(theseDFNames == "X3N")] <- "N3"
      
      names(thisCSV) <- theseDFNames
    }
    
    {
      # fix sensorNames
      
      theseSensorNames <- thisCSV$sensorName
      
      theseSensorNames[which(theseSensorNames == "P1")] <- "LPneumo"
      theseSensorNames[which(theseSensorNames == "P2")] <- "UPneumo"
      theseSensorNames[which(theseSensorNames == "EDA")] <- "AutoEDA"
      theseSensorNames[which(theseSensorNames == "C")] <- "Cardio"
      theseSensorNames[which(theseSensorNames == "E")] <- "AutoEDA"
      theseSensorNames[which(theseSensorNames == "M")] <- "Move1"
      theseSensorNames[which(theseSensorNames == "F")] <- "PLE"
      theseSensorNames[which(theseSensorNames == "V")] <- "PLE"
      
      thisCSV$sensorName <- theseSensorNames

      # View(thisCSV)      
    }
    
    {
      # get the exam details
      
      # examName <- str_sub(theseCSVFiles[i], 1, -24)
      examName <- thisCSV$examName[1]
      examName <- str_sub(examName, 2, -1)
      
      # seriesName <- unique(str_sub(theseCSVFiles[i], -22, -22))
      seriesName <- thisCSV$seriesName[1]
      
      chartNames <- unique(thisCSV$chartName)
      
      eventNames <- names(thisCSV[5:ncol(thisCSV)])
      
      sensorNames <- unique(thisCSV$sensorName)
    }
    
    {
      # initializee a sensor for the combined respiration score
      
      useSensors <- c("UPneumo", "LPneumo", "Pneumo", "AutoEDA", "Cardio", "PLE")
      
      sensorNames <- useSensors[useSensors %in% unique(c(sensorNames, "Pneumo"))]
    }
    
    {
      # initialize the RqCqDFSeries
      
      nRowDF <- 
        length(chartNames) * length(eventNames) * length(sensorNames)
      
      newRqCqDFSeries <- 
        cbind.data.frame(examName=rep(examName, length=nRowDF),
                         seriesName=seriesName,
                         chartName=rep(chartNames, each=nRowDF/length(chartNames)),
                         Label=rep(rep(eventNames, each=length(sensorNames)), times=length(chartNames)),
                         eventLabel=rep(rep(eventNames, each=length(sensorNames)), times=length(chartNames)),
                         sensorName=rep(rep(sensorNames, times=length(eventNames)), times=length(chartNames)),
                         sensorMeasurement=as.character(NA),
                         OSS3Score=NA ) 
      # View (newRqCqDFSeries)
    }
    
    # now populate the sensorMeasurement Column
    
    # iterate over the rows of the input CSV
    j=1
    for(j in 1:nrow(thisCSV)) {
      
      thisRow <- thisCSV[j,]
      
      thisChart <- thisRow$chartName
      
      thisSensor <- thisRow$sensorName
      
      # iterate over the sensor colums
      k=5
      for(k in 5:length(thisRow)) {
        
        thisEventName <- names(thisRow)[k]
          
        thisMeasurement <- as.character(thisRow[k])
        
        # add the measurement to the 
        
        chartRows <- which(newRqCqDFSeries$chartName == thisChart)
        
        eventRows <- which(newRqCqDFSeries$eventLabel == thisEventName)
        
        sensorRows <- which(newRqCqDFSeries$sensorName == thisSensor)
        
        # locate the row
        thisNewRow <- 
          chartRows[ which( chartRows %in% sensorRows[which(sensorRows %in% eventRows)] ) ] 
                
        # submit the measurement to the long data frame            
        newRqCqDFSeries$sensorMeasurement[thisNewRow] <- thisMeasurement
          
      } # end k loop over rows
      
      newRqCqDFSeries$sensorMeasurement <- as.character(newRqCqDFSeries$sensorMeasurement)

      # View(newRqCqDFSeries)
      
    } # end j loop over CSV rows 
    
    if(isTRUE(saveCSV)) {
      csvName <- paste0(examName, "_", fileName, ".csv")
      write_csv(x=newRqCqDFSeries, file=csvName)
    }
    
    return(newRqCqDFSeries)
    
  } # end i loop over CSV files
  
} # end toLongMeasurementTableFn()



# toLongMeasurementTableFn(x="*_measurementTable.csv",
#                           saveCSV=TRUE,
#                           fileName="newRqCqDFSeries")



# toLongMeasurementTableFn(x="James_featureExtraction_excel.csv")
# toLongMeasurementTableFn(x="James_featureExtraction_LX.csv")
# toLongMeasurementTableFn(x="*_measurementTable.csv")



############################



toWideMeasurementTableFn <- function(x="_Measurements.csv",
                                        saveCSV=TRUE,
                                        fileName="_newMeasurementTable") {
  # R function to transform a long measurement table
  # to a wide measurement score sheet
  # May 13, 2021
  # Raymond Nelson 
  #
  # x input is the name of a .csv file with the long format measurement table
  #
  # output is the wide format measurement score sheet 
  # 
  # output can be saved as a CSV 
  #
  # can be used the submit R feature extraction vals to Excel OSS-3
  # 
  ####
  
  if(!exists("x")) x <- "_Measurements.csv"
  if(!exists("fileName")) fileName <- "newRqCqDFSeries"
  
  theseCSVFiles <- list.files(pattern = x)
  
  i=1
  for(i in 1:length(theseCSVFiles)) {
    
    {
      # get the long measurement data frame 
      
      # keep only 10 columns, including the feature extractuion values
      thisCSV <- read.csv(theseCSVFiles[i], header=TRUE)  [,1:10]
      
      # keep only RQ and CQ rows
      keepRows1 <- grep(pattern="[R, C]", thisCSV$Label)
      thisCSV <- thisCSV[keepRows1,]
      
      # keep only some sensors
      keepSensors <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE")
      keepRows2 <- which(thisCSV$sensorName %in% keepSensors)
      thisCSV <- thisCSV[keepRows2,]
      
      # View(thisCSV)
    }
    
    {
      # fix question labels
      theseEventNames <- thisCSV$eventLabel
      
      theseEventNames[which(theseEventNames == "2R")] <- "R2"
      theseEventNames[which(theseEventNames == "3R")] <- "R3"
      theseEventNames[which(theseEventNames == "4R")] <- "R4"
      theseEventNames[which(theseEventNames == "5R")] <- "R5"
      theseEventNames[which(theseEventNames == "6R")] <- "R6"
      theseEventNames[which(theseEventNames == "7R")] <- "R7"
      theseEventNames[which(theseEventNames == "8R")] <- "R8"
      theseEventNames[which(theseEventNames == "9R")] <- "R9"
      theseEventNames[which(theseEventNames == "10R")] <- "R10"
      theseEventNames[which(theseEventNames == "11R")] <- "R11"
      
      theseEventNames[which(theseEventNames == "3C")] <- "C3"
      theseEventNames[which(theseEventNames == "4C")] <- "C4"
      theseEventNames[which(theseEventNames == "5C")] <- "C5"
      theseEventNames[which(theseEventNames == "6C")] <- "C6"
      theseEventNames[which(theseEventNames == "7C")] <- "C7"
      theseEventNames[which(theseEventNames == "8C")] <- "C8"
      theseEventNames[which(theseEventNames == "9C")] <- "C9"
      theseEventNames[which(theseEventNames == "10C")] <- "C10"
      
      theseEventNames[which(theseEventNames == "1N")] <- "N1"
      theseEventNames[which(theseEventNames == "2N")] <- "N2"
      theseEventNames[which(theseEventNames == "3N")] <- "N3"
      
      thisCSV$eventLabel <- theseEventNames
    }
    
    {
      # get the exam details
      
      examName <- thisCSV$examName[1]
      examName <- str_sub(examName, 2, -1)
      
      seriesName <- as.character(thisCSV$seriesName[1])
      
      chartNames <- unique(thisCSV$chartName)
      
      eventNames <- unique(thisCSV$eventLabel)
      
      sensorNames <- unique(thisCSV$sensorName)
      sensorNames <- sensorNames[sensorNames %in% keepSensors]
    }
    
    {
      # initialize a wide format score sheet 
      
      nRowDF <- 
        length(chartNames) * length(sensorNames)
      
      newMeasurementScoreSheetDF <- 
        cbind.data.frame(examName=rep(examName, length=nRowDF),
                         seriesName=seriesName,
                         chartName=rep(chartNames, each=length(sensorNames)),
                         sensorName=rep(sensorNames, times=length(chartNames)) )
      
      # add colums for the RQ and CQ events
      for(j in 1:length(eventNames)) {
        newMeasurementScoreSheetDF <- cbind(newMeasurementScoreSheetDF, NA)
      }
      names(newMeasurementScoreSheetDF)[5:ncol(newMeasurementScoreSheetDF)] <- eventNames
      
      # View(newMeasurementScoreSheetDF)
    }
    
    # iterate over the input data and populate the measurement score sheet
    k=1
    for(k in 1:nrow(thisCSV)) {
      
      thisChart <- thisCSV$chartName[k]
      thisSensor <- thisCSV$sensorName[k]
      thisEvent <- thisCSV$eventLabel[k]
      
      thisMeasurement <- round(as.numeric(thisCSV$sensorMeasurement[k]), 2)
       
      # locate the row and column in the wide data frame
      theseChartRows <- newMeasurementScoreSheetDF$chartName == thisChart
      theseSensorRows <- newMeasurementScoreSheetDF$sensorName == thisSensor
      thisNewRow <- which(theseChartRows & theseSensorRows)
      
      # locate the column for this event
      thisEventCol <- which(names(newMeasurementScoreSheetDF) == thisEvent)
      
      # submit the feature extraction value to the wide data frame
      newMeasurementScoreSheetDF[thisNewRow,thisEventCol] <- thisMeasurement
      
    } # end k loop over CSV rows
    
    # View(newMeasurementScoreSheetDF)
    
  } # end i loop over long CSV files
  
  
  
  if(isTRUE(saveCSV)) {
    csvName <- paste0(examName, "_", fileName, ".csv")
    write_csv(x=newMeasurementScoreSheetDF, file=csvName)
  }
  
  return(newMeasurementScoreSheetDF)
  
} # end toWideMeasurementTableFn()



# toWideMeasurementTableFn(x="_Measurements.csv",
#                      saveCSV=TRUE,
#                      fileName="_newMeasurementTable")


# toWideMeasurementTableFn(x="_Measurements.csv")


# use this function to put data from the R OSS-3 algorithm 
# into the Excel OSS-3 prototype



###################



source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Scores.R', echo=FALSE)

source(paste0(RPath, 'R/NCCA_ASCII_Parse/outputScores.R'), echo=FALSE)

source(paste0(RPath, 'R/NCCA_ASCII_Parse/KWANOVA.R'), echo=FALSE)

source(paste0(RPath, 'R/NCCA_ASCII_Parse/decisionRules.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/autoSelectTSRSSR.R'), echo=FALSE)




runAlgorithmFn <- function(x="RqCqDFSeries.csv", 
                           analysisListName="_OSS3_ANALYSIS") {
  # R function to call the OSS-3 algorithm script
  # May 15, 2021
  # Raymond Nelson
  # 
  # this function will search the cwd for 
  #
  # x input is a character string to search for the .csv file
  # with the feature extraction measurements in long format
  #
  # output is a side effect from each algorithm
  # the algorithm analysis and result are added to a list
  
  ####
  
  if(!exists("analysisListName")) analysisListName <- "_OSS3_ANALYSIS"
  
  searchString <- x
  
  RqCqDFNames <- list.files(pattern=searchString)
  
  #### iterate on the vector of RcQqDFSeries names ####
  
  i=1
  for(i in 1:length(RqCqDFNames)) {
    
    {
      RqCqDFSeries <- read_csv(RqCqDFNames[i])
      
      RqCqDFSeries$seriesName <- as.character(RqCqDFSeries$seriesName)
      
      RqCqDFSeries <- as.data.frame(RqCqDFSeries)
      
      if(!("OSS3Score" %in% names(RqCqDFSeries))) RqCqDFSeries$OSS3Score <- NA
      if(!("CQMean" %in% names(RqCqDFSeries))) RqCqDFSeries$CQMean <- NA
    }
      
    {
      # initialize an output list to hold the analysis results
      
      analysisResultList <- as.list(NULL)
      analysisListName2 <- paste0(examName, analysisListName)
      # assign("analysisResultList", analysisResultList, envir=.GlobalEnv)
      assign(analysisListName2, analysisResultList, envir=.GlobalEnv)
      # each scoring algorithm can access and add to this list
    }
    
    {
      # get the exam details
      
      examName <- RqCqDFSeries$examName[1]
      
      seriesName <- RqCqDFSeries$seriesName[1]
      
      chartNames <- unique(RqCqDFSeries$chartName)
      
      eventNames <- unique(RqCqDFSeries$eventLabel)
    }
    
    #### call the algorithm ####
    
    OSS3ScoresFn(RqCqDFSeries=RqCqDFSeries,
                 OSS3Alpha2=OSS3Alpha,
                 OSS3DecisionRule="TSR",
                 minPresentations=2,
                 makeDF=FALSE,
                 saveCSV=FALSE,
                 analysisListName=analysisListName2 )
    
    
    
    
    
  } # end I loop over RqCqDFNames
  
  print(paste("Analysis completed for", length(RqCqDFNames), "series."))
  
  return(RqCqDFNames)
  
} # end runAlgorithmFn()



# call the function to run an algorithm

# runAlgorithmFn(x="RqCqDFSeries.csv", analysisListName="_OSS3_ANALYSIS")


###################3
