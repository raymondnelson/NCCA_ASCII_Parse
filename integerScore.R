# R function to get RQ and CQ measurements from the measurment DF
# Raymond Nelson
# 10-25-2016


##########################
source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)



rankScoreFn <- function(x=examName, y=seriesName, z=chartName, makeDF=FALSE, output=FALSE) {
  # function to compute the R/C Ratio and scores from the measurement data frome
  # x input is a vector of unique exam names for exams in the global environment
  # 

  uniqueExams <- x
  seriesName <- y
  chartName <- z
  
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Measurements", "*")
  
  measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  # assign("measurementDF", measurementDF, pos=1)
  
  # keep only the measurements for the selected series and chart
  selectRows <- which(measurementDF$seriesName==seriesName & measurementDF$chartName==chartName)
  chartMeasurementDF <- measurementDF[selectRows,]
  
  # make a data frame of RQs and a data frame of CQs
  rqRows <- grep("R", chartMeasurementDF$eventName)
  rqDF <- chartMeasurementDF[rqRows,]
  cqRows <- grep("C", chartMeasurementDF$eventName)
  cqDF <- chartMeasurementDF[cqRows,]
  
  # make a data frame of RQs and CQs together
  rqcqRows <- grep("[CR]+", chartMeasurementDF$eventName)
  rqcqDF <- chartMeasurementDF[rqcqRows,]
  
  # remove NA rows where there is no sensor measurement
  # rqcqDF <- rqcqDF[!is.na(rqcqDF$sensorMeasurement),]
  
  rank(chartMeasurementDF[rqcqRows,'sensorMeasurement'])
  
  
   # make a vector of unique RQs
  uniqueRQs <- unique(rqDF$eventName)
  
  # and another vector of unique CQs
  uniqueCQs <- unique(cqDF$eventName)
  
  # make a vector of unique sensor names
  # uniqueSensors <- unique(str_sub(names(RQs), (str_locate(names(myEvents), "_")[,1]+1), -1))
  uniqueSensors <- as.character(unique(measurementDF$sensorName))
  
  # set the constraints
  posPneumoHigh <- log(1.5)
  posPneumoLow <- log(1.25)
  negPneumoHigh <- -log(1.5)
  negPneumoLow <- -log(1.05)
  posEDAHigh <- log(3)
  posEDALow <- log(1.05)
  negEDAHigh <- -log(3)
  negEDALow <- -log(1.05)
  posCardioHigh <- log(3)
  posCardioLow <- log(1.05)
  negCardioHigh <- -log(3)
  negCardioLow <- -log(1.05)
  posPLEHigh <- log(3) 
  posPLELow <- log(1.1)
  negPLEHigh <- -log(3)
  negPLELow <- -log(1.1)
  
  
  # iterate over the sensors and calculate the integer score
  for (i in length(uniqueSensors)) {
    if(uniqueSensors[i] == "UPneumo" | "LPneumo") {
      ifelse(chartMeasurementDF$score >= posPneumoLow & chartMeasurementDF$score <= posPneumoHigh,
             chartMeasurementDF$
             
    } else if(uniqueSensors[i] == "EDA") {
      
    } else if(uniqueSensors[i] == "Cardio") {
      
    } else if(uniqueSensors[i] == "FC") {
      
    } else if(uniqueSensors[i] == "eCardio") {
      
    } else if(uniqueSensors[i] == "PLE") {
      
    } else {
      
    }
  }
  
  
  
  
  
  
  # calculate the average CQs measurement for each sensor
  cqMeans <- rep("", times=length(uniqueSensors))
  for (m in 1:length(uniqueSensors)) {
    cqMeans[m] <- mean(cqDF[cqDF$sensorName==uniqueSensors[m],"sensorMeasurement"])
  }
  cqMeans <- as.numeric((cqMeans))
  names(cqMeans) <- uniqueSensors
  
  # loop over the RQs and calculate the RC ratio
  for (m in 1:length(uniqueRQs)) {
    # and also loop over the sensors
    for (n in 1:length(uniqueSensors)) {
      # first get the row indices for the RQ and sensor
      rowSelect <- rqDF$eventName==uniqueRQs[m] & rqDF$sensorName==uniqueSensors[n]
      # then calculate the RQ / CQmean ratio
      loggedRatio <- log( rqDF[rowSelect,'sensorMeasurement'] / cqMeans[n] )
      if(loggedRatio)
      rqDF[rowSelect,'integerScore'] <- 
    }
  }
  
  
  
  ifelse(rqDF[rowSelect,'integerScore'] > 0,
         ifel)
  
  # may need to invert the pneumo sign value
  
  for (m in 1:length(uniqueSensors)) {
    rowSelect <- rqcqDF$sensorName==uniqueSensors[m]
    rqcqDF[rowSelect,'rankScore'] <- rank(rqcqDF[rowSelect,'sensorMeasurement'], ties.method="average", na.last="keep")
  }
  
  # pass the rqDF back to the measurement DF and save to the global environment
  chartMeasurementDF[rqRows,] <- rqDF
  
  # assign(paste0("chartMeasurementDF",i), chartMeasurementDF,pos=1)
  
  measurementDF[selectRows,] <- chartMeasurementDF
  
  # assign the measurement DF to the original object name in the global invironment
  if(makeDF==TRUE) {
    assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)
  }
    
  # function output
  if(output == TRUE) return(measurementDF)

} # end rankScoreFn function



myExamFUN <- function() {print("ray")}
mySeriesFUN <- function() {print("irv")}
myChartFUN <- function() {print("nel")}
mySegmentFUN <- function() {print("rin")}


getExamFn(x=uniqueExams,
          examNum="ALL",
          seriesNum="ALL",
          chartNum="ALL",
          segmentNum="ALL",
          examFUN="myExamFUN",
          seriesFUN="mySeriesFUN",
          chartFUN="integerScoreFn",
          segmentFUN="mySegmentFUN",
          showNames=TRUE,
          output=FALSE )
# pass the name of the function as a string 



