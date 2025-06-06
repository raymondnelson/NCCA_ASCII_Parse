# R function to get RQ and CQ measurements from the measurment DF
# Raymond Nelson
# 10-25-2016



##########################
source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)



rankScoreFn <- function(x=examName, 
                        y=seriesName, 
                        z=chartName, 
                        makeDF=FALSE, 
                        output=FALSE) {
  # function to compute the R/C Ratio and scores from the measurement data frome
  # x input is a vector of unique exam names for exams in the global environment
  # 
  # x input is a scalar with a unique exam name
  # this function does not iterate over a vector of exam names
  # y input is a scalar with the series name
  # z input is a scalar with the chart name
  # makeDF=TRUE will assign the funtion output to a data frame in the global env as a side effect
  # output=TRUE will output the measurement data frame in the normal manner
  
  examName <- x
  seriesName <- y
  chartName <- z
  
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Measurements", "*")
  
  measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  # measurmentDF includes these colums 
  # [1] "examName"          "seriesName"        "chartName"         "eventName"        
  # [5] "sensorName"        "sensorMeasurement" "rankScore"         "score"            
  # [9] "integerScore" 
  
  # assign("measurementDF", measurementDF, pos=1)
  
  # keep only the measurements for the selected series and chart
  selectRows <- which(measurementDF$seriesName==seriesName & measurementDF$chartName==chartName)
  chartMeasurementDF <- measurementDF[selectRows,]
  
  # make a data frame of RQs and CQs together
  workingRows <- grep("[CR]+", chartMeasurementDF$eventName)
  # use all events if no RQs and CQs
  if(length(workingRows) == 0) {
    workingRows <- which(!(chartMeasurementDF$eventName %in% excludeEvents))
    # exclude the first and last event
    workingEvents <- unique(chartMeasurementDF$eventName[workingRows])
    workingEvents <- workingEvents[-c(1,length(workingEvents))]
    workingRows <- which(chartMeasurementDF$eventName %in% workingEvents)
  }
  
  # initiate the working data frame
  workingDF <- chartMeasurementDF[workingRows,]
  
  # calculate the rank scores for each sensor
  for (m in 1:length(uniqueSensors)) {
    rowSelect <- workingDF$sensorName==uniqueSensors[m]
    workingDF[rowSelect,'rankScore'] <- rank(workingDF[rowSelect,'sensorMeasurement'], 
                                             ties.method="average", 
                                             na.last="keep")
  }
  
  # pass the workingDF back to the measurement DF and save to the global environment
  chartMeasurementDF[workingRows,] <- workingDF
  
  # assign(paste0("chartMeasurementDF",i), chartMeasurementDF,pos=1)
  
  # pass the chartMeasurementDF back to the measurementDF
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



# pass the name of the function as a string 
getExamFn(x=uniqueExams,
          examNum="ALL",
          seriesNum="ALL",
          chartNum="ALL",
          segmentNum="ALL",
          showNames=TRUE,
          output=FALSE,
          examFUN="myExamFUN",
          seriesFUN="mySeriesFUN",
          chartFUN="rankScoreFn",
          segmentFUN="mySegmentFUN" )






