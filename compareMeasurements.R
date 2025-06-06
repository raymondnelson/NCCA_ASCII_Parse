# R script to compare the Kircher measurements using 2 different measurement rules
# 10-13-2015
# Raymond Nelson
#
####################################

# file names are coded for 1) strict ROW, 2) long response, 3) descent stop

# measurement files are .csv ending with "_measurements.csv"
myMeasurementFiles <- list.files(pattern="*_measurements.csv")


# read all the data
for (i in 1:length(myMeasurementFiles)) {
  assign(str_sub(myMeasurementFiles[i], 1, -5), read.csv(myMeasurementFiles[i], stringsAsFactors = FALSE))
}

# get the names of the exams without the file name appendage
myMeasurementFiles2 <- unique(str_sub(myMeasurementFiles, 1, -33))

# get the file name appendages for the different measurement conditions
myDifferentMeasurements <- unique(str_sub(myMeasurementFiles, -32, -5))


# make a vector of exams for which different rules produce different measurements
# first make a NULL vector to hold the result
isDifferent <- NULL
for (i in 1:length(myMeasurementFiles2)) {
  measure1 <- get(paste0(myMeasurementFiles2[i], myDifferentMeasurements[1]))['sensorMeasurement']
  measure2 <- get(paste0(myMeasurementFiles2[i], myDifferentMeasurements[2]))['sensorMeasurement']
  if( !identical(measure1, measure2) ) { isDifferent <- c(isDifferent, myMeasurementFiles2[i]) }
}

# make a data frame of segments for which different rules produce different measurements
# First make a NULL data frame to hold the result
# myDF <- cbind.data.frame(get(paste0(myMeasurementFiles2[i], myDifferentMeasurements[1]))[,1:5],measure1, measure2)

  diffSegmentsDF <- NULL
for (j in 1:length(isDifferent)) {
  measure1 <- get(paste0(isDifferent[j], myDifferentMeasurements[1]))['sensorMeasurement']
  measure2 <- get(paste0(isDifferent[j], myDifferentMeasurements[2]))['sensorMeasurement']
  
  
  # make a data frame
  myMeasurements <- cbind.data.frame(measure1, measure2)
  names(myMeasurements) <- c("measure1", "measure2")
  # make a function to apply
  compareMeasurements <- function(x) { x[1] != x[2] }
  myDiffs <- which(apply(myMeasurements, 1, compareMeasurements))
  
  # make a data frame of segments for which different rules produce different measurements
  # first make a NULL data frame to hold the result
  
  for (k in 1:length(myDiffs)) {
    diffSeg <- cbind.data.frame(get(paste0(isDifferent[j], myDifferentMeasurements[1]))[myDiffs[k],1:5],measure1[myDiffs[k],], measure2[myDiffs[k],])
    # diffSegmentsDF[1,] <- diffSeg[1,1:7]
    diffSegmentsDF <- rbind(diffSegmentsDF, diffSeg)
    }
}
names(diffSegmentsDF) <- c("examName", "seriesName", "chartName", "eventName", "sensorName", "measure1", "measure2")



# 
# 
# 
# isTRUE(all.equal(measure1, measure2))
# 
# which(measure1 != measure2)






