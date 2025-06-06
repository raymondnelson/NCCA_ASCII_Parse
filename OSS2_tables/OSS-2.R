# OSS-2 algorithm
# Raymond Nelson
# 6/18/2016


# load the OSS-2 ratios
# OSS2_RLE_table <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/OSS2/OSS2_RLE_table.csv", header=FALSE, stringsAsFactors=FALSE, col.names=c("ratio","score"))
# OSS2_EDA_table <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/OSS2/OSS2_EDA_table.csv", header=FALSE, stringsAsFactors=FALSE, col.names=c("ratio","score"))
# OSS2_CDO_table <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/OSS2/OSS2_CDO_table.csv", header=FALSE, stringsAsFactors=FALSE, col.names=c("ratio","score"))

OSS2_RLE_table <- cbind.data.frame(ratio=c(0.00, 0.78, 0.87, 0.94, 1.03, 1.11, 1.22), score=c(-3,-2,-1,0,1,2,3))
OSS2_EDA_table <- cbind.data.frame(ratio=c(0.00, 0.45, 0.70, 0.95, 1.27, 1.72, 2.60),  score=c(6,4,2,0,-2,-4,-6))
OSS2_CDO_table <- cbind.data.frame(ratio=c(0.00, 0.56, 0.74, 0.93, 1.14, 1.39, 1.81), score=c(3,2,1,0,-1,-2,-3))


# load the OSS-2 cutscore tables
# OSS2_DI_cutscores <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/OSS2/OSS2_DI_cutscores.csv", header=FALSE, col.names=c("alpha","cutscore"))
# OSS2_NDI_cutscores <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/OSS2/OSS2_NDI_cutscores.csv", header=FALSE, col.names=c("alpha","cutscore"))


OSS2_DI_cutscores <- cbind.data.frame(alpha=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.09, 0.11, 0.13, 0.16, 0.19, 0.22),
                                      cutscore=c(-22, -16, -12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8) )
OSS2_NDI_cutscores <- cbind.data.frame(alpha=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.09, 0.11, 0.13, 0.15, 0.17, 0.20, 0.23),
                                       cutcores=c(20, 16, 14, 12, 10,  8,  6,  4,  2,  0, -2, -4, -6, -8) )


# load the OSS-2 lookup table
# OSS2_lookup <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/OSS2/OSS2_lookup.csv", header=FALSE, col.names=c("score","innocentCDF","guiltyCDFINV"))

OSS2_DI_lookup <- cbind.data.frame(
  score=seq(from=-40, to=8, by=2), 
  CDF=c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.02, 0.03, 0.03, 0.04, 0.05, 0.06, 0.07, 0.09, 0.11, 0.13, 0.16, 0.19, 0.22))
OSS2_NDI_lookup <- cbind.data.frame(
  score=seq(from=-8, to=40, by=2),
  CDFINV=c(0.23, 0.20, 0.17, 0.15, 0.13, 0.11, 0.09, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.00) )


makeRCRatiosFn <- function(x=measurementDF) {
  # function to compute R/C ratios
  # x input is a data frame of measurements
  measurementDF <- x
  # loop over 3 charts to make a data frame of R/C ratios
  # initialize an output variable
  OSS2OutputDF <- NULL
  for (i in 1:3) {
    # get the current chart
    currentChartDF <- measurementDF[measurementDF$chartName==use.charts[i],]
    # get the RQs
    current.RQs <- which(substr(currentChartDF$eventName,1,1)=="R")
    names(current.RQs) <- currentChartDF$sensorName[current.RQs]
    # get the CQs
    current.CQs <- which(substr(currentChartDF$eventName,1,1)=="C")
    names(current.CQs) <- currentChartDF$sensorName[current.CQs]
    # initialize a vector of RQ tags for the current chart
    RQ.Labels <- as.character(currentChartDF$eventName[current.RQs])
    # compute the OSS-2 ratios
    OSS2.ratios <- currentChartDF$sensorMeasurement[current.RQs] / currentChartDF$sensorMeasurement[current.CQs]
    names(OSS2.ratios) <- names(current.RQs)
    # make the data frame
    resultDF <- cbind.data.frame(rep(paste0("Chart",i), times=length(OSS2.ratios)),
                                 c(rep("R1", times=length(unique(currentChartDF$sensorName))),
                                   rep("R2", times=length(unique(unique(currentChartDF$sensorName)))),
                                   rep("R3", times=length(unique(unique(currentChartDF$sensorName))))
                                 ),
                                 names(OSS2.ratios),
                                 OSS2.ratios, 
                                 stringsAsFactors=FALSE)
    colnames(resultDF) <- c("chartName", "eventName", "sensorName", "RCRatio")
    # bind the data frame to the output
    OSS2OutputDF <- rbind(OSS2OutputDF, resultDF)
  }
  # add a column to hold the OSS2 numerical score
  OSS2OutputDF$score <- ""
  # output
  return(OSS2OutputDF)
} # end makeRCRatiosFn()


# pneumo rows and pneumo scores

pneumoScoreFn <- function(x=OSS2OutputDF[pneumo.rows,"RCRatio"], tab=OSS2_RLE_table) {
  # function to lookup OSS2 numerical score for pneumo data
  # x is the R/C ratios for the upper and lower pneumo scores 
  # tab is the lookup table for OSS-2 scores
  # output is a vector the same length as x
  ###
  # initialize the output to zero values
  output <- numeric(length=length(x))
  for (i in 1:length(pneumo.rows)) {
    if(is.na(x[i])) next()
    # get the score
    output[i] <- tab$score[max(which(tab$ratio <= x[i]))]
  }
  return(output)
} # end pneumoScoreFn()


# EDA rows and EDA scores

edaScoreFn <- function(x=OSS2OutputDF[eda.rows,"RCRatio"], tab=OSS2_EDA_table) {
  # function to lookup OSS2 numerical score for EDA data
  # x is the R/C ratios for the EDA scores 
  # tab is the lookup table for OSS-2 scores
  # output is a vector the same length as x
  ###
  # initialize the output to zero values
  output <- numeric(length=length(x))
  for (i in 1:length(eda.rows)) {
    # get the score
    ifelse(is.na(x[i]),
           output[i] <- x[i],
           output[i] <- tab$score[max(which(tab$ratio <= x[i]))] )
  }
  return(output)
} # end edaScoreFn


# cardio rows and cardio scores

cardioScoreFn <- function(x=OSS2OutputDF[cardio.rows,"RCRatio"], tab=OSS2_CDO_table) {
  # function to lookup OSS2 numerical score for EDA data
  # x is the R/C ratios for the EDA scores 
  # tab is the lookup table for OSS-2 scores
  # output is a vector the same length as x
  ###
  # initialize the output to zero values
  output <- numeric(length=length(x))
  for (i in 1:length(cardio.rows)) {
    # get the score
    ifelse(is.na(x[i]),
           output[i] <- x[i],
           output[i] <- tab$score[max(which(tab$ratio <= x[i]))] )
  }
  return(output)
} # end cardioScoreFn


# combine the upper and lower pneumo scores
combinePneumosFn <- function(x=OSS2OutputDF) {
  # function to combine the upper and lower pneumo scores to a single score
  # x input is a data frame
  # output is a data frame with a single pneumo score
  ###
  # get the pneum rows
  upper.pneumo.rows <- which(x$sensorName=="UPneumo")
  lower.pneumo.rows <- which(x$sensorName=="LPneumo")
  # make a new column for the combined pneumo scores
  x$OSS2Score <- x$score 
  # remove the pneumo scores from the new column
  x$OSS2Score[upper.pneumo.rows] <- ""
  x$OSS2Score[lower.pneumo.rows] <- ""
  # loop over the data frame and combine the pneumo scores
  for (i in 1:length(upper.pneumo.rows)) {
    p.score <- ifelse(x[upper.pneumo.rows[i],"score"] * x[lower.pneumo.rows[i],"score"] < 0,
                      0,
                      ifelse(x[upper.pneumo.rows[i],"score"] + x[lower.pneumo.rows[i],"score"] >= 0, 
                             max(c(x[upper.pneumo.rows[i],"score"], x[lower.pneumo.rows[i],"score"])),
                             min(c(x[upper.pneumo.rows[i],"score"], x[lower.pneumo.rows[i],"score"])) ) )
    ifelse(x$score[upper.pneumo.rows[i]] == p.score,
           x$OSS2Score[upper.pneumo.rows[i]] <- p.score,
           x$OSS2Score[lower.pneumo.rows[i]] <- p.score )
  }
  x[,"OSS2Score"] <- as.numeric(x[,"OSS2Score"])
  # turn the data frame
  return(x)  
} # end combinePneumosFn
  

# make a score sheet table for all charts by sensor and event
makeOSS2ScoreTableFn <- function(x=OSS2OutputDF2) {
  # function to make a score sheet table of the OSS2 scores
  # x input is a data frame of OSS2 scores
  # output is a data frame for OSS2 scores by chart by sensor by event
  ###
  OSS2OutputDF2 <- x
  unique.charts <- unique(OSS2OutputDF2$chartName)
  # loop over the charts
  # first initialize the output variable
  OSS2ScoreTable <- NULL
  for (i in 1:length(unique.charts)) {
    use.rows <- OSS2OutputDF2$chartName==unique.charts[i]
    dat <- matrix(OSS2OutputDF2[use.rows,"OSS2Score"],byrow=FALSE, ncol=3)
    colnames(dat) <- c("R1", "R2", "R3")
    rownames(dat) <- unique(OSS2OutputDF2$sensorName)
    OSS2ScoreTable <- rbind(OSS2ScoreTable, dat)
  }
  return(OSS2ScoreTable)
} # end makeOSS2ScoreTableFn


###################################################


# set the working directory and load the measurement data frame
setwd("~/Dropbox/LAFAYETTE/OSS-2/Gustav 9.6.16")

# load the data
measurementDF <- read.csv("~/Dropbox/LAFAYETTE/OSS-2/Gustav 9.6.16/Gustav_9_6_16_Measurements.csv")

# initialize some variables with the id vectors in the measurement data frame
series.names <- unique(measurementDF$seriesName)
chart.names <- unique(measurementDF$chartName)
event.names <- unique(measurementDF$eventName)
sensor.names <- unique(measurementDF$sensorName)

# make the OSS-2 ratios
OSS2OutputDF <- makeRCRatiosFn(x=measurementDF)

# initialize some variable for the sensor indices
pneumo.rows <- which(OSS2OutputDF$sensorName=="UPneumo" | OSS2OutputDF$sensorName=="LPneumo")
eda.rows <- which(OSS2OutputDF$sensorName=="EDA")
cardio.rows <- which(OSS2OutputDF$sensorName=="Cardio")

# keep only the OSS-2 sensors
use.sensors <- c("UPneumo", "LPneumo", "EDA", "Cardio")
measurementDF <- measurementDF[measurementDF$sensorName %in% use.sensors,]

# keep on the useable charts
use.charts <- c("2_01A", "2_02A", "2_03A")
measurementDF <- measurementDF[measurementDF$chartName %in% use.charts,]

# make the OSS-2 numericall scores
OSS2OutputDF[pneumo.rows, "score"] <- pneumoScoreFn(x=OSS2OutputDF[pneumo.rows,"RCRatio"], tab=OSS2_RLE_table)
OSS2OutputDF[eda.rows, "score"] <- edaScoreFn(x=OSS2OutputDF[eda.rows,"RCRatio"], tab=OSS2_EDA_table)
OSS2OutputDF[cardio.rows, "score"] <- cardioScoreFn(x=OSS2OutputDF[cardio.rows,"RCRatio"], tab=OSS2_CDO_table)

# change the colum to numeric
OSS2OutputDF$score <- as.numeric(OSS2OutputDF$score)

# combine the upper and lower pneumo scores
OSS2OutputDF2 <- combinePneumosFn(x=OSS2OutputDF)

# save the scores as .csv
write.csv(OSS2OutputDF2, file="OSS2OutputDF.csv")

# calculate the OSS-2 grand total score
sum(OSS2OutputDF2$OSS2Score, na.rm=TRUE)
# [1] -25

# make another arrangement of the OSS-2 score table and save the result as .csv
OSS2ScoreTable <- makeOSS2ScoreTableFn(x=OSS2OutputDF2)
OSS2ScoreTable <- as.data.frame(OSS2ScoreTable)
write.csv(OSS2ScoreTable, file="OSS2ScoreTable.csv")


