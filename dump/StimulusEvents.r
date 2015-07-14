# r script to isolate and measure stimulus events of of time-series PDD data
# 2-4-2014 raymond.nelson
#
# requires conversion of the data form NCCA ASCII output to .csv format 
# use parseFilesCSV.r to convert the data
# 
# start by reading the .csv after conversion from the ASCII NCCA output
# dataCSV <- read.csv("D&-20131002 DLSC YANGO ARCIA-1.01A_data.csv", nrows = -1)
dataCSV <- read.csv("D&-09N-0609-2.01A_data.csv", nrows = -1)
#
# define the measurement space
sampleRate <- 30L # sampling rate per second
measurementPeriod <- 25L # number of seconds
segmentLength <- sampleRate * measurementPeriod
EDALatency <- .5 # required latency for response onset 
responseOnsetWindow <- 5L # defines the latest x point of response onset as the number of seconds after answer 
#
# dataTimeSeriesIndex <- dataCSV[[1]] # commented out 2-7-14 because it is not needed. each column is already indexed
dataRecordingTime <- dataCSV[[2]] # make a vector from from column 2 timescale
dataStimulusEvents <- dataCSV[[3]] # make a vector from column 3 stimulus events
stimulusEventList <- levels(dataStimulusEvents) # make a vector of unique stim event lables
dataPneumoUpper <- dataCSV[[4]] # upper pneumo data
dataPneumoLower <- dataCSV[[5]] # lower pneumo data
dataEDA <- dataCSV[[6]] # electrodermal data
dataCardio <- dataCSV[[7]] # cardio data
#
# remove answers and announcements from the event list
if("No" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("No", stimulusEventList)]
if("Yes" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("Yes", stimulusEventList)]
if("" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("", stimulusEventList)]
if("xx" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("xx", stimulusEventList)]
if("x" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("x", stimulusEventList)]
if("XX" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("XX", stimulusEventList)]
if("X" %in% stimulusEventList) stimulusEventList <- stimulusEventList[-match("X", stimulusEventList)]
#
# remove neutral, sacrifice, introductory, and outside issue ("symptomatic") questions
if (length(grep("n", stimulusEventList, ignore.case = TRUE)) > 0) stimulusEventList <- stimulusEventList[-grep("n", stimulusEventList, ignore.case = TRUE)]
if (length(grep("i", stimulusEventList, ignore.case = TRUE)) > 0) stimulusEventList <- stimulusEventList[-grep("i", stimulusEventList, ignore.case = TRUE)]
if (length(grep("s", stimulusEventList, ignore.case = TRUE)) > 0) stimulusEventList <- stimulusEventList[-grep("s", stimulusEventList, ignore.case = TRUE)]
if (length(grep("o", stimulusEventList, ignore.case = TRUE)) > 0) stimulusEventList <- stimulusEventList[-grep("o", stimulusEventList, ignore.case = TRUE)]
#
# retain relevant and comparison stimulus events
if (length(grep("r", stimulusEventList, ignore.case = TRUE)) > 0) stimulusEventListRelevant <- stimulusEventList[-grep("r", stimulusEventList, ignore.case = TRUE)]
if (length(grep("c", stimulusEventList, ignore.case = TRUE)) > 0) stimulusEventListComparison <- stimulusEventList[-grep("c", stimulusEventList, ignore.case = TRUE)]
#
# concatenate the list of relevant and comparison stimuli
stimulusEventList <- c(stimulusEventListComparison, stimulusEventListRelevant)
# 
# # determine the number of stim events # commented out 2-6-14 because a variable is not needed for this
# numberStimulusEvents <- length(stimulusEventList) # a variable to represent the number of unique stim events
# 
# locate the onset row for each stimulus event, and resort the stimulus events into the order of presentation
stimulusOnsetRow <- match(stimulusEventList, dataStimulusEvents) # onset row for each stim event
sort(stimulusOnsetRow) # re-sort the stim events by sequence of occurence - necessary because the list of unique events is alphabetical
# 
# construct an index vector for each stimulus event # commented out 2-6-14 because it is not necessary
# for (i in 1:length(stimulusEventList)) {
#   assign(paste("segment", i, sep = ""), seq(stimulusOnsetRow[i],stimulusOnsetRow[i] + segmentLength)) 
# }
#
# create separate data vectors for each stimulus segment, using the index value to locate the data in stimulusEvents vector
for (i in 1:length(stimulusOnsetRow)) {
  assign(paste("segment", i, "dataPneumoUpper", sep = ""), 
         dataPneumoUpper[seq(stimulusOnsetRow[1],stimulusOnsetRow[1] + segmentLength -1)]
         )
  assign(paste("segment", i, "dataPneumoLower", sep = ""), 
         dataPneumoUpper[seq(stimulusOnsetRow[1],stimulusOnsetRow[1] + segmentLength -1)]
  )
  assign(paste("segment", i, "dataEDA", sep = ""), 
         dataPneumoUpper[seq(stimulusOnsetRow[1],stimulusOnsetRow[1] + segmentLength -1)]
  )
  assign(paste("segment", i, "dataCardio", sep = ""), 
         dataPneumoUpper[seq(stimulusOnsetRow[1],stimulusOnsetRow[1] + segmentLength -1)]
  )
}
#
#
