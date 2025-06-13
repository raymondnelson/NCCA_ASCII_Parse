#########################################################
# R script to locate charts with Relevant and Comparison stimuli 
# and compare the order of presentation
# working 2-24-2014
# 
#
# this script will assemble a collection of stimulus events from all charts for an exam
#
#########################################################
#
# Get a list of file names for all charts
chartNames <- list.files(path = ".", pattern = "*events.csv", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE)
#
# number of charts
numberCharts  <- length(chartNames) 
#
# define some variables for later use - any variable used in a formula needs to be defined first
# eventLabelsMatrix <- NULL
maxEvents <- 0
numberEvents <- NULL
eventListNames <- NULL
#
# a loop to read the data from all CSV files in the eventList
for (i in 1:numberCharts) {
  # CSVName <- paste("CSVEvents", "1", sep = "")
  CSVName <- paste("CSVEvents", i, sep = "")
  tempCSV <- assign(CSVName, read.csv(chartNames[i]))
  # tempCSV <- assign(CSVName, read.csv(chartNames[1]))
  #
  # a nested loop to make a list of event lables for each chart
  eventLabels <- NULL
  for (j in 1:nrow(tempCSV)) {
    eventLabels <- c(eventLabels, as.character(tempCSV[j,2]))
    # eventLabels <- c(eventLabels, as.character(tempCSV[1,2]))
    } # end of loop to read event labels for each chart
  #
  # remove "" elements
  eventLabels <- eventLabels[which(eventLabels != "")]
  # determine the number of events and concatenate it to a vector
  numberEvents <- as.character(c(numberEvents, length(eventLabels)))
  # create the unique eventList name
  eventListName <- paste("eventList", i, sep = "")
  # assign the events to a unique vector
  assign(eventListName, eventLabels)
  # make a vector of unique event list names
  eventListNames <- c(eventListNames, eventListName)                   
} # end of loop to read each *_events.csv file for events 
#
# determine the max length of the eventList for all charts
maxEvents <- max(as.integer(numberEvents))
#
#
numberEventLists <- length(eventListNames)
# a loop to fix the length of all event lists to the max length
for (k in 1:numberEventLists) {
  # need to use each element of numberEventLists as the name of a vector 
  tempList <- get(eventListNames[k])
  tempList <- c(tempList, rep("-", maxEvents - length(tempList)))
  assign(paste("eventList", k, sep = ""), tempList)
}
#
# retain charts that include CQs and RQs
retainEventListNames <- NULL
for (l in 1:length(eventListNames)) {
  eventListName <- paste("eventList", l, sep = "")
  RQs <- length(grep("R", get(eventListName), 
                     ignore.case = TRUE, 
                     perl = FALSE, 
                     value = FALSE,
                     fixed = FALSE, 
                     useBytes = FALSE, 
                     invert = FALSE)) >= 2 
  #
  CQs <- length(grep("C", get(eventListName), 
                     ignore.case = TRUE, 
                     perl = FALSE, 
                     value = FALSE,
                     fixed = FALSE, 
                     useBytes = FALSE, 
                     invert = FALSE)) >= 2
  #
  if (CQs & RQs) retainEventListNames <- c(retainEventListNames, eventListName)
}
#
# make a list of unique event names
# eventListNames <- ls(pattern = "eventList?")
allEventNames <- c(sapply(retainEventListNames, get))
uniqueEventNames <- unique(allEventNames)
#
# make a combined matrix of event labels for charts with RQs and CQs
numberCharts <- length(retainEventListNames)
eventMatrix <- NULL
for (m in 1:numberCharts) {
  #nested loop to verify each event in each chart
  numberEvents <- length(uniqueEventNames)
  eventVector <- NULL
  for (n in 1:numberEvents) {
    includedEvents <- which(uniqueEventNames[n] == get(retainEventListNames[m]))
    eventVector <- c(eventVector, includedEvents)
    # eventMatrix <- rbind(eventMatrix, includedEvents)
  } 
  eventMatrix <- rbind(eventMatrix, eventVector)
}
# set the column and row names for the matrix that describes the sequence of stimulus presentation
rownames(eventMatrix) <- retainEventListNames
colnames(eventMatrix) <- uniqueEventNames
# set the CSV file name
CSVName <- paste(strtrim(chartNames[1], nchar(chartNames[1]) - 17), "_eventMatrix.csv", sep = "")
# no need to assign a unique name because this will need to be done for all exam
# assign(CSVName, eventMatrix) # using assign() allows the use of the variable name that is held by another variable
# need to save the CSV with a unique name for each exam
# save the .csv file 
write.table(eventMatrix, file = CSVName, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\n", 
            na = "NA", 
            dec = ".", 
            row.names = FALSE, 
            col.names = TRUE, 
            qmethod = "double", 
            fileEncoding = "UTF-8")
#
# eventMatrix
#
# rm(chartNames)
# rm(retainEventlistNames)
# rm(list = ls(pattern = "CSVEvents*"))
#
# clean up
#
rm(tempCSV)
rm(i)
rm(j)
rm(k)
rm(l)
rm(m)
rm(n)
rm(CQs)
rm(RQs)
rm(allEventNames)
rm(tempList)
rm(CSVName)
rm(eventLabels)
rm(eventListName)
rm(eventListNames)
rm(maxEvents)
rm(numberCharts)
rm(numberEventLists)
rm(numberEvents)
rm(includedEvents)
rm(uniqueEventNames)
rm(eventVector)
#
################### END ################

