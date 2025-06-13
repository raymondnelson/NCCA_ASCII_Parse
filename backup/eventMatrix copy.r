#########################################
#
# R script to make a csv table of all stimulus events for all charts
# 
# will also parse the total unique events and missing events 
# 
# x needs to be the name of a character vector 
# containing the names of "*_events.csv" files
#
# input is a list of events.csv files
#
# output is a table matrix (.csv) of all events for all charts 
# within an exam
#
# modified 4-6-2014 from the eventMatrix function
#
###########################################
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#
setwd("~/Documents/R programming/Recursion")
#
# make a list of all events.csv files
x = "_events.csv$"
allCharts <- list.files(path = ".", pattern = x, 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE)
#
# Make a list of unique PF series names
uniqueSeries <- unique(str_sub(allCharts, 1L, -16L))
#
# Make a loop to cycle through each Unique Series
for (h in 1:length(uniqueSeries)) {
  #
  # make a vector of names of all charts for each unique series  
  name <- paste(uniqueSeries[i], "....", "_events.csv", sep = "")
  # name <- paste(uniqueSeries[1], "....", "_events.csv", sep = "")
  eventNames <- list.files(path = ".", pattern = name, 
                           all.files = FALSE,
                           full.names = FALSE, 
                           recursive = FALSE,
                           ignore.case = FALSE, 
                           include.dirs = FALSE)
  #
  # define some variables for later use - any variable used in a formula needs to be defined first
  # eventLabelsMatrix <- NULL
  maxEvents <- 0
  numberEvents <- NULL # the number of stim events in each chart
  eventListNames <- NULL
  # retainChartNames <- NULL
  #
  # a loop to read the info from each CSV files in the eventNames
  for (i in 1:length(eventNames)) {
    # tempName is a holder for each series name
    tempName <- eventNames[i]
    # tempName <- eventNames[1]
    #
    # tempCSV is a holder for the events.csv data
    tempCSV <- read.csv(tempName)
    #
    eventLabels <- NULL
    # a nested loop to make a vector of event lables for each chart
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
    eventListName <- tempName
    # eventListName <- paste("eventList", i, sep = "")
    # eventListName <- eventNames[i]
    # assign the events to a unique vector
    assign(eventListName, eventLabels)
    # make a vector of unique event list names
    eventListNames <- c(eventListNames, eventListName)
  } # end of loop to read each "_events.csv$" file for events
    # for each unique series
  #
  # determine the max length of the eventList for all charts
  # within each series
  maxEvents <- max(as.integer(numberEvents))
  numberEventLists <- length(eventListNames)
  #
  # a loop to fix the length of all event lists to the max length 
  # for each exam
  for (k in 1:numberEventLists) {
    # need to use each element of numberEventLists as the name of a vector 
    tempList <- get(eventListNames[k])
    tempList <- c(tempList, rep("-", maxEvents - length(tempList)))
    assign(paste("eventList", k, sep = ""), tempList)
    assign(eventListNames[k], get(paste("eventList", k, sep = ""))) ### added 3-23
  } # end loop to fix the length of all event lists to the max for each exam
  # 
  retainEventListNames <- NULL
  allEventNames <- NULL
  # a loop to retain charts that include min 2 CQs and min 2 RQs
  for (l in 1:numberEventLists) {
    eventListName <- eventNames[l] ## vector of events.csv$ file names
    RQs <- length(grep("R", get(eventListName), 
                       ignore.case = TRUE, 
                       perl = FALSE, 
                       value = FALSE,
                       fixed = FALSE, 
                       useBytes = FALSE, 
                       invert = FALSE)) >= 2 
    #
    #
    CQs <- length(grep("C", get(eventListName), 
                       ignore.case = TRUE, 
                       perl = FALSE, 
                       value = FALSE,
                       fixed = FALSE, 
                       useBytes = FALSE, 
                       invert = FALSE)) >= 2
    #
    #
    if (CQs & RQs) {
      eventListName <- paste("eventList", l, sep = "") # change the eventListName to get the padded list 
      retainEventListNames <- c(retainEventListNames, eventNames[l])    
      # make a long vector of all events from all charts with CQs and RQs
      allEventNames <- c(allEventNames, get(eventListName))
      # allEventNames <- c(allEventNames, get(paste("eventList", 1, sep= "")))
    }
  } # end loop retain charts with min 2 CQs and min 2 RQs
  #
  eventMatrix <- NULL
  # numberCharts <- length(retainEventListNames)
  #
  # make a vector of unique event names
  uniqueEventNames <- unique(allEventNames)
  numberEvents <- length(uniqueEventNames)
  # 
  # make a combined matrix of event labels for charts with RQs and CQs
  for (m in 1:length(retainEventListNames)) {
    # nested loop to verify each event in each chart
    eventVector <- NULL
    for (n in 1:numberEvents) {
      includedEvents <- which(uniqueEventNames[n] == get(retainEventListNames[m]))
      eventVector <- c(eventVector, includedEvents)
      # eventMatrix <- rbind(eventMatrix, includedEvents)
    } 
    #
    # construct the eventMatrix
    eventMatrix <- rbind(eventMatrix, eventVector)
    #
    # retainChartNames <- tempName
    # retainChartNames <- c(retainChartNames, tempName)
    #
  }
  #
  # define the table name
  retainEventListNamesCSV <- paste(strtrim(eventNames[1], nchar(eventNames[1]) - 17), "_CQTcharts.csv", sep = "")
  # write the .csv table of events for each unique series
  write.table(retainEventListNames, file = retainEventListNamesCSV, 
              append = FALSE, 
              quote = TRUE, 
              sep = ",", 
              eol = "\n", 
              na = "NA", 
              dec = ".", 
              row.names = FALSE, 
              col.names = FALSE, 
              qmethod = "double", 
              fileEncoding = "UTF-8")
  #
  #
  # set the column and row names for the matrix that describes the sequence of stimulus presentation
  rownames(eventMatrix) <- retainEventListNames
  # remove the padding event holders
  uniqueEventNames <- uniqueEventNames[c(which(uniqueEventNames != "-"))]
  colnames(eventMatrix) <- uniqueEventNames
  # set the CSV file name
  CSVName <- paste(strtrim(eventNames[1], nchar(eventNames[1]) - 17), "_eventMatrix.csv", sep = "")
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
  # rm(list = eventNames)
  #
# Next PF
} # end of loop for each unique series



# } # modified from eventMatrix function 4-6-2014
#
