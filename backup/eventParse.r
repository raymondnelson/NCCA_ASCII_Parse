############# R Functions for parsing NCCA ASCII header information ##############
# 3-10-2014 Raymond Nelson
#
#
# this script contains the following functions
#
# eventCSVNames()
# to make a character vector of the names of "*events.csv" files
#
# eventMatrix()
# to make a csv table of the stimlus events for all charts
# also make a csv table of the names of charts containing CQs and RQs
#
#########################################


library(stringr)


##########


eventCSVNames <- function(x = "*_events.csv$") {
  # function to make a character vector of the filenames 
  # for all *events.csv" files
  # input is the string that identifies the events.csv files
  x <- x
  y <- list.files(path = ".", 
                  pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE)
  assign("eventFiles", y, pos = 1)
  return(y)
} # eventCSVNames end

eventFiles <- eventCSVNames()


#########################################



uniqueExamsE <- function(x = eventFiles) {
  # function to remove the "_events.csv" and other suffix info from each file name
  # to identify unique exams
  # input is the output vector eventFiles that contains the names of events.csv files
  y <- str_replace_all(x, "*_events.csv$", "")
  y <- str_sub(y, 1, nchar(y) -6)
  y <- unique(y)
  return(y)
}

uniqueExams <- uniqueExamsE()


#########################################



eventMatrix <- function(x = "*_events.csv$") {
  # function to make a csv table of all stimulus events for all charts for each exam
  # will also parse the total unique events and missing events 
  # x needs to be the name of a character string to identify the events.csv files 
  #
  eventFiles <- list.files(path = ".", pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE)
  # assign("eventFiles", eventFiles, pos = 1)
  #
  # get the vector of file names for events
  # eventFiles <- get(eventFiles, pos = 1)
  #
  # a loop to read the info from all CSV files in the eventVector
  numberEvents <- NULL # the number of stim events in each chart in the loop
  eventVectorNames <- NULL # used in the following loop
  for (i in 1:length(eventFiles)) {
    fileName <- eventFiles[i]
    CSVdata <- read.csv(fileName)
    # a nested loop to make a vector of event lables for each chart
    eventLabels <- NULL
    for (j in 1:nrow(CSVdata)) {
      eventLabels <- c(eventLabels, as.character(CSVdata[j,2]))
    } # end of nested loop to read event labels for each chart
    # remove "" elements
    eventLabels <- eventLabels[which(eventLabels != "")]
    # determine the number of events and concatenate it to a vector
    numberEvents <- as.character(c(numberEvents, length(eventLabels)))
    # create the unique eventVector name
    eventVectorName <- str_sub(fileName, 1, nchar(fileName) - 4)
    # eventVectorName <- paste("eventVector", i, sep = "")
    # eventVectorName <- eventFiles[i]
    # assign the events to a unique vector
    assign(eventVectorName, eventLabels)
    # make a vector of unique event vector names
    eventVectorNames <- c(eventVectorNames, eventVectorName)
  } # end of loop to read each "_events.csv$" file for events 
  #
  # determine the max length of the eventVector for all charts
  # maxEvents <- 0
  maxEvents <- max(as.integer(numberEvents))
  numberEventVectors <- length(eventVectorNames)
  #
  # a loop to fix the length of all event vectors to the max length for each exam
  for (k in 1:numberEventVectors) {
    tempVector <- get(eventVectorNames[k])
    tempVector <- c(tempVector, rep("-", maxEvents - length(tempVector)))
    # assign(paste("eventVector", k, sep = ""), tempVector)
    assign(eventVectorNames[k], tempVector)
    # maybe write the csv to include the padded events
  } # end loop to fix the length of all event vectors to the max for each exam
  #
  # 
  
  
  ############## maybe move the rest of this to a separate function
  
  
  # a loop to retain charts that include min 2 CQs and min 2 RQs
  retainEventVectorNames <- NULL
  allEventNames <- NULL
  for (l in 1:numberEventVectors) {
    eventVectorName <- eventVectorNames[l] 
    RQs <- length(grep("R", get(eventVectorName), 
                       ignore.case = TRUE, 
                       perl = FALSE, 
                       value = FALSE,
                       fixed = FALSE, 
                       useBytes = FALSE, 
                       invert = FALSE)) >= 2 # make a logical scalar for RQs >= 2
    #
    CQs <- length(grep("C", get(eventVectorName), 
                       ignore.case = TRUE, 
                       perl = FALSE, 
                       value = FALSE,
                       fixed = FALSE, 
                       useBytes = FALSE, 
                       invert = FALSE)) >= 2 # make a logical scalar for CQs >= 2
    #
    if (CQs & RQs) {
      # eventVectorName <- paste("eventVector", l, sep = "") # change the eventVectorName to get the padded vector 
      retainEventVectorNames <- c(retainEventVectorNames, eventVectorNames[l])    
      # make a long vector of all events from all charts with CQs and RQs
      allEventNames <- c(allEventNames, get(eventVectorName))
      # allEventNames <- c(allEventNames, get(paste("eventVector", 1, sep= "")))
    }
  } # end loop retain charts with min 2 CQs and min 2 RQs
  #
  #

  
   # numberCharts <- length(retainEventVectorNames)
  #
  # make a vector of unique event names
  uniqueEventNames <- unique(allEventNames)
  numberUniqueEvents <- length(uniqueEventNames)
  # 
  # loop to make a combined matrix of event labels for charts with >=2 RQs and >= 2 CQs
  eventMatrixOut <- NULL
  for (m in 1:length(retainEventVectorNames)) {
    # nested loop to verify each event in each chart
    eventVector <- NULL
    for (n in 1:numberUniqueEvents) {
      includedEvents <- which(uniqueEventNames[n] == get(retainEventVectorNames[m]))
      eventVector <- c(eventVector, includedEvents)
      # eventMatrix <- rbind(eventMatrix, includedEvents)
    } 
    #
    # construct the eventMatrix
    eventMatrixOut <- rbind(eventMatrixOut, eventVector)
    #
    # retainChartNames <- tempName
    # retainChartNames <- c(retainChartNames, tempName)
    #
  }
  #
  
  
  retainEventVectorNamesCSV <- paste(strtrim(eventFiles[1], nchar(eventFiles[1]) - 17), "_CQTcharts.csv", sep = "")
  write.table(retainEventVectorNames, file = retainEventVectorNamesCSV, 
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
  rownames(eventMatrixOut) <- retainEventVectorNames
  # remove the padding event holders
  uniqueEventNames <- uniqueEventNames[c(which(uniqueEventNames != "-"))]
  colnames(eventMatrixOut) <- uniqueEventNames
  # set the CSV file name
  CSVName <- paste(strtrim(eventFiles[1], nchar(eventFiles[1]) - 17), "_eventMatrix.csv", sep = "")
  # no need to assign a unique name because this will need to be done for all exam
  # assign(CSVName, eventMatrix) # using assign() allows the use of the variable name that is held by another variable
  # need to save the CSV with a unique name for each exam
  # save the .csv file 
  write.table(eventMatrixOut, file = CSVName, 
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
  # rm(list = eventFiles)
  #
} # eventMatrix end

# eventMatrix()

