############# R Functions for parsing NCCA ASCII header information ##############
# 3-10-2014 Raymond Nelson
# 6-12-2015
#
#
# this script contains the following functions
#
# eventCSV()
# to make a character vector of the names of "*events.csv" files
#
# uniqueExams()
# to make a character vector of the names of unique exams
#
# eventMatrix()
# to make a csv table of the stimlus events for all charts
# also make a csv table of the names of charts containing CQs and RQs
#
#########################################


library(stringr)


eventCSV <- function(x = "*_events.csv$") {  
  # function to make a vector of .csv files with events data
  # input x is a character string that identifies the events data files in the working directory
  # output is a vector of .csv names
  eventFiles <- list.files(path = ".", pattern = x, 
                           all.files = FALSE,
                           full.names = FALSE, 
                           recursive = FALSE,
                           ignore.case = FALSE, 
                           include.dirs = FALSE)
  return(eventFiles)  
}

# eventFiles <- eventCSV()



#########################

##########################


uniqueExamsFn <- function(x = eventFiles) {
  # function to make a vector of unique exams
  # input is the output vector from the eventCSV function
  # ouput is a vector of unique exams
  uniqueExams <- unique(str_sub(x, 1, nchar(x) - 17))
  return(uniqueExams)
}

# uniqueExams <- uniqueExamsFn(eventCSV())

###########################


# use the vector of unique exams to call then next function to load charts for each exam

eventMatrix <- function(x=uniqueExams) {
  # function to make a csv table of all stimulus events for all charts for each exam
  # will also parse the total unique events and missing events 
  # x is a character vector of unique exams 
  # output is 
  #
  ####
  
  uniqueExams <- x # same name as input vector
   
  #   ############# make some functions to run inside the loop for each unique exam  
  #   1) make a character vector of each _events.csv file
  #   2) make a vector of unique events for each exam
  #   3) fix the length of all events vectors to the max length for each exam
  #   4) make a vector of CQT charts with >= 2 RQs and >= 2 CQs
  #   5) make a vector of unique events and a data frame of events for all charts
  #   6) clean up before loading the next unique exam
  #   #############  
  
  ####### define some functions

  listFiles <- function(x = "_events.csv") {
    # function to make a vector of events.csv files
    # x is a character string "_events.csv" that identifies the events.csv files
    y <- list.files(path = ".", pattern = paste("^", uniqueExams[i], ".{6}", "_events.csv", "$", sep = ""), 
                    all.files = FALSE,
                    full.names = FALSE, 
                    recursive = FALSE,
                    ignore.case = FALSE, 
                    include.dirs = FALSE)
    return(y)
  } # end listfiles function

  ###

  readCSVs <- function(x = eventFileVector) {
    # function to read the info from the events.csv files in the eventFileVector
    # x is a vector of events.csv file names
    # a loop to read the info from all CSV files in the eventFileVector
    numberEvents <- NULL # the number of stim events for each chart in the loop
    eventVectorNames <- NULL # used in the following loop
    for (j in 1:length(eventFileVector)) {
      fileName <- eventFileVector[j]
      CSVdata <- read.csv(fileName, stringsAsFactors = FALSE)
      
      # a nested loop to make a vector of event lables for each chart
      #     eventLabels <- NULL
      #     for (j in 1:nrow(CSVdata)) {
      #       eventLabels <- c(eventLabels, as.character(CSVdata[j,2]))
      #     } # end of nested loop to read event labels for each chart
      
      eventLabels <- CSVdata[,2]
      # remove "" elements
      eventLabels <- eventLabels[which(eventLabels != "")]
      # determine the number of events and concatenate it to a vector
      numberEvents <- as.character(c(numberEvents, length(eventLabels)))
      # assign the events to a unique vector
      assign(fileName, eventLabels, pos = 1)
      # make a vector of unique event vector names
      eventVectorNames <- c(eventVectorNames, fileName)
    } # end of loop to read each "_events.csv$" file for events 
    assign("numberEvents", numberEvents, pos = 1)
    #
    return(eventVectorNames)
  } # end readCSVs function

  ###

  fixMax <- function(x = eventVectorNames) {
    # function to fix the number of events to the max length for each unique exam
    # x is a vector of names of vectors of events for each chart
      maxEvents <- max(as.integer(numberEvents))
      numberEventVectors <- length(eventVectorNames)
      #
      # a loop to fix the length of all event vectors to the max length for each exam
      for (k in 1:numberEventVectors) {
        tempVector <- get(eventVectorNames[k])
        tempVector <- c(tempVector, rep("-", maxEvents - length(tempVector)))
        # assign(paste("eventVector", k, sep = ""), tempVector)
        assign(eventVectorNames[k], tempVector, pos = 1)
        # maybe write the csv to include the padded events
        #
      } # end loop to fix the length of all event vectors to the max for each exam
      return(maxEvents)
  } # end of fixMax function

  ###

  CQTCharts <- function(x = eventVectorNames) {
    # function to identify charts with >=2 RQs and >=2 CQs
    # x is a
    retainEventVectorNames <- NULL
    allEventNames <- NULL
    eventVectorNames <- x
    for (l in 1:length(eventVectorNames)) {
      eventVectorName <- eventVectorNames[l] 
      RQs <- length(grep("R", get(eventVectorName), 
                         ignore.case = TRUE, 
                         perl = FALSE, 
                         value = FALSE,
                         fixed = FALSE, 
                         useBytes = FALSE, 
                         invert = FALSE)) >= 2 # make a logical scalar for RQs >= 2  
      CQs <- length(grep("C", get(eventVectorName), 
                         ignore.case = TRUE, 
                         perl = FALSE, 
                         value = FALSE,
                         fixed = FALSE, 
                         useBytes = FALSE, 
                         invert = FALSE)) >= 2 # make a logical scalar for CQs >= 2
      # keep only those charts with >= 2 RQs and >= 2 CQs
      if (CQs & RQs) {
        # eventVectorName <- paste("eventVector", l, sep = "") # change the eventVectorName to get the padded vector 
        retainEventVectorNames <- c(retainEventVectorNames, eventVectorNames[l])    
        # make a long vector of all events from all charts with CQs and RQs
        allEventNames <- c(allEventNames, get(eventVectorName))
        # allEventNames <- c(allEventNames, get(paste("eventVector", 1, sep= "")))
        #
      } # end if
      #
    } # end loop retain charts with min 2 CQs and min 2 RQs
    #
    # save the number of charts 
    numberCharts <- length(retainEventVectorNames)
    assign("numberCharts", length(retainEventVectorNames), pos = 1)
    assign("retainEventVectorNames", retainEventVectorNames, pos = 1)
    assign("allEventNames", allEventNames, pos = 1)
    #
    # set the file name and then save the csv of the names of CQT charts 
    # to retain for analysis
    # retainEventVectorNamesCSV <- paste(strtrim(eventFiles[1], nchar(eventFiles[1]) - 17), "_CQTcharts.csv", sep = "")
    retainEventVectorNamesCSV <- paste(uniqueExams[i],
                                       "_CQTCharts.csv", sep = "")
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
    return(retainEventVectorNames)
  } # end CQTCharts function

  ###

  makeEventDF <- function(x = allEventNames) {
    # function to make a data frame of events for all charts for each exam
    # x is a vector of all events for all charts
    #
    allEventNames <- x
    # make a vector of unique event names
    uniqueEventNames <- unique(allEventNames)
    # remove the padding event holders
    # uniqueEventNames <- uniqueEventNames[c(which(uniqueEventNames != "-"))]    
    assign("uniqueEventNames", uniqueEventNames, pos = 1)
    #
    # loop to make a combined matrix of event labels for charts with >=2 RQs and >= 2 CQs
    eventMatrixOut <- NULL
    for (m in 1:length(retainEventVectorNames)) {
      #     # nested loop to verify each event in each chart
      #     eventVector <- NULL
      #     for (n in 1:length(uniqueEventNames)) {
      #       includedEvents <- which(uniqueEventNames[n] == get(retainEventVectorNames[m]))
      #       eventVector <- c(eventVector, includedEvents)
      #       # eventMatrix <- rbind(eventMatrix, includedEvents)
      #       #
      #     } end nested loop       
      # construct the eventMatrix
      eventMatrixOut <- rbind(eventMatrixOut, get(retainEventVectorNames[m]))
      #  
    } # end loop to make a combined matrix of event lables for all charts with 2 RQs and 2 CQs
    #
    eventMatrixOut <- as.data.frame(eventMatrixOut)
    # set the row names for the matrix that describes the sequence of stimulus presentation
    rownames(eventMatrixOut) <- retainEventVectorNames
    # set the column names
    # colnames(eventMatrixOut) <- uniqueEventNames
    #
    # set the CSV file name
    CSVName <- paste(uniqueExams[i], 
                     "_eventMatrix.csv", sep = "")
    
    # no need to assign a unique name because this will need to be done for all exams
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
                row.names = TRUE, 
                col.names = FALSE, 
                qmethod = "double", 
                fileEncoding = "UTF-8")
    #
    return(eventMatrixOut)
  } # end makeEventDF

  ###

  cleanUp <- function(x = eventFileVector) {
    # function to clean up before loading the events.csv files for the the next unique exam
    # x is a vector of names of the vectors that hold data from the events.csv file
    rm(list = c("allEventNames", "numberCharts", "numberEvents", "uniqueEventNames", "retainEventVectorNames"), pos = 1)
    rm(list = x, pos = 1)
  } # end cleanUp function

  ####### end of function definitions

  # a loop to process the _events.csv files for each unique exam
  eventFileVector <- NULL
  for (i in 1:length(uniqueExams)) {
    eventFileVector <- listFiles("events.csv")
    eventVectorNames <- readCSVs(eventFileVector)
    maxEvents <- fixMax(eventVectorNames)
    retainEventVectorNames <- CQTCharts(eventVectorNames)
    eventMatrixOut <- makeEventDF(allEventNames)
    cleanUp()
  } # end loop for each unique exam loop
  #
  # still need to verify that each event is in each chart
  #
} # end eventMatrix()

# eventMatrix(uniqueExams)

# eventMatrix(uniqueExamsFn(eventCSV()))
