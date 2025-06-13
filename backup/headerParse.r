############# R Functions for parsing NCCA ASCII header information ##############
# 3-6-2014 Raymond Nelson
# 8-1-2014
#
# this script contains the following 3 functions
#
# chartHeader()
# to make a .csv file of the header data for each chart
#
# stimText()
# to make a .csv file of the stimulus text for each chart
#
# eventTable()
# to make a .csv file of the table of stimulus events for each chart
# will replace milling event values
#
##############################################
#
#
chartHeader <- function(x = "headerNames") {
  # function to make a .csv file of the names of all *.header.txt files
  #
  headerNames <- get(x, pos = 1) # same name in the local environment as in the project environment
  #
  # a loop to open all the header files in the current working directory
  for (i in 1:length(headerNames)) { 
    fileName <- headerNames[i]
    headerFile <- readLines(fileName, 
                            n = -1, 
                            ok = TRUE, 
                            warn = FALSE, 
                            encoding = "UTF-8")
    #select the header lines and separate the data
    headerFileHeaders <- headerFile[1:(pmatch("Event    Label Statement", 
                                              headerFile) - 2)]
    headerFileHeaders <- strsplit(headerFileHeaders, ": ")
    #
    # define some null variables
    headerFileHeadersElement <- NULL
    headerFileHeaderLabels <- NULL
    headerFileHeadersElementList <- NULL
    #
    # another loop to turn the header info into vectors that can be concatenated and saved as .csv
    for (k in 1:length(headerFileHeaders)) {
      headerFileHeadersElement <- headerFileHeaders[[k]]
      headerFileHeaderLabels <- c(headerFileHeaderLabels, 
                                  headerFileHeadersElement[1])
      headerFileHeadersElementList <- c(headerFileHeadersElementList, 
                                        headerFileHeadersElement[2])
    }
    #  set the file name to .csv
    fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 4), ".csv", 
                         sep = "")
    write.table(rbind(as.list(headerFileHeaderLabels), 
                      as.list(headerFileHeadersElementList)), 
                file = fileNameCSV, 
                append = FALSE, 
                quote = TRUE, 
                sep = ",", eol = "\n", 
                na = "NA", dec = ".", 
                row.names = FALSE, 
                col.names = FALSE, 
                qmethod = "double", 
                fileEncoding = "UTF-8")
  }
}
#
##########################################
#
# 
stimText <- function(x = "headerNames") {
  # function to make a csv table of the stimulus text for each chart
  #
  headerNames <- get(x, pos = 1) # same name in the local environment as in the project environment
  #
  # define a null variable for later use
  # stimTableList <- NULL
  #
  for (i in 1:length(headerNames)) { 
    fileName <- headerNames[i]
    headerFile <- readLines(fileName, 
                            n = -1, 
                            ok = TRUE, 
                            warn = FALSE, 
                            encoding = "UTF-8")
    #
    # get the stimulus text lines
    headerFileStimuli <- 
      headerFile[pmatch("Event    Label Statement", headerFile):
                   (pmatch("Event    Label      Begin        End     Answer", 
                           headerFile) - 2)]
    #
    # create a null vector to hold the stimulus text
    stimulusLines <- NULL
    # a nested loop to fix wrapped text lines
    for (j in 1:length(headerFileStimuli)) { # 2-22-2014 fixed so it handles multiple wrapped lines 
      # i <- 1 # for testing and development
      if (strtrim(headerFileStimuli[j], 6) == "      ") 
        stimulusLines[length(stimulusLines)] <- 
        paste(stimulusLines[length(stimulusLines)], 
              str_sub(headerFileStimuli[j], 16, nchar(headerFileStimuli[j])), 
              sep = "")
      if (strtrim(headerFileStimuli[j], 6) != "      ") stimulusLines <- 
        c(stimulusLines, headerFileStimuli[j])
    }
    headerFileStimuli <- stimulusLines
    #
    # set the header row
    # headerFileStimHeader <- headerFileStimuli[1]
    # headerFileStimHeader <- c(strtrim(headerFileStimHeader, 5), str_sub(headerFileStimHeader, 10, 14), str_sub(headerFileStimHeader, 16, 25))
    # get the event numbers
    eventNumb <- str_trim(strtrim(headerFileStimuli, 6), side = "both") 
    # get the event labels
    eventLabels <- str_trim(str_sub(headerFileStimuli, 7, 14), side = "both")  
    # get the stimulus text
    stimText <- str_trim(str_sub(headerFileStimuli, 16, 
                                 nchar(headerFileStimuli)), side = "both")
    #
    # set the filename and save the file
    fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 11), 
                         "_stimuli.csv", sep = "")
    #
    write.table(cbind(eventNumb[1:length(stimText)], 
                      eventLabels[1:length(stimText)], stimText), 
                file = fileNameCSV, 
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
    # create a vector for each list of stim questions
    # tableName <- paste("stimTable", i, sep = "")
    # assign(tableName, cbind(eventNumb[1:length(stimText)], eventLabels[1:length(stimText)], stimText))
    #
    # stimTableList <- c(stimTableList, tableName[1])  
    # 
    # paste("stimTable", i, sep = "") <- cbind(eventNumb[1:length(stimText)], eventLabels[1:length(stimText)], stimText)
    # 
  }
}
#
###########################################
#
#
eventTable <- function(x = "headerNames") {
  # function to make a table of all stimulus events
  #
  headerNames <- get(x, pos = 1) # same name in the local environment as in the project environment
  #
  # define a null variable for later use
  # stimTableList <- NULL
  #
  for (i in 1:length(headerNames)) { 
    fileName <- headerNames[i]
    headerFile <- readLines(fileName, 
                            n = -1, 
                            ok = TRUE, 
                            warn = FALSE, 
                            encoding = "UTF-8")
    #
    # get the lines with stimulus text statements
    headerFileEvents <- headerFile[pmatch("Event    Label      Begin        End     Answer", headerFile):length(headerFile)]
    #
    # get the event numbers
    eventNumb <- str_trim(strtrim(headerFileEvents, 6), side = "both")
    # get the event labels
    eventLabels <- str_trim(str_sub(headerFileEvents, 7, 14), side = "both")
    # get the event onset
    eventOnset <- str_trim(str_sub(headerFileEvents, 15, 25), side = "both")
    # get the event offset
    eventOffset <- str_trim(str_sub(headerFileEvents, 26, 36), side = "both")
    # get the event answer
    eventAnswer <- str_trim(str_sub(headerFileEvents, 37, 47), side = "both")
    #
    # make the event table
    tempCSV <- as.data.frame(cbind(eventNumb, eventLabels, eventOnset, eventOffset, eventAnswer), stringsAsFactors = FALSE)    
    #
    # assign("tempCSV", tempCSV, pos = 1)
    # replace missing events
    #
    # first define a null chacter character vector for use in the loop
    # tempCSV <- read.csv(chartNames[i])
    # tempCSV <- read.csv(chartNames[1])
    tempCSV2 <- NULL
    #
    # then make a nested loop to remove lines with no data
    for (j in 1:nrow(as.vector(tempCSV))) {
      if (as.vector(tempCSV[j,2]) != "") {
        if (nchar(as.character(tempCSV[j,4])) == 0) tempCSV[j,4] <- as.vector(tempCSV[j,3])
        if (nchar(as.character(tempCSV[j,5])) == 0) tempCSV[j,5] <- as.vector(tempCSV[j,4])
        tempCSV2 <- rbind(as.vector(tempCSV2), as.vector(tempCSV[j,]))
      }                   
    }
    #
    # set the filename and save the file
    fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 11), "_events.csv", sep = "")
    write.table(tempCSV2, file = fileNameCSV, 
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
    # loop will repeat with header files for all charts
  } 
}
#  
