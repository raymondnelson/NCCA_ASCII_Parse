# r script to extract the exam header information from the NCCA ASCII output
# will make separate .csv files for stimulus questions after fixing wrapped lines
# make a separate .csv for stimulus events
# working 2-22-2014
#
# use the parseFilesCSV.r script before this one
# this script will make a vector of all *.header.txt files and then separate out the exam header info
# after that the test stimulus will be separated, and make separate .csv files for questions and events
#
# this script should also leave no objects in the workspace
#
#############################################################
#
# setwd("~/Documents/R programming/activity_multi-chanel_device/09N-0609output")
# #
# library(stringr)
#
######################
#
# make a vector of the names of all *.header.txt files
headerList <- list.files(path = ".", pattern = "*.header.txt", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE)
#
#######################
#
# a loop to open all the header file in the current working directory
#
for (i in 1:length(headerList)) { 
  fileName <- headerList[i] 
  # fileName <- headerList[1] # for testing - comment this out to use the loop
  #
  # read the file
  headerFile <- readLines(fileName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  #
  # locate the string "Event    Label Statement" and read the headerFile without the event lines
  headerFileHeaders <- headerFile[1:(pmatch("Event    Label Statement", headerFile) - 2)]
  headerFileHeaders <- strsplit(headerFileHeaders, ": ")
  #
  #####################
  #
  # another loop to turn the header info into vectors that can be concatenated and saved as .csv
  headerFileHeadersElement <- NULL
  headerFileHeaderLabels <- NULL
  headerFileHeadersElementList <- NULL
  for (k in 1:length(headerFileHeaders)) {
    headerFileHeadersElement <- headerFileHeaders[[k]]
    headerFileHeaderLabels <- c(headerFileHeaderLabels, headerFileHeadersElement[1])
    headerFileHeadersElementList <- c(headerFileHeadersElementList, headerFileHeadersElement[2])
  }
  #  set the file name to .csv
  fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 4), ".csv", sep = "")
  write.table(rbind(as.list(headerFileHeaderLabels), as.list(headerFileHeadersElementList)), 
              file = fileNameCSV, 
              append = FALSE, 
              quote = TRUE, 
              sep = ",", eol = "\n", 
              na = "NA", dec = ".", 
              row.names = FALSE, 
              col.names = FALSE, 
              qmethod = "double", 
              fileEncoding = "UTF-8")
  #  
  ####################
  #
  # read the stimulus text and make a CSV table
  #
  headerFileStimuli <- headerFile[pmatch("Event    Label Statement", headerFile):(pmatch("Event    Label      Begin        End     Answer", headerFile) - 2)]
  #
  # headerFileStimuli
  #
  # a nested loop to fix wrapped text lines
  stimulusLines <- NULL
  for (j in 1:length(headerFileStimuli)) { # 2-22-2014 fixed so it handles multiple wrapped lines 
    # i <- 1 # for testing and development
    # if (strtrim(headerFileStimuli[i], 6) == "      ") stimulusLines <- 
    #  c(stimulusLines[-length(stimulusLines)], paste(headerFileStimuli[i -1], str_sub(headerFileStimuli[i], 16, nchar(headerFileStimuli[i])), sep = ""))
    if (strtrim(headerFileStimuli[j], 6) == "      ") stimulusLines[length(stimulusLines)] <- 
      paste(stimulusLines[length(stimulusLines)], str_sub(headerFileStimuli[j], 16, nchar(headerFileStimuli[j])), sep = "")
    if (strtrim(headerFileStimuli[j], 6) != "      ") stimulusLines <- c(stimulusLines, headerFileStimuli[j])
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
  stimText <- str_trim(str_sub(headerFileStimuli, 16, nchar(headerFileStimuli)), side = "both")
  #
  # concatenate the vectors
  # stimTable <- cbind(eventNumb, eventLabels, stimText)
  # set the filename and save the file
  fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 10), "stimuli.csv", sep = "")
#   write.table(cbind(eventNumb, eventLabels, stimText), file = fileNameCSV, append = FALSE, 
#               quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", 
#               row.names = FALSE, col.names = FALSE, 
#               qmethod = "double", fileEncoding = "UTF-8")
  write.table(cbind(eventNumb[1:length(stimText)], eventLabels[1:length(stimText)], stimText), 
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
  tableName <- paste("stimTable", i, sep = "")
  assign(tableName, cbind(eventNumb[1:length(stimText)], eventLabels[1:length(stimText)], stimText))
#   paste("stimTable", i, sep = "") <- cbind(eventNumb[1:length(stimText)], eventLabels[1:length(stimText)], stimText)
  # 
  ######################
  #
  # make a CSV table of the stimulus events
  #
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
  # set the filename and save the file
  fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 10), "events.csv", sep = "")
  write.table(cbind(eventNumb, eventLabels, eventOnset, eventOffset, eventAnswer), 
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
  # loop will repeat with header files for all charts
}
#
# clean up
#
rm(eventLabels)
rm(eventNumb)
rm(eventOffset)
rm(eventOnset)
rm(fileName)
rm(fileNameCSV)
rm(headerFile)
rm(headerFileEvents)
rm(headerFileHeaders)
rm(headerFileStimuli)
rm(headerList)
rm(i)
rm(eventAnswer)
rm(stimText)
rm(headerFileHeaderLabels)
rm(headerFileHeadersElement)
rm(headerFileHeadersElementList)
rm(stimulusLines)
#
############ END ############
