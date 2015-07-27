################ R Functions for parsing NCCA ASCII text output ##############
# 3-2-2014 Raymond Nelson
# 8-1-2014
# 6-9-2015
# 6-12-2015 working
#
#
# this script contains the following functions
#
# getcharts() 
# to make a character vector of the names of NCCAASCII text output files
#
# uniqueExams()
# to make a short list of unique exam names
#
# headerFile()
# to make a separate txt file of the header information
#
# dataFile()
# to make a separate txt file of the time series data
#
# cleanUp()
# to remove items from the global environment
#
# parseUniqueExams()
# to load each unique exam and parse the data using the headerFile() and dataFile() fuctions
#
# this script will also source 5 other scripts from the NCCA_ASCII_Parse directory
#
# fixFileNames.R # to remove problem characters from file names 
# dataParse.R # to save the time series data in csv format
# headerParse.R # to save the header information in csv format
#
# eventParse.R # to save the events in csv format
# stimulusParse.R # to save the question stimuli in csv format
#
# first put all NCCA ASCII output files in the same working directory
#
# input for this script is a directory containing the NCCA ASCII text files
#
# ouput will be 
#
########################################################

# requires "stringr" library
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library", quietly = TRUE)

# setwd("~/Documents/R_programming/NCCA_ASCII_Parse/data/Backster_YouPhase_N6 ")

# setwd("~/Documents/R_programming/NCCA_ASCII_Parse/PF090316 (Cleaned Copy)")

setwd("~/Documents/R_programming/NCCA_ASCII_Parse/data_working")

#################

# may need to first fix all problem characters in file and directory names

# source("~/Documents/R_programming/NCCA_ASCII_Parse/fixFileNames.r", echo=FALSE)

fixFileNames <- function() {
  # function to remove problem characters from filenames
  
  # remove ( characters from all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("\\(", "", fileNames[i]))
  }
  # newFileNames <- list.files()
  
  # remove ) characters from all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("\\)", "", fileNames[i]))
  }
  # newFileNames <- list.files()
  
  # replace space characters for all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub(" ", "_", fileNames[i]))
  }
  # newFileNames <- list.files()
  
  # replace "_Cleaned_Copy" in all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("_Cleaned_Copy", "", fileNames[i]))
  }
  
  #output
  fileNames <- list.files()
  return(fileNames)
  
} # fixFileNames()

# fixFileNames()

fixFileNames()




################# 

# make a character vector of all charts in the working directory

getCharts <- function(x = "D&+") {
  # function to make a list of files with specified strings in the file names
  # the needed string should identify the NCCA ASCII output files beginning with "D&"
  
  # uses a regular expression in the input argument
  # output is a vector of file names in the current working directory
  
  y <- list.files(path = ".", 
                  pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE,
                  no.. = FALSE)
  y <- grep(x, y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = FALSE) # includes only those files that match the input string
  y <- grep("*.txt", y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = TRUE) # excludes .txt files
  y <- grep("*.csv", y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = TRUE) # excludes .csv files
  
  # create a vector of file names
  # assign("fileNames", y, pos = 1)
  
  return(y)
  
} # getCharts

fileNames <- getCharts("D&+")



####################################

# make a vector of unique exams from the fileNames vector

uniqueNames <- function(x = fileNames) {
  # function to make a list of uniques exams in a directory
  # input is a vector of unique file names from the getCharts function
  # output is a vector of unique exam names
  y <- unique(str_sub(x, 1, -7))
  return(y)
}

uniqueExamNames <- uniqueNames(fileNames) # need to call the getCharts function first
print(uniqueExamNames)


####################################

##### header

# function to separate header information into a separate object for each chart
# and create a vector of header names for each exam
# will also create a data frame for each header file
headerFile <- function(x=fileNames, makeVector=FALSE, saveTXT=FALSE) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the header info from the data
  
  # input is a vector of filenames from the getCharts function
  # output is a vector of header names for a unique data frame for each chart header
  # will also create a data frame for each header file
  
  # this function can be recursively called by the parseUniqueExam function 
  
  # this function can recursively create .txt files 
  # from the header info of all charts in the "fileNames" vector
  
  ####
  
  # make a null vector to hold the names of the headerFiles
  myNames <- x
  
  # fix problem characters
  myNames <- str_replace(myNames, "Cleaned Copy", "CleanedCopy")
  myNames <- str_replace_all(myNames, "D&_", "")

  myNames <- str_replace_all(myNames, "[:punct:]", "_")
  
  # do not need these when using the [:punct:]
  #   myNames <- str_replace(myNames, ")-", "_")
  #   myNames <- str_replace(myNames, " \\(", "_")
  #   myNames <- str_replace_all(myNames, "\\.", "_")
  #   myNames <- str_replace_all(myNames, " ", "_")
  #   myNames <- str_replace_all(myNames, "-", "_")
  
  # append "_header" to the names
  myNames <- paste(str_sub(myNames, 4, -1), "_header", sep = "") 
  # start at the 4th character
  
  # add the file extension
  myNames <- paste(myNames, ".txt", sep = "")
  
  # loop over the file names and read the files
  for (i in 1:length(x)) {
    # read 100 lines from each file
    m <- readLines(x[i], n = 100L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # locate the begining of the time-series data - the end of the header section
    z <- pmatch("Sample     Time", str_trim(strtrim(m, 15), side = "both")) - 1
    # keep only the header lines
    m <- m[1:z]
    # create a character vector for each header 
    if(makeVector==TRUE) assign(strtrim(myNames[i], nchar(myNames[i]) -4), m, pos = 1)
    # save the txt file
    if(saveTXT==TRUE) cat(m, file = myNames[i], sep = "\n")
  }
  
  # return the character vector of header file names
  # assign("headerNames", str_sub(myNames, 1, -5), pos = 1)
  return(str_sub(myNames, 1, -5))
  
} # headerFile end

# headerFile() # this function is called recursively by the parseUniqueExams function

chartHeader <- function(x=headerNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to make a .csv file of the header data for each exam
  # input is a vector of names of the headers for each chart
  # output is a data frame of names of the header data for all charts
  #
  ####
  
  # make an empty data frame for the output
  myDF <- NULL
  
  # a loop to open all the header files in the current working directory
  # i <- 1
  for (i in 1:length(x)) { 
    chartName <- x[i]
    chartHeaders <- get(chartName)
    
    # not reading from disk
    #     chartHeaders <- readLines(fileName, 
    #                             n = -1, 
    #                             ok = TRUE, 
    #                             warn = FALSE, 
    #                             encoding = "UTF-8")
    
    #select the header lines and separate the header data
    chartHeaders <- chartHeaders[1:(pmatch("Event    Label Statement", 
                                           chartHeaders) - 2)]
    
    # split the chart headers into lables and data
    # result is a list
    chartHeaders <- strsplit(chartHeaders, ": ")
    
    # get the header names
    hNames <- NULL
    for (j in 1:length(chartHeaders)) {
      hNames <- c(hNames, chartHeaders[[j]][1])
    }
    
    # headerDatum <- NULL
    
    # get the header data
    headerData <- NULL
    for (k in 1:length(chartHeaders)) {
      headerData <- c(headerData, chartHeaders[[k]][2])
    }
    
    # add the chartName and examName
    headerData <- c(str_sub(chartName, 1, -14), str_sub(chartName, -12, -8), headerData )
    hNames <- c("examName", "chartName", hNames)
    
    # rbind the header data to the output data frame
    headerData <- rbind.data.frame(headerData)
    colnames(headerData) <- as.vector(hNames)
    myDF <- rbind(myDF, headerData)
    
  } # end loop to open all chart header vectors
  
  #  set the name of the ouput object
  outName <- paste(strtrim(chartName, nchar(chartName) - 13), "_Header", sep="")
  
  # and save it as a .csv in the cwd
  if (saveCSV==TRUE) write.table(myDF, 
                                 file = paste(outName, ".csv", sep=""),
                                 append = FALSE, 
                                 quote = TRUE, 
                                 sep = ",", eol = "\n", 
                                 na = "NA", dec = ".", 
                                 row.names = FALSE, 
                                 col.names = TRUE, 
                                 qmethod = "double", 
                                 fileEncoding = "UTF-8")
  
  # save the chart header data frame to the global environment 
  if(makeDF==TRUE) assign(outName, 
                          value=myDF, 
                          pos = 1)
  
  # return a vector of the name of the exams
  return(strtrim(outName, nchar(outName) - 7))
  
} # chartHeader end

# chartHeader(x=headerNames, makeDF=TRUE, saveCSV=TRUE)




##########################################


stimEvents <- function(x=headerNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to make a csv table of the stimulus text for all charts for each exam
  # will create a .csv file and data frame with the stim text for each exam
  # also includes onset offset and answer data for each event
  # input is a vector of names of data frames for the chart header info
  # output is a vector of names of the exams
  #
  ####
  
  # make null vector to cbind the data frame
  myDF <- NULL
  
  # loop to process the stimulus text for each chart
  # i <- 1
  for (i in 1:length(x)) { 
    chartName <- x[i]
    chartHeaders <- get(chartName)
    
    # not reading from disk, using a vector instead    
    #     chartHeaders <- readLines(fileName, 
    #                             n = -1, 
    #                             ok = TRUE, 
    #                             warn = FALSE, 
    #                             encoding = "UTF-8")
    
    # get the stimulus text lines
    headerFileStimuli <- chartHeaders[pmatch("Event    Label Statement", chartHeaders):
                                        (pmatch("Event    Label      Begin        End     Answer", chartHeaders) - 2)]
    
    # create a null vector to hold the stimulus text
    stimulusLines <- NULL
    
    # a nested loop to fix wrapped text lines
    for (j in 1:length(headerFileStimuli)) { # 2-22-2014 fixed so it handles multiple wrapped lines 
      # i <- 1 # for testing
      if (strtrim(headerFileStimuli[j], 6) == "      ") 
        stimulusLines[length(stimulusLines)] <- 
        paste(stimulusLines[length(stimulusLines)], 
              str_sub(headerFileStimuli[j], 16, nchar(headerFileStimuli[j])), 
              sep = "")
      if (strtrim(headerFileStimuli[j], 6) != "      ") stimulusLines <- 
        c(stimulusLines, headerFileStimuli[j])
    }
    headerFileStimuli <- stimulusLines
    
    # set the header row
    # headerFileStimHeader <- headerFileStimuli[1]
    # headerFileStimHeader <- c(strtrim(headerFileStimHeader, 5), str_sub(headerFileStimHeader, 10, 14), str_sub(headerFileStimHeader, 16, 25))
    
    # get the event numbers
    eventNumb <- str_trim(strtrim(headerFileStimuli, 6), side = "both") 
    # get the event labels
    eventLabels <- str_trim(str_sub(headerFileStimuli, 7, 14), side = "both")  
    # get the stimulus text
    stimText <- as.character(str_trim(str_sub(headerFileStimuli, 16, 
                                              nchar(headerFileStimuli)), side = "both"))
    
    # cbind to build the data frame 
    stimTextDF <- cbind.data.frame(eventNumb[1:length(stimText)], 
                                   eventLabels[1:length(stimText)], stimText, stringsAsFactors=FALSE)
    # use the first row as the column names
    names(stimTextDF) <- stimTextDF[1,]
    stimTextDF <- stimTextDF[2:nrow(stimTextDF),]
    
    # add the examName seriesName and chartName columns
    stimTextDF <- cbind(rep(str_sub(chartName, 1, -14), times=nrow(stimTextDF)), 
                        rep(str_sub(chartName, -12, -12), times=nrow(stimTextDF)),
                        rep(str_sub(chartName, -12, -8), times=nrow(stimTextDF)),
                        stimTextDF)
    
    names(stimTextDF) <- c("examName", "seriesName", "chartName", names(stimTextDF)[-c(1:3)])
    
    # and finally cbind the data frame to the output data frame
    myDF <- rbind(myDF, stimTextDF)
    
  } # end loop to iterate through the exam charts
  
  myDF$examName <- as.character(myDF$examName)
  myDF$chartName <- as.character(myDF$chartName)
  myDF$Statement <- as.character(myDF$Statement)
  # set the filename and save the file
  outName <- paste(strtrim(chartName, nchar(chartName) - 13), "_Stimuli", sep = "")
  
  if (saveCSV==TRUE) write.table(myDF, 
                                 file = paste(outName, ".csv", sep=""), 
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
  
  if (makeDF==TRUE) assign(outName, 
                           myDF, 
                           pos = 1)
  
  # output
  return(str_sub(chartName, 1, -14))
  
} # stimEvents end

# stimEvents(x=headerNames, makeDF=TRUE, saveCSV=TRUE)



###########################################

# when makeDF and saveCSV are both FALSE the Begin End and Answer Columns
# are added to the _Stimuli data frame

eventTable <- function(x=headerNames, makeDF=FALSE, saveCSV=FALSE) {
  # function to make a table of all stimulus events
  # parameters set to FALSE by default 
  # because the onset offset and answer are added to the stimuli data frame
  # setting TRUE will create a .csv file and data frame 
  # of the events for each exam
  # input is a vector of names of data frames for the chart header info
  # output is a vector of names of the exams
  ####
  
  # make a null vector to uild the output
  myDF <- NULL
  
  # loop through all the exam charts
  # i <- 1 # for testing only
  for (i in 1:length(x)) { 
    chartName <- x[i]
    headerFile <- get(chartName)
    
    # not reading from disk
    #     headerFile <- readLines(fileName, 
    #                             n = -1, 
    #                             ok = TRUE, 
    #                             warn = FALSE, 
    #                             encoding = "UTF-8")
    
    # get the lines with stimulus text statements
    headerFileEvents <- headerFile[pmatch("Event    Label      Begin        End     Answer",
                                          headerFile):length(headerFile)]
    
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
    
    # make the event table
    eventDF <- as.data.frame(cbind(eventNumb, 
                                   eventLabels, 
                                   eventOnset, 
                                   eventOffset, 
                                   eventAnswer), 
                             row.names = NULL, 
                             stringsAsFactors = FALSE)
    names(eventDF) <- eventDF[1,]
    eventDF <- eventDF[2:nrow(eventDF),]
    
    # a loop to remove lines with no data
    eventDF2 <- NULL
    for (j in 1:nrow(as.vector(eventDF))) {
      if (as.vector(eventDF[j,2]) != "") {
        if (nchar(as.character(eventDF[j,4])) == 0) eventDF[j,4] <- as.vector(eventDF[j,3])
        if (nchar(as.character(eventDF[j,5])) == 0) eventDF[j,5] <- as.vector(eventDF[j,4])
        eventDF2 <- rbind(as.vector(eventDF2), as.vector(eventDF[j,]))
      }                   
    }
    
    # add the examName and chartName colums
    eventDF2 <- cbind(rep(str_sub(chartName, 1, -14), times=nrow(eventDF2)), 
                      rep(str_sub(chartName, -12, -12), times=nrow(eventDF2)),
                      rep(str_sub(chartName, -12, -8), times=nrow(eventDF2)),
                      eventDF2)
    names(eventDF2) <- c("examName", "seriesName", "chartName", names(eventDF2)[-c(1:3)])
    
    # rbind the data frame with the output
    myDF <- rbind(myDF, eventDF2)
    
  } # end loop through charts
  
  # cbind the onset offset and answer events to the stimuli data frame
  # and save the result as a .csv and data frame
  # but only when the events data frame itself is not saved
  if (saveCSV==FALSE && makeDF==FALSE) {
    stimDFName <- paste0(strtrim(chartName, nchar(chartName) - 13), "_Stimuli")
    stimDF <- get(stimDFName)
    myDF <- cbind(stimDF[,1:5], myDF[,6:8], stimDF[,6], row.names=NULL)    
    names(myDF) <- c("examName", "seriesName", "chartName", "Event", "Label", "Begin",  "End", "Answer", "Statement")
    myDF$Statement <- as.character(myDF$Statement)
    assign(stimDFName, myDF, pos = 1)
    write.table(myDF, 
                file = paste0(stimDFName, ".csv"), 
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
  }
  
  # set the filename
  outName <- paste(strtrim(chartName, nchar(chartName) - 13), "_Events", sep = "")
  
  if (saveCSV==TRUE) write.table(myDF, 
                                 file = paste0(outName, ".csv"), 
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
  
  if (makeDF==TRUE) {
    assign(outName, myDF, pos = 1)
  }
  
  #ouput the name of the exam
  return(str_sub(chartName, 1, -14))
  
} # eventTable end

# eventTable(x=headerNames, makeDF=FALSE, saveCSV=FALSE)



##### data

# function to separate the data from header info
# and create a vector of data names
# will also create a data frame for each data file
dataFile <- function(x=fileNames, makeVector=FALSE, saveTXT=FALSE) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the time series data by removing the header lines
  
  # input is a vector of file names from the getCharts function
  # output is a vector of names of unique objects for each data file
  
  # this function can be recursively called by the parseUniqueExam function 
  
  # this function can recursively create .txt files 
  # from the data info of all charts in the "fileNames" vector
  
  ####
  
  # make a null vector to hold the names of the data Files
  myNames <- x
  
  # fix problem characters
  myNames <- str_replace(myNames, "Cleaned Copy", "CleanedCopy")
  myNames <- str_replace_all(myNames, "D&_", "")
 
  myNames <- str_replace_all(myNames, "[:punct:]", "_")
  
  # do not need these when using the [:punct:]
  #   myNames <- str_replace(myNames, ")-", "_")
  #   myNames <- str_replace(myNames, " \\(", "_")
  #   myNames <- str_replace_all(myNames, "\\.", "_")
  #   myNames <- str_replace_all(myNames, " ", "_")
  #   myNames <- str_replace_all(myNames, "-", "_")
  
  # append "_data" to the names
  myNames <- paste(str_sub(myNames, 4, -1), "_data", sep = "") # start at the 4th character
  
  # add the file extension
  myNames <- paste(myNames, ".txt", sep = "")
  
  # loop over the file names and read the files
  for (i in 1:length(x)) {
    # read 100 lines from each file
    m <- readLines(x[i], n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # locate the begining of the time-series data - the end of the header section
    z <- pmatch("Sample     Time", str_trim(strtrim(m, 15), side = "both"))
    # keep only the time series data lines
    m <- m[z:length(m)]
    # create a character vector for each data 
    if(makeVector==TRUE) assign(strtrim(myNames[i], nchar(myNames) -4), m, pos = 1)
    # save the .TXT file
    if(saveTXT==TRUE) cat(m, file = myNames[i], sep = "\n")
    
  }
  
  # return the character vector of data file names
  return(str_sub(myNames, 1, -5))
  
} # dataFile end

# dataFile() # this function is called recursively by the parseUniqueExams function

##################

dataParse <- function(x=dataNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to read the NCCA ASCII time series data to a data frame
  # and also create csv version of the time series data
  
  # input is a vector of names from the output of the dataFile function
  # output is a vector of names of the exams
  
  # add columns for the exam name and chart name
  
  ####
  
  # first make an empty vector to hold the output names
  myNames <- character()
  
  # make an empty vector to build the output data frame
  outDF <- NULL
  
  # a loop over the vector of chart data files
  # i <- 1
  for(i in 1:length(x)) {
    # first make a variable to hold the name of the chart data
    # x <- x # not sure this is necessary
    currentChartName <- str_sub(x[i], 1, -1)
    # make a vector of column names for each chart
    # this should be done individually for each chart
    # because some difference may exist between exams
    
    ###
    
    # set the number of data columns
    cols <- 7
    colWidths <- rep(11, cols)
    
    # read the text data from the data vector
    cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                            widths = c(6, 9, 9, colWidths), 
                                            header = FALSE, 
                                            skip = 0,
                                            n = 1)
    )
    ),
    side = "both")
    myDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                     widths = c(6, 9, 9, colWidths), 
                     header = FALSE, 
                     skip = 1,
                     col.names = cNames,
                     n = -1L)                    
    
    # add a column for the chart name
    chartName <- rep(str_sub(currentChartName, -10, -6), nrow(myDF))
    myDF <- cbind(chartName, myDF)
    # add a column for the series name
    seriesName <- rep(str_sub(currentChartName, -10, -10), nrow(myDF))
    myDF <- cbind(seriesName, myDF)
    # add a column for the exam name
    examName <- rep(str_sub(currentChartName, 1, -12), nrow(myDF))
    myDF <- cbind(examName, myDF)
    
    # rbind the DFs for all charts
    outDF <- rbind(outDF, myDF)
  }
  
  # create the CSV file name
  outName <- paste(str_sub(currentChartName, 1, -12), "_Data", sep = "")
  # save the data as a CSV
  if (saveCSV==TRUE) write.csv(outDF, 
                               file=paste(outName, ".csv", sep = ""), 
                               row.names = FALSE)
  # save the data as a data frame 
  if(makeDF==TRUE) assign(x=outName, 
                          value=outDF, 
                          pos = 1)
  
  # concatenate the current chart name with the vector of chart names
  myNames <- c(myNames, str_sub(currentChartName, 1, -12))
  
  # output is the name of the exam
  myNames <- paste(myNames, "_data", sep = "")
  return(myNames) 
  
} # dataParse

# dataParse(x=dataNames, makeDF=TRUE, saveCSV=TRUE) # called by the parseUniqueExams function in the NCCAASCIIParse.r script



###########################################

# function to clean up the global environment
# by removing the _data and _header files
cleanUp <- function(removeEach=FALSE) {
  # function to clean up the data vectors in the global environment
  # the removeEach parameter will alsoremove the data frames for each exam
  
  # this function can be recursively called by the parseUniqueExam function 
  
  # rm(list = dataNames, pos = 1)
  # rm(list = headerNames, pos = 1)
  # rm(fileNames, pos = 1)
  # rm(dataNames, pos = 1)
  # rm(headerNames, pos = 1)
  # rm(uniqueExamNames, pos = 1)
  # rm(list = ls(pattern = "_DF", pos = 1), pos = 1)
  rm(list = ls(pattern = "_header", pos = 1), pos = 1)
  rm(list = ls(pattern = "_data", pos = 1), pos = 1)
  
  if (removeEach==TRUE) {
    rm(list = ls(pattern = "_Data$", pos = 1), pos = 1)
    rm(list = ls(pattern = "_Events$", pos = 1), pos = 1)
    rm(list = ls(pattern = "_Header$", pos = 1), pos = 1)
    rm(list = ls(pattern = "_Stimuli$", pos = 1), pos = 1)    
  }
  
} # cleanUp end

# cleanUp()



##########################################

# function to loop over the unique exam names
# and load each exam and parse the header and time series data
# into separate txt files
# using the headerFile and dataFile functions in this script
# this script will also source the headerParse.R and dataParse.R scripts
# and call the functions from those scripts

# ### first source the dataParse.R script because this function will call dataParse()
# source('~/Documents/R_programming/NCCA_ASCII_Parse/dataParse.R', echo=FALSE)

# ### also sourse the headerParse.R script to load chartHeader() stimEvents() and eventTable()
# source('~/Documents/R_programming/NCCA_ASCII_Parse/headerParse.r', echo=FALSE)



parseUniqueExams <- function(x=uniqueExamNames, clean=TRUE) {
  # function to loop over the unique exam names, and then
  # load all charts for each exam and parse the header and time series data 
  # using the headerFile and dataFile functions
  
  # call the getCharts and uniqueExams functions first
  
  # input x is a vector of unique exam names from the uniqueExams function
  # output is 
  
  # can use cleanUp to remove items from the global environment  
  if(clean==TRUE) cleanUp(removeEach=FALSE)
  
  # make a for loop to process multiple charts for each unique exam
  # i <- 1
  for (i in 1:length(x)) {
    # examName <- x[i]
    print(x[i])
    # make a regular expression of the searchString
    searchString <- paste(x[i], "+", sep = "")
    # make a list of charts for each exam
    examCharts <- list.files(path = ".", 
                    pattern = searchString, 
                    all.files = FALSE,
                    full.names = FALSE, 
                    recursive = FALSE,
                    ignore.case = FALSE, 
                    include.dirs = FALSE,
                    no.. = FALSE)
    
    # header files
    
    # create a vector for each chart header and a vector of chart header names
    headerNames <- headerFile(x=examCharts, makeVector=TRUE, saveTXT=FALSE) 
    # _header vectors are removed later by the cleanUp function
    
    # make sure to source the headerParse.R script first for the next three functions
    
    # make a csv and data frame from the chart header info
    # output is a vector of names of header data frames
    chartHeader(x=headerNames, makeDF=TRUE, saveCSV=TRUE) # call the headerFile() function first
    # make a csv table from from the stimulus text statements for each chart
    stimEvents(x=headerNames, makeDF=TRUE, saveCSV=TRUE) # call the headerFile() function first
    # make a csv table from the stimulus events for each chart
    eventTable(x=headerNames, makeDF=FALSE, saveCSV=FALSE) # call the headerFile() function first
    # when makeDF and saveCSV are both false the Begin End and Answer are added 
    # to the _Stimuli data frame created by the stimEvents() function
    
    ###
    
    # data files
    
    # create a data vector for each chart and a vector of data vector names
    dataNames <- dataFile(x=examCharts, makeVector=TRUE, saveTXT=FALSE) 
    # _data vectors are removed later by the cleanUp function
    
    # make sure to source the dataParse.R script first for the the next function
    
    # save the data in csv format for each exam
    dataParse(x=dataNames, makeDF=TRUE, saveCSV=TRUE)
    
    # clean up before loading the next exam
    if(clean == TRUE) cleanUp(removeEach=FALSE)
    
  } # end loop to process multiple charts for each exam
  
} # parseUniqueExams end

# parseUniqueExams(uniqueExams(getCharts()))
parseUniqueExams(x=uniqueExamNames, clean=TRUE)
# clean=TRUE will remove the _data and _header vectors and keep the data frames



# ##########################################
# 
# # source the script and call the function to parse the stimulus events
# 
# source('~/Documents/R_programming/NCCA_ASCII_Parse/eventParse.r', echo=FALSE)
# 
# # eventParse function make a stimulus matrix for the events
# # for each series, keeping only the charts with >=2 CQs and >=2 RQS,
# # and will set the length of the stimulus sequence to the max length
# # for each series
# 
# eventParse(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE, type="CQT")
# 
# 
# 
# #########################################
# 
# 
# 
# source('~/Documents/R_programming/NCCA_ASCII_Parse/stimulusParse.r', echo=FALSE)
# 
# 
# # function to determine whether stimulus text is identical for questions on all charts
# # x input is a character string to identify the Stimuli data frames
# # output is a data frame and csv warning regarding differences 
# 
# stimCheck(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE)
# 
# ####################################



# save the data for each exam in .Rda .RData format

library(stringr)

# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

saveData <- function(x) {
  # function to save the parsed NCCA ASCII data as .Rda for each exam in the cwd
  # x is a vector of unique exams in the global environment
  for (i in 1:length(uniqueExams)) {
    save(list=ls(pattern=uniqueExams[i], pos=1), file=paste0(uniqueExams[i], ".Rda"))
  }
  
} # end saveData function

saveData(uniqueExams)

#####

# clean up 

rm(chartHeader)
rm(cleanUp)
rm(dataFile)
rm(dataParse)
rm(eventTable)
rm(fixFileNames)
rm(getCharts)
rm(headerFile)
rm(parseUniqueExams)
rm(saveData)
rm(stimEvents)
rm(uniqueNames)

rm(list=ls(pattern="_Header$"))


#####

# save environment in this state

save.image(file="NCCAworking.Rda")

# load(file="NCCAworking.Rda")


