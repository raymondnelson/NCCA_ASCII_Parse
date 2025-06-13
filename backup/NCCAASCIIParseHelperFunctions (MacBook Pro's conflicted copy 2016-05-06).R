# helper functions for the NCCAASCIIParse function
# 4-15-2016
# Raymond Nelson

###########################

# 7 helper functions 

# headerFile()

# chartHeader()

# stimEvents()

# eventTable()

# dataFile()

# dataParse()

# cleanUp()


###################### helper functions ##############

##### header

# function to separate header information for each chart
headerFile <- function(x=fileNames, makeVector=FALSE, saveTXT=FALSE) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the header info from the data
  # this function is called recursively by the parseUniqueExams function
  
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
  myNames <- paste(str_sub(myNames, 1, -1), "_header", sep = "") 
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

# function to make a data frame and .csv file of the header data for each exam
chartHeader <- function(x=headerNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to make a data frame and .csv file of the header data for each exam
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
    
    # get the header data
    headerData <- NULL
    for (k in 1:length(chartHeaders)) {
      headerData <- c(headerData, chartHeaders[[k]][2])
    }
    
    # add the chartName and examName
    headerData <- c(str_sub(chartName, 1, -14), str_sub(chartName, -12, -8), headerData )
    hNames <- c("examName", "chartName", hNames)
    
    # names(myDF)
    # names
    
    # rbind the header data to the output data frame
    headerData <- rbind.data.frame(headerData)
    colnames(headerData) <- as.vector(hNames)
    
    # check to see that the column names are the same
    if(!is.null(myDF)) {
      if(!identical(names(myDF), names(headerData))) {
        print(paste0("column names are different for chart ", chartName))
        break()
      }
    }
    
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
  if(makeDF==TRUE) assign(outName, value=myDF, pos = 1)
  
  # return a vector of the name of the exams
  return(strtrim(outName, nchar(outName) - 7))
  
} # chartHeader end

# chartHeader(x=headerNames, makeDF=TRUE, saveCSV=TRUE)

##### stimulus events

# function to make a data frame and csv table of the stimulus text for all charts for each exam
stimEvents <- function(x=headerNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to make a data frame and csv table of the stimulus text for all charts for each exam
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
    
    if(length(headerFileStimuli)==1) next()
    
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

# function to make a data fram and csv table of all stimulus events
eventTable <- function(x=headerNames, makeDF=FALSE, saveCSV=FALSE) {
  # function to make a data frame and csv table of all stimulus events
  # parameters set to FALSE by default 
  # because the onset offset and answer are added to the stimuli data frame
  # setting TRUE will create a .csv file and data frame 
  # of the events for each exam
  # input is a vector of names of data frames for the chart header info
  # output is a vector of names of the exams
  
  # when makeDF and saveCSV are both FALSE the Begin End and Answer Columns
  # are added to the _Stimuli data frame
  
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
    
    if(length(headerFileEvents)==2) next()
    
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
dataFile <- function(x=fileNames, makeVector=FALSE, saveTXT=FALSE) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the time series data by removing the header lines
  # and create a vector of data names
  # will also create a data frame for each data file
  # this function is called recursively by the parseUniqueExam function
  
  # will also create a data frame for each data file
  
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
  myNames <- paste(str_sub(myNames, 1, -1), "_data", sep = "") # start at the 4th character
  
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

# function to read the NCCA ASCII time series data to a data frame and csv file
dataParse <- function(x=dataNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to read the NCCA ASCII time series data to a data frame
  # and also create csv version of the time series data
  
  # input is a vector of names from the output of the dataFile function
  # output is a vector of names of the exams
  
  ####
  
  # first make an empty vector to hold the output names
  myNames <- character()
  
  # make an empty vector to build the output data frame
  outDF <- NULL
  
  # a loop over the vector of chart data files
  for(i in 1:length(x)) {
    # i=1
    # first make a variable to hold the name of the chart data
    # x <- x # not sure this is necessary
    currentChartName <- str_sub(x[i], 1, -1)
    # make a vector of column names for each chart
    # this should be done individually for each chart
    # because some difference may exist between exams
    
    ###
    
    # set the number of data columns
    dataCols <- nchar(str_sub(get(currentChartName)[1], 25, -1L)) / 11
    
    # divide by 11 to determine the number of columns 
    
    # cols <- dataCols
    colWidths <- rep(11, dataCols)
    
    # read the text data from the data vector
    cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                            widths = c(6, 9, 9, colWidths), 
                                            header = FALSE, 
                                            skip = 0,
                                            n = 1))),
                       side = "both"
    )
    
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


#####################################

# function to clean up the data vectors in the global environment
cleanUp <- function(removeEach=FALSE) {
  # function to clean up the data vectors in the global environment
  # the removeEach parameter will also remove the data frames for each exam
  rm(list = ls(pattern = "_header", pos = 1), pos = 1)
  rm(list = ls(pattern = "_data", pos = 1), pos = 1)
  if (removeEach==TRUE) {
    rm(list = ls(pattern = "_Data$", pos = 1), pos = 1)
    rm(list = ls(pattern = "_Events$", pos = 1), pos = 1)
    rm(list = ls(pattern = "_Header$", pos = 1), pos = 1)
    rm(list = ls(pattern = "_Stimuli$", pos = 1), pos = 1)
  }
} # end cleanUp() function


