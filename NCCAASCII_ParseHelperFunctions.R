# helper functions for the NCCAASCIIParse function
# 4-15-2016
# Raymond Nelson
#
####

library(stringr)

####### helper functions for parseUniqueExamHeaders() and parseUniqueExamData()

# headerFile() to make a vector of chart header names

# stimEvents() to make a data frame for the stimulus text for each chart

# eventTable() to make a data frame for the stimulus events for each chart

# dataFile() to create a data vector of data vector names

# chartHeader() to make a csv and data frame from the chart header info

# dataParse() to create a data frame for each exam with all series and all charts

######## other helper functions 

# eventParse()

# stimCheck()

# dataReduceFn()

# cleanUp()

# fixDupFn()

# centerColumn()

# setColRange()

# fixTagsFn()

####

{
  # source a script to load the addColumnsFn() function
  source(paste0(RPath, 'addColumns.R'), echo=FALSE)
  
  # source a script for the dataReduceFn()
  source(paste0(RPath, 'dataReduce.R'), echo=FALSE)
  
  # dataReduceFn needs another function for the time scale
  source(paste0(RPath, 'toMinSec.R'), echo=FALSE)
}


##############################   header Files  ############################



# function to separate header information for each chart
headerFile <- function(x=examCharts, 
                       makeVector=TRUE, 
                       saveTXT=FALSE,
                       makeDF=TRUE, 
                       saveCSV=FALSE) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the header info from the data
  # called recursively by the parseUniqueExams function
  
  # x input is a vector of NCCA ASCII filenames from the getCharts function
  # makeVector=TRUE will save a character vector of the header data to the global environment
  # saveTXT=TRUE will save the header data to a text file in the current working directory
  # output is a vector of header names for a unique data frame for each chart header
  # will also create a data frame of a single header file for the input exam (all charts)
  
  # this function can be recursively called by the parseUniqueExam function 
  
  # this function can recursively create .txt files 
  # from the header info of all charts in the "fileNames" vector
  
  ####
  
  # make a vector to hold the names of the headerFiles
  
  examCharts <- x
  
  outNames <- examCharts
  
  # fix problem characters
  
  {
    
    outNames <- str_replace(outNames, "Cleaned Copy", "CleanedCopy")
    outNames <- str_replace_all(outNames, "D&_", "")
    
    outNames <- str_replace_all(outNames, "D\\$_", "")
    outNames <- str_replace_all(outNames, "\\$", "")
    
    outNames <- str_replace_all(outNames, "D%_", "")
    outNames <- str_replace_all(outNames, "D#_", "")
    
    outNames <- str_replace_all(outNames, "[:punct:]", "")
    
    # do not need these when using the [:punct:]
    #   outNames <- str_replace(outNames, ")-", "_")
    #   outNames <- str_replace(outNames, " \\(", "_")
    #   outNames <- str_replace_all(outNames, "\\.", "_")
    #   outNames <- str_replace_all(outNames, " ", "_")
    #   outNames <- str_replace_all(outNames, "-", "_")
    
  }
  
  # append "_header" to the names
  outNames <- paste(str_sub(outNames, 1, -1), "_header", sep = "") 
  # start at the 4th character
  
  # add the file extension
  outNamesTXT <- paste(outNames, ".txt", sep = "")
  
  # NCCA ASCII spec, 15 chars
  matchString1 <- "Sample     Time"
  # alternate ASCII spec for Don Krapohl's 2019 data, 18 chars
  matchString2 <- "Sample        Time"
  
  
  # loop over the file names and read the files
  i=1
  for (i in 1:length(examCharts)) {
    # read 150 lines from each file
    dataLines <- readLines(examCharts[i], n = 150L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # locate the begining of the time-series data - the end of the header section
    endLine <- 
      pmatch(matchString1, strtrim(str_trim(dataLines, side = "both"), nchar(matchString1)) ) - 1
    if(is.na(endLine)) {
      endLine <- 
        pmatch(matchString2, strtrim(str_trim(dataLines, side = "both"), nchar(matchString2)) ) - 1
    }
    # keep only the header lines
    dataLines <- dataLines[1:endLine]
    # create a character vector for each header 
    if(makeVector==TRUE) assign(strtrim(outNames[i], nchar(outNames[i])), dataLines, pos = 1)
    # save the txt file
    if(saveTXT==TRUE) cat(m, file = outNamesTXT[i], sep = "\n")
  }
  
  # return the character vector of header file names
  # assign("headerNames", str_sub(outNames, 1, -5), pos = 1)
  # return(str_sub(outNames, 1, -5)) 
  
  headerNames <- outNames
  
  # a loop to open all the header files in the current working directory
  # first make an empty data frame for the output
  myDF <- NULL
  i=1
  for (i in 1:length(headerNames)) {
    chartName <- headerNames[i]
    chartHeaders <- get(chartName)
    
    # not reading from disk
    #     chartHeaders <- readLines(fileName, 
    #                             n = -1, 
    #                             ok = TRUE, 
    #                             warn = FALSE, 
    #                             encoding = "UTF-8")
    
    #select the header lines and separate the header data
    headerEndLine <- pmatch("Event    Label Statement", chartHeaders) - 2
    if(is.na(headerEndLine)) {
      # alternate for Don Krapohl's 2019 data
      headerEndLine <- pmatch("Qs", str_trim(chartHeaders, side="both")) -2
    }
    chartHeaders <- chartHeaders[1:headerEndLine]
    
    # split the chart headers into lables and data
    chartHeaders <- strsplit(chartHeaders, ": ")
    # result is a list of character vectors each with 2 elements
    # the first is the name
    # then second element is the header data
    
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
    headerData <- c(str_sub(chartName, 1, -12), str_sub(chartName, -11, -8), headerData )
    hNames <- c("examName", "chartName", hNames)
    
    # rbind the header data to the output data frame
    headerData <- rbind.data.frame(headerData)
    colnames(headerData) <- as.vector(hNames)
    
    # check to see that the column names are the same
    if(!is.null(myDF)) {
      if(!identical(names(myDF), names(headerData))) {
        print(paste0("WARNING: column names are different for chart ", chartName))
        break()
      }
    }
    
    myDF <- rbind(myDF, headerData)
    
  } # end loop i to open all chart header vectors for the input exam
  
  #  set the name of the ouput object
  outName <- paste(strtrim(chartName, nchar(chartName) - 11), "_Header", sep="")
  
  # outName <- str_replace_all(outName, "D\\$_", "")
  # outName <- str_replace_all(outName, "\\$", "")
  
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
  
  # return a vector of the name of the input exam
  # return(strtrim(outName, nchar(outName) - 7))
  # return(str_sub(outNames, 1, -5))
  return(str_sub(outNames, 1, -1))
  
} # headerFile end

# headerFile() # this function is called recursively by the parseUniqueExams function


####################################   chart Header #######################


# chartHeader <- function(x=headerNames, makeDF=TRUE, saveCSV=FALSE) {
#   # function to make a data frame and .csv file of the header data for each exam
#   # input is a vector of names of the headers for each chart
#   # output is a vector of names of the names of the input exams for all charts
#   # called by the parseUniqueExams function
# 
#   ####
# 
#   headerNames <- x
# 
#   # a loop to open all the header files in the current working directory
#   # first make an empty data frame for the output
#   myDF <- NULL
#   # i=1
#   for (i in 1:length(headerNames)) {
#     chartName <- headerNames[i]
#     chartHeaders <- get(chartName)
# 
#     # not reading from disk
#     #     chartHeaders <- readLines(fileName,
#     #                             n = -1,
#     #                             ok = TRUE,
#     #                             warn = FALSE,
#     #                             encoding = "UTF-8")
# 
#     #select the header lines and separate the header data
#     chartHeaders <- chartHeaders[1:(pmatch("Event    Label Statement", chartHeaders) - 2)]
# 
#     # split the chart headers into lables and data
#     chartHeaders <- strsplit(chartHeaders, ": ")
#     # result is a list of character vectors each with 2 elements
#     # the first is the name
#     # then second element is the header data
# 
#     # get the header names
#     hNames <- NULL
#     for (j in 1:length(chartHeaders)) {
#       hNames <- c(hNames, chartHeaders[[j]][1])
#     }
# 
#     # get the header data
#     headerData <- NULL
#     for (k in 1:length(chartHeaders)) {
#       headerData <- c(headerData, chartHeaders[[k]][2])
#     }
# 
#     # add the chartName and examName
#     headerData <- c(str_sub(chartName, 1, -12), str_sub(chartName, -11, -8), headerData )
#     hNames <- c("examName", "chartName", hNames)
# 
#     # rbind the header data to the output data frame
#     headerData <- rbind.data.frame(headerData)
#     colnames(headerData) <- as.vector(hNames)
# 
#     # check to see that the column names are the same
#     if(!is.null(myDF)) {
#       if(!identical(names(myDF), names(headerData))) {
#         print(paste0("WARNING: column names are different for chart ", chartName))
#         break()
#       }
#     }
# 
#     myDF <- rbind(myDF, headerData)
# 
#   } # end loop i to open all chart header vectors for the input exam
# 
#   #  set the name of the ouput object
#   outName <- paste(strtrim(chartName, nchar(chartName) - 11), "_Header", sep="")
# 
#   # outName <- str_replace_all(outName, "D\\$_", "")
#   # outName <- str_replace_all(outName, "\\$", "")
# 
#   # and save it as a .csv in the cwd
#   if (saveCSV==TRUE) write.table(myDF,
#                                  file = paste(outName, ".csv", sep=""),
#                                  append = FALSE,
#                                  quote = TRUE,
#                                  sep = ",", eol = "\n",
#                                  na = "NA", dec = ".",
#                                  row.names = FALSE,
#                                  col.names = TRUE,
#                                  qmethod = "double",
#                                  fileEncoding = "UTF-8")
# 
#   # save the chart header data frame to the global environment
#   if(makeDF==TRUE) assign(outName, value=myDF, pos = 1)
# 
#   # return a vector of the name of the input exam
#   return(strtrim(outName, nchar(outName) - 7))
# 
# } # chartHeader end

# chartHeader(x=headerNames, makeDF=TRUE, saveCSV=TRUE)


#####################################   stimulus events   #######################


stimEvents <- function(x=headerNames, 
                       saveCSV=FALSE, 
                       makeDF=TRUE, 
                       keepText=FALSE ) {
  # function to make a data frame and csv table of the stimulus text for all charts for each exam
  # and also create a .csv file and data frame with the stim text for each exam
  # also includes onset offset and answer data for each event
  
  # input x is a vector of names of data frames for the chart header info
  # output is a vector of names of the exams
  # called by the the parseUniqueExams function

  ####
  
  headerNames <- x
  
  # loop to process the stimulus text for each chart
  # first make null vector to cbind the data frame
  myDF <- NULL
  i=1
  for (i in 1:length(headerNames)) {
    chartName <- headerNames[i]
    chartHeaders <- get(chartName)
    
    # reset 
    skipThis <- FALSE
    
    # not reading from disk, using a vector instead    
    # chartHeaders <- readLines(fileName, 
    #                           n = -1, 
    #                           ok = TRUE, 
    #                           warn = FALSE, 
    #                           encoding = "UTF-8")
    
    # get the stimulus text lines
    startLine <- pmatch("Event    Label Statement", chartHeaders)
    if(is.na(startLine)) {
      # alternate for Don Krapohl's 2019 data
      startLine <- pmatch("Qs", str_trim(chartHeaders, side="both")) 
      skipThis <- TRUE
    }
    
    endLine <- pmatch("Event    Label      Begin        End     Answer",
                      chartHeaders) - 2
    
    if(is.na(endLine)) {
      # alternate for Don Krapohl's 2109 data
      endLine <- pmatch("Qs       Begin         End      Answer",
                        str_trim(chartHeaders, side="both") ) - 1
      skipThis <- TRUE
    }
    
    # Sep 11, 2023
    # Limestone omits the statements
    if(endLine == startLine) {
      # headerFileStimuli <- c(chartHeaders[startLine],
      #                        str_sub(chartHeaders[(startLine+3):(length(chartHeaders) - 1)], 1, 14))
      
      headerFileStimuli <- ""
    } else { 
      headerFileStimuli <- chartHeaders[startLine:endLine]
    }
    
    # remove blank lines
    headerFileStimuli <- headerFileStimuli[headerFileStimuli != ""]
    
    # increment the i loop to the next chart if no stimulus events
    if(length(headerFileStimuli)<=1) next()
    
    if(isTRUE(skipThis)) next() # myDF will remain NULL
    
    # create a null vector to hold the stimulus text
    stimulusLines <- NULL
    
    # a loop to fix wrapped text lines
    for (j in 1:length(headerFileStimuli)) { # 2-22-2014 fixed so it handles multiple wrapped lines 
      # i <- 1 # for testing
      if(strtrim(headerFileStimuli[j], 6) == "      ") {
        stimulusLines[length(stimulusLines)] <- 
          paste(stimulusLines[length(stimulusLines)], 
                str_sub(headerFileStimuli[j], 16, nchar(headerFileStimuli[j])), 
                sep = "") 
      } # end if
      if(strtrim(headerFileStimuli[j], 6) != "      ") {
        stimulusLines <- c(stimulusLines, headerFileStimuli[j]) 
      } # end if
    } # end for loop j
    
    # replace it
    headerFileStimuli <- stimulusLines
    
    # set the header row
    # headerFileStimHeader <- headerFileStimuli[1]
    # headerFileStimHeader <- c(strtrim(headerFileStimHeader, 5), str_sub(headerFileStimHeader, 10, 14), str_sub(headerFileStimHeader, 16, 25))
    
    # get the event numbers
    eventNumb <- str_trim(strtrim(headerFileStimuli, 6), side = "both") 
    
    # get the event labels
    Labels <- str_trim(str_sub(headerFileStimuli, 7, 14), side = "both")
    # fix problem question tags 2-16-2017
    Labels <- fixTagsFn(Labels)
    
    
    {
      
      # fix alphanumeric questions 2021-08-27
      theseEventNames <- Labels
      
      # {
      #   theseEventNames[which(theseEventNames == "2R")] <- "R2"
      #   theseEventNames[which(theseEventNames == "3R")] <- "R3"
      #   theseEventNames[which(theseEventNames == "4R")] <- "R4"
      #   theseEventNames[which(theseEventNames == "5R")] <- "R5"
      #   theseEventNames[which(theseEventNames == "6R")] <- "R6"
      #   theseEventNames[which(theseEventNames == "7R")] <- "R7"
      #   theseEventNames[which(theseEventNames == "8R")] <- "R8"
      #   theseEventNames[which(theseEventNames == "9R")] <- "R9"
      #   theseEventNames[which(theseEventNames == "10R")] <- "R10"
      #   theseEventNames[which(theseEventNames == "11R")] <- "R11"
      #   
      #   theseEventNames[which(theseEventNames == "3C")] <- "C3"
      #   theseEventNames[which(theseEventNames == "4C")] <- "C4"
      #   theseEventNames[which(theseEventNames == "5C")] <- "C5"
      #   theseEventNames[which(theseEventNames == "6C")] <- "C6"
      #   theseEventNames[which(theseEventNames == "7C")] <- "C7"
      #   theseEventNames[which(theseEventNames == "8C")] <- "C8"
      #   theseEventNames[which(theseEventNames == "9C")] <- "C9"
      #   theseEventNames[which(theseEventNames == "10C")] <- "C10"
      #   
      #   theseEventNames[which(theseEventNames == "1N")] <- "N1"
      #   theseEventNames[which(theseEventNames == "2N")] <- "N2"
      #   theseEventNames[which(theseEventNames == "3N")] <- "N3"
      #   
      #   theseEventNames[which(theseEventNames == "4K")] <- "K4"
      # }
      
      Labels <- theseEventNames
      
    }
    
    
    
    # fix duplicate event labels 2-16-2017
    eventLabels <- fixDupFn(Labels)
    eventLabels[1] <- "eventLabel"
    
    # get the stimulus text
    stimText <- as.character(str_trim(
      str_sub(headerFileStimuli, 16, nchar(headerFileStimuli)),
      side = "both") 
    )
    
    # use an input parameter to replace the stimulus text with the question tag
    if(!isTRUE(keepText)) {
      stimText[2:length(stimText)] <- Labels[2:length(Labels)]
    }
    
    # build the stimTextDF data frame 
    stimTextDF <- cbind.data.frame(Event=eventNumb[1:length(stimText)], 
                                   Label=Labels[1:length(stimText)],
                                   eventLabel=eventLabels[1:length(stimText)],
                                   Statement=stimText, 
                                   stringsAsFactors=FALSE)
    # View(stimTextDF)
    
    # use the first row as the column names
    # names(stimTextDF) <- stimTextDF[1,]
    # names(stimTextDF) <- c("Event", "Label", "eventLabel", "Statement")
    
    # add the examName seriesName and chartName columns
    stimTextDF <- cbind(rep(str_sub(chartName, 1, -12), times=nrow(stimTextDF)), 
                        rep(str_sub(chartName, -11, -11), times=nrow(stimTextDF)),
                        rep(str_sub(chartName, -10, -8), times=nrow(stimTextDF)),
                        stimTextDF)
    
    names(stimTextDF) <- c("examName", "seriesName", "chartName", names(stimTextDF)[-c(1:3)])
    
    ### check for empty events with no Label # added 5/12/2016
    stimTextDF <- stimTextDF[which(stimTextDF$Label!=""),]
    
    if(length(stimTextDF) > 1) {
      stimTextDF <- stimTextDF[2:nrow(stimTextDF),]
    } else { 
      stimTextDF <- stimTextDF[NULL,]
    }
    
    # for testing
    # assign("stimTextDF", stimTextDF, pos=1)
    
    # and finally cbind the data frame to the output data frame
    
    if(length(stimTextDF)==0) {
      myDF <- stimTextDF
    } else {
      myDF <- rbind(myDF, stimTextDF)
    }
    # View(myDF)
    
  } # end loop i to iterate through the exam charts
  
  ####
  
  # only if the myDF is not NULL
  # myDF will be NULL if there are no stimulus events
  if(length(myDF) != 0) {
    
    myDF$examName <- as.character(myDF$examName)
    myDF$seriesName <- as.character(myDF$seriesName)
    myDF$chartName <- as.character(myDF$chartName)
    myDF$Statement <- as.character(myDF$Statement)
    
    # 20200118
    # myDF$Begin <- as.numeric(myDF$Begin)
    # myDF$End <- as.numeric(myDF$End)
    # myDF$Answer <- as.numeric(myDF$Answer)
    
    # 1/8/2017
    # fix some potential problems with Limestone question tags
    myDF$Label <- gsub("S S", "S", myDF$Label)
    myDF$Label <- gsub("SY SY", "SY", myDF$Label)
    
    myDF$Label <- gsub("R R", "R", myDF$Label)
    myDF$Label <- gsub("C C", "C", myDF$Label)
    myDF$Label <- gsub("N N", "N", myDF$Label)
    myDF$Label <- gsub("I I", "I", myDF$Label)
    myDF$Label <- gsub("SR SR", "S", myDF$Label)
    myDF$Label <- gsub("RS RS", "S", myDF$Label)
    
  } # end if(!is.null(myDF)) 
  
  # set the filename and save the file
  outName <- paste(strtrim(chartName, nchar(chartName) - 11), "_Stimuli", sep = "")
  
  # outName <- str_replace_all(outName, "D\\$_", "")
  # outName <- str_replace_all(outName, "\\$", "")
  
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
  return(str_sub(chartName, 1, -12))
  
} # end stimEvents() function

# stimEvents(x=headerNames, makeDF=TRUE, saveCSV=FALSE)


##################################### event Table  ##########################

# function to make a data frame and csv table of all stimulus events
# used to add begin end and answer events to the _Stimuli data frame
eventTable <- function(x=headerNames, saveCSV=FALSE, makeDF=FALSE) {
  # function to make a data frame and csv table of all stimulus events
  # called by the parseUniqueExams function
  
  # input x is a vector of names of data frames for the chart header info
  # makeDF and saveCSV input parameters set to FALSE by default 
  
  # when makeDF and saveCSV are both FALSE the Begin End and Answer Columns
  # are added to the _Stimuli data frame
  # that was created by the stimEvents() function
  # setting TRUE will create a .csv file and data frame 
  # of the events for each exam 
  # but will not add the info to the _Stimuli data frame
  
  # output is a vector of names of the exams
  
  ####
  
  headerNames <- x
  
  # each headerName is a separate chart within an exam
  
  # make a null vector to build the output
  myDF <- NULL
  # loop through all the exam charts 
  # to make a data frame and csv table of all stimulus events
  i=1
  for (i in 1:length(headerNames)) {
    
    {
      
      chartName <- headerNames[i]
      
      # get the header info
      header <- get(chartName)
      
      # reset
      skipThis <- FALSE
      useAlternate <- FALSE
      
      # not reading from disk
      #     header <- readLines(fileName, 
      #                             n = -1, 
      #                             ok = TRUE, 
      #                             warn = FALSE, 
      #                             encoding = "UTF-8")
      
    }
    
    {
      
      ## get the lines with stimulus text statements ##
      
      startLine <- pmatch("Event    Label      Begin        End     Answer",
                          header)
      if(is.na(startLine)) {
        # alternate for Don Krapohl's 2109 data
        startLine <- pmatch("Qs       Begin         End      Answer",
                          str_trim(header, side="both") )
        skipthis <- TRUE
      }
      endLine <- length(header)
        
      headerFileEvents <- header[startLine:endLine]
      
      # remove blank lines
      headerFileEvents <- headerFileEvents[headerFileEvents != ""]
      
      if(length(headerFileEvents) < 2) next()
      
    }
    
    {
      
      ## get the event labels and event numbers ##
      
      # column widths for alternate
      # 5 6 11 11 11 with 1 space in between each column
      
      # get the event numbers
      eventNumb <- str_trim(strtrim(headerFileEvents, 6), side = "both")
      if(paste(eventNumb, collapse="") == "") {
        useAlternate <- TRUE
        eventNumb <- c("Event", 1:(length(headerFileEvents)-1))
      }
      
      # get the event labels
      startChar <- 7
      endChar <- ifelse(isTRUE(useAlternate), 12, 14)
      Labels <- 
        str_trim(str_sub(headerFileEvents, startChar, endChar), side = "both")
      # fix problem question tags 2-16-2017
      Labels <- fixTagsFn(Labels)
      Labels[1] <- "Label"
      
      # fix duplicate event labels 2-16-2017
      eventLabels <- fixDupFn(Labels)
      eventLabels[1] <- "eventLabel"
      
    }
    
    {
      
      ## get the event onset offset and answer indices ##
      
      onsetStartChar <- ifelse(isTRUE(useAlternate), 14, 15)
      offsetStartChar <- ifelse(isTRUE(useAlternate), 26, 26)
      answerStartChar <- ifelse(isTRUE(useAlternate), 38, 37)
      
      onsetEndChar <- ifelse(isTRUE(useAlternate), 24, 25)
      offsetEndChar <- ifelse(isTRUE(useAlternate), 36, 36)
      answerEndChar <- ifelse(isTRUE(useAlternate), 48, 47)
      
      # get the event onset
      eventOnset <- 
        str_trim(str_sub(headerFileEvents, onsetStartChar, onsetEndChar), side = "both")
      # get the event offset
      eventOffset <- str_trim(str_sub(headerFileEvents, offsetStartChar, offsetEndChar), side = "both")
      # get the event answer
      eventAnswer <- str_trim(str_sub(headerFileEvents, answerStartChar, answerEndChar), side = "both")
      
    }
    
    {
      
      ## make the event table ##
      
      eventDF <- as.data.frame(cbind(eventNumb, 
                                     Labels,
                                     eventLabels,
                                     eventOnset, 
                                     eventOffset, 
                                     eventAnswer), 
                               row.names = NULL, 
                               stringsAsFactors = FALSE)
      names(eventDF) <- eventDF[1,]
      if(isTRUE(useAlternate)) {
        names(eventDF) <- c("Event", "Label", "eventLabel", "Begin", "End", "Answer")
      }
      eventDF <- eventDF[2:nrow(eventDF),]
      # View(eventDF)
      
      # assign("eventDF", eventDF, envir=.GlobalEnv)
      # stop()

      # a loop to remove lines with no data
      eventDF2 <- NULL
      # for (j in 1:nrow(as.vector(eventDF))) {
      for (j in 1:nrow(eventDF)) {
        # modified Oct 12, 2022 to remove as.vector wrappings - not sure why these were used
        if (as.vector(eventDF[j,2]) != "") {
          
          # if (nchar(as.character(eventDF[j,4])) == 0) { eventDF[j,4] <- as.vector(eventDF[j,3]) }
          if (nchar(as.character(eventDF[j,4])) == 0) { eventDF[j,4] <- eventDF[j,3] }
          # if (nchar(as.character(eventDF[j,5])) == 0) { eventDF[j,5] <- as.vector(eventDF[j,4]) }
          if (nchar(as.character(eventDF[j,5])) == 0) { eventDF[j,5] <- eventDF[j,4] }
          # eventDF2 <- rbind(as.vector(eventDF2), as.vector(eventDF[j,]))
          
          # eventDF2 <- rbind(as.vector(eventDF2), as.vector(eventDF[j,]))
          eventDF2 <- rbind.data.frame(as.vector(eventDF2), as.vector(eventDF[j,]))
        }                   
      }
      
    }
    
    {
      
      ## re-order the events ##
      
      # necessary because Lafayette NCCAASCII output may have the events out of order
      # when there is an annotation during an event
      # because events are ordered by the event end not the event onset
      # commented out 12-8-2017 because it fails when events are not numbered sequentially
      # eventDF2 <- eventDF2[as.numeric(eventDF2$Event),]
      
      # add the examName seriesName and chartName columns
      eventDF2 <- cbind(rep(str_sub(chartName, 1, -12), times=nrow(eventDF2)), 
                        rep(str_sub(chartName, -11, -11), times=nrow(eventDF2)),
                        rep(str_sub(chartName, -10, -8), times=nrow(eventDF2)),
                        eventDF2)
      names(eventDF2) <- c("examName", "seriesName", "chartName", names(eventDF2)[-c(1:3)])
      # View(eventDF2)
      
    }

    ### may need to check to ensure that onset offset and answers
    ### all exist on different rows
    
    # rbind the data frame with the output
    myDF <- rbind(myDF, eventDF2)
   
  } # end loop i through the charts (header names) for the input exam
  # View(myDF)
  
  # only if there is some data
  # if(!is.null(myDF)) 
  
  # myDF <- as.data.frame(myDF)
    
  {
    
    ## add the event onset offset and answer index columns to the stimulus data frame ##
    
    # cbind the onset offset and answer events to the _Stimuli data frame
    # and save the result as a .csv and data frame
    # but only when the events data frame itself is not saved as a data frame or csv
    # if (saveCSV==FALSE && makeDF==FALSE) {
    
    # make the name of the stimuli data frame
    stimDFName <- paste0(strtrim(chartName, nchar(chartName) - 11), "_Stimuli")
    
    # get the stimuli data frame
    # exists(stimDFName)
    stimDF <- get(stimDFName, pos=1)
    # View(stimDF)
    
    # June 6, 2023, stop if the event labels are incorrect
    if(nrow(myDF) != nrow(stimDF)) {
      print(chartName)
      print("incorrect event labels")
      print("stimDF:")
      print(stimDF$Label)
      print("myDF:")
      print(myDF$Label)
      stop("incorrect event labels")
    }
    
    if(is.null(stimDF)) {
      myDF <- cbind(myDF[,1:9],
                    Statement=myDF$eventLabel,
                    row.names=NULL)
    } else {
      myDF <- cbind(stimDF[,1:6], 
                    myDF[,7:9], 
                    stimDF[,7], 
                    row.names=NULL)    
    }
    
    names(myDF) <- c("examName", "seriesName", "chartName", "Event", "Label", "eventLabel", "Begin",  "End", "Answer", "Statement")
    # make a new data frame
    # View(myDF)
    
    # make sure the text statement is a character column
    myDF$Statement <- as.character(myDF$Statement)
    
    # assign it to the global environment
    assign(stimDFName, myDF, pos = 1)
    
  }
    
  # set the filename
  outName <- paste(strtrim(chartName, nchar(chartName) - 13), "_Events", sep = "")
  
  if (saveCSV==TRUE) { write.table(myDF, 
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
  }
  
  if (makeDF==TRUE) { assign(outName, myDF, pos = 1) }
    
  # } # end !is.null(mydF)
  
  #ouput the name of the exam
  return(str_sub(chartName, 1, -12))
  
} # eventTable end

# eventTable(x=headerNames, makeDF=FALSE, saveCSV=FALSE)


#####################################   data File   ########################


# function to separate the time series data from the header info
dataFile <- function(x=examCharts, makeVector=TRUE, saveTXT=FALSE) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the time series data by removing the header lines
  # and create a vector of data names
  # this function is called recursively by the parseUniqueExam function
  
  # will also create a data frame for each data file
  
  # input is a vector of NCCAASCII charts for the exam
  
  # output is a vector of names of unique objects for each data file
  
  # this function can be recursively called by the parseUniqueExam function 
  
  # this function can recursively create .txt files 
  # from the data info of all charts in the "fileNames" vector
  
  ####
  
  # make a null vector to hold the names of the data Files
  
  examCharts <- x
  
  outNames <- examCharts
  
  # fix problem characters
  outNames <- str_replace(outNames, "Cleaned Copy", "CleanedCopy")
  outNames <- str_replace_all(outNames, "D&_", "")
  outNames <- str_replace_all(outNames, "D\\$_", "")
  outNames <- str_replace_all(outNames, "\\$", "")
  
  outNames <- str_replace_all(outNames, "D%_", "")
  outNames <- str_replace_all(outNames, "D#_", "")
  
  outNames <- str_replace_all(outNames, "[:punct:]", "")
  
  # do not need these when using the [:punct:]
  #   outNames <- str_replace(outNames, ")-", "_")
  #   outNames <- str_replace(outNames, " \\(", "_")
  #   outNames <- str_replace_all(outNames, "\\.", "_")
  #   outNames <- str_replace_all(outNames, " ", "_")
  #   outNames <- str_replace_all(outNames, "-", "_")
  
  # append "_data" to the names
  outNames <- paste(outNames, "_data", sep = "") # 
  
  # NCCA ASCII spec, 15 chars
  matchString1 <- "Sample     Time"
  # alternate ASCII spec for Don Krapohl's 2019 data, 18 chars
  matchString2 <- "Sample        Time"
  
  
  # loop over the file names and read the files
  i=1
  for (i in 1:length(examCharts)) {
    # read the lines from each file
    dataLines <- readLines(examCharts[i], n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # locate the begining of the time-series data - the end of the header section
    startLine <- 
      pmatch(matchString1, str_trim(strtrim(dataLines, nchar(matchString1)), side = "both"))
    if(is.na(startLine)) {
      startLine <- 
        pmatch(matchString2, strtrim(str_trim(dataLines, side = "both"), nchar(matchString2)) )
    }
    
    # keep only the time series data lines
    dataLines <- dataLines[startLine:length(dataLines)]
    # create a character vector for the data for each chart
    if(makeVector==TRUE) assign(outNames[i], dataLines, pos = 1)
    # save the .TXT file
    if(saveTXT==TRUE) cat(dataLines, file = paste(outNames[i], ".txt", sep=""), sep = "\n")
  }
  
  # return the character vector of data file names
  return(str_sub(outNames, 1, -1))
  
} # end dataFile() function

# dataFile() # this function is called in a loop by the parseUniqueExams function


#################################   data Parse   ########################


dataParse <- function(x=dataNames, y=thisExamName, saveCSV=FALSE, makeDF=TRUE) {
  # function to read the NCCA ASCII time series data to a data frame
  # also create csv version of the time series data
  # input is a vector of exam names from the output of the dataFile function
  # output is a vector of exam names 
  # this function is not vectorized for multiple exams
  # is called by the parseUniqueExams() function in NCCAASCII_Parse.R
  #
  ####
  
  {
    if(!exists("dataNames")) dataNames <- x
    if(!exists("thisExamName")) thisExamName <- y
    
  }
  
  {
  
    # intialize some empty objects
    outDF <- NULL # for the output
    chartDF <- NULL # for the working chart in the i loop
    
  }
  
  ### loop over the vector of chart data files ###
  
  i = 1
  for(i in 1:length(dataNames)) {
    
    {
      # first make a variable to hold the name of the chart data
      currentChartName <- str_sub(dataNames[i], 1, -1)
      
      # may be necessary with Axciton charts
      currentChartName <- gsub("\\$", "\\\\$", currentChartName)
      
      # reset this
      useAlternate <- FALSE
      
      # get the first text row, with all column names
      headerRow <- get(currentChartName)[1]
      # nchar(headerRow)
      
      # check which NCCA ASCII format using the header row
      # the NCCA ASCI spec uses "UPneumo" and LPnuemo"
      # while the NCCA pReview application seems to use "Upneumo" and "Lpneumo"
      if(str_sub(headerRow, 25, 35) != "    UPneumo") {
        useAlternate <- TRUE
      }
      
    }
    
    ### initialize the chart DF from the NCCA ASCII data ###
    
    if(isTRUE(useAlternate)) {
      
      # alternate for Don Krapohl's 2019 data
      
      # set the number of data columns
      # use the first row of the chart data vector
      # and divide by 12 to determine the number of columns 
      # first two columns are Sample and time and are 12 chars each
      # last column is Event and is 6 chars
      dataCols <- nchar(str_sub(get(currentChartName)[1], 25, -7L)) / 12
      
      # cols <- dataCols
      colWidths <- rep(12, dataCols)
      
      # read the column names from the data vector
      cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                              widths = c(12, 12, colWidths, 6), 
                                              header = FALSE, 
                                              skip = 0,
                                              n = 1))),
                         side = "both" )
      
      # read the data from the data vector for this chart
      # skip the first (column labels) row
      chartDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                          widths = c(12, 12, colWidths, 6), 
                          header = FALSE, 
                          skip = 1,
                          col.names = cNames,
                          n = -1L,
                          stringsAsFactors=FALSE ) 
      
      # chartDF$Label <- as.character(chartDF$Label)
      
      # re-order the colums to the normal
      chartDF <- chartDF[,c(1,2, ncol(chartDF), 3:(ncol(chartDF)-1))]
      
      # reset the column names
      names(chartDF) <- c("Sample", "Time", "Label", "UPneumo", "LPneumo", "EDA1", "Cardio1")
      
      # fix the answers and Labels
      chartDF$Label <- str_trim(chartDF$Label, side="both")
      chartDF$Label[which(chartDF$Label == ".")] <- ""
      chartDF$Label[which(chartDF$Label == "Ans")] <- "No"
      # all answers are No at this point, regardless of the actual answer"
      # chartDF$Label[which(chartDF$Label != "")]
      chartDF$Label <- toupper(chartDF$Label)
      
      # 2025 June 19 # fix the question labels in the chartDF
      chartDF$Label <- fixTagsFn(x=chartDF$Label)
      
      chartDF$Time <- str_trim(chartDF$Time, side="both")
      chartDF$Time <- str_sub(paste0("0", chartDF$Time), -8, -1)
      
    } # end if isTRUE for useAlternate
    
    ### normal process - for normal NCCA ASCII data ###
    
    if(!isTRUE(useAlternate)) {
      
      # for standard NCCA ASCII output
      
      # set the number of data columns
      # use the first row of the chart data vector
      # and divide by 11 to determine the number of columns 
      dataCols <- nchar(str_sub(get(currentChartName)[1], 25, -1L)) / 11
      
      # cols <- dataCols
      colWidths <- rep(11, dataCols)
      
      # read the column names from the data vector
      cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                              widths = c(6, 9, 9, colWidths), 
                                              header = FALSE, 
                                              skip = 0,
                                              n = 1))),
                         side = "both" )
      
      # read the data from the data vector for this chart
      # skip the first (column labels) row
      chartDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                          widths = c(6, 9, 9, colWidths), 
                          header = FALSE, 
                          skip = 1,
                          col.names = cNames,
                          n = -1L,
                          stringsAsFactors=FALSE ) 
      
      chartDF <- chartDF[which(!is.na(chartDF$Sample)),]
      
      # chartDF$Label <- as.character(chartDF$Label)
      
      # 2025 June 19 # fix the question labels in the chartDF
      chartDF$Label <- fixTagsFn(x=chartDF$Label)
      # unique(chartDF$Label)
      
    } # end if is FALSE  useAlternate
    
    # we now have a data frame for the chart #
    
    ### always include the Move1 column (required sensor) if it is missing ###
    
    if(!("Move1" %in% names(chartDF))) {
      
      # June 10, 2025 
      # always include a Move1 column (required sensor) even when it is missing
      
      chartDF$Move1 <- "-9.9"
      
      # dataCols <- dataCols + 1
      dataCols <- ncol(chartDF) - 3
      # include the width for the added activity sensor column
      colWidths <- c(colWidths, colWidths[length(colWidths)])
      cNames <- c(cNames, "Move1")      
      cNames <- unique(cNames)
      
      # assign("chartDF", chartDF, pos=1)
      
    }
    
    ### always include the PPG1 column (not a required sensor) if it is missing ###
    
    if(!("PPG1" %in% names(chartDF)) && isTRUE(includePLEData)) {
      
      # # June 10, 2025 
      # # always include a PPG1 column (not a required sensor) even when it is missing
      # 
      # chartDF$PPG1 <- "-9.9"
      # 
      # # dataCols <- dataCols + 1
      # dataCols <- ncol(chartDF) - 3
      # # include the width for the added activity sensor column
      # colWidths <- c(colWidths, colWidths[length(colWidths)])
      # cNames <- c(cNames, "PPG1")      
      # cNames <- unique(cNames)
      # 
      # # assign("chartDF", chartDF, pos=1)
      
    }
    
    ### check some conditions ###
    
    {
      # increment the i loop if there is no data
      if(length(which(!is.na(chartDF$Sample))) == 0) next()
      
      # increment the i loop if the chart is a short or aborted fragment
      if(length(which(!is.na(chartDF$Sample))) <= 300) next()
      
      # make the labels all upper case and remove the "-" character
      chartLabels <- toupper(str_replace_all(chartDF$Label, "[ -]", ""))
        
      # Feb 3, 2023
      # increment the i loop for any chart that is missing the X or XX announcement
      if(stopXXX) {
        if(any(!(c("X", "XX") %in% chartLabels))) {
          print("missing X and/or XX")
          # stop()
          next()
        }
      }
    }
    
    ### add some columns ###
    
    {
      # add a column for the chart name
      chartDF <- cbind(rep(str_sub(currentChartName, -8, -6), nrow(chartDF)), chartDF)
      # add a column for the series name
      chartDF <- cbind(rep(str_sub(currentChartName, -9, -9), nrow(chartDF)), chartDF)
      # add a column for the exam name
      chartDF <- cbind(rep(str_sub(currentChartName, 1, -10), nrow(chartDF)), chartDF)
      
      names(chartDF)[1:3] <- c("examName", "seriesName", "chartName")
      
      # assign("outDF", outDF, pos=1)
      # assign("chartDF", chartDF, pos=1)
      # View(chartDF)
    }
    
    ### check for columns that differ for the test charts ###
    
    {
      missingCols <- names(outDF)[which(!(names(outDF) %in% names(chartDF)))]
      if(length(missingCols)>0) {for(j in 1:length(missingCols)) {chartDF[missingCols[j]] <- numeric()}}
      
      missingCols <- names(chartDF)[which(!(names(chartDF) %in% names(outDF)))]
      if(length(missingCols)>0) {for(j in 1:length(missingCols)) {outDF[missingCols[j]] <- numeric()}}
    }
    
    ### resample the data to 30 cps if necessary ###
    
    {
      # source("~/Dropbox/dataReduce.R", echo=TRUE)
      
      # check if non Lafayette chart
      if(!grepl("^D&.*$", thisExamName[1])) {
        # set reSample to TRUE for non-lafayette charts
        reSample <- TRUE
      } else {
        # reduceSampleRate is a setting in the NCCAASCII_init.R script
        reSample <- reduceSampleRate
      }
      
      # call the dataReduceFn() from this script
      if(reSample==TRUE) {
        # cps is a parameter from the NCCAASCII_init.R script
        # dataReduceFn is a function in the dataReduce.R script
        
        # source(paste0(RPath, 'dataReduce.R'), echo=FALSE)
        chartDF <- dataReduceFn(chartDF=chartDF, newRate=cps)
        # View(chartDF)
      } 
      
      # at this point the chartDF exists at 30cps
    }
    
    #### check for overlapping events or overlapping annotations ####
    
    {
      
      thisExamName
      
    }
    
    ### rbind the DFs for all charts ###
    
    outDF <- rbind(outDF, chartDF)
    
    # View(chartDF)
    # View(outDF)
    
  } # end loop over i chart data files
  
  ###   fix some potential problems   ###
  
  {
    
    outDF$Label <- as.character(outDF$Label)
    outDF$Label <- toupper(str_replace_all(outDF$Label, "[- ]", ""))
    # outDF$Label <- toupper(outDF$Label)
    
    # outDF$Move1 <- as.numeric(outDF$Move1)
    
    # fix some potential problems with Limestone question labels
    # outDF$Label <- gsub("S S", "S", outDF$Label)
    # outDF$Label <- gsub("SS", "S", outDF$Label)
    # outDF$Label <- gsub("SY SY", "SY", outDF$Label)
    # outDF$Label <- gsub("R R", "R", outDF$Label)
    # outDF$Label <- gsub("RR", "R", outDF$Label)
    # outDF$Label <- gsub("C C", "C", outDF$Label)
    # outDF$Label <- gsub("CC", "C", outDF$Label)
    # outDF$Label <- gsub("SR SR", "S", outDF$Label)
    # outDF$Label <- gsub("RS RS", "S", outDF$Label)
    # outDF$Label <- gsub("SRSR", "S", outDF$Label)
    # outDF$Label <- gsub("RSRS", "S", outDF$Label)
    # outDF$Label <- gsub("N N", "N", outDF$Label)
    # outDF$Label <- gsub("I I", "I", outDF$Label)
    # outDF$Label <- gsub("II", "I", outDF$Label)
    # outDF$Label <- gsub("NN", "N", outDF$Label)
    
    # remove period characters 
    # for examiners who are silly enough to use them in question tags
    outDF$Label <- gsub("\\.", "", outDF$Label)
    
  }
  
  ### add columns for eventLabel, Events, stimText, and Answer ###
  
  {
    
    eventLabel <- "" # used to hold unique Labels when events are repeaated
    Events <- ""# 
    stimText <- "" # stimulus text statement"
    Answer <- "" # 
    
    outDF <- cbind.data.frame(outDF[,(1:6)],
                              eventLabel,
                              Events,
                              stimText,
                              Answer,
                              outDF[,(7:ncol(outDF))] )
    
    outDF$eventLabel <- as.character(outDF$eventLabel)
    outDF$Events <- as.character(outDF$Events)
    outDF$stimText <- as.character(outDF$stimText)
    outDF$Answer <- as.character(outDF$Answer)
    
  }
  
  ### add the centered data columns ###
  
  {
    
    # make a vector of names for some new columns for the centered data
    newColNames <- paste0("c_", names(outDF[11:ncol(outDF)]))
    
    # add the names to the output data frame
    for (m in 1:length(newColNames)) {
      outDF <- cbind(outDF, rep(0, times=nrow(outDF)))
      # name each new column
      names(outDF)[ncol(outDF)] <- newColNames[m]
    } 
    
  }
  
  ### call a function to add columns for signal processing ###
  # source(paste0(RPath, 'addColumns.R'), echo=FALSE)
  outDF <- addColumnsFn(x=outDF)
  
  ### construct the output ###
  
  {
    
    # create the output file name
    outName <- paste(str_sub(currentChartName, 1, -10), "_Data", sep = "")
    
    # save the data as a CSV
    if (saveCSV==TRUE) write.csv(outDF, 
                                 file=paste(outName, ".csv", sep = ""), 
                                 row.names = FALSE)
    
    # save the data as a data frame 
    if(makeDF==TRUE) assign(x=outName, value=outDF, pos = 1)
    
    
  }
  
  # visible output is the name of the last input exam
  return(str_sub(outName, 1, -1)) 
  
} # end dataParse() function

# dataParse(x=dataNames, makeDF=TRUE, saveCSV=TRUE) # called by the parseUniqueExams function in the NCCAASCIIParse.r script



############################   event parse   ##############################



eventParse <- function(x="_Stimuli$", makeDF=TRUE, saveCSV=FALSE) {
  # function to identify charts that have 2 CQs and 2 RQs 
  # and verify that all stimulus events are present in all charts
  # x is a regex string to identify the stimuli data frames
  # with 1 data frame with all event stimuli for all charts for each exam
  #
  # will also make a warning message if each event is not present in all charts
  #
  # type can be CQT, CIT, SPOT, RI, ACQT or NULL
  #
  # output is a data frame, "*_eventMatrix" assigned to the global env with all event labels for all charts
  #
  ####
  
  if(!exists("x")) x <- "_Stimuli$"
  
  # if(length(ls(pattern="_Stimuli$", pos=1)) == 0) stop("no stimulus data frame")
  
  # initialize a vector to hold the names of the _Stimuli data frames
  stimDFs <- NULL
  
  # populate vector of names of event stimuli data frames
  if(length(ls(pattern=x, pos=1)) > 0) stimDFs <- ls(pattern=x, pos=1)
  
  # proceed only if there are any event stimuli
  if(is.null(stimDFs)) {
    print("no stimulus data frame")
    return()
  }
  
  #### loop over the events ("*_Stimuli") for each exam
  i=1
  for (i in 1:length(stimDFs)) {
    # print(stimDFs[i])
    # create an empty warning message
    
    # work with the exam stimuli data frames
    examDF <- get(stimDFs[i])
    # View(examDF)
    
    # get the exam name
    # examName <- substr(stimDFs[i], start=1, stop=nchar(stimDFs[i])-8)
    examName <- unique(examDF$examName)[1]
    
    # get the chart names
    # uniqueCharts <- unique(as.character(examDF[,3]))
    # uniqueCharts <- unique(as.character(examDF[,'chartName']))
    # length(uniqueCharts)
    
    # get the numbers of the unique series
    # uniqueSeriesNames <- unique(substr(uniqueCharts, start=1, stop=1))
    # uniqueSeriesNames <- unique(as.character(examDF[,2]))
    uniqueSeriesNames <- unique(as.character(examDF[,'seriesName']))
    
    print(paste("exam", i, "of", length(stimDFs), "examName:", examName))
    
    ####
    
    # insert some operation here
    # not used at theis point
    
    ####
    
    #### make a loop for each unique series
    j=1
    for (j in 1:length(uniqueSeriesNames)) {
      seriesName <- uniqueSeriesNames[j]
      
      # initialize the warningMessage to NULl
      warningMessage <- NULL
      
      # make a separate data frames for the series
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      # View(seriesDF)
      
      # get the chart names
      # uniqueCharts <- unique(as.character(seriesDF[,3]))
      uniqueCharts <- unique(as.character(seriesDF[,'chartName']))
      
      # length(uniqueCharts)
      
      # initialize some variables 
      eventVectorNames <- NULL
      numberEvents <- NULL
      
      # iterate over the charts to make a vector for event labels 
      k=1
      for (k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        # get the vector of event lables for each chart
        eventVector <- seriesDF[seriesDF$chartName==chartName,"Label"]
        # remove "" elements
        eventVector <- eventVector[which(eventVector != "")]
        # determine the number of events and concatenate it to a vector numberEvents
        numberEvents <- as.character(c(numberEvents, length(eventVector)))
        # initialize a name and assign the lables to the vector
        dfName <- paste0("x_", uniqueCharts[k], "_labels") # need to append "x_."
        # assign it to a data frame
        assign(dfName, eventVector)
        # add the name of the event vector to a vector of names
        eventVectorNames <- c(eventVectorNames, dfName)
        
      } # end loop for k unique charts
      
      # we now have the name and number of events for each chart
      # eventVectorNames
      # numberEvents
      
      ####
      
      ### create a warning message if the number of events not the same for all charts
      if (length(unique(numberEvents))!=1) {
        warningMessage <- c(warningMessage, paste("01 Some charts in series",
                                                  seriesName,
                                                  chartName,
                                                  "have different numbers of stimulus events."))
      }
      
      # use a loop to fix the length of all event vectors 
      # to the max length for each exam
      maxEvents <- max(as.integer(numberEvents))
      numberEventVectors <- length(eventVectorNames)
      tempVector <- NULL
      l=1
      for (l in 1:numberEventVectors) {
        tempVector <- get(eventVectorNames[l])
        # adjust the length
        tempVector <- c(tempVector, rep("-", maxEvents - length(tempVector)))
        # assign(paste("eventVector", l, sep = ""), tempVector)
        assign(eventVectorNames[l], tempVector)
        # maybe write the csv to include the padded events
        #
      } # end loop l to fix the length of all event vectors to the max for each exam
      
      # initialize some needed vectors
      retainEventVectorNames <- NULL # vector for the names of charts with 2 CQs and 2 RQs
      allEventNames <- NULL
      
      ### for CQT exams ###
      
      # type scalar is set in the NCCAASCII_init.R script
      if (type=="CQT") {
        
        ### select charts with 2 or more CQs and RQs
        # also make a vector of all event labels for all charts with 2 CQs and 2 RQs
        
        # loop over the eventVectorNames 
        m=1
        for (m in 1:length(eventVectorNames)) {
          eventVectorName <- eventVectorNames[m] 
          # make a logical scalar if RQs >= 2
          RQs <- length(grep("R", get(eventVectorName), 
                             ignore.case = TRUE, 
                             perl = FALSE, 
                             value = FALSE,
                             fixed = FALSE, 
                             useBytes = FALSE, 
                             invert = FALSE) ) >= 2
          # make a logical scalar if CQs >= 2
          CQs <- length(grep("C", get(eventVectorName), 
                             ignore.case = TRUE, 
                             perl = FALSE, 
                             value = FALSE,
                             fixed = FALSE, 
                             useBytes = FALSE, 
                             invert = FALSE)) >= 2 
          ## keep the chart if >= 2 RQs and >= 2 CQs
          if (CQs && RQs) {
            retainEventVectorNames <- c(retainEventVectorNames, eventVectorNames[m])    
            # make a long vector of all events from all charts with 2 CQs and RQs
            allEventNames <- c(allEventNames, get(eventVectorNames[m]))
          } # end if
          ## warning if some charts in the series do not have >=2 CQs and >=2 RQs
          if(!CQs || !RQs) {
            warningMessage <- c(warningMessage, paste("02 Some charts in series", 
                                                      seriesName,
                                                      chartName,
                                                      "are excluded because they do not include >=2 CQs and >=2 RQs."))
          } # end it
        } # end for loop over m eventVectorNames
        
      } # end if type == CQT
      
      ### for all exams ###
      
      # if (type==NULL) retainEventVectorNames <- tempVector # 6-14-15 need to fix this
      
      # make a vector of unique event names
      # unique events may be less than max events because of repetion
      uniqueEventNames <- unique(allEventNames)
      
      ### make an output data frame ###
      
      tempOutVector <- NULL
      outputDF <- NULL
      
      if(!is.null(retainEventVectorNames)) {
        # n=1
        for (n in 1:length(retainEventVectorNames)) {
          tempOutVector <- get(retainEventVectorNames[n])
          # add the chart name
          tempOutVector <- c(examName, 
                             substr(retainEventVectorNames[n], 
                                    start=3, 
                                    stop=nchar(retainEventVectorNames[n])-7), 
                             tempOutVector)
          # add the exam name
          outputDF <- rbind(outputDF, tempOutVector)
        } # end for loop n to make the output data frame
        row.names(outputDF) <- NULL
        outputDF <- as.data.frame(outputDF)
      } # end if
      
      if(!is.null(outputDF)) {
        names(outputDF) <- c("examName", 
                             "chartName", 
                             paste0("E", 1:(ncol(outputDF)-2)))
      }
      
      ############ ouput ############
      
      if(!is.null(outputDF)) {
        
        outName <- paste0(examName, "_", seriesName, "_eventMatrix")
        
        if (makeDF==TRUE) {
          # if(!is.null(outputDF)) {
          assign(outName, outputDF, pos=1)
          # }
        }
        
        if(saveCSV==TRUE) {
          write.table(outputDF, file = paste0(outName, ".csv"), 
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
        
      }
      
      # save the warning message
      if(length(warningMessage) > 0) {
        if(isTRUE(makeDF)) {
          # name of the warning message
          warnName <- paste0(examName, "_", seriesName, "_event_warnings")
          # make a vector in the global env
          assign(warnName, as.data.frame(warningMessage), pos=1)
        }
        if (saveCSV==TRUE) {
          # save the warning message as txt
          fileName <- file(paste0(warnName, ".txt"), "w")
          writeLines(warningMessage, con=fileName)
          close(fileName)
        }
      } 
      
    } # end loop j for each unique series
    
  } # end loop i for each exam
  
  # output is the stimDFs data frame 
  # which is a vector of names of all "_Stimuli" data frames
  return(stimDFs)
  
} # end eventParse() function

# eventParse(x="_Stimuli$")

# need to modify this to make a data frame of all events for all charts and series regardless 
# and specify the selection of CQT RI CIT or ACQT


####################   stim check    ################


stimCheck <- function(x="_Stimuli$", makeDF=TRUE, saveCSV=FALSE) {
  # function to determine whether stimulus text is identical for questions on all charts
  # x input is a character string to identify the Stimuli data frames
  # output is a data frame (_stim_text_warning) and csv regarding differences in test stimuli
  #
  ####
  
  if(!exists("x")) x <- "_Stimuli$"
  
  #   stimNames <- list.files(path = ".", pattern = x, 
  #                            all.files = FALSE, 
  #                            full.names = FALSE, 
  #                            recursive = FALSE, 
  #                            ignore.case = FALSE, 
  #                            include.dirs = FALSE)
  
  # library(stringr)
  
  # stimNames <- ls(pattern=x, pos=1)
  stimNames <- ls(pattern=x, env=.GlobalEnv)
  
  # return if no _Stimuli data frams 
  if(length(stimNames) == 0) { return("no stimuli for this exam") }
  
  # add a parameter to load the .Stimuli data if necessary
  # not needed
  # uniqueExamNames <- unique(str_sub(stimNames, 1L, -9L)) # not needed
  
  # loop over each unique exam
  i=1
  for (i in 1:length(stimNames)) {
    # make a list of charts for each unique series  
    
    # get the unique exam name
    # examName <- strtrim(stimNames[i], (nchar(stimNames[i]) - 8)
    examName <- str_sub(stimNames[i], start=1, end=nchar(stimNames[i])-8)
    
    # not needed
    # name <- paste(uniqueExamNames[h], "....", "_stimuli.csv", sep = "") 
    
    # get the data frame 
    examStimuliDF <- get(stimNames[i], pos=1)
    
    # get the numbers of the unique series
    uniqueSeriesNames <- as.character(unique(examStimuliDF$seriesName))
    
    # initialize the series DF to NULL to avoid problems
    seriesStimuliDF <- NULL
    
    print(paste("exam", i, "of", length(stimNames), "examName:", examName))
    
    # iterate over each unique series
    j=7
    for (j in 1:length(uniqueSeriesNames)) {
      seriesName <- uniqueSeriesNames[j]
      
      # initialize the warningMessage to NULl
      warningMessage <- NULL
      
      # construct the name of the event matrix data frame
      eventMatrixName <- paste0(examName, "_", seriesName, "_eventMatrix")
      
      if(!exists(eventMatrixName)) {
        print(paste0("no event matrix for this series: ", examName, "_", seriesName))
        next()
      }
      
      # check to see if stimulus text are available and set a warning if not
      if(identical(as.character(examStimuliDF$Label), 
                   as.character(examStimuliDF$Statement))==TRUE) {
        warningMessage <- c(warningMessage, paste("stimulus text not available.", 
                                                  "stimuli are assumed identical for all charts in series",
                                                  seriesName))
      }
      
      # get the data frame from the event matrix
      seriesDF <- get(eventMatrixName, pos=1)
      
      # skip the series if no charts in the eventMatrix
      if(is.null(seriesDF)) next()
      if(nrow(seriesDF) < 2) next()
      
      # get the unique question tags for all charts in the series
      uniqueEvents <- unique(as.vector(c(t(seriesDF[,-c(1:2)]))))
      
      # get the number of uniqueEvents
      numberUniqueEvents <- length(uniqueEvents)
      
      ## use a loop to read the stimulus text from each chart ##
      
      # initialize an output data frame for differences in events 
      diffTable <- NULL # output data frame for differences
      
      # initialize a scalar to count the differences
      diffSum <- 0 # number of differences 

      # get the _Stimuli data frame with all text stimuli for all charts 
      # stimuliA <- get(paste0(examName, "_Stimuli"), pos=1)
      # use examStimuliDF instead
      
      # get the names for charts with CQs and RQs
      chartNames <- as.character(seriesDF$chartName)
      
      # get the number of charts in the series
      numberCharts <- length(chartNames)
      if(numberCharts == 1) next()
      
      k=1
      for (k in 1:length(chartNames)) {
        
        # compare each chart to the others
        chartNameA <- chartNames[k]
        
        # initialize a vector to hold the result of the comparison
        diffResult <- NULL
        
        # stimuliA <- read.csv(chartNameA, stringsAsFactors = FALSE)
        # stimTextA <- stimuliA$Statement
        
        # get the _Stimuli data frame with all text stimuli for all charts 
        # stimuliA <- get(paste0(examName, "_Stimuli"), pos=1)
        # use examStimuliDF instead
        
        # get all text statements for the current chart k
        theseRowsA <- 
          which(examStimuliDF$seriesName==seriesName & 
                  examStimuliDF$chartName==chartNameA)
        stimTextA <- as.character(examStimuliDF$Statement[theseRowsA])
        lengthA <- length(stimTextA)
        
        # nested loop to read each statement for each chart
        l=1
        for (l in 1:numberCharts) {
          chartNameB <- chartNames[l]
          # stimuliB <- read.csv(chartNameB, stringsAsFactors = FALSE)
          # stimTextB <- stimuliB$Statement
          # get a data frame of all stimuli for all charts in the series
          stimuliB <- get(paste0(examName, "_Stimuli"), pos=1)
          # get the text statements for the current chart l in series j
          theseRowsB <- 
            which(stimuliB$seriesName==seriesName & 
                    stimuliB$chartName==chartNameB)
          stimTextB <- as.character(stimuliB$Statement[theseRowsB])
          # compare A and B charts
          matchText <- stimTextA %in% stimTextB
          lengthMatch <- length(which(matchText))
          # cat the result with the diffResult
          diffResult <- c(diffResult, (lengthA == lengthMatch))
        } # end nested loop
        
        # sum the number of differences found
        diffSum <- diffSum + length(which(diffResult == FALSE))
        
        # bind the diffResult to the diffTable
        diffTable <- cbind(diffTable, diffResult)
        
        # probably this can be reduced by half instead of comparing every chart to every other
        
      } # end loop over k charts
      
      colnames(diffTable) <- chartNames
      rownames(diffTable) <- chartNames
      
      # construct a warning if event statements are not identical for all charts
      if (diffSum > 0) {
        diffMessage <- paste0("02 Stimulus text is not identical for some charts in series", seriesName)
        warningMessage <- c(warningMessage, diffMessage)
      } # end warning
      
      ### output
      
      # save the warning message
      if(length(warningMessage) > 0) {
        if(makeDF==TRUE) {
          # name of the warning message
          warnName <- paste0(examName, "_", seriesName, "_stim_warnings")
          # make a vector in the global env
          assign(warnName, as.data.frame(warningMessage), pos=1)
        }
        if(saveCSV==TRUE) {
          # save the warning message as txt
          fileName <- file(paste0(warnName, ".txt"), "w")
          writeLines(warningMessage, con=fileName)
          close(fileName)
        } # end if save CSV
      } # end if save warning message
      
      # set the output name for the txt and data frame
      outName <- paste0(examName,
                        "_", 
                        seriesName,
                        "_stim_text_warning")
      
      if(makeDF==TRUE && length(warningMessage > 0)) {
        assign(outName, as.data.frame(diffTable), pos = 1)
      } # end if makeDF
      
      if(saveCSV==TRUE) {
        write.table(diffTable, file = paste0(outName, ".csv"),
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
      } # end if saveCSV
      
    } # end loop over each j unique series
    
  } # end loop over each i unique exam
  
  # print(paste("stimulus events checked for", i, "exams"))
  return(paste("stimulus events checked for", i, "exams"))
  
} # end stimCheck()

# stimCheck(x="_Stimuli$")



##################################   clean Up   ##########################


# function to clean up the data vectors in the global environment
cleanUpFn <- function(removeEach=FALSE) {
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
} # end cleanUpFn() function


################# fix labels for repeated stimulus events ###################


fixDupFn <- function (x) {
  # function to fix duplicate events by appending to the name of the repeated event
  # used by the sigProc.R script
  # also used by the fixDup.R script
  # x is a vector of event names from the selected chart
  # output is a vector of corrected event names
  ###
  # initialize a new x vector
  xNew <- x
  # x2 <- x
  # keepIdx <- which(!(xNew %in% excludeEvents))
  # xNew <- xNew[keepIdx]
  # make a vector of indices for duplicated events
  dups <- which(duplicated(xNew, fromLast = TRUE))
  # exit if no duplicates
  if(anyDuplicated(x)==0) return(x)
  while(length(dups) > 0) {  
    dups <- which(duplicated(xNew, fromLast = TRUE))
    # xNew[dups] <- paste0(xNew[dups], 2:(length(dups)+1))
    xNew[dups] <- paste0(xNew[dups], "a")
    # x <- fixDup(xNew) # added 10-3-2015 to check for multiple repetitions of an event
  }
  # fixDup(xNew)
  # xNew <- x2[keepIdx] <- xNew
  return(xNew)
} # end fixDupFn() function


# fixDupFn(x=c("A", "B", "C", "A", "B", "C", "A"))


#######   center a column


centerColumn <- function(x) {
  # private function to set the onset value of the sensor data
  # to zero
  # x is a data column from a data frame of recorded time series data
  # output is a vector of centered values
  ifelse(max(x)==min(x),
         x <- x*0,
         x <- x - x[1]
  )
  return(x)
} # end private centerColumn() function


####### make a small private function to call for each column in each data frame


setColRange <- function(x, y) {
  # function to set each column range or scale 
  # x is a zero centered column from the data frome of recorded time series data
  # y is the max range value
  ###
  # set the range for the segment from 1 to 3 min or 1 to min or the entire segment
  ifelse(length(x>=5400),
         rangeVal <- max(x[1801:5400])-min(x[1801:5400]),
         ifelse(length(x>=3600),
                rangeVal <- max(x[1801:3600])-min(x[1801:3600]),
                rangeVal <- max(x)-min(x)
         )
  )
  rangeCoef <- y / rangeVal
  # in case there is a dead sensor with no activity
  ifelse(rangeVal==0,
         x <- x * 0,
         x <- x * rangeCoef
  )
  return(x)
} # end setColRange function


#######   fix problem question labels 


fixTagsFn <- function(x=chartDF$Label) {
  # function to fix problem question tags 
  # and ensure consistency in question labels
  # input x is a vector or data frame column containing the question tags
  # chartDF$Label
  # chartDF$eventLabel
  # output is a vector of the same length as the input
  ###
  
  # called by preproc function
  # called by eventTable function
  # called by the stimEvents function
  
  
  x <- toupper(x)
  
  x <- gsub("X_X", "X", x)
  x <- gsub("XX_XX", "XX", x)
  x <- gsub("XXX", "XX", x)
  x <- gsub("XXXX", "XX", x)
  
  x <- gsub("SR_Sa1", "SA", x)
  x <- gsub("SA1", "SA", x)
            
  x <- gsub("N_N", "N", x)
  x <- gsub("R_R", "R", x)
  x <- gsub("C_C", "C", x)
  
  x <- gsub("N_4KEY", "4KEY", x)
  
  x <- gsub("RELEVANT", "R", x)
  x <- gsub("RELEVA", "R", x)
  
  x <- gsub("COMPARISON", "C", x)
  x <- gsub("COMPAR", "C", x)
  
  x <- gsub("NEUTRAL", "N", x)
  x <- gsub("NEUTRA", "N", x)
  
  x <- gsub("IRRELEVANT", "I", x)
  x <- gsub("IRRELE", "I", x)
  x <- gsub("IR", "I", x)
  
  x <- gsub("SACRIFICE", "SA", x)
  x <- gsub("SACRIF", "SA", x)
  x <- gsub("SACRIFICE RELEVANT", "SA", x)
  
  x <- gsub("SYMPTOMATIC", "SY", x)
  x <- gsub("SYM", "SY", x)
  x <- gsub("SYMPTO", "SY", x)
  
  x <- gsub("INTRODUCTORY", "INT", x)
  x <- gsub("INTROD", "INT", x)
  
  x <- gsub(" +", "", x)
  
  # all punctuation characters
  x <- gsub("[[:punct:]]", "", x)
  
  x <- gsub("RR", "R", x)
  x <- gsub("R R", "R", x)
  x <- gsub("CC", "C", x)
  x <- gsub("C C", "C", x)
  x <- gsub("NN", "N", x)
  x <- gsub("N N", "N", x)
  x <- gsub("I", "I", x)
  x <- gsub("II", "I", x)
  x <- gsub("I I", "I", x)
  x <- gsub("SR", "SA", x)
  x <- gsub("SRSR", "SA", x)
  x <- gsub("SR SR", "SA", x)
  x <- gsub("RS RS", "SA", x)
  x <- gsub("RSRS", "SA", x)
  x <- gsub("S S", "SA", x)
  x <- gsub("SASA", "SA", x)
  x <- gsub("SS", "SA", x)
  x <- gsub("SY SY", "SY", x)
  x <- gsub("SYSY", "SY", x)
  x <- gsub("SYMSYM", "SY", x)
  x <- gsub("SYM SYM", "SY", x)
  
  x <- gsub(" ", "", x)
  x <- gsub("\\.", "", x)
  #
  return(x)
} # end fixTagsFn()



