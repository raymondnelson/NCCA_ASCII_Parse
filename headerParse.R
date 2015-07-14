############# R Functions for parsing NCCA ASCII header information ##############
# 3-6-2014 Raymond Nelson
# 8-1-2014
# 6-15-2015 now works recursively
#
# this script contains the following 3 functions
#
# chartHeader()
# to make a .csv file of the header data for all charts
#
# stimEvents()
# to make a .csv file of the stimulus text for all charts
#
# eventTable()
# to make a .csv file of the table of onset, offset and answer
# for all stimulus events for each chart
#
##############################################


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


