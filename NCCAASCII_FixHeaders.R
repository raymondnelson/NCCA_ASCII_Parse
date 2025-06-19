# R Script to fix the NCCA ASCII header info
# 2020-2-8
# Raymond Nelson
#
####


# contains 2 functions

# changeNCCAASCIIFileHeadersFn() to fix the header details

# fixProblemCharsFn() to fix problem character strings



####

# chartNames <- list.files(pattern="^D\\&-")


changeNCCAASCIIFileHeadersFn <- function(x=chartNames, 
                                         fixFileName=FALSE,
                                         fixSourceName=FALSE,
                                         removeText=FALSE,
                                         fixlineNumbers=FALSE,
                                         fixInstrument=FALSE,
                                         fixSoftwareVersion=FALSE,
                                         fixDate=FALSE,
                                         fixTime=FALSE ) {
  # R function to fix headers in the NCCA ASCII output
  # 20200208
  # Raymond Nelson
  #
  ####
  # x is a vector of NCCA ASCII chart names
  # does not rename the chart
  # only fixes the header info
  
  chartNames <- x
  
  # outNames <- chartNames
  
  i=1
  for (i in 1:length(chartNames)) {
    
    print(paste(chartNames[i], "chart", i, "of", length(chartNames)))
    
    # read the lines from each chart file
    # include all lines because they will be re-written with the new header info
    dataLines <- readLines(chartNames[i], n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    #### slice the header info for the chart ####
    
    {
      # first locate the header info
      
      # locate the begining of the time-series data 
      # to indicate the end of the header section
      endLine <- pmatch("Sample     Time", str_trim(strtrim(dataLines, 15), side = "both")) - 0
      
      # slice the header text
      headerText <- dataLines[1:endLine]
      
      # locate the lines with the file name, exam name, and date and trim the whitespace
      # file name
      nameLine <- pmatch("Name of this file: ", str_trim(strtrim(headerText, 20), side = "both"))
      # locate the source file name - is the exam name
      sourceLine <- pmatch("Source file: ", str_trim(strtrim(headerText, 20), side = "both"))
      # locate the dateLine
      dateLine <- pmatch("Chart Date: ", str_trim(strtrim(headerText, 20), side = "both"))
      
      # get the start line and end line for event statements
      eventStatementStartLine <- 
        pmatch("Event    Label Statement", str_trim(strtrim(headerText, 35), side = "both")) + 1
      eventStatementEndLine <-
        pmatch("Event    Label      Begin", str_trim(strtrim(headerText, 35), side = "both")) - 3
      
      # get the start line and end line for event row indices
      eventIndicesStartLine <- 
        pmatch("Event    Label      Begin", str_trim(strtrim(headerText, 35), side = "both")) + 1
      eventIndicesEndLine <- 
        pmatch("Sample     Time    Label", str_trim(strtrim(headerText, 35), side = "both")) - 2      
      
      # get the series number from lines 7
      seriesNumber <- str_sub(headerText[7], 21, -1)
      # also locate the chart number from lines 8 
      chartNumber <- str_sub(headerText[8], 15, 07)
      # get the number of questions from line 9
      numberQuestions <- str_sub(headerText[9], 22, -1)
    }
    
    #### edit header data ####
    
    if(fixFileName) {
      # locate and correct the file name
      # fileName <- str_sub(headerText[nameLine], 20, -1)
      headerText[nameLine] <- paste0("Name of this file: ",
                                     chartNames[i] )
      
      # get the new chart number from the NCCA ASCII text file name
      newChartNumber <- str_sub(chartNames[i], -3, -2)
      
      # get the new series number
      newSeriesNumber <- str_sub(chartNames[i], -5, -5)
      
      # reset the chart number
      headerText[8] <- paste0("Chart Number: ", newChartNumber)
    }
    
    #### edit the source file info ####
    
    if(fixSourceName) {
      # original source name
      sourceName <- str_sub(headerText[sourceLine], 14, -1)
      # locate the correct the source file name
      
      # set the source file info
      headerText[sourceLine] <- paste0("Source file: ",
                                       str_sub(chartNames[i], 4, -7) )
      
      # axciton source files end in .012 where the second decimal digit is  the chart number
      # headerText[sourceLine] <- paste0("Source file: ",
      #                                  str_sub(chartNames[i], 4, -7),
      #                                  paste0(".", newChartNumber, "2") )
    }
    
    #### fix the instrument and software info ####
    
    if(fixInstrument) {
      # get the original instrument info
      orgInstrument <- str_sub(headerText[3], 13, -1)
      
      # set the instrument
      headerText[3] <- paste0("Instrument: ", "Lafayette Windows")
    }
    
    if(fixSoftwareVersion) {
      # get the original software version 
      orgSoftwareVersion <- str_sub(headerText[4], 19, -1)
      
      # reset the software version
      headerText[4] <- paste0("Software Version: ", "NA")
    }
    
    #### edit the date and time of the chart ####
    
    if(fixDate) { 
      # change the date
      # headerText[5] <- paste0("Chart Date: ", "31Dec69")
      headerText[5] <- paste0("Chart Date: ", "01JAN70")
    }
    
    if(fixTime) {  
      # change the time
      # headerText[6] <- "Time: 00:00"
      
      seriesNumbers <- c("01", "02", "03", "04", "05")
      chartTimes <- c(":10", ":20", ":30", ":40", ":50")
      
      thisChartTime <- paste0(seriesNumbers[newSeriesNumber], chartTimes[newChartNumber])  
      
      headerText[6] <- paste0("Time: ", thisChartTime)
    }
      
    #### fix the number of questions and number of data samples ####
    
    {
      # the get last sample number
      newNumberSamples <- str_trim(str_sub(dataLines[length(dataLines)], 1, 6), "both")
      headerText[11] <- paste0("Number of samples: ", newNumberSamples)
      
      # fix the number of questions
      numberQuestions <- eventIndicesEndLine - eventIndicesStartLine + 1
      headerText[9] <- paste0("Number of questions: ", numberQuestions)
    }
    
    #### fix/remove the question tesxt #####
    
    if(removeText) {
      # remove the questions
      questionTextLines <- eventStatementStartLine:eventStatementEndLine
      headerText[questionTextLines] <- 
        str_sub(headerText[questionTextLines],  1, 15)
      
      # then use the question ID as the question text
      j=eventStatementStartLine
      for (j in eventStatementStartLine:eventStatementEndLine) {
        # also call toupper to set upper case IDs and text
        headerText[j] <- 
          toupper(paste0(str_sub(headerText[j], 1, 15),
                         str_trim(str_sub(headerText[j], 7, 14), "both") ) )
      }
    }
    
    #### remove wrapped lines from the question text statements ####
    
    if(removeText) {
      wrappedLines <- 
        which(str_sub(headerText[eventStatementStartLine:eventStatementEndLine], 1, 2) == "  ")
      
      # adjust both the header and data if any wrapped lines
      if(length(wrappedLines) > 0) {
        wrappedLines <- wrappedLines + (eventStatementStartLine - 1)
        headerText <- headerText[-wrappedLines]
        dataLines <- dataLines[-wrappedLines]
        # adjust the endLine headerText end line
        endLine <- endLine - length(wrappedLines)
        eventStatementEndLine <- eventStatementEndLine - length(wrappedLines)
        eventIndicesStartLine <- eventIndicesStartLine - length(wrappedLines)
        eventIndicesEndLine <- eventIndicesEndLine - length(wrappedLines)
        # fix the number of questions again
        numberQuestions <- eventIndicesEndLine - eventIndicesStartLine + 1
        headerText[9] <- paste0("Number of questions: ", numberQuestions)
      }
    }
    
    #### fix and re-number the question text lines ####
    
    if(fixlineNumbers) {
      
      n <- eventStatementStartLine:eventStatementEndLine
      
      newNumbers <- str_pad(c(1:length(n)), 2, side="left", pad="0")
      
      for(k in 1:length(n)) {
        headerText[n[k]] <- 
          paste0(newNumbers[k], str_sub(headerText[n[k]], 3, -1))
      }
      
    }
    
    #### fix and re-number the stimulus event lines 
    
    if(fixlineNumbers) {
      
      n <- eventIndicesStartLine:eventIndicesEndLine
      
      newNumbers <- str_pad(c(1:length(n)), 2, side="left", pad="0")
      
      for(l in 1:length(n)) {
        headerText[n[l]] <- 
          paste0(newNumbers[l], str_sub(headerText[n[l]], 3, -1))
      }
      
    }
    
    #### call toupper() for the question labels and time series events ####
    
    if(removeText) {
      # call toupper for the question labels in the events section
      eventLines <- eventIndicesStartLine:eventIndicesEndLine
      headerText[eventLines] <- toupper(headerText[eventLines])
      
      # call toupper() for the time series data 
      # so that all question tags are upper case
      dataLines[(endLine+1):length(dataLines)] <- 
        toupper(dataLines[(endLine+1):length(dataLines)])
    }
    
    #### write the new header info to the data lines ####
    
    dataLines[1:endLine] <- headerText
    
    #### write the data lines to the file #####
    
    cat(dataLines, file=chartNames[i], sep="\r\n")
    # cat(dataLines, file="test_NCCA_ASCII_fix-1.01A", sep="\r\n")
    
  } # end for loop over NCCA ASCII file names
  
  return(paste(length(chartNames), "charts processed"))
  
} # end changeNCCAASCIIFileHeadersFn function


# call the function
# changeNCCAASCIIFileHeadersFn(x=chartNames)



####



fixProblemCharsFn <- function(chartNames=chartNames) {
  # R function to look for problematic annotations and question tags
  
  
  i=1
  for (i in 210:length(chartNames)) {
    
    # length(chartNames)
    
    print(paste(chartNames[i], "chart", i, "of", length(chartNames)))
    
    dataLines <- readLines(chartNames[i], n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    dataLines <- toupper(dataLines)
    
    # head(dataLines, 50)
    
    lineNumbers <- NULL
    
    checkLines <- c(grep("1C 1C", dataLines),
                    grep("CT CT", dataLines),
                    grep("C C", dataLines),
                    # grep("-------T", dataLines),
                    grep("R R", dataLines),
                    grep("WRQ WRQ", dataLines),
                    grep("WR WR", dataLines),
                    grep("-------C ", dataLines),
                    grep("------CT ", dataLines), 
                    grep("-------R ", dataLines),
                    grep("------WR ", dataLines),
                    grep("-----WRQ ", dataLines),
                    grep("-------K ", dataLines),
                    grep("-------Y ", dataLines)
    )
    
    if(length(na.omit(checkLines)) > 0) {
      
      stop(paste("check lines", toString(na.omit(checkLines))))
    }
    
  } # end for loop
  
  print(paste("checked", length(chartNames), "charts for problem event characters"))
  
} # endfixProblemCharsFn()


# fixProblemCharsFn(chartNames=chartNames)


# head(dataLines, 50)
