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
                                         removeText=FALSE,
                                         fixFileName=FALSE,
                                         fixSourceName=FALSE,
                                         fixlineNumbers=FALSE,
                                         fixInstrument=FALSE,
                                         fixSoftwareVersion=FALSE,
                                         fixDate=FALSE,
                                         fixTime=FALSE, 
                                         trimData=FALSE,
                                         newSourceName=NULL,
                                         instrumentName=NULL,
                                         softwareVersion=NULL,
                                         newDate=NULL, 
                                         newTime=NULL ) {
  # R function to fix headers in the NCCA ASCII output
  # 20200208
  # Raymond Nelson
  ####
  # x is a vector of NCCA ASCII chart names
  # does not rename the chart
  # only fixes the header info
  
  chartNames <- x
  
  if(!is.null(newSourceName)) {
    if(length(newSourceName) != length(chartNames)) stop("non-NULL source file names must be equal in length to input chart names")
  }
  
  # outNames <- chartNames
  
  i=1
  for (i in 1:length(chartNames)) {
    
    print(paste(chartNames[i], "chart", i, "of", length(chartNames)))
    
    # read the lines from each chart file
    # include all lines because they will be re-written with the new header info
    dataLines <- readLines(chartNames[i], n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    #### slice the header info for the chart ####
    
    {
      # startline
      headerStartLine <- 1
      
      # locate the begining of the time-series data 
      # to indicate the end of the header section
      headerEndLine <- pmatch("Sample     Time", str_trim(strtrim(dataLines, 15), side = "both")) - 0
      
      # slice the header text for editing
      headerText <- dataLines[c(headerStartLine:(headerEndLine))]
    }
      
    #### locate the header line numbers ####
    
    {
      
      # locate the lines with the file name, exam name, and date and trim the whitespace
      # 1. file name
      nameLine <- pmatch("Name of this file: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 2. locate the source file name - is the exam name
      sourceLine <- pmatch("Source file: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 3. instrument
      instrumentLine <- pmatch("Instrument: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 4. software version
      softwareVersionLine <- pmatch("Software Version: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 5. locate the dateLine
      dateLine <- pmatch("Chart Date: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 6. locate the Time: 
      timeLine <- pmatch("Time: ", str_trim(strtrim(headerText, 30), side = "both"))
      
      # 7. Series Number # referred to as "exam number"
      seriesNumberLine <- pmatch("Examination Number: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 8. Chart Number
      chartNumberLine <- pmatch("Chart Number: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 9.number of questions line
      numberQuestionsLine <- pmatch("Number of questions: ", str_trim(strtrim(headerText, 30), side = "both"))
      
      # 10. sample rate line 
      sampleRateLine <- pmatch("Fastest Sample Rate (Hz): ", str_trim(strtrim(headerText, 30), side = "both"))
      # 11. number of samples line
      numberOfSamplesLine <- pmatch("Number of samples: ", str_trim(strtrim(headerText, 30), side = "both"))
      # 12. number of channels line
      numberOfChannelsLine <- pmatch("Number of channels: ", str_trim(strtrim(headerText, 30), side = "both"))
      
    }
    
    #### then locate the header info ####
    
    {
      
      # 1. name of this file
      oldFileName <- str_sub(headerText[nameLine], 20, -1)
      # 2. source file name
      oldSourceFileName <- str_sub(headerText[sourceLine], 14, -1)
      
      # 3. instrument
      oldInstrumentName <- str_sub(headerText[instrumentLine], 13, -1)
      # 4. software version
      oldSoftwarVersion <- str_sub(headerText[softwareVersionLine], 19, -1)
      
      # 5. date
      oldDateText <- str_sub(headerText[dateLine], 13, -1)
      # 6. time
      oldTimeText <- str_sub(headerText[timeLine], 7, -1)

      # 7. get the series number # referred to as "exam number" in the header
      oldSeriesNumber <- str_sub(headerText[seriesNumberLine], 21, -1)
      # 8. locate the chart number 
      oldChartNumber <- str_sub(headerText[chartNumberLine], 15, -1)
      # 9. get the number of questions from line 9
      oldNumberQuestions <- str_sub(headerText[numberQuestionsLine], 22, -1)
      
      # 10. sample rate
      oldFastestSampleRate <- str_sub(headerText[sampleRateLine], 27, -1)
      # 11. number of samples
      oldNumberOfSamples <- str_sub(headerText[numberOfSamplesLine], 20, -1)
      # 12. number of channels
      oldNumberOfChannels <- str_sub(headerText[numberOfChannelsLine], 21, -1)
      
    }
    
    #### locate question table and events table ####
    
    {
      # get the start line and end line for event statements
      eventStatementStartLine <- 
        pmatch("Event    Label Statement", str_trim(strtrim(headerText, 35), side = "both")) + 1
      eventStatementEndLine <-
        pmatch("Event    Label      Begin", str_trim(strtrim(headerText, 35), side = "both")) - 2
      
      # get the start line and end line for event row indices
      eventIndicesStartLine <- 
        pmatch("Event    Label      Begin", str_trim(strtrim(headerText, 35), side = "both")) + 1
      eventIndicesEndLine <- 
        pmatch("Sample     Time    Label", str_trim(strtrim(headerText, 35), side = "both")) - 2      
    }
    
    #### edit header data ####
    
    {
      
      if(fixFileName) {
        # locate and correct the file name
        
        # 1 fileName <- str_sub(headerText[nameLine], 20, -1)
        headerText[nameLine] <- paste0("Name of this file: ",
                                       chartNames[i] )
        
        # 7.set the new series number
        newSeriesNumber <- str_sub(chartNames[i], -5, -5)
        headerText[seriesNumberLine] <- paste0("Examination Number: ", newSeriesNumber)
        
        # 8. set the new chart number from the NCCA ASCII text file name
        newChartNumber <- str_sub(chartNames[i], -3, -2)
        headerText[chartNumberLine] <- paste0("Chart Number: ", newChartNumber)
        
      } 
      
      if(fixSourceName) {
        # 2. set the source file name
        # sourceName <- str_sub(headerText[sourceLine], 14, -1)
        thisSourceName <- ifelse(is.null(newSourceName), 
                                 str_sub(chartNames[i], 4, -7), 
                                 newSourceName[i])
          
        headerText[sourceLine] <- paste0("Source file: ", thisSourceName )
        
        # axciton source files end in .012 where the second decimal digit is  the chart number
        # headerText[sourceLine] <- paste0("Source file: ",
        #                                  str_sub(chartNames[i], 4, -7),
        #                                  paste0(".", newChartNumber, "2") )
      }
      
      if(fixInstrument) {
        # 3. instrument name
        headerText[instrumentLine] <- paste0("Instrument: ", "Lafayette Windows")
      }
      
      if(fixSoftwareVersion) {
        # 4. software version
        headerText[softwareVersionLine] <- paste0("Software Version: ", "NA")
      }
      
      if(fixDate) { 
        # 5. change the date
        # headerText[5] <- paste0("Chart Date: ", "31Dec69")
        headerText[dateLine] <- paste0("Chart Date: ", newDate)
      }
      
      if(fixTime && is.null(newTime)) {  
        # 6 change the time
        # headerText[6] <- "Time: 00:00"
        seriesNumbers <- c("01", "02", "03", "04", "05")
        chartTimes <- c(":10", ":20", ":30", ":40", ":50")
        # newSeriesNumber and newChartNumber are lines 7 and 8 and are obtained from the chartName
        thisChartTime <- paste0(seriesNumbers[newSeriesNumber], chartTimes[newChartNumber])  
        headerText[timeLine] <- paste0("Time: ", thisChartTime)
      } else if(fixTime && is.null(newTime)) {
        headerText[timeLine] <- paste0("Time: ", newTime)
      } else {
        headerText[timeLine] <- paste0("Time: ", oldTimeText)
      }
      
      #### fix the number of questions and number of data samples ####
      
      {
        # 9. fix the number of questions
        # headerText[numberQuestionsLine] <- paste0("Number of questions: ", oldNumberQuestions)
        newNumberQuestions <- eventIndicesEndLine - eventIndicesStartLine + 1
        headerText[numberQuestionsLine] <- paste0("Number of questions: ", newNumberQuestions)
        
        # 10. set the sampling rate # re-use the old fastest sampling rate
        headerText[sampleRateLine] <- paste0("Fastest Sample Rate (Hz): ", oldFastestSampleRate)
          
        # 11. then get last sample number
        newNumberSamples <- str_trim(str_sub(dataLines[length(dataLines)], 1, 6), "both")
        headerText[numberOfSamplesLine] <- paste0("Number of samples: ", newNumberSamples)
        
        # 12. re-use the old number of channels
        headerText[numberOfChannelsLine] <- paste0("Number of channels: ", oldNumberOfChannels)
      }
      
    }
    
    #### fix and re-number the stimulus event lines 
    
    if(fixlineNumbers) {
      
      ## renumber the events before working with the stimulus text lines ##
      
      n <- eventIndicesStartLine:eventIndicesEndLine
      
      newNumbers <- str_pad(c(1:length(n)), 2, side="left", pad="0")
      
      for(l in 1:length(n)) {
        headerText[n[l]] <- 
          paste0(newNumbers[l], str_sub(headerText[n[l]], 3, -1))
      }
      
      # re-fix line 9 for the number of questions
      newNumberQuestions <- eventIndicesEndLine - eventIndicesStartLine + 1
      headerText[numberQuestionsLine] <- paste0("Number of questions: ", newNumberQuestions)
      
    }
    
    #### fix/remove the question text #####
    
    if(removeText) {
      # remove the questions
      questionTextLines <- eventStatementStartLine:eventStatementEndLine
      # "01           X " # is 15 characters
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
      
      # adjust both the headerText and dataLines if any wrapped lines are removed
      if(length(wrappedLines) > 0) {
        wrappedLines <- wrappedLines + (eventStatementStartLine - 1)
        # headerText is line 1 to the header row for the time series data
        headerText <- headerText[-wrappedLines]
        # dataLines includes all lines in the NCCA ASCII file
        dataLines <- dataLines[-wrappedLines]
        # adjust the headerEndLine for the headerText end line
        headerEndLine <- headerEndLine - length(wrappedLines)
        eventStatementEndLine <- eventStatementEndLine - length(wrappedLines)
        eventIndicesStartLine <- eventIndicesStartLine - length(wrappedLines)
        eventIndicesEndLine <- eventIndicesEndLine - length(wrappedLines)
        
        # fix the number of questions again
        numberQuestions <- eventIndicesEndLine - eventIndicesStartLine + 1
        headerText[numberQuestionsLine] <- paste0("Number of questions: ", numberQuestions)
      }
    }
    
    #### call toupper() for the question labels and time series events ####
    
    if(removeText) {
      # call toupper for the question labels in the events section
      eventLines <- eventIndicesStartLine:eventIndicesEndLine
      headerText[eventLines] <- toupper(headerText[eventLines])
      
      # call toupper() for the time series data 
      # so that all question tags are upper case
      dataLines[(headerEndLine+1):length(dataLines)] <- 
        toupper(dataLines[(headerEndLine+1):length(dataLines)])
    }
    
    #### trim the data to include only the necessary channels 
    
    if(trimData) {
      
      ## trim the time series data lines to include only the required sensors
      
      # str_sub(dataLines[headerEndLine], 29,90)
      # str_sub(dataLines[headerEndLine], 29,79)
      
      newNumberChannels <- 
        length(as.vector(str_split(str_sub(dataLines[headerEndLine], 29,90), "\\s+", simplify = TRUE)))
      
      headerText[headerEndLine] <- str_sub(headerText[headerEndLine], 1, 90)
      
      dataLines[c((headerEndLine+1):length(dataLines))] <- str_sub(dataLines[c((headerEndLine+1):length(dataLines))], 1, 93)
      # dataLines[c((headerEndLine+1):length(dataLines))] <- str_sub(dataLines[c((headerEndLine+1):length(dataLines))], 1, 82)
      
      # 12. fix number of channels
      headerText[numberOfChannelsLine] <- paste0("Number of channels: ", newNumberChannels)
      
    }
    
    #### write the new header info to the data lines ####
    
    dataLines[1:headerEndLine] <- headerText
    
    #### write the data lines to the file #####
    
    cat(dataLines, file=chartNames[i], sep="\r\n")
    # cat(dataLines, file="test_NCCA_ASCII_fix-1.01A", sep="\r\n")
    
  } # end for loop over NCCA ASCII file names
  
  return(paste(length(chartNames), "charts processed"))
  
} # end changeNCCAASCIIFileHeadersFn function


# call the function
# changeNCCAASCIIFileHeadersFn(x=chartNames,
#                              removeText=TRUE,
#                              fixFileName=TRUE,
#                              fixSourceName=TRUE,
#                              fixlineNumbers=TRUE,
#                              fixInstrument=TRUE,
#                              fixSoftwareVersion=TRUE,
#                              fixDate=TRUE,
#                              fixTime=TRUE,
#                              trimData=TRUE,
#                              newSourceName=NULL,
#                              instrumentName=NULL,
#                              softwareVersion=NULL,
#                              newDate="01Jan70",
#                              newTime=NULL )



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
