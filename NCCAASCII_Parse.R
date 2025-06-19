# R Functions for parsing NCCA ASCII text output 
# 3-2-2014 Raymond Nelson
# 8-1-2014
# 6-9-2015
# 6-12-2015 working
#
# this script contains the following functions
#
# main function
# parseUniqueExams() # for Headers and Data
# 
#
# older functions 
# 
# parseUniqueExamHeaders()
# parseUniqueExamData()
#
#
# first put all NCCA ASCII output files in the same working directory
#
########################################################



# rm(list=ls())
# library("stringr")


# source the initialization file
# print("source the NCCAASCII_init.R script")
# source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)


# # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
# source(paste0(RPath, 'getExamNames.R'), echo=FALSE)

# # call the two functions together to make a list of unique exams in the current working directory
# uniqueExamNames <- uniqueNames(getCharts(searchPattern))
# or call one function with the uniqueTest parameter
# uniqueExamNames <- getCharts("D&+", uniqueTests=TRUE)

# # select an exam from the vector of exam names
# # uniqueExamNames <- uniqueExamNames[16]
# if(!exists("selectExams")) {selectExams <- "ALL"} 
# if(selectExams != "ALL") uniqueExamNames <- uniqueExamNames[selectExams] 
# print(paste("Found", length(uniqueExamNames), "exams"))
# print(uniqueExamNames)



#############  source the additional helper functions   ##############



# print("source the NCCAASCIIParseHelperFunctions.R script")
# source(paste0(RPath, 'NCCAASCIIParseHelperFunctions.R'), echo=FALSE)



### requires 6 helper functions from the NCCAASCIIParseHelperFunctions.R script

# headerFile() to make a vector of chart header names

# dataFile() to create a data vector of data vector names

# chartHeader() to make a data frame and csv from the chart header info

# stimEvents() to make a data frame for the stimulus text for each chart

# eventTable() to make a data frame for the stimulus events for each chart

# dataParse() to create a data frame for each exam with all series and all charts



############  main function  ##############



parseUniqueExams <- function(uniqueExamNames=uniqueExamNames,
                             saveCSV=FALSE,
                             makeDF=TRUE, 
                             keepText=FALSE ) {
  # function to parse all chart headers for a single exam exam 
  # including the header, stimulus events
  # use the helper functions in the NCCAASCIIParseHelperfunctions.R script
  #
  # also parses the time series data charts to a single data frame
  # including all series
  #
  # x input is a scalar containing the name of a single exam
  # saveCSV  and makeDF will be passed to the other functions 
  #
  # keepText is passed to the stimEvents function
  #
  # output is a side effect creation of three data frames
  # for header, stimulus events, and data
  #
  ####
  
  {
    if(!exists("saveCSV")) saveCSV=FALSE
    if(!exists("makeDF")) makeDF=TRUE
    if(!exists("keepText")) keepText=FALSE
  }
  
  if(length(uniqueExamNames) == 0) return("no exams to parse")
  
  #### iterate over the exams ####
  
  i=1
  
  for(i in 1:length(uniqueExamNames)) {
      
    {
      # get the names of the files for the exam charts
      
      thisExamName <- uniqueExamNames[i]
      
      # save the exam name for observation and inspection
      assign("examName", thisExamName, pos=1)
      
      assign("i", i, envir=.GlobalEnv)
      
      print(paste("exam", i, "of", length(uniqueExamNames), "examName:", thisExamName))
      
      # make a regular expression of the searchString
      searchString <- thisExamName
      
      # fix problem characters in the AXCITON file names
      # special characters must be escaped twice
      # once for R and once for the system
      # multiple special characters can be used after a double escape
      searchString <- gsub("\\$", "\\\\$", searchString)
      
      # make a vector of the names of exam charts in the current working directory
      examCharts <- list.files(path = ".", 
                               pattern = searchString, 
                               all.files = FALSE,
                               full.names = FALSE, 
                               recursive = FALSE,
                               ignore.case = FALSE, 
                               include.dirs = FALSE,
                               no.. = FALSE)
      
      # exclude .txt .pdf .csv .Rda
      excludePatterns <- c("txt", "pdf", "csv", "doc", "Rda")
      # !grepl("txt|pdf|csv|Rda", examCharts)
      
      examCharts <- examCharts[!grepl("txt|pdf|csv|Rda", examCharts)]
      
      # exam charts always start with "D"
      examCharts <- examCharts[which(str_sub(examCharts, 1, 1) == "D")]
      
      # exclude partial name matches
      examCharts <- examCharts[which( substr(examCharts, 1, nchar(examCharts)-6) == 
                                        unique(substr(examCharts, 1, nchar(examCharts)-6))[1] )]
    }
    
    ### header files
    
    {
      # make a vector for each chart header and output vector of chart header names
      print("  get the exam header vectors in the global environment")
      
      # call the headerFile function 
      # from the NCCAASCIIParseHelperfunction.R. script 
      headerNames <- headerFile(x=examCharts, 
                                makeVector=TRUE, 
                                saveTXT=FALSE, 
                                makeDF=TRUE, 
                                saveCSV=FALSE ) 
      # visible output is a vector of names of header data frames
      # side effect for makeVector=TRUE is a vector with the header info in the global env
      # _header vectors are removed later by the cleanUp function
    }
    
    ### stimulus events 
    
    {
      # make a data frame for the stimulus text statements for each chart
      
      print("  parse the stimulus text statements for each chart")
      
      if(!exists("keepText")) keepText <- TRUE
      
      print("  make a data frame for all events for all series and all charts")
      
      # call the stimEvents function
      # from the NCCAASCIIParseHelperFunctions.R script
      stimEvents(x=headerNames, 
                 saveCSV=FALSE, 
                 makeDF=TRUE,
                 keepText=keepText ) 
      # visible output from the stimEvents() function is the exam name
      # side effect of the function is a collection of data frames
    }
    
    ### event locations
    
    {
      # add the Begin, End and Answer location to the _Stimuli data frame
      
      assign("headerNames", headerNames, envir=.GlobalEnv)
      # stop()
      
      print("  parse stimulus events for all charts")
      print("  add the event onset, offset and answer to the stimuli data frame")
      
      # call the eventTable function
      # from the NCCAASCIIParseHelperFunctions.R script
      eventTable(x=headerNames, 
                 saveCSV=FALSE, 
                 makeDF=FALSE)
      # 'Begin' 'End' and 'Answer' are added to the _Stimuli data frame 
      # makeDF=TRUE will create a data frame of events 
      # with Begin End and Answer locations
      # and so makeDF can be FALSE with no loss of info
      # visible output is the name of the exam
    }
    
    ### data file names
    
    {
      # create a data vector for each chart and a vector of data vector names
      
      print("  get the data vectors")
      
      # call the dataFile function
      # from the NCCAASCIIParseHelperFunctions.R script
      dataNames <- dataFile(x=examCharts, 
                            makeVector=TRUE, 
                            saveTXT=FALSE)
      # _data vectors are removed later by the cleanUp() function
      # output is a character vector of the names of the created _data files 
    }
    
    ### data files 
    
    {
      # initialize a data frame for each exam
      print("  parse the chart data vectors to a single data frame")
      
      # call the dataParse function from the NCCAASCII_ParseHelperFunctions.R script
      outName <- dataParse(x=dataNames, 
                           y=thisExamName, 
                           saveCSV=FALSE, 
                           makeDF=TRUE)
      # visible output from dataParse() is a character vector of data file names
      # side effect from dataParse is to create a single data frame for all series and charts
      assign("outName", outName, envir=.GlobalEnv)
      print(outName)
    }
    
    ### check for overlapping events ###
    
    {
      
      # # Sep 27, 2023
      # 
      # print("check for overlapping events")
      # 
      # # construct the names of the _Data and _Stimuli data frames
      # stimDFName <- paste0("D", str_replace_all(thisExamName, "[:punct:]", ""), "_Stimuli")
      # dataDFname <- paste0("D", str_replace_all(thisExamName, "[:punct:]", ""), "_Data")
      # 
      # stimDFName <- str_replace_all(stimDFName, "\\$", "")
      # dataDFname <- str_replace_all(dataDFname, "\\$", "")
      # 
      # thisStimuliDF <- get(stimDFName, pos=1)
      # thisDataDF <- get(dataDFname, pos=1)
      # 
      # # get the starting index for the 
      # 
      # # iterate backward over the stimuli data frame
      # j=1
      # for(j in 1:(nrow(thisStimuliDF)-1)) {
      #   thisAnswerRow <- as.numeric(thisStimuliDF$Answer[j])
      #   if(thisAnswerRow == "" || is.na(thisAnswerRow)) next()
      #   # get the chart name, chart onset row and addIdx to locate the row in the time series
      #   thisSeriesName <- thisStimuliDF$seriesName[j]
      #   thisChartName <- thisStimuliDF$chartName[j]
      #   # onset row for the chart in the _Data 
      #   thisChartOnsetRow <- 
      #     which(thisDataDF$seriesName == thisSeriesName & 
      #             thisDataDF$chartName == thisChartName)[1]
      #   #
      #   if(thisAnswerRow > as.numeric(thisStimuliDF$End[(j+1)])) {
      #     thisStimuliDF$Answer[j] <- ""
      #     
      #     thisDataDF$Label[(thisAnswerRow + (thisChartOnsetRow-1))] <- ""
      #     thisDataDF$eventLabel[(thisAnswerRow + (thisChartOnsetRow-1))] <- ""
      #   }
      # }
      # 
      # assign(stimDFName, thisStimuliDF, envir=.GlobalEnv)
      # assign(dataDFname, thisDataDF, envir=.GlobalEnv)

    }
    
    ### fix the RQ and CQ labels ###
    
    {
      
      # # May 15, 2021
      # 
      # # use the outName vector from the dataParse() function 
      # 
      # # need to correct the labels in both the _Data and _Measurments data frames
      # 
      # # outName <- paste0("D", uniqueExamNames[i], "_Data")
      # 
      # thisCSV <- get(outName)
      # 
      # 
      # # fix question labels
      # theseEventNames <- thisCSV$eventLabel
      # 
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
      # 
      # thisCSV$eventLabel <- theseEventNames
      # 
      # theseEventNames <- thisCSV$Label
      # 
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
      # 
      # thisCSV$Label <- theseEventNames
      # 
      # # replace the data frame
      # 
      # assign(outName, thisCSV, envir=.GlobalEnv)
      # 
      # #### now the stimuli data frame
      # 
      # thisCSV <- get(paste0(str_sub(outName, 1, -5), "Stimuli"))
      # 
      # # fix question labels
      # theseEventNames <- thisCSV$eventLabel
      # 
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
      # 
      # thisCSV$eventLabel <- theseEventNames
      # 
      # theseEventNames <- thisCSV$Label
      # 
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
      # 
      # thisCSV$Label <- theseEventNames
      # 
      # # replace the data frame
      # 
      # assign(paste0(str_sub(outName, 1, -5), "Stimuli"), thisCSV, envir=.GlobalEnv)
      
    }
    
  } # end i loop over unique exam names
  
  ###
  
  return(uniqueExamNames)
  
} # end parseUniqueExams function




