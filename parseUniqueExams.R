parseUniqueExams <- function(x=uniqueExamNames[1],
                                   saveTXT=FALSE,
                                   saveCSV=FALSE,
                                   makeDF=TRUE,
                                   makeVector=TRUE) {
  # function to parse all chart headers for a single exam exam 
  # including the header, stimulus events
  # use the helper functions in the NCCAASCIIParseHelperfunctions.R script
  
  # also parses the time series data charts to a single data frame
  # including all series
  
  # x input is a scalar containing the name of a single exam
  # writeCSV will be passed to the other functions 
  # to make .csv files with the header, stimuli and data
  
  # output is the side effect creation of three data frames
  # for header, stimulus events, and data
  
  # requires a loop for multiple exam names
  ###
  
  # keep only the first input item so that this function is not vectorized
  # it will require a loop to call this function with multiple input exam names
  uniqueExamNames <- x[1]
  
  # use a slice just in case a vector of names is input as x
  # examName <- uniqueExamNames[1]
  assign("examName", uniqueExamNames, pos=1)
  
  print(uniqueExamNames)
  
  # make a regular expression of the searchString
  # searchString <- paste0(examName, "+")
  searchString <- uniqueExamNames
  
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
  
  # exclude partial name matches
  examCharts <- examCharts[which( substr(examCharts, 1, nchar(examCharts)-6) == 
                                    unique(substr(examCharts, 1, nchar(examCharts)-6))[1] )]
  
  ### header files
  
  # make a vector for each chart header and a vector of chart header names
  print("  getting header vectors in the global environment")
  headerNames <- headerFile(x=examCharts, makeVector=makeVector, saveTXT=saveTXT) 
  # output is a vector of names of header data frames
  # side effect makeVector=TRUE will put a vector with the header infor in the global env
  # _header vectors are removed later by the cleanUp function
  
  # make a csv and data frame from the chart header info
  print("  getting and parsing header information")
  chartHeader(x=headerNames, makeDF=makeDF, saveCSV=saveCSV)
  # visible output is a character vector with the exam name
  # side effect is adata frame with the exam header information
  
  ### stimulus events 
  
  # make a data frame for the stimulus text statements for each chart
  print("  parsing stimulus text statements for each chart")
  stimEvents(x=headerNames, saveCSV=saveCSV, makeDF=makeDF) 
  # output from the stimEvents() function is the exam name
  # side effect of the function is a collection of data frames
  
  # make a data frame for the stimulus events for each chart
  print("  parsing stimulus events for all charts")
  eventTable(x=headerNames, saveCSV=FALSE, makeDF=FALSE)
  # output is the name of the exam
  # when makeDF and saveCSV are both FALSE the 'Begin' 'End' and 'Answer' are added 
  # to the _Stimuli data frame created by the stimEvents() function
  
  ### data files 
  
  # create a data vector for each chart and a vector of data vector names
  # print("  getting data vectors")
  dataNames <- dataFile(x=examCharts, makeVector=makeVector, saveTXT=saveTXT)
  # _data vectors are removed later by the cleanUp() function
  # output is a character vector of the names of the created _data files 
  
  # create a data frame for each exam
  print("  getting data vectors and parsing the chart data to a single data frame")
  outName <- dataParse(x=dataNames, y=uniqueExamNames, saveCSV=saveCSV, makeDF=makeDF)
  # visible output from dataParse() is a character vector of data file names
  # side effect from dataParse is to create a single data frame for all series and charts
  print(outName)
  
  ###
  
  return(uniqueExamNames)
  
} # end parseUniqueExams function

