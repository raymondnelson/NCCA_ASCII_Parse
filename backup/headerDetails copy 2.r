# r script to extract the exam header information from the NCCA ASCII output
# use the parseFilesCSV.r script before this one
# this script will make a vector of all *.header.txt files and then separate out the exam header info
# after that the test stimulus will be separated, and 
#
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#
# make a vector of the names of all *.header.txt files
headerList <- list.files(path = ".", pattern = "*.header.txt", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE)
#
for (i in 1:length(headerList)) { # a loop to open all the header file in the current working directory
  fileName <- headerList[i]
  fileName <- headerList[1] # for testing - comment this out to use the loop
  #
  # read the file
  headerFile <- readLines(fileName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  #
  # locate the string "Event    Label Statement" and read the headerFile without the event lines
  headerFileHeaders <- headerFile[1:(pmatch("Event    Label Statement", headerFile) - 2)]
  headerFileHeaders <- strsplit(headerFileHeaders, ": ")
  #
  # a loop to turn the header info into vectors that can be concatenated and saved as .csv
  v1 <- NULL
  v2 <- NULL
  v3 <- NULL
  for (i in 1:length(headerFileHeaders)) {
    v1 <- headerFileHeaders[[i]]
    v2 <- c(v2, v1[1])
    v3 <- c(v3, v1[2])
  }
  #  set the file name to .csv
  fileNameCSV <- paste(strtrim(fileName, nchar(fileName) - 4), ".csv", sep = "")
  # write the .csv file
  write.table(rbind(as.list(v2), as.list(v3)), file = fileNameCSV, append = FALSE, 
              quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE, 
              qmethod = "double", fileEncoding = "UTF-8")
  #  
  # read the stimulus events
  headerFileStimuli <- headerFile[pmatch("Event    Label Statement", headerFile):(pmatch("Event    Label      Begin        End     Answer", headerFile) - 2)]
  #
  # fix wrapped lines
  v1 <- NULL # 
  v2 <- NULL # 
  v3 <- NULL # 
  for (i in 1:length(headerFileStimuli)) {
    # i = 10 # for testing only 
    v1 <- headerFileStimuli[i]
    v2 <- headerFileStimuli[i + 1]
    if (strtrim(v2, 6) == "      ")
      v3 <- c(v3, paste(v1, str_trim(v2, side = "left"), sep = ""))
     
    if (strtrim(v1, 6) == "      ") next
    v3 <- c(v3, v1)
    }
  
  
  # a loop to get the event names
  v1 <- NULL
  v2 <- NULL
  v3 <- NULL
  for (i in 1:length(headFileStimuli)) {
    v1 <- headerFileStimuli[[i]]
    v1 <- headerFileStimuli[[2]] # for testing only 
    v2 <- str_trim(str_sub(v1, 1, 6), side = "both")
  }
  # fix wrapped lines
  
  
  
  
  
  fileName <- strtrim(fileName, nchar(fileName) - 4)
  headerFileName <- paste(fileName, ".csv", sep = "")
  
  
  
  
  
  # read the stimulus event indices
  headerFileEvents <- headerFile[pmatch("Event    Label      Begin        End     Answer", headerFile):(NROW(headerFile) -1)]
  
  
  
  

  

  
  
  # set the name of the header file
  # first trim the last 4 characters (.txt) from the header file name
  # nchar(fileName)
  fileName <- strtrim(fileName, nchar(fileName) - 4)
  headerFileName <- paste(fileName, ".csv", sep = "")
  # cat(headerFile, file = headerFileName, sep = "\n")
  #
  # look for
  
  # count the number of lines
  nrow(fileName)
  
  firstLine <- pmatch("     1", headerFileTrim6) -1 # set the number of lines in the header section
  # remove the header lines
  dataFile <- dataFile[firstLine:lastLine]
  
  
  
  # save the header data in .csv format
  write.csv(myList, file = "myList.csv", quote = TRUE,
            eol = "\n", na = "NA", row.names = FALSE,
            fileEncoding = "")
  
  
  # separate the data file
  dataFile <- readLines(fileName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  # determine thelength of the data file
  lastLine <-length(dataFile)
  # and determine the number of header lines to skip

  firstLine <- pmatch("     1", headerFileTrim6) -1 # set the number of lines in the header section
  # remove the header lines
  dataFile <- dataFile[firstLine:lastLine]
  # set the name of the data file
  dataFileName <- paste(fileName, "_data.txt", sep = "")
  # finally save the data file
  cat(dataFile, file = dataFileName, sep = "\n")
  
}
