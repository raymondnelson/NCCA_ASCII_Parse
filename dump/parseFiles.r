# R script to parse NCCA ASCII output
# working script 2-5-2014
#
#
# uses the strigr library
#
# this script will source several other other scripts
#
#
#
###############################################################
#
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#
# get the current directory
# workingDirectory <- getwd()
# 
# define the NCCAASCIList vector
#
NCCAASCIIList <- list.files(path = ".", pattern = NULL, all.files = FALSE,
                            #list.files(path = ".", pattern = "D&*", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE)
# remove .txt and .csv filenames from the list
# may need to reduce this list to .01A .02A .03A .04A 05A 06A 07A 08A
NCCAASCIIList <- grep("D&*", NCCAASCIIList, ignore.case = FALSE, perl = FALSE, value = TRUE,
                      fixed = FALSE, useBytes = FALSE, invert = FALSE)
NCCAASCIIList <- grep("*.txt", NCCAASCIIList, ignore.case = FALSE, perl = FALSE, value = TRUE,
                      fixed = FALSE, useBytes = FALSE, invert = TRUE)
NCCAASCIIList <- grep("*.csv", NCCAASCIIList, ignore.case = FALSE, perl = FALSE, value = TRUE,
                      fixed = FALSE, useBytes = FALSE, invert = TRUE)
#
#
# then create the CSV files from the NCCA ASCII output files
#
#
# this script will isolate the header section and data section
# and export the data section into a separate .csv file 
# 
# three files will be saved
# 
# D&-20131002 DLSC YANGO ARCIA-1.01A_data.csv
# D&-20131002 DLSC YANGO ARCIA-1.01A_data.txt
# D&-20131002 DLSC YANGO ARCIA-1.01A_headers.txt
# 
# enter the NCCA ASCII file name
#
# fileName <- "D&-20131002 DLSC YANGO ARCIA-1.01A"
NCCAListLength <- length(NCCAASCIIList)
for (i in 1:NCCAListLength) {
  fileName <- NCCAASCIIList[i]
  # read the file
  NCCAASCIIdata <- readLines(fileName, n = 80, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  # trim the data to 6 characters
  NCCAASCIIdataTrim6 <- strtrim(NCCAASCIIdata, 6) # trim the vector to 6 characters
  #
  # separate the header lines from the data
  # find the first line of data table by searching for "     1" and subtracting 2
  numberHeaderLines <- pmatch("     1", NCCAASCIIdataTrim6) -2 # set the number of lines in the header section
  # reload the file using numberHeaderLines
  headerFile <- NCCAASCIIdata[1:numberHeaderLines]
  # headerFile <- readLines(fileName, n = numberHeaderLines, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  # set the name of the header file
  headerFileName <- paste(fileName, "_header.txt", sep = "")
  # save the header section as a text file
  cat(headerFile, file = headerFileName, sep = "\n")
  #
  # then get the entire file and save it as a fixed width formate data file
  #
  # separate the data file
  NCCAASCIIdata <- readLines(fileName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  # determine thelength of the data file
  lastLine <-length(NCCAASCIIdata)
  # and determine the number of header lines to skip
  firstLine <- pmatch("     1", NCCAASCIIdataTrim6) -1 # set the number of lines in the header section
  # remove the header lines
  NCCAASCIIdata <- NCCAASCIIdata[firstLine:lastLine]
  # set the name of the data file
  NCCAASCIIdataName <- paste(fileName, "_data.txt", sep = "")
  # finally save the data file
  cat(NCCAASCIIdata, file = NCCAASCIIdataName, sep = "\n")
  #
}
#
# clean up
rm(NCCAASCIIList)
rm(NCCAListLength)
# rm(NCCAASCIIdata) # need to keep thuis for the dataCSV.r script
rm(NCCAASCIIdataName)
rm(NCCAASCIIdataTrim6)
rm(fileName)
rm(firstLine)
rm(headerFile)
rm(headerFileName)
rm(i)
rm(lastLine)
rm(numberHeaderLines)
#
# 
# now parse the header files
# source("/Users/raymondnelson/Documents/R programming/activity_multi-chanel_device/headerDetails.r")
# transform the data to .csv format
# source("/Users/raymondnelson/Documents/R programming/activity_multi-chanel_device/dataCSV.r")  
# make a list of stimulus events
# source("/Users/raymondnelson/Documents/R programming/activity_multi-chanel_device/eventList.r")
