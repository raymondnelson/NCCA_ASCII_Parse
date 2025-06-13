# R script to parse NCCA ASCII output
# working script 2-22-2014
#
# this script will open each NCCA ASCII text document and separate the header infor from the data
#
# uses the strigr library
#
##### this script will source several other other scripts at the end
# headerDetails.r
# eventList.r
# events.r
#
#
# this script should leave no objects in the workspace
#
###############################################################
#
# get the current directory
# workingDirectory <- getwd()
setwd("~/Documents/R programming/activity_multi-chanel_device/09N-0609output")
#
# library(stringr)
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#
##################################################################
#
# define the NCCAASCIList vector
#
NCCAASCIIList <- list.files(path = ".", 
                            pattern = NULL, 
                            all.files = FALSE,
                            full.names = FALSE, 
                            recursive = FALSE,
                            ignore.case = FALSE, 
                            include.dirs = FALSE)
#
# keep only NCCA ASCII output files
NCCAASCIIList <- grep("D&*", NCCAASCIIList, 
                      ignore.case = FALSE, 
                      perl = FALSE, 
                      value = TRUE,
                      fixed = FALSE, 
                      useBytes = FALSE, 
                      invert = FALSE)
#
# remove .txt and .csv filenames from the list
NCCAASCIIList <- grep("*.txt", NCCAASCIIList, 
                      ignore.case = FALSE, 
                      perl = FALSE, 
                      value = TRUE,
                      fixed = FALSE, 
                      useBytes = FALSE, 
                      invert = TRUE)
#
NCCAASCIIList <- grep("*.csv", NCCAASCIIList, 
                      ignore.case = FALSE, 
                      perl = FALSE, 
                      value = TRUE,
                      fixed = FALSE, 
                      useBytes = FALSE, 
                      invert = TRUE)
#
# then create the CSV files from the NCCA ASCII output files
#
# this script will isolate the header section and data section
# and export the data section into a separate .csv file 
# 
# three files will be saved
# 
# *_headers.txt
# *_data.txt
# *_data.csv
# 
# enter the NCCA ASCII file name
# fileName <- "D&-20131002 DLSC YANGO ARCIA-1.01A"
#
dataFrameList <- NULL
NCCAListLength <- length(NCCAASCIIList)
for (i in 1:NCCAListLength) {
  fileName <- NCCAASCIIList[i]
  # read the file
  # NCCAASCIIdata <- readLines(fileName, n = 80, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  NCCAASCIIdata <- readLines(fileName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
  # trim the data to 6 characters
  # NCCAASCIIdataTrim6 <- strtrim(NCCAASCIIdata, 6) # trim the vector to 6 characters
  NCCAASCIIdataTrim6 <- strtrim(NCCAASCIIdata[1:80], 6) # trim the vector to 6 characters
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
  # create a character vector for the header 
  assign(strtrim(headerFileName, nchar(headerFileName) -4), headerFile)
  #
  # then get the entire file and save it as a fixed width formate data file
  #
  #########################
  #
  # separate the data file
  # NCCAASCIIdata <- readLines(fileName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
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
  # change the format of the data file to .csv
  # 
  # start by trimming leading and trailing spaces from all rows in the character vector "NCCAASCIIdata"
  NCCAASCIIdata <- str_trim(NCCAASCIIdata, side = "both")
  # first fix the -9.9 filler
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-9.9", "9.9")
  # then remove dash characters
  # uses "ZZZ" as a placeholder so the field is not lost
  # when the spaces are substituted with commas at a later stage
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "--------", "ZZZ") #8
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-------", "ZZZ") #7
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "------", "ZZZ") #6
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-----", "ZZZ") #5
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "----", "ZZZ") #4
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "---", "ZZZ") #3
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "--", "ZZZ") #2
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-", "ZZZ") #1
  # remove excess spaces
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "        ", " ") #8
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "       ", " ") #7
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "      ", " ") #6
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "     ", " ") #5
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "    ", " ") #4
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "   ", " ") #3
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "  ", " ") #2
  # replace all spaces with comma
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, " ", ",")
  # replace the ZZZ placeholders
  NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "ZZZ", "")
  # save the result
  # # cat(NCCAASCIIdata, file = "trimdata.txt", sep = "\n") ######## this line may not be necessary
  # set the name of the data file
  NCCAASCIIdataCSV <- paste(fileName, "_data.csv", sep = "")
  # finally save the data file
  cat(NCCAASCIIdata, file = NCCAASCIIdataCSV, sep = "\n")
  #
  # create a vector for the header 
  NCCAASCIIdataCSV <- strtrim(NCCAASCIIdataCSV, nchar(NCCAASCIIdataCSV) -4)
  assign(NCCAASCIIdataCSV, as.data.frame(NCCAASCIIdata))
  # read the .csv 
  # dataCSV <- read.csv("D&-20131002 DLSC YANGO ARCIA-1.01A_data.csv", nrows = -1)
  # dataCSVnames <- names(dataCSV)
  # dataCSVnames 
  #
  # make a vector of data frames 
  dataFrameList <- c(dataFrameList, NCCAASCIIdataCSV)
}
#
# remove the exam header vectors 
# rm(list = ls(pattern = "*_header"))
# remove the exam data data.frames
# rm(list = ls(pattern ≠ "*_data"))
#
# clean up
#
rm(NCCAASCIIList)
rm(NCCAListLength)
rm(NCCAASCIIdata)
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
rm(NCCAASCIIdataCSV)
#
# # now parse the header files
source("/Users/raymondnelson/Documents/R programming/activity_multi-chanel_device/headerDetails.r")
# # and parse the stimulus events stimulus events
source("/Users/raymondnelson/Documents/R programming/activity_multi-chanel_device/stimulusEvents.r")
# #
# #
# # make a collection of all stimulus events for all charts
# source("/Users/raymondnelson/Documents/R programming/activity_multi-chanel_device/eventList.r")
#
############## END ###############################################
