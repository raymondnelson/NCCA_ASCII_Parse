# script to open the NCCA ASCII text output 2-4-2014 raymond.nelson
# working script 2-4-2014
# requires the ListFiles.r script to define the NCCAASCIIList vector
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
# FileNameNChar <- nchar(fileName) # the length of the filename
#
# read the file
headerFile <- readLines(fileName, n = 80, ok = TRUE, warn = FALSE, encoding = "UTF-8")
# trim the data to 6 characters
headerFileTrim6 <- strtrim(headerFile, 6) # trim the vector to 6 characters
# find the first line of data table by searching for "     1" and subtracting 2
numberHeaderLines <- pmatch("     1", headerFileTrim6) -2 # set the number of lines in the header section
# reload the file using numberHeaderLines
headerFile <- readLines(fileName, n = numberHeaderLines, ok = TRUE, warn = FALSE, encoding = "UTF-8")
# set the name of the header file
headerFileName <- paste(fileName, "_header.txt", sep = "")
# save the header section as a text file
cat(headerFile, file = headerFileName, sep = "\n")
#
# then get the entire file and save it as a fixed width formate data file
#
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
#
# change the format of the data file to .csv
# 
# start by trimming leading and trailing spaces from all rows in the character vector "dataFile"
dataFile <- str_trim(dataFile, side = "both")
# first fix the -9.9 filler
dataFile <- str_replace_all(dataFile, "-9.9", "9.9")
# then remove dash characters
# uses "ZZZ" as a placeholder so the field is not lost
# when the spaces are substituted with commas at a later stage
dataFile <- str_replace_all(dataFile, "--------", "ZZZ") #8
dataFile <- str_replace_all(dataFile, "-------", "ZZZ") #7
dataFile <- str_replace_all(dataFile, "------", "ZZZ") #6
dataFile <- str_replace_all(dataFile, "-----", "ZZZ") #5
dataFile <- str_replace_all(dataFile, "----", "ZZZ") #4
dataFile <- str_replace_all(dataFile, "---", "ZZZ") #3
dataFile <- str_replace_all(dataFile, "--", "ZZZ") #2
dataFile <- str_replace_all(dataFile, "-", "ZZZ") #1
# remove excess spaces
dataFile <- str_replace_all(dataFile, "        ", " ") #8
dataFile <- str_replace_all(dataFile, "       ", " ") #7
dataFile <- str_replace_all(dataFile, "      ", " ") #6
dataFile <- str_replace_all(dataFile, "     ", " ") #5
dataFile <- str_replace_all(dataFile, "    ", " ") #4
dataFile <- str_replace_all(dataFile, "   ", " ") #3
dataFile <- str_replace_all(dataFile, "  ", " ") #2
# replace all spaces with comma
dataFile <- str_replace_all(dataFile, " ", ",")
# replace the ZZZ placeholders
dataFile <- str_replace_all(dataFile, "ZZZ", "")
# save the result
cat(dataFile, file = "trimdata.txt", sep = "\n")
# set the name of the data file
dataFileCSV <- paste(fileName, "_data.csv", sep = "")
# finally save the data file
cat(dataFile, file = dataFileCSV, sep = "\n")
#
# read the .csv 
# dataCSV <- read.csv("D&-20131002 DLSC YANGO ARCIA-1.01A_data.csv", nrows = -1)
# dataCSVnames <- names(dataCSV)
# dataCSVnames
}
# clean up
rm(dataFile)
rm(firstLine)
rm(lastLine)
rm(headerFile)
rm(headerFileName)
rm(headerFileTrim6)
rm(numberHeaderLines)
rm(dataFileName)
rm(fileName)
rm(dataFileCSV)
# rm(dataCSVnames)
rm(NCCAListLength)
rm(i)
