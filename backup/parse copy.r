############################################
# R Script to recursively parse NCCA ASCII text output to .csv
# 3-5-2014 Raymond Nelson
# 8-1-2014
# 6-9-2015
#
# requires 6 functions in the NCCAASCIIParse.r script
# in addition to 3 functions in the headerParse.r script
# 
#
# first put all NCCA ASCII output files in the same working directory
#
#
############################################

# requires "stringr" library
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library", quietly = TRUE)

# setwd("~/Documents/R programming/NCCA_ASCII_Parse/data/Backster_YouPhase_N6 ")

# setwd("~/Documents/R programming/NCCA_ASCII_Parse/PF090316 (Cleaned Copy)")

# may need to first fix all problem characters in file and directory names
source("~/Documents/R programming/NCCA_ASCII_Parse/fixFileNames.r")
fixFileNames()

#################

# load the library of functions to parse the NCCA ASCII text output
source("~/Documents/R programming/NCCA_ASCII_Parse/NCCAASCIIParse.r") 

# make a character vector of all charts in the working directory
fileNames <- getCharts("D&+") # call this first

# make a short list of unique exams
uniqueExamNames <- uniqueExams(fileNames) # need to call the getCharts function first

# parseUniqueExams(uniqueExamNames)
parseUniqueExams(uniqueExams(getCharts("D&+")))
                 
# separate the header info from the data for each chart within each exam
headerNames <- headerFile(fileNames) # call the getCharts() function first

# make a separate file from the data for each chart
dataNames <- dataFile(fileNames) # call the getCharts() function first

###########################################

# load the library of functions to parse the time series data file

source('~/Documents/R programming/NCCA_ASCII_Parse/dataParse.R', echo=TRUE)

dataToCSV(dataNames)



###########################################

# load the library of functions to parse the header file

source('~/Documents/R programming/NCCA_ASCII_Parse/headerParse.r')

# make a csv from the chart header info
# chartHeader(headerNames) # call the headerFile() function first

# make a csv table from from the stimulus text statements for each chart
# stimText(headerNames) # call the headerFile() function first

# make a csv table from the stimulus events for each chart
# eventTable(headerNames) # call the headerFile() function first



