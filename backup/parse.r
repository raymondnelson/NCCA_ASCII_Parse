############################################
# R Script to recursively parse NCCA ASCII text output to .csv
# 3-5-2014 Raymond Nelson
# 8-1-2014
#
# requires 6 functions in the NCCAASCIIParse.r script
# in addition to 3 functions in the headerParse.r script
# 
#
############################################
#
# requires "stringr" library
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library", quietly = TRUE)
#
############################################
#
# set the working directory
setwd("~/Documents/R programming/NCCA_ASCII_Parse/data/Backster_YouPhase_N6 ")
#
# put all NCCA ASCII output files in the same working directory
#
# load the library of 6 functions to parse the NCCA ASCII text output
#
# source('~/Documents/R programming/activity_multi-chanel_device/NCCAASCIIParse.r')
source("~/Documents/R programming/NCCA_ASCII_Parse/NCCAASCIIParse.r") # will load the next 6 functions
#
# make a character vector of all charts in the working directory
fileNames <- getCharts("D&+") # call this first
#
# recursively separate the header info from the data for each chart
headerNames <- headerFile(fileNames) # call the getCharts() function first
#
# recursively make a separate file from the data for each chart
dataNames <- dataFile(fileNames)
#
# make a character vector of header vector file names in the working directory
# probably do not need this
headerNames <- getHeaderNames()
#
# make a character vector of data vector file names in the working directory
# probably do not need this
dataNames <- getDataNames()
#
# recursively parse all data files to .csv
dataToCSV(dataNames) # call the getDataNames() function first
#
###########################################
#
# load the library of 3 functions to parte the header file
#
# source('~/Documents/R programming/activity_multi-chanel_device/headerParse.r')
source('~/Documents/R programming/NCCA_ASCII_Parse/headerParse.r')
#
# make a csv from the chart header info
chartHeader("headerNames") # call the getHeaderNames() function first
#
# make a csv table from from the stimulus text statements for each chart
stimText("headerNames")
#
# make a csv table from the stimulus events for each chart
eventTable("headerNames")
#
