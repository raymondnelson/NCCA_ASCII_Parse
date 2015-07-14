# list the files in the current directory 2-4-2014 raymond.nelson
# working 2-4-2014
# script to construct a vector (NCCAASCIList) of filenames from the current directory
# including all ASCII output files in the NCCA ASCII format
# 
# first get the current directory
# workingDirectory <- getwd()
# 
NCCAASCIIList <- list.files(path = ".", pattern = NULL, all.files = FALSE,
#list.files(path = ".", pattern = "D&*", all.files = FALSE,
full.names = FALSE, recursive = FALSE,
ignore.case = FALSE, include.dirs = FALSE)
# remove .txt and .csv filenames from the list
NCCAASCIIList <- grep("D&*", NCCAASCIIList, ignore.case = FALSE, perl = FALSE, value = TRUE,
fixed = FALSE, useBytes = FALSE, invert = FALSE)
NCCAASCIIList <- grep("*.txt", NCCAASCIIList, ignore.case = FALSE, perl = FALSE, value = TRUE,
                      fixed = FALSE, useBytes = FALSE, invert = TRUE)
NCCAASCIIList <- grep("*.csv", NCCAASCIIList, ignore.case = FALSE, perl = FALSE, value = TRUE,
                      fixed = FALSE, useBytes = FALSE, invert = TRUE)
