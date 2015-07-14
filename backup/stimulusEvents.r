# R script to remove blank lines and fill missing sample numbers for event onset, offset and answer events
# working 2-26-2014
# 
# 
# this script will make a collection of stimulus events for all charts for an exam 
#
# need to run the parseFilesCSV.r script before running this script
# also need to run the headerDetails.r script before this script.
# parseFilesCSV.r should source this script
#
##########################################################
#
# get a list of all charts
chartNames <- list.files(path = ".", pattern = "*events.csv", all.files = FALSE,
                         full.names = FALSE, recursive = FALSE,
                         ignore.case = FALSE, include.dirs = FALSE)
#
# number of charts
numberCharts  <- length(chartNames) 
#
# define some variables for later use
CSVNameList = NULL
#
# make a loop to iterate through the events for each chart
for (i in 1:numberCharts) {
  tempCSV <- read.csv(chartNames[i])
  # tempCSV <- read.csv(chartNames[1])
  tempCSV2 <- NULL
  # make a nested loop to remove lines with no data
  for (j in 1:nrow(tempCSV)) {
    if (as.vector(tempCSV[j,2]) != "") tempCSV2 <- rbind(as.vector(tempCSV2), as.vector(tempCSV[j,]))
    #
    # make another nested loop to process through each event and replace missing offset and answer timings
    for (k in 1:nrow(tempCSV2)) {
      if( is.na(tempCSV2[k,4])) tempCSV2[k,4] <- as.integer(tempCSV2[k,3])
      if( is.na(tempCSV2[k,5])) tempCSV2[k,5] <- as.integer(tempCSV2[k,4])
    }  
  }
  CSVName <- paste("CSVEvents", i, sep = "")
  assign(CSVName, tempCSV2) # using assign() allows the use of the variable name that is held by another variable
  #
  # save the .csv file 
  write.table(tempCSV2, file = chartNames[i], 
              append = FALSE, 
              quote = TRUE, 
              sep = ",", 
              eol = "\n", 
              na = "NA", 
              dec = ".", 
              row.names = FALSE, 
              col.names = TRUE, 
              qmethod = "double", 
              fileEncoding = "UTF-8")
  #
  # add the CSVName to the CSVNameList vector
  CSVNameList <- c(CSVNameList, CSVName)
}
#
# rm(list = ls(pattern = "CSVEvent*"))
# rm(CSVNameList)
#
# 
# clean up remaining objects
#
rm(tempCSV)
rm(tempCSV2)
rm(CSVName)
rm(i)
rm(j)
rm(k)
rm(chartNames)
rm(numberCharts)
#
################## END #########################################
