# R script to make an array of stimulus questions for all charts within each exam
#
# need to check whether stimulus text is identical for all charts for each unique stim tag
# need a list of all unique stim tags for all charts
#
#############################################
#
# make a list of all chart names
getCharts <- function(x) {
  list.files(path = ".", 
             pattern = x, 
             all.files = FALSE,
             full.names = FALSE, 
             recursive = FALSE,
             ignore.case = FALSE, 
             include.dirs = FALSE)
}
#
chartNames <- getCharts("*stimuli.csv")
#
# number of charts
numberCharts  <- length(chartNames) 
#
# define some variables for later use - any variable used in a formula needs to be defined first
# eventLabelsMatrix <- NULL
maxEvents <- 0
numberEvents <- NULL
eventListNames <- NULL
#
# read the data from all CSV files in the eventList


