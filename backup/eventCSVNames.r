############# R Functions for parsing NCCA ASCII header information ##############
# 3-10-2014 Raymond Nelson
#
#
# this script contains the following functions
#
# eventCSVNames()
# to make a character vector of the names of "*events.csv" files
#
#########################################
#
# function to make a character vector of the filenames 
# for all *events.csv" files
#
eventCSVNames <- function(x = "*_events.csv") {
  y <- list.files(path = ".", pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE)
  assign("eventNames", y, pos = 1)
  return(y)
}
#
