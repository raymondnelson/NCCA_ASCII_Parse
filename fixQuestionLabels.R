# fix the question types for NCCA ASCII output with Lafayette charts


# set the working directory
setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/fromCharlesHonts/HontsProblemFile")


# initiolize a vector of NCCA ASCII file names
NCCAFileNames <- c()


# initialize a vector of bad question lables
badQuestionLables <- c("4", "5", "6", "7")


# initialize a vector of correct question lables
correctQuestionLables <- c("C4", "R5", "C6", "R7")
# this needs to be the same length as the 


# make a function to iterate over the files and questions
fixQuestionLablesFn <- function(x=NCCAFileNames,
                                y=badQuestionLables,
                                z=correctQuestionLables) {
  # iterate over the file names
  for (i in 1:length(x)) {
    
  }
}
