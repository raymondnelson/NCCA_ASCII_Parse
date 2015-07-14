############ R function to make a matrix of all stimulus tags #############
# 3-11-2014 Raymond Nelson
#
#
# uniqueExams()
# to make a character vector of the names of unique exams
# for recursion
#
# eventMatrix()
# to make a matrix of all stimulus tags for all charts for each exam
#
###################################
#
# function to locate unique exams 
# arugment must be a character vector of NCCA ASCII chart output filenames
#
uniqueExams <- function(x = "fileNames") {
  x <- unique(strtrim(x, nchar(x) - 6))
  return(x)
}
#
