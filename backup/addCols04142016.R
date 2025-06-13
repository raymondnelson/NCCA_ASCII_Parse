addCols <- function(x, y) {
  # function to add columns to the examDF
  # x is a data frame
  # y is a vector of new column names
  examDF <- x
  newColNames <- y
  for(i in 1:length(y)){
    newColName <- newColNames[i]
    examDF[newColName] <- rep(0, nrow(examDF))
  }
    return(examDF)
}

# addCols(x=examDF, y=newColNames)


