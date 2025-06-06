# fix problem character strings in the NCCA ASCII output 

fixStringsFn <- function(x="D&+", oldString="-SA2R", newString="---SA") {
  
  fileNames <- list.files(pattern = x)
  
  # oldName <- oldString
  # newName <- newString
  
  i=1
  for (i in 1:length(fileNames)) {
    y <- readLines(fileNames[i], encoding="latin1")
    
    if(length(grep(oldString, y)) == 0) next()
    
    y[13:length(y)] <- gsub(oldString, newString, y[13:length(y)])

    writeLines(y, fileNames[i])
  }
  
} # end fixStringsFn() function

# fixStringFn(x="D&+", oldString="-S\\?2", newString="--S2")


# "      Aux01"
# "         SE"


