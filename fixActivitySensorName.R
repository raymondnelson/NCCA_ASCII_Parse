#### fix activity sensor channel name

fixSensorNameFn <- function(x=searchPattern, oldSensorName="Aux01", newSensorName="   SE") {
  
  fileNames <- list.files(pattern = x)
  
  oldName <- oldSensorName
  newName <- newSensorName
  
  for (i in 1:length(fileNames)) {
    x <- readLines(fileNames[i], encoding="latin1")
    
    # 2025Oct1 only if the old sensor name is present
    if(any(grepl(oldName, x[5:150]))) {
      
      x[5:150] <- gsub(oldName, newName, x[5:150])
      
      writeLines(x, fileNames[i])
      
      print(fileNames[i])
    }
  }
  
} # end fixSensorNameFn() function

# fixSensorNameFn(x="D&+", oldSensorName <- "Aux01", newSensorName <- "   SE")


# "      Aux01"
# "         SE"





#### fix bad Move1 sensor


fixMove1Fn <- function(x=searchPattern, oldName="Move1", newName="MoveX") {
  
  fileNames <- list.files(pattern = x)
  
  for (i in 1:length(fileNames)) {
    x <- readLines(fileNames[i], encoding="latin1")
    
    # increment the loop if no Move1 sensor    
    if(!isTRUE(grepl(oldName, x[5:150]))) { next() }
    
    x[5:150] <- gsub(oldName, newName, x[5:150])
    
    writeLines(x, fileNames[i])
  }
  
} # end function


# fixMove1Fn(x=x="D&+", oldName="Move1", newName="MoveX")