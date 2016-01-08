# fix activity sensor channel name

fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  x <- readLines(fileNames[i], encoding="latin1")
  
  x[10:100] <- gsub("      Aux01", "         SE", x[10:100])
  
  writeLines(x, fileNames[i])
}


