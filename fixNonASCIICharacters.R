# fix non-ASCII characters in text files

fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  x <- readLines(fileNames[i], encoding="latin1")
 
  x[1:100] <- iconv(x[1:100], "latin1", "ASCII", sub="_")
 
  writeLines(x, fileNames[i])

}


