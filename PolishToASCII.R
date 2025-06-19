# convert polish character encoding "ISO8859-2" to "ASCII"

library(utils)

######

convertFileNamesToASCII <- function(x="ISO8859-2", y="ASCII") {
  fileNames <- list.files(pattern = "D..")
  newFileNames <- make.names(iconv(fileNames, from=x, to=y, sub="_"), unique=TRUE)
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], newFileNames[i])
  } # end loop
  return(paste(length(fileNames), "files renamed"))
} # end function

convertFileNamesToASCII()

######

fileNames <- list.files(pattern="D..")

convertTextToASCII <- function(txtFiles=fileNames, x="ISO8859-2", y="ASCII") {
  for (i in 1:length(fileNames)) {
    getFile <- txtFiles[i]
    txtDoc <- readLines(getFile, n=-1, ok=TRUE, warn=TRUE, encoding="IS08859-2", skipNul=TRUE)
    newTxtDoc <- sapply(txtDoc, iconv, from="ISO8859-2", to="ASCII", sub="_", simplify=TRUE, USE.NAMES = TRUE)
    writeLines(newTxtDoc, con=getFile)
  } # end loop
  return(paste(length(txtFiles), "files processed"))
} # end function

convertTextToASCII()

######

fileNames <- list.files(pattern="D..")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("D..", "D&.", fileNames[i]))
} # end loop
