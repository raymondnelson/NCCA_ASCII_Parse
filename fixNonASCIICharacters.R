# fix non-ASCII characters in text files
# changed this script to a function 10-4-2016
# raymond nelson
#####

fixNonASCIICharactersFn <- function(searchPattern="^D&+") {
  # function to fix non ASCII characters in NCCA ASCII text files 
  # non-Enlgish characters
  # x input is a character string to identify the NCCA ASCII text files
  ####
  
  # if(!exists("searchPattern", envir=.GlobalEnv)) searchPattern <- "^D&+"
  
  fileNames <- list.files(pattern = searchPattern)
  
  i=1
  for (i in 1:length(fileNames)) {
    x <- readLines(fileNames[i], encoding="latin1", warn=0)
  
    # Sep 12, 2023
    endLine <- pmatch("Sample     Time    Label", x) - 1
    
    x[1:endLine] <- iconv(x[1:endLine], "latin1", "ASCII", sub="_")
    
    writeLines(x, fileNames[i])
    
  }
  
  # rm(x)
  # rm(fileNames)
  # rm(i)
  
  return(fileNames)
} # end fixNonASCIICharactersFn() 

# fixNonASCIICharactersFn(searchPattern)
