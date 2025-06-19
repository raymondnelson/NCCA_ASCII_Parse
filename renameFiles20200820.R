fileNames <- list.files(pattern = "Test ")

library(readr)

newFileNames <- str_replace(fileNames, "Test ", "Test_")

cbind(fileNames, newFileNames)

for(i in 1:length(fileNames)) {
  
  file.rename(fileNames[i], newFileNames[i])
  
}

