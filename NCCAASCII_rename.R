# R script to manage NCCA ASCII file names
# Oct 31, 2024
# Raymond Nelson 
####




# setwd("~/Dropbox/R/NCCAASCII_data")
# setwd("~/Dropbox/R/NCCAASCII_data/respiration artifacts 2024/ncca")



{
  
  library(stringr)
  
  library(readr)
  
  if(!exists("RPath")) {
    # mac
    # RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  
  # set the NCCAASCII fileNameKey 
  # Lafayette
  # NCCAASCIIFileNameKey <- "D\\&"
  # Axciton <- 
  NCCAASCIIFileNameKey <- "D\\$"
  
  
  NCCAASCIIChartNames <- getCharts(x=NCCAASCIIFileNameKey)
  
  uniqueExamNames <- uniqueNames(getCharts(x=NCCAASCIIFileNameKey))
  
  uniqueSeriesNames <- str_sub(getCharts(x=NCCAASCIIFileNameKey), -5, -5)  

  uniqueChartNames <- str_sub(getCharts(x=NCCAASCIIFileNameKey), -3, -1)
  
  
}

# rename the chart file extensions 


# i=1
# for (i in 1:length(uniqueExamNames)) {
#   the the names of all charts for each exam
#   chartNames <- list.files(pattern=uniqueExamNames[i])
#   for(j in 1:length(chartNames)) {
#     file.rename(from=chartNames[j], to=paste0("D$-", uniqueExamNames[i], "-2.0", j, "A"))
#   }
# }




# [1] "DaveTestGreg-1"     
# "DaveTestMikeA-0"    
# "DougTestKaren-1"    
# "GlendaTestMikeA-0" 
# [5] "GregTestKaren-1"    
# "MikeATestDoug-0"    
# "OrlandoTestDavid-0" 
# "OrlandoTestKaren-1"
# [9] "TedTestDave-0" 


# newNames <- c("IrregDB", "RegActivity", "InstabDB", "severeDisruption", "deepBreaths", "baselineArtifacts", "DBIrreg", "DBInstabSevere", "activtyInstabSevere")


# rename the series number 

# i=1
# for (i in 1:length(uniqueExamNames)) {
#   chartNames <- list.files(pattern=uniqueExamNames[i])
#   for(j in 1:length(chartNames)) {
#     file.rename(from=chartNames[j], to=paste0("D&-", newNames[i], "-2.0", j, "A"))
#   }
# }




{
  
  # # scores
  # 
  # chartPlotNames <- list.files(pattern="pdf$")
  # 
  # for(i in 1:length(chartPlotNames)) {
  #   file.rename(chartPlotNames[i], paste0(str_sub(chartPlotNames[i], 1, -5), "_Scores.pdf"))
  # }
  
}


{
  
  # no scores
  
  # chartPlotNames <- list.files(pattern="pdf$")
  # 
  # for(i in 1:length(chartPlotNames)) {
  #   file.rename(chartPlotNames[i], paste0(str_sub(chartPlotNames[i], 1, -5), "_NoScores.pdf"))
  # }
  
}




