# R script to manage NCCA ASCII file names
# 2025 May 4
# Raymond Nelson




{
  
  rm(list=ls())
  
  library(stringr)
  
  library(readr)
  
  if(!exists("RPath")) {
    # mac
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  if(!exists("NCCAASCIIChartNames")) {
    NCCAASCIIChartNames <- getCharts((x="D\\&"))
  }
  
  if(!exists("uniqueExamNames")) {
    uniqueExamNames <- uniqueNames(getCharts((x="D\\&")))
  }
  
  
  uniqueSeriesNames <- unique(str_sub(getCharts((x="D\\&")), -5, -5))
  
  
  uniqueChartNames <- unique(str_sub(getCharts((x="D\\&")), -3, -1))
  
  
  # nchar("ray")
  
}

#### rename the chart file extensions ####


# i=1
# for (i in 1:length(uniqueExamNames)) {
#   chartNames <- as.character(list.files(pattern=uniqueExamNames[i]))
#   chartNames <- chartNames[which(str_sub(chartNames, -1, -1) == "A")]
#   j=1
#   for(j in 1:length(chartNames)) {
#     # including the series name
#     newName <- paste0("D&-", uniqueExamNames[i], "-2.0", j, "A")
#     # stop()
#     if(file.exists(newName)) next()
#     file.rename(from=chartNames[j], to=newName)
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


newNames <- c("IrregDB", "RegActivity", "InstabDB", "severeDisruption", "deepBreaths", "baselineArtifacts", "DBIrreg", "DBInstabSevere", "activtyInstabSevere")


# i=1
# for (i in 1:length(uniqueExamNames)) {
#   chartNames <- list.files(pattern=uniqueExamNames[i]) 
#   for(j in 1:length(chartNames)) {
#     file.rename(from=chartNames[j], to=paste0("D&-", newNames[i], "-2.0", j, "A"))
#   }
# }


#### fix the criterion code in the NCCA ASCII chart name ####



# i=1
# for(i in 1:length(NCCAASCIIChartNames)) {
#   
#   thisChartName <- NCCAASCIIChartNames[i]
#   
#   thisCriterionState <- str_sub(thisChartName, -7, -7)
#   
#   if(thisCriterionState == "1") {
#     newChartName <- paste0(str_sub(thisChartName, 1, -9), "1", str_sub(thisChartName, -6, -1))
#   } else if(thisCriterionState == "0") {
#     newChartName <- paste0(str_sub(thisChartName, 1, -9), "0", str_sub(thisChartName, -6, -1))
#   }
#   
#   file.rename(from=thisChartName, to=newChartName)
#   
# }

