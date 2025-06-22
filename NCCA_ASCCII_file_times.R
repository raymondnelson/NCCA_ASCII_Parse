# R script to reset the file mod time for NCCA ASCII files
# 2025 Jun 22
# Raymond Nelson
####
# to set consistent file times 
####






chartNames <- list.files(pattern="^D\\&-")





# new date-time (POSIXct format)
new_time <- as.POSIXct("2023-12-31 23:59:00", tz = "UTC")


resetNCCAASCIIFileTime <- TRUE
resetNCCAASCIIFileTime <- FALSE



if(resetNCCAASCIIFileTime) {
  
  for(i in 1:length(chartNames)) {
    
    # Set modification time
    Sys.setFileTime(chartNames[i], new_time)
    
  }
  
  print(paste("File times reset for", length(chartNames), "NCCA ASCII files."))
  
  resetNCCAASCIIFileTime <- FALSE
  
}


