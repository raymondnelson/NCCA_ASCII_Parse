# R script to call the workFlow.R script at a scheduled interval


# as.numeric(as.POSIXct(Sys.time()))
# 
# 
# todayDate <- str_sub(Sys.time(), 1, 10)
# 
# currentTime <- str_sub(Sys.time(), 12, 19)
# 
# 
# 
# scheduledTime <- "11:30:00"




callNCCAASCIIParseWorkFlowFn <- function(scheduledTime <- "11:30:01") {
  # call the NCCA ASCII Parse work flow at a desired time 
  
  todayDate <- str_sub(Sys.time(), 1, 10)
  
  currentTime <- str_sub(Sys.time(), 12, 19)
  
  # scheduledTime <- "10:11:00"
  scheduledTime <- "11:30:01"
  
  schedPOSIXct <- as.numeric(as.POSIXct(paste(todayDate, scheduledTime, "EDT")))
  
  currentPOSIXct <- as.numeric(as.POSIXct(Sys.time()))
  
  waitPeriod <- round(schedPOSIXct - currentPOSIXct)
  
  Sys.sleep(waitPeriod)
  
  # print("Hello World")

  source("~/Dropbox/R/NCCA_ASCII_Parse/workFlow.R")

  
}

# callNCCAASCIIParseWorkFlowFn("10:10:00")
