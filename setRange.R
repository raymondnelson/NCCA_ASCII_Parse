# function to scale the exam chart data to a range of 0 to 100
#
#
# source the centerData.R and DSP_filters.R scripts first
#
#########################################

# library(stringr)
# 
# # get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

###

# first make a function to call for each column in each data frame
setColRange <- function(x1, y) {
  # function to set each column range
  # x is a zero centered column from the data frome of recorded time series data
  # y is the max range value 
  # function to st the range of values to 100
  # input is a column vector from the chartData data frame
  # maxVal <- 
  # minVal <- min(x)
  rangeVal <- max(x1)-min(x1)
  rangeCoef <- y / rangeVal
  ifelse(rangeVal==0,
         x1 <- x1 * 0,
         x1 <- x1 * rangeCoef
  )
  return(x1)
} # end setColRange function

###

setRange <- function(x=uniqueExams, y=100, showNames=FALSE, outputData=FALSE) {
  # function to set range for all column ranges
  # x is a vector of unique exams in the global environment
  # y is the range of the output data
  # showNames will print the exam series and chart names to the console
  # outputData will return the list for the last exam series in the input
  
#   ###
#   
#   # first make a private function to call for each column in each data frame
#   setColRange <- function(x1) {
#     # function to set each column range
#     # x is a zero centered column from the data frome of recorded time series data
#     # y is the max range value 
#     # function to st the range of values to 100
#     # input is a column vector from the chartData data frame
#     # maxVal <- 
#     # minVal <- min(x)
#     rangeVal <- max(x1)-min(x1)
#     rangeCoef <- colrange / rangeVal
#     ifelse(rangeVal==0,
#            x1 <- x1 * 0,
#            x1 <- x1 * rangeCoef
#     )
#     return(x1)
#   } # end setColRange function
#   
#   ###
  
  # get the range from the function input y value
  colRange <- as.numeric(y)
  
  # loop over each exam in the list 
  # i <- 1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0(examName, "*", "_Charts")
    seriesNames <- ls(pattern=glob2rx(searchString), pos=1)
    
    # loop over each unique series
    # j <- 1
    for(j in 1:length(seriesNames)) {
      
      if(showNames==TRUE) print(seriesNames[j])
      
      # get the list of time series data for the charts in the exam
      seriesData <- get(seriesNames[j], pos=1)
      chartNames <- names(seriesData)
      
      # loop over each chart in the series 
      # k <- 1
      for(k in 1:length(seriesData)) {
        # get the data frame with the time series data for each chart in the series
        chartData <- seriesData[[k]]
        
        if(showNames==TRUE) print(chartNames[k])
        
        ###
        
        # call the function for all data columns in the data frame
        # l <- 6
        for (l in 7:ncol(chartData)) {
          chartData[,l] <- setColRange(x1=chartData[,l], y=colRange)
        } # end for loop to set all column ranges
  
        # save the result 
        seriesData[[k]] <- chartData
        
      } # end for loop over each chart
        
      # save the list for the unique series
      assign(seriesNames[j], seriesData, pos=1)
      
    } # end for loop over each series
  
  } # end for loop over unique exam names
  
  # return the last
  if(outputData==TRUE) return(seriesData)
  
} # end setRange function

setRange(x=uniqueExams, y=100)

# myData <- PF090316_1_Charts[[1]]
# head(myData)
# myEDA <- myData$AutoEDA
# max(myEDA)
# min(myEDA)
# range(myData$AutoEDA)
# range(myData$UPneumoS)
# range(myData$LPneumoS)
# range(myData$Cardio1)
  