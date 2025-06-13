# function to scale the exam chart data to a range of 0 to 100
#
#
# source the centerData.R and DSP_filters.R scripts first
#
#########################################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))




setRange <- function(x=uniqueExams, y=100, showNames=TRUE, output=FALSE) {
  # function to set range for all column ranges
  # x is a vector of unique exams in the global environment
  # y is the range of the output data
  # showNames will print the exam series and chart names to the console
  # outputData will return the list for the last exam series in the input
  
  ###
  
  # get the range from the function input y value
  colRange <- as.numeric(y)
  
  uniqueExams <- x
  
  ###
  
  # first make a small function to call for each column in each data frame
  setColRange <- function(x1, y) {
    # function to set each column range
    # x is a zero centered column from the data frome of recorded time series data
    # y is the max range value 
    #
    # set the range for the segment from 1 to 3 min or 1 to min or the entire segment
    ifelse(length(x1>=5400),
           rangeVal <- max(x1[1801:5400])-min(x1[1801:5400]),
           ifelse(length(x1>=3600),
                  rangeVal <- max(x1[1801:3600])-min(x1[1801:3600]),
                  rangeVal <- max(x1)-min(x1)
           )
    )
    rangeCoef <- y / rangeVal
    ifelse(rangeVal==0,
           x1 <- x1 * 0,
           x1 <- x1 * rangeCoef
    )
    return(x1)
  } # end setColRange function
  
  ### 
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")

    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        chartName <- uniqueCharts[k]
        
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(showNames==TRUE) print(chartName)
        
        # get the processed time series columns
        useCols <- c((ncol(chartDF)-length(names(chartDF[strtrim(names(chartDF), 2)=="c_"]))+1):(ncol(chartDF)))        
        
        # call the function for all data columns in the data frame
        for (l in min(useCols):max(useCols)) {
          # l=1
          chartDF[,l] <- setColRange(x1=chartDF[,l], y=colRange)
        } # end for loop to set all column ranges
  
       # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end for loop over each k chart
        
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF
      
    } # end for loop over each l series
    
    # save the examDF to the global environment with the centered data
    assign(paste0(examName, "_Data"), examDF, pos=1)
  
  } # end for loop over unique exam names
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) return(examDF) 
  
} # end setRange function

# setRange(x=uniqueExams, y=100, showNames=TRUE, output=FALSE)


