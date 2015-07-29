# function to scale the exam chart data to a range of 0 to 100
#
#
# source the centerData.R and DSP_filters.R scripts first
#
#########################################


library(stringr)


# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))


###

# first make a small function to call for each column in each data frame
setColRange <- function(x1, y) {
  # function to set each column range
  # x is a zero centered column from the data frome of recorded time series data
  # y is the max range value 
  # function to st the range of values to 100
  # input is a column vector from the chartDF data frame
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

setRange <- function(x=uniqueExams, y=100, showNames=TRUE, output=FALSE) {
  # function to set range for all column ranges
  # x is a vector of unique exams in the global environment
  # y is the range of the output data
  # showNames will print the exam series and chart names to the console
  # outputData will return the list for the last exam series in the input
  
  ###
  
  uniqueExams <- x
  
  # get the range from the function input y value
  colRange <- as.numeric(y)
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # make an empty list to hold the output
    outputList <- NULL
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartDF <- examDF[examDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(examDF$chartName==uniqueCharts[k])[1]
        
        ### get the processed time series columns
        useCols <- c((ncol(chartDF)-length(names(chartDF[strtrim(names(chartDF), 2)=="c_"]))+1):(ncol(chartDF)))        
        ###
        
        # call the function for all data columns in the data frame
        # l=
        for (l in min(useCols):max(useCols)) {
          chartDF[,l] <- setColRange(x1=chartDF[,l], y=colRange)
        } # end for loop to set all column ranges
  
        # save the result 
        outputList[[k]] <- chartDF
        
        # save the chartDF to the examDF
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end for loop over each chart
        
      # save the list for the unique series
      # assign(uniqueSeries[j], seriesDF, pos=1)
      
      # name the data frames in the ouput list
      names(outputList) <- uniqueCharts
      
    } # end for loop over each series
    
    # save the examDF to the global environment with the centered data
    assign(paste0(examName, "_Data"), examDF, pos=1)
  
  } # end for loop over unique exam names
  
  # return the last
  if(output==TRUE) return(examDF) 
  
} # end setRange function

setRange(x=uniqueExams, y=100, showNames=TRUE, output=FALSE)




# myData <- PF090316_1_Charts[[1]]
# head(myData)
# myEDA <- myData$AutoEDA
# max(myEDA)
# min(myEDA)
# range(myData$AutoEDA)
# range(myData$UPneumoS)
# range(myData$LPneumoS)
# range(myData$Cardio1)
  