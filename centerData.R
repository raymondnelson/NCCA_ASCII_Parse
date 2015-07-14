# set the center and range for the selected chart data
#
#
#
#######################################

library(stringr)



# # get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# chartName <- unique(PF090316_Stimuli$chartName)[1]

# get chart 1

# chartData <- PF090316_Data[PF090316_Data$chartName==chartName,]

# eventData <- PF090316_Stimuli[PF090316_Stimuli$chartName==chartName,]

# center the data at zero for all columns
centerData <- function(x=uniqueExams, output=FALSE, makeDF=TRUE) {
  # input x is a vector of unique exams names for the data frames 
  # in the global environment
  # use a search string "*_Data^" to identify the exams for which
  # the time series data is in the global invironment
  
  # first make a small internal function to center a column
  centerColumn <- function(x) {
    #function to set the onset value of the sensor data
    # to zero
    # x is a data column from a data frame of recorded time series data
    # out put is a vector of centered values
    ifelse(max(x)==min(x),
           x <- x*0,
           x <- x - x[1]
    )
    return(x)
  } # end centerColumn function
  
  ###
  
  # loop over the exams
  # i <- 1
  for (i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    stimData <- get(paste0(examName, "_Data"), pos=1)
    
    # remove extra characters from the lables column
    library(stringr)
    stimData$Label <- as.character(stimData$Label)
    stimData$Label <- str_replace_all(stimData$Label, "[- ]", "")
    
    # make a vector of the unique series in each exam
    # uniqueSeries <- unique(strtrim(stimData$chartName, 1))
    uniqueSeries <- unique(stimData$seriesName)
    
    # make an empty list to hold the output
    outputList <- NULL
    
    # loop over each unique series
    # j <- 1
    for (j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      searchString <- paste0(seriesName, ".")
      uniqueCharts <- unique(grep(searchString, stimData$chartName, value=TRUE))
      
      # loop over unique charts for each series for each exam
      # k <- 1
      for (k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        
        # get the exam data 
        myData <- stimData[stimData$chartName==chartName,]
        # myData <- as.data.frame(get(paste0(examName, "_Data"), pos=1))
        
        # loop over columns and use the function to center the data
        for (l in 7:ncol(myData)) {
          myData[,l] <- centerColumn(myData[,l])
        } # end loop over columns to center data
  
        # add the data frame to the ouput list
        outputList[[k]] <- myData
        
        
        
        
      } # end loop over unique charts
      
      # name the data frames in the ouput list
      names(outputList) <- uniqueCharts
      
      # save the output in a single list of centered chart data frames for each exam
      outputListName <- paste0(examName, "_", seriesName, "_Charts")
      # examChartDataName <- paste0(examName, "_", chartName, "_Data")
      if(makeDF==TRUE) {
        assign(outputListName, outputList, pos=1)
        # assign(examChartDataName, myData, pos=1)
      } # end if
      
    } # end loop over unique series

    
    
    
    } # end loop over unique exams
  
  # return the output from the last unique exam
  if(output==TRUE) return(myData)
  
} # end function centerData

chartData <- centerData(x=uniqueExams, output=FALSE, makeDF=TRUE)


