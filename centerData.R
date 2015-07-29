# set the center and range for the selected chart data
#
#
#
#######################################

library(stringr)



# # get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))




#####

# first make a small function to center a column
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

#####

# center the data at zero for all columns
centerData <- function(x=uniqueExams, output=FALSE) {
  # input x is a vector of unique exams names for the data frames 
  # in the global environment
  # use a search string "*_Data^" to identify the exams for which
  # the time series data is in the global invironment
  
  ###
  
  # loop over the exams
  # i=1
  for (i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    
    print(examName)
    
    examDF <- get(paste0(examName, "_Data"), pos=1)
    
    # remove any previously centered columns
    examDFNames <- names(examDF)
    examDFNames <- examDFNames[strtrim(examDFNames, 2)!="c_"]
    
    examDF <- examDF[,1:length(examDFNames)]
    
    # add additional columns for the centered data
    newColNames <- paste0("c_", names(examDF[11:ncol(examDF)]))
    # newColNames <- newColNames[strtrim(newColNames, 4)!="c_c_"]
    # m=1
    for (m in 1:length(newColNames)) {
      # this should not continue to add new columns
      # if the script is sourced repeatedly
      examDF <- cbind(examDF, rep(0, times=nrow(examDF)))
      # examDF[,(length(which(strtrim(names(examDF), 2)!="c_"))+1)] <- rep(0, times=nrow(examDF))
      # name each new column
      names(examDF)[ncol(examDF)] <- newColNames[m]
      # names(examDF)[(length(which(strtrim(names(examDF), 2)!="c_"))+m)] <- newColNames[m]
    }
    
    # make a vector of the unique series in each exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # make an empty list to hold the output
    outputList <- NULL
    
    # loop over each unique series
    # j=1
    for (j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      
      print(paste("series ", uniqueSeries[j]))
      
      # get a vector of unique chart names
      searchString <- paste0(seriesName, ".")
      uniqueCharts <- unique(grep(searchString, examDF$chartName, value=TRUE))
      
      # loop over unique charts for each series for each exam
      # k=1
      for (k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        
        print(chartName)
        
        chartOnsetRow <- which(examDF$chartName==uniqueCharts[k])[1]
        
        # get the exam data 
        chartDF <- examDF[examDF$chartName==chartName,]
        # chartDF <- as.data.frame(get(paste0(examName, "_Data"), pos=1))
        
        ###
        
        # loop over columns and use the function to center the data
        # l=11
        for (l in 11:(ncol(chartDF)-length(newColNames))) {
          # add a new column for each centered data vector
          chartDF[,l+length(newColNames)] <- centerColumn(chartDF[,l])
        } # end loop over columns to center data
        
        # add the centered data frame to the ouput list
        outputList[[k]] <- chartDF
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over unique charts
      
      # name the data frames in the ouput list
      names(outputList) <- uniqueCharts
      
    } # end loop over unique series

    # save the examDF to the global environment with the centered data
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over unique exams
  
  # return the output from the last unique exam
  if(output==TRUE) return(chartDF)
  
} # end function centerData

chartData <- centerData(x=uniqueExams, output=FALSE)


