# add artifact columns to the exam data frame
# 
# 
#
################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[5]




##############



artifactCols<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # function to iterate over a vector of data frame names 
  # and add artifact columns to the time series data frame
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  ########
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # add artifact columns for the centered and processed data
    if(length(grep("_a$", names(examDF)))!=0) next
    myColNames <- names(examDF)[which(strtrim(names(examDF), 2) == "c_")]
    newColNames <- paste0(myColNames, "_a")
    # newColNames <- paste0("c_", names(examDF[11:ncol(examDF)]))
    # newColNames <- newColNames[strtrim(newColNames, 4)!="c_c_"]
    for (m in 1:length(newColNames)) {
      # m=1
      # this should not continue to add new columns
      # if the script is sourced repeatedly
      examDF <- cbind(examDF, rep(0, times=nrow(examDF)))
      # examDF[,(length(which(strtrim(names(examDF), 2)!="c_"))+1)] <- rep(0, times=nrow(examDF))
      # name each new column
      names(examDF)[ncol(examDF)] <- newColNames[m]
      # names(examDF)[(length(which(strtrim(names(examDF), 2)!="c_"))+m)] <- newColNames[m]
    }
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      
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
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end artifactCols() function

# artifactCols(x=uniqueExams, showNames=TRUE, output=FALSE)




