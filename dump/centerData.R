# set the center and range for the selected chart data
#
#
#
#######################################

# library(stringr)



# # get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[1]




# center the data at zero for all columns
centerData <- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # input x is a vector of unique exams names for the data frames 
  # in the global environment
  # use a search string "*_Data^" to identify the exams for which
  # the time series data is in the global invironment
  
  #####
  
  # first make a small private function to center a column
  centerColumn <- function(x) {
    # private function to set the onset value of the sensor data
    # to zero
    # x is a data column from a data frame of recorded time series data
    # out put is a vector of centered values
    ifelse(max(x)==min(x),
           x <- x*0,
           x <- x - x[1]
    )
    return(x)
  } # end private centerColumn() function
  
  ###
  
  uniqueExams <- x
  
  ###
  
  # loop over the exams
  for (i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    examDF <- get(paste0(examName, "_Data"), pos=1)
    
    
    # remove any previously centered columns
    examDFNames <- names(examDF)
    if(length(examDFNames[strtrim(examDFNames, 2)=="c_"]) > 0) next()
    
    examDFNames <- examDFNames[strtrim(examDFNames, 2)!="c_"]
    examDF <- examDF[,1:length(examDFNames)]
    
    
    # add additional columns for the centered data
    # after checking for existing centered data columns
    if(length(grep(".*c_.*", names(examDF)))!=0) next
    
    # if(length(grep(".*c_.*", names(examDF)))==0) {
      newColNames <- paste0("c_", names(examDF[11:ncol(examDF)]))
      #
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
    # }
    
#     # make an empty list to hold the output
#     outputList <- NULL
    
    # make a vector of the unique series in each exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for (j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series ", seriesName))
      
      # get a vector of unique chart names
      searchString <- paste0(seriesName, ".")
      uniqueCharts <- unique(grep(searchString, seriesDF$chartName, value=TRUE))
      
      # loop over unique charts for each series for each exam
      for (k in 1:length(uniqueCharts)) {
        # k=3
        chartName <- uniqueCharts[k]
        
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        
        # get the exam data 
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        # chartDF <- as.data.frame(get(paste0(examName, "_Data"), pos=1))
        
        if(showNames==TRUE) print(chartName)
        
        ##### apply the function to the data columns
        
        # loop over columns and use the function to center the data
        for (l in 11:(ncol(chartDF)-length(newColNames))) {
          # l=1
          # add a new column for each centered data vector
          chartDF[,l+length(newColNames)] <- centerColumn(chartDF[,l])
        } # end loop over l columns to center data
        
        ### save the chartDF to the larger seriesDF for all charts
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over k unique charts
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF
      
    } # end loop over j unique series

    # save the examDF to the global environment with the centered data
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the output from the last unique exam
  if(output==TRUE) return(examDF)
  
} # end centerData function

# centerData(x=uniqueExams, output=FALSE)


