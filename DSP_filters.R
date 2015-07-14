# DSP filters for NCCA ASCII output
#
# data should be centered at zero (onset) for these filters to work
#
# source the rangeCenter.R script first
#
# make a separate function for each filter
# 
#
#
# this script contains the following functions
#
# 
# applyFilter()
# function to apply the filters recursively
# for all exams, series, and charts in the current working directory
#
#
# all time series data should be centered at onset zero for these filters to work
#
#call the centerData.R script before calling these filters
#
#
###############################################



# library(stringr)
# 
# # get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



##################



applyFilter <- function(x=uniqueExams, 
                        output=FALSE, 
                        showNames=FALSE) {
  # function to apply a digital filter to the time series data
  # x input is a list of unique exams
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the list for the last exam series in the nput
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  # then apply the filters
  # output is a list with the same name as the input list
  # ouput list contains the same data frames 
  
  ###
  
  # source the scripts for the DSP filters
  source('~/Documents/R_programming/NCCA_ASCII_Parse/lowPass.886.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/lowPass.2.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/highPass.03.R', echo=TRUE)
  
  ###
  
  uniqueExams <- x
  
  # loop over each chart in the list 
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

        chartData$UPneumoS <- lowPass.886(x=chartData$UPneumo)
        chartData$LPneumoS <- lowPass.886(x=chartData$LPneumo)
        chartData$AutoEDA <- lowPass.2(x=chartData$EDA1)
        chartData$AutoEDA <- highPass.03(data=chartData$AutoEDA)
        # plot.ts()  
        
        # save the result to the list
        seriesData[[k]] <- chartData
        
        if(showNames==TRUE) print(chartNames[k])
        
      } # end for loop over each chart in each series
      
      # save the list for the unique series
      assign(seriesNames[j], seriesData, pos=1)
      
    } # end loop over unique series
    
  } # end loop over unique exams
  
  # return the last
  if(output==TRUE) return(seriesData) 
  
} # end applyFilter function


####

# call the function to recursively apply the filters
applyFilter(x=uniqueExams, output=FALSE, showNames=TRUE)



