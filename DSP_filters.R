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



library(stringr)
 


# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



##################



applyFilter <- function(x=uniqueExams, 
                        output=FALSE, 
                        showNames=TRUE) {
  # function to apply a digital filter to the time series data
  # x input is a vector of unique exam names
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the list for the last exam series in the nput
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  # then apply the filters
  # output is a list with the same name as the input list
  # ouput list contains the same data frames 
  
  ###
  
  # source the scripts for the DSP filters
#   source('~/Documents/R_programming/NCCA_ASCII_Parse/lowPass.886.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/lowPass.2.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/highPass.03.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/MASmooth.R', echo=TRUE)
#   source('~/Documents/R_programming/NCCA_ASCII_Parse/tsVar.R', echo=TRUE)
  
  ###
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(uniqueExams[i])
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # add a column
    examDF$c_AutoEDA <- rep(0, times=nrow(examDF))
    
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
      # seriesDF <- get(uniqueSeries[j], pos=1)
      
      # make a vector of unique exam names
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
        
        # apply the filter functions to the data frame columns

#         chartDF$UPneumoS <- lowPass.886(x=chartDF$UPneumo)
#         chartDF$LPneumoS <- lowPass.886(x=chartDF$LPneumo)
#         
#         chartDF$UPneumoMean <- tsMean(x=chartDF$UPneumoS)
#         chartDF$UPneumoVar <- tsVar(x=chartDF$UPneumoS)
#         
#         chartDF$LPneumoMean <- tsMean(x=chartDF$LPneumoS)
#         chartDF$LPneumoVar <- tsVar(x=chartDF$LPneumoS)
                
        chartDF$c_AutoEDA <- lowPass.2(x=chartDF$c_EDA1)
        chartDF$c_AutoEDA <- highPass.03(data=chartDF$c_AutoEDA)
        #chartDF$AutoEDA <- lowPass.886(x=chartDF$AutoEDA)
        # smooth the Auto EDA more to improve the feature extraction algorithm
        chartDF$c_AutoEDA <- MASmooth(x=chartDF$c_AutoEDA, y=5, times=6)
        # plot.ts()
        
        # save the result to the output list
        outputList[[k]] <- chartDF

        # save the chartDF to the examDF
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over unique charts

      # name the data frames in the ouput list
      names(outputList) <- uniqueCharts
      
    } # end loop over unique series
    
#     # save the list for the unique series
#     assign(uniqueSeries[j], seriesDF, pos=1)

    # save the examDF to the global environment with the centered data
    assign(paste0(examName, "_Data"), examDF, pos=1)
      
  } # end loop over unique exams
  
  # return the last
  if(output==TRUE) return(seriesDF) 
  
} # end applyFilter function

####

# call the function to recursively apply the filters
applyFilter(x=uniqueExams, output=FALSE, showNames=TRUE)



