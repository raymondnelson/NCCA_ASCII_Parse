# function to process the time series pneumograph data

# data should be centered at onset zero for these filters to work



library(stringr)



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# uniqueExams <- uniqueExams[1:2]



pneumoSigProc <- function(x=uniqueExams, 
                        output=FALSE, 
                        showNames=TRUE) {
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
  source('~/Documents/R_programming/NCCA_ASCII_Parse/MASmooth.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/tsVar.R', echo=TRUE)
  
  ###
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # add 2 new columns for the processed pneumoData
    examDF$c_UPneumoS <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoS <- rep(0, times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # make an empty list to hold the output
    outputList <- NULL
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # get the list of time series data for the charts in the exam
      # seriesDF <- get(uniqueSeries[j], pos=1)
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
        
        # apply the filter functions to the data frame columns
        
        chartDF$c_UPneumoS <- lowPass.886(x=chartDF$c_UPneumo)
        chartDF$c_LPneumoS <- lowPass.886(x=chartDF$c_LPneumo)
        
#         chartDF$UPneumoMean <- tsMean(x=chartDF$UPneumoS)
#         chartDF$UPneumoVar <- tsVar(x=chartDF$UPneumoS)
#         
#         chartDF$LPneumoMean <- tsMean(x=chartDF$LPneumoS)
#         chartDF$LPneumoVar <- tsVar(x=chartDF$LPneumoS)
        
        # plot.ts()
        
        # save the result to the list
        outputList[[k]] <- chartDF
        
        # save the chartDF to the examDF
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF

      } # end for loop over each chart in each series
      
      # name the data frames in the ouput list
      names(outputList) <- uniqueCharts

#       # save the list for the unique series
#       assign(uniqueSeries[j], seriesDF, pos=1)
      
    } # end loop over unique series

    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over unique exams
  
  # return the last
  if(output==TRUE) return(examDF) 
  
} # end pneumoSigProc function

####

# call the function to recursively apply the filters
pneumoSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)



