# function to process the time series pneumograph data

# data should be centered at onset zero for these filters to work



library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# uniqueExams <- uniqueExams[1:2]



pneumoSigProc <- function(x=uniqueExams, 
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
  source('~/Documents/R_programming/NCCA_ASCII_Parse/MASmooth.R', echo=TRUE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/tsVar.R', echo=TRUE)
  
  ###
  
  uniqueExams <- x
  
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
        
        # apply the filter functions to the data frame columns
        
        chartData$UPneumoS <- lowPass.886(x=chartData$UPneumo)
        chartData$LPneumoS <- lowPass.886(x=chartData$LPneumo)
        
#         chartData$UPneumoMean <- tsMean(x=chartData$UPneumoS)
#         chartData$UPneumoVar <- tsVar(x=chartData$UPneumoS)
#         
#         chartData$LPneumoMean <- tsMean(x=chartData$LPneumoS)
#         chartData$LPneumoVar <- tsVar(x=chartData$LPneumoS)
        
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
  
} # end pneumoSigProc function

####

# call the function to recursively apply the filters
pneumoSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)



