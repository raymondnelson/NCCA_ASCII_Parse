# function to process the time series pneumograph data
#
# data should be centered at onset zero for these filters to work
#
###########################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[2]



###



######################################

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
  
  uniqueExams <- x
  
  # source the scripts for the DSP filters
  source('~/Documents/R_programming/NCCA_ASCII_Parse/lowPass.886.R', echo=TRUE)
#   source('~/Documents/R_programming/NCCA_ASCII_Parse/MASmooth.R', echo=TRUE)
  
  # source the maxPeak, minPeak and interpolatePeaks functions
  source('~/Documents/R_programming/NCCA_ASCII_Parse/pneumoSigProcHelper.R', echo=TRUE)
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    # add new columns for the processed pneumo Data
    examDF$c_UPneumoDiff <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoDiff <- rep(0, times=nrow(examDF))
    examDF$c_PneumoDiff <- rep(0, times=nrow(examDF))
    examDF$c_UPneumoMid <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoMid <- rep(0, times=nrow(examDF))
    examDF$c_UPneumoInh <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoInh <- rep(0, times=nrow(examDF))
    examDF$c_UPneumoExh <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoExh <- rep(0, times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # make an empty list to hold the output
#    outputList <- NULL
    
    # loop over each unique series
    # j=3
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        #########
        
        chartDF$c_UPneumo <- lowPass.886(x=chartDF$c_UPneumo)
        chartDF$c_LPneumo <- lowPass.886(x=chartDF$c_LPneumo)
        
        # apply the filter functions to the data frame columns
        
        chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumo, y=5*cps, times=12) # was y=40, times=8 11/19/2015
        chartDF$c_UPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_UPneumo, y=1.333*cps), 
                                                 y=chartDF$c_UPneumo[maxPeak(x=chartDF$c_UPneumo, y=1.333*cps)])
        chartDF$c_UPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_UPneumo, y=1.333*cps), 
                                                 y=chartDF$c_UPneumo[minPeak(x=chartDF$c_UPneumo, y=1.333*cps)])
        chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumo, y=5*cps, times=12)
        chartDF$c_LPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_LPneumo, y=1.333*cps), 
                                                 y=chartDF$c_LPneumo[maxPeak(x=chartDF$c_LPneumo, y=1.333*cps)])
        chartDF$c_LPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_LPneumo, y=1.333*cps), 
                                                 y=chartDF$c_LPneumo[minPeak(x=chartDF$c_LPneumo, y=1.333*cps)])
        
        #########
        
        chartDF$c_UPneumoDiff <- chartDF$c_UPneumoInh - chartDF$c_UPneumoExh
        chartDF$c_LPneumoDiff <- chartDF$c_LPneumoInh - chartDF$c_LPneumoExh
        
        # chartDF$c_PneumoDiff <- chartDF$c_UPneumoMid - chartDF$c_LPneumoMid
        chartDF$c_PneumoDiff <- chartDF$c_UPneumo - chartDF$c_LPneumo
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF

      } # end for loop over each k chart in each series
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series

    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) return(examDF) 
  
} # end pneumoSigProc function

####

# call the function to recursively apply the filters
# pneumoSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)



