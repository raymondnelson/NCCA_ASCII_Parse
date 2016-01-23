# DSP filters for NCCA ASCII output
#
# data should be centered at zero (onset) for these filters to work
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
 


# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))




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
  
  uniqueExams <- x
  
  # source the scripts for the DSP filters
  source('~/Documents/R_programming/NCCA_ASCII_Parse/lowPass.2.R', echo=FALSE)
  source('~/Documents/R_programming/NCCA_ASCII_Parse/highPass.03.R', echo=FALSE)

  # source the script for some helper functions minPeak, MASmooth, maxPeak, interpolatePeaks
  source('~/Documents/R_programming/NCCA_ASCII_Parse/EDASigProcHelper.R', echo=FALSE)
  # source('~/Documents/R_programming/NCCA_ASCII_Parse/tsVar.R', echo=TRUE)
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")

    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    # add some columns
    examDF$c_AutoEDA <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDADiff <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDABase <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDAPeak <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDAMid <- rep(0, times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))

      seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
            
      # make a vector of unique exam names
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        # apply the filter functions to the data frame columns

        chartDF$c_AutoEDA <- chartDF$c_EDA1
                
        # call the high pass filter function
        chartDF$c_AutoEDA <- highPass.03(x=chartDF$c_AutoEDA)
        
        # call the low pass filter function
        chartDF$c_AutoEDA <- lowPass.2(x=chartDF$c_AutoEDA)

        # compute the moving average to show the tonic EDL activity
        chartDF$c_AutoEDAMid <- MASmooth(x=chartDF$c_AutoEDA, y=7.5*cps, times=10) # was y=150, times=6 11/26/2015
        
        # compute  the response peaks and interpolated peak lines
        
        chartDF$c_AutoEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_AutoEDA, y=1.333*cps), 
                                                  y=chartDF$c_AutoEDA[maxPeak(x=chartDF$c_AutoEDA, y=40)])
        chartDF$c_AutoEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_AutoEDA, y=1.333*cps), 
                                                  y=chartDF$c_AutoEDA[minPeak(x=chartDF$c_AutoEDA, y=40)])
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over unique charts

      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series
    
    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
      
  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) return(examDF) 
  
} # end applyFilter() function

# applyFilter(x=uniqueExams, output=FALSE, showNames=TRUE)



