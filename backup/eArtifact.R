# electrodermal sensor artifacts
# 
# Y-axis excursion
# sum of absolute differences of each successive sample
# in the measured stimulus segment
#
# sampling rate is 30cps
# x input is a vector of time series data
# x length is from stimulus onset to 15 seconds after stimulus onset
# measurement is the sum of absolute differences between successive samples 
#
################################






# get exam names from the _Data data frames
library(stringr)
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[11]




electrodermalArtifactFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # function to iterate over a vector of data frame names 
  # and add activitt sensor artifacts to the time series data frame
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  ########
  
  uniqueExams <- x
  
  # source a script to load the helper functions
  # source('~/dropbox/R_programming/NCCA_ASCII_Parse/EDASigProcHelper.R', echo=FALSE)
  # minPeak()
  # maxPeak()
  # interpolatePeaks()
  # MASmooth()
  # source('~/R/NCCA_ASCII_Parse/TukeyFences.R', echo=FALSE)
  # tukeyFence2()
  #####
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # add the EDA artifact channels to the data frame
    examDF$c_AutoEDA_a <- rep("", times=nrow(examDF))
    examDF$c_AutoEDAMid_a <- rep("", times=nrow(examDF))
    examDF$c_AutoEDABase_a <- rep("", times=nrow(examDF))
    examDF$c_AutoEDAPeak_a <- rep("", times=nrow(examDF))
    examDF$c_AutoEDADiff_a <- rep("", times=nrow(examDF))

    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        chartName <- uniqueCharts[k]
        
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(nrow(chartDF)<300) next()
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- which(examDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        EDAData <- chartDF$c_AutoEDA
        EDAMid <- chartDF$c_AutoEDAMid
        
        ### finger movement artifacts 
        
        # make a vector to work with the EDA data
        EDAData <- chartDF$c_AutoEDA
        # set all values to NA if not less than the mid 
        # so that descending changes above the mide are not artifacted
        EDAData[which(chartDF$c_AutoEDA > chartDF$c_AutoEDAMid)] <- NA
        
        # then compute the lower tukey fence 
        # chartDF$c_AutoEDA_a[tukeyFence2(x=EDAData,y=EDAMid,z=3,fence="lower")] <- "Artifact"
        EDAArtifact <- tukeyFence4(x=EDAData,y=chartDF$c_AutoEDA,z=15,fence="lower")
        chartDF$c_AutoEDA_a[EDAArtifact] <- "Artifact"
        

        
        ### add the artifacts to the chart data frame
              
#         chartDF$c_AutoEDA_a[maxAmpChange] <- "Artifact"
#         # chartDF$c_AutoEDAMid_a[maxAmpChange] <- "Artifact"
#         # chartDF$c_AutoEDABase_a[minAmpChange] <- "Artifact"
#         chartDF$c_AutoEDAPeak_a[minAmpChange] <- "Artifact"
#         # chartDF$c_AutoEDADiff_a <-
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end activityArtifactFn()

# electrodermalArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)




