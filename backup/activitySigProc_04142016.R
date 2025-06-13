# function for activity sensor signal processing 


########################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))


# uniqueExams <- uniqueExams[1]


############



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15



#################



# call some helper functions
# source('~/dropbox/R_programming/NCCA_ASCII_Parse/lowPass.886.R', echo=FALSE)
# source('~/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=TRUE)



#######################################

activitySigProcFn <- function(x=uniqueExams, 
                       outputNames=FALSE, 
                       showNames=TRUE) {
  # function to process the time series PLE data for the exams in the cwd
  # x input is a list of unique exams
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the data frame for the last exam series in the input
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  
  # first source a script with the helper functions 
  # to process the cardio time series data
  
  uniqueExams <- x
  
  # call the script with the helper functions
  # source('~/dropbox/R_programming/NCCA_ASCII_Parse/activitySigProcHelper.R')
  
  # loop over each exam series and chart 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # skip the rest and go to the next chart if there is no activity column in the data frame
    if(sum(pmatch(names(examDF), "c_SE", nomatch=0))==0) next
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    # add 3 new columns for the processed cardio data
    examDF$c_SEMax <- rep(0, times=nrow(examDF))
    examDF$c_SEMin <- rep(0, times=nrow(examDF))
    examDF$c_SEMA <- rep(0, times=nrow(examDF))
    examDF$c_SEAmp <- rep(0, times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      # get the time series data for the series
      # seriesDF <- get(uniqueSeries[j], pos=1)
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # get the naems of unique charts
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartName <- uniqueCharts[k]
        
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(nrow(chartDF)<300) next()
        
        # chartOnsetRow <- which(examDF$chartName==chartName)[1]
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        if(showNames==TRUE) print(chartName)
        
        #####
        
        # first make sure that all peaks are recorded on a single sample
        
        chartDF$c_SE <- fixPeak(x=chartDF$c_SE, times=2)
        
        myData <- chartDF$c_SE
        
#         # use a helper functon to make a vector of slope values
#         mySlope <- slopeDir(x=myData)  
#         
#         # use a helper function to smooth the slope by removing slope changes of small duration
#         mySlope1 <- smoothSlope(x=mySlope, n=1)
#         
#         # fill the zero slope segments
#         mySlope2 <- fillSlope(x=mySlope1)
#         
#         mySlope3 <- positiveSlope(x=mySlope2)
#         
#         # locate the indices of all positive slope onset rows
#         exhMin <- positiveOnset(x=mySlope3)
#         
#         exhMin[1] <- 1
#         exhMin[length(myData)] <- 1
#         
#         exhMinVal <- myData[which(exhMin!=0)] 
#         
#         # interpolate between the exhalation min indices
#         exhMin2 <- interpolatePeaks(x=which(exhMin!=0), y=exhMinVal)
        
        chartDF$c_SEMin <- interpolatePeaks(x=minPeak(x=myData, y=40), y=myData[minPeak(x=myData, y=40)])
        
        ###

        # locate the idices of all nebaetive slope onset rows
#         exhMax2 <- interpolatePeaks(x=maxPeak(x=myData, y=40), y=myData[maxPeak(x=myData, y=40)])
        
        chartDF$c_SEMax <- interpolatePeaks(x=maxPeak(x=myData, y=40), y=myData[maxPeak(x=myData, y=40)])
        
        # process the min cycle
        
        # compute the inhalation max for each respiration cycle
        
        ###
        
        # compute the moving average of the activity sensor data
        chartDF$c_SEMA <- MASmooth(x=myData, y=150, times=8)
        # plot.ts(chartDF$c_SEMA)
        # plot.ts(chartDF$c_SE)
        #####
        
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
  if(outputNames==TRUE) return(examDF) 
  
} # end activitySigProcFn()

####

# call the function 
# activitySigProcFn(x=uniqueExams, output=FALSE, showNames=TRUE)


