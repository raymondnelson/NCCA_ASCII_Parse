# function for PLE signal processing 



########################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))


# uniqueExams <- uniqueExams[1]


#########################

PLESigProc <- function(x=uniqueExams, 
                          outputNames=FALSE, 
                          showNames=TRUE) { 
  # function to process the time series PLE data for the exams in the cwd
  # x input is a list of unique exams
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the data frame for the last exam series in the input
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  
  uniqueExams <- x
  
  # call the script with the helper functions
  # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=TRUE)  # minPeak()

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
    
    # skip the rest and go to the next chart if there is no PLE column in the data frame
    if(sum(pmatch(names(examDF), "c_PL", nomatch=0))==0) next
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    # add 3 new columns for the processed cardio data
    examDF$c_PLMax <- rep(0, times=nrow(examDF))
    examDF$c_PLMin <- rep(0, times=nrow(examDF))
    examDF$c_PLMA <- rep(0, times=nrow(examDF))
    examDF$c_PLAmp <- rep(0, times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # make an empty list to hold the output
    #    outputList <- NULL
    
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
        
        if(showNames==TRUE) print(chartName)
        
        # chartOnsetRow <- which(examDF$chartName==chartName)[1]
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ####
        
        chartDF$c_PL <- highPass.338628(chartDF$c_PL)
        
        # ts.plot(chartDF$c_PL)
        # ts.plot(highPass.338628(chartDF$c_PL))
        
        ####
        
        # use a function to get the min peak rows
        minOut <- minPeak(x=chartDF$c_PL, y=8)
        # change to y=12 for slow pulse 
        
        # get the min peak values for the min peak rows
        minVal <- chartDF$c_PL[na.omit(minOut)]
        
        ### 10-6-2015 use the tukey fence to remove 
        
        # keep only those min values that exceed the Tukey lower inner fence
#         minOut <- tukeyFence1(x=chartDF$c_PL, y=minOut, z=5)
#         minVal <- chartDF$c_PL[na.omit(minOut)]        
        
        ###
        
        # interpolate between the min peak values
        PLMinInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
        # plot.ts(diastolicInterp, ylim=c(-3,10))
        
        # add the vector to the diastolic cardio column 
        chartDF$c_PLMin <- PLMinInterp[1:nrow(chartDF)]
        # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        ####
        
        # get the max peak indices
        maxOut <- maxPeak(x=chartDF$c_PL, y=8)
        
        # get the max peak values
        maxVal <- chartDF$c_PL[maxOut]
        
        ### use the tukey fence to remove problems
        
        # keep only those min values that exceed the Tukey lower inner fence
#         maxOut <- tukeyFence1(x=chartDF$c_PL, y=maxOut, z=5)
#         maxVal <- chartDF$c_PL[na.omit(maxOut)]
        
        ###
        
        # interpolate between max peak values
        PLMaxInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
        # plot.ts(systolicInterp, ylim=c(-3,10))
        # myCardioData2$CardioSyst <- systolicInterp
        # ts.plot(myCardioData2[1:3000,c(1,2,6)])
        
        # add the systolic time series to the data frame
        chartDF$c_PLMax <- PLMaxInterp
        
        ####
        
        #### 
        
        # compute the smoothed PLE data
        smoothedPL <- MASmooth(x=chartDF$c_PL, y=30, times=4) # y=15, times=3 will show the respiration
        # plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))
        
        # add the smoothed PLE to the time series data frame
        chartDF$c_PLMA <- smoothedPL[1:nrow(chartDF)]
        # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        # add the PLE pulse amplitude data to the daeta frame
        chartDF$c_PLAmp <- chartDF$c_PLMax - chartDF$c_PLMin
        
        #####
        
        # save the chartDF in the output list
        # outputList[[k]] <- chartDF
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end for loop over each chart in each series
      
      # name the data frames in the ouput list
      # names(outputList) <- uniqueCharts
      
      #     # save the list for the unique series
      #     assign(uniqueSeries[j], seriesDF, pos=1)
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series
    
    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(outputNames==TRUE) return(examDF) 
  
} # end PLESigProg function


####

# call the function to recursively apply the filters
# PLESigProc(x=uniqueExams, output=FALSE, showNames=TRUE)


