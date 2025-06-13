# functions for cardio signal processing 

# minPeak()
# maxPeak()
# minMaxPeak()
# MASmooth()

########################



# library(stringr)



# # call the script with the helper functions
# source('~/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=TRUE)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]
  


#########################

# a function to process the the cardio data for a list of input exams in the cwd

cardioSigProc <- function(x=uniqueExams, 
                          outputNames=FALSE, 
                          showNames=TRUE) {
  # function to process the time series cardio data
  # x input is a list of unique exams
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the data frame for the last exam series in the input
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  
  # first source a script with the helper functions 
  # to process the cardio time series data
  
  uniqueExams <- x
  
  # call the script with the helper functions
  # source('~/R/NCCA_ASCII_Parse/cardioSigProcHelper.R')

  # loop over each exam series and chart 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")

    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    # add some new columns for the processed cardio data
    examDF$c_CardioSystolic <- rep(0, times=nrow(examDF))
    examDF$c_CardioDiastolic <- rep(0, times=nrow(examDF))
    examDF$c_CardioMinMax <- rep(0, times=nrow(examDF))
    examDF$c_CardioMid <- rep(0, times=nrow(examDF))
    # CardioMA is a very slow smoothed cardio
    examDF$c_CardioMA <- rep(0, times=nrow(examDF))
    examDF$c_CardioAmp <- rep(0, times=nrow(examDF)) 
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      # get the time series data for the series
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # get the naems of unique charts
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        chartName <- uniqueCharts[k]
        
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        if(nrow(chartDF)<300) next()
        
        if(showNames==TRUE) print(chartName)
        
        ################################################
        
        # first ensure that all peaks are recorded on a single sample
        # because there are rare times in which the time series reduction 
        # can result in a peak value over 2 adjacent samples
        # interpolate duplicated peak points
        chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1, times=2)
        
        #### calculate the cardio rate
        
        cardioRate <- ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps)
        # ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="lower",dataRate=cps)
        # ts.plot(lowPass1.7hz.2nd(chartDF$c_Cardio1)[1000:1500])
        # ts.plot(chartDF$c_Cardio1[1000:1500])
        
        # calculate the buffer length for the cardio rate
        # bufferLen <- floor(1/cardioRate*60*cps*.60) - 1  # round down
        # bufferLen <- bufferLenFn(x = ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps))
        bufferLen <- bufferLenFn(cardioRate)
        ###
        
        # get the max peak values
        maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
        
        # ts.plot(lowPass2hz(chartDF$c_Cardio1[1000:3000]))
        # ts.plot(chartDF$c_Cardio1[1000:3000])
                
        # to calculate number of samples for the rate per min
        # 1/60*60*30/2
        # 1/80*60*30/2
        # 1/100*60*30/2
        
        # get the max peak values
        maxVal <- chartDF$c_Cardio1[maxOut]
        
        # interpolate between max peak values
        systolicInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
        
        # plot.ts(systolicInterp, ylim=c(-3,10))
        # myCardioData2$CardioSyst <- systolicInterp
        # ts.plot(myCardioData2[1:3000,c(1,2,6)])
        
        # add the systolic time series to the data frame
        chartDF$c_CardioSystolic <- systolicInterp
        
        ####
        
        # use a function to get the min peak rows
        minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
        # minOut <- minPeak(x=chartDF$c_Cardio1, y=4)
        
        # get the min peak values for the min peak rows
        minVal <- chartDF$c_Cardio1[na.omit(minOut)]
        
        # interpolate between the min peak values
        diastolicInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))

        # a test plot 
        # plot.ts(diastolicInterp, ylim=c(-3,10))
        
        # add the vector to the diastolic cardio column 
        chartDF$c_CardioDiastolic <- diastolicInterp[1:nrow(chartDF)]

        # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        ####
        
        # compute a min-Max of the cardio time series data
        
        # get the minMax output
        minMaxOut <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
        
        # get the minMax values
        minMaxVal <- chartDF$c_Cardio1[minMaxOut]
        
        # interpolate the minMax values
        minMaxInterp <- na.omit(c(interpolatePeaks(x=minMaxOut, y=minMaxVal), 0))[1:nrow(chartDF)]
        
        # plot.ts(minMaxInterp, ylim=c(-3,10))
        
        # add the time series to the data frame
        chartDF$c_CardioMinMax <- minMaxInterp
        
        #### 
        
        # compute the smoothed cardio dta
        # smoothedCardio <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
        
        # add the smoothed cardio to the time series data frame
        # chartDF$c_CardioMid <- smoothedCardio[1:nrow(chartDF)]
        chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
        
        # a test plot
        # plot.ts(chartDF$c_CardioMid[1:3000], ylim=c(-3,10))
        # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        #####
        
        # compute a more stable moving average for the cardio data
        # to help with evaluation of stability of the data
        # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y = round(.5*cps,0), times=12)
        chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMid, y=round(3*cps,0), times=3)
        
        #####
        
        # compute and add the cardio pulse amplitude to the time series data frame
        chartDF$c_CardioAmp <- chartDF$c_CardioSystolic - chartDF$c_CardioDiastolic
        
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
  
} # end cardioSicProg function


# call the function to recursively apply the filters to all uniqueExams
# cardioSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)


