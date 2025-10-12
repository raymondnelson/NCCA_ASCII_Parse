# function to process the time series data
# including pneumo, EDA, cardio, PLE and activity sensors
# 4/23/2016
# Raymond Nelson

###########################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[2]


####



# x=uniqueExams
# output=FALSE
# showNames=TRUE
# i=1
# j=1
# k=1


{
  # source a script to load the helper functions
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  source(paste0(RPath, 'sigProc_filters.R'), echo=FALSE)
  
  source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)
  
  # source the scripts with signal processing functions for the sensors
  source(paste0(RPath, 'pneumoSigProc.R'), echo=FALSE)
  source(paste0(RPath, 'EDASigProc.R'), echo=FALSE)
  source(paste0(RPath, 'cardioSigProc.R'), echo=FALSE)
  source(paste0(RPath, 'FCSigProc.R'), echo=FALSE)
  source(paste0(RPath, 'eCardioSigProc.R'), echo=FALSE)
  source(paste0(RPath, 'PLESigProc.R'), echo=FALSE)
  source(paste0(RPath, 'activitySigProc.R'), echo=FALSE)
  
  source(paste0(RPath, "PTTSigProc.R"), echo=FALSE)
  
  source(paste0(RPath, 'DetrendedEDAFilter.R'), echo=FALSE)
}






sigProc <- function(uniqueExams=uniqueExams,
                    uniqueExamNames=uniqueExamNames,
                    scaleOffset=FALSE,
                    makeDF=TRUE ) {
  # function to process the NCCA ASCII time series data
  # including pneumo, EDA, cardio, PLE and activity sensors
  # uniquExams input is a vector of unique exams in the working directory
  # uniqueExamNames vector is used to determine the filter specs
  #
  # the input data is the output from the function in the preProc.R script and preProc() function
  # output=TRUE will output the time series for the last exam in the input vector of exam names
  #
  # uses some values from the global environment
  # EDAFilt
  # EDAHighPass
  # EDALowPass
  # EDABandPass
  # PneumoLowPass
  #
  # use scaleOffset=TRUE to perform scaling here
  # otherwise it can be done as a separate step
  #
  # use makeDF=TRUE to save the _Data data frame to the global env
  #
  ####
  
  ####  loop over each exam in the list ####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      # examDF <- get(examName, pos=1)
      
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      assign("examDF", examDF, pos=1)
      assign("examName", examName, pos=1)
      
      if(showNames==TRUE) print(paste("exam:", i, "of", length(uniqueExams), examName))
      
      # get the names of each unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
    }
    
    #### select the filter specs ####
    
    {  
      
      if(any(length(uniqueExamNames) == 0, is.null(uniqueExamNames),
             !exists("uniqueExamNames"))) {
        uniqueExamNames <- uniqueExams
      }
      
      # turn the filters off in the init script for non-Lafayette charts
      if(length(uniqueExamNames) > 0) {
        if(!grepl("^D&.*$", uniqueExamNames[i])) {
          # set the filter switches to FALSE
          # 2-23-2017 set EDAL and PnLow to TRUE to avoid problems with Axciton data
          # EDAHighPass is set in the NCCAASCII_init.R script
          EDAH <- EDAHighPass
          EDAL <- TRUE 
          EDAB <- FALSE
          PnLow <- TRUE
        } else {
          # get the values from the global environment
          EDAH <- EDAHighPass
          EDAL <- EDALowPass
          EDAB <- EDABandPass
          PnLow <- PneumoLowPass
        } 
      } else {
        # if uniqueExamNames is NULL
        EDAH <- FALSE
        EDAL <- EDALowPass
        EDAB <- EDABandPass
        PnLow <- PneumoLowPass
      }
      
      # choose the EDA filter
      if(grepl("^D&.*$", uniqueExamNames[i])) {
        # lafayette
        # EDAFilter <- "laf18" 
        EDAFilter <- EDAFilt
      } else if(grepl("^D\\$.*$", uniqueExamNames[i])) {
        # axciton
        EDAFilter <- "none"
      } else if(grepl("^D#.*$", uniqueExamNames[i])) {
        # Stoelting
        EDAFilter <- "laf18"
      } else if(grepl("^D%.*$", uniqueExamNames[i])) {
        # Limestone
        EDAFilter <- "none"
      } else EDAFilter <- EDAFilt
      
    }
    
    #### loop over each unique series ####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
    
      {
        
        seriesName <- uniqueSeries[j]
        
        # get the list of time series data for the charts in the exam
        # seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        # seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        # assign("seriesDF", seriesDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
        
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
      
      ######  loop over each chart in the series ######
      
      k=1
      for(k in 1:length(uniqueCharts)) {
      
        {
          
          chartName <- uniqueCharts[k]
          
          # get the data frame with the time series data for each chart in the series
          # chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          if(nrow(chartDF)<600) next()
          
          # chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          # assign("chartDF", chartDF, pos=1)
          # assign("chartName", chartName, pos=1)
          
          if(showNames==TRUE) print(uniqueCharts[k])
          
          # make a vector of event names
          # used to get the first and last event 
          # eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
          eventNames <- toupper(chartDF$Label[chartDF$Label!=""])
          
          # make a vector of row indices for each event
          eventIndices <- which(chartDF$Label!="")
          
          # remove excluded events
          # eventIndices <- eventIndices[-which(eventNames %in% excludeEvents)]
          # eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          
          ###### get indices for the first event onset and last event end ######
          
          if(length(eventNames) == 0) {
            print("no stimulus events. none processed")
            # next()
            firstEvent=1
            lastEventEnd=nrow(chartDF)
          } 
          
          firstEvent <- getFirstLastEventFn(x=chartDF)[1]
          lastEventEnd <- getFirstLastEventFn(x=chartDF)[2] - 450
          # assign("firstLastEvents", firstLastEvents, pos=1)
          
          # fix condition where there are no events that are not excluded
          if(is.na(firstEvent)) {
            firstEvent <- 1
            lastEvent <- nrow(chartDF)
            lastEventEnd <- nrow(chartDF)
          }
          
        }
        
        # assign("firsEvent", firstEvent, envir = .GlobalEnv)
        # assign("lastEventEnd", lastEventEnd, envir = .GlobalEnv)
        
        ######  Pneumo signal processing ######
        
        if(chartDF$UPneumo[1] != -9.9) {
          
          if(showNames==TRUE) print("  pneumo signal processing - upper and lower")
          
          # source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoSigProc.R", echo = FALSE)
          
          # firstEvent and lastEventEnd do not include the X and XX announcements
          chartDF <- pneumoSigProcFn(x=chartDF, 
                                     PneumoL=PnLow, # filter switch
                                     first=firstEvent, 
                                     last=lastEventEnd )
          
        }
        
        ##### EDA signal processing ######
        
        if(showNames==TRUE) print("  EDA signal processing")
        
        chartDF <- EDASigProcFn(x=chartDF,
                                EDAHigh=EDAH,
                                EDALow=EDAL,
                                EDAFilter=EDAFilter,
                                first=firstEvent, 
                                last=lastEventEnd )
        
        ###### additional EDA signal processing ########
        
        # EDAFilt="resp"
        # {
        #   lowPass_resp <- lowPass8th2hz_resp
        #   highPass_resp <- highPass8th1hz_resp
        #   chartDF <- EDASigProcFn(x=chartDF, EDAHigh=EDAH, EDALow=EDAL, first=firstEvent, last=lastEventEnd)
        # }
        # EDAFilt="laf"
        
        ###### Cardio signal processing ######
        
        if(chartDF$Cardio1[1] != -9.9) {
          
          if(showNames==TRUE) print("  cardio signal processing")
          
          # firstEvent and lastEventEnd do not include the X and XX announcements
          chartDF <- cardioSigProcFn(x=chartDF, 
                                     first=firstEvent, 
                                     last=lastEventEnd )
          
        }
        
        ##### electronic Cardio signal processing ########
        
        inclECardio <- FALSE
        
        # check to see if PLE data are present in the exam data frame
        if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0)) != 0) {
          
          inclECardio <- TRUE
          
          if(showNames==TRUE) print("  electronic cardio signal processing")
          
          # firstEvent and lastEventEnd do not include the X and XX announcements
          chartDF <- eCardioSigProcFn(x=chartDF, 
                                      first=firstEvent, 
                                      last=lastEventEnd )
          
        } # end if for electronic cardio
        
        ###### finger cuff signal processing ########
        
        inclFC <- FALSE
        
        # check to see if PLE data are present in the exam data frame
        if(sum(pmatch(names(examDF), "c_FC", nomatch=0)) != 0) {
          
          inclFC <- TRUE
          
          if(showNames==TRUE) print("  FC signal processing")
          
          # firstEvent and lastEventEnd do not include the X and XX announcements
          chartDF <- FCSigProcFn(x=chartDF, 
                                 first=firstEvent, 
                                 last=lastEventEnd )
          
        } # end if for finger cuff
        
        ##### PLE signal processing ######
        
        inclPLE <- FALSE
        
        # check to see if PLE data are present in the exam data frame
        if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0)) != 0) {

          inclPLE <- TRUE
          
          if(showNames==TRUE) print("  PLE signal processing")
          
          # assign("chartDF", chartDF, envir=.GlobalEnv)
          # stop()
          
          # firstEvent and lastEventEnd do not include the X and XX announcements
          chartDF <- PLESigProcFn(x=chartDF, 
                                  first=firstEvent, 
                                  last=lastEventEnd,
          												baseLine=PLEBaseline )

        } # end if for PLE
        
        ##### Activity sensor signal processing ######
      
        inclActivity <- FALSE
        
        # check to see if activity sensor data are present in the examDF
        if(sum(pmatch(names(examDF), "c_Move1", nomatch=0))!=0) {

          inclActivity <- TRUE
          
          if("Move1" %in% names(chartDF) && chartDF$Move1[1] != -9.9) {
            
            if(showNames==TRUE) print("  activity signal processing")
            
            # firstEvent and lastEventEnd do not include the X and XX announcements
            chartDF <- activitySigProcFn(x=chartDF, 
                                         first=firstEvent, 
                                         last=lastEventEnd )
            
          }

        } else print("  WARNING: missing activity sensor data") # end if for activity sensor data
        
        ##### PTT sensor signal processing ######
        
        inclPTT <- FALSE
        
        # check to see if activity sensor data are present in the examDF
        if(sum(pmatch(names(examDF), "c_PTTPTT", nomatch=0))!=0) {
          
          inclPTT <- TRUE
          
          if("PTTPTT" %in% names(chartDF) && chartDF$PTTPTT[1] != -9.9) {
            
            if(showNames==TRUE) print("  PTT signal processing")
            
            # assign("chartDF", chartDF, envir=.GlobalEnv)
            # stop()
            
            # firstEvent and lastEventEnd do not include the X and XX announcements
            chartDF <- PTTSigProcFn(x=chartDF,
                                    first=firstEvent,
                                    last=lastEventEnd )
            
          }
          
        }
        
        ###################################
        
        ####### Scale and offset the data ######
        
        # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
        
        {
          
          # input parameter
          # scaleOffset=TRUE
          
          if(!exists("scaleOffset")) scaleOffset=FALSE
          
          # to disable scaling and offsetting
          # and use the scaleOffsetData.R script instead
          # scaleOffset <- FALSE
          
          pneumoScale=TRUE
          EDAScale=TRUE
          cardioScale=TRUE
          PLEScale=TRUE
          activityScale=TRUE
          PTTScale=TRUE
          
        }
        
        ####
        
        {
          
          # scaleVal and newScaleVal are variables in the global environment,
          # created by the scaleDataFn() and offsetDataFn()
          
          ######  Pneumo scale and offset #######
          
          if(isTRUE(pneumoScale) && isTRUE(scaleOffset)) {

            if(showNames==TRUE) print("  pneumo scale and offset - upper and lower")

            # scale the pneumo data
            chartDF$c_UPneumoSm <- scaleDataFn(x=chartDF$c_UPneumoSm, sec=30, times=20, ignore=10, yRange=scaleVals['uPneumo'], maxY=yMaxVals['uPneumo'], minY=yMinVals['uPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            chartDF$c_LPneumoSm <- scaleDataFn(x=chartDF$c_LPneumoSm, sec=30, times=20, ignore=10, yRange=scaleVals['lPneumo'], maxY=yMaxVals['lPneumo'], minY=yMinVals['lPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))

            # offset the pneumo data
            chartDF$c_UPneumoSm <- offsetDataFn(x=chartDF$c_UPneumoSm, y=yOffset['uPneumo'], maxY=yMaxVals['uPneumo'], minY=yMinVals['uPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            chartDF$c_LPneumoSm <- offsetDataFn(x=chartDF$c_LPneumoSm, y=yOffset['lPneumo'], maxY=yMaxVals['lPneumo'], minY=yMinVals['lPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))

            ### make sure the pneumo channels are adequately separated at the first event
            if( (chartDF$c_UPneumoSm[firstEvent] - chartDF$c_LPneumoSm[firstEvent]) < 325) {
              # calculate the lowerOffset
              lowerOffset1 <- (chartDF$c_UPneumoSm[firstEvent] - 325) - chartDF$c_LPneumoSm[firstEvent]
              # then add the offset value to the vector
              chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm + lowerOffset1
            }

            ### make sure the pneumo channels are adequately separated at the last event
            if( (chartDF$c_UPneumoSm[(lastEventEnd)] - chartDF$c_LPneumoSm[(lastEventEnd)]) < 325) {
              # calculate the lowerOffset
              lowerOffset2 <- (chartDF$c_UPneumoSm[(lastEventEnd)] - 325) - chartDF$c_LPneumoSm[(lastEventEnd)]
              # add the second offset to the vector
              chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm + lowerOffset2
            }

            # compute the additional pneumo channels
            chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoSm, y=round(6*cps,0), times=10) # was y=40, times=8 11/19/2015
            chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumoSm, y=round(6*cps,0), times=10)

            # call the maxPeak functions from the sigProcHelper.R script
            maxPeaks <- maxPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0))
            # remove peak points accross consecutive samples
            consecPeaks <- which((maxPeaks[2:length(maxPeaks)]-1)==maxPeaks[1:(length(maxPeaks)-1)])
            if(length(consecPeaks > 0)) { maxPeaks <- maxPeaks[-consecPeaks] }
            # remove peak changes less than 1/4 second
            shortSegments <- which(diff(maxPeaks) <= round(.25*cps,0)) + 1
            if(length(shortSegments) > 0) maxPeaks <- maxPeaks[-shortSegments]

            # call the minPeak functions from the sigProcHelper.R script
            minPeaks <- minPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0))
            # remove peak points accross consecutive samples
            consecPeaks <- which((minPeaks[2:length(minPeaks)]-1)==minPeaks[1:(length(minPeaks)-1)])
            if(length(consecPeaks > 0)) { minPeaks <- minPeaks[-consecPeaks] }
            # remove peak changes less than 1/4 second
            shortSegments <- which(diff(minPeaks) <= round(.25*cps,0)) + 1
            if(length(shortSegments) > 0) minPeaks <- minPeaks[-shortSegments]

            # calculate the inhalation and exhalation lines
            chartDF$c_UPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0)),
                                                     y=chartDF$c_UPneumoSm[maxPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0))])
            chartDF$c_UPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0)),
                                                     y=chartDF$c_UPneumoSm[minPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0))])

            chartDF$c_LPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0)),
                                                     y=chartDF$c_LPneumoSm[maxPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0))])
            chartDF$c_LPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0)),
                                                     y=chartDF$c_LPneumoSm[minPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0))])

          }
          
          ##################### EDA scale and offset #####################
          
          if(isTRUE(EDAScale) && isTRUE(scaleOffset)) {

            if(showNames==TRUE) print("  EDA scale and offset")

            # first source the sigProcHelper.R script to load the scaleDataFn and offsetDataFn

            ### Auto EDA

            # scale the EDA data
            chartDF$c_AutoEDA <- scaleDataFn(x=chartDF$c_AutoEDA, sec=30, times=15, ignore=7, yRange=scaleVals['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))

            # offset the EDA data
            chartDF$c_AutoEDA <- offsetDataFn(x=chartDF$c_AutoEDA, y=yOffset['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))

            # scale and offset the additional EDA channels
            chartDF$c_AutoEDAMid <- MASmooth(x=chartDF$c_AutoEDA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
            chartDF$c_AutoEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0)),
                                                      y=chartDF$c_AutoEDA[maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0))])
            chartDF$c_AutoEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_AutoEDA, y=round(1.5*cps,0)),
                                                      y=chartDF$c_AutoEDA[minPeak(x=chartDF$c_AutoEDA, y=round(1.5*cps,0))])

            ### Manual EDA

            # scale the EDA data
            chartDF$c_ManualEDA <- scaleDataFn(x=chartDF$c_ManualEDA, sec=30, times=15, ignore=7, yRange=scaleVals['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))

            # offset the EDA data
            chartDF$c_ManualEDA <- offsetDataFn(x=chartDF$c_ManualEDA, y=(yOffset['eda']-250), maxY=950, minY=950, firstRow=firstEvent, lastRow=(lastEventEnd))

            # scale and offset the additional EDA channels
            chartDF$c_ManualEDAMid <- MASmooth(x=chartDF$c_ManualEDA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
            chartDF$c_ManualEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_ManualEDA, y=round(1*cps,0)),
                                                        y=chartDF$c_ManualEDA[maxPeak(x=chartDF$c_ManualEDA, y=round(1*cps,0))])
            chartDF$c_ManualEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_ManualEDA, y=round(1.5*cps,0)),
                                                        y=chartDF$c_ManualEDA[minPeak(x=chartDF$c_ManualEDA, y=round(1.5*cps,0))])

            ### LX auto eda output

            if( "c_EA" %in% colnames(chartDF) &
                length(which(is.na(chartDF$c_EA))) != nrow(chartDF) ) {

              # scale the EDA data
              chartDF$c_EA <- scaleDataFn(x=chartDF$c_EA, sec=30, times=15, ignore=7, yRange=scaleVals['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # offset the EDA data
              chartDF$c_EA <- offsetDataFn(x=chartDF$c_EA, y=(yOffset['eda']-25), maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd))

              # scale and offset the additional EDA channels
              # chartDF$c_EAMid <- MASmooth(x=chartDF$c_EA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
              # chartDF$c_EAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_EA, y=round(1*cps,0)),
              #                                      y=chartDF$c_EA[maxPeak(x=chartDF$c_EA, y=round(1*cps,0))])
              # chartDF$c_EABase <- interpolatePeaks(x=minPeak(x=chartDF$c_EA, y=round(1.5*cps,0)),
              #                                      y=chartDF$c_EA[minPeak(x=chartDF$c_EA, y=round(1.5*cps,0))])

            }

            ### also scale and offset the filtered EDA data

            # # scale the filtered EDA data
            # chartDF$c_EDAFilt <- scaleDataFn(x=chartDF$c_EDAFilt, sec=40, times=10, ignore=2, yRange=scaleVals[3], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd))
            #
            # # offset the filtered EDA data
            # chartDF$c_EDAFilt <- offsetDataFn(x=chartDF$c_EDAFilt, y=yOffset[3], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd))
            #
            # # scale and offset the additional filtered EDA channels
            # chartDF$c_EDAFiltMid <- MASmooth(x=chartDF$c_EDAFilt, y=round(5*cps,0), times=6)
            # chartDF$c_EDAFiltMax <- interpolatePeaks(x=maxPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0)),
            #                                          y=chartDF$c_EDAFilt[maxPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0))])
            # chartDF$c_EDAFiltMin <- interpolatePeaks(x=minPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0)),
            #                                          y=chartDF$c_EDAFilt[minPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0))])
            #
            # # compute the difference between Max and Min
            # chartDF$c_EDAFiltDiff <- chartDF$c_EDAFiltMax - chartDF$c_EDAFiltMin

          }
          
          ####### second EDA 2 scale and offset #######
          
          if(sum(pmatch(names(chartDF), "c_EDA2", nomatch=0)) != 0) {
            
            # chartDF$c_ManualEDA2 <- scaleDataFn(x=chartDF$c_ManualEDA2, sec=30, times=15, ignore=7, yRange=scaleVals['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
          }
          
          ####### Cardio scale and offset #######
          
          if(isTRUE(cardioScale) && isTRUE(scaleOffset)) {

            if(showNames==TRUE) print("  cardio scale and offset")

            # make sure that every cardio peak exists on a single sample
            # this was also done in the cardioSigProc function and is done again here
            # for additional peak correction
            chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1, times=2)

            # scale the cardio data
            chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=30, times=15, ignore=7, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))

            # offset the cardio data
            chartDF$c_Cardio1 <- offsetDataFn(x=chartDF$c_Cardio1, y=yOffset['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))

            # scale and offset the additional cardio channels

            # compute the cardio mid line
            chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.4*cps,0), times=3)

            # then compute a slower moving average
            # was 1.5 sec and times=2 3/17/2017
            # was 1.2 sec and times=3 6/9/2020
            chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(.8*cps,0), times=4)



            # compute the cardio rate to get the buffer to get the peaks
            cardioRate <- ratePerMin(chartDF$c_Cardio1,
                                     buffer=9,
                                     peaks="upper",
                                     lowPass=TRUE)
            bufferLen <- bufferLenFn(x=cardioRate, y=.6)

            # locate the systolic peaks
            systPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
            # compute the systolic line
            chartDF$c_CardioSystolic <-
              interpolatePeaks(x=systPeaks,
                               y=chartDF$c_Cardio1[systPeaks])[1:nrow(chartDF)]

            # locate the diastolic peaks
            diastPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
            # compute the diastolic line
            chartDF$c_CardioDiastolic <-
              interpolatePeaks(x=diastPeaks,
                               y=chartDF$c_Cardio1[diastPeaks])[1:nrow(chartDF)]

            # add the systolic and diastolic peaks to the data from
            # to use for peak to peak information processing
            chartDF$CardioPeak[systPeaks] <- "systolic"
            chartDF$CardioPeak[diastPeaks] <- "diastolic"

            # compute the beat to beat intervals for the systPeaks
            chartDF$c_cardioRateSystolic <-
              interpolatePeaks(x=systPeaks,
                               y=(60 / (diff(systPeaks) / 30)))[1:nrow(chartDF)]

            ######## electronic cardio cuff scale and offset ########

            # check to see if electronic cardio data are present in the exam data frame
            if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0)) != 0) {

              if(showNames==TRUE) print("  electronic cardio scale and offset")

              # make sure that every cardio peak exists on a single sample
              # this was also done in the cardioSigProc function and is done again here
              # for additional peak correction
              chartDF$c_eCardio <- fixPeak(x=chartDF$c_eCardio, times=2)

              # scale the electronic cardio cuff data
              chartDF$c_eCardio <- scaleDataFn(chartDF$c_eCardio, sec=30, times=15, ignore=7, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # offset the electronic cardio cuff data
              chartDF$c_eCardio <- offsetDataFn(x=chartDF$c_eCardio, y=yOffset['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # scale and offset the additional electronic cardio cuff channels

              chartDF$c_eCardioMid <- MASmooth(x=chartDF$c_eCardio, y=round(.5*cps,0), times=3)
              chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardio, y=round(1.5*cps,0), times=3)

              # chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardioMA, y=round(.5*cps,0), times=2)

              cardioRate <- ratePerMin(chartDF$c_eCardio,buffer=9,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate, y=.6)

              # calculate the systolic and diastolic lines
              maxOut <- maxPeak(x=chartDF$c_eCardio, y=bufferLen)
              maxVal <- chartDF$c_eCardio[maxOut]
              chartDF$c_eCardioSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
              minOut <- minPeak(x=chartDF$c_eCardio, y=bufferLen)
              minVal <- chartDF$c_eCardio[na.omit(minOut)]
              chartDF$c_eCardioDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]

            } # end if for electronic cardio

            ###### forearm cuff or finger cuff scale and offset ######

            # check to see if PLE data are present in the exam data frame
            if(sum(pmatch(names(examDF), "c_FC", nomatch=0)) != 0) {

              if(showNames==TRUE) print("  forearm cuff scale and offset")

              # make sure that every cardio peak exists on a single sample
              # this was also done in the cardioSigProc function and is done again here
              # for additional peak correction
              chartDF$c_FC <- fixPeak(x=chartDF$c_FC, times=2)

              # scale the finger cuff data
              chartDF$c_FC <- scaleDataFn(chartDF$c_FC, sec=30, times=15, ignore=7, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # offset the finger cuff data
              chartDF$c_FC <- offsetDataFn(x=chartDF$c_FC, y=yOffset['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # scale and offset the additional finger cuff channels
              # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)

              chartDF$c_FCMid <- MASmooth(x=chartDF$c_FC, y=round(.5*cps,0), times=3)
              chartDF$c_FCMA <- MASmooth(x=chartDF$c_FC, y=round(1.5*cps,0), times=3)

              # chartDF$c_FCMA <- MASmooth(x=chartDF$c_FCMA, y=round(.5*cps,0), times=2)

              cardioRate <- ratePerMin(chartDF$c_FC,buffer=9,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate, y=.6)

              maxOut <- maxPeak(x=chartDF$c_FC, y=bufferLen)
              maxVal <- chartDF$c_FC[maxOut]
              chartDF$c_FCSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
              minOut <- minPeak(x=chartDF$c_FC, y=bufferLen)
              minVal <- chartDF$c_FC[na.omit(minOut)]
              chartDF$c_FCDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]

            } # end if for finger cuff

          } # if cardio scale and offset
          
          #########  PLE scale and offset #########
          
          if(isTRUE(PLEScale) && isTRUE(scaleOffset)) {

            # check to see if PLE data are present in the exam data frame
            if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0)) != 0) {

              # make sure that every PLE peak exists on a single sample
              # this was also done in the PLESigProc function and is done again here
              # for additional peak correction
              chartDF$c_PPG1 <- fixPeak(x=chartDF$c_PPG1, times=2)

              if(showNames==TRUE) print("  PLE scale and offset")

              # scale the PLE data
              chartDF$c_PPG1 <- scaleDataFn(chartDF$c_PPG1, sec=30, times=15, ignore=7, yRange=scaleVals['ple'], maxY=yMaxVals['ple'], minY=yMinVals['ple'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # offset the PLE data
              chartDF$c_PPG1 <- offsetDataFn(x=chartDF$c_PPG1, y=yOffset['ple'], maxY=yMaxVals['ple'], minY=yMinVals['ple'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # scale and offset the additional PLE channels
              chartDF$c_PPG1MA <- MASmooth(x=chartDF$c_PPG1, y=30, times=4) # y=15, times=3 will show the respiration
              # chartDF$c_PPG1MA <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)

              # compute the cardio rate to get the buffer to get the peaks
              # 7-29-2017 to fix the bd systolic line in PLE data
              cardioRate <- ratePerMin(chartDF$c_PPG1,buffer=9,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate, y=.75)
              # cardioRate <- ratePerMin(chartDF$c_PPG1,buffer=3,peaks="upper",lowPass=TRUE)
              # bufferLen <- bufferLenFn(cardioRate)

              # get the systolic peaks
              maxOut <- maxPeak(x=chartDF$c_PPG1, y=bufferLen)
              # interpolate between systolic peaks
              chartDF$c_PPG1Max <- interpolatePeaks(x=maxOut, y=chartDF$c_PPG1[maxOut])[1:nrow(chartDF)]
              # get the diastolic peaks
              minOut <- minPeak(x=chartDF$c_PPG1, y=bufferLen)
              # interpolate between diastolic peaks
              chartDF$c_PPG1Min <- interpolatePeaks(x=minOut, y=chartDF$c_PPG1[minOut])[1:nrow(chartDF)]

            } # end if for PLE

          } # end if for extant PLE sensor data
          
          ###### Activity sensor scale and offset ######
          
          if(isTRUE(activityScale) && isTRUE(scaleOffset)) {

            # check to see if activity sensor data are present in the examDF
            if(sum(pmatch(names(examDF), "c_Move1", nomatch=0))!=0) {

              if(showNames==TRUE) print("  activity scale and offset")

              # scale the activity sensor data
              chartDF$c_Move1 <- scaleDataFn(x=chartDF$c_Move1, sec=30, times=15, ignore=7, yRange=scaleVals['activity'], maxY=yMaxVals['activity'], minY=yMinVals['activity'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # offset the activity sensor data
              chartDF$c_Move1 <- offsetDataFn(x=chartDF$c_Move1, y=yOffset['activity'], maxY=yMaxVals['activity'], minY=yMinVals['activity'], firstRow=firstEvent, lastRow=(lastEventEnd))

              # scale and offset the additions activity channels
              chartDF$c_Move1MA <- MASmooth(x=chartDF$c_Move1, y=round(6*cps,0), times=10)

              # scale and offset the processed activity sensor data
              # chartDF$c_Move1Proc <- scaleDataFn(chartDF$c_Move1Proc, sec=12, times=20, yRange=scaleVals[6], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd))
              # chartDF$c_Move1Proc <- offsetDataFn(x=chartDF$c_Move1Proc, y=(yOffset[6]), yMax=yMax, yMin=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd))

              # make a new processed activity data
              # was y=round(.0625*cps,0) 2020-06-6
              chartDF$c_Move1Proc <- MASmooth(x=chartDF$c_Move1, y=round(.5*cps,0), times=2)
              chartDF$c_Move1ProcMA <- MASmooth(x=chartDF$c_Move1Proc, y=round(6*cps,0), times=10)

              # scale and offset the additional activity channels
              cardioRate <- ratePerMin(chartDF$c_Move1Proc,buffer=3,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate)

              maxOut <- maxPeak(x=chartDF$c_Move1Proc, y=bufferLen)
              chartDF$c_Move1Max <- interpolatePeaks(x=maxOut, y=chartDF$c_Move1Proc[maxOut])[1:nrow(chartDF)]
              minOut <- minPeak(x=chartDF$c_Move1Proc, y=bufferLen)
              chartDF$c_Move1Min <- interpolatePeaks(x=minOut, y=chartDF$c_Move1Proc[minOut])[1:nrow(chartDF)]

              # commented out 4-25-2017 not sure if this is needed or used now
              # chartDF$c_Move1Result <- (chartDF$c_Move1Result - chartDF$c_Move1Result[1]) * (scaleVal * newScaleVal) + chartDF$c_Move1Proc[1] - (.05*yRange)

            } else print("  WARNING: missing activity sensor data") # end if for activity sensor data

          } # end if for extant activity sensor data
          
          ###### PTT sensor scale and offset ######
          
          if(isTRUE(PTTScale) && isTRUE(scaleOffset)) {
            
            # check to see if activity sensor data are present in the examDF
            if(sum(pmatch(names(examDF), "c_PTTPTT", nomatch=0))!=0) {
              
              if(showNames==TRUE) print("  PTT scale and offset")
              
              # invert the PTT data
              chartDF$c_PTTPTT_abs <- exp(chartDF$c_PTTPTT_abs)
              
              # scale the PTT sensor data
              
              chartDF$c_PTTPTT_abs <- scaleDataFn(x=chartDF$c_PTTPTT_abs, sec=30, times=15, ignore=7, yRange=scaleVals['PTTPTT'], maxY=yMaxVals['PTTPTT'], minY=yMinVals['PTTPTT'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the PTT sensor data
              chartDF$c_PTTPTT_abs <- offsetDataFn(x=chartDF$c_PTTPTT_abs, y=yOffset['PTTPTT'], maxY=yMaxVals['PTTPTT'], minY=yMinVals['PTTPTT'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # scale and offset the additions PTT channels
              chartDF$c_PTTPTT_MA <- MASmooth(x=chartDF$c_PTTPTT, y=round(7.5*cps,0), times=3)
              
            } 
            
          } # end if for extant PTT sensor data
          
          ###### end scale and center data ######
          
        }
        
        ###### save the chartDF to the seriesDF ######
        
        # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        # save the chartDF to the examDF
        examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF

      } # end for loop over each k chart in each series
      
      # save the seriesDF to the examDF
      # not needed when the chartDF is saved directly to the examDF
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series

    # save the examDF to the global environment
    if(makeDF==TRUE) assign(paste0(examName, "_Data"), examDF, pos=1)
    
    # reset the filters for each exam
    if(!grepl("D&.*$", uniqueExamNames[i])) {
      EDAH <- EDAHighPass
      EDAL <- EDALowPass
      EDAB <- EDABandPass
      PnLow <- PneumoLowPass
    }

  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  # output parameter exists in the global envir
  if(output==TRUE) { return(examDF) } else {
    return(uniqueExams)
  }
  
} # end sigProc function



# sigProc(x=uniqueExams, output=FALSE, showNames=TRUE)



