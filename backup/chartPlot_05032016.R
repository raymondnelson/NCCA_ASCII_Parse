# plot an entire chart in one graphic
#
# sampling rate is 30cps
# 
#
################################


library(stringr)
library(ggplot2)
library(grid)


# source the list of excluded events so that measurements are not plotted for these
source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R')


# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[2]


# run the init script to set the plot parameters
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R')
# source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot_init.R')


# parameters to describe the data
# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15
# addSeg <- 5
# cardioLine <- "mid"


# # to control the print output
# showNames <- TRUE
# output <- FALSE
# showMeasurements <- TRUE
# showArtifacts <- TRUE
# outputChartFileName <- "_chartsPlot.pdf"
# separateCharts <- FALSE
# printPlot <- TRUE

 
# # plot a single segment instead of iterating through all exams in the global environment
# getSegment <- FALSE
# examNum <- 1
# seriesNum <- 1
# chartNum <- 2
# segmentNum <- "ALL"


if(getSegment == TRUE) {
  if(examNum!="ALL") uniqueExams <- uniqueExams[examNum] 
  # assign("examName", uniqueExams[examNum])
} 


# loop over each exam in the list and plot the charts
for(i in 1:length(uniqueExams)) {
  # i=1
  examName <- uniqueExams[i]
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Data", "*")
  
  # get the time series data
  examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  examStartRow <- 1
  examEndRow <- nrow(examDF)
  
  if(showNames==TRUE) print(examName)
  
  graphics.off()
  dev.new()
  
  if(printPlot == TRUE) {
    if(separateCharts==FALSE) {
      pdf(paste(examName, outputChartFileName, sep="_"), height=5.75, width=11)
    } else {
      pdf(paste(examName, chartName, outputChartFileName, sep="_"), height=5.75, width=11)
    }
  }
  
  # get the names of all unique series in the exam
  uniqueSeries <- as.character(unique(examDF$seriesName))
  
  if(getSegment == TRUE) {
    if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum]
  }
  
  # loop over each unique series
  for(j in 1:length(uniqueSeries)) {
    # j=1
    seriesName <- uniqueSeries[j]
    
    # get the time series for each unique series
    seriesDF <- examDF[examDF$seriesName==seriesName,]
    
    seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
    seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
    
    if(showNames==TRUE) print(paste("series", seriesName))
    
    # get the names of unique charts in the seires
    uniqueCharts <- as.character(unique(seriesDF$chartName))
    
    if(getSegment == TRUE) {
      if(chartNum!="ALL") uniqueCharts <- uniqueCharts[chartNum]
    }
    
    # loop over each chart in the series 
    for(k in 1:length(uniqueCharts)) {
      # k=1
      chartName <- uniqueCharts[k]
      
      # get the data frame with the time series data for each chart in the series
      chartDF <- seriesDF[seriesDF$chartName==chartName,]
      
      # skip short charts less than 10 seconds
      if(nrow(chartDF)<600) next()
      
      chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
      chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
      
      if(showNames==TRUE) print(chartName)
      
      if(getSegment == TRUE) { 
        if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
      }
      
      # initialize a pdf graphic device for the chart if separateCharts = TRUE
      if(printPlot == TRUE) {
        if(separateCharts==TRUE) {
          pdf(paste(examName, chartName, outputChartFileName, sep="_"), height=5, width=8)
        }
      }
      
      # make the plot title to include the exam and chart name
      plotTitle <- paste(examName, chartName)
      
      # make a vector of event names
      eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
      # and a vector of event onset rows
      eventRows <- which(chartDF$eventLabel!="")

      # get the first and last events
      firstEvent <- eventRows[!(eventNames %in% excludeEvents)][1]
      lastEvent <- eventRows[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
      lastEventEnd <- lastEvent + measuredSeg * cps
      # fix problem when lastEventEnd exceeds the data frame rows for the chart
      if(lastEventEnd > nrow(chartDF)) { lastEventEnd <- nrow(chartDF) }
      
      ###### set some warnings ######
      
      # set a warning for the X/XX announcements 
      XWarning <- ifelse( !("X" %in% eventNames) & !("XX" %in% eventNames),
                          "MISSING X AND XX ANNOUNCEMENTS",
                          ifelse(!("X" %in% eventNames),
                                 "MISSING X ANNOUNCEMENT",
                                 ifelse(!("XX" %in% eventNames),
                                        "MISSING XX ANNOUNCEMENT",
                                        "none") ) )
      
      # set a variable to determine if PLE data exist in the current chart
      inclPLE <- ifelse(sum(pmatch(names(chartDF), "c_PL", nomatch=0))>0,
                        TRUE,
                        FALSE)
      
      # set a variable for activity sensor warning
      activityWarning <- ifelse( !("c_SE" %in% names(examDF)),
                                 "MISSING ACTIVITY SENSOR DATA1",
                                 "none" )
      
      # check the cardio pulse rate
      cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",dataRate=cps)
      if(cardioRate < 60 | cardioRate > 100) {
        cardioRateWarning <- paste("CARDIO RHYTHM", cardioRate, "OUTSIDE NORMAL RANGE")
      } else cardioRateWarning <- "none"
      
      # check the respiration rate
      pneumoRate  <- ratePerMin(chartDF$c_UPneumoSm,buffer=40,peaks="upper",dataRate=cps)
      if(pneumoRate < 12 | pneumoRate > 20) {
        pneumoRateWarning <- paste("RESPIRATION RATE", pneumoRate, "OUTSIDE NORMAL RANGE")
      } else pneumoRateWarning <- "none"
      
      #############################################################
      
      
      
      
      
      
      
      
      #################### scale the data for the chart ####################
      
      # # set the scaling values for all sensors
      # # scaleVals <- c(35, 35, 170, 30, 30, 15)
      # # names(scaleVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity")
      # 
      # # scale the pneumo from 10 to 20 seconds
      # UPneumoScale <- ifelse(nrow(chartDF) < 600,
      #                        scaleVals['uPneumo'] / (max(chartDF$c_UPneumoSm[(1:nrow(chartDF))]) - min(chartDF$c_UPneumoSm[(1:nrow(chartDF))])),
      #                        scaleVals['uPneumo'] / (max(chartDF$c_UPneumoSm[301:600]) - min(chartDF$c_UPneumoSm[301:600])))
      # LPneumoScale <- ifelse(nrow(chartDF) < 600,
      #                        scaleVals['lPneumo'] / (max(chartDF$c_LPneumoSm[(1:nrow(chartDF))]) - min(chartDF$c_LPneumoSm[(1:nrow(chartDF))])),
      #                        scaleVals['lPneumo'] / (max(chartDF$c_LPneumoSm[301:600]) - min(chartDF$c_LPneumoSm[301:600])))
      # 
      # # scale the EDA data
      # EDAScale <- scaleVals['eda'] / (max(na.omit(chartDF$c_AutoEDA)) - min(na.omit(chartDF$c_AutoEDA)))
      # 
      # # scale the cardio pulse amplitude for 5 seconds at the first stimulus event
      # outDiff <- NULL
      # for (m in firstEvent:(firstEvent+149)) {
      #   outDiff <- c(outDiff, diff(range(chartDF$c_Cardio1[m:(m+29)])))
      # }
      # cardioScale <- scaleVals['cardio'] / mean(outDiff)
      # 
      # # scale the PLE from 10 to 20 seconds
      # if(inclPLE==TRUE) { PLScale <- scaleVals['ple'] / (max(chartDF$c_PL[301:600]) - min(chartDF$c_PL[301:600])) }
      # 
      # # scale the activity sensor data from 10 to 20 seconds
      # if(activityWarning=="none") {
      #   activitySEScale <- scaleVals['activity'] / (max(chartDF$c_SE[301:600]) - min(chartDF$c_SE[301:600]))
      # }
      # 
      # ### scale the data for plotting
      # 
      # chartDF$c_UPneumoSm <- chartDF$c_UPneumoSm * UPneumoScale
      # chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm * LPneumoScale
      # chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid * UPneumoScale
      # chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid * LPneumoScale
      # chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh * UPneumoScale
      # chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh * LPneumoScale
      # chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh * UPneumoScale
      # chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh * LPneumoScale
      # 
      # chartDF$c_AutoEDA <- chartDF$c_AutoEDA * EDAScale
      # chartDF$c_AutoEDAMid <- chartDF$c_AutoEDAMid * EDAScale
      # chartDF$c_AutoEDAPeak <- chartDF$c_AutoEDAPeak * EDAScale
      # chartDF$c_AutoEDABase <- chartDF$c_AutoEDABase * EDAScale
      # 
      # chartDF$c_Cardio1 <- chartDF$c_Cardio1 * cardioScale
      # chartDF$c_CardioMid <- chartDF$c_CardioMid * cardioScale
      # chartDF$c_CardioMA <- chartDF$c_CardioMA * cardioScale
      # chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic * cardioScale
      # chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic * cardioScale
      # 
      # if(inclPLE==TRUE) {
      #   chartDF$c_PL <- chartDF$c_PL * PLScale
      #   chartDF$c_PLMA <- chartDF$c_PLMA * PLScale
      #   chartDF$c_PLMax <- chartDF$c_PLMax * PLScale
      #   chartDF$c_PLMin <- chartDF$c_PLMin * PLScale
      #   chartDF$PLEMeans <- as.numeric(chartDF$PLEMeans) * PLScale
      # } # end if to include PLE data
      # 
      # if(activityWarning=="none") {
      #   chartDF$c_SE <- chartDF$c_SE * activitySEScale
      #   chartDF$c_SEMA <- chartDF$c_SEMA * activitySEScale
      #   chartDF$c_SEMin <- chartDF$c_SEMin * activitySEScale
      #   chartDF$c_SEMax <- chartDF$c_SEMax * activitySEScale
      # }
      
      ####################################### offset the stimulus segments
      
        # # Get the offset values
        # UPneumoOffset <- chartDF$c_UPneumoSm[firstEvent]
        # LPneumoOffset <- chartDF$c_LPneumoSm[firstEvent]
        # AutoEDAOffset <- chartDF$c_AutoEDA[firstEvent]
        # Cardio1Offset <- chartDF$c_CardioMid[firstEvent]
        # if(inclPLE==TRUE) PLOffset <- chartDF$c_PLMA[firstEvent]
        # if(activityWarning == "none") ActivityOffset <- chartDF$c_SEMA[firstEvent]
        # 
        # # # set the y offset value for the plot
        # yOffset <- c(130, 75, 10, -45, -110, -145)
        # names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity")
        # 
        # ### offset the data for plotting
        # 
        # # pneumo offset
        # chartDF$c_UPneumoSm <- chartDF$c_UPneumoSm - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm - LPneumoOffset + yOffset[2]
        # chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid - LPneumoOffset + yOffset[2]
        # chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh - LPneumoOffset + yOffset[2]
        # chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh - LPneumoOffset + yOffset[2]
        # 
        # # EDA offset
        # chartDF$c_AutoEDA <- chartDF$c_AutoEDA - AutoEDAOffset + yOffset[3]
        # chartDF$c_AutoEDAMid <- chartDF$c_AutoEDAMid - AutoEDAOffset + yOffset[3]
        # chartDF$c_AutoEDAPeak <- chartDF$c_AutoEDAPeak - AutoEDAOffset + yOffset[3]
        # chartDF$c_AutoEDABase <- chartDF$c_AutoEDABase - AutoEDAOffset + yOffset[3]
        # 
        # # cardio offset
        # chartDF$c_Cardio1 <- chartDF$c_Cardio1 - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioMid <- chartDF$c_CardioMid - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioMA <- chartDF$c_CardioMA - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - Cardio1Offset + yOffset[4]
        # 
        # # PLE offset
        # if(inclPLE==TRUE) {
        #   chartDF$c_PL <- chartDF$c_PL - PLOffset + yOffset[5]
        #   chartDF$c_PLMA <- chartDF$c_PLMA - PLOffset + yOffset[5]
        #   chartDF$c_PLMax <- chartDF$c_PLMax - PLOffset + yOffset[5]
        #   chartDF$c_PLMin <- chartDF$c_PLMin - PLOffset + yOffset[5]
        # } # end if for PLE
        # 
        # # activity offset
        # if(activityWarning == "none") {
        #   chartDF$c_SE <- chartDF$c_SE - ActivityOffset + yOffset[6]
        #   chartDF$c_SEMA <- chartDF$c_SEMA - ActivityOffset + yOffset[6]
        #   chartDF$c_SEMin <- chartDF$c_SEMin - ActivityOffset + yOffset[6]
        #   chartDF$c_SEMax <- chartDF$c_SEMax - ActivityOffset + yOffset[6]
        # }

        
        
        
        
        
        ###### keep the data on the plot ######
        
        # # re-offset the pneumo data to keep the data on the plot
        # if(max(chartDF$c_UPneumoSm) > 165) {
        #   newPneumoOffset <- max(chartDF$c_UPneumoSm[1:(length(chartDF$c_UPneumoSm)-150)]) - 166
        #   chartDF$c_UPneumoSm <- chartDF$c_UPneumoSm - newPneumoOffset
        #   chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm - newPneumoOffset
        #   # include the mid inhalation and exhalation trend lines
        #   chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid - newPneumoOffset
        #   chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid - newPneumoOffset
        #   chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh - newPneumoOffset
        #   chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh - newPneumoOffset
        #   chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh - newPneumoOffset
        #   chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh - newPneumoOffset
        #   yOffset[1] <- yOffset[1] - newPneumoOffset
        #   yOffset[1] <- yOffset[2] - newPneumoOffset
        #   pneumoWarning <- "UNSTABLE PNEUMO DATA1"
        # } else {
        #   pneumoWarning <- "none"
        #   newPneumoOffset=0
        #   }
      
        
        
      
      
        
        ### rescale the cardio data if necessary
        
        # check the cardio range
        # cardioWarning <- ifelse( diff(range(chartDF$c_Cardio1[firstEvent:lastEventEnd]))>=330,
        #                          "UNSTABLE CARDIO DATA1",
        #                          "none"
        # )
        
        # if(cardioWarning=="UNSTABLE CARDIO DATA") {
        #   # rescale the unstable cardio
        #   reScaleCardio <- 330/diff(range(chartDF$c_Cardio1[firstEvent:lastEventEnd]))
        #   chartDF$c_Cardio1 <- chartDF$c_Cardio1 * reScaleCardio
        #   chartDF$c_CardioMid <- chartDF$c_CardioMid * reScaleCardio
        #   chartDF$c_CardioMA <- chartDF$c_CardioMA * reScaleCardio
        #   chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic * reScaleCardio
        #   chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic * reScaleCardio
        #   # re offset the unstable cardio
        #   reOffsetCardio <- min(chartDF$c_Cardio1[firstEvent:lastEventEnd]) - -165
        #   chartDF$c_Cardio1 <- chartDF$c_Cardio1 - reOffsetCardio
        #   chartDF$c_CardioMid <- chartDF$c_CardioMid - reOffsetCardio
        #   chartDF$c_CardioMA <- chartDF$c_CardioMA - reOffsetCardio
        #   chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - reOffsetCardio
        #   chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - reOffsetCardio
        #   yOffset[4] <- yOffset[4] - reOffsetCardio
        # } else reOffsetCardio=0
        
        
        

      
      
        # re-offset the stable cardio data to keep the data on the plot
        # if(cardioWarning=="none") {
        # 
        #   if(min(chartDF$c_Cardio1[firstEvent:lastEventEnd]) <= -165) {
        #     newCardio1Offset <- min(chartDF$c_Cardio1[firstEvent:lastEventEnd]) - -165
        #     cardioWarning <- "UNSTABLE CARDIO DATA2"
        #   } else if(max(chartDF$c_Cardio1[firstEvent:lastEventEnd]) >= 165) {
        #     newCardio1Offset <- max(chartDF$c_Cardio1[firstEvent:lastEventEnd]) - 165
        #     cardioWarning <- "UNSTABLE CARDIO DATA3"
        #   } else newCardio1Offset <- 0
        # 
        #   if(newCardio1Offset!=0) {
        #     chartDF$c_Cardio1 <- chartDF$c_Cardio1 - newCardio1Offset
        #     chartDF$c_CardioMid <- chartDF$c_CardioMid - newCardio1Offset
        #     chartDF$c_CardioMA <- chartDF$c_CardioMA - newCardio1Offset
        #     chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - newCardio1Offset
        #     chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - newCardio1Offset
        #     yOffset[4] <- yOffset[4] - newCardio1Offset
        #   }
        # 
        # } # end if to reoffset stable cardio if necessary
        
        
        
        
        
        
        # recenter the cardio data if it goes off the top or the bottom of the plot
        
        # # start by setting the row number to recenter at the first
        # reCenter <- 1
        # # make a vector of reCentering events
        # reCenterEvents <- NULL
        # reCenterEventsDn <- NULL
        # reCenterEventsUp <- NULL
        
        # # recenter both high and low values
        # while(length(which(chartDF$c_Cardio1[firstEvent:lastEventEnd] >= 165 | chartDF$c_Cardio1[firstEvent:lastEventEnd] <= -165)) > 0) {
        #   # make a vector of indices that are at the top or bottom of the graphic
        #   reCenter <- min(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165))
        #   # make a vector of offset values for indices that are at the top or bottom of the graphic
        #   centerVal <- chartDF$c_Cardio1[min(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165))] - yOffset[4]
        #   # # add the reCenter event to the Up or Dn vector
        #   # if(centerVal > 0) {
        #   #   reCenterEventsDn <- c(reCenterEventsDn, reCenter)
        #   # } else reCenterEventsUp <- c(reCenterEventsUp, reCenter)
        #   # recenter from the reCenter index to the end of chart
        #   chartDF$c_Cardio1[reCenter:length(chartDF$c_Cardio1)] <- chartDF$c_Cardio1[reCenter:length(chartDF$c_Cardio1)] - centerVal
        #   chartDF$c_CardioMA[reCenter:length(chartDF$c_CardioMA)] <- chartDF$c_CardioMA[reCenter:length(chartDF$c_CardioMA)] - centerVal
        #   chartDF$c_CardioMid[reCenter:length(chartDF$c_CardioMid)] <- chartDF$c_CardioMid[reCenter:length(chartDF$c_CardioMid)] - centerVal
        #   chartDF$c_CardioSystolic[reCenter:length(chartDF$c_CardioSystolic)] <- chartDF$c_CardioSystolic[reCenter:length(chartDF$c_CardioSystolic)] - centerVal
        #   chartDF$c_CardioDiastolic[reCenter:length(chartDF$c_CardioDiastolic)] <- chartDF$c_CardioDiastolic[reCenter:length(chartDF$c_CardioDiastolic)] - centerVal
        #   reCenterEvents <- c(reCenterEvents, reCenter)
        # # } # end while loop


      
        # # cardioReCenterUp <- function(x=chartDF$c_Cardio1) {
        #   # function to recenter the cardio data if it goes off the bottom of the plot
        #   while(length(which(x <= -165))>0) {
        #     # get the last stimulus event before the data go off the bottom of the plot
        #     reCenter <- max(eventRows[(eventRows <= min(which(x <= -165)))])
        #     # relocate the index to recenter
        #     reCenter <- reCenter - 120
        #     if(reCenter<1) reCenter <- 1
        #     # get the offset value
        #     # centerVal <- x[min(which(x <= -165))] - yOffset[4]
        #     centerVal <- x[reCenter] - yOffset[4]
        #     # recenter from the recenter index to the end of chart
        #     x[reCenter:length(x)] <- x[reCenter:length(x)] - centerVal
        #     # add the reCenter event to the reCenterEventsUp vector
        #     reCenterEventsUp <- c(reCenterEventsUp, reCenter)
        #     # # chec for values off the top of the chart
        #     # if(length(which(x >= 165))>0) {
        #     #   cardioReCenterDn(x=x)
        #     # } # end if
        #   }
        #   return(x)
        # } # end cardioReCenterUp() function

        # chartDF$c_Cardio1 <- cardioReCenterUp(x=chartDF$c_Cardio1)

        # # cardioReCenterDn <- function(x=chartDF$c_Cardio1) {
        #   # function to recenter the cardio data if it goes off the top of the plot
        #   while(length(which(x >= 165))>0) {
        #     # get the last stimulus event before the data go off the bottom of the plot
        #     reCenter <- max(eventRows[(eventRows <= min(which(x >= 165)))])
        #     # relocate the index to recenter
        #     reCenter <- reCenter - 120
        #     if(reCenter<1) reCenter <- 1
        #     # get the offset value
        #     # centerVal <- x[min(which(x >= 165))] - yOffset[4]
        #     centerVal <- x[reCenter] - yOffset[4]
        #     # recenter from the recenter index to the end of chart
        #     x[reCenter:length(x)] <- x[reCenter:length(x)] - centerVal
        #     # add the reCenter event to the reCenterEventsUp vector
        #     reCenterEventsDn <- c(reCenterEventsDn, reCenter)

        #     # # chec for values off the bottom of the chart
        #     # if(length(which(x <= -165))>0) {
        #     #   cardioReCenterUp(x=x)
        #     # } # end if

        #     # check to see if the data still go off the top
        #     while(length(which(x >= 165))>0) {
        #       while( (max(eventRows[(eventRows <= min(which(x >= 165)))])-120) == reCenter ) {
        #         # get the recenter2 index
        #         reCenter2 <- min(which(x >= 165))
        #         # get the recenter2 offset value
        #         centerVal2 <- x[reCenter2] - yOffset[4]
        #         # recenter the data again
        #         x[reCenter2:nrow(chartDF)] <- x[reCenter2:nrow(chartDF)] - centerVal2
        #         reCenterEventsDn = c(reCenterEventsDn, reCenter2)
        #       }
        #     }
        # 
        #   }
        #   return(x)
        # } # end cardioReCenterDn() function

        # chartDF$c_Cardio1 <- cardioReCenterDn(x=chartDF$c_Cardio1)
        
        
      
        # while(length(which(chartDF$c_Cardio1 >= 165))>0) {
        #   # get the data row for the last stimulus onset prior to the data going off the chart
        #   reCenter <- max(eventRows[(eventRows <= min(which(chartDF$c_Cardio1 >= 165)))])
        #   # calculate the centering value to put the stim onset at the baseline
        #   centerVal <- chartDF$c_Cardio1[min(which(chartDF$c_Cardio1 >= 165))] - 0
        #   # offset the centering row to 4 seconds before stimulus onset
        #   reCenter <- reCenter - 120
        #   # fix centering prior to the first row
        #   if(reCenter<1) reCenter <- 1
        #   # and center the data from the centering row to end of chart
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] - centerVal
        #   # add the recenter event to the reCenterEvents vector for plotting
        #   reCenterEventsDn = c(reCenterEventsDn, reCenter)
        # }
        
        # while(length(which(chartDF$c_Cardio1 <= -175))>0) {
        #   reCenter <- max(eventRows[(eventRows <= min(which(chartDF$c_Cardio1 < -175)))])
        #   centerVal <- -45 - chartDF$c_Cardio1[reCenter]
        #   reCenter <- reCenter - 120
        #   if(reCenter<1) reCenter <- 1
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
        #   reCenterEventsUp = c(reCenterEventsUp, reCenter)
        # }

        
        
        # another while loop to recenter when data go off the bottom of chart
        # while(length(which(chartDF$c_Cardio1 <= -165))>0) {
        #   reCenter <- min(which(chartDF$c_Cardio1 < -165))
        #   centerVal <- 0 - chartDF$c_Cardio1[reCenter]
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
        #   reCenterEventsUp = c(reCenterEventsUp, reCenter)
        # }

        # and another while loop to recenter when data go off the top
        # while(length(which(chartDF$c_Cardio1 >= 165))>0) {
        #   reCenter <- min(which(chartDF$c_Cardio1 > 165))
        #   centerVal <- 0 - chartDF$c_Cardio1[reCenter]
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
        #   reCenterEventsDn = c(reCenterEventsDn, reCenter)
        # }
          
        #### end of auto recenter cardio 
        
        
        
        
        
       
        #############################################################
        
        # save the data frame for analysis 
        # assign("chartDF", chartDF, pos=1)
        
        ###########################################################################
        ###########################################################################
        
        # make the plot
        g <- ggplot()
        # ggplot normally executes in the global environment
        
        ############ tracing baselines ##################
        
        # remove the PLE baseline if PLE data are missing
        if(inclPLE==FALSE) { 
          g <- g + geom_hline(aes(yintercept=yOffset[-5]), color="brown", size=.15)
        } else g <- g + geom_hline(aes(yintercept=yOffset), color="brown", size=.15)
        
        #### Cardio recentering Events ####
          
          # if(!is.null(reCenterEventsDn)) {
          #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsDn)), color="yellow")
          # }
          # if(!is.null(reCenterEventsUp)) {
          #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsUp)), color="green")
          # }
          # # if(!is.null(reCenterEvents)) {
          # #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEvents)), color="orange")
          # # }
        
        ############ vertical lines for stimulus events
        
        eventIndices <- which(chartDF$Events!="")
        # for (l in 1:length(eventIndices)) {
          g <- g + geom_vline(aes(xintercept=as.numeric(eventIndices)))
        # }
        
#         # another way to make the stimulus event lines 
#         onsetRow <- which(chartDF$Events=="onsetRow")[1]
#         onsetRow <- segOnsetRow
#         g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
#         # EDA latency
#         # EDALatRow <- which(chartDF$Events=="onsetRow")[1]+(EDALat*cps)
#         EDALatRow <- onsetRow+(EDALat*cps)
#         g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
#         # offset line
#         offsetRow <- which(chartDF$Events[onsetRow:nrow(chartDF)]=="offsetRow")[1] + onsetRow - 1
#         g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
#         # answer line
#         answerRow <- which(chartDF$Events[onsetRow:nrow(chartDF)]=="answerRow")[1] + onsetRow - 1
#         g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="black")
#         # end of response onset window
#         # ROWEndRow <- which(chartDF$Events[answerRow:nrow(chartDF)]=="answerRow")[1] + answerRow - 1 + (ROWEnd*cps) 
#         ROWEndRow <- answerRow + (ROWEnd*cps)
#         g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
#         # end of scoring window
#         # endRow <- which(chartDF$Events=="onsetRow")[1]+(measuredSeg*cps)
#         segEndRow <- onsetRow+(measuredSeg*cps) - 1
#         if(segEndRow > nrow(chartDF)) segEndRow <- (nrow(chartDF))
#         g <- g + geom_vline(aes(xintercept=as.numeric(segEndRow)), color="grey70")
        
        ############ shaded areas ####################
        
        stimOnset <- which(chartDF$Events=="onsetRow")
        stimOffset <- which(chartDF$Events=="offsetRow")
        answerRow <- which(chartDF$Events=="answerRow")
        
        # stimulus question shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(stimOffset),
                          ymin=-175, 
                          ymax=175, 
                          alpha=.10, 
                          fill="grey10")
        
        # scoring window shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(stimOffset), 
                          xmax=as.numeric(stimOnset+(measuredSeg*cps)), 
                          ymin=-175, 
                          ymax=175, 
                          alpha=.10, 
                          fill="blue")

        # latency shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(stimOnset+(cps*EDALat)),
                          ymin=-175,
                          ymax=175,
                          alpha=.125,
                          fill="red")
        
        # response onset window shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(answerRow+(ROWEnd*cps)),
                          ymin=-165,
                          ymax=165,
                          alpha=.10,
                          fill="blue")
        
        ############ question labels###################
        
        g <- g + annotate(geom="text", 
                          x=which(chartDF$eventLabel!=""), 
                          y=-165, 
                          label=eventNames,
                          color="black", 
                          size=4)
        
        ############ data segments ######################
        
        # upper pnuemo data
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoSm), color="blue1", size=.3) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoMid), color="blue1", size=.15) + coord_cartesian(ylim=c(-175, 175))

        # lower pneumo data
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoSm), color="blue4", size=.3) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoMid), color="blue4", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # cardio data
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Cardio1), color="red", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMA), color="red", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # EDA data
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDA), color="green4", size=.5) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAMid), color="green4", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # photoplethysmograph PLE data
        if(inclPLE==TRUE) { 
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PL), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMA), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
        } # end if for PLE
        
        # seat sensor aivity datat
        if(activityWarning=="none") {
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SE), color="grey35", size=.4) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        }
        
        # un-used sensor columns
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1), color="grey20")
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Aux02), color="grey20")
        
        
        ######################## artifacts ##############
        
        if(showArtifacts==TRUE) {
          
          ### Penumo artifacts ###
          
          ## uppper pneumo artifacts
          
          g <- g + annotate("point", x=which(chartDF$UPneumo_a=="Artifact"),
                            y=chartDF$c_UPneumoSm[which(chartDF$UPneumo_a=="Artifact")],
                            shape=4, size=3, color='black')
          # g <- g + annotate("point", x=which(chartDF$UPneumoInh_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoInh_a=="Artifact")],
          #                   shape=4, size=3)
          # g <- g + annotate("point", x=which(chartDF$UPneumoExh_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoExh_a=="Artifact")],
          #                   shape=4, size=2, color="red")
          # g <- g + annotate("point", x=which(chartDF$UPneumoMid_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoMid_a=="Artifact")],
          #                   shape=4, size=.5, color="yellow")
          
          ## lower pneumo artifacts
          
          g <- g + annotate("point", x=which(chartDF$LPneumo_a=="Artifact"),
                    y=chartDF$c_LPneumoSm[which(chartDF$LPneumo_a=="Artifact")],
                    shape=4, size=3,color='black')
          # g <- g + annotate("point", x=which(chartDF$LPneumoInh_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoInh_a=="Artifact")],
          #                   shape=4, size=2)
          # g <- g + annotate("point", x=which(chartDF$LPneumoExh_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoExh_a=="Artifact")],
          #                   shape=4, size=1, color="red")
          # g <- g + annotate("point", x=which(chartDF$LPneumoMid_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoMid_a=="Artifact")],
          #                   shape=4, size=.5, color="yellow")

          ### EDA artifacts ###
          
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="Artifact"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="Artifact")],
          #                   shape=4, size=1)
          
          ### cardio artifacts ###
          
          # pulse amplitude
          g <- g + annotate("point", x=which(chartDF$Cardio1_a=="Artifact"),
                            y=chartDF$c_CardioMid[which(chartDF$Cardio1_a=="Artifact")],
                            shape=4, size=1, color="black")
          # diastolic distance from mid
          g <- g + annotate("point", x=which(chartDF$CardioSystolic_a=="Artifact"),
                            y=chartDF$c_CardioSystolic[which(chartDF$CardioSystolic_a=="Artifact")],
                            shape=4, size=1, color="green")
          # systolic distance from mid
          g <- g + annotate("point", x=which(chartDF$CardioDiastolic_a=="Artifact"),
                            y=chartDF$c_CardioDiastolic[which(chartDF$CardioDiastolic_a=="Artifact")],
                            shape=4, size=1, color="blue")
          # MA change
          g <- g + annotate("point", x=which(chartDF$CardioMA_a=="Artifact"),
                            y=chartDF$c_CardioMA[which(chartDF$CardioMA_a=="Artifact")],
                            shape=4, size=1, color="brown")
          # g <- g + annotate("point", x=which(chartDF$CardioMid_a=="Artifact"),
          #                   y=chartDF$c_CardioMid[which(chartDF$CardioMid_a=="Artifact")],
          #                   shape=4, size=1, color="slateblue1")
          
          ### activity sensor artifacts ###
          
          if(activityWarning=="none") {
            g <- g + annotate("point", x=which(chartDF$SEMA_a=="Artifact"),
                              y=chartDF$c_SE[which(chartDF$SEMA_a=="Artifact")],
                              shape=4, size=3, color="red") 
            #         g <- g + annotate("point", x=which(chartDF$SEMax_a=="Artifact"),
            #                           y=chartDF$c_SE[which(chartDF$SEMax_a=="Artifact")],
            #                           shape=4, size=3 color="red") 
            #         g <- g + annotate("point", x=which(chartDF$SEMin_a=="Artifact"),
            #                           y=chartDF$c_SE[which(chartDF$SEMin_a=="Artifact")],
            #                           shape=4, size=3 color="red") 
            #         g <- g + annotate("point", x=which(chartDF$SE_a=="Artifact"),
            #                           y=chartDF$c_SE[which(chartDF$SE_a=="Artifact")],
            #                           shape=4, size=3, color="red") 
          }
          
        } # end if showArtifacts==TRUE
        
        
        ############    measurements     #############

        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN", "ISN")
        # excludeEvents <- c(excludeEvents, "I1", "INT", "Int", "N1", "N2", "N3", "N4", "N7", "S", "SR", "Sy", "Sa", "SA", "SAC", "O")
        # excludeEvents <- c(excludeEvents, "I1", "I2", "I3", "I4", "I7", "SY3", "SY8", "Sy3", "Sy8", "S3", "S8", "SR2", "S2")

        ### Pneumo measurement lines ###
        
        if(showMeasurements==TRUE) {
          
          # make a vector of response onset and end rows for upper adn lower pnuemo sensors
          responseOnsetPnUpper <- which(chartDF$UPneumoExtract=="responseOnsetRow")
          responseEndPnUpper <- responseOnsetPnUpper + measuredSeg*cps -1
          
          responseOnsetPnLower <- which(chartDF$LPneumoExtract=="responseOnsetRow")
          responseEndPnLower <- responseOnsetPnLower + measuredSeg*cps -1
          
          # fix condition where response end exceeds the number of rows in the chart data frame
          if(responseEndPnUpper[length(responseEndPnUpper)] > nrow(chartDF)) { 
            responseEndPnUpper[length(responseEndPnUpper)] <- nrow(chartDF) }
          if(responseEndPnLower[length(responseEndPnLower)] > nrow(chartDF)) { 
            responseEndPnLower[length(responseEndPnLower)] <- nrow(chartDF) }
            
          ### upper pneumo
          
          # make a data frame for each segment
          for (l in 1:length(responseOnsetPnUpper)) {
            DF <- as.data.frame(matrix(NA, nrow=450, ncol=2,dimnames=list(NULL,c("UPn", "Idx"))))
            # check the length of the data frame if the last segment ends prematurely
            if( (responseEndPnUpper[l] - responseOnsetPnUpper[l] + 1) < 450 ) {
              DF <- DF[1:(responseEndPnUpper[l] - responseOnsetPnUpper[l] + 1),]
            }
            DF$UPn <- chartDF$c_UPneumoSm[responseOnsetPnUpper[l]:responseEndPnUpper[l]]
            DF$Idx <- c(responseOnsetPnUpper[l]:responseEndPnUpper[l])
            assign(paste0("UPnDF", l), DF, pos=1)
          }
          
          # loop over the data frames
          # this is done using separate data frames for each segement
          # because ggplot has lazy evaluation and will evaluate only the last x indices
          UPneumoDFs <- ls(pattern="UPnDF", pos=1)
          for (m in 1:length(UPneumoDFs)) {
            g <- g + geom_line(data=get(UPneumoDFs[m], pos=1), aes(x=Idx, y=UPn), color="blue1", size=.8)
          }
          
          ### lower pneumo
          
          for (l in 1:length(responseOnsetPnLower)) {
            DF <- as.data.frame(matrix(NA, nrow=450, ncol=2,dimnames=list(NULL,c("LPn", "Idx"))))
            # check the length of the data frame if it ends abruptly
            if( (responseEndPnLower[l] - responseOnsetPnLower[l] + 1) < 450 ) {
              DF <- DF[1:(responseEndPnLower[l] - responseOnsetPnLower[l] + 1),]
            }
            DF$LPn <- chartDF$c_LPneumoSm[responseOnsetPnLower[l]:responseEndPnLower[l]]
            DF$Idx <- c(responseOnsetPnLower[l]:responseEndPnLower[l])
            assign(paste0("LPnDF", l), DF, pos=1)
          }
          
          # loop over the data frames
          # this is done using separate data frames for each segement
          # because ggplot has lazy evaluation and will evaluate only the last x indices
          LPneumoDFs <- ls(pattern="LPnDF", pos=1)
          for (m in 1:length(LPneumoDFs)) {
            g <- g + geom_line(data=get(LPneumoDFs[m], pos=1), aes(x=Idx, y=LPn), color="blue4", size=.8)
          }
          
          ### clean up
          rm(list=ls(pattern="UPnDF"))
          rm(list=ls(pattern="LPnDF"))
          
        } # end if showMeasurements for Pneumo
          
        ### EDA measurement lines ###

        if(showMeasurements==TRUE) {
          
          responseOnsetXEDA <- which(chartDF$AutoEDAExtract == "responseOnsetRow")
          responseOnsetYEDA <- chartDF$c_AutoEDA[responseOnsetXEDA]
          responseEndXEDA <- which(chartDF$AutoEDAExtract == "responseEndRow")
          responseEndYEDA <- chartDF$c_AutoEDA[responseEndXEDA]
          # horizontal line begins at the end and points to the onset
          g <- g + annotate("segment",
                            x=responseEndXEDA,
                            xend=responseOnsetXEDA,
                            y=responseOnsetYEDA, 
                            yend=responseOnsetYEDA,
                            color="purple",
                            size=.4,
                            arrow=arrow(length=unit(0.2, "cm")))
          # vertical line
          g <- g + annotate("segment",
                            x=responseEndXEDA,
                            xend=responseEndXEDA,
                            y=responseOnsetYEDA, 
                            yend=responseEndYEDA,
                            color="purple",
                            size=.4,
                            arrow=arrow(length=unit(0.2, "cm")))
          
        } # end if showMeasurements for EDA
        
          ### Cardio measurement Lines ###

        if(showMeasurements==TRUE) {
          
          responseOnsetXCardio <- which(chartDF$CardioExtract == "responseOnsetRow")
          responseEndXCardio <- which(chartDF$CardioExtract == "responseEndRow")
          
          useCardio <- switch(cardioLine,
                              "ma" = chartDF$c_CardioMA,
                              "diastolic" = chartDF$c_CardioDiastolic,
                              "systolic" = chartDF$c_CardioSystolic,
                              "mid" = chartDF$c_CardioMid,
                              "otherwise: last")
          
          responseOnsetYCardio <- useCardio[responseOnsetXCardio]
          responseEndYCardio <- useCardio[responseEndXCardio]
          
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseOnsetXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseOnsetYCardio,
                            color="blue",
                            size=.4,
                            arrow=arrow(length=unit(0.2, "cm")))
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseEndXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseEndYCardio,
                            color="blue",
                            size=.4,
                            arrow=arrow(length=unit(0.2, "cm")))
          
        } # end if showMeasurements for Cardio
                    
          ### PLE measurment ###

              if(inclPLE==TRUE) {
                
                if(showMeasurements==TRUE) {
                  
                  # check to see if the there is any PLE extraction
                  if(!is.na(which(chartDF$PLEExtract=="prestimSegOnset")[1])) {
                    
                    ### prestim measurements
                    preOnX <- which(chartDF$PLEExtract=="prestimSegOnset")
                    preOffX <- which(chartDF$PLEExtract=="prestimSegOffset")
                    
                    # preOnY <- mean(chartDF$c_PLMax[preOnX:preOffX])
                    # preOffY <- mean(chartDF$c_PLMin[preOnX:preOffX])
                    # different method using the PLEMeans
                    # preOnY <- chartDF$c_PLMA[(preOnX+45)] + ((as.numeric(chartDF$PLEMeans[preOnX]) * PLScale) / 2)
                    # preOffY <- chartDF$c_PLMA[(preOnX+45)] - ((as.numeric(chartDF$PLEMeans[preOffX]) * PLScale) / 2)
                    
                    # make a data frame for each prestim segment
                      prePLE <- rep(NA, times=4)
                      names(prePLE) <- c("preOnX", "preOffX", "preOnY", "preOffY")
                    for(l in 1:length(preOnX)) {
                      # DF <- as.data.frame(matrix(NA, nrow=length(preOnX),ncol=4,dimnames=list(NULL,c("preOnX","preOffX","preOnY","preOffY"))))
                      prePLE['preOnX'] <- preOnX[l]
                      prePLE['preOffX'] <- preOffX[l]
                      prePLE['preOnY'] <- mean(chartDF$c_PLMax[preOnX[l]:preOffX[l]])
                      prePLE['preOffY'] <- mean(chartDF$c_PLMin[preOnX[l]:preOffX[l]])
                      # other method is centered around the PLMA whichis not centered in the PLE plot
                      # prePLE['preOnY'] <- chartDF$c_PLMA[(preOnX[l]+45)] + (as.numeric(chartDF$PLEMeans[preOnX[l]]) / 2)
                      # prePLE['preOffY'] <- chartDF$c_PLMA[(preOnX[l]+45)] - (as.numeric(chartDF$PLEMeans[preOnX[l]]) / 2)
                      #
                      # DF$preOnX[l] <- preOnX[l]
                      # DF$preOffX[l] <- preOffX[l]
                      # DF$preOnY[l] <- mean(chartDF$c_PLMax[preOnX[l]:preOffX[l]])
                      # DF$preOffY[l] <- mean(chartDF$c_PLMin[preOnX[l]:preOffX[l]])
                      # other method
                      # DF$preOnY <- chartDF$c_PLMA[(preOnX[l]+45)] + ((as.numeric(chartDF$PLEMeans[preOnX[l]]) * PLScale) / 2)
                      # DF$preOffY <- chartDF$c_PLMA[(preOnX[l]+45)] - ((as.numeric(chartDF$PLEMeans[preOnX[l]]) * PLScale) / 2)
                      # assign(paste0("prePLEDF", l), DF, pos=1)
                      assign(paste0("prePLEVector", l), prePLE, pos=1)
                    }
                    
                    # loop over the data frames for the prestim segments
                    # this is done using separate data frames for each segement
                    # because ggplot has lazy evaluation and will evaluate only the last x indices
                    prePLEs <- ls(pattern="prePLEVector", pos=1)
                    for (m in 1:length(prePLEs)) {
                      preVector <- get(paste0("prePLEVector", m), pos=1)
                      g <- g + annotate("rect",
                                        xmin=preVector['preOnX'],
                                        xmax=preVector['preOffX'],
                                        ymin=preVector['preOnY'],
                                        ymax=preVector['preOffY'],
                                        color="black",
                                        size=.3,
                                        alpha=.2,
                                        fill="green")
                    }
                    
                    # ### poststim measurements
                    postOnX <- which(chartDF$PLEExtract=="poststimSegOnset")
                    postOffX <- which(chartDF$PLEExtract=="poststimSegOffset")
                    
                    # postOnY <- mean(chartDF$c_PLMax[postOnX:postOffX])
                    # postOffY <- mean(chartDF$c_PLMin[postOnX:postOffX])
                    # different method using the PLEMeans
                    # postOnY <- chartDF$c_PLMA[(postOnX+45)] + ((as.numeric(chartDF$PLEMeans[postOnX]) * PLScale) / 2)
                    # postOffY <- chartDF$c_PLMA[(postOnX+45)] - ((as.numeric(chartDF$PLEMeans[postOffX])* PLScale) / 2)
                    
                    # make a data frame for each poststim segment
                    postPLE <- rep(NA, times=4)
                    names(postPLE) <- c("postOnX", "postOffX", "postOnY", "postOffY")
                    for(l in 1:length(postOnX)) {
                      # DF <- as.data.frame(matrix(NA, nrow=length(postOnX),ncol=4,dimnames=list(NULL,c("postOnX","postOffX","postOnY","postOffY"))))
                      postPLE['postOnX'] <- postOnX[l]
                      postPLE['postOffX'] <- postOffX[l]
                      postPLE['postOnY'] <- mean(chartDF$c_PLMax[postOnX[l]:postOffX[l]])
                      postPLE['postOffY'] <- mean(chartDF$c_PLMin[postOnX[l]:postOffX[l]])
                      
                      # other method is centered around the PLMA whichis not centered in the PLE plot
                      # postPLE['postOnY'] <- chartDF$c_PLMA[(postOnX[l]+45)] + (as.numeric(chartDF$PLEMeans[postOnX[l]]) / 2)
                      # postPLE['postOffY'] <- chartDF$c_PLMA[(postOnX[l]+45)] - (as.numeric(chartDF$PLEMeans[postOnX[l]]) / 2)
                      
                      # DF$postOnX[l] <- postOnX[l]
                      # DF$postOffX[l] <- postOffX[l]
                      # DF$postOnY[l] <- mean(chartDF$c_PLMax[postOnX[l]:postOffX[l]])
                      # DF$postOffY[l] <- mean(chartDF$c_PLMin[postOnX[l]:postOffX[l]])
                      # other method
                      # DF$postOnY <- chartDF$c_PLMA[(postOnX[l]+45)] + ((as.numeric(chartDF$PLEMeans[postOnX[l]]) * PLScale) / 2)
                      # DF$postOffY <- chartDF$c_PLMA[(postOnX[l]+45)] - ((as.numeric(chartDF$PLEMeans[postOnX[l]]) * PLScale) / 2)
                      # assign(paste0("postPLEDF", l), DF, pos=1)
                      
                      assign(paste0("postPLEVector", l), postPLE, pos=1)
                    }
                    
                    # loop over the data frames for the poststim segments
                    # this is done using separate data frames for each segement
                    # because ggplot has lazy evaluation and will evaluate only the last x indices
                    postPLEs <- ls(pattern="postPLEVector", pos=1)
                    for (m in 1:length(postPLEs)) {
                      postVector <- get(paste0("postPLEVector", m), pos=1)
                      g <- g + annotate("rect",
                                        xmin=postVector['postOnX'],
                                        xmax=postVector['postOffX'],
                                        ymin=postVector['postOnY'],
                                        ymax=postVector['postOffY'],
                                        color="black",
                                        size=.3,
                                        alpha=.2,
                                        fill="green")
                    }
                    
                    # clean up 
                    rm(list=ls(pattern="prePLE"))
                    rm(list=ls(pattern="postPLE"))
                    
                  } # end if !is.na
                  
                } # end if showMeasurements for PLE

              } # end if inclPLE
        
        ############ warnings ###########################
        
        if(XWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=-112, 
                            label=XWarning,
                            color="black", 
                            fontface="bold",
                            hjust=0,
                            size=2.5)
        }
        
        # if(cardioWarning != "none") {
        #   g <- g + annotate(geom="text", 
        #                     x=60, 
        #                     y=(yOffset[4]+reOffsetCardio+newCardio1Offset), 
        #                     label=cardioWarning,
        #                     color="black", 
        #                     fontface="bold",
        #                     hjust=0,
        #                     size=2.5)
        # }
        
        if(cardioRateWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=-100, 
                            label=cardioRateWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=2.5)
        }
        
        if(activityWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yOffset[6], 
                            label=activityWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=2.5)
        }
        
        # if(pneumoWarning != "none") {
        #   g <- g + annotate(geom="text", 
        #                     x=60, 
        #                     y=median(c((yOffset[1]+newPneumoOffset),
        #                                (yOffset[2]+newPneumoOffset))), 
        #                     label=pneumoWarning,
        #                     color="black", 
        #                     fontface="bold",
        #                     hjust=0,
        #                     size=3.5)
        # }
        
        if(pneumoRateWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yOffset[2] - 15, 
                            label=pneumoRateWarning,
                            color="black", 
                            fontface="bold",
                            hjust=0,
                            size=2.5)
        }
      
        ######################### plot appearance #########################
        
        g <- g + ylab("y-change")
        g <- g + xlab("x-time")
        g <- g + ggtitle(plotTitle)
        g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(chartDF),300))
        
        # print the chart to the graphics device
        print(g)
        
        if(printPlot==TRUE) {
          if(separateCharts==TRUE){
            graphics.off()
            dev.new() 
          }
        }
        
        #######################
        
    } # end iteration over k charts
    
  } # end iteration over j series 
  
  if(printPlot == TRUE) { 
    graphics.off()
    dev.new() } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

# reset the NCCA ASCII init 
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)

