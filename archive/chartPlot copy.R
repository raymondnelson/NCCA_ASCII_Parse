# plot an entire polygraph chart in one graphic
# raymond nelson
# raymond.nelson@gmail.com
#
# sampling rate is 30cps
# 
#
# ggplot runs in the global environment and cannot run in a function
#
#####################################################################


library(stringr)


library(ggplot2)
library(grid)


# source the list of excluded events so measurements are not plotted for these
# source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R')

# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=TRUE)

source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R')
source('~/Dropbox/R/NCCA_ASCII_Parse/rbpfProb.R')
source('~/Dropbox/R/NCCA_ASCII_Parse/dataCheck.R')
source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoCheck.R', echo=FALSE)

# source('~/Dropbox/R/NCCA_ASCII_Parse/EDACheck.R', echo=FALSE)


# run the init script to set the plot parameters
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R')
# source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot_init.R')


##################################################


# to control the print output
showNames <- TRUE
output <- FALSE
outputChartFileName <- "_chartPlot.pdf"
printPlot <- TRUE
separateCharts <- FALSE


showMeasurements <- FALSE
showEDAComplexity <- FALSE

showEDADuration <- FALSE
showArtifacts <- FALSE
showData <- FALSE
showPneumoData <- TRUE
showEDAData <- TRUE
showCardioData <- TRUE
showPLEData <- TRUE
showActivityData <- TRUE
showWarnings <- FALSE
showStimulusLines <- TRUE
showShadedAreas <- TRUE
showEventLabels <- TRUE
showScores <- FALSE

selectScores <- "auto"
# selectScores can be "auto" "miritello" "raskin", "rank" "integer" "RC" "ipZ"


showManualEDA <- FALSE


# source the init script to reset the options above
# source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot_init.R')


###########################################################


# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[11]

 
# work with a single segment 
# instead of iterating over all exams in the global environment
# use "ALL" or an integer for each of these
getSegment <- TRUE
examNum <- 1
seriesNum <- 1
chartNum <- 4
segmentNum <- "ALL"


if(getSegment == TRUE) {
  if(examNum!="ALL") uniqueExams <- uniqueExams[examNum] 
} 


##########################################################


# loop over each exam in the list and plot the charts
i=1
for(i in 1:length(uniqueExams)) {
  examName <- uniqueExams[i]
  
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Data", "*")
  # searchString <- paste0(examName, "_Data")
  
  # get the time series data
  examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  examStartRow <- 1
  examEndRow <- nrow(examDF)
  
  if(showNames==TRUE) print(paste("exam:", examName))
  
  # get the names of all unique series in the exam
  uniqueSeries <- as.character(unique(examDF$seriesName))
  
  #####################################
  
  #### insert some operations here
  graphics.off()
  dev.new()
  
  if(printPlot == TRUE) {
    if(separateCharts==FALSE) {
      pdf(paste(examName, outputChartFileName, sep=""), 
          height=5.75, 
          width=11)
    } else {
      pdf(paste(examName, chartName, outputChartFileName, sep=""), 
          height=5.75, 
          width=11)
    }
  }
  
  if(getSegment == TRUE) {
    if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum]
  }
  
  #####################################
  
  # loop over each unique series
  j=1
  for(j in 1:length(uniqueSeries)) {
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
    k=1
    for(k in 1:length(uniqueCharts)) {
      chartName <- uniqueCharts[k]
      
      # get the data frame with the time series data for each chart
      chartDF <- seriesDF[seriesDF$chartName==chartName,]
      
      chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
      chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
      
      # skip short charts less than 10 seconds
      if(nrow(chartDF)<600) next()
      
      if(showNames==TRUE) print(paste("plotting", chartName))
      
      # make a vector of event names
      eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
      
      if(getSegment == TRUE) { 
        if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
      }
      
      ##########################################################
      
      # # remove excess time series data prior to X announcement
      # XannouncementRow <- which(chartDF$eventLabel==eventNames[1])[1]
      # if(XannouncementRow > 10*cps) {
      #   chartDF <- chartDF[(XannouncementRow-(10*cps)+1):nrow(chartDF),]
      # }
      
      # # remove excess time series data after XX announcement
      # XXannouncementRow <- which(toupper(chartDF$eventLabel)==
      #                              eventNames[length(eventNames)])
      # XXannouncementRow <- XXannouncementRow[length(XXannouncementRow)]
      # if(nrow(chartDF) > XXannouncementRow+(25*cps)) {
      #   chartDF <- chartDF[1:(XXannouncementRow+(25*cps)-1),]
      # }
      
      ##########################################################
      
      # initialize a pdf graphic device for the chart if separateCharts = TRUE
      if(printPlot == TRUE) {
        if(separateCharts==TRUE) {
          pdf(paste(examName, chartName, outputChartFileName, sep=""), 
              height=5, 
              width=8)
        }
      }

      # make the plot title to include the exam and chart name
      plotTitle <- paste(examName, seriesName, chartName, sep="_")
      
      # get the first and last stimulus events
      if(length(eventNames)==0) {
        print("no stimulus events")
        # next()
        firstEvent=1
        lastEventEnd=nrow(chartDF)
      } 
        
      firstEvent <- getFirstLastEventFn(x=chartDF)[1]
      lastEventEnd <- getFirstLastEventFn(x=chartDF)[2]
 
      
      ######################################################################
      
      ################  check for additional channels  #################
      
      # set a variable to determine if the eCardio data exist
      inclECardio <- ifelse(sum(pmatch(names(chartDF), 
                                       "c_eCardio", nomatch=0))>0,
                            TRUE,
                            FALSE)
      
      # set a variable to determine if the FC finger cuff data exist
      inclFC <- ifelse(sum(pmatch(names(chartDF), 
                                  "c_FC", nomatch=0))>0,
                       TRUE,
                       FALSE)
      
      # set a variable to determine if PLE data exist in the current chart
      inclPLE <- ifelse(sum(pmatch(names(chartDF), 
                                   "c_PL", nomatch=0)) > 0,
                        TRUE,
                        FALSE)
      
      #######################################################################
      
      ################ set some warnings for the chart #################
      
      # set a warning for the X/XX announcements 
      XWarning <- ifelse( !("X" %in% eventNames) & !("XX" %in% eventNames),
                          "MISSING X AND XX ANNOUNCEMENTS",
                          ifelse(!("X" %in% eventNames),
                                 "MISSING X ANNOUNCEMENT",
                                 ifelse(!("XX" %in% eventNames),
                                        "MISSING XX ANNOUNCEMENT",
                                        "none") ) )
      
      if(XWarning == "none") {
        XWarning <- ifelse( length(which(eventNames %in% "X")) > 1 | 
                              length(which(eventNames %in% "XX")) > 1,
                          "REPEATED X/XX ANNOUNCEMENT",
                          ifelse( which(eventNames %in% "X") != 1,
                                  "X ANNOUNCEMENT - NOT AT RECORDING ONSET",
                                  ifelse( which(eventNames %in% "XX") != 
                                            length(eventNames),
                                          "XX ANNOUNCEMENT - NOT AT END",
                                          "none") ) )
      } 
      
      # set a variable for activity sensor warning
      activityWarning <- ifelse( !("c_SE" %in% names(examDF)),
                                 "MISSING ACTIVITY SENSOR DATA",
                                 "none" )
      
      ######################################
      
      # check the cardio pulse rate
      # was buffer=3 4-14-2017
      cardioRate <- ratePerMin(chartDF$c_Cardio1,
                               buffer=12,
                               peaks="upper",
                               lowPass=TRUE)
      if(cardioRate < 60 | cardioRate > 100) {
        cardioRateWarning <- paste("CARDIO RATE", 
                                   cardioRate, 
                                   "OUTSIDE NORMAL RANGE")
      } else cardioRateWarning <- paste("CARDIO RATE:", cardioRate)
      
      # recalculate the cardio to check for arrhythmia
      cardioRate1 <- ratePerMin(chartDF$c_Cardio1,
                                buffer=3,
                                peaks="upper",
                                lowPass=TRUE)
      cardioRate2 <- ratePerMin(chartDF$c_Cardio1,
                                buffer=3,
                                peaks="lower",
                                lowPass=TRUE)
      
      # compare the systolic and diastolic cardio rates 
      # and set the arrhythmia warning if necessary
      if(exp(-abs(log(cardioRate1/cardioRate2))) <= .95) {
        cardioRateWarning <- paste0(cardioRateWarning, 
                                    ", possible cardio arrhythmia")
      } 
      
      # check the respiration rate
      pneumoRate  <- ratePerMin(chartDF$c_UPneumoSm)
      if(pneumoRate < 10 | pneumoRate > 22) {
        pneumoRateWarning <- paste("RESPIRATION RATE", 
                                   pneumoRate, 
                                   "OUTSIDE NORMAL RANGE")
      } else pneumoRateWarning <- paste("RESPIRATION RATE", pneumoRate)
      
      # check for respiratory blood pressure fluctuation
      rbpfMsg <- rbpfProbFn(x=chartDF)
      if(rbpfMsg != "none") {
        rbpfWarning <- rbpfMsg
      } else rbpfWarning <- "none" # rbpfMsg
      
      # check for unresponsive EDA
      edaWarning <- "none"
      dataWarning <- "none"
      dataCheckFn(x=chartDF$c_AutoEDA, 
                  sec=5, 
                  times=30, 
                  omit=10, 
                  firstRow=NULL, 
                  lastRow=NULL, 
                  sVal=200,
                  columnName="AutoEDAUnresponse_a",
                  output="message")
      if(dataWarning != "none") edaWarning <- "unresponsive EDA"
      
      # check for unresponsive pneumo data
      pneumoDataWarning <- "none"
      # pneumoDataWarning <- pneumoCheckFn(x1=chartDF$c_UPneumoSm, 
      #                                    x2=chartDF$c_LPneumoSm, 
      #                                    sec=5, 
      #                                    times=30, 
      #                                    omit=10, 
      #                                    firstRow=NULL, 
      #                                    lastRow=NULL)
      
      #############################################################
      
      # scaling and offsetting is performed by the scaleOffsetData.R script
      
      ######################################################################
      ######################################################################
      ######################################################################
      
      ############    make the plot   ################
      
      g <- ggplot()
      
      # ggplot normally executes in the global environment
      # this means that ggplot is not easily called from within a function 
      
      ############ plot tracing baselines ##################
      
      # remove the PLE baseline if PLE data are missing
      # if(inclPLE==FALSE) { 
      #   g <- g + geom_hline(aes(yintercept=yOffset[-5]), color="brown", size=.15)
      # } else g <- g + geom_hline(aes(yintercept=yOffset), color="brown", size=.15)
      
      #### Cardio recentering Events ####
      
      # not used 9-2016
      # if(!is.null(reCenterEventsDn)) {
      #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsDn)), color="yellow")
      # }
      # if(!is.null(reCenterEventsUp)) {
      #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsUp)), color="green")
      # }
      # # if(!is.null(reCenterEvents)) {
      # #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEvents)), color="orange")
      # # }

      ######################################################################
      ################  stimulus lines and shaded areas   ##################
            
      if(length(eventNames) != 0) {
        
        if(showStimulusLines==TRUE) {
          
          ############### plot vertical lines for stimulus events  ##############
          
          eventIndices <- which(chartDF$Events != "")
          # no need for a loop because geom_vline is vectorized
          # g <- g + geom_vline(aes(xintercept=as.numeric(eventIndices)))
          
          # another way to make the stimulus event lines one by one
          onsetRow <- which(chartDF$Events=="onsetRow")[]
          # onsetRow <- segOnsetRow
          g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
          # EDA latency
          # EDALatRow <- which(chartDF$Events=="onsetRow")[1]+(EDALat*cps)
          EDALatRow <- onsetRow+(EDALat*cps)
          g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="red") # grey80
          # offset line
          offsetRow <- which(chartDF$Events[onsetRow:nrow(chartDF)]=="offsetRow")[1] + segOnsetRow - 1
          g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
          # answer line
          answerRow <- which(chartDF$Events[onsetRow:nrow(chartDF)]=="answerRow")[1] + segOnsetRow - 1
          g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="green") # black
          # end of response onset window
          # ROWEndRow <- which(chartDF$Events[answerRow:nrow(chartDF)]=="answerRow")[1] + answerRow - 1 + (ROWEnd*cps)
          ROWEndRow <- answerRow + (ROWEnd*cps)
          g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
          # end of scoring window
          # endRow <- which(chartDF$Events=="onsetRow")[1]+(measuredSeg*cps)
          segEndRow <- segOnsetRow+(measuredSeg*cps) - 1
          if(segEndRow > nrow(chartDF)) segEndRow <- (nrow(chartDF))
          g <- g + geom_vline(aes(xintercept=as.numeric(segEndRow)), color="blue") # grey70
          
        } # end if showStimulusLines==TRUE
        
        ###################### plot shaded areas #####################
        
        if(showShadedAreas==TRUE) {
          
          stimOnset <- which(chartDF$Events=="onsetRow")
          stimOffset <- which(chartDF$Events=="offsetRow")
          answerRow <- which(chartDF$Events=="answerRow")
          
          # stimulus question shaded area
          g <- g + annotate("rect", 
                            xmin=as.numeric(stimOnset),
                            xmax=as.numeric(stimOffset),
                            ymin=yMin, 
                            ymax=yMax, 
                            alpha=.10, 
                            fill="grey10")
          
          # scoring window shaded area
          g <- g + annotate("rect", 
                            xmin=as.numeric(stimOffset), 
                            xmax=as.numeric(stimOnset+measuredSeg*cps), 
                            ymin=yMin, 
                            ymax=yMax, 
                            alpha=.10, 
                            fill="blue")
          
          # latency shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(stimOnset),
                            xmax=as.numeric(stimOnset+EDALat*cps),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.125,
                            fill="red")
          
          # response onset window shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(stimOnset),
                            xmax=as.numeric(answerRow+ROWEnd*cps),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.10,
                            fill="blue")
          
        } # end if showShadedAreas==TRUE
        
        ################### print question labels #####################
        
        if(showEventLabels==TRUE) {
          
          g <- g + annotate(geom="text", 
                            x=which(chartDF$eventLabel!=""), 
                            y=yMin, 
                            label=eventNames,
                            color="black", 
                            size=4)
          
        } # end if showEventLabels==TRUE
        
      } # end if length(eventNames) != 0
      
      ##############################################################################
      ###########################   plot the artifacts   ###########################
      
      if(showArtifacts==TRUE) {
        
        ######## Penumo artifacts ######## 
        
        if(isTRUE(showPneumoData)) {
          
          #### uppper pneumo artifacts
          
          # g <- g + annotate("point", x=which(chartDF$Pneumo_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$Pneumo_a=="Artifact")],
          #                   shape=4, size=3, color='black')
          # g <- g + annotate("point", x=which(chartDF$UPneumoInh_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoInh_a=="Artifact")],
          #                   shape=4, size=3, color="black")
          # g <- g + annotate("point", x=which(chartDF$UPneumoExh_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoExh_a=="Artifact")],
          #                   shape=4, size=2.5, color="red")
          # g <- g + annotate("point", x=which(chartDF$UPneumoMid_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoMid_a=="Artifact")],
          #                   shape=4, size=2, color="brown")
          
          #### lower pneumo artifacts
          
          # g <- g + annotate("point", x=which(chartDF$Pneumo_a=="Artifact"),
          #           y=chartDF$c_LPneumoSm[which(chartDF$Pneumo_a=="Artifact")],
          #           shape=4, size=3,color='black')
          # g <- g + annotate("point", x=which(chartDF$LPneumoInh_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoInh_a=="Artifact")],
          #                   shape=4, size=3, color="black")
          # g <- g + annotate("point", x=which(chartDF$LPneumoExh_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoExh_a=="Artifact")],
          #                   shape=4, size=2.5, color="red")
          # g <- g + annotate("point", x=which(chartDF$LPneumoMid_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoMid_a=="Artifact")],
          #                   shape=4, size=2, color="brown")
          
          #### upper pneumo answer buffer
          
          # make a vector of answer indices
          answerRows <- which(chartDF$Events=="answerRow")
          # exclude answer indices that are adjacent to the offsetRow (no verbal answer)
          answerRows <- answerRows[-which(chartDF$Events[answerRows - 1] != "")]
          
          ## skip the answer buffer if there are no answerRows
          
          if(length(answerRows) != 0) {
            
            # initialize the answer buffer indices
            aBuffXOn <- answerRows - 2*cps
            aBuffXOff <- answerRows + 2*cps
            
            # correct for potential problems
            aBuffXOn[aBuffXOn < 1] <- 1
            aBuffXOff[aBuffXOff > nrow(chartDF)] <- nrow(chartDF)
            
            # initialize a vector
            aBuffDF <- rep(NA, times=4)
            names(aBuffDF) <- c("aBuffOnX", "aBuffOffX", "aBuffOnY", "aBuffOffY")
            
            # make a separate vector for each verbal answer and assign it to the global environment
            for(l in 1:length(aBuffXOn)) {
              aBuffDF['aBuffOnX'] <- aBuffXOn[l]
              aBuffDF['aBuffOffX'] <- aBuffXOff[l]
              aBuffDF['aBuffOnY'] <- chartDF$c_UPneumoSm[aBuffXOn[l]]
              aBuffDF['aBuffOffY'] <- chartDF$c_UPneumoSm[aBuffXOff[l]]
              assign(paste0("aBuffVector", l), aBuffDF, pos=1)
            }
            
            # loop over the aBuffUDF data frames for the answer buffer segments
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            aBuffers <- ls(pattern="aBuffVector", pos=1)
            
            for (m in 1:length(aBuffers)) {
              aBuff <- get(paste0("aBuffVector", m), pos=1)
              g <- g + annotate("segment",
                                x=aBuff['aBuffOnX'],
                                xend=aBuff['aBuffOffX'],
                                y=aBuff['aBuffOnY'],
                                yend=aBuff['aBuffOffY'],
                                color="black",
                                linetype="solid",
                                size=.8,
                                alpha=.75)
            }
            
            rm(list=aBuffers)
            rm(aBuffers)
            
            #### lower pneumo answer buffer
            
            # initialize a vector
            aBuffDF <- rep(NA, times=4)
            names(aBuffDF) <- c("aBuffOnX", "aBuffOffX", "aBuffOnY", "aBuffOffY")
            
            # make a separate vector for each verbal answer and assign it to the global environment
            for(l in 1:length(aBuffXOn)) {
              aBuffDF['aBuffOnX'] <- aBuffXOn[l]
              aBuffDF['aBuffOffX'] <- aBuffXOff[l]
              aBuffDF['aBuffOnY'] <- chartDF$c_LPneumoSm[aBuffXOn[l]]
              aBuffDF['aBuffOffY'] <- chartDF$c_LPneumoSm[aBuffXOff[l]]
              assign(paste0("aBuffVector", l), aBuffDF, pos=1)
            }
            
            # loop over the aBuffUDF data frames for the answer buffer segments
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            aBuffers <- ls(pattern="aBuffVector", pos=1)
            
            for (m in 1:length(aBuffers)) {
              aBuff <- get(paste0("aBuffVector", m), pos=1)
              g <- g + annotate("segment",
                                x=aBuff['aBuffOnX'],
                                xend=aBuff['aBuffOffX'],
                                y=aBuff['aBuffOnY'],
                                yend=aBuff['aBuffOffY'],
                                color="black",
                                linetype="solid",
                                size=.8,
                                alpha=.75)
            }
            
            rm(list=aBuffers)
            rm(aBuffers)
            
          }
          
        } # end if showPneumoData
        
        ######## EDA artifacts ########
        
        if(isTRUE(showEDAData)) {
          
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="Artifact"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="Artifact")],
          #                   shape=4, size=1)
          
          
          
          # point
          # non-stimulus artfifacts
          g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact1c"),
                            y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact1c")],
                            shape=20, size=3, alpha=.1, color="hotpink")
          
          # post-stimulus artifacts
          g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact1b"),
                            y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact1b")],
                            shape=20, size=3, alpha=.1, color="orange")
          
          # prestimulus artifacts
          g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact1a"),
                            y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact1a")],
                            shape=20, size=3, alpha=.15, color="red")
          
          
          
          # point
          g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact2"),
                            y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact2")],
                            shape=18, size=3, alpha=.75, color="brown")
          
          # point
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact3"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact3")],
          #                   shape=18, size=3, alpha=.2, color="black")
          
          
          
          # vertical line
          # g <- g + annotate("segment",
          #                   x=which(chartDF$AutoEDA_a=="artifact"),
          #                   xend=which(chartDF$AutoEDA_a=="artifact"),
          #                   # y=chartDF$c_EDAMax [which(chartDF$AutoEDA_a=="artifact")],
          #                   # yend=chartDF$c_EDAFiltMax[which(chartDF$AutoEDA_a=="artifact")],
          #                   y=chartDF$c_AutoEDABase[which(chartDF$AutoEDA_a=="artifact")],
          #                   yend=chartDF$c_AutoEDAPeak[which(chartDF$AutoEDA_a=="artifact")],
          #                   color="red",
          #                   alpha=.1,
          #                   size=.1 )
          
        }
        
        
        ######## cardio artifacts ########
        
        if(isTRUE(showCardioData)) {
          
          # change in pulse amplitude
          g <- g + annotate("point", x=which(chartDF$CardioMid_a=="ArtifactMinMaxAmp"),
                            y=chartDF$c_Cardio1[which(chartDF$CardioMid_a=="ArtifactMinMaxAmp")],
                            shape=8, size=6, color="black") # was shape=4 color="slateblue1"
          # was "red1"
          # pulse amplitude
          # g <- g + annotate("point", x=which(chartDF$Cardio1_a=="Artifact"),
          #                   y=chartDF$c_CardioMid[which(chartDF$Cardio1_a=="Artifact")],
          #                   shape=4, size=5, color="black")
          # systolic distance from mid
          g <- g + annotate("point", x=which(chartDF$CardioSystolic_a=="ArtifactMaxAmp"),
                            y=chartDF$c_Cardio1[which(chartDF$CardioSystolic_a=="ArtifactMaxAmp")],
                            shape=8, size=4, color="blue")
          # diastolic distance from MA
          # g <- g + annotate("point", x=which(chartDF$CardioDiastolic_a=="ArtifactMinAmp"),
          #                   y=chartDF$c_CardioDiastolic[which(chartDF$CardioDiastolic_a=="ArtifactMinAmp")],
          #                   shape=4, size=8, color="blue") # was "blue4"
          # MA change
          g <- g + annotate("point", x=which(chartDF$CardioMA_a=="ArtifactMA"),
                            y=chartDF$c_Cardio1[which(chartDF$CardioMA_a=="ArtifactMA")],
                            shape=8, size=4, color="brown") # was "slateblue2"
          # systolic beat to beat
          # g <- g + annotate("point", x=which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB"),
          #                   y=chartDF$c_CardioSystolic[which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB")],
          #                   shape=4, size=6, color="brown") # was "green3" 
          # diastolic beat to beat
          # g <- g + annotate("point", x=which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB"),
          #                   y=chartDF$c_CardioDiastolic[which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB")],
          #                   shape=4, size=6, color="brown") # was "green2" 
          
        }
        
        
        ######## activity sensor artifacts ########
        
        if(isTRUE(showActivityData)) {
          
          if(activityWarning=="none") {
            
            # max peak amplitude
            # g <- g + annotate("point", x=which(chartDF$SEMax_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SEMax_a!=0)],
            #                   shape=4, size=1, color="red") # shape 3 is a +
            # max peak rate
            # g <- g + annotate("point", x=which(chartDF$SEMax_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SEMax_a!=0)],
            #                   shape=3, size=1, color="brown") # shape 4 is an X
            # min peak amplitude
            # g <- g + annotate("point", x=which(chartDF$SEMin_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SEMin_a!=0)],
            #                   shape=4, size=1, color="red")
            # min peak rate
            # g <- g + annotate("point", x=which(chartDF$SEMin_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SEMin_a!=0)],
            #                   shape=3, size=1, color="brown")
            # min max amplitude
            # g <- g + annotate("point", x=which(chartDF$SE_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SE_a=="ArtifactMinMaxAmp")],
            #                   shape=4, size=1, color="red") # shape 2 is a triangle
            # activity sensor instability
            # g <- g + annotate("point", x=which(chartDF$SEMA_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SEMA_a!=0)],
            #                   shape=6, size=1, color="red") # shape 6 is an upsidedown triangle
            # processed activity data
            # g <- g + annotate("point", x=which(chartDF$SE_a!=0),
            #                   y=chartDF$c_SEProc[which(chartDF$SE_a!=0)],
            #                   shape=2, size=1, color="red")
            
            # 1-2-2017
            # auto-marked artifacts
            
            g <- g + annotate("point", x=which(chartDF$SE_a!=""),
                              y=chartDF$c_SEProc[which(chartDF$SE_a!="")],
                              shape=20, size=3, alpha=.10, color="red")
            
            
          } # end if for activity sensor warning
          
        }
        
      } # end if showArtifacts==TRUE
      
      ##########################################################################
      #########################  plot the data segments   ######################
      
      if(showData == TRUE) {
        if(isTRUE(showPneumoData)) {
          # upper pneumo data
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoMid), color="blue2", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoSm), color="blue3", size=.4) + coord_cartesian(ylim=c(yMin, yMax))
        
          # lower pneumo data
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoMid), color="blue4", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoSm), color="blue4", size=.4) + coord_cartesian(ylim=c(yMin, yMax))
        }
        
        if(isTRUE(showEDAData)) {
          # filtered EDA data for data quality, stability and artifact analysis
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFiltMax), color="grey60", size=.15, alpha=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFiltMin), color="grey60", size=.15, alpha=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFiltMid), color="black", size=.15, alpha=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFilt), color="green4", size=.5, alpha=.15) + coord_cartesian(ylim=c(yMin, yMax))
        }
        
        if(isTRUE(showCardioData)) {
          # cardio data
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMA), color="red", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMid), color="black", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Cardio1), color="red", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_cardioRateSystolic), color="brown", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          
          # electronic cardio data
          if(inclECardio==TRUE) { 
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioMA), color="red", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioMid), color="black", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardio), color="orange", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          } # end if for electronic cardio
          
          # finger cuff data
          if(inclFC==TRUE) { 
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FC), color="orange", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCMid), color="black", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCMA), color="red", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          } # end if for FC
        }
        
        if(isTRUE(showEDAData)) {
          # Filtered EDA data
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAMid), color="black", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDA), color="green4", size=.5) + coord_cartesian(ylim=c(yMin, yMax))
          
          if(isTRUE(showManualEDA)) {
            # Un-filtered EDA data
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAMid), color="black", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDA), color="green3", size=.5) + coord_cartesian(ylim=c(yMin, yMax))
          }
        }
        
        if(isTRUE(showPLEData)) {
          # photoplethysmograph PLE data
          if(inclPLE==TRUE) { 
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMax), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMin), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMA), color="brown", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PL), color="brown", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
          } # end if for PLE
        }
        
        if(isTRUE(showActivityData)) {
          # seat sensor activity data
          if(activityWarning=="none") {
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEProc), color="tan4", size=.35) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMin), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMax), color="grey60", size=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMA), color="black", size=.2) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SE), color="grey35", size=.25) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEProcMA), color="black", size=.5) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEResult), color="red", size=.25) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEAbstrct1), color="grey35", size=.25) + coord_cartesian(ylim=c(yMin, yMax))
          }
        }
        
      } # end if showData==TRUE
      
      ########################################################################
      ###################   plot the measurement lines     ###################
      
      if(showMeasurements==TRUE & length(eventNames)!=0) {
        
        ######### Pneumo measurement lines #########
        
        if(isTRUE(showPneumoData)) {
          
          # make a vector of response onset and end rows for upper and lower pneumo sensors
          responseOnsetPnUpper <- which(chartDF$UPneumoExtract=="responseOnsetRow")
          responseEndPnUpper <- responseOnsetPnUpper + measuredSeg*cps -1
          # fix condition where response end exceeds nrow(chartDF)
          responseEndPnUpper[which(responseEndPnUpper > nrow(chartDF))] <- nrow(chartDF)
          
          responseOnsetPnLower <- which(chartDF$LPneumoExtract=="responseOnsetRow")
          responseEndPnLower <- responseOnsetPnLower + measuredSeg*cps -1
          # fix condition where response end exceeds nrow(chartDF)
          responseEndPnLower[which(responseEndPnLower > nrow(chartDF))] <- nrow(chartDF)
          
          if(length(responseOnsetPnUpper) > 0) {
            
            # fix condition where response end exceeds the number of rows in the chart data frame
            if(responseEndPnUpper[length(responseEndPnUpper)] > nrow(chartDF)) { 
              responseEndPnUpper[length(responseEndPnUpper)] <- nrow(chartDF) }
            if(responseEndPnLower[length(responseEndPnLower)] > nrow(chartDF)) { 
              responseEndPnLower[length(responseEndPnLower)] <- nrow(chartDF) }
            
            ######## upper pneumo measurement
            
            # make a data frame for each segment
            for (l in 1:length(responseOnsetPnUpper)) {
              DF <- as.data.frame(matrix(NA, nrow=(measuredSeg*cps), ncol=2,dimnames=list(NULL,c("UPn", "Idx"))))
              # check the length of the data frame if the last segment ends prematurely
              if( (responseEndPnUpper[l] - responseOnsetPnUpper[l] + 1) < (measuredSeg*cps) ) {
                DF <- DF[1:(responseEndPnUpper[l] - responseOnsetPnUpper[l] + 1),]
              }
              DF$UPn <- chartDF$c_UPneumoSm[responseOnsetPnUpper[l]:responseEndPnUpper[l]]
              DF$Idx <- c(responseOnsetPnUpper[l]:responseEndPnUpper[l])
              assign(paste0("UPnDF", l), DF, pos=1)
            } # end for loop to make data frames for each upper pneumo segment
            
            # loop over the data frames
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            UPneumoDFs <- ls(pattern="UPnDF", pos=1)
            for (m in 1:length(UPneumoDFs)) {
              g <- g + geom_line(data=get(UPneumoDFs[m], pos=1), aes(x=Idx, y=UPn), color="blue", size=.7)
              # was "blue1"
            }
            
            ######## lower pneumo measurement
            
            for (l in 1:length(responseOnsetPnLower)) {
              DF <- as.data.frame(matrix(NA, nrow=(measuredSeg*cps), ncol=2,dimnames=list(NULL,c("LPn", "Idx"))))
              # check the length of the data frame if it ends abruptly
              if( (responseEndPnLower[l] - responseOnsetPnLower[l] + 1) < (measuredSeg*cps) ) {
                DF <- DF[1:(responseEndPnLower[l] - responseOnsetPnLower[l] + 1),]
              }
              DF$LPn <- chartDF$c_LPneumoSm[responseOnsetPnLower[l]:responseEndPnLower[l]]
              DF$Idx <- c(responseOnsetPnLower[l]:responseEndPnLower[l])
              assign(paste0("LPnDF", l), DF, pos=1)
            } # end for loop to make data frames for each lower pneumo segment
            
            # loop over the data frames
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            LPneumoDFs <- ls(pattern="LPnDF", pos=1)
            for (m in 1:length(LPneumoDFs)) {
              g <- g + geom_line(data=get(LPneumoDFs[m], pos=1), aes(x=Idx, y=LPn), color="blue", size=.7)
              # was "blue4"
            }
            
            ### clean up
            rm(list=ls(pattern="UPnDF"))
            rm(list=ls(pattern="LPnDF"))
            rm(DF)
            
          } # end if 
          
        } # end if show pneumo data
        
        ######### Auto EDA measurements #########
        
        if(isTRUE(showEDAData)) { 
          
          responseOnsetXAutoEDA <- which(chartDF$AutoEDAExtract == "responseOnsetRow")
          responseEndXAutoEDA <- which(chartDF$AutoEDAExtract == "responseEndRow")
          
          # 9-8-2016 check and fix the length of the EDA onset and end vectors if necessary
          if(length(responseOnsetXAutoEDA) != length(responseEndXAutoEDA)) { 
            # combine the lengths of the onset and end vectors
            onsetEndVectors <- c(length(responseOnsetXAutoEDA), length(responseEndXAutoEDA))
            # then calculate the difference in the length of the onset and end vectors
            diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
            ifelse (which.min(onsetEndVectors)==1,
                    responseOnsetXAutoEDA <- c(responseOnsetXAutoEDA, rep(1, diffLen)),
                    responseEndXAutoEDA <- c(responseEndXAutoEDA, rep(1, diffLen))
            ) 
            # which(responseOnsetXAutoEDA[2:length(responseOnsetXAutoEDA)] < 
            #         responseEndXAutoEDA[1:(length(responseEndXAutoEDA)-1)]) + 1
            # correct condition if any response end index exceeds the data frame length
            if(length(which(responseEndXAutoEDA > nrow(chartDF))) != 0) {
              responseOnsetXAutoEDA <- responseOnsetXAutoEDA[-which(responseEndXAutoEDA > nrow(chartDF)) > 0]
              responseEndXAutoEDA <- responseEndXAutoEDA[-which(responseEndXAutoEDA > nrow(chartDF)) > 0]
            }
          } # end if fix length
          
          # 9-5-2016
          newOnset <- NULL
          newEnd <- NULL
          # iterate backwards over the vector of response onsets
          for (l in length(responseOnsetXAutoEDA):1) {
            # for each stimulus onset get the min stimulus end that is greater
            # first silence warnings from this section when there is no end index for an onset index
            oldw <- getOption("warn")
            options(warn = -1)
            ifelse(is.na(responseEndXAutoEDA[min(which(responseEndXAutoEDA >= responseOnsetXAutoEDA[l]))]),
                   endVal <- NA,
                   endVal <- responseEndXAutoEDA[min(which(responseEndXAutoEDA >= responseOnsetXAutoEDA[l]))]
            )
            # reset the warning level
            options(warn = oldw)
            # remove the onset value from the new onset vector if there is no response end index
            if(!is.na(endVal)) { newEnd <- c(newEnd, endVal) }
            if(!is.na(endVal)) { newOnset <- c(newOnset, responseOnsetXAutoEDA[l]) }
            if(!is.na(endVal)) { responseEndXAutoEDA <- responseEndXAutoEDA[-which(responseEndXAutoEDA==endVal)] }
          } # end for loop
          # need to reverse the vectors because of iterating backwards
          responseOnsetXAutoEDA <- rev(newOnset)
          responseEndXAutoEDA <- rev(newEnd)
          
          # now get the Y values
          responseOnsetYAutoEDA <- chartDF$c_AutoEDA[responseOnsetXAutoEDA]
          responseEndYAutoEDA <- chartDF$c_AutoEDA[responseEndXAutoEDA]
          
          # horizontal line begins at the response end and points to the response onset
          g <- g + annotate("segment",
                            x=responseEndXAutoEDA,
                            xend=responseOnsetXAutoEDA,
                            y=responseOnsetYAutoEDA, 
                            yend=responseOnsetYAutoEDA,
                            color="blue",
                            size=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          # vertical line begins at response baseline and points to the peak
          g <- g + annotate("segment",
                            x=responseEndXAutoEDA,
                            xend=responseEndXAutoEDA,
                            y=responseOnsetYAutoEDA, 
                            yend=responseEndYAutoEDA,
                            # was "purple",
                            color="blue",
                            size=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          
          ###### auto EDA response duration
          
          halfRecoveryXAutoEDA <- which(chartDF$AutoEDAExtract == "halfRecoveryRow")
          # which(chartDF$AutoEDAExtract == "responseOnsetRow")
          
          # check and fix the length of halfRecoveryXAutoEDA and responseEndXAutoEDA
          if(length(responseEndXAutoEDA) != length(halfRecoveryXAutoEDA)) {
            onsetEndVectors <- c(length(responseEndXAutoEDA), length(halfRecoveryXAutoEDA))
            # then calculate the difference in the length of the onset and end vectors
            diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
            ifelse (which.min(onsetEndVectors)==1,
                    responseEndXAutoEDA <- c(responseEndXAutoEDA, rep(1, diffLen)),
                    halfRecoveryXAutoEDA <- c(halfRecoveryXAutoEDA, rep(1, diffLen))
            ) 
          }# end if fix length
          
          newEnd <- NULL
          newHalfRec <- NULL
          # for each stimulus end index get the half recovering index 
          # that is greater than the stimulus end index
          l=length(responseEndXAutoEDA)
          for (l in length(responseEndXAutoEDA):1) {
            # iterate backwards over the vector of response end indices
            # first silence warnings from this section when there is no end index for an onset index
            oldw <- getOption("warn")
            options(warn = -1)
            # set the halfRec value
            ifelse(is.na(halfRecoveryXAutoEDA[min(which(halfRecoveryXAutoEDA >= responseEndXAutoEDA[l]))]),
                   halfRec <- NA,
                   halfRec <- halfRecoveryXAutoEDA[min(which(halfRecoveryXAutoEDA >= responseEndXAutoEDA[l]))]
            )
            # reset the warning level
            options(warn = oldw)
            # remove the onset value from the new onset vector if there is no response end index
            if(!is.na(halfRec)) { 
              newHalfRec <- c(newHalfRec, halfRec) 
              newEnd <- c(newEnd, responseEndXAutoEDA[l]) 
              # remove the halfRec value from the halfRecoveryXAutoEDA vector before the next iteration
              halfRecoveryXAutoEDA <- halfRecoveryXAutoEDA[-which(halfRecoveryXAutoEDA==halfRec)] 
            }
          } # end for loop
          # need to reverse the vectors because of iterating backwards
          responseEndXAutoEDA <- rev(newEnd)
          halfRecoveryXAutoEDA <- rev(newHalfRec)
          
          # then get the Y values
          responseEndYAutoEDA <- chartDF$c_AutoEDA[responseEndXAutoEDA]
          halfRecoveryYAutoEDA <- chartDF$c_AutoEDA[halfRecoveryXAutoEDA]
          
          # now add the Auto EDA duration to the plot
          
          if(isTRUE(showEDADuration)) {
            # horizontal line begins at the response end and points to the response onset
            g <- g + annotate("segment",
                              x=responseEndXAutoEDA,
                              xend=halfRecoveryXAutoEDA,
                              y=responseOnsetYAutoEDA, 
                              yend=responseOnsetYAutoEDA,
                              color="blue",
                              size=1,
                              alpha=.3,
                              arrow=arrow(length=unit(0.2, "cm")))
            # vertical line begins at response baseline and points to the peak
            g <- g + annotate("segment",
                              x=halfRecoveryXAutoEDA,
                              xend=halfRecoveryXAutoEDA,
                              y=responseOnsetYAutoEDA, 
                              yend=halfRecoveryYAutoEDA,
                              # was "purple",
                              color="black",
                              size=.4)
            
          } # end if istrue showEDADuration
          
          ###### auto EDA response complexity
          
          complexityXAutoEDA <- which(chartDF$AutoEDAExtract == "complexityRow")
          complexityYAutoEDA <- chartDF$c_AutoEDA[complexityXAutoEDA]
          
          complexityXOnsetAutoEDA <- NULL
          if(length(complexityXAutoEDA) > 0) {
            # initialize a vector
            # find the largest xOnset index preceeding each value in the complexityXAutoEDA vector
            # interate over the complexityXAutoEDA vector
            l=1
            for(i in 1:length(complexityXAutoEDA)) {
              thisIndex <- complexityXAutoEDA[l]
              keepIdx <- max(responseOnsetXAutoEDA[responseOnsetXAutoEDA < thisIndex])
              complexityXOnsetAutoEDA <- c(complexityXOnsetAutoEDA, keepIdx)
            } # end for
          } # end if
          
          # get the Y values for the response onset for complex responses
          complexityYOnsetAutoEDA <- chartDF$c_AutoEDA[complexityXOnsetAutoEDA]
          
          # the add Auto EDA complexity to the plot
          
          if(isTRUE(showEDAComplexity) && length(complexityXOnsetAutoEDA) > 0) {
            # vertical segment
            g <- g + annotate("segment",
                              x=complexityXAutoEDA,
                              xend=complexityXAutoEDA,
                              y=complexityYOnsetAutoEDA, 
                              yend=complexityYAutoEDA,
                              # was "purple",
                              color="blue",
                              alpha=.4,
                              # arrow=arrow(length=unit(0.2, "cm"))
                              size=.6)
            
          } # end if show complexity auto EDA
          
          
          ########### Manual EDA measurements ###########
          
          if(isTRUE(showManualEDA)) {
            
            responseOnsetXManualEDA <- which(chartDF$ManualEDAExtract == "responseOnsetRow")
            responseEndXManualEDA <- which(chartDF$ManualEDAExtract == "responseEndRow")
            
            # 9-8-2016 check and fix the length of the EDA onset and end vectors if necessary
            if(length(responseOnsetXManualEDA) != length(responseEndXManualEDA)) { 
              onsetEndVectors <- c(length(responseOnsetXManualEDA), length(responseEndXManualEDA))
              diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
              ifelse (which.min(onsetEndVectors)==1,
                      responseOnsetXManualEDA <- c(responseOnsetXManualEDA, rep(1, diffLen)),
                      responseEndXManualEDA <- c(responseEndXManualEDA, rep(1, diffLen))
              ) 
              which(responseOnsetXManualEDA[2:length(responseOnsetXManualEDA)] < 
                      responseEndXManualEDA[1:(length(responseEndXManualEDA)-1)]) + 1
              # correct condition if any response end index exceeds the data frame length
              if(length(which(responseEndXManualEDA > nrow(chartDF))) != 0) {
                responseOnsetXManualEDA <- responseOnsetXManualEDA[-which(responseEndXManualEDA > nrow(chartDF)) > 0]
                responseEndXManualEDA <- responseEndXManualEDA[-which(responseEndXManualEDA > nrow(chartDF)) > 0]
              }
            } # end if fix length
            
            # 9-5-2016
            # for each stimulus onset get the min stimulus end that is greater
            newOnset <- NULL
            newEnd <- NULL
            for (l in length(responseOnsetXManualEDA):1) {
              # silence warnings from this section when there is no end index for an onset index
              oldw <- getOption("warn")
              options(warn = -1)
              ifelse(is.na(responseEndXManualEDA[min(which(responseEndXManualEDA >= responseOnsetXManualEDA[l]))]),
                     endVal <- NA,
                     endVal <- responseEndXManualEDA[min(which(responseEndXManualEDA >= responseOnsetXManualEDA[l]))]
              )
              # reset the warning level
              options(warn = oldw)
              # remove the onset value from the new onset vector if there is no response end index
              if(!is.na(endVal)) { newEnd <- c(newEnd, endVal) }
              if(!is.na(endVal)) { newOnset <- c(newOnset, responseOnsetXManualEDA[l]) }
              if(!is.na(endVal)) { responseEndXManualEDA <- responseEndXManualEDA[-which(responseEndXManualEDA==endVal)] }
            }
            responseOnsetXManualEDA <- rev(newOnset)
            responseEndXManualEDA <- rev(newEnd)
            
            responseOnsetYManualEDA <- chartDF$c_ManualEDA[responseOnsetXManualEDA]
            responseEndYManualEDA <- chartDF$c_ManualEDA[responseEndXManualEDA]
            
            # horizontal line begins at the end and points to the onset
            g <- g + annotate("segment",
                              x=responseEndXManualEDA,
                              xend=responseOnsetXManualEDA,
                              y=responseOnsetYManualEDA,
                              yend=responseOnsetYManualEDA,
                              color="purple",
                              size=.5,
                              arrow=arrow(length=unit(0.2, "cm")))
            # vertical line
            g <- g + annotate("segment",
                              x=responseEndXManualEDA,
                              xend=responseEndXManualEDA,
                              y=responseOnsetYManualEDA,
                              yend=responseEndYManualEDA,
                              color="purple",
                              size=.5,
                              arrow=arrow(length=unit(0.2, "cm")))
            
            ### manual EDA response duration
            
            halfRecoveryXManualEDA <- which(chartDF$ManualEDAExtract == "halfRecoveryRow")
            
            # check and fix the length of halfRecoveryXManualEDA and responseEndXManualEDA
            if(length(responseEndXManualEDA) != length(halfRecoveryXManualEDA)) {
              onsetEndVectors <- c(length(responseEndXManualEDA), length(halfRecoveryXManualEDA))
              # then calculate the difference in the length of the onset and end vectors
              diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
              ifelse (which.min(onsetEndVectors)==1,
                      responseEndXManualEDA <- c(responseEndXManualEDA, rep(1, diffLen)),
                      halfRecoveryXManualEDA <- c(halfRecoveryXManualEDA, rep(1, diffLen))
              ) 
            }# end if fix length
            
            # for each stimulus end index get the half recovering index that is greater
            newEnd <- NULL
            newHalfRec <- NULL
            # iterate backwards over the vector of response end indices
            l=length(responseEndXManualEDA)
            for (l in length(responseEndXManualEDA):1) {
              # silence warnings from this section when there is no end index for an onset index
              oldw <- getOption("warn")
              options(warn = -1)
              # set the halfRec value
              ifelse(is.na(halfRecoveryXManualEDA[min(which(halfRecoveryXManualEDA >= responseEndXManualEDA[l]))]),
                     halfRec <- NA,
                     halfRec <- halfRecoveryXManualEDA[min(which(halfRecoveryXManualEDA >= responseEndXManualEDA[l]))]
              )
              # reset the warning level
              options(warn = oldw)
              # remove the onset value from the new onset vector if there is no response end index
              if(!is.na(halfRec)) { 
                newHalfRec <- c(newHalfRec, halfRec) 
                newEnd <- c(newEnd, responseEndXManualEDA[l]) 
                # remove the halfRec value from the halfRecoveryXManualEDA vector before the next iteration
                halfRecoveryXManualEDA <- halfRecoveryXManualEDA[-which(halfRecoveryXManualEDA==halfRec)] 
              }
            } # end for loop
            # need to reverse the vectors because of iterating backwards
            responseEndXManualEDA <- rev(newEnd)
            halfRecoveryXManualEDA <- rev(newHalfRec)
            
            # then get the Y values
            responseEndYManualEDA <- chartDF$c_ManualEDA[responseEndXManualEDA]
            halfRecoveryYManualEDA <- chartDF$c_ManualEDA[halfRecoveryXManualEDA]
            
            # now add the manual EDA duration to the plot
            
            if(isTRUE(showEDADuration)) {
              # horizontal line begins at the response end and points to the response onset
              g <- g + annotate("segment",
                                x=responseEndXManualEDA,
                                xend=halfRecoveryXManualEDA,
                                y=responseOnsetYManualEDA, 
                                yend=responseOnsetYManualEDA,
                                color="purple",
                                size=1,
                                alpha=.3,
                                arrow=arrow(length=unit(0.2, "cm")))
              # vertical line begins at response baseline and points to the peak
              g <- g + annotate("segment",
                                x=halfRecoveryXManualEDA,
                                xend=halfRecoveryXManualEDA,
                                y=responseOnsetYManualEDA, 
                                yend=halfRecoveryYManualEDA,
                                # was "purple",
                                color="black",
                                size=.4)
            }
            
            ## manual EDA response complexity
            
            complexityXManualEDA <- which(chartDF$ManualEDAExtract == "complexityRow")
            complexityYManualEDA <- chartDF$c_ManualEDA[complexityXManualEDA]
            
            complexityXOnsetManualEDA <- NULL
            if(length(complexityXManualEDA) > 0) {
              # initialize a vector
              # find the largest xOnset index preceeding each value in the complexityXManualEDA vector
              # interate over the complexityXManualEDA vector
              l=1
              for(l in 1:length(complexityXManualEDA)) {
                thisIndex <- complexityXManualEDA[l]
                keepIdx <- max(responseOnsetXManualEDA[responseOnsetXManualEDA < thisIndex])
                complexityXOnsetManualEDA <- c(complexityXOnsetManualEDA, keepIdx)
              } # end for
            } # end if
            
            # get the Y values for the response onset for complex responses
            complexityYOnsetManualEDA <- chartDF$c_ManualEDA[complexityXOnsetManualEDA]
            
            # the add the manual EDA Complexity to the plot
            
            if(isTRUE(showEDAComplexity) && length(complexityXOnsetManualEDA) > 0) {
              # vertical segment
              g <- g + annotate("segment",
                                x=complexityXManualEDA,
                                xend=complexityXManualEDA,
                                y=complexityYOnsetManualEDA, 
                                yend=complexityYManualEDA,
                                color="purple",
                                alpha=.4,
                                # arrow=arrow(length=unit(0.2, "cm"))
                                size=.6)
            }
            
          } # end if showManualEDA
          
        } # end if show EDA data
        
        
        ######## Cardio measurement Lines #########
        
        if(isTRUE(showCardioData)) {
          
          responseOnsetXCardio <- which(chartDF$CardioExtract == "responseOnsetRow")
          responseEndXCardio <- which(chartDF$CardioExtract == "responseEndRow")
          
          
          
          
          # check and fix the length of the cardio onset and end vectors if necessary
          if(length(responseOnsetXCardio) != length(responseEndXCardio)) { 
            onsetEndVectors <- c(length(responseOnsetXCardio), length(responseEndXCardio))
            diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
            ifelse (which.min(onsetEndVectors)==1,
                    responseOnsetXCardio <- c(responseOnsetXCardio, rep(1, diffLen)),
                    responseEndXCardio <- c(responseEndXCardio, rep(1, diffLen))
            ) 
            which(responseOnsetXCardio[2:length(responseOnsetXCardio)] < 
                    responseEndXCardio[1:(length(responseEndXCardio)-1)]) + 1
            # correct condition if any response end index exceeds the data frame length
            if(length(which(responseEndXCardio > nrow(chartDF))) != 0) {
              responseOnsetXCardio <- responseOnsetXCardio[-which(responseEndXCardio > nrow(chartDF)) > 0]
              respnseEndXCardio <- responseEndXCardio[-which(responseEndXCardio > nrow(chartDF)) > 0]
            }
          } # end if fix length
          
          # 9-5-2016
          # for each stimulus onset get the min stimulus end that is greater
          newOnset <- NULL
          newEnd <- NULL
          for (l in length(responseOnsetXCardio):1) {
            # silence warnings from this section when there is no end index for an onset index
            oldw <- getOption("warn")
            options(warn = -1)
            ifelse(is.na(responseEndXCardio[min(which(responseEndXCardio >= responseOnsetXCardio[l]))]),
                   endVal <- NA,
                   endVal <- responseEndXCardio[min(which(responseEndXCardio >= responseOnsetXCardio[l]))]
            )
            # reset the warning level
            options(warn = oldw)
            # remove the onset value from the new onset vector if there is no response end index
            if(!is.na(endVal)) { newEnd <- c(newEnd, endVal) }
            if(!is.na(endVal)) { newOnset <- c(newOnset, responseOnsetXCardio[l]) }
            if(!is.na(endVal)) { responseEndXCardio <- responseEndXCardio[-which(responseEndXCardio==endVal)] }
          }
          responseOnsetXCardio <- rev(newOnset)
          responseEndXCardio <- rev(newEnd)
          
          
          
          
          
          
          # # working 9-5-2016 but commented out to try a different method
          # # make sure that each repsonse end is before the next response onset
          # for (i in 1:(length(responseEndXCardio)-1)) {
          #   if(responseEndXCardio[i] >= responseOnsetXCardio[(i+1)]) {
          #     responseEndXCardio[i] <- responseOnsetXCardio[(i+1)]-1
          #     # responseEndXCardio <- c(responseEndXCardio[1:(i-1)],
          #     #                         (responseOnsetXCardio[(i+1)]-1),
          #     #                         responseEndXCardio[(i+2):length(responseEndXCardio)])
          #   }
          # }
          # # fix the last
          # if(responseEndXCardio[length(responseEndXCardio)] <= responseOnsetXCardio[length(responseEndXCardio)]) {
          #   responseEndXCardio[length(responseEndXCardio)] <- (responseOnsetXCardio[length(responseEndXCardio)] + 1)
          # }
          
          
          
          
          # get the cardio data for the measurement
          # must be done before getting they cardio Y onset and end values
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
                            # was blue
                            size=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseEndXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseEndYCardio,
                            color="blue",
                            # was "blue"
                            size=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          
          ######### FC finger cuff measurement Lines #########
          
          if(inclFC==TRUE) {
            
            responseOnsetXFC <- which(chartDF$FCExtract == "responseOnsetRow")
            responseEndXFC <- which(chartDF$FCExtract == "responseEndRow")
            
            # get the cardio data to show the measurement
            useFC <- switch(FCLine,
                            "ma" = chartDF$c_FCMA,
                            "diastolic" = chartDF$c_FCDiastolic,
                            "systolic" = chartDF$c_FCSystolic,
                            "mid" = chartDF$c_FCMid,
                            "otherwise: last")
            
            responseOnsetYFC <- useFC[responseOnsetXFC]
            responseEndYFC <- useFC[responseEndXFC]
            
            g <- g + annotate("segment",
                              x=responseEndXFC,
                              xend=responseOnsetXFC,
                              y=responseOnsetYFC,
                              yend=responseOnsetYFC,
                              color="blue",
                              size=.4,
                              arrow=arrow(length=unit(0.2, "cm")))
            g <- g + annotate("segment",
                              x=responseEndXFC,
                              xend=responseEndXFC,
                              y=responseOnsetYFC, 
                              yend=responseEndYFC,
                              color="blue",
                              size=.4,
                              arrow=arrow(length=unit(0.2, "cm")))
            
          } # end if inclFC
          
        }
        
        ######### PLE measurment #########
        
        if(isTRUE(showPLEData)) {
          
          if(inclPLE==TRUE) {
            
            # check to see if the there is any PLE extraction
            if(!is.na(which(chartDF$PLEExtract=="prestimSegOnset")[1])) {
              
              ### PLE prestim measurements ###
              
              # make a vector of prestim onset x indices
              preOnX <- which(chartDF$PLEExtract=="prestimSegOnset")
              # preOffX <- which(chartDF$PLEExtract=="prestimSegOffset")
              preOffX <- preOnX + 89
              
              # preOnY <- mean(chartDF$c_PLMax[preOnX:preOffX])
              # preOffY <- mean(chartDF$c_PLMin[preOnX:preOffX])
              # different method using the PLEMeans
              # preOnY <- chartDF$c_PLMA[(preOnX+45)] + ((as.numeric(chartDF$PLEMeans[preOnX]) * PLScale) / 2)
              # preOffY <- chartDF$c_PLMA[(preOnX+45)] - ((as.numeric(chartDF$PLEMeans[preOffX]) * PLScale) / 2)
              
              # make a data frame for each PLE prestim segment
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
                
                # DF$preOnX[l] <- preOnX[l]
                # DF$preOffX[l] <- preOffX[l]
                # DF$preOnY[l] <- mean(chartDF$c_PLMax[preOnX[l]:preOffX[l]])
                # DF$preOffY[l] <- mean(chartDF$c_PLMin[preOnX[l]:preOffX[l]])
                
                # other method
                # DF$preOnY <- chartDF$c_PLMA[(preOnX[l]+45)] + ((as.numeric(chartDF$PLEMeans[preOnX[l]]) * PLScale) / 2)
                # DF$preOffY <- chartDF$c_PLMA[(preOnX[l]+45)] - ((as.numeric(chartDF$PLEMeans[preOnX[l]]) * PLScale) / 2)
                # assign(paste0("prePLEDF", l), DF, pos=1)
                
                # assign the prePLEVectors to the global environment
                assign(paste0("prePLEVector", l), prePLE, pos=1)
              }
              
              # loop over the prePLEVector data frames for the prestim segments
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
                                  size=.25,
                                  alpha=.25,
                                  # was "green"
                                  fill="blue")
              }
              
              ### PLE poststim measurements ###
              
              # make a vector of poststim onset x indices
              postOnX <- which(chartDF$PLEExtract=="poststimSegOnset")
              postOnX[postOnX >= nrow(chartDF)] <- nrow(chartDF)-2
              
              # postOffX <- which(chartDF$PLEExtract=="poststimSegOffset")
              postOffX <- postOnX + 149
              postOffX[postOffX >= nrow(chartDF)] <- nrow(chartDF)-1
              
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
                
                # assign the postPLEVectors to the global environment
                assign(paste0("postPLEVector", l), postPLE, pos=1)
              }
              
              # loop over the data frames for the poststim segments
              # this is done using separate data frames for each segement
              # because ggplot has lazy evaluation and will evaluate only the last x indices
              postPLEs <- ls(pattern="postPLEVector", pos=1)
              for (l in 1:length(postPLEs)) {
                postVector <- get(paste0("postPLEVector", l), pos=1)
                g <- g + annotate("rect",
                                  xmin=postVector['postOnX'],
                                  xmax=postVector['postOffX'],
                                  ymin=postVector['postOnY'],
                                  ymax=postVector['postOffY'],
                                  color="black",
                                  size=.25,
                                  alpha=.25,
                                  # was "green"
                                  fill="blue")
              }
              
              # clean up the global environment
              rm(list=ls(pattern="prePLE"))
              rm(list=ls(pattern="postPLE"))
              
            } # end if !is.na
            
          } # end if inclPLE
          
        }
        
      } # end if showMeasurements==TRUE & length(eventNames)!=0
      
      #################################################################################
      
      ##########################  print scores     ########################
      
      if(showScores == TRUE) {
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # uPneumoScores
          # llPneumoScores
          # pneumoScores
          # edaScores
          # cardioScores
          # pleScores
          
          # if(length(which(chartMeasurementDF$integerScore != "")) > 0) {
          
          # first get all row indices for all integer scores
          # measurementRows <- which(chartMeasurementDF$integerScore != "")
          measurementRows <- which(chartMeasurementDF$rankScore != "")
          
          # make a private function to remove leading zeroes from numeric scores
          numFormatFn <- function(x) {
            # function to remove leading zeros
            # vectorized already and needs no loop
            outVector <- x
            useIdx <- which(!(is.na(x) | x == ""))
            x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.2f", as.numeric(x[useIdx]))) )
            outVector[useIdx] <- x2
            return(outVector)
          }
          
          # make a vector of scores for printing
          # use the rank scores if there are no integer scores
          if(selectScores == "auto") {
            ifelse(length(which(chartMeasurementDF$integerScore != "")) > 0,
                   printScores <- chartMeasurementDF$integerScore,
                   # printScores <- as.character(round(as.numeric(chartMeasurementDF$score),1)),
                   # printScores <- chartMeasurementDF$miritelloRank,
                   # printScores <- chartMeasurementDF$RRMScore,
                   printScores <- chartMeasurementDF$rankScore
            )
          } else {
            printScores <- switch(selectScores, 
                                  "miritello"=chartMeasurementDF$miritelloRank,
                                  "raskin"=chartMeasurementDF$RRMScore,
                                  "rank"=chartMeasurementDF$rankScore,
                                  "integer"=chartMeasurementDF$integerScore,
                                  "RC"=chartMeasurementDF$score,
                                  "ipZ"=chartMeasurementDF$ipZ 
                                  )
          }
          
          # use a function to remove leading zeros
          printScores <- numFormatFn(printScores)
          
          ###########
          
          if(isTRUE(showPneumoData)) {
            
            showPneumoScores <- TRUE
            
            if(showPneumoScores==TRUE) {
              
              ###  upper pneumo  ### 
              
              # get all row indices for the upper pneumo data
              uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
              # then select the row indices for the upper pneumo scores in the chart measurements
              uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
              # get the upper pneumo scores
              uPneumoScores <- printScores[uPneumoSelectRows]
              # uPneumoScores <- chartMeasurementDF$integerScore[uPneumoSelectRows]
              # uPneumoScores <- chartMeasurementDF$rankScore[uPneumoSelectRows]
              # get the row indices for the upper pneumo scores in the chartDF
              uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
              # plot the upper pneumo scores
              if(length(uPneumoScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(uPneumoScoreIndices + 150),
                                  y=rep(yOffset['uPneumo'], times=length(uPneumoScores)),
                                  label=uPneumoScores,
                                  color="black",
                                  alpha=.3,
                                  size=6)
              }
              
              ### lower pneumo  ###
              
              # get all row indices for the lower pneumo data
              lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
              # then select the row indices for the lower pneumo scores in the chart measurements
              lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
              # get the lower pneumo scores
              lPneumoScores <- printScores[lPneumoSelectRows]
              # lPneumoScores <- chartMeasurementDF$integerScore[lPneumoSelectRows]
              # lPneumoScores <- chartMeasurementDF$rankScore[lPneumoSelectRows]
              # get the row indices for the lower pneumo scores in the chartDF
              lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
              # plot the lower pneumo scores
              if(length(lPneumoScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(lPneumoScoreIndices + 150),
                                  y=rep(yOffset['lPneumo'], times=length(lPneumoScores)),
                                  label=lPneumoScores,
                                  color="black",
                                  alpha=.3,
                                  size=6)
              }
              
              ### combined pneumo score  ###
              
              # get all row indices for the combined pneumo score
              pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
              # then select the row indices for the combined pneumo scores in the chart measurements
              pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
              # get the combined pneumo scores
              pneumoScores <- printScores[pneumoSelectRows]
              # pneumoScores <- chartMeasurementDF$integerScore[pneumoSelectRows]
              # pneumoScores <- chartMeasurementDF$rankScore[pneumoSelectRows]
              # get the row indices for the combined pneumo scores in the chartDF
              pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
              # calculate the y offset
              yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
              # plot the combined pneumo scores
              if(length(pneumoScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(pneumoScoreIndices + 150),
                                  y=rep(yPneumo, times=length(pneumoScores)),
                                  label=pneumoScores,
                                  color="black",
                                  size=6)
              }
              
            } # end if showPneumoScores == TRUE
            
          }
        
          ###  EDA  ###
          
          if(isTRUE(showEDAData)) {
            
            showEDAScores <- TRUE
            
            if(showEDAScores==TRUE) {
              
              ### Auto EDA
              
              # get all row indices for the EDA data
              AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
              # then select the row indices for the EDA scores 
              # in the chart measurements DF
              AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
              # get the EDA scores
              # use printScores to automatically select rank or integer scores
              AutoEDAScores <- printScores[AutoEDASelectRows]
              # AutoEDAScores <- chartMeasurementDF$integerScore[AutoEDASelectRows]
              # AutoEDAScores <- chartMeasurementDF$rankScore[AutoEDASelectRows]
              # get the row indices for the EDA scores in the chartDF
              if(length(AutoEDAScores != 0)) {
                AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                               chartMeasurementDF$eventLabel[AutoEDASelectRows])
                # plot the EDA scores
                if(length(AutoEDAScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(AutoEDAScoreIndices + 150),
                                    y=rep(yOffset['eda'], times=length(AutoEDAScores)),
                                    label=AutoEDAScores,
                                    color="black",
                                    size=7,
                                    na.rm=TRUE)
                }
              }
              
              ### Manual/un-filtered EDA
              
              if(isTRUE(showManualEDA)) {
                
                # get all row indices for the EDA data
                ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
                # then select the row indices for the EDA scores 
                # in the chart measurements DF
                ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
                # get the EDA scores
                # use printScores to automatically select rank or integer scores
                ManualEDAScores <- printScores[ManualEDASelectRows]
                # ManualEDAScores <- chartMeasurementDF$integerScore[ManualEDASelectRows]
                # ManualEDAScores <- chartMeasurementDF$rankScore[ManualEDASelectRows]
                # get the row indices for the EDA scores in the chartDF
                if(length(ManualEDAScores != 0)) {
                  ManualEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                                   chartMeasurementDF$eventLabel[ManualEDASelectRows])
                  # plot the EDA scores
                  if(length(ManualEDAScores) != 0) {
                    g <- g + annotate(geom="text",
                                      x=(ManualEDAScoreIndices + 150),
                                      y=rep((yOffset['eda']-100), times=length(ManualEDAScores)),
                                      label=ManualEDAScores,
                                      color="black",
                                      size=7,
                                      na.rm=TRUE)
                  }
                }
                
              } # end if showManualEDA
              
            } # end if showEDAScores
            
          }
          
          ###  Cardio  ###
          
          if(isTRUE(showCardioData)) {
            
            showCardioScores <- TRUE
            
            if(showCardioScores==TRUE) {
              
              # get all row indices for the Cardio data
              cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
              # then select the row indices for the Cardio scores in the chart measurements
              cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
              # get the Cardio scores
              cardioScores <- printScores[cardioSelectRows]
              # cardioScores <- chartMeasurementDF$integerScore[cardioSelectRows]
              # cardioScores <- chartMeasurementDF$rankScore[cardioSelectRows]
              # get the row indices for the Cardio scores in the chartDF
              if(length(cardioScores) > 0) {
                cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
                # plot the Cardio scores
                if(length(cardioScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(cardioScoreIndices + 150),
                                    # need to use rep() for y in order to avoid a warning about names
                                    y=rep(yOffset['cardio'], times=length(cardioScores)),
                                    label=cardioScores,
                                    color="black",
                                    size=6,
                                    na.rm=TRUE)
                }
              }
              
            } # end if showCardioScores
            
          }
          
          ###  PLE  ###
          
          if(isTRUE(showPLEData)) {
            
            showPLEScores <- TRUE
            
            if(showPLEScores==TRUE) {
              
              # get all row indices for the PLE data
              pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
              # then select the row indices for the PLE scores in the chart measurements
              pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
              # get the PLE scores
              pleScores <- printScores[pleSelectRows]
              # pleScores <- chartMeasurementDF$integerScore[pleSelectRows]
              # pleScores <- chartMeasurementDF$rankScore[pleSelectRows]
              # get the row indices for the PLE scores in the chartDF
              if( length(pleScores) > 0 & !all(is.na(pleScores)) ) {
                pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
                # plot the PLE scores
                if(length(pleScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(pleScoreIndices + 150),
                                    # need to use rep() for y in order to avoid a warning about names
                                    y=rep(yOffset['ple'], times=length(pleScores)),
                                    label=pleScores,
                                    color="black",
                                    size=6,
                                    na.rm=TRUE)
                }
              }
              
            } # end if showPLEScores
            
          }
          
          # } # end if for integer scores
          
        } # end if !is.null(measurementDF)
        
      } # end if showScores==TRUE
      
      ############################ print warnings ############################
      
      if(showWarnings==TRUE) {
        
        if(XWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yMin+(.05*yRange), 
                            label=XWarning,
                            color="black", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        # if(cardioWarning != "none") {
        #   g <- g + annotate(geom="text", 
        #                     x=60, 
        #                     y=(yOffset[4]+reOffsetCardio+newCardio1Offset), 
        #                     label=cardioWarning,
        #                     color="black", 
        #                     fontface="bold",
        #                     hjust=0,
        #                     size=3.5)
        # }
        
        if(cardioRateWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yMin+(.22*yRange), 
                            label=cardioRateWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        if(activityWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yOffset[6], 
                            label=activityWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
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
                            y=yOffset[2] - .1*yRange, 
                            label=pneumoRateWarning,
                            color="black", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        if(rbpfWarning != "none") {
          g <- g + annotate(geom="text",
                            x=60,
                            y=yOffset[4] + (.05*yRange),
                            label=rbpfWarning,
                            color="black",
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        if(edaWarning != "none") {
          g <- g + annotate(geom="text",
                            x=60,
                            y=yOffset[3] - (.05*yRange),
                            label=edaWarning,
                            color="black",
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
      } # end if showWarnings==TRUE
   
      ########################## print other information ######################
      
      # cardio and eCardio correlation
      if(inclECardio==TRUE) {
        cardioCor <- round(cor(chartDF$c_Cardio1, chartDF$c_eCardio), 4)
        g <- g + annotate(geom="text",
                          x=150,
                          y=yOffset[4] - (.1*yRange),
                          label=paste0("cardio eCardio correlation: r = ", cardioCor),
                          color="black",
                          fontface="plain",
                          hjust=0,
                          size=5)
      }
      
      ######################### plot appearance #########################
      
      g <- g + ylab("y-change")
      g <- g + xlab("x-time")
      # g <- g + scale_x_continuous(breaks=seq(0,nrow(chartDF),300))
      g <- g + ggtitle(plotTitle)
      g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(chartDF),600))
      
      ##########
      
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
    dev.new() 
  } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

# reset the NCCA ASCII init 
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)

