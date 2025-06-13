# plot a stimulus segment recursively
#
# sampling rate is 30cps
# 
#
################################


library(stringr)
library(ggplot2)
library(grid)


# source the list of excluded events so that measurements are not plotted for these
# source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R')


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[11]


# source the initialization script to set the plot parameters
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot_init.R')


# # parameters to describe the data collection, signal processing, and feature extraction
# cps <- 30
# prestimSeg <- 10
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15
# addSeg <- 10


# # to control the print output
# showNames <- TRUE
# output <- FALSE
# showMeasurements <- TRUE
# showArtifacts <- TRUE
# outputSegmentFileName <- "_segmentPlot.pdf"
# separateCharts <- FALSE
# printPlot <- TRUE


# # plot a single segment instead of iterating through all exams in the global environment
# getSegment <- FALSE
# examNum <- 1
# seriesNum <- 1
# chartNum <- 1
# segmentNum <- 8


# to re-initialize the graphics device
# graphics.off()
# dev.new()


if(getSegment == TRUE) {
  assign("examName", uniqueExams[examNum])
  uniqueExams <- uniqueExams[examNum] 
} 


# loop over each exam in the list and plot the stimulus segments
for(i in 1:length(uniqueExams)) {
  # i=1
  examName <- uniqueExams[i]
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Data", "*")
  examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  if(showNames==TRUE) print(examName)
  
  if(printPlot == TRUE) {
    if(separateCharts==FALSE) {
      pdf(paste(examName, outputSegmentFileName, sep="_"), height=5.5, width=3.5)
    } else {
      pdf(paste(examName, chartName, outputSegmentFileName, sep="_"), height=5.5, width=3.5)
    }
  }

  examStartRow <- 1
  examEndRow <- nrow(examDF)
  
  ### add additional columns here
  
  # get the names of all unique series in the exam
  uniqueSeries <- as.character(unique(examDF$seriesName))
  
  if(getSegment == TRUE) {
    if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum]
  }
  
  # loop over each unique series
  for(j in 1:length(uniqueSeries)) {
    # j=1
    seriesName <- uniqueSeries[j]
    
    # get the list of time series data for the charts in the exam
    seriesDF <- examDF[examDF$seriesName==seriesName,]
    
    if(showNames==TRUE) print(paste("series", seriesName))
    
    seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
    seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
    
    # uniqueCharts <- names(seriesDF)
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
      
      if(nrow(chartDF)<600) next()
      
      if(showNames==TRUE) print(chartName)
      
      chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
      chartEndRow <- chartOnsetRow + nrow(chartDF) - 1

      if(printPlot == TRUE) {
        if(separateCharts==TRUE) {
          pdf(paste(examName, chartName, outputSegmentFileName, sep="_"), height=5.5, width=3.5)
        }
      }
      
      # make a vector of event names
      eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
      # and a vector of event onset rows
      eventRows <- which(chartDF$eventLabel!="")
      # get the first and last events
      firstEvent <- eventRows[!(eventNames %in% excludeEvents)][1]
      lastEvent <- eventRows[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
      lastEventEnd <- lastEvent + 480
      
      ############################## scale the data for the chart #################
      
      # scaling is performed in the sigProc function
      
      ####################### process the stimulus segments #################
      
      if(getSegment == TRUE) { 
        if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
      }
      
      # loop over all the events in the chart data frame
      for (l in 1:length(eventNames)) {
        # l=1
        segmentName <- eventNames[l]
        # get the onset row  for the chart DF so that events during the prestimSegment are ignored
        segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
        # segOnsetRow <- eventRows[l] # does not work when getting a single segment
        # get the segment prestim row
        prestimRow <- segOnsetRow - prestimSeg*cps
        if(prestimRow < 1) prestimRow <- 1
        # set the end row so that the data frame
        segEndRow <- segOnsetRow + measuredSeg*cps - 1
        if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
        # set the row number for the end of the stimulus segment
        endRow <- segEndRow + addSeg*cps
        if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
        # get the segment start row
        startRow <- prestimRow
        
        # get the segment data frame
        segmentDF <- chartDF[startRow:endRow,]
        
        # skip the segment if it is too short
        if(nrow(segmentDF) < 180) next
        
        # plotTitle <- paste(examName, chartName, segmentName)
        plotTitle <- paste(chartName, segmentName)
        
        # adjust the rows so that row numbers refer to data in the segmentDF not the chartDF
        prestimRow <- prestimRow - startRow + 1 # this will set the prestim row to 1
        segOnsetRow <- segOnsetRow - startRow + 1 # will normally set to 151
        segEndRow <- segEndRow - startRow + 1 # will normally set to 600
        
        if(showNames==TRUE) print(segmentName)
        
        #################################### 
        
        ### offsetting is performed by a function in the sigProc function
        
        ##################################
        
        # save the data frame for analysis 
        # assign("segmentDF", segmentDF, pos=1)
        
        ###########################################################################
        ###########################################################################
        
        # make the plot using the ggplot2 package
        # ggplot normally executes in the global environment
        g <- ggplot()
        
        ############ tracing baselines #############
        
        g <- g + geom_hline(aes(yintercept=yOffset), color="brown", size=.15)
        
        ############ vertical lines
        
        # stimulus onset line
        onsetRow <- segOnsetRow
        g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
        # end of scoring window
        segEndRow <- onsetRow+(measuredSeg*cps) - 1
        if(segEndRow > nrow(segmentDF)) segEndRow <- (nrow(segmentDF))
        g <- g + geom_vline(aes(xintercept=as.numeric(segEndRow)), color="grey70")
        # EDA latency
        EDALatRow <- onsetRow+(EDALat*cps)
        g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
        # offset line for the end of the stimulus question
        offsetRow <- which(segmentDF$Events[onsetRow:nrow(segmentDF)]=="offsetRow")[1] + onsetRow - 1
        g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
        # answer line
        answerRow <- which(segmentDF$Events[onsetRow:nrow(segmentDF)]=="answerRow")[1] + onsetRow - 1
        if(is.na(answerRow)) { answerRow <- offsetRow - 1 }
        g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="black")
        # end of response onset window
        ROWEndRow <- answerRow + (ROWEnd*cps)
        g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
        
        ############ shaded areas
        
        # scoring window shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(onsetRow), 
                          xmax=as.numeric(segEndRow), 
                          ymin=-175, 
                          ymax=175, 
                          alpha=.12, 
                          fill="blue")
        # stimulus question shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(onsetRow),
                          xmax=as.numeric(offsetRow),
                          ymin=-135, 
                          ymax=135, 
                          alpha=.3, 
                          fill="grey20")
        # latency shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(onsetRow),
                          xmax=as.numeric(onsetRow+15),
                          ymin=-175,
                          ymax=175,
                          alpha=.125,
                          fill="red")
        # response onset window shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(offsetRow),
                          xmax=as.numeric(ROWEndRow),
                          ymin=-150,
                          ymax=150,
                          alpha=.125,
                          fill="blue")
        
        ############ warnings ###########################
        
        # if(cardioWarning != "none") {
        #   g <- g + annotate(geom="text", 
        #                     x=60, 
        #                     y=yOffset['cardio'], 
        #                     label=cardioWarning,
        #                     color="red", 
        #                     fontface="bold",
        #                     hjust=0,
        #                     size=3)
        # }
        
        if(activityWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yOffset['activity'], 
                            label=activityWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=3)
        }
        
        #################### data segments ###################
        
        # upper pnuemo
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoSm), color="blue1", size=.25) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoMid), color="blue1", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # lower pneumo
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoSm), color="blue4", size=.25) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoMid), color="blue4", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # eda
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDA), color="green4", size=.4) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDAMid), color="green4", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # cardio
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Cardio1), color="red", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioMA), color="red", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # photoplethysmograph
        if(inclPLE==TRUE) { 
          # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PLMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PLMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PL), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PLMA), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
        } # end if for PLE
        # seat activity sensor
        if(activityWarning=="none") {
          # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_SEMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_SEMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_SE), color="grey35", size=.4) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_SEMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        }
        
        # # un-used sensor columns
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1), color="grey20")
        # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Aux02), color="grey20")
        
        ########### artifacts
        
        if(showArtifacts==TRUE) {
          
          ### Penumo artifacts
          
          g <- g + annotate("point", x=which(segmentDF$UPneumo_a=="Artifact"),
                            y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumo_a=="Artifact")],
                            shape=4, size=3)
          # g <- g + annotate("point", x=which(segmentDF$UPneumoInh_a=="Artifact"),
          #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumoInh_a=="Artifact")],
          #                   shape=4, size=3)
          # g <- g + annotate("point", x=which(segmentDF$UPneumoExh_a=="Artifact"),
          #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumoExh_a=="Artifact")],
          #                   shape=4, size=3)
          # g <- g + annotate("point", x=which(segmentDF$UPneumoMid_a=="Artifact"),
          #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumoMid_a=="Artifact")],
          #                   shape=4, size=3)
          
          g <- g + annotate("point", x=which(segmentDF$LPneumo_a=="Artifact"),
                            y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumo_a=="Artifact")],
                            shape=4, size=3)
          # g <- g + annotate("point", x=which(segmentDF$LPneumoInh_a=="Artifact"),
          #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumoInh_a=="Artifact")],
          #                   shape=4, size=3)
          # g <- g + annotate("point", x=which(segmentDF$LPneumoExh_a=="Artifact"),
          #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumoExh_a=="Artifact")],
          #                   shape=4, size=3)
          # g <- g + annotate("point", x=which(segmentDF$LPneumoMid_a=="Artifact"),
          #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumoMid_a=="Artifact")],
          #                   shape=4, size=3)
          
          ### buffer around the pneumo artifacts
          
          halfBuffer <- round(1.5*cps,0)
          
          UPneumoArtifacts <- which(segmentDF$UPneumo_a=="Artifact")
          UPArtifactBuffXOn <- UPneumoArtifacts[which(UPneumoArtifacts>halfBuffer & UPneumoArtifacts<nrow(segmentDF)-halfBuffer)] - halfBuffer
          
          UPArtifactBuffXOff <- UPneumoArtifacts[which(UPneumoArtifacts>halfBuffer & UPneumoArtifacts<nrow(segmentDF)-halfBuffer)] + halfBuffer
          UPArtifactBuffYOn <- segmentDF$c_UPneumoSm[UPArtifactBuffXOn]
          UPArtifactBuffYOff <- segmentDF$c_UPneumoSm[UPArtifactBuffXOff]
          g <- g + annotate("segment",
                            x=UPArtifactBuffXOn,
                            xend=UPArtifactBuffXOff,
                            y=UPArtifactBuffYOn,
                            yend=UPArtifactBuffYOff,
                            color="black",
                            linetype="solid",
                            size=.5)
          
          LPneumoArtifacts <- which(segmentDF$LPneumo_a=="Artifact")
          LPArtifactBuffXOn <- LPneumoArtifacts[which(LPneumoArtifacts>halfBuffer & LPneumoArtifacts<nrow(segmentDF)-halfBuffer)] - halfBuffer
          
          LPArtifactBuffXOff <- LPneumoArtifacts[which(LPneumoArtifacts>halfBuffer & LPneumoArtifacts<nrow(segmentDF)-halfBuffer)] + halfBuffer
          LPArtifactBuffYOn <- segmentDF$c_LPneumoSm[LPArtifactBuffXOn]
          LPArtifactBuffYOff <- segmentDF$c_LPneumoSm[LPArtifactBuffXOff]
          g <- g + annotate("segment",
                            x=LPArtifactBuffXOn,
                            xend=LPArtifactBuffXOff,
                            y=LPArtifactBuffYOn,
                            yend=LPArtifactBuffYOff,
                            color="black",
                            linetype="solid",
                            size=.5)
          
          ### EDA artifacts
          
          # g <- g + annotate("point", x=which(segmentDF$c_AutoEDA_a=="Artifact"),
          #                   y=segmentDF$c_AutoEDA[which(segmentDF$c_AutoEDA_a=="Artifact")],
          #                   shape=4, size=1)
          
          ### cardio artifacts
          
          g <- g + annotate("point", x=which(segmentDF$Cardio1_a=="Artifact"),
                            y=segmentDF$c_CardioMid[which(segmentDF$Cardio1_a=="Artifact")],
                            shape=4, size=1, color="brown")
          g <- g + annotate("point", x=which(segmentDF$CardioSystolic_a=="Artifact"),
                            y=segmentDF$c_CardioSystolic[which(segmentDF$CardioSystolic_a=="Artifact")],
                            shape=4, size=1, color="green")
          g <- g + annotate("point", x=which(segmentDF$CardioDiastolic_a=="Artifact"),
                            y=segmentDF$c_CardioDiastolic[which(segmentDF$CardioDiastolic_a=="Artifact")],
                            shape=4, size=1, color="blue")
          g <- g + annotate("point", x=which(segmentDF$CardioMA_a=="Artifact"),
                            y=segmentDF$c_CardioMA[which(segmentDF$CardioMA_a=="Artifact")],
                            shape=4, size=1, color="black")
          # g <- g + annotate("point", x=which(segmentDF$CardioMid_a=="Artifact"),
          #                   y=segmentDF$c_CardioMid[which(segmentDF$CardioMid_a=="Artifact")],
          #                   shape=4, size=1, color="slateblue1")
          
          ### PLE artifacts
          
          
          
          ### seat activitiy sensor artifacts
          
          if(activityWarning=="none") {
            
            g <- g + annotate("point", x=which(segmentDF$SEMA_a=="Artifact"),
                              y=segmentDF$c_SE[which(segmentDF$SEMA_a=="Artifact")],
                              shape=4, size=3, color="red") 
            #         g <- g + annotate("point", x=which(segmentDF$SEMax_a=="Artifact"),
            #                           y=segmentDF$c_SE[which(segmentDF$SEMax_a=="Artifact")],
            #                           shape=4, size=3, color="red") 
            #         g <- g + annotate("point", x=which(segmentDF$SEMin_a=="Artifact"),
            #                           y=segmentDF$c_SE[which(segmentDF$SEMin_a=="Artifact")],
            #                           shape=4, size=3, color="red") 
            #         g <- g + annotate("point", x=which(segmentDF$SE_a=="Artifact"),
            #                           y=segmentDF$c_SE[which(segmentDF$SE_a=="Artifact")],
            #                           shape=4, size=3, color="red") 
            
          }
          
        } # end if showArtifacts==TRUE
        
        ###################    measurements     ######
        
        # # not needed when using a source file to name the excluded events
        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN", "ISN")
        # excludeEvents <- c(excludeEvents, "I1", "INT", "Int", "INT1", "Int1", "N1", "N2", "N3", "N4", "N7", "S", "SR", "Sy", "Sa", "SA", "SAC", "O")
        # excludeEvents <- c(excludeEvents, "I1", "I2", "I3", "I4", "I7", "SY3", "SY8", "Sy3", "Sy8", "S3", "S8", "SR2", "S2")
        
        # giant if statement to plot measurements only for the measured segments
        if(!(segmentName %in% excludeEvents)) {
          
          # Pneumo measurement lines
          
          if(showMeasurements==TRUE) {
            if(!is.na(which(segmentDF$UPneumoExtract=="responseOnsetRow")[1])) {
              g <- g + geom_line(data=segmentDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_UPneumoSm), color="blue1", size=.8)
              g <- g + geom_line(data=segmentDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_LPneumoSm), color="blue3", size=.8)
              
          # commented out 4/5/2016 not needed when artifact buffers are used    
          #     #### buffer around the verbal response is not included in the measurement
          #     # upper pneumo answer buffer
          #     aBuffXOnU <- which(segmentDF$UPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOn")[1] + onsetRow - 1
          #     aBuffXOffU <- which(segmentDF$UPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOff")[1] + onsetRow - 1
          #     aBuffYOnU <- segmentDF$c_UPneumoSm[aBuffXOnU][1]
          #     aBuffYOffU <- segmentDF$c_UPneumoSm[aBuffXOffU][1]
          #     g <- g + annotate("segment",
          #                       x=aBuffXOnU,
          #                       xend=aBuffXOffU,
          #                       y=aBuffYOnU,
          #                       yend=aBuffYOffU,
          #                       color="black",
          #                       linetype="solid",
          #                       size=1)
          #     # lower pneumo answer buffer
          #     aBuffXOnL <- which(segmentDF$LPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOn")[1] + onsetRow - 1
          #     aBuffXOffL <- which(segmentDF$LPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOff")[1] + onsetRow - 1
          #     aBuffYOnL <- segmentDF$c_LPneumoSm[aBuffXOnU][1]
          #     aBuffYOffL <- segmentDF$c_LPneumoSm[aBuffXOffU][1]
          #     g <- g + annotate("segment",
          #                       x=aBuffXOnL,
          #                       xend=aBuffXOffL,
          #                       y=aBuffYOnL,
          #                       yend=aBuffYOffL,
          #                       color="black",
          #                       linetype="solid",
          #                       size=1)
          #     
              
            } # end if !is.na for pneumo response onset
          } # end if showMeasurements for Pneumo
          
          ### EDA measurement lines
          
          if(showMeasurements==TRUE) {
            if(!is.na(which(segmentDF$AutoEDAExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1])) {
              EDAxOn <- segOnsetRow - 1 + which(segmentDF$AutoEDAExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1]
              if(!is.na(EDAxOn)) { if(EDAxOn > ROWEndRow) EDAxOn <- NA }
              if(is.na(EDAxOn)) { EDAyOn <- NA; EDAxOff <- NA; EDAyOff<- NA }
              if(!is.na(EDAxOn)) EDAxOff <- EDAxOn - 1 + which(segmentDF$AutoEDAExtract[EDAxOn:nrow(segmentDF)]=="responseEndRow")[1]
              if(!is.na(EDAxOn)) EDAyOn <- segmentDF$c_AutoEDA[EDAxOn]
              if(!is.na(EDAxOn)) EDAyOff <- segmentDF$c_AutoEDA[EDAxOff]
              if(!is.na(EDAxOn)) {
                # horzontal EDA segment measurement
                g <- g + annotate("segment",
                                  x=EDAxOff,
                                  xend=EDAxOn,
                                  y=EDAyOn,
                                  yend=EDAyOn,
                                  color="purple",
                                  size=.5,
                                  arrow=arrow(length=unit(0.2, "cm")))
                # vertical EDA segment
                # geom_segment and annotate are two different ways of adding the lines
                # annotate is probably better
                g <- g + annotate("segment",
                                  x=EDAxOff,
                                  xend=EDAxOff,
                                  y=EDAyOn,
                                  yend=EDAyOff,
                                  color="purple",
                                  size=.5,
                                  arrow=arrow(length=unit(0.2, "cm")))
              }
            } # end if !is.na for EDA response onset
          } # end if showMeasurements for EDA 
          
          ### Cardio measurement Lines
          
          if(showMeasurements==TRUE) {
            if(!is.na(which(segmentDF$CardioExtract=="responseOnsetRow")[1])) {
              # cardio X onsent annd offset
              cardioXOn <- segOnsetRow - 1 + which(segmentDF$CardioExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1]
              if(!is.na(cardioXOn)) { if(cardioXOn > ROWEndRow) cardioXOn <- NA }
              if(!is.na(cardioXOn)) cardioXOff <- cardioXOn - 1 + which(segmentDF$CardioExtract[cardioXOn:nrow(segmentDF)]=="responseEndRow")[1]
              # choose the cardio line to measure
              useCardio <- switch(cardioLine,
                                  "ma" = segmentDF$c_CardioMA,
                                  "diastolic" = segmentDF$c_CardioDiastolic,
                                  "systolic" = segmentDF$c_CardioSystolic,
                                  "mid" = segmentDF$c_CardioMid,
                                  "otherwise: last")
              # cardio Y onset and offset
              if(!is.na(cardioXOn)) cardioYOn <- useCardio[cardioXOn]
              if(!is.na(cardioXOn)) cardioYOff <- useCardio[cardioXOff]
              # add the cardio extraction to the plot
              if(!is.na(cardioXOn)) {
                # horzontal Cardio segment measurement
                g <- g + annotate("segment",
                                  x=cardioXOff,
                                  xend=cardioXOn,
                                  y=cardioYOn,
                                  yend=cardioYOn,
                                  color="blue",
                                  size=.5,
                                  arrow=arrow(length=unit(0.2, "cm")))
                # vertical Cardio segment
                # geom_segment and annotate are two different ways of adding the lines
                # annotate is probably better
                g <- g + annotate("segment",
                                  x=cardioXOff,
                                  xend=cardioXOff,
                                  y=cardioYOn,
                                  yend=cardioYOff,
                                  color="blue",
                                  size=.5,
                                  arrow=arrow(length=unit(0.2, "cm")))
              } # end cardio segment measurements
            } # end if !is.na
          } # end if showMeasurements for Cardio
          
          ### PLE measurment indicators
          
              if(inclPLE==TRUE) { 
                if(showMeasurements==TRUE) {
                  if(!is.na(which(segmentDF$PLEExtract=="prestimSegOnset")[1])) {
                    
                    # prestim measurements
                    # this will locate the box in the middle of the x scale for the segment
                    preOnX <- which(segmentDF$PLEExtract=="prestimSegOnset")[1]
                    preOffX <- which(segmentDF$PLEExtract=="prestimSegOffset")[1]
                    preOnY <- mean(segmentDF$c_PLMax[preOnX:preOffX])
                    preOffY <- mean(segmentDF$c_PLMin[preOnX:preOffX])
                    # # different method using the PLEMeans
                    # # does not locate the box in the middle of the x scale for the segment
                    # preOnY <- segmentDF$c_PLMA[(preOnX+45)] + ((as.numeric(segmentDF$PLEMeans[preOnX]) * PLScale) / 2)
                    # preOffY <- segmentDF$c_PLMA[(preOnX+45)] - ((as.numeric(segmentDF$PLEMeans[preOffX]) * PLScale) / 2)
                    
                    # poststim measurements
                    # this will locate the box in the middle of the x scale for the segment
                    postOnX <- which(segmentDF$PLEExtract=="poststimSegOnset")[1]
                    postOffX <- which(segmentDF$PLEExtract=="poststimSegOffset")[1]
                    postOnY <- mean(segmentDF$c_PLMax[postOnX:postOffX])
                    postOffY <- mean(segmentDF$c_PLMin[postOnX:postOffX])
                    # # different method using the PLEMeans
                    # # does not locate the box in the middle of the x scale for the segment
                    # postOnY <- segmentDF$c_PLMA[(postOnX+75)] + ((as.numeric(segmentDF$PLEMeans[postOnX]) * PLScale) / 2)
                    # postOffY <- segmentDF$c_PLMA[(postOnX+75)] - ((as.numeric(segmentDF$PLEMeans[postOffX])* PLScale) / 2)
                    
                    # prestim shaded area
                    g <- g + annotate("rect", 
                                      xmin=as.numeric(preOnX), 
                                      xmax=as.numeric(preOffX), 
                                      ymin=preOnY, 
                                      ymax=preOffY, 
                                      color="black",
                                      size=.3,
                                      alpha=.2, 
                                      fill="green")
                    
                    # stimulus shaded area
                    g <- g + annotate("rect", 
                                      xmin=as.numeric(postOnX), 
                                      xmax=as.numeric(postOffX), 
                                      ymin=postOnY, 
                                      ymax=postOffY, 
                                      color="black",
                                      size=.3,
                                      alpha=.2, 
                                      fill="green")
                    
                  } # end if !is.na
                } # end if showMeasurements for PLE
              } # end if inclPLE
          
        } # end giant if to plot measurements for measured segments
        
        ######################### plot appearance #####################
        
        g <- g + ylab("y-change")
        g <- g + xlab("x-time")
        
        g <- g + ggtitle(plotTitle) + 
          theme(plot.title = element_text(size=8, color="#666666", face="bold", hjust=0))
        
        g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(segmentDF),150))
        
        # print the chart to the graphics device
        print(g)
        
        #######################
        
      } # end loop over l event stimulus segments in each chart
      
    } # end iteration over k charts
    
  } # end iteration over j series 
  
  if(printPlot == TRUE) { 
    graphics.off()
    dev.new() } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

# reset the NCCA ASCII init 
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)



