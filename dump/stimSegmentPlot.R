# function to plot a stimulus segment
#
#
#
##############################################

library(stringr)

# call the setRange.R function first



# PF090316_1.01A_eventList

# source('~/Documents/R_programming/NCCA_ASCII_Parse/centerData.R', echo=TRUE)

# source('~/Documents/R_programming/NCCA_ASCII_Parse/DSP_filters.R', echo=TRUE)

# source('~/Documents/R_programming/NCCA_ASCII_Parse/setRange.R', echo=TRUE)

####

# segEvents <- as.numeric(eventData[6,5:7])
# names(segEvents) <- c("onsetRow", "offsetRow", "answerRow")

# stimSegment <- chartData[(segEvents[1]-30*5):(segEvents[1]+30*25),]

####

chartNames <- unique(str_sub(ls(pattern="_dataSegmentList"), 1, -17))
chartNames <- chartNames[1]

stimSegment <- get(paste0(chartNames, "_dataSegmentList"), pos=1) 
# stimSegment is a data frame containing the time series data for a stimulus segment
stimSegment <- stimSegment[[6]]

segEvents <- get(paste0(chartNames, "_eventList"), pos=1) 
# segEvents is a dataframe of events containing the 
# onset offset and answer row for each stimulus events
segEvents <- segEvents[[6]]

# Row end period == 5 seconds after answer
# ROWEndPer <- 5
# samples per second = 30
# cps <- 30

####



# input is currently 2 data frames: stimSegment and segEvents
# need to process input as 2 lists 

stimSegmentPlot <- function(x=stimSegment, 
                            events=segEvents, 
                            yOffset=c(125,50,0,-75), 
                            cps=30, 
                            lat=.5, 
                            mPer=15,
                            prestimPer=5,
                            ROWEndPer=5,
                            plotTitle=segEvents$Label) {
  # function to plot a polygraph chart
  #
  # x is a data frames containing the time series data
  # events is a data frame of stimulus events
  #
  # yOffset is a vector of offset values for the plot
  # events is a vector of 3 numeric row numbers (stim onset, stim offset, answer) 
  # cps is the sampling rate in cycles per second
  # lat is the latency value in seconds for measurement after stimulus onset
  # mPer is the measurement period in seconds
  # prestimPer is the prestimulus period in seconds
  # ROWEndPer is the response onset window length in seconds after answer
  #
  # output is a plot object
  
  ####
  
  library(ggplot2) # need to load grid library for arrows
  library(grid) # for ggplot2 arrows
  
  stimSegment <- x
  segEvents <- events
  
  # these next 2 are needed because ggplot2 is called in the global envir
  # and will not find these in the function environment
  assign(x="ROWEndPer", value=ROWEndPer, pos=1)
  assign(x="cps", value=cps, pos=1)
  
  #loop over the stimSegment and segEvents for each stimulus in the chart
  # i <- 1
  # for (i in 1:length(stimSegmentsList)) {
#     stimSegment <- stimSegmentsList[[i]]
#     print(str(stimSegment))
#     segEvents <- segEvents[i,]
    
    examName <- unique(stimSegment$examName)
    chartName <- unique(stimSegment$chartName)
    
    # set the scale for the sensor types
    stimSegment$UPneumoS <- stimSegment$UPneumoS * 1
    stimSegment$LPneumoS <- stimSegment$LPneumoS * 1
    stimSegment$AutoEDA <- stimSegment$AutoEDA * 1
    stimSegment$Cardio1 <- stimSegment$Cardio1 * 10
    
    # set the offset for the sensor data
    UPneumoOffset <- stimSegment$UPneumoS[1]
    LPneumoOffset <- stimSegment$LPneumoS[1]
    AutoEDAOffset <- stimSegment$AutoEDA[1]
    Cardio1Offset <- stimSegment$Cardio1[1]
    stimSegment$UPneumoS <- stimSegment$UPneumoS - UPneumoOffset + yOffset[1]
    stimSegment$LPneumoS <- stimSegment$LPneumoS - LPneumoOffset + yOffset[2]
    stimSegment$AutoEDA <- stimSegment$AutoEDA - AutoEDAOffset + yOffset[3]
    stimSegment$Cardio1 <- stimSegment$Cardio1 - Cardio1Offset + yOffset[4]
    
    #  row numbers for measurement and plotting the current segment
    print(examName)
    print(chartName)
    prestimRow <- segEvents[2]-stimSegment$Sample[1]-prestimPer*cps
    onsetRow <- segEvents[2]-stimSegment$Sample[1]
    latencyRow <- segEvents[2]-stimSegment$Sample[1]+lat*cps
    offsetRow <- segEvents[3]-stimSegment$Sample[1]
    answerRow <- segEvents[4]-stimSegment$Sample[1]
    ROWEndRow <- segEvents[4]-stimSegment$Sample[1]+ROWEndPer*cps
    endRow <- nrow(stimSegment)
    
    EDAEvents <- unlist(c(prestimRow, onsetRow, latencyRow, offsetRow, answerRow, ROWEndRow, endRow))
    names(EDAEvents) <- c("prestimRow", "onsetRow", "latencyRow", "offsetRow", "answerRow", "ROWEndRow", "endRow")
    
    CardioEvents <- unlist(c(prestimRow, onsetRow, latencyRow, offsetRow, answerRow, ROWEndRow, endRow))
    names(CardioEvents) <- c("prestimRow", "onsetRow", "latencyRow", "offsetRow", "answerRow", "ROWEndRow", "endRow")
    
    
    # Pneumo measurement
    source('~/Documents/R_programming/NCCA_ASCII_Parse/Pneumo_extract.R', echo=TRUE)
    P1 <- pneumoExtract(stimSegment$LPneumoS[150:600])
    P2 <- pneumoExtract(stimSegment$UPneumoS[150:600])
    print(c("P1", P1))
    print(c("P2", P2))
    
    # EDA measurement
    source('~/Documents/R_programming/NCCA_ASCII_Parse/EDA_extract.R', echo=TRUE)
    autoEDA <- EDAExtract(x=stimSegment$AutoEDA[0:600], y=EDAEvents)
    # fix the offset for plotting
    autoEDA["responseOnsetValue"] <- autoEDA["responseOnsetValue"] - AutoEDAOffset
    autoEDA["responsePeakValue"] <- autoEDA["responsePeakValue"] - AutoEDAOffset
    print(autoEDA)
    
    # Cardio measurement
    source('~/Documents/R_programming/NCCA_ASCII_Parse/Cardio_extract.R', echo=TRUE)
    # cardio1 <- cardioExtract(x=stimSegment$Cardio1[0:600], y=CardioEvents)
    
    
    ##### make the plot
    g <- ggplot(environment = environment())
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=AutoEDA), color="green4")
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Cardio1), color="red")
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=UPneumoS), color="blue1")
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=LPneumoS), color="blue4")
    # g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Move1), color="grey20")
    # g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Aux02), color="grey20")
    
    ### vertical lines 
    # onset line
    g <- g + geom_vline(aes(xintercept=as.numeric(segEvents[2]-stimSegment$Sample[1])))
    # offset line
    g <- g + geom_vline(aes(xintercept=as.numeric(segEvents[3]-stimSegment$Sample[1])))
    # answer line
    g <- g + geom_vline(aes(xintercept=as.numeric(segEvents[4]-stimSegment$Sample[1])), color="grey40")
    # end of response onset window
    g <- g + geom_vline(aes(xintercept=as.numeric(segEvents[4]-stimSegment$Sample[1]+ROWEndPer*cps)), color="grey70")
    
    ### shaded areas
    # scoring window shaded area
    g <- g + annotate("rect", 
                      xmin=as.numeric(segEvents[2]-stimSegment$Sample[1]), 
                      xmax=as.numeric(segEvents[2]+(cps*mPer)-stimSegment$Sample[1]), 
                      ymin=-150, 
                      ymax=150, 
                      alpha=.1, 
                      fill="blue")
    # stimulus question shaded area
    g <- g + annotate("rect", 
                      xmin=as.numeric(segEvents[2]-stimSegment$Sample[1]), 
                      xmax=as.numeric(segEvents[3]-stimSegment$Sample[1]), 
                      ymin=-135, 
                      ymax=135, 
                      alpha=.3, 
                      fill="grey20")
    
    # Pneumo measurement lines
    
    g <- g + geom_line(data=stimSegment[150:600,], aes(x=150:600, y=UPneumoS), color="blue1", size=1.25)
    g <- g + geom_line(data=stimSegment[150:600,], aes(x=150:600, y=LPneumoS), color="blue4", size=1.25)

    # EDA measurement lines
    
    yOffset <- stimSegment$AutoEDA[1]
    
    # horzontal segment measurement
    g <- g + annotate("segment",
                      x=autoEDA["responsePeakRow"],
                      xend=autoEDA["responseOnsetRow"],
                      y=autoEDA["responseOnsetValue"],
                      yend=autoEDA["responseOnsetValue"],
                      color="purple",
                      size=.5,
                      arrow=arrow(length=unit(0.4, "cm")))
    # vertical segment
    # geom_segment and annotate are two different ways of adding the lines
    # annotate is probably better
    g <- g + annotate("segment",
                      x=autoEDA["responsePeakRow"],
                      xend=autoEDA["responsePeakRow"],
                      y=autoEDA["responseOnsetValue"],
                      yend=autoEDA["responsePeakValue"],
                      color="purple",
                      size=.5,
                      arrow=arrow(length=unit(0.4, "cm")))
    #   g <- g + geom_segment(aes(x=autoEDA["responsePeakRow"], 
    #                             xend=autoEDA["responsePeakRow"], 
    #                             y=autoEDA["responseOnsetValue"], 
    #                             yend=autoEDA["responsePeakValue"]), 
    #                         color="purple", 
    #                         size=.5, 
    #                         arrow=arrow(length=unit(0.4, "cm")))
    
    ### plot appearance
    g <- g + ylab("y-change")
    g <- g + xlab("x-time")
    
    g <- g + ggtitle(paste(examName, chartName, plotTitle))
    
    g <- g + theme_bw()
    
    return(g)
  
#  } # end for loop over stim segments
  
} # end stimSegmentPlot

# call the function
stimSegmentPlot(x=stimSegment, 
                events=segEvents, 
                yOffset=c(125,50,0,-75), 
                cps=30, 
                lat=.5, 
                mPer=15,
                prestimPer=5,
                ROWEndPer=5,
                plotTitle=segEvents$Label)



####


# call the function recursively 


# stimSegmentPlotRecurse <- function(x=stimSegment, y=segEvents) {
#   # function to call the stimSegmentPlot recursively
#   # for all charts in the global environment
#   # x is a list containing the measurement segments for all test stimuli
#   # y is a data frame containing the Label along with the Begin End and Answer rows
#   # for all test stimuli
#   
#   ####
#   
#   stimSegmentsList <- x
#   segEventsDF <- y
#   
#   # loop over the list of stimulus segments
#   # i <- 1
#   for (i in 1:length(stimSegmentsList)) {
#     stimSegment <- stimSegmentsList[[i]]
#     segEvents <- segEventsDF[i,]
#     
#     # call the function to make the plot
#     
#     stimSegmentPlot(x=stimSegment, 
#                     events=segEvents, 
#                     yOffset=c(125,50,0,-75), 
#                     cps=30, 
#                     lat=.5, 
#                     mPer=15,
#                     prestimPer=5,
#                     ROWEndPer=5,
#                     plotTitle=segEvents$Label)
#     
#     
#   } # end loop over stim segments list
#   
# } # end function to make stimulus plots recursively
# 
# stimSegmentPlotRecurse(x=stimSegment, y=segEvents)
# 
# 
