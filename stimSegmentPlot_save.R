# function to plot a stimulus segment
#
##############################################

library(stringr)

# call the setRange.R function first



# PF090316_1.01A_eventList

# source('~/Documents/R_programming/NCCA_ASCII_Parse/centerData.R', echo=TRUE)

# source('~/Documents/R_programming/NCCA_ASCII_Parse/DSP_filters.R', echo=TRUE)

# source('~/Documents/R_programming/NCCA_ASCII_Parse/setRange.R', echo=TRUE)


# segEvents <- as.numeric(eventData[6,5:7])
# names(segEvents) <- c("onsetRow", "offsetRow", "answerRow")

# stimSegment <- chartData[(segEvents[1]-30*5):(segEvents[1]+30*25),]

####

chartNames <- unique(str_sub(ls(pattern="_dataSegmentList"), 1, -17))
chartNames <- chartNames[1]

stimSegmentList <- get(paste0(chartNames, "_dataSegmentList"), pos=1)
segEvents <- get(paste0(chartNames, "_eventList"), pos=1)


####

stimSegmentPlot <- function(x=stimSegmentList, 
                            events=segEvents, 
                            yOffset=c(125,50,0,-75), 
                            cps=30, 
                            lat=.5, 
                            mPer=15,
                            prestimPer=5,
                            ROWEndPer=5,
                            plotTitle="PF090316_1.01A") {
  # function to plot a polygraph chart
  #
  # x is an input data frame containing the time series data
  # events is a vector of stimulus events
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
  
  stimSegmentList <- x
  segEvents <- events
  
  #loop over the stimSegment and segEvents for each stimulus in the chart
  # i <- 1
  for (i in 1:length(stimEvent))
    stimSegment <- stimSegmentList[[i]]
    
    
    
    
  # reset the scale for the sensor types
  stimSegment$UPneumoS <- stimSegment$UPneumoS * 1
  stimSegment$LPneumoS <- stimSegment$LPneumoS * 1
  stimSegment$AutoEDA <- stimSegment$AutoEDA * 1
  stimSegment$Cardio1 <- stimSegment$Cardio1 * 10
  
  # set the offset for the sensor data
  stimSegment$UPneumoS <- stimSegment$UPneumoS - stimSegment$UPneumoS[1] + yOffset[1]
  stimSegment$LPneumoS <- stimSegment$LPneumoS - stimSegment$LPneumoS[1] + yOffset[2]
  stimSegment$AutoEDA <- stimSegment$AutoEDA - stimSegment$AutoEDA[1] + yOffset[3]
  stimSegment$Cardio1 <- stimSegment$Cardio1 - stimSegment$Cardio1[1] + yOffset[4]
  
# #   # onset row for the current segment
#   sampleOne <- stimSegment$Sample[1]
#   print(sampleOne)

#   
#   onsetX <- segEvents[1]-sampleOne
#   print(onsetX)
#   offsetX <- segEvents[2]-sampleOne
#   answerX <- segEvents[3]-stimSegment$Sample[1]

prestimRow <- segEvents[1]-stimSegment$Sample[1]-prestimPer*cps

ROWEndRow <- segEvents[3]-stimSegment$Sample[1]+ROWEndPer*cps

latencyRow <- segEvents[1]-stimSegment$Sample[1]+lat*cps


  
  g <- ggplot()
  g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=AutoEDA), color="green4")
  g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Cardio1), color="red")
  g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=UPneumoS), color="blue1")
  g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=LPneumoS), color="blue4")
  # g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Move1), color="grey20")
  # g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Aux02), color="grey20")
  
  # onset line
  g <- g + geom_vline(aes(xintercept=(segEvents[1]-prestimRow)))
  # offset line
  g <- g + geom_vline(aes(xintercept=(segEvents[2]-prestimRow)))
  # answer line
  g <- g + geom_vline(aes(xintercept=(segEvents[3]-prestimRow)), 
                      color="grey40")
  
  # end of response onset window
  g <- g + geom_vline(aes(xintercept=ROWEndRow-prestimRow), color="grey70")
  
  # scoring window shaded area
  g <- g + annotate("rect", 
                    xmin=latencyRow, 
                    xmax=segEvents[1]+(cps*mPer)-stimSegment$Sample[1], 
                    ymin=-150, 
                    ymax=150, 
                    alpha=.1, 
                    fill="blue")
  
  # stimulus question shaded area
  g <- g + annotate("rect", 
                    xmin=segEvents[1]-stimSegment$Sample[1], 
                    xmax=segEvents[2]-stimSegment$Sample[1], 
                    ymin=-125, 
                    ymax=125, 
                    alpha=.3, 
                    fill="grey20")
  
  
  # horzontal segment measurement
  g <- g + annotate("segment",
                    x=165,
                    xend=600,
                    y=0,
                    yend=0,
                    color="blue",
                    size=1,
                    arrow=arrow())
  
  
  # vertical segment
  g <- g + geom_segment(aes(x = 300, 
                            y = 0, 
                            xend = 300, 
                            yend = 100), 
                        color="blue", 
                        size=1, 
                        arrow=arrow(length=unit(0.5, "cm")))
  
  
  g <- g + ylab("y-change")
  g <- g + xlab("x-time")

  g <- g + ggtitle(plotTitle)

  g <- g + theme_bw()
  
  return(g)
  
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
                plotTitle="PF090316_1.01A")




