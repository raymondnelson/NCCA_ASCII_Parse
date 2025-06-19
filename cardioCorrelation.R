# R script to calculate the correlation of the cardio and FC cuff
# Feb 26, 2023
# Raymond Nelson


cardioCorFn <- function(x=chartDF, cardio=NULL, FC=NULL, lastSample=5000, trend=FALSE) {
  # R script to calculate the correlation of the cardio and FC cuff
  # Feb 26, 2023
  # Raymond Nelson
  ###
  # x input is the chartDF
  # cardio input allows 
  # output is the correlation
  ###
  
  thisChart <- x
    
  {
    if(!exists("cardio")) cardio <- NULL
    if(!exists("FC")) FC <- NULL
    if(!exists("lastSample")) lastSample <- 5000
    if(!exists("trend")) trend <- FALSE
  }
  
  # initialized the result vector
  corDAToffset <- NULL
  
  # number of offset samples before and after 
  n <- 45
  
  # a vector to hold the sequence of offset correlations
  corDATcase <- rep(NA, times=(2*n))
  
  # cardio data
  if(is.null(cardio)) {
    cardioDAT <- thisChart$c_CardioMA
  } else {
    thisCol <- which(names(thisChart) == cardio)
    cardioDAT <- thisChart[,thisCol]
  }
  if(trend) {
    cardioDiff <- ( cardioDAT[1] - cardioDAT[length(cardioDAT)] )
    # cardioDiff <- ( cardioDAT[1] - cardioDAT[length(cardioDAT)] ) / length(cardioDAT)
    # cardioTrend <- cumsum(rep(cardioDiff, times=length(cardioDAT)))
    # cardioDAT <- cardioDAT + cardioTrend
  }

  # finger cuff data
  if(is.null(FC)) {
    FCDAT <- thisChart$c_FCMA
  } else {
    thisFCCol <- which(names(thisChart) == FC)
    FCDAT <- thisChart[,thisFCCol]
  }
  if(trend) {
    FCDiff <- ( FCDAT[1] - FCDAT[length(FCDAT)] )
    # FCDiff <- ( FCDAT[1] - FCDAT[length(FCDAT)] ) / length(FCDAT)
    cardFCDiff <- (cardioDiff - FCDiff) / length(FCDAT)
    # FCTrend <- cumsum(rep(FCDiff, times=length(FCDAT)))
    FCTrend <- cumsum(rep(cardFCDiff, times=length(FCDAT)))
    FCDAT <- FCDAT + FCTrend
  }
  
  # plot.ts(cardioDAT)
  # plot.ts(FCDAT)
  
  j=1
  for(j in 1:(2*n)) {
    FCDATa <- FCDAT[(500-n+j):(lastSample-n+j)]
    corDATcase[j] <- cor(cardioDAT[500:lastSample], FCDATa, use="complete.obs")
  }
  
  return(max(corDATcase)) 

}

# cardioCorFn()


