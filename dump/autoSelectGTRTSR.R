# R script to auto-select the GTR or TSR decision rules
# Raymond Nelson
# 
# used by the ESS-M algorithm
# 
# works with integer scores
#
####



######### auto select the TSR or SSR for ESS-M ###########



autoSelectTSRSSRFn <- function(x=c(1,2,3, 4), PLE=TRUE, alpha=.01) {
  # R function to auto-select the TSR or SSR for ESS-M classifications
  # using pairwise comparison of all RQs 
  # and a binomial test of proportions
  # works only with integer scores and integer cutscores
  # Feb 12, 2019
  # Raymond Nelson
  #
  # x input is a vector of named numbers for RQ subtotal scores
  # names of items in x are the RQ names
  # n input is the max abs subtotal score (charts x sensors) with double EDA
  # alpha is the level of significance - use .01 without correction
  # 
  # output is a vector of 2 values
  # sig or ns
  # level of significance
  #
  # uses another function for the pairwise comparison of all RQ pair
  # 
  ###
  nRQ <- length(x)
  if(nRQ < 2) return() 
  ###
  # initialize the likelihood table
  # probLookup20DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup20DF.csv", stringsAsFactors=FALSE)
  # probLookup25DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup25DF.csv", stringsAsFactors=FALSE)
  # select the likelihood distribution
  if(isTRUE(PLE)) {
    probLookupDF <- probLookup25DF
  } else {
    probLookupDF <- probLookup20DF
  }
  ###
  # initialize a vector
  probX <- rep(NA, length(x))
  # lookup the probabilities for x
  for(i in 1:length(probX)) {
    probX[i] <- probLookupDF$probLookup[which(probLookupDF$score == x[i])]
  }
  # get the max possible integer score for 5 charts
  maxVal <- ifelse(isTRUE(PLE), 25, 20)
  # estimate the values for the input
  xVal <- round(maxVal * probX,0)
  # calculate the differences
  diffVal <- maxVal - xVal
  # calucate the sums
  sumX <- xVal + diffVal
  ###
  # calculate the number of pairwise comparisions
  numPairs <- choose(nRQ,2)
  # initialize the output vector
  xOut <- rep(NA, times=numPairs)
  # set a counter
  k = 1
  # then iterate over the RQ pairs
  i=1; j=2
  for(i in 1:(nRQ-1)) {
    for(j in (i+1):nRQ) {
      # binomial test of proportions
      xProp1 <- (sumX[i] - xVal[i]) / sumX[i]
      xProp2 <- (sumX[j] - xVal[j]) / sumX[j]
      x12Inv <- (xVal[i] + xVal[j]) / (sumX[i] + sumX[j])
      x12Prod <- (1 - x12Inv) * x12Inv
      sqrSum12 <- sqrt(abs(x12Prod / sumX[i] + x12Prod / sumX[j]))
      xPropDiff <- xProp1 - xProp2
      zVal <- -abs(xPropDiff / sqrSum12)
      xOut[k] <- pnorm(zVal)
      k <- k + 1
    }
  }
  # check which pairwise comparisons are significant
  sigVals <- which(xOut <= alpha)
  pairwiseP <- min(xOut)
  # parse the result
  if(length(sigVals) != 0) {
    pairwiseResult <- "sig"
  } else {
    pairwiseResult <- "ns"
  }
  # select the decision rule
  useRule <- ifelse(pairwiseResult=="sig", "SSR", "TSR")
  # output
  return(c(pairwiseResult=pairwiseResult, p=pairwiseP, rule=useRule)) 
}


####### likelihood functions for pairwise analysis ###########

# initialize a data frame for the probability lookup subtotal with PLE
# subtotal table from Nelson (2017)
probLookup25DF <- cbind.data.frame(score=-15:15,probLookup=c(0.0006589, 
                                                             0.001464, 
                                                             0.003036, 
                                                             0.005916, 
                                                             0.01088,
                                                             0.01899,
                                                             0.03153,
                                                             0.04997,
                                                             0.07583,
                                                             0.1104,
                                                             0.1546,
                                                             0.2087,
                                                             0.272,
                                                             0.3432,
                                                             0.4201,
                                                             0.5,
                                                             0.5799,
                                                             0.6568,
                                                             0.728,
                                                             0.7913,
                                                             0.8454,
                                                             0.8896,
                                                             0.9242,
                                                             0.95,
                                                             0.9685,
                                                             0.981,
                                                             0.9891,
                                                             0.9941,
                                                             0.997,
                                                             0.9985,
                                                             0.9993 ) )

# write.csv(probLookup25DF, file=,"probLookup25DF.csv", row.names=FALSE)

# probLookup25DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup25DF.csv", stringsAsFactors=FALSE)



# initialize a data frame for the probability lookup subtotal w/o PLE
# subtotal table from Nelson (2017)
probLookup20DF <- cbind.data.frame(score=-14:14,probLookup=c(0.0005074,
                                                             0.001283,
                                                             0.002937,
                                                             0.006171,
                                                             0.01202,
                                                             0.02188,
                                                             0.03748,
                                                             0.06068,
                                                             0.09328,
                                                             0.1367,
                                                             0.1914,
                                                             0.2571,
                                                             0.3322,
                                                             0.4143,
                                                             0.5,
                                                             0.5857,
                                                             0.6678,
                                                             0.7429,
                                                             0.8086,
                                                             0.8633,
                                                             0.9067,
                                                             0.9393,
                                                             0.9625,
                                                             0.9781,
                                                             0.988,
                                                             0.9938,
                                                             0.9971,
                                                             0.9987,
                                                             0.9995 ) )

# write.csv(probLookup20DF, file=,"probLookup20DF.csv", row.names=FALSE)

# probLookup20DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup20DF.csv", stringsAsFactors=FALSE)
# probLookup25DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup25DF.csv", stringsAsFactors=FALSE)


