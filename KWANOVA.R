



KWANOVAFn <- function(withinChartRQMeans=withinChartRQMeans, a=.05) {
  # Kruskall-Wallace ANOVA
  # used to compare relevant questions
  # Mar 3, 2019
  # Raymond Nelson
  # 
  ####
  # calculate the Kruskall-Wallace non-parametric ANOVA
  # used by the OSS-3 screening rule
  # called by SCNFn() in the decsisionRules.R script
  #
  # input the data frame output from the withinChartRQMeans
  # winthin chart weighted mean z-scores for each RQ
  # column vectors are the weighted log(R/Cmean) scores for for the RQs of each chart
  # alpha is the one-tailed level of significance for "sig" or "ns" output
  #
  # 1. rank all the data for all questions and all charts
  #    using the mean tied rank method for equivalent values
  # 2. sum the rank scores for each RQ
  # 3. calculate the mean of the ranks for each RQ
  # 4. calculate the grand total for all ranks for all RQs
  # 5. calculate the grand mean for all RQ ranks
  # 6. calculate the squared dev for each RQ mean 
  # 7. multiply the squared deviations by the n for each RQ
  #    ng*(xg-X)^2
  #    this = between groups sum of squares for ranks
  # 8. Sum the squared deviates (line58)
  # 9. compute the mean of sum squared deviates
  #    (k-1)*(N*(N+1)/12) - line 62
  # 10. compute the number of RQs (k) - line 63
  # 11. compute the degrees of freedom k-1 (line 65)
  # 12. compute H = sum( bsSS(R) ) * sum of squared deviates^2  - this is line 64
  #
  # resulting stat is the chidist of H using df
  #
  # output is a list of 4 items, including:
  # 1) "sig" or "ns", 2)  p from chiDist, 3) H value, and 4) df
  #
  ####
  if(!exists("a")) a <- .05
  # get the scores
  allScores <- as.matrix(withinChartRQMeans[,4:ncol(withinChartRQMeans)])
  # calculate the ranks using the mean of tied ranks
  rankVals <- rank(allScores, na.last=TRUE, ties.method="average")
  # sum of all rank scores
  totalRanks <- sum(rankVals)
  # calculate the mean
  meanRank <- mean(rankVals, na.rm=TRUE)
  rankValsDF <- as.data.frame(matrix(rankVals, 
                                     byrow=FALSE, 
                                     ncol=ncol(allScores)))
  names(rankValsDF) <- names(withinChartRQMeans)[4:ncol(withinChartRQMeans)]
  # a private function to count the the n for each RQ
  countFn <- function(x=c(1, 2, NA, 3)) {
    length(x) - length(which(is.na(x)))
  }
  # call the private function to get the n for usable RQ presentations
  rqNs <- apply(rankValsDF, 2, countFn)
  names(rqNs) <- names(rankValsDF)
  # sum the ranks between charts for each RQ
  RQSumRanks <- colSums(rankValsDF, na.rm=TRUE)
  names(RQSumRanks) <- names(rankValsDF)
  # calculate the mean rank for each RQ
  RQMeanRanks <- colMeans(rankValsDF, na.rm=TRUE)
  names(RQMeanRanks) <- names(rankValsDF)
  # grand mean of RQ rank means
  grandMean <- mean(RQMeanRanks, na.rm=TRUE)
  # calculate the squared between group deviations 
  sqdDevs <- rqNs * (RQMeanRanks - grandMean)^2
  # calculate the sum of squared between group deviationS
  SSbg <- sum(sqdDevs, na.rm=TRUE)
  # number of groups
  k <- ncol(rankValsDF)
  # total N of events
  N <- sum(rqNs)
  # mean SSbg
  meanSSbg <- (k-1) * (N*(N+1)/12)
  # degrees of freedom
  df <- k - 1
  # calculate the value of H
  H <-  round(SSbg / ((N*(N+1)) / 12), 3)
  # calculate the chi-squared statistic from H
  p <- round(pchisq(H, df, lower.tail=FALSE), 3)
  # check to see if it is sig
  KWResult <- ifelse(p <= a, "sig", "ns")
  outStatement <- ifelse(KWResult=="ns",
                         "No significant differences in subtotal scores",
                         "Difference in subtotal scores is statistically significant")
  #### output ####
  # print(paste("KWResult:", ifelse(p <= a, "sig", "ns"), "p  =", p))
  list(KWResult=KWResult, p=p, H=H, df=df, a=a, KWStatement=outStatement)
}
  
  
    

