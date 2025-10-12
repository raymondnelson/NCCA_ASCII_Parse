# Clopper-Pearson exact binomial confidence interval
# May 16, 2017
# Raymond Nelson
# 
####


clopperPearsonFn <- function(p=.8697865, 
                             RQs=3,
                             maxCharts=5,
                             n=9, 
                             a2=.05, 
                             PLE=TRUE,
                             # simpleESSM=TRUE,
                             odds=TRUE ) {
  # Clopper-Pearson exact binomial confidence interval
  # May 16, 2017
  # Raymond Nelson
  #
  # called by the ESSScoresFn() to compute the lower limit of posterior odds
  # 
  # p input is a decimal probablity value 
  # n input is the sample size
  # a2 is the one-tailed alpha level for the lower limit post odds
  # odds is a boolean value to obtain the output in the form of odds
  # p input should also be in the form of odds when odds=TRUE
  # 
  # ouput is a named vector with the lower limit (LL) and upper limit (UL)
  # of the Clopper-Pearson interval
  #
  # n = max abs score 
  # n = 25 for subtotals with PLE sensor
  # (1xPneumo + 2xEDA + 1xCardio + 1xPLE * 5 charts)
  # n = 20 for subtotals without the PLE sensor
  # n=100 for grand total scores with 4 RQs including the PLE sensor
  # (25 x 4RQs)
  # n=75 for grand total scores with 3 RQs including the PLE sensor
  # (25 x 3RQs)
  # n=50 for grand totals scores with 2 RQs including the PLE sensor
  # (25 x 2RQs)
  # n=80 for grand total scores with 4 RQs without the PLE sensor
  # (20 x 4RQs)
  # n=60 for grand total scores with 3 RQs without the PLE sensor
  # (20 x 3RQs)
  # n=40 for grand total scores with 2 RQs without the PlE sensor
  #
  # for PCAT with 2 sensors n = 8 for subtotal scores
  # PCAT n = 16 for grand total scores
  #
  # Clopper-Pearson interval is an exact interval based on the binomial
  # actual coverage always exceeds the nominal level
  #
  ####
  
  {
    # if(!exists("RQs")) RQs=3
    # if(!exists("maxCharts")) maxCharts=5
    if(!exists("n")) n=75
    if(!exists("a2")) a2=.05
  }
  
  
  {
    # if(p >= .5) p = 1-p
    
    # p compliment
    q <- 1 - p
    
    # 2 tailed alpha
    # a2 <- a * 2
  }
  
  {
    # degrees of freedom
    v1=trunc(round(2*(n*q+1),0))
    v2=trunc(round(2*n*p,0))
    v3=trunc(round(2*(n*p+1),0))
    v4=trunc(round(2*n*q,0))
    # truncated degrees of freedom
    # give the same result as Excel
    
    if(v1 == 0) v1 = 1
    if(v2 == 0) v2 = 1
    if(v3 == 0) v3 = 1
    if(v4 == 0) v4 = 1
  }
  
  {
    # calculate the f statistics for the upper and lower limit
    FLow <- qf((1-a2),v1,v2)
    FUp <- qf(a2,v3,v4)
  }
  
  {
    # calculate the limits
    LL <- (1+FLow*(q+1/n)/p)^-1
    UL <- (1 + q / (1 / n + p) * FUp)^-1
    
    if(isTRUE(odds)) {
      LL <- LL / (1 - LL)
      UL <- UL / (1 - UL)
    }
  }
  
  outVector <- c(lowerLimit=LL, upperLimit=UL)
  
  return(outVector)
  
  # end clopperPearsonFn()
}




# qf takes a proportion and give F
# pf takes F and gives a proportion
# df takes quantile or proportion and gives f


# n=9
# p=.8697865
# q=1-p
# v1=trunc(2*(n*q+1),0)
# v2=trunc(2*n*p,0)
# v3=trunc(2*(n*p+1),0)
# v4=trunc(2*n*q,0)
# 
# a=.1
# 
# FLow <- qf((1-(a/2)),v1,v2)
# FUp <- qf((a/2),v3,v4)
# 
# qf(.95,4,15)
# qf(.05,17,2)
# 
# # CL = [1 + F.025,v1,v2*(q+1/n)/p]^-1
# LL <- 
#   (1+FLow*(q+1/n)/p)^-1
# 
# # CU = [1 + q / (1/n+p)*F.025,v3,v4]^-1
# UL <- 
#   (1 + q / (1 / n + p) * FUp)^-1
# 
# (1 + q / (.1111111 + p) * FUp)^-1
# (1 + 0.1302135 / (.1111111 + .8697865) * 0.2784328)^-1
# 
# 1/9
# 
# print(LL)
# print(UL)
# 
# (LL/(1-LL))
# (UL/(1-UL))







# meaning:
# there is 1-a/2 probability that the likelihood of the 
# DI or NDI result is greater than chance


# http://freakonometrics.hypotheses.org/18117
# a Bayesian statistician would say 
# “given our observed data, there is a 95% probability 
# that the true value of theta falls 
# within the credible region” 
# while a Frequentist statistician would say 
# “there is a 95% probability that 
# when I compute a confidence interval 
# from data of this sort, the true value of 
# theta will fall within it”.


