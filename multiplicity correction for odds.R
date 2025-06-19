# multiplicity correction for odds
# Sept 6, 2021
# Raymond Nelson
####

mutiplicityOddsFn <- function(odds=1.38, n=3, inv=FALSE) {
  # R function to calculate a multiplicity correction for an odds ratio
  # odds can be a whole number or decimal
  # n is the number of simultaneous odds or decisions
  # inv will invert the calculation
  ###
  # p <- odds / (1 + odds)
  # mp <- 1- ( 1 + exp( log(p /(1-p)) / n ) )^-1
  if(isTRUE(inv)) n <- 1/n
  mp <- 1 - ( 1 + exp( log(odds) / n ) )^-1
  oddsM <- mp / (1-mp)
  return(oddsM)
}



p = .4201
n = 3

1 - 1/ ( 1+exp(log(p /(1-p)) / n) )
# [1] 0.4731622

# same as 

1 - ( 1+exp(log(p /(1-p)) / n) )^-1
# [1] 0.4731622

(1-0.4731622) / (0.4731622)
[1] 1.11344

1 - ( 1+exp(log((1-p) /p) / n^-1) )^-1
# [1] 0.7245388

0.7245388 / (1-0.7245388)
# [1] 2.630275



