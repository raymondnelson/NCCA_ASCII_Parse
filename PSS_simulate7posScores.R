#### simulate the permutation of 7 position scores for 3RQs and 3 charts ####

# was part of the PSSModel.R script until May 18, 2020


standardizeFn <- function(x) {
  xSD <- sd(x)
  xMean <- mean(x)
  (x - xMean) / xSD
}

# initialize a million random log ratios
simDAT <- standardizeFn(log((runif(5*10^6) / runif(5*10^6))))

# exclude extreme values
# simDAT[abs(simDAT) > 3.49] <- NA
simDAT[abs(simDAT) > 3.49] <- 3.49 * sign(simDAT[which(abs(simDAT) > 3.49)])

which(is.na(simDAT)) # less than 1%

# convert to 7 position integer scores
simDAT <- trunc(simDAT + sign(simDAT) * .5)

library("dplyr")
simDAT2 <- count(data.frame(score=na.omit(simDAT)), score)

# calculate the number of scores
n <- sum(simDAT2$n)

simDAT2$pmf <- simDAT2$n/n

# plot(simDAT2$pmf)

# simDAT2$pmf can be a weighting vector for a 7 position multinomial 


####### simulate the multinomial distribution of uniform septile scores #######

nSim <- 30*10^6

# nSim <- 100000

# nScores <- 80 # 4 sensors * 4 RQs * 5 charts
nScores <- 27 # 3 sensors * 3 RQs * 3 charts

# maxN <- 3 * 4 * 3 * 3 # RQs * pts * charts * maxScore # 3 chart FZCT
# maxN <- 4 * 5 * 5 * 3 # 5 chart Raskin technique with vasomotor

# minN <- -maxN


# initialize a matrix by sampling from the 7 position scale
simDAT7 <- as.data.frame(matrix(nrow=nSim, ncol=nScores))
scoreVc <- c(-3:3)
for(i in 1:ncol(simDAT7)) {
  simDAT7[,i] <- sample(scoreVc, size=nSim, replace=TRUE)
  print(i)
}
# simDAT7 <- data.frame(simDAT7)
names(simDAT7) <- paste0("x", 1:nScores)

# double the values for 9 EDA columns for weighted EDA scores of FZCT exams
# 20 EDA columns for max Raskin technique with vasomotor

# simDAT7[,c(1:20)] <- simDAT7[,1:20] * 2
simDAT7[,c(1:9)] <- simDAT7[,1:9] * 2

# calculate the total score
simDAT7$permutationScore <- apply(simDAT7, 1, sum)

# count the number of occurrences for each permutation score
library("dplyr")
simDAT7Dist <- count(simDAT7, permutationScore)

# check the max and min values
maxVal <- max(simDAT7Dist$permutationScore)
minVal <- min(simDAT7Dist$permutationScore)
diffVal <- maxVal - abs(minVal)

while(diffVal != 0) {
  thisMax <- which.max(c(maxVal, abs(minVal)))
  for(i in 1:length(diffVal)) {
    if(thisMax == 1) {
      simDAT7Dist <- rbind(c((minVal - 1), 0), simDAT7Dist)
    } else {
      simDAT7Dist <- rbind(simDAT7Dist, c((maxVal + 1), 0))
    }
  }
  maxVal <- max(simDAT7Dist$permutationScore)
  minVal <- min(simDAT7Dist$permutationScore)
  diffVal <- maxVal - abs(minVal)
}

simDAT7Dist$n <- ceiling(rowMeans(cbind(simDAT7Dist$n, rev(simDAT7Dist$n))))


maxVal <- max(simDAT7Dist$permutationScore)


# initialize an output data frame
simDAT7Dist2 <- data.frame(permutationScore=seq(-maxVal, maxVal, by=1))


findCount <- function(x, y=simDAT7Dist) {
  y$n[y$permutationScore == x]
}

# call a function to populate the data frame n (count) for each score
simDAT7Dist2$n <- as.numeric(apply(simDAT7Dist2[1], 1, findCount))

# use 1 instead of 0 or NA
simDAT7Dist2$n[which(is.na(simDAT7Dist2$n))] <- 1
simDAT7Dist2$n[which(simDAT7Dist2$n==0)] <- 1


## recall a previous output ##
# simDAT7Dist2 <- PSSModel7x27[,c(2:3)]

## extend the range to the max ##
maxVal <- max(simDAT7Dist2$permutationScore)
minVal <- min(simDAT7Dist2$permutationScore)
while(maxVal < 108 || minVal > -108) {
  thisMax <- which.max(c(maxVal, abs(minVal)))
  thisMin <- which.max(c(maxVal, abs(minVal)))
  if(thisMax < 108) {
    simDAT7Dist2 <- rbind(simDAT7Dist2, c((maxVal + 1), 1))
    maxVal <- max(simDAT7Dist2$permutationScore)
  }
  if(thisMin > - 108) {
    simDAT7Dist2 <- rbind(c((minVal - 1), 1), simDAT7Dist2)
    minVal <- min(simDAT7Dist2$permutationScore)
  }
}




# invert the scale
simDAT7Dist2$nRev <- rev(simDAT7Dist2$n)

# smooth it by averaging the n and nRev
simDAT7Dist2$nSmooth <- apply(simDAT7Dist2[2:3], 1, mean, na.rm=TRUE)

# calculate the probability mass function
simDAT7Dist2$pmf <- 
  round( simDAT7Dist2$nSmooth / sum(simDAT7Dist2$nSmooth), 9)

# simDAT7Dist2$pmf <- simDAT7Dist2$pmf / sum(simDAT7Dist2$pmf, na.rm=TRUE)

colSums(simDAT7Dist2, na.rm=TRUE)

# calculate the cumulative distribution function
simDAT7Dist2$cdf <- round( cumsum(simDAT7Dist2$pmf) , 9)

# calculate the continuity correction
simDAT7Dist2$cdfContCor <- 0
for(i in 2:nrow(simDAT7Dist2)) {
  if(simDAT7Dist2$permutationScore[i] <= 0) {
    simDAT7Dist2$cdfContCor[i] <- round(mean(c(simDAT7Dist2$cdf[(i-1):i])), 9)
  } else {
    # use the lower tail for scores > 0
    simDAT7Dist2$cdfContCor[i] <- 1 - round(mean(c(simDAT7Dist2$cdf[(i-1):i])), 9)
  }
}

# correct the first row
simDAT7Dist2$cdfContCor[1] <- round(simDAT7Dist2$cdfContCor[2] / 2, 9)

# PSS_Model_7x27b <- simDAT7Dist2

# View(simDAT7Dist2)


# write.csv(simDAT7Dist2, file="PSSModel7x27.csv")



