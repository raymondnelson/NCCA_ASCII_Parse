# Cardio smoothing


plot.ts(myCardioData[,7][1:3000])

cardioSmooth1 <- function(x=myCardioData[,7], y=15) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  xOut <- c(rep(x[1], times=y), numeric(length=(length(x)-y)))
  # xOut <- rep(NA, times=length(x))
  input_buffer1 <- c(x[1:y], rep(x[y+1], times=y))
  for (i in (y+1):(length(x)-(2*y))) {
    xOut[i-y] <- mean(input_buffer1)
    input_buffer1 <- c(input_buffer1[2:(2*y)], x[i])
  } # end for loop
  return(na.omit(xOut))
} # end cardioSmooth function
smoothedCardio <- cardioSmooth1(x=cardioSmooth1(x=cardioSmooth1()))
plot.ts(smoothedCardio[1:3000])



cardioSmooth3 <- function(x=myCardioData[,7], y=15) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  xOut <- c(rep(x[1], times=y), numeric(length=(length(x)-y)))
  # xOut <- rep(NA, times=length(x))
  input_buffer1 <- c(x[1:y], rep(x[y+1], times=y))
  input_buffer2 <- c(x[2*y+1:3*y], rep(x[(3*y+1)], times=y))
  input_buffer3 <- c(x[4*y+1:5*y], rep(x[(5*y+1)], times=y))
  for (i in (6*y+1):(length(x)-(2*y))) {
    xOut[i-(5*y)] <- mean(input_buffer1)
    input_buffer1 <- c(input_buffer1[2:(2*y)], mean(input_buffer2))
    input_buffer2 <- c(input_buffer2[2:(2*y)], mean(input_buffer3))
    input_buffer3 <- c(input_buffer3[2:(2*y)], x[i])
  } # end for loop
return(na.omit(xOut))
} # end cardioSmooth function
smoothedCardio <- cardioSmooth(x=myCardioData[,7], y=15)
plot.ts(smoothedCardio[1:1000])

















diastolicExtract 

x <- myCardioData[,7]

# input x is a time series vector
# normal range of pulse is 40-100 with mean 70
# 60 / 40 = 1.5
# 1.5 * 30 cps = 45
# 60 / 100 = .67
# .67 * 30 = 20
# 60 / 70 = .87
# .87 * 30 = 26



60/40
1.5/30

#input buffer
# x input is the time series cardio data

plot.ts(x[1:60])


# uses 26 samples for 30cps sample rate
# .87hz is a 
# i <- 14
xOut <- numeric(length=length(x))
input_buffer <- c(x[1:13], rep(x[14], times=14))
for (i in 14:length(x)-27) {
  input_buffer <- c(input_buffer[2:27], x[i])
  ifelse(input_buffer[14]==max(input_buffer), xOut[i-13] <- (i-13), next())
}

plot.ts(x[1:100])
xOut[1:100]

maxVal <- max(input_buffer)

x13<-x[1]; x12<-x[1]; x11<-x[1]; x10<-x[1]; x9<-x[1]; x8<-x[1] 
x7<-x[1]; x6<-x[1];x5<-x[1]; x4<-x[1]; x3<-x[1]; x2<-x[1]; x1<-x[1]
y1<-x[1]; y2<-x[1]; y3<-x[1]; y4<-x[1]; y5<-x[1]; y6<-x[1]; y7<-x[1]; 
y8<-x[1]; y9<-x[1]; y10<-x[1]; y11<-x[1]; y12<-x[1]; y13<-x[1]

x0 <- M
