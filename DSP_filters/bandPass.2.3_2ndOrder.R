# bandPass.2.3_2ndOrder() 
# 2nd order Butterworth Filter
# designed with the bilinear transform method
# corner frequency is the frequency at which the magnitude of the response is -3 dB
####


bandPass.2.3.2nd <- function(x) {
  # bandpass Butterworth filter to compute a stable moving avg for activity sensor data
  ###
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  # initialize the output vector
  output <- rep(NA, length(x))
  GAIN <- 1.04974910741543e+002
  for (i in 1:length(x)) {
  	# increment the xv registers
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    # next input
    xv4 <- x[i] / GAIN
    # increment the yv registers
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    # compute the new output value
    yv4 <- xv0 + xv4 - (2 * xv2) +
      ( -0.74365519504887 * yv0) +
      (  2.78033101084566 * yv1) +
      ( -4.32103493811738 * yv2) +
      (  3.22677892566737 * yv3)
    output[i] <- yv4
  }
  return(output)
} # end filter function


myData <- chartDF$c_SEProc - chartDF$c_SEProc[1]
myPlot <- bandPass.2.3.2nd(x=myData)
plot.ts(myPlot[300:600] + chartDF$c_SEProc[1], ylim=c(-700,-900))
plot.ts(chartDF$c_Cardio1[300:600])
plot.ts(myPlot[300:600] + chartDF$c_SEProc[1], ylim=c(-700,-900))
plot.ts((myData[300:600] + chartDF$c_SEProc[1]), ylim=c(-700,-900))
plot.ts(chartDF$c_SEProc)
plot.ts(chartDF$c_SEProc, ylim=c(-700,-900))
plot.ts(myPlot + chartDF$c_SEProc[1])
plot.ts(myPlot + chartDF$c_SEProc[1], ylim=c(-700,-900))
