# bandPass.3rdOrder() 
# 2nd order Butterworth Filter
# designed with the bilinear transform method
# corner frequency is the frequency at which the magnitude of the response is -3 dB
####

# .03hz to .1hz
bandPass.3rd <- function(x, 
                         gain=2575425.20490802960000, 
                         z1= -0.97110391872449, 
                         z2=5.85471225881942, 
                         z3=-14.70779924427351, 
                         z4=19.70616326095255,
                         z5=-14.85225823024395,
                         z6=5.97028587346772) {
  # bandpass Butterworth filter to compute a stable moving avg for activity sensor data
  ###
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  xv6 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  # initialize the output vector
  output <- rep(NA, length(x))
  GAIN <- gain
  for (i in 1:length(x)) {
    # increment the xv registers
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    # next input
    xv6 <- x[i] / GAIN
    # increment the yv registers
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    # compute the new output value
    yv6 <- (xv6 - xv0) + 3 * (xv2 - xv4) +
      (z1 * yv0) +
      (z2 * yv1) +
      (z3 * yv2) +
      (z4 * yv3) +
      (z5 * yv4) +
      (z6 * yv5)
    output[i] <- yv6
  }
  return(output)
} # end filter function


myData <- chartDF$c_SEProc - chartDF$c_SEProc[1]
# myData <- chartDF$c_Cardio1 - chartDF$c_Cardio1
myPlot <- bandPass.3rd(x=myData)
plot.ts(myPlot[300:600] + chartDF$c_SEProc[1])
plot.ts(myPlot[300:600] + chartDF$c_SEProc[1], ylim=c(-900,-700))
plot.ts(chartDF$c_Cardio1[300:600])
plot.ts(chartDF$c_SEProc)
plot.ts(chartDF$c_SEProc, ylim=c(-900,-700))
plot.ts(myPlot + chartDF$c_SEProc[1])
plot.ts(myPlot[2000:3000] + chartDF$c_SEProc[1])
plot.ts(myPlot + chartDF$c_SEProc[1], ylim=c(-900,-700))
plot.ts(myPlot[300:450] + chartDF$c_SEProc[1], ylim=c(-900,-700))
plot.ts(myPlot[300:330])
