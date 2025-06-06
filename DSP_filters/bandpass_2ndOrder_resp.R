# bandPass.13to1.2_2ndOrder() 
# 2nd order Butterworth Filter
# designed with the bilinear transform method
# corner frequency is the frequency at which the magnitude of the response is -3 dB
####


# .13hz to 1.2z
bandPass_2nd_respFn <- function(x, 
                         gain= 9.13874496108193e+001, 
                         z1=-0.72839963878213, 
                         z2=3.13119010377342, 
                         z3=-5.07535624928687, 
                         z4=3.67252553263943) {
  # bandpass 2nd order Butterworth filter 
  # to identify the respiration signal in EDA data
  # .13hz to 1.2hz
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
  GAIN <- gain
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
    yv4 <- (xv0 + xv4) - 2 * xv2 +
      (z1 * yv0) +
      (z2 * yv1) +
      (z3 * yv2) +
      (z4 * yv3)
    output[i] <- yv4
  }
  return(output)
} # end filter function


myData <- chartDF$c_SEProc - chartDF$c_SEProc[1]
myPlot <- bandPass.2nd(x=myData)
plot.ts(myPlot[300:600] + chartDF$c_SEProc[1])
plot.ts(myPlot[300:600] + chartDF$c_SEProc[1], ylim=c(-700,-900))
plot.ts(chartDF$c_Cardio1[300:600])
plot.ts(chartDF$c_SEProc)
plot.ts(chartDF$c_SEProc, ylim=c(-700,-900))
plot.ts(myPlot + chartDF$c_SEProc[1])
plot.ts(myPlot + chartDF$c_SEProc[1], ylim=c(-700,-900))
plot.ts(myPlot[300:450] + chartDF$c_SEProc[1], ylim=c(-700,-900))
plot.ts(myPlot[300:450])
