# lowPass.443() 
# to reduce noise and improve the diagnostic coeficient of the pneumo data
# can also be used with the raw EDA data
# will also add 2 columns to the time series data frames 
# for the filtered upper and lower pneumo data
# first order Butterworth filter
#
###

# define the low pass .43 function
lowPass.443 <- function(x, GAIN = 2.254050840e+01, zplane = 0.9112708567) {
  # filter to smooth peneumo and raw EDA data 
  # to improve the diagnostic coefficient by reducing high frequency noise
  
  # to improve the 
  
  # x is a column vector from the time series 
  
  #         data <- data
  #         GAIN <- GAIN
  #         zplane <- zplane
  
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  # output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
    # output <- c(output, yv1)
  }
  return(output)
} # end lowpass .443 function

plot.ts(myCardioData[,7][1:1000])
smoothedCardio <- lowPass.443(myCardioData[,7])
plot.ts(smoothedCardio[1:1000])
smoothedCardio2 <- lowPass.443(smoothedCardio)
plot.ts(smoothedCardio2[1:1000])
smoothedCardio3 <- lowPass.443(smoothedCardio2)
plot.ts(smoothedCardio3[1:1000])


smoothCardioA1 <- 
  for (i in 1:nrow(smoothedCardio)) {
    
    smoothCardioA1 <- mean(smoothedCardio[,7][i:i+29])
  return(x)
  }

