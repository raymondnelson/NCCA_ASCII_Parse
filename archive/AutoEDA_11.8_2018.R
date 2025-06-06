
lowPass12hz <- function(x, GAIN = 1.32491969623291e+000, zplane = -0.50952544949443) {
  # lowPass12hz()
  # low pass filter to improve the diagnostic coeficient of the Auto EDA data
  # will add the AutoEDA column to the data frame
  # first order Butterworth filter
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass12hz function


highPass.0159hz <- function(x, GAIN = 1.00166504564511e+000, zplane = 0.99667544424686) {
  # highPass.0159()
  # high pass filter to auto center the Auto EDA
  # to improve the visual appearance and diagnostic coeficient of the EDA data
  # auto centering filter
  # first order Butterworth filter
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end highPass.0159 function


lowPass.443hz <- function(x, GAIN = 2.254050840e+01, zplane = 0.9112708567) {
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
} # end lowpass.443 function


