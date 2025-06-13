# PCASS PLE filter



highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.035475913e+00, zplane = 0.9314790191) {
  # high pass filter at .338628Hz
  # to constrain the PLE data to a stable baseline
  # first order Butterworth filter
  ####
  xv1 <- data[1]
  yv1 <- 0
  output <- rep(NA, length(data))
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
}