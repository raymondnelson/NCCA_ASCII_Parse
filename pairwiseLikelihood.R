# likelihood functions for pairwise analysis
#

# initialize a data frame for the probability lookup subtotal with PLE
# subtotal table from Nelson (2017)
probLookup25DF <- cbind.data.frame(score=-15:15,probLookup=c(0.0006589, 
                                                             0.001464, 
                                                             0.003036, 
                                                             0.005916, 
                                                             0.01088,
                                                             0.01899,
                                                             0.03153,
                                                             0.04997,
                                                             0.07583,
                                                             0.1104,
                                                             0.1546,
                                                             0.2087,
                                                             0.272,
                                                             0.3432,
                                                             0.4201,
                                                             0.5,
                                                             0.5799,
                                                             0.6568,
                                                             0.728,
                                                             0.7913,
                                                             0.8454,
                                                             0.8896,
                                                             0.9242,
                                                             0.95,
                                                             0.9685,
                                                             0.981,
                                                             0.9891,
                                                             0.9941,
                                                             0.997,
                                                             0.9985,
                                                             0.9993 ) )

# write.csv(probLookup25DF, file=,"probLookup25DF.csv", row.names=FALSE)

# probLookup25DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup25DF.csv", stringsAsFactors=FALSE)



# initialize a data frame for the probability lookup subtotal w/o PLE
# subtotal table from Nelson (2017)
probLookup20DF <- cbind.data.frame(score=-14:14,probLookup=c(0.0005074,
                                                             0.001283,
                                                             0.002937,
                                                             0.006171,
                                                             0.01202,
                                                             0.02188,
                                                             0.03748,
                                                             0.06068,
                                                             0.09328,
                                                             0.1367,
                                                             0.1914,
                                                             0.2571,
                                                             0.3322,
                                                             0.4143,
                                                             0.5,
                                                             0.5857,
                                                             0.6678,
                                                             0.7429,
                                                             0.8086,
                                                             0.8633,
                                                             0.9067,
                                                             0.9393,
                                                             0.9625,
                                                             0.9781,
                                                             0.988,
                                                             0.9938,
                                                             0.9971,
                                                             0.9987,
                                                             0.9995 ) )

# write.csv(probLookup20DF, file=,"probLookup20DF.csv", row.names=FALSE)

# probLookup20DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup20DF.csv", stringsAsFactors=FALSE)
# probLookup25DF <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/probLookup25DF.csv", stringsAsFactors=FALSE)

