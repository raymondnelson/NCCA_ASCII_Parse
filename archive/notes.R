use saveRDS() not save because it is serialized and can be loaded to a different name

then use readRDS()

## save a single object to file
saveRDS(women, "women.rds")
## restore it under a different name
women2 <- readRDS("women.rds")

