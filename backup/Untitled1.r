ls(pattern = "eventList?")
c(eventList1, eventList2, eventList3, eventList4)
length(ls(pattern = "eventList?"))

l <- ls(pattern = "eventList?")
lapply(l, get)
c(sapply(l, get))


c(lapply(l, get))
m <- sapply(l, get, simplify = "vector")
m <- sapply(l, get)

