oldFileNames <- list.files(pattern="pmf.csv")
print(oldFileNames)
for (i in 1:length(oldFileNames)) {
  file.rename(oldFileNames[i], gsub("pmf.csv", "pmfI.csv", oldFileNames[i]))
}
print(list.files(pattern="measurements.csv"))

