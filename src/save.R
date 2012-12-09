
message("Saving injury intent data to csv and RData")
write.csv(deaths, file = bzfile(file.path("clean-data", "injury-intent.csv.bz2")),
          row.names = FALSE)
save(deaths, file = file.path("clean-data", "injury-intent.RData"))


##sapply(deaths, class)
##deaths$age[1:5]
