
message("Saving injury intent data to csv and RData")
write.csv(deaths, file = bzfile(file.path("clean-data", "injury.intent.csv.bz2")),
          row.names = FALSE)
injury.intent <- deaths
injury.intent$date_reg <- NULL
injury.intent$date_occur <- NULL
injury.intent$month <- NULL
save(injury.intent,
     compress = "bzip2",
     file = file.path("clean-data", "injury.intent.RData"))
## resaveRdaFiles(file.path("clean-data", "injury.intent.RData"))

##sapply(deaths, class)
##deaths$age[1:5]
