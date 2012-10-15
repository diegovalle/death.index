if(file.exists(file.path("clean-data", "injury-intent.csv.bz2"))) {
   deaths <- read.csv(file.path("clean-data", "injury-intent.csv.bz2"))
} else
   source("lib/clean_mortality_database.R")

#Deaths with no year of occurance, narco-mines,
#too decomposed, etc
#Assume the deaths occured on the year they were registered
##deaths$ANIODEF <- as.numeric(deaths$ANIODEF)
##deaths$EDADVALOR <- as.numeric(deaths$EDADVALOR)


## kminy <- min(deaths$ANIODEF)
## kmaxy <- max(deaths$ANIODEF)
## last.year <- kmaxy
## last.day <- as.Date(str_c(kmaxy, "12", "31", sep = "-"))

## hom <- subset(deaths, PRESUNTOtxt == "Homicide")
## ddply(hom, .(ANIODEF), nrow)
## ddply(hom, .(ANIODEF, ABBRV), nrow)

