if(file.exists(file.path("clean-data", "violent-deaths.csv.bz2"))) {
   deaths <- read.csv(file.path("clean-data", "violent-deaths.csv.bz2"))
} else
   source("lib/clean_mortality_database.R")
