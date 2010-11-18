source("app/clean-database.R")
source('lib/boot.R')
#source('lib/run_tests.R')

hom <- subset(deaths, PRESUNTO == 2)

#Stats for Important cities, females, states, one armed midgets
#less than 5 years old, etc
source("app/summary-stats.R")

#Small Multiple of State Rates
source("app/state-rates.R")
#Small Multiple of Municipality Rates
source("app/municipality-rates.R")

#Choropleths
source("app/choropleths.R")
#Clustering of Municipalities with similar homicide patterns
#source("app/cluster.R")


