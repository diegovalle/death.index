source("app/clean-database.R")
source('lib/boot.R')
#source('lib/run_tests.R')


source("app/summary-stats.R")
source("app/label-charts.R")

#Stats for all of Mexico
#Only presumed homicides
hom <- subset(deaths, PRESUNTO == 2)
llcharts <- generateCharts(hom, 2008, "México")
saveCharts(llcharts, "mexico")

#Stats for Ciudad Juárez
hom.juarez <- subset(hom, ENTOCU %in% c(08) &
                          MUNOCU %in% c(037))
llcharts.j <- generateCharts(hom.juarez, 2008, "Cd. Juárez")
llcharts.j <- labelJuarez(llcharts.j)
saveCharts(llcharts.j, "juarez")

#Stats for Tijuana
hom.tj <- subset(hom, ENTOCU %in% c(02) &
                          MUNOCU %in% c(004))
llcharts.tj <- generateCharts(hom.tj, 2008, "Tijuana")
saveCharts(llcharts.tj, "tijuana")


#It might be worthwhile to include other states, counties, and
#also presumed homicides + violent deaths of unknown intent
#hom <- subset(hom, SEXO == 2)
#hom <- subset(hom, ENTOCU %in% c(19, 28))
#hom <- subset(hom, ENTOCU %in% c(9, 12, 15, 16, 17))
#hom <- subset(hom, ENTOCU %in% c(2, 8, 10, 18, 25, 26))
#005,016

#Small Multiple of State Rates
source("app/state-rates.R")
#Small Multiple of Municipality Rates
source("app/municipality-rates.R")
#Choropleths
source("app/choropleths-rates.R")
#Clustering of Municipalities with similar homicide patterns
#source("app/cluster.R")


