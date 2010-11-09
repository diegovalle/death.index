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


chartRegion(hom, c(08), c(037), 2008,
            "Juárez")
chartRegion(hom, c(08), c(2, 4, 19), 2008,
            "Chihuahua")
chartRegion(hom, c(02), c(004, 003, 005), 2008,
            "Tijuana (MA)")  
chartRegion(hom, c(25), c(006), 2008,
            "Culiacán")  
chartRegion(hom, c(25), c(012), 2008,
            "Mazatlán")  
chartRegion(hom, c(12), c(001, 021), 2008,
            "Acapulco")
chartRegion(hom, c(28), c(027), 2008,
            "Nuevo Laredo")
chartRegion(hom, c(28), c(022), 2008,
            "Matamoros")  
chartRegion(hom, c(28), c(032, 033), 2008,
            "Reynosa")
chartRegion(hom, c(26), c(043), 2008,
            "Nogales")
chartRegion(hom, c(19),
            c(6, 9, 018, 19, 21, 26, 31, 39, 45, 46, 48, 49),
            2008,
            "Monterrey")

chartRegion(hom, c(10), c(005), 2008,
            "durango")
hom.lag <- subset(hom, (ENTOCU == 5 & MUNOCU %in% c(17,35)) |
              (ENTOCU == 10 & MUNOCU %in% c(7,12)))
llcharts <- generateCharts(hom.lag, 2008, "La Laguna")
saveCharts(llcharts, "laguna")

hom.ver <- subset(hom, ENTOCU == 30)
llcharts <- generateCharts(hom.ver, 2008, "Veracruz (State)")
saveCharts(llcharts, "veracruz")

hom.dur <- subset(hom, ENTOCU == 10)
llcharts <- generateCharts(hom.dur, 2008, "Durango (State)")
saveCharts(llcharts, "durango (state)")

hom.zac <- subset(hom, ENTOCU == 32)
llcharts <- generateCharts(hom.zac, 2008, "Zacatecas (State)")
saveCharts(llcharts, "zacatecas (state)")

#chartRegion(hom, c(12), c(001, 021), 2008,
#            "Lázaro Cárdenas")  

#It might be worthwhile to include other states, counties, and
#also presumed homicides + violent deaths of unknown intent
#hom <- subset(hom, SEXO == 2)
#hom <- subset(hom, ENTOCU %in% c(19, 28))
#hom <- subset(hom, ENTOCU %in% c(9, 12, 15, 16, 17))
#hom <- subset(hom, ENTOCU %in% c(2, 8, 10, 18, 25, 26))

#Small Multiple of State Rates
source("app/state-rates.R")
#Small Multiple of Municipality Rates
source("app/municipality-rates.R")
#Choropleths
source("app/choropleths.R")
#Clustering of Municipalities with similar homicide patterns
#source("app/cluster.R")


