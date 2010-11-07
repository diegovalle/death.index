source("app/clean-database.R")
source('lib/boot.R')
source('lib/run_tests.R')


source("app/summary-stats.R")

#Stats for all of Mexico

hom <- subset(def, PRESUNTO == 2)

llcharts <- generateCharts(hom, 2008, "México")
saveCharts(llcharts, "mexico")


#Stats for Ciudad Juárez
hom.juarez <- subset(hom, ENTOCU %in% c(08) &
                          MUNOCU %in% c(037))
llcharts.j <- generateCharts(hom.juarez, 2008, "Cd. Juárez")

op.mich <- as.Date("2006-12-11")
op.chi <- as.Date("2008-03-27")
llcharts.j$weekly <- llcharts.j$weekly +
    geom_vline(aes(xintercept = op.chi), alpha = .7) +
    geom_vline(aes(xintercept = op.mich), alpha = .7) +
    geom_text(aes(x,y, label = "Joint Operation Chihuahua"),
            data = data.frame(x = op.chi, y = 55),
            size = 4, hjust = 1.01, vjust = 0) +
    geom_text(aes(x,y, label = "Start of the Drug War"),
            data = data.frame(x = op.mich, y = 55),
            size = 4, hjust = 1.01, vjust = 0)

saveCharts(llcharts.j, "juarez")

#Other Stats
#hom <- subset(hom, SEXO == 2)
#hom <- subset(hom, ENTOCU %in% c(19, 28))
#hom <- subset(hom, ENTOCU %in% c(9, 12, 15, 16, 17))
#hom <- subset(hom, ENTOCU %in% c(2, 8, 10, 18, 25, 26))
#005,016
