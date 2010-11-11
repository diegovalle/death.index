########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Wed Nov 10 19:43:32 2010
########################################################
#Summary statistics of homicde

#Stats for all of Mexico
#Only presumed homicides
hom <- subset(deaths, PRESUNTO == 2)
llcharts <- generateCharts(hom, 2008, "México")
saveCharts(llcharts, "mexico")


#Femicides
fem <- subset(hom, SEXO == 2 )
llcharts <- generateCharts(fem, 2008, "México (Females)")
saveCharts(llcharts, "femicide")

#Mexico
fmx <- ddply(fem, .(ANIODEF), nrow) / c(53219640, 53723982, 54216256) * 100000
#US - FBI UNIFORM CRIME REPORTS
fus <- c(3156, 3177, 3078) / c(151751781, 153102214, 154446928) * 100000

fhom.rates <- data.frame(year = factor(2006:2008),
                         US = fus,
                         MX = fmx$V1)
ggplot(melt(fhom.rates, id = "year"), aes(year, value, group = variable, color = variable)) +
  geom_line() +
  opts(title = "Female Homicide Rates in the US and Mexico (2006-2008)") +
  ylab("homicide rate per 100,000 women") +
  ylim(0, max(fhom.rates$MX)) +
  scale_colour_manual("Country", values = c("blue", "green"))
ggsave("graphs/femicide-rates-us-mx.png", dpi = 100,
       width = 8, height = 6)

babies <- subset(hom, SEXO != 0 & EDADVALOR < 800)
ggplot(babies, aes(EDADVALOR, ..count.. ,group = SEXOtxt,
                   fill = SEXOtxt))+
  geom_histogram(position = "identity", binwidth = 1, alpha = .5)

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
            "Reynosa (MA)")
chartRegion(hom, c(26), c(043), 2008,
            "Nogales")
chartRegion(hom, c(19),
            c(6, 9, 018, 19, 21, 26, 31, 39, 45, 46, 48, 49),
            2008,
            "Monterrey (MA)")

chartRegion(hom, c(10), c(005), 2008,
            "Durango")
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

hom.count <- formatDaily(hom)

#chartRegion(hom, c(12), c(001, 021), 2008,
#            "Lázaro Cárdenas")  

#It might be worthwhile to include other states, counties, and
#also presumed homicides + violent deaths of unknown intent
#hom <- subset(hom, SEXO == 2)
#hom <- subset(hom, ENTOCU %in% c(19, 28))
#hom <- subset(hom, ENTOCU %in% c(9, 12, 15, 16, 17))
#hom <- subset(hom, ENTOCU %in% c(2, 8, 10, 18, 25, 26))
