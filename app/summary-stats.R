########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Wed Nov 10 19:43:32 2010
########################################################
#Summary statistics of homicde

source("app/summary-stats-functions.R")
source("app/label-charts.R")

#Stats for all of Mexico
#Only presumed homicides
year <- 2009

llcharts <- generateCharts(hom, year, "México")
llcharts$weekly <- labelChart(llcharts$weekly, "Start of Drug War",
                              300, op.mich)
llcharts$monthly <- labelChart(llcharts$monthly, "Start of Drug War",
                              1100, op.mich)
saveCharts(llcharts, "mexico")


#Femicides
fem <- subset(hom, SEXO == 2)
llcharts <- generateCharts(fem, year, "México (Females)")
saveCharts(llcharts, "femicide")

fem.j <- subset(hom, SEXO == 2 & ENTOCU == 08 & MUNOCU==037)
llcharts <- generateCharts(fem.j, year, "Juárez (Females)")
saveCharts(llcharts, "juarez-femicide")

hom.j <- subset(hom, ENTOCU == 08 & MUNOCU %in% c(037))
llcharts <- generateCharts(hom.j, year, "Juárez")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2008-03-28")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2008-03-28"), 60,
                label = "J.O. Chihuahua"),
            hjust = 1.03, vjust = 0) +
  geom_vline(aes(xintercept = as.Date("2009-03-01")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2009-03-01"), 80,
                label = "Reinforcements Sent"),
            hjust = 1.03, vjust = 0) 
saveCharts(llcharts, "juárez (ma)")
#chartRegion(hom, c(08), c(037), year,
 #           "Juárez (MA)")


chartRegion(hom, c(08), c(2, 4, 19), year,
            "Chihuahua (MA)")
chartRegion(hom, c(08), c(029), year,
            "Guadalup y Calvo")
chartRegion(hom, c(08), c(032), year,
            "Hidalgo del Parral")
chartRegion(hom, c(08), c(17), year,
            "Cuauhtemoc")


hom.can <- subset(hom, ENTOCU == 23 & MUNOCU %in% c(005))
llcharts <- generateCharts(hom.can, year, "Cancún")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2009-02-09")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2009-02-09"), 6,
                label = "J.O. Cancún"),
             hjust = 1.03, vjust = 0) 
saveCharts(llcharts, "cancún")
#chartRegion(hom, c(23), c(005), year,
 #           "Cancún")

hom.tj <- subset(hom, ENTOCU == 02 & MUNOCU %in% c(004, 003, 005))
llcharts <- generateCharts(hom.tj, year, "Tijuana (MA)")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2007-01-03")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2007-01-03"), 30,
                label = "J.O. Tijuana"),
             hjust = 1.03, vjust = 0) +
  geom_vline(aes(xintercept = as.Date("2008-10-26")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2008-10-26"), 70,
                label = "E.A.F. Captured"),
             hjust = 1.03, vjust = 0) 
saveCharts(llcharts, "tijuana")
#chartRegion(hom, c(02), c(004, 003, 005), year,
 #           "Tijuana (MA)")  


hom.cul <- subset(hom, ENTOCU == 25 & MUNOCU %in% c(006, 018))
llcharts <- generateCharts(hom.cul, year, "Culiacán - Navolato")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2008-05-13")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2008-05-13"), 22,
                label = "J.O. Culiacan-Navolato"),
             hjust = 1.03, vjust = 0)
saveCharts(llcharts, "culiacán")
#chartRegion(hom, c(25), c(006), year,
#            "Culiacán")

hom.maz <- subset(hom, ENTOCU == 25 & MUNOCU %in% c(012))
llcharts <- generateCharts(hom.maz, year, "Mazatlán")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2008-07-15")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2008-07-15"), 8, label = "J.O. Culiacán - Navolato (Mazatlán)"),
             hjust = 1.03, vjust = 0)
saveCharts(llcharts, "mazatlán")
#chartRegion(hom, c(25), c(012), year,
 #           "Mazatlán", func)

hom.aca <- subset(hom, ENTOCU == 12 & MUNOCU %in% c(001, 021))
llcharts <- generateCharts(hom.aca, year, "Acapulco")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2007-01-15")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2007-01-15"), 20, label = "J.O. Acapulco"),
             hjust = 1.03, vjust = 0)
saveCharts(llcharts, "acapulco (ma)")

chartRegion(hom, c(28), c(027), year,
            "Nuevo Laredo")
chartRegion(hom, c(28), c(022), year,
            "Matamoros (MA)")  
chartRegion(hom, c(28), c(032, 033), year,
            "Reynosa (MA)")

chartRegion(hom, c(26), c(043), year,
            "Nogales")

chartRegion(hom, c(05), c(004, 027, 030), year,
            "Saltillo (MA)")

chartRegion(hom, c(18), c(008, 017), year,
            "Tepic (MA)")

chartRegion(hom, c(06), c(007), year,
            "Manzanillo")


chartRegion(hom, c(16), c(053, 088), year,
            "Morelia (MA)")

hom.mon <- subset(hom, ENTOCU == 19 &
                  MUNOCU %in% c(6, 9, 018, 19, 21, 26, 31, 39,
                                45, 46, 48, 49))
llcharts <- generateCharts(hom.mon, year, "Monterrey (MA)")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2007-02-19")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2007-02-19"), 13, label = "J.O.\nTamaulipas - Nuevo León"),
             hjust = 1.03, vjust = 0)
saveCharts(llcharts, "monterrey (ma)")
#chartRegion(hom, c(19),
 #           c(6, 9, 018, 19, 21, 26, 31, 39, 45, 46, 48, 49),
  #          2008,
   #         "Monterrey (MA)")

hom.vall <- subset(hom, (ENTOCU == 14 & MUNOCU %in% c(067)) |
              (ENTOCU == 18 & MUNOCU %in% c(20)))
llcharts <- generateCharts(hom.vall, year, "Puerto Vallarta")
saveCharts(llcharts, "puerto_vallarta")

chartRegion(hom, c(10), c(005), year,
            "Durango")
chartRegion(hom, c(17), c(007, 008, 011, 018, 020, 028), year,
            "Cuernavaca")
chartRegion(hom, c(10), c(032), year,
            "Santiago Papasquiaro")
hom.lag <- subset(hom, (ENTOCU == 5 & MUNOCU %in% c(17,35)) |
              (ENTOCU == 10 & MUNOCU %in% c(7,12)))
llcharts <- generateCharts(hom.lag, year, "La Laguna")
saveCharts(llcharts, "laguna")

hom.ver <- subset(hom, ENTOCU == 30)
llcharts <- generateCharts(hom.ver, year, "Veracruz (State)")
llcharts$monthly <- llcharts$monthly +
  geom_vline(aes(xintercept = as.Date("2007-05-14")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2007-05-14"), 45, label = "J.O. Veracruz"),
             hjust = 1.1, vjust = 0)
saveCharts(llcharts, "veracruz")

hom.dur <- subset(hom, ENTOCU == 10)
llcharts <- generateCharts(hom.dur, year, "Durango (State)")
saveCharts(llcharts, "durango (state)")

hom.mich <- subset(hom, ENTOCU == 16)
llcharts <- generateCharts(hom.mich, year, "Tamaulipas (State)")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2007-02-19")), alpha = .7,
               linetype = 2) +
  geom_text(aes(as.Date("2007-02-19"), 13, label = "J.O.\nTamaulipas - Nuevo León"),
             hjust = 1.03, vjust = 0)
saveCharts(llcharts, "tamaulipas (state)")

hom.zac <- subset(hom, ENTOCU == 32)
llcharts <- generateCharts(hom.zac, year, "Zacatecas (State)")
saveCharts(llcharts, "zacatecas (state)")

#Femicide rate in Mexico
#Mexico CONAPO
fmx <- ddply(fem, .(ANIODEF), nrow) / c(53219640, 53723982, 54216256,
                                        54696909) * 100000
#US - FBI UNIFORM CRIME REPORTS
#http://www2.fbi.gov/ucr/cius2009/offenses/expanded_information/data/shrtable_01.html
fus <- c(3156, 3177, 3078, 3122) / c(151751781, 153102214,
                                      154446928, 154446928) * 100000

fhom.rates <- data.frame(year = factor(2006:2009),
                         US = fus,
                         MX = fmx$V1)
ggplot(melt(fhom.rates, id = "year"), aes(year, value, group = variable, color = variable)) +
  geom_line() +
  opts(title = "Female Homicide Rates in the US and Mexico (2006-2009)") +
  ylab("homicide rate per 100,000 women") +
  ylim(0, max(fhom.rates$MX)) +
  scale_colour_manual("Country",
                      values = c("blue", "darkgreen"))
ggsave("graphs/femicide-rates-us-mx.png", dpi = 100,
       width = 8, height = 6)

babies <- subset(hom, SEXO != 0 & EDADVALOR < 800)
ggplot(babies, aes(EDADVALOR, ..count.. ,group = SEXOtxt,
                   fill = SEXOtxt))+
  geom_histogram(position = "identity", binwidth = 1, alpha = .5)

chartRegion(hom, c(16), c(52), year,
            "Lázaro Cárdenas")  

#It might be worthwhile to include other states, counties, and
#also presumed homicides + violent deaths of unknown intent
#hom <- subset(hom, SEXO == 2)
#hom <- subset(hom, ENTOCU %in% c(19, 28))
#hom <- subset(hom, ENTOCU %in% c(9, 12, 15, 16, 17))
#hom <- subset(hom, ENTOCU %in% c(2, 8, 10, 18, 25, 26))
ddply(hom, .(ANIODEF), nrow)
#Total homicides in 2009
(19121 *  1/(13628/14175) ) / 1110
#write.csv(ddply(hom, .(ANIODEF, MESDEF), nrow), "monthly.csv")

ddply(hom.j, .(ANIODEF), nrow)
2316*1610/1570
#write.csv(ddply(hom, .(ANIODEF, ABBRV), nrow), "temp.csv")
sd(subset(hom, ANIODEF == 2009 & EDADVALOR < 120)$EDADVALOR)
