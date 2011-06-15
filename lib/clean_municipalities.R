#population from the CONAPO
cleanPopCONAPO <- function(filename) {
  pop <- read.csv(filename, skip = 2)
  pop <- na.omit(pop)
  col2cvt <- 3:ncol(pop)
  pop[,col2cvt] <- lapply(pop[ ,col2cvt],
                          function(x){
                              as.numeric(gsub(" ", "", x))})
  popm <- melt(pop, id = c("Clave", "Entidad.federativa.o.municipio"))
  #The CONAPO adds a "0" to the county codes, remove it
  popm$variable <- substring(popm$variable, 2)
  names(popm) <- c("Code", "Mun", "Year", "Population")
  popm <- subset(popm, Year %in% kminy:kmaxy )
  popm$Year <- as.numeric(popm$Year)
  popm$ENTOCU <- floor(popm$Code / 1000)
  popm$MUNOCU <- popm$Code %% 1000
  popm
}

cleanMuns <- function(hom, pop, cutoff){
  hom.mun <- ddply(hom, .(ENTOCU, MUNOCU, ANIODEF),
                   function(df) nrow(df))


  homrates <- merge(hom.mun, pop,
                    by.x = c("ENTOCU", "MUNOCU", "ANIODEF"),
                    by.y = c("ENTOCU", "MUNOCU", "Year"),
                    all.y = TRUE)

  homrates$rates <- with(homrates, V1 / Population * 10^5)
  homrates[is.na(homrates)] <- 0
  homrates$Code <- with(homrates, str_c(ENTOCU,
                                        format(MUNOCU, width = 3)))
  homrates$Code <- as.numeric(str_replace_all(homrates$Code,
                                              "[ ]", "0"))
  
  homrates <- addAbbrv(homrates)
  homrates$Mun <- gsub("* de .*","", homrates$Mun)
  homrates$Mun <- str_c(homrates$Mun, ", ", homrates$ABBRV)
  #Include only the municipalities that have more than 100K
  #including those that didn't have 100K for the entire period
  subset(homrates, Code %in% unique(subset(homrates, Population >= cutoff)$Code))
}

popmun <- cleanPopCONAPO("data/municipal-population/popmun.csv.bz2")
popmun.f <- cleanPopCONAPO("data/municipal-population/popmun-f.csv.bz2")
popmun.m <- cleanPopCONAPO("data/municipal-population/popmun-m.csv.bz2")
homicides.municipalities.all <- cleanMuns(hom, popmun, 0)
homicides.municipalities.f <- cleanMuns(subset(hom, SEXOtxt == "Female"), popmun.f, 0)
homicides.municipalities.m <- cleanMuns(subset(hom, SEXOtxt == "Male"), popmun.m, 0)

homicides.municipality <- merge(homicides.municipalities.all,
                                homicides.municipalities.f,
                  by = c("ENTOCU", "MUNOCU", "ANIODEF", "Code", "Mun", "ABBRV"))
homicides.municipality <- merge(homicides.municipality,
                                homicides.municipalities.m,
                  by = c("ENTOCU", "MUNOCU", "ANIODEF", "Code", "Mun", "ABBRV"))
names(homicides.municipality) <- c("ENTOCU", "MUNOCU", "Year", "Code",
                                   "MunName", "ABBRV", "Homicides", 
"Population", "Rate", "HomicidesFemale", "PopulationFemale", "RateFemale", 
"HomicidesMale", "PopulationMale", "RateMale")
homicides.municipality <- merge(homicides.municipality, metropolitan.areas,
      by.x = "Code", by.y = "id", all.x = TRUE)

cabeceras <- cabeceras[, c("ENT", "MUN", "CABECERA", "ALTITUD", "LONG", "LAT")]
cabeceras$Code <- with(cabeceras, str_c(ENT, format(MUN, digits = 3)))
cabeceras$Code <- gsub(" ", "0", cabeceras$Code)
homicides.municipality <- merge(homicides.municipality, cabeceras,
      by = "Code", all.x = TRUE)
homicides.municipality$ENT <- NULL
homicides.municipality$MUN <- NULL
names(homicides.municipality) <- c("Code", "ENTOCU", "MUNOCU", "Year",
                                   "MunName", "StateAbbrv", "Homicides", 
                                   "Population", "Rate", "HomicidesFemale",
                                   "PopulationFemale", 
                                   "RateFemale", "HomicidesMale",
                                   "PopulationMale", "RateMale", 
                                   "MetroArea", "Cabecera", "Altitude", "Long", "Lat")
write.csv(homicides.municipality, "homicides-mun-sex.csv", row.names = FALSE)

head(hom.mun.month)
head(popmun.month, 72)
popmun$date <- as.Date(str_c(popmun$Year, "06", "15", sep = "-"))
dates <- seq(as.Date(str_c(kminy, "01", "15", sep = "-")),
    as.Date(str_c(kmaxy, "12", "15", sep = "-")),
    by = "month")
popmun.month <- merge(
                      data.frame(Code = rep(na.omit(unique(popmun$Code)),
                                   each = length(dates)),
                                 date = dates),
                      popmun,
                      by = c("Code", "date"), all.x = TRUE)
popmun.month <- ddply(popmun.month, .(Code), transform,
                                 Population = na.spline(Population, na.rm=FALSE))
popmun.month$ENTOCU <- NULL
popmun.month$MUNOCU <- NULL
popmun.month$Mun <- ifelse(is.na(popmun.month$Mun), NA,
                           str_c(format(as.numeric(popmun.month$Code), digits = 6),
                                 popmun.month$Mun))
popmun.month$Mun <- rep(na.omit(unique(popmun.month$Mun)),
                        each = length(dates))
popmun.month$Year <- format(popmun.month$date, "%Y")
names(popmun.month) <- c("Code", "Date", "Mun", "Year", "Population")


hom.mun.month <- ddply(hom, .(ENTOCU, MUNOCU, ANIODEF, MESDEF),
function(df) nrow(df))
hom.mun.month$Code <- str_c(
                        gsub(" ", "0", str_c(format(hom.mun.month$ENTOCU, digits = 2))),                             gsub(" ", "0", str_c(format(hom.mun.month$MUNOCU, digits = 3))))
hom.mun.month$Code <- as.numeric(hom.mun.month$Code)
hom.mun.month$Date <- as.Date(str_c(hom.mun.month$ANIODEF,
                                    hom.mun.month$MESDEF,
                                    "15", sep = "-"))
temp <- merge(hom.mun.month, popmun.month, by = c("Code", "Date"), all = TRUE)
temp[is.na(temp$V1),]$V1 <- 0
temp$ENTOCU <- str_sub(temp$Code, end = 2)
temp$MUNOCU <- str_sub(temp$Code, start = 3)
temp$ANIODEF <- format(temp$Date, "%Y")
temp$MESDEF <- format(temp$Date, "%m")
temp <- merge(temp, cabeceras, by = "Code")
temp$Mun <- str_sub(temp$Mun, start = 6)
temp <- merge(temp,
      unique(homicides.municipality[,c("ABBRV", "ENTOCU")]),,
      by.x = "ENT", by.y = "ENTOCU", all.x = TRUE)
temp <- merge(temp, metropolitan.areas,
              by.x = "Code", by.y = "id", all.y = TRUE)
write.csv(temp, "homicides-mun-sex-month.csv", row.names = FALSE)


tj.ma <- subset(temp, MA == "Tijuana")
write.csv(tj.ma, "tj.ma.csv", row.names = FALSE)
head(temp)

rm(homicides.municipalities.f)
rm(homicides.municipalities.m)
rm(homicides.municipalities.all)
