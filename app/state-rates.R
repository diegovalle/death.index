















########################################################
#Small multiples of rates
########################################################


kminy <- min(hom$ANIODEF)
kmaxy <- max(hom$ANIODEF)


cleanPop <- function(pop) {
  popm <- melt(pop[1:32, ], id = c("State", "Code"))
  popm$variable <- as.numeric(str_sub(popm$variable, 2))
  subset(popm, variable %in% kminy:kmaxy )
}

addRates <- function(hom.st, popm){
  homrates <- merge(hom.st, popm, by.y=c("Code", "variable"),
                  by.x=c("ENTOCU", "ANIODEF"),
                  all.y = TRUE)
  homrates$rates <- with(homrates, V1 / value * 10^5)
  homrates[is.na(homrates)] <- 0
  homrates <- ddply(homrates, .(State), transform,
                    order = rates[length(rates)])
  homrates$State <- with(homrates, reorder(State, -order))
  homrates
}

smallMStates <- function(hom, pop, title, kminy, kmaxy) {
  hom.st <- ddply(hom, .(ENTOCU, ANIODEF), function(df) nrow(df))

  pop <- cleanPop(pop)
  homrates <- addRates(hom.st, pop)

  ggplot(homrates, aes(ANIODEF, rates)) +
      geom_line() +
      geom_point(aes(size = V1)) +
      scale_size("Number of\nhomicides") +
      ylab("homicide rate") +
      xlab("year") +
      opts(title = title) +
      opts(axis.text.x = theme_text(angle = 60, hjust = 1)) +
      scale_x_continuous(breaks = c(kminy:kmaxy)) +
      scale_y_continuous(limits = c(0, max(homrates$rates))) +
      facet_wrap(~ State)
}

title <- str_c("Homicide Rates in Mexico by State ", "(", kminy,
               "-", kmaxy, ")")
pop <- read.csv("data/states-all.csv")
smallMStates(hom, pop, title, kminy, kmaxy)

titlew <- str_c("Female Homicide Rates in Mexico by State ", "(",
               kminy, "-", kmaxy, ")")
popw <- read.csv("data/states-women.csv")
#popw$State <- iconv(popw$State, "Latin2", "UTF-8")
smallMStates(subset(hom, SEXO == 2) , popw, titlew, kminy, kmaxy)


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

addAbbrv <- function(df){
    df$ABBRV <- car::recode(df$ENTOCU, "1 = 'Ags';
                                 2 = 'BC';
                                 3 = 'BCS';
                                 4 = 'Camp';
                                 5 = 'Coah';
                                 6 = 'Col';
                                 7 = 'Chis';
                    8 = 'Chih';
                    9 = 'DF';
                    10 = 'Dgo';
                    11 ='Gto';
                    12 ='Gro';
                    13 ='Hgo';
                    14 ='Jal';
                    15 ='Mex';
                    16 ='Mich';
                    17 ='Mor';
                    18 ='Nay';
                    19 ='NL';
                    20 ='Oax';
                    21 ='Pue';
                    22 ='Qro';
                    23 ='QR';
                    24 ='SLP';
                    25 ='Sin';
                    26 ='Son';
                    27 ='Tab';
                    28 ='Tamps';
                    29 ='Tlax';
                    30 ='Ver';
                    31 ='Yuc';
                    32 = 'Zac'")
    df
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
  homrates$Mun <- str_c(homrates$Mun, " - ", homrates$ABBRV)
  #Include only the municipalities that have more than 100K
  #correc including those that didn't have 100K for the entire period
  subset(homrates, Code %in% unique(subset(homrates, Population >= cutoff)$Code))
}

smallMMun <- function(hom, pop, title, kminy, kmaxy, cutoff,
                      max = TRUE) {
  homrates <- cleanMuns(hom, pop, cutoff)

  homrates <- ddply(homrates, .(ENTOCU, MUNOCU), transform,
                    order = rates[length(rates)])
  if(max) {
      homrates <- homrates[order(-homrates$order), ]
  } else {
      homrates <- homrates[order(homrates$order), ]
  }
  homrates <- homrates[1:c(16*(kmaxy-kminy+1)), ]
  homrates$Mun <- with(homrates, reorder(Mun, -order))

  ggplot(homrates, aes(ANIODEF, rates)) +
      geom_line() +
      geom_point(aes(size = V1)) +
      scale_size("Number of\nhomicides") +
      ylab("homicide rate") +
      xlab("year") +
      opts(title = title) +
      opts(axis.text.x = theme_text(angle = 60, hjust = 1)) +
      scale_x_continuous(breaks = c(kminy:kmaxy)) +
      scale_y_continuous(limits = c(0, max(homrates$rates))) +
      facet_wrap(~ Mun)
}
cutoff <- 10^5
popmun <- cleanPopCONAPO("data/popmun.csv.bz2")
titlemun <- str_c("The Most Violent Municipalities in Mexico with more than ", format(cutoff, scientific = FALSE, big.mark = ","), " People")
smallMMun(hom, popmun, titlemun, kminy, kmaxy, cutoff)

cutoffw <- 50000
popmunw <- cleanPopCONAPO("data/popmun-f.csv.bz2")
titlemunw <- str_c("The Most Violent Municipalities in Mexico with more than ", format(cutoffw, scientific = FALSE, big.mark = ","), " Women")
smallMMun(subset(hom, SEXO == 2), popmunw, titlemunw, kminy, kmaxy, cutoffw)

#County map
map.inegi.ct <- "maps/MUNICIPIOS-60.shp"
mexico.ct <- readShapePoly(map.inegi.ct,
                               proj4string = CRS("+proj=aea"))
#State map
map.inegi.st <- "maps/ESTADOS-90.shp"
mexico.st <- readShapePoly(map.inegi.st,
                               proj4string = CRS("+proj=aea"))


popmun <- subset(popmun, ENTOCU %in% c(19, 28))
popmun <- subset(popmun, ENTOCU %in% c(2, 8, 10, 18, 25, 26))
popmun <- subset(popmun, ENTOCU %in% c(9, 12, 15, 16, 17))

mexico.st <- mexico.st[mexico.st$CVE_ENT %in% c("19", "28"),]
mexico.ct <- mexico.ct[mexico.ct$CVE_ENT %in% c("19", "28"),]

mexico.st <- mexico.st[mexico.st$CVE_ENT %in% c("02", "08", "10", "18", "25", "26"),]
mexico.ct <- mexico.ct[mexico.ct$CVE_ENT %in% c("02", "08", "10", "18", "25", "26"),]

mexico.st <- mexico.st[mexico.st$CVE_ENT %in% c("09", "12", "15", "16", "17"),]
mexico.ct <- mexico.ct[mexico.ct$CVE_ENT %in% c("09", "12", "15", "16", "17"),]
plot(mexico.ct)

map.shp <- data.frame(NOM_MUN= mexico.ct$NOM_MUN,
                           ENTOCU = as.numeric(mexico.ct$CVE_ENT),
                           MUNOCU = as.numeric(mexico.ct$CVE_MUN))


#hom.count <- ddply(hom, .(ANIODEF, MESDEF, DIADEF,ENTOCU,
 #                         MUNOCU, ANIODEF), nrow)
#hom.count$date <-  as.Date(paste(hom.count$ANIODEF,
 #                                    hom.count$MESDEF,
  #                                   hom.count$DIADEF,
   #                                  sep = "/"),
    #                                 "%Y/%m/%d")

hom.mun <- ddply(hom, .(ENTOCU, MUNOCU, ANIODEF),
                   function(df) nrow(df))
hom.pop <- merge(popmun, hom.mun,
                    by.y = c("ENTOCU", "MUNOCU", "ANIODEF"),
                    by.x = c("ENTOCU", "MUNOCU", "Year"),
                    all.x = TRUE)

ifelse(str_len(hom.pop$Code) == 4, str_c("0", hom.pop$Code),
       hom.pop$Code)
changeSp <- function(str){
    gsub(" ", "0", str)
}

hom.pop$mex_muni_code <- str_c(changeSp(format(hom.pop$ENTOCU,
                                               width = 2)),
                               changeSp(format(hom.pop$MUNOCU,
                                               width = 3)))
hom.pop$rate <- with(hom.pop, V1 / Population * 10^5)
hom.pop$rate[is.na(hom.pop$rate)] <- 0
csv <- hom.pop[,c(9,10,3)]

names(csv) <- c("mex_muni_code", "value", "time")
write.csv(csv, "rates.csv", row.names = FALSE)

drawMap <- function(vector, title, breaks, text = NA) {
  maxh <- 13
  plotvar<- unlist(vector)
  nclr <- 9
  plotclr <- brewer.pal(nclr,"Reds")
  fillRed <- colorRampPalette(plotclr)
  plotvar[plotvar >= maxh] <- maxh -1
  colcode <- fillRed(maxh)[round(plotvar) + 1]
  plot(mexico.ct, col = colcode, lty = 0, border = "gray")
  plot(mexico.st, add = TRUE, lwd=1, border = "black")
  title(main = title)
  colorlegend(posy = c(0.05,0.9), posx = c(0.9,0.92),
              col = fillRed(maxh),
              zlim=c(0, maxh), zval = breaks,
              main = "homicides per\n100,000")
  par(bg='white')
}



for(year in kminy:kmaxy) {
    Cairo(file = paste("graphs/drug-war-", year,
              ".png", sep=""), width = 960, height = 600)
    maprates <- join(map.shp,
                 subset(hom.pop, Year == year))

    maprates$V1[is.na(maprates$V1)] <- 0
    maprates$rates <- with(maprates, V1 / Population * 10^5)
    drawMap(maprates$rates, "", c(0, 50, 100))
    dev.off()
}


########################################################
#Section
########################################################

#Monthly Population Interpolation
#pop <- read.csv("data/conapo-pop.csv")
#pop2 <- data.frame(date = seq(as.Date("2006-01-01"),
 #                  as.Date("2008-12-31"),
  #         by = "day"),
   #        MUNOCU = c(rep(popmun$MUNOCU, each = 365),
    #       rep(popmun$MUNOCU, each = 365),
     #      rep(popmun$MUNOCU, each = 366)))
pop2 <- data.frame(Year = rep(kminy:kmaxy, each = nrow(popmun)*4),
                   Month = rep(1:12),
                   Code = rep(popmun$Code, each = 12),
                   ENTOCU = rep(popmun$ENTOCU, each = 12),
                   MUNOCU = rep(popmun$MUNOCU, each = 12))
popmun$Month <- 6
popmonth <- join(pop2, popmun)
head(pop2)
#pop2$MonthlyNA[pop2$Month == 6] <- unlist(popmun$Population)

poplist <- dlply(popmonth, .(Code), transform,
           pop = na.spline(Population, na.rm = FALSE))
popmonth <- rbind.fill(poplist)


hom.mun <- ddply(hom, .(ENTOCU, MUNOCU, MESDEF, ANIODEF),
                   function(df) nrow(df))
hom.pop <- merge(popmonth, hom.mun,
                    by.y = c("ENTOCU", "MUNOCU", "MESDEF", "ANIODEF"),
                    by.x = c("ENTOCU", "MUNOCU", "Month", "Year"),
                    all.x = TRUE)
names(hom.pop)
hom.pop <- hom.pop[order(hom.pop$ENTOCU, hom.pop$MUNOCU,
                         hom.pop$Year, hom.pop$Month),]
hom.pop$V1[is.na(hom.pop$V1)] <- 0
hom.pop$rate <- with(hom.pop, V1 / pop * 10^5 * 12)

hom.ll.trend <- dlply(hom.pop, .(ENTOCU, MUNOCU), transform, trend = data.frame(stl(ts(rate, start = 2006, freq = 12), "per")$time.series)$trend )
hom.pop <- rbind.fill(hom.ll.trend)

hom.pop$trend[hom.pop$trend < 0] <- 0
head(hom.pop)

i <- 1
for(year in kminy:kmaxy) {
    for(month in 1:12){
        print(i)
        Cairo(file = paste("frames/drug-war-", i,
              ".png", sep=""), width = 640, height = 480)
        maprates <- join(map.shp,
                         subset(hom.pop,
                                Year == year & Month == month))

        #maprates$V1[is.na(maprates$V1)] <- 0
        #maprates$rates <- with(maprates, V1 / pop * 10^5 * 12)
        drawMap(maprates$trend, as.character(year), c(0, 50, 100))
        dev.off()
        i <- i + 1
    }
}

gpclibPermit()
names(mexico.ct)
mx.st.map <- fortify(mexico.st, region = "CVE_ENT")
mx.ct.map <- fortify(mexico.ct, region = "CVE_MUN")

ggplot(mx.ct.map, aes(long,lat)) +
         geom_polygon(aes(group = group), fill = "white",
                   color = I("black")) +
         theme_bw() +
         coord_map(projection = "gilbert")

#mogrify -background white -flatten "*.png" *.png
#ffmpeg -r 2 -i drug-war-%d.png -b 600k drug-war.mp4

fix(pop2)
fix(hom.mun)
fix(popmun)
fix(popmonth)
fix(map.shp)
fix(hom.pop)
fix(maprates)
