########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sun Nov  7 16:15:15 2010
########################################################
#Choropleths of the homicide rate in Mexico


#County map
map.inegi.ct <- "maps/MUNICIPIOS-60.shp"
mexico.ct <- readShapePoly(map.inegi.ct,
                               proj4string = CRS("+proj=aea"))
#State map
map.inegi.st <- "maps/ESTADOS-90.shp"
mexico.st <- readShapePoly(map.inegi.st,
                               proj4string = CRS("+proj=aea"))


#popmun <- subset(popmun, ENTOCU %in% c(19, 28))
#popmun <- subset(popmun, ENTOCU %in% c(2, 8, 10, 18, 25, 26))
#popmun <- subset(popmun, ENTOCU %in% c(9, 12, 15, 16, 17))

#mexico.st <- mexico.st[mexico.st$CVE_ENT %in% c("19", "28"),]
#mexico.ct <- mexico.ct[mexico.ct$CVE_ENT %in% c("19", "28"),]

#mexico.st <- mexico.st[mexico.st$CVE_ENT %in% c("02", "08", "10", "18", "25", "26"),]
#mexico.ct <- mexico.ct[mexico.ct$CVE_ENT %in% c("02", "08", "10", "18", "25", "26"),]

#mexico.st <- mexico.st[mexico.st$CVE_ENT %in% c("09", "12", "15", "16", "17"),]
#mexico.ct <- mexico.ct[mexico.ct$CVE_ENT %in% c("09", "12", "15", "16", "17"),]
plot(mexico.st)

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

########################################################
#Create a csv file for OpenHeatMap
########################################################
saveCSVOHM <- function(hom.pop) {
  space2Zero <- function(str){
    gsub(" ", "0", str)
  }

  hom.pop$mex_muni_code <- str_c(space2Zero(format(hom.pop$ENTOCU,
                                               width = 2)),
                               space2Zero(format(hom.pop$MUNOCU,
                                               width = 3)))
  hom.pop$rate <- with(hom.pop, V1 / Population * 10^5)
  hom.pop$rate[is.na(hom.pop$rate)] <- 0
  csv <- hom.pop[,c(8,9,3)]

  names(csv) <- c("mex_muni_code", "value", "time")
  write.csv(csv, "reports/rates.csv", row.names = FALSE)
}

saveCSVOHM(hom.pop)

drawMap <- function(vector, title, breaks, text = NA) {
  maxh <- 100
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


