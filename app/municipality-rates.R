########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sun Nov  7 13:25:48 2010
########################################################
#Small Multiples of the Homicide rates of the different Municipalities of Mexico


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
  homrates$Mun <- str_c(homrates$Mun, " - ", homrates$ABBRV)
  #Include only the municipalities that have more than 100K
  #including those that didn't have 100K for the entire period
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
  homrates <- homrates[1:c(20*(kmaxy-kminy+1)), ]
  
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
popmun <- cleanPopCONAPO("data/municipal-population/popmun.csv.bz2")
titlemun <- str_c("The Most Violent Municipalities in Mexico with more than a ", format(cutoff, scientific = FALSE, big.mark = ","), " People")
smallMMun(hom, popmun, titlemun, kminy, kmaxy, cutoff)
ggsave("graphs/municipalities-rates.png", dpi = 100,
       width = 9, height = 7)

homrates <- cleanMuns(hom, popmun, 0)
homrates <- homrates[order(-homrates$rates),]
homrates <- ddply(homrates, .(ANIODEF), transform,
                  cumsum = cumsum(Population))
homrates <- ddply(homrates, .(ANIODEF), transform,
                  per = cumsum / sum(Population))
homrates$ANIODEF <- factor(homrates$ANIODEF)
ddply(homrates, .(ANIODEF), function(df) df[which(df$rate <= 10.1)[1],])
ggplot(homrates, aes(rates, per, group = ANIODEF,
                     color = ANIODEF)) +
  geom_step() +
  coord_cartesian(xlim = c(0,100)) +
  scale_y_continuous(formatter = "percent") +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  scale_colour_brewer("Year", palette="RdBu") +
  opts(title = "Percentage of the  Mexican Population Living At or Above\na Homicide Rate, by Year") +
  ylab("percentage population living at or above a homicide rate") +
  xlab("homicide rate") + 
  annotate("text", y = .5, x = 50, label = "Percentage of the Population\nliving in municipalities with a\nhomicide rate higher than 50:\n\n2006 - 1%\n2007 - .8% \n2008 - 5%\n2009 - 8%", hjust = 0) +
  theme_bw()
ggsave("graphs/percente-above.png", dpi = 100,
       width = 7, height = 6)


hom.mun <- ddply(hom, .(id, MA, ANIODEF),
                   function(df) nrow(df))
hom.mun <- hom.mun[!is.na(hom.mun$MA), ]
homrates <- merge(hom.mun, popmun,
                    by.x = c("id", "ANIODEF"),
                    by.y = c("Code", "Year"),
                    all.x = TRUE)
hom.ma <- ddply(homrates, .(ANIODEF, MA),
                function(df) c(sum(df$V1), sum(df$Population)))
hom.ma$rates <- with(hom.ma, V1 / V2 * 10^5)
hom.ma <- ddply(hom.ma, .(MA), transform,
                    order = rates[length(rates)])


hom.ma.max <- subset(hom.ma, ANIODEF == kmaxy)

hom.ma.max <- hom.ma.max[order(-hom.ma.max$rates),]
hom.ma.max <- hom.ma.max[1:20,]
hom.ma.max$MA <- factor(hom.ma.max$MA)
hom.ma.max$MA <- reorder(hom.ma.max$MA, hom.ma.max$order)
ggplot(hom.ma.max, aes(rates, MA)) +
  geom_point() +
  xlim(0, max(hom.ma.max$rates)) +
  ylab("") + xlab("homicide rate") +
  opts(title = str_c("The Most Violent Metropolitan Areas in", kmaxy))

hom.ma <- subset(hom.ma, MA %in% hom.ma.max$MA)
hom.ma$MA <- reorder(hom.ma$MA, -hom.ma$order)
ggplot(hom.ma, aes(ANIODEF, rates)) +
      geom_line() +
      geom_point(aes(size = V1)) +
      scale_area("Number of\nhomicides") +
      ylab("homicide rate") +
      xlab("year") +
      opts(title = "") +
      opts(axis.text.x = theme_text(angle = 60, hjust = 1)) +
      scale_x_continuous(breaks = c(kminy:kmaxy)) +
      facet_wrap(~ MA)+
      opts(title = "The Most Violent Metropolitan Areas in Mexico")
fix(hom.ma.max)

cutoffw <- 50000
popmun.f <- cleanPopCONAPO("data/municipal-population/popmun-f.csv.bz2")
titlemunw <- str_c("The Most Violent Municipalities in Mexico with more than ", format(cutoffw, scientific = FALSE, big.mark = ","), " Women")
smallMMun(subset(hom, SEXO == 2), popmun.f, titlemunw, kminy, kmaxy, cutoffw)
ggsave("graphs/municipalities-f-rates.png", dpi = 100,
       width = 9, height = 7)

#hom$metro.area <- str_c(hom$ENTOCU, hom$MUNCOU)
#hom[which(hom$ENTOCU == 01 & hom$MUNOCU == 01),]$metro.area <- "Aguascalientes"

hom.mun <- ddply(hom, .(MA, ANIODEF),
                   function(df) nrow(df))


  homrates <- merge(hom.mun, pop,
                    by.x = c("ENTOCU", "MUNOCU", "ANIODEF"),
                    by.y = c("ENTOCU", "MUNOCU", "Year"),
                    all.y = TRUE)

  homrates$rates <- with(homrates, V1 / Population * 10^5)
  homrates[is.na(homrates)] <- 0

mi <- ddply(subset(hom, ENTOCU == 16), .(ANIODEF, MESDEF, DIADEF),
            nrow)

mi <- subset(mi, MESDEF != 0 | DIADEF != 0)
mi$date <- with(mi, as.Date(str_c(ANIODEF, MESDEF, DIADEF, sep="-")))
ggplot(mi, aes(date, V1)) +
  geom_line() +
  facet_wrap(~MUNOCU) +
  scale_x_date()

mi <- ddply(subset(hom, ENTOCU == 12), .(ANIODEF, MESDEF, DIADEF),
            nrow)

mi <- subset(mi, MESDEF != 0 | DIADEF != 0)
mi$date <- with(mi, as.Date(str_c(ANIODEF, MESDEF, DIADEF, sep="-")))
ggplot(mi, aes(date, V1)) +
  geom_line() +
  facet_wrap(~MUNOCU) +
  scale_x_date()
