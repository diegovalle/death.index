source("app/clean-database.R")
source("lib/boot.R")
#source('lib/run_tests.R')

#Deaths with no year of occurance, narco-mines,
#too decomposed, etc
#Assume the deaths occured on the year they were registered
deaths[which(deaths$ANIODEF == 0),]$ANIODEF <- deaths[which(deaths$ANIODEF == 0),]$ANIOREG

hom <- subset(deaths, PRESUNTOtxt == "Homicide")


#Stats for Important cities, females, states, one armed midgets
#less than 5 years old, etc
source("app/summary-stats.R")
source("app/military.R")

#Small Multiple of State Rates
source("app/state-rates.R")
#Small Multiple of Municipality Rates
source("app/municipality-rates.R")

#Choropleths
source("app/choropleths.R")
#Clustering of Municipalities with similar homicide patterns
#source("app/cluster.R")


(10495 + 3268 ) / 21313
7100 / 21313

(4186 + 2105 ) / 7971
1680 / 7971

ddply(subset(hom, ABBRV == "Mex"), .(ANIODEF), nrow)
ddply(subset(hom, ABBRV == "Gto"), .(ANIODEF), nrow)

ddply(subset(hom, ABBRV == "Ver" &
             MUNOCU %in% c(123, 133, 152, 205,
                           121, 161, 155, 63, 152, 154, 60,
                           55, 155, 167, 151, 167,58, 27,202,
                           76, 72, 198, 180, 160, 157, 83, 189)), .(ANIODEF), nrow)
669/7643000*10^5
339/7643000*10^5
669/339
69/36
ddply(subset(hom, ABBRV == "DF"), .(ANIODEF, MUNOCU), nrow)

ggplot(ddply(subset(hom, MA == "Valle de México"), .(ANIODEF, ABBRV), nrow),
       aes(ANIODEF, V1, group = ABBRV)) +
  geom_line()

cities <- homicides.municipality
cities$is.ma <- ifelse(is.na(homicides.municipality$MetroArea), "Metro", "NonMetro")
cities$zone <- cut(cities$Population, c(0,5000,10000,100000, Inf))
ddply(cities, .(zone, Year),
                 function(df) sum(df$Homicides) / sum(df$Population) * 10^5)
ggplot(cities, aes(Population, Rate)) +
  geom_point() +
  geom_smooth() +
  ylim(0,200)
nrow(cities)


