########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Fri Oct 12 19:07:31 2012
## Email: diegovalle at gmail.com
## Purpose: Clean the Mexican Mortality Database 
## Copyright (c) Diego Valle-Jones. All rights reserved

source("lib/download-files.R") ##Download the Mortality DBs 2004-2012
source("lib/extract-convert.R") ##Unzip and convert the dbf's to csv
source("lib/boot.R")  ##Cleanup

source("src/classify.R")  ##Impute deaths of unknown intent
source("src/save.R") ##Save to a csv and RData
test_dir("test1.R", reporter = "summary")


library(mxmortality)


##General Treviño
##http://mexico.cnn.com/nacional/2010/09/16/al-menos-19-personas-mueren-en-enfrentamientos-en-nl-y-tamaulipas
deaths$abbrev <- stateToAbbrev(deaths$state_occur)

ddply(subset(deaths, abbrev == "Tamps" & year_reg== 2010 &
             (intent == "Legal Intervention" & cause == "Firearm")), .(year(date_reg)), nrow)


ddply(subset(deaths, intent == "Legal Intervention"), 
      .(abbrev,year_reg, icd4, cause), nrow)

ddply(subset(deaths, intent =="Homicide"), .(year(date_occur)), nrow)
ddply(subset(deaths, intent =="Homicide"), .(year(date_reg)), nrow)
ddply(subset(deaths, intent =="Accident"), .(cause_detail, year(date_reg)), nrow)
ddply(subset(deaths, cause_detail == "Other Transport"),
      .(icd_title, year(date_reg)), nrow)
ddply(subset(deaths, intent.imputed =="Homicide"), .(year(date_reg)), nrow)

ddply(subset(deaths, intent.imputed =="Homicide" |
             (intent.imputed == "Accident" & cause == "Firearm")), .(year(date_reg)), nrow)
ddply(subset(deaths, abbrev == "BC" &
             (intent.imputed == "Accident" & cause == "Firearm")), .(year(date_reg)), nrow)
ddply(subset(deaths, abbrev == "Sin" &
             (intent == "Accident" & cause == "Firearm")), .(year(date_occur)), nrow)
ddply(subset(deaths, abbrev == "Tamps" &
             (intent == "Accident" & cause == "Firearm")), .(year(date_reg)), nrow)
ddply(subset(deaths, abbrev == "DF" &
             (intent.imputed == "Homicide")), .(year(date_reg), month(date_reg)), nrow)
ddply(subset(deaths, abbrev == "Gro" &
             (is.na(intent)) & year_reg== 2007), .(year(date_reg),  month(date_reg), cause), nrow)
ddply(subset(deaths, abbrev == "Sin" &
             (intent == "Accident")), .(year(date_reg), cause), nrow)


ddply(subset(deaths, intent =="Accident"), .(year(date_occur)), nrow)
ddply(subset(deaths, intent =="Suicide"), .(year(date_reg)), nrow)


ddply(subset(deaths, intent =="Homicide"), .(year(date_occur)), nrow)
ddply(subset(deaths, intent =="Accident" &
             cause == "Firearm" ), .(year(date_reg)), nrow)

ddply(subset(deaths, intent =="Accident" &
             cause_detail== "Firearm" ), .(year(date_reg)), nrow)

ddply(subset(deaths, abbrev == "Sin" &
             cause == "Firearm" & year_reg %in% 2007:2009),
      .(abbrev, year(date_reg)), nrow)

write.csv(ddply(subset(deaths, intent.imputed == "Homicide" | (intent.imputed == "Accident"
                       & cause_detail== "Firearm")),
                .(year(date_reg), month(date_reg)), nrow),
          "imputed.csv", row.names = FALSE)
write.csv(ddply(subset(deaths, intent.imputed == "Homicide"),
                .(year(date_reg), month(date_reg)), nrow),
          "imputed2.csv", row.names = FALSE)




for(i in levels(deaths$cause_detail)){
  message(i)
  print(levels(droplevels(deaths[deaths$cause_detail == i,]$icd_title)))
  print(levels(droplevels(deaths[deaths$cause_detail == i,]$icd4)))
  
}
levels(droplevels(deaths[deaths$cause == "Firearm",]$icd_title))

t <- subset(deaths, as.character(t$cause) != as.character(t$cause_detail))
t[as.character(t$cause) != as.character(t$cause_detail),]

comparison <- compare(deaths,class)
comparison$tM
str(comparison)

names(deaths)
head(deaths)
levels(deaths$cause_detail)
levels(deaths$motor_vehicle_traffic)
levels(deaths$cause)


head(deaths[is.na(deaths$date_reg),])



t <- ddply(subset(deaths, intent  == "Homicide"),
           .(abbrev, year(date_reg), month(date_reg)), summarise,
           hom = length(intent))
t2 <- ddply(subset(deaths, intent.imputed  == "Homicide"),
           .(abbrev, year(date_reg), month(date_reg)), summarise,
           hom.imputed = length(intent.imputed))

t$date <- as.Date(str_c(t$year, "-", t$month, "-", "01"))
t2$date <- as.Date(str_c(t2$year, "-", t2$month, "-", "01"))
t <- merge(t, t2)
t$diff <- t$hom.imputed - t$hom

t$abbrev <- reorder(t$abbrev, -t$diff, function(x) {sum(x[50:96], na.rm = TRUE)})

ggplot(t, aes(date, diff)) +
  geom_line()+
  facet_wrap(~abbrev) +
  theme_bw() +
  xlab("date") +
  labs(title = "Deaths of unknown intent classified as homicides (plus accidents in Sinaloa,BC, and the DF)")   +
  ylab("number of deaths") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
ggsave("extra.png", dpi = 100, width = 9.6, height = 6)


ggplot(t, aes(date, hom)) +
  geom_line(color = "black") +
  geom_line(aes(date, hom.imputed), color = "red") +
  ##xlim(c(as.Date("2005-11-01"), as.Date("2009-02-01")))+
  facet_wrap(~abbrev, scales = "free")
##ggplot()


t <- ddply(subset(deaths, year_occur %in% 2004:2011 &
                  intent == "Accident" & is.na(cause)),
      .(abbrev, as.yearmon(date_occur)), nrow)
t2 <- ddply(subset(deaths, year_occur %in% 2004:2011 &
                  intent == "Homicide"),
      .(abbrev, as.yearmon(date_occur)), nrow)
names(t) <- c("abbrev", "date", "acc")
names(t2) <- c("abbrev", "date", "hom")
t <- merge(t, t2)

ggplot(subset(t, abbrev %in% c("Ags", "Gto", "Tamps",
                               "Qro", "SLP", "Mor",
                               "Coah", "DF")),
       aes(as.Date(date), hom)) +
  geom_line(color = "red") +
  geom_line(aes(as.Date(date), acc)) +
  facet_wrap(~abbrev, scale = "free")


t <- ddply(subset(deaths, year_occur %in% 2004:2011 &
                  intent == "Accident" & is.na(cause)),
      .(abbrev, state_occur, as.yearmon(date_occur)), nrow)
t2 <- ddply(subset(deaths, year_occur %in% 2004:2011 &
                  intent == "Homicide"),
      .(abbrev, state_occur, as.yearmon(date_occur)), nrow)
write.csv(t2, "hom.csv", row.names = FALSE)

names(t) <- c("abbrev", "StateCode", "date", "acc")
names(t2) <- c("abbrev", "StateCode", "date", "hom")
t <- merge(t, t2)

ggplot(subset(t, abbrev %in% c("Ags", "Gto", "Tamps",
                               "Qro", "SLP", "Mor",
                               "Coah", "DF")),
       aes(as.Date(date), hom)) +
  geom_line(color = "red") +
  geom_line(aes(as.Date(date), acc)) +
  facet_wrap(~abbrev, scale = "free")+
  theme_bw() +
  xlab("date") +
  ylab("number of deaths") +
  labs(title = "Homicides (red) and accidental deaths by unknown mechanism (black)")
ggsave("accnoesp.png", dpi = 100, width = 7, height = 5)



t <- ddply(subset(deaths, year_occur %in% 2004:2011 &
                  intent != "Homicide" & cause == "Firearm"),
      .(abbrev, as.yearmon(date_occur)), nrow)
names(t) <- c("abbrev", "date", "hom")

ggplot(t,
       aes(as.Date(date), hom)) +
  geom_line(color = "red") +
  facet_wrap(~abbrev, scale = "free")


ddply(subset(deaths, year_occur %in% 2004:2011 &
                  is.na(intent) & abbrev == "Ver"),
      .(abbrev, year_occur, cause), nrow)

ddply(subset(deaths, year_occur %in% 2004:2011 &
                  is.na(intent) & intent.imputed == "Homicide"& abbrev == "Ver"),
      .(abbrev, year_occur, cause), nrow)


ggplot(ddply(subset(deaths, year_occur %in% 2004:2011 &
                  is.na(intent) & cause == "Firearm"),
      .(year_occur), nrow),
       aes(year_occur, V1)) +
  geom_line() +
  ylab("number of deaths") +
  xlab("year") +
  scale_y_continuous(limits= c(0, 1500)) +
  labs(title = "Deaths of unspecified intent by firearm (2004-2011)") +
  theme_bw()
ggsave("noesp.png", dpi = 100, width = 7, height = 5)


ma <- read.csv("data/metropolitan-areas200k.csv")
ma <- subset(ma, MA == "Valle de México")


deaths$fips_res <- with(deaths, as.numeric(str_c(deaths$state_res,
                                                 gsub(" ", "0", format(deaths$mun_res, digits = 3)))))
mxc_res <- ddply(subset(deaths, intent.imputed =="Homicide" &
             fips_res %in% ma$id &
             year_occur %in% 2011), .(fips_res, year_occur), nrow)

mxc_oc <- ddply(subset(deaths, intent.imputed =="Homicide" &
             fips %in% ma$id &
             year_occur %in% 2011), .(fips, year_occur), nrow)

mxc <- merge(mxc_res, mxc_oc, by.x = "fips_res", by.y = "fips")

ggplot(mxc, aes(V1.x, V1.y)) +
  geom_point()
