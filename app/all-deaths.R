not.hom <- subset(deaths, PRESUNTO != 2)

not.hom.count <- formatDaily(not.hom)
weekly(not.hom.count, "All Deaths Not Homicide")
monthly(not.hom.count, "All Deaths")


flu.codes <- str_c("J", gsub(" ", "0", format(9:18, digits = 2)))
flu <- subset(deaths, CAUSADEF %in% c("J09", "J10", "J11"))
flu <- subset(deaths, CAUSADEF %in% flu.codes)
flu <- subset(deaths, CAUSADEF %in% flu.codes &
              ABBRV == "DF")
flu.count <- formatDaily(flu)


weekly(flu.count, "Deaths by Influenza")

flu.states <- ddply(flu, .(ABBRV, ANIODEF, MESDEF), nrow)
flu.states$date <- with(flu.states, as.Date(str_c(ANIODEF, MESDEF, "15",
                                                  sep = "-")))
                    
ggplot(flu.states, aes(date, V1)) +
  geom_line() +
  facet_wrap(~ABBRV)


coke.codes <- str_c("F", 10:19)
coke.deaths <- subset(deaths, CAUSADEF %in% coke.codes)
psy.deaths <- ddply(coke.deaths, .(ANIODEF, MESDEF), nrow)
psy.deaths <- subset(psy.deaths, MESDEF != 0 &
                     (ANIODEF != kmaxy | MESDEF != 12))
psy.deaths$date <- with(psy.deaths, as.Date(str_c(ANIODEF, MESDEF, "15", sep = "-")))
ggplot(psy.deaths, aes(date, V1)) +
  geom_line() +
  geom_smooth() +
  opts(title = "Deaths due to psychoactive substance use") +
  ylab("number of deaths") +
  ylim(0, max(psy.deaths$V1))
2578 / 2704
3290 / 3543
########################################################
#Day Ligth Savings Time
########################################################
#First Sundays


library(chron)
end = FALSE
FirstSundayApr <- function(year) {
  date <- as.Date(str_c(year, "-04-01"))
  end <- FALSE    
  while(!end) {
    if(weekdays(date) == "Sunday") {
      end = TRUE
    } else {
    date <- date + 1
    }
  }
  date
}

LastSundayOct <- function(year) {
  date <- as.Date(str_c(year, "-10-31"))
  end <- FALSE    
  while(!end) {
    if(weekdays(date) == "Sunday") {
      end = TRUE
    } else {
    date <- date - 1
    }
  }
  date
}
FirstSundayApr(2007)
LastSundayOct(2009)

acc <- subset(deaths, PRESUNTO == 3 & ABBRV != "Son")
acc$date <- with(acc, as.Date(str_c(ANIODEF, MESDEF, DIADEF, sep = "-")))


sub10 <- function(df, fun = FirstSundayApr){
  year <- df$ANIODEF[1]
  dst <- subset(df, date > fun(year) - 20 &
                date < fun(year) + 20)
  ddply(dst, .(date), nrow)
}
ll <- dlply(acc, .(ANIODEF), sub10)
x <- rep(0, 39)
for(l in ll) {
  x <- x + l$V1
}
x
qplot(-19:19, x, geom = "line") 

date
April 5 2009
6 2008
1 2007
2 2006

Oct 29 2006
Oct 28 2007
Oct 26 2008
Oct 25 2009
