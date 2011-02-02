not.hom <- subset(deaths, PRESUNTO != 2)

not.hom.count <- formatDaily(not.hom)
weekly(not.hom.count, "All Deaths Not Homicide")
monthly(not. hom.count, "All Deaths")


flu.codes <- str_c("J", gsub(" ", "0", format(9:18, digits = 2)))
flu <- subset(deaths, CAUSADEF %in% c("J09", "J10", "J11"))
flu <- subset(deaths, CAUSADEF %in% flu.codes)
flu <- subset(deaths, CAUSADEF %in% flu.codes &
              ABBRV == "DF")
flu.count <- formatDaily(flu)
ddply(flu, .(ABBRV), nrow)
weekly(flu.count, "Deaths by Influenza")
levels(factor(deaths$CAUSADEF))
fix(flu)


########################################################
#Day Ligth Savings Time
########################################################
#First Sundays
April 5 2009
6 2008
1 2007
2 2006

Oct 29 2006
Oct 28 2007
Oct 26 2008
Oct 25 2009
