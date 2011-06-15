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
  homrates
}

createStateHom <- function(hom, pop, name = "Rate") {
  hom.st <- ddply(hom, .(ENTOCU, ANIODEF), function(df) nrow(df))

  pop <- cleanPop(pop)
  homrates <- addRates(hom.st, pop)
  homrates$State <- gsub("de .*", "", homrates$State)
  homrates$State <- with(homrates, reorder(State, -order))

  names(homrates) <- c("CVE_ENT", "Year", "Homicides", "State", "Population", name, "Order")
  homrates
}

homicide.state <- merge(merge(createStateHom(hom, states),
            createStateHom(subset(hom, SEXOtxt == "Females") , states.f, "RateFemales"),
            by = c("CVE_ENT", "Year", "State")),
            createStateHom(subset(hom, SEXOtxt == "Males") , states.m, "RateMales"),)
names(homicide.state) <-  c("CVE_ENT", "Year", "State", "Homicides", "Population", 
"Rate", "Order.x", "HomicidesFemales", "PopulationFemales", "RateFemales", 
"Order.y", "HomicidesMales", "PopulationMales", "RateMales", "Order")
homicide.state$Order.x <- NULL; homicide.state$Order.y <- NULL
homicide.state$Order <- NULL
write.csv(homicide.state, "homicide.state.sex.csv", row.names = FALSE)

addMonths <- function(pop, namepop = "Population") {
  dates <- seq(as.Date(str_c(kminy, "01", "15", sep = "-")),
               as.Date(str_c(kmaxy, "12", "15", sep = "-")),
               by = "month")

  statesm <- cleanPop(states)
  statesm$date <- as.Date(str_c(statesm$variable, "06", "15", sep = "-"))
  homicide.states.month <- merge(
                                 data.frame(Code = rep(unique(homicide.state$CVE_ENT),
                                              each = length(dates)),
                                            date = dates),
                                 statesm,
                                 by = c("Code", "date"),
                                 all.x = TRUE)
  homicide.states.month <- ddply(homicide.states.month, .(Code), transform,
                                 population = na.spline(value, na.rm=FALSE))
  homicide.states.month$value <- NULL
  homicide.states.month$State <- rep(unique(na.omit(homicide.states.month$State)),
                                     each = length(dates))
  homicide.states.month$variable <- format(homicide.states.month$date, "%Y")
  names(homicide.states.month) <- c("Code", "Date", "State", "Year", namepop)
  homicide.states.month
}

addHomicides <- function(hom, pop.states, namehom = "Homicides",
                         namerate = "Rate") {
  hom.st.month <- ddply(hom, .(ENTOCU, MESDEF, ANIODEF), function(df) nrow(df))
  hom.st.month <- subset(hom.st.month, MESDEF != 0)
  hom.st.month$Date <- as.Date(str_c(hom.st.month$ANIODEF,
                                     hom.st.month$MESDEF,
                                     "15", sep = "-"))
  names(hom.st.month) <- c("Code", "Month", "Year", "Homicides", "Date")
  temp <- merge(hom.st.month, pop.states, by = c("Code", "Date", "Year"))
  temp$Rate <- temp$Homicides / temp$Population * 10^5 * 12
  names(temp) <- c("Code", "Date", "Year", "Month", namehom, "State",
                   names(temp)[7], namerate)
  temp
}

homicides.state.month <- merge(addHomicides(hom, addMonths(states)),
                               addHomicides(subset(hom, SEXOtxt == "Females"),
                                            addMonths(states.f, "PopulationFemales"),
                                            "HomicideFemales", "RateFemales"),
                               by = c("Code", "Date", "Year", "Month", "State"))

homicides.state.month <- merge(homicides.state.month,
                               addHomicides(subset(hom, SEXOtxt == "Males"),
                                            addMonths(states.m, "PopulationMales"),
                                            "HomicideMales", "RateMales"),
                               by = c("Code", "Date", "Year", "Month", "State"))
write.csv(homicide.state, "homicide.state.sex.month.csv", row.names = FALSE)
