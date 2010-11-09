########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sun Nov  7 16:20:43 2010
########################################################
#Small Multiples of Homicide Rates by State




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
pop <- read.csv("data/states.csv")
smallMStates(hom, pop, title, kminy, kmaxy)
ggsave("graphs/state-rates.png", dpi = 100,
       width = 9, height = 7)

titlew <- str_c("Female Homicide Rates in Mexico by State ", "(",
               kminy, "-", kmaxy, ")")
popw <- read.csv("data/states-f.csv")
#popw$State <- iconv(popw$State, "Latin2", "UTF-8")
smallMStates(subset(hom, SEXO == 2) , popw, titlew, kminy, kmaxy)
ggsave("graphs/state-f-rates.png", dpi = 100,
       width = 9, height = 7)



