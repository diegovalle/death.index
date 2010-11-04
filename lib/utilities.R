clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  return(variable.name)
}

#Save plots to png
saveAAPlot <- function(p, filename, width = 800, height = 600) {
    Cairo(file = filename, width = width, height = height)
    print(p)
    dev.off()
}

savePlot <- function(p, filename, width = 800, height = 600) {
    png(file = filename, width = width, height = height)
    print(p)
    dev.off()
}

#Generate all the plots and save them as a list
generateCharts <- function(hom, year, name) {
 ll <- list()
 titles <- c("Daily Number of Homicides in",
                "Weekly Number of Homicides in",
                "Monthly Number of Homicides in",
                "Kernel Density of Age at Death for all Homicides in",
                "Kernel Density of Age at Death for all Homicides by Year in",
                "Kernel Density of Age at Death for all Homicides by Sex in",
                "Homicides by Sex as a Percentage of All Homicides in",
                "Time of death for all Homicides in",
                "Homicides by Age Group as a Percentage of all Homicides in",
                "Number of Homicides by Age Group in",
                "Homicides by Firearm as a Percentage of all homicides in",
                "Total Number of Homicides in",
                "Place Where the Deceased was Found in",

                "School Level of the Deceased in",
                "Occupation of the Deceased in",
                "Marital Status of the Deceased in")

 titles <- str_c(titles, " ", name)
 titles[4] <- str_c(titles[4], " (", as.character(year), ")")


 hom.count <- formatDaily(hom)
 hom08 <- subset(hom, ANIODEF == year)

 ll$daily <- daily(hom.count, titles[1])
 ll$weekly <- weekly(hom.count, titles[2])
 ll$monthly <- monthly(hom.count, titles[3])

 ll$age.den <- ageDensity(hom, titles[4])
 ll$age.year <- ageDensityYear(hom, titles[5])
 ll$age.sex <- ageDensitySex(hom, titles[6])

 ll$sex.per <- bumpChart(subset(hom, SEXO != 0), "SEXOtxt",
                         scale = "Sex", title = titles[7])

 ll$time.of.day <- plotHours(hom, 2008, fix = TRUE, title = titles[8])


 age.groups <- c(0,15,20,25,30,35,40,45,50,55,60,65,Inf)

 ll$age.bump <- plotAgeBump(hom, age.groups, titles[9])
 ll$age.dot <- plotAgeDot(hom, age.groups, 2008, titles[10])

 ll$firearm <- plotFirearmPer(hom, titles[11])
 ll$total.hom <- totalHomicides(hom, titles[12])

 ########################################################
 #Places where people are most likely to die
 ########################################################
 ll$place.bump <- bumpChart(hom, "LUGLEStxt",
                            directlabel = FALSE,
                            title = titles[13],
                            scale ="Location")
 ll$place.dot <- dotPlot(hom08, "LUGLEStxt")


 ########################################################
 #Schooling
 ########################################################
 ll$school.bump <- bumpChart(hom, "ESCOLtxt", last.points,
                             title = titles[14],
                             scale = "School Level")
 ll$school.dot <- dotPlot(hom08, "ESCOLtxt")


 ########################################################
 #Ocupation
 ########################################################
 ll$ocu.bump <- bumpChart(hom, "OCUPACIONtxt",
                         directlabel = FALSE,
                          title = titles[15],
                          scale = "Occupation")
 ll$ocu.dot <- dotPlot(hom08, "OCUPACIONtxt")


 ########################################################
 #Marital Status
 ########################################################
 ll$marital.bump <- bumpChart(hom, "EDOCIVILtxt",
                             directlabel = FALSE,
                              title = titles[16],
                              scale = "Marital\nStatus")
 ll$marital.dot <- dotPlot(hom08, "EDOCIVILtxt")

 ll
}


saveCharts <- function(ll, location) {
  filenames <- c("daily", "weekly", "monthly",
                 "age-den", "age-year", "age-sex", "sex-per",
                 "time-of-day", "age-bump", "age-dot",
                 "firearm", "total-homicides",
                 "place-bump", "place-dot",
                 "school-bump", "school-dot",
                 "ocupation-bump", "ocupation-dot",
                 "marital-bump", "marital-dot")

  filenames <- str_c("graphs/", location, "-", filenames,
                     ".png")

  i <- 1
  for(plot in ll) {
    cat(str_c("Saving ", filenames[i], "\n"))
    savePlot(plot, filenames[i])
    i <- i + 1
  }
}
