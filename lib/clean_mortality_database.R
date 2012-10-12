ICDseq <- function(start, end) {
  letter <- str_sub(start, 1, 1)
  number1 <-  as.numeric(str_sub(start, 2, length(str) + 2))
  number2 <- as.numeric(str_sub(end, 2, length(str) + 2))
  str_c(letter, gsub(" ", "0",
                     format(number1:number2, width = 2)))
}

CDeathSeq <- function(start, end, num1, num2) {
  ssequence <- ICDseq(start, end)
  sequence <- c()
  for(j in 1:length(ssequence))
    for(i in num1:num2)
      sequence <- c(sequence, str_c(ssequence[j], i))
  sequence
}

CDeathSeq1 <- function(start, num1, num2) {
  sequence <- c()
  for (i in num1:num2)
    sequence <- c(sequence, str_c(start, i))
  return(sequence)
}

MortalityMatrix <- function(df, codes, name) {
  str <- codes
  str <- str_replace_all(str, "\\*", "")
  str <- str_replace_all(str, "\\.", "")
  l <-  str_split(str, ", ")

  sequence <- c();Cdeaths <- c(); Ccausadef <- c()

  for (i in l[[1]]) {
    if(str_detect(i, "-")) {
      x <- str_split(i, "-")
      sequence <- c(sequence, ICDseq(x[[1]][1], x[[1]][2]))
    } else {
      if(str_length(i) == 4) {
        Cdeaths <-  c(Cdeaths, i)
      }
      if(str_length(i) == 3) {
        Ccausadef <- c(Ccausadef, i)
      }
    }
  }
 #print(name)
  #print(sequence)
  if(length(sequence))
    try(df[df$CAUSADEF %in% sequence,]$CAUSE <-  name, silent = TRUE)
  if(length(Cdeaths))
   try(df[df$CDEATH %in% Cdeaths,]$CAUSE <-  name, silent = TRUE)
  if(length(Ccausadef))
    try(df[df$CAUSADEF %in% Ccausadef,]$CAUSE <- name, silent = TRUE)
  return(df)
}

cleanDeaths <- function(deaths) {
  ##Max year and min year
  kminy <- min(deaths$ANIODEF)
  kmaxy <- max(deaths$ANIODEF)
  last.day <- as.Date(str_c(kmaxy, "12", "31", sep = "-"))


  ##Nice variable names
  deaths$SEXO <- car::recode(deaths$SEXO, "0 = NA;
                                              1 = 'Male';
                                              2 = 'Female';")

  deaths$LUGLES <- car::recode(deaths$LUGLES,  "0 = 'Home';
                                   1 = 'Residential Institution';
                                   2 = 'School or Office';
                                   3 = 'Sporting Area';
                                   4 = 'Public Street';
                                   5 = 'Commercial Area';
                                   6 = 'Industrial Area';
                                   7 = 'Farm';
                                   8 = 'Other';
                                   9 = NA;
                                   88 = 'Natural Death';")

  deaths$DERHAB <- car::recode(deaths$DERHAB, "0 = NA;
                                1 = 'None';
                                2 = 'IMSS';
                                3 = 'ISSSTE';
                                4 = 'PEMEX';
                                5 = 'SEDENA';
                                6 = 'SEMAR';
                                7 = 'Seguro Popular';
                                8 = 'Other'")

  deaths$ESCOL <- car::recode(deaths$ESCOL, "0 = NA;
                                1 = 'No schooling';
                                2 = 'Grade School Incomplete';
                                3 = 'Grade School Completed';
                                4 = 'Secundaria Incomplete';
                                5 = 'Secundaria Completed';
                                6 = 'Preparatoria';
                                7 = 'Tertiary Education';
                                8 = 'Not App. (< 6 yrs old)'")

  deaths$OCUPACION <- car::recode(deaths$OCUPACION, "0 = NA;
    2 = 'Inactive';
    11 = 'Professionals';
    12 = 'Technician';
    13 = 'Education';
    14 = 'Arts and Sports';
    21 = 'Public, private and social sectors';
    41 = 'Farmers';
    51 = 'Industrial activities (foremen)';
    52 = 'Industrial production (workers)';
    53 = 'Industrial production (machine operators)';
    54 = 'Industrial production (helpers)';
    55 = 'Transportation';
    61 = 'Services';
    62 = 'Administration';
    71 = 'Sales';
    72 = 'Street salesmen';
    81 = 'Personal services';
    82 = 'Servants';
    83 = 'Army, policemen and private security';
    98 = 'Not App. (less than 12 years old)';
    99 = 'Insufficiently specified';")

  deaths$EDOCIVIL <- car::recode(deaths$EDOCIVIL, "0 = NA;
                                      1 = 'Single';
                                      2 = 'Widow';
                                      3 = 'Divorced';
                                      4 = 'Living Together';
                                      5 = 'Married';
                                      8 = 'Not App. (< 12 yrs old)';")


  deaths$NECROPCIA <- car::recode(deaths$NECROPCIA, "0 = NA;
                                      1 = 'Yes';
                                      2 = 'No';")
  deaths$PESO <- car::recode(deaths$PESO, "0 = NA;
                                      8888 = 'Not App. (Age > 1 day)';")

  deaths$PRESUNTO <- car::recode(deaths$PRESUNTO, "1 = 'Accident';
                                      2 = 'Homicide';
                                      3 = 'Suicide';
4 = NA;
5 = 'Legal Intervention';
8 = 'Natural death';")

  deaths$CERTIF <- car::recode(deaths$CERTIF, "0 = NA;
                                      1 = 'Attending Physician';
                                      2 = 'Forensic Doctor';
                                      3 = 'Other Doctor';
                                      4 = 'SSA Personnel';
                                      5 = 'Civil Authority';
                                      8 = 'Other';")


  deaths$NACION <- car::recode(deaths$NACION, "1 = 'Mexican';
                                      2 = 'Foreigner';")

  deaths$TAMLOCRH <- car::recode(deaths$TAMLOCRH, "0 = NA;
    1 = '999';
    2 = '1999';
    3 = '2999';
    4 = '4999';
    5 = '9999';
    6 = '14999';
    7 = '19999';
    8 = '29999';
    9 = '39999';
    10 = '49999';
    11 = '74999';
    12 = '99999';
    13 = '249999';
    14 = '499999';
    15 = '999999';
    16 = '1499999';
    17 = 'Inf';")

  deaths$ASIST <- car::recode(deaths$ASIST, "1 = 'Yes';
                                      2 = 'No';
                                        0 = NA;")
  deaths$TRABAJO <- car::recode(deaths$TRABAJO, "8 = 'Natural Death';
                                      2 = 'No';
                                        1 = 'Yes';
                                      0 = NA;")
  deaths$VIOLFAM <- car::recode(deaths$VIOLFAM, "0 = NA;
                                      1 = 'Yes';
                                        2 ='No';
                                      8 = 'Natural Death';")
  
  deaths$CONDEMBA <- car::recode(deaths$CONDEMBA, "0 = NA;
                                      1 = 'Pregnancy';
                                      2 = 'Birth';
                                      3 = 'Puerperium';
                                      4 = '43 days to 11 months after giving birth';
                                      5 = 'No pregancy for 11 months';
                                      6 = 'No pregnancy for 12 months';
                                      8 = NA")
  deaths$REL_EMBA <- car::recode(deaths$REL_EMBA, "
                                      1 = 'Yes';
                                        2 ='No';
                                      8 = 'Natural Death';")
  deaths$COMPLICARO <- car::recode(deaths$COMPLICARO, "
                                      1 = 'Yes';
                                        2 ='No';
                                      8 = 'Natural Death';")
  
  deaths$MUNOCU[which(deaths$MUNOCU == 0)] <- NA
  deaths$MUNRH[which(deaths$MUNRH == 0)] <- NA
  deaths$LOCOCU[which(deaths$LOCOCU == 0)] <- NA
  deaths$LOCRH[which(deaths$LOCRH == 0)] <- NA
  deaths <- addAbbrv(deaths)
  ##Don't count deaths that occurred in foreign lands
  deaths <- subset(deaths, !ENTOCU %in% c(33, 34, 35))

  

  
  #deaths$date <- with(deaths, as.Date(str_c(ANIODEF, MESDEF, DIADEF,
  #                                          sep = "-")))

  ##http://stackoverflow.com/questions/267399/how-do-you-match-only-valid-roman-numerals-with-a-regular-expression
  ##icd.10[grep("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\\.", icd.10$ICDTitle),]$Code

  ##icd.10[grep("-", icd.10$Code),]$Code


  icd.10 <- subset(icd.10, str_length(Code) == 3)
  deaths <- merge(deaths, icd.10, by.x = "CAUSADEF", by.y = "Code", all.x = TRUE)

  deaths$CDEATH <- str_c(deaths$CAUSADEF, deaths$DESDOBLA)
  deaths$DESDOBLA <- as.numeric(deaths$DESDOBLA)










  mmatrixcodes <- c("W25-W29, W45, X78, X99, Y28, Y35.4",
                    "W65-W74, X71, X92, Y21",
                    "W00-W19, X80, Y01, Y30",
                    "X00-X19, X76-X77, X97-X98, Y26-Y27, Y36.3, *U01.2",
                    "W32-W34, X72-X74, X93-X95, Y22-Y24, Y35.0, *U01.4",
                    "W24, W30-W31",
                    "V01-V99, X82, Y03, Y32, Y36.1, *U01.1",
                    "W42, W43, W53-W64, W92-W99, X20-X39, X51-X57",
                    "X50",
                    "X40-X49, X60-X69, X85-X90, Y10-Y19, Y35.2, *U01.6, *U01.7)",
                    "W20-W22, W50-W52, X79, Y00, Y04, Y29, Y35.3",
                    "W75-W84, X70, X91, Y20",
                    "W23, W35-W41, W44, W49, W85-W91, Y85, X75, X81, X96, Y02, Y05-Y07, Y25, Y31, Y35.1, Y35.5, Y36.0, Y36.2, Y36.4, Y36.5, Y36.6, Y36.7, Y36.8, Y36.4",
                    "X58, Y86, X83, Y87.0, Y08, Y87.1, Y33, Y87.2, Y35.6, Y89.0, Y89.1, *U01.8, *U02",
                    "X59, X84, Y09, Y34, Y89.9, Y35.7, Y36.9, *U01.9, *U03.9",
                    "Y40-Y59, Y60-Y84, Y88")

  death.names <- c("Cut/pierce",
                   "Drowning",
                   "Fall",
                   "Fire/ hot object or substance",
                   "Firearm",
                   "Machinery",
                   "All Transport",
                   "Natural /environmental",
                   "Overexertion",
                   "Poisoning",
                   "Struck by or against",
                   "Suffocation",
                   "Other specified, classifiable",
                   "Other specified, nec",
                   "Unspecified",
                   "Adverse effects")



  deaths$CAUSE <- NA
  for(i in 1:length(mmatrixcodes)) {
    deaths <- MortalityMatrix(deaths, mmatrixcodes[i], death.names[i])
  }


  ##names(deaths) <- c("yob", "mob", "dob", "sex", "age_unit", "age", "nation",
  ## "marital", "stateL", "countyL", "locationL", "popL", "job", "edu", "derhab",
  ## "statD", "countyD", "locationD", "popD", "placeD", "yod", "mod", "dod",
  ## "hod", "minod", "med_help", "cod", "des", "presume", "working", "injury_loc",
  ## "domestic_v", "autopsy", "certifier", "state_reg", "county_reg", "year_reg",
  ## "mon_reg", "day_reg", "weight", "year_cert", "mon_cert", "day_cert", 
  ## "pregnant", "labor_cod", "labor_c")




  ##head(deaths)
  ##deaths$UrbanArea <- deaths$MA
  ##deaths[is.na(deaths$UrbanArea),]$UrbanArea <- str_c(deaths[is.na(deaths$UrbanArea),]$ENTOCU,
  ##deaths[is.na(deaths$UrbanArea),]$MUNOCU)
  ##ddply(deaths, .(UrbanArea, ICDTitle), function(df) nrow)


########################################################
  ##Section
########################################################


  deaths$CAUSECDC <- deaths$CAUSE
  sequence <- c(CDeathSeq("V30", "V90", 4, 9),
                str_c("V81", 1),
                str_c("V82", 1),
                CDeathSeq("V83", "V86", 0, 3),
                CDeathSeq("V20", "V28", 3, 9),
                CDeathSeq1("V29", 4, 9),
                CDeathSeq("V12", "V14", 3, 9),
                CDeathSeq1("V19", 4, 6),
                str_c(ICDseq("V02", "V04"), 1),
                str_c(ICDseq("V02", "V04"), 9),
                "V092",
                CDeathSeq1("V80", 3, 5),
                CDeathSeq1("V87", 0, 8),
                "V892")
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Motor Vehicle Traffic",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Motor Vehicle Traffic",
      silent = TRUE)

  sequence <- c('V10','V11','V15','V16','V17','V18',
                CDeathSeq("V12", "V14", 0, 2),
                CDeathSeq1("V19", 0, 3),
                "V199", "V198")
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Pedal cyclist, other",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Pedal cyclist, other",
      silent = TRUE)

  sequence <- c('V01','V05','V06',
                "V020", "V030", "V040",
                "V090","V091","V093","V099")
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Pedestrian, other",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Pedestrian, other",
      silent = TRUE)

  sequence <- c(CDeathSeq("V20", "V28", 0, 2),
                CDeathSeq1("V29", 0, 3),
                CDeathSeq("V30", "V79", 0, 3),
                CDeathSeq1("V80", 0, 2),
                CDeathSeq1("V80", 6, 9),
                "V810", "V820",
                CDeathSeq("V81", "V82", 2, 9),
                CDeathSeq("V83", "V86", 4, 9),
                "V879",
                CDeathSeq1("V88", 0, 9),
                "V890","V891","V893","V899",
                "X82", "Y03", "Y32")
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Other land transport",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Other land transport",
      silent = TRUE)

  sequence <- c(ICDseq("V90", "V99"),
                "Y361", "U011")
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Other Transport",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Other Transport",
      silent = TRUE)




  deaths$MVTPER <- NA
  sequence <- c(CDeathSeq("V30", "V79", 4, 9),
                CDeathSeq("V83", "V86", 0, 3)
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$MVTPER <-  "Occupant",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$MVTPER <-  "Occupant",
      silent = TRUE)


  sequence <- c(CDeathSeq("V20", "V28", 3, 9),
                CDeathSeq1("V29", 4, 9)
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$MVTPER <-  "Motorcyclist",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$MVTPER <-  "Motorcyclist",
      silent = TRUE)


  sequence <- c(CDeathSeq("V12", "V14", 3, 9),
                CDeathSeq1("V19", 4, 6)
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$MVTPER <-  "Pedal cyclist",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$MVTPER <-  "Pedal cyclist",
      silent = TRUE)


  sequence <- c(str_c(ICDseq("V02", "V04"), 1),
                str_c(ICDseq("V02", "V04"), 9),
                "V092")
  try(deaths[deaths$CAUSADEF %in% sequence,]$MVTPER <-  "Pedestrian",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$MVTPER <-  "Pedestrian",
      silent = TRUE)


  sequence <- c(CDeathSeq1("V80", 3, 5),
                "V811", "V821"
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$MVTPER <-  "Other",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$MVTPER <-  "Other",
      silent = TRUE)


  sequence <- c(CDeathSeq1("V87", 0, 8),
                "V892"
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$MVTPER <-  "Unspecified",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$MVTPER <-  "Unspecified",
      silent = TRUE)





  deaths$MUNOCU2 <- deaths$MUNOCU
  ##Tulum was created from Solidaridad
  try(deaths[which(deaths$ENTOCU == 23 & deaths$MUNOCU == 9),]$MUNOCU2 <- 008,
      silent = TRUE)
  ##Replace San Ignacio Cerro Gordo with Arandas to match the population data
  try(deaths[which(deaths$ENTOCU == 14 & deaths$MUNOCU == 125),]$MUNOCU2 <- 008,
      silent = TRUE)

  ##Assign each municipality a code based on the state and muncipality id
  deaths$id <- with(deaths, str_c(format(ENTOCU, width = 2),
                                  format(MUNOCU2, width = 3)))
  deaths$id <- as.numeric(gsub(" ", "0", deaths$id))

  #metropolitan.areas <- read.csv("data/metropolitan-areas.csv")
  deaths <- join(deaths, metropolitan.areas)
  
  #deaths$Block <- NA
  #blocks <- data.frame()
  #for(i in 1:nrow(icd.blocks)) {
  #  sequence <- ICDseq(icd.blocks[i,1], icd.blocks[i,2])
  #  x <- data.frame(CAUSADEF = sequence, Block = icd.blocks[i,3])
  #  blocks <- rbind(blocks, x)
  #}

  #join(deaths, blocks)

  deaths$date <- with(deaths, parse_date(str_c(ANIODEF, MESDEF, DIADEF,
                                            sep = "-"),
                      c("%Y", "%m", "%d"), seps = "-"))
  deaths$date2 <- deaths$date
  deaths$date2[which(is.na(deaths$date))] <- with(deaths[which(is.na(deaths$date)),],
                                                 parse_date(str_c(ANIOREG, MESREG, DIAREG,
                                                               sep = "-"),
                      c("%Y", "%m", "%d"), seps = "-"))
  ##Invalid dates (e.g February 31) are assumed as having happened on the 15th of each month
  deaths$date2[which(is.na(deaths$date))] <- with(deaths[which(is.na(deaths$date)),],
                                                 parse_date(str_c(ANIOREG, MESREG, "15",
                                                               sep = "-"),
                      c("%Y", "%m", "%d"), seps = "-"))
  
  ##Age in years
  deaths$Age <- deaths$EDADVALOR
  ##If the age unit is month, hour or day then set to 0
  deaths[which(deaths$EDADUNI == "M"),]$Age <- 0
  deaths[which(deaths$EDADUNI == "H"),]$Age <- 0
  deaths[which(deaths$EDADUNI == "D"),]$Age <- 0
  ##NA values
  try(deaths[which(deaths$EDADUNI == "M" & deaths$EDADVALOR == 98),]$Age <- NA,
      silent = TRUE)
  try(deaths[which(deaths$EDADUNI == "H" & deaths$EDADVALOR == 98),]$Age <- NA,
      silent = TRUE)
  try(deaths[which(deaths$EDADUNI == "H" & deaths$EDADVALOR == 97),]$Age <- NA,
      silent = TRUE)
  try(deaths[which(deaths$EDADUNI == "D" & deaths$EDADVALOR == 98),]$Age <- NA,
      silent = TRUE)
  try(deaths[which(deaths$EDADVALOR == 998),]$Age <- NA,
      silent = TRUE)

  deaths$CDEATH <- NULL
  names(deaths) <- c("icd3", "yob", "mob", "dob", "sex",
                 "age.unit", "age.in.units", "nation", "marital", "state.of.residence",
                 "county.of.residence", "loc.of.residence", "loc.of.residence.size", "job", "edu",
                 "insurance", "state.of.death", "county.of.death", "loc.of.death", "loc.of.death.size",
                 "place.of.injury04", "yod", "mod", "dod", "hod",
                 "minod", "med.help", "icd4",  "injury.intent", "job",
                 "place.of.injury", "domestic.v", "autopsy", "certifier", "state.reg",
                 "county.reg", "year.reg", "mon.reg", "day.reg", "weight",
                 "year.cert", "mon.cert", "day.cert", "pregnancy.condition", "pregnancy.related",
                 "pregnancy.complication",
                 "state.abbrev", "icd.title", "cause", 
"causecdc", "mvtper", "county.of.death10", "fips", "metro.area", "date", "date2", "age")

  return(deaths)
}

##SQLite
drv <- dbDriver("SQLite")
tfile <- file.path("clean-data", "mortality-database.sqlite")
unlink(tfile)
con <- dbConnect(drv, dbname = tfile)

message("Adding fields to the mortality databases...\n")

di2004 <- read.csv("clean-data/di2004.sinais.csv.bz2")
di2004 <- cleanDeaths(di2004)
##write.csv(di2004, bzfile("clean-data/mortality04.csv.bz2"), row.names = FALSE)
dbWriteTable(con, "mortality", di2004, append = TRUE, row.names = FALSE)
di2004 <- t <- subset(di2004, !injury.intent %in% c("Natural Death"))

di2005 <- read.csv("clean-data/di2005.sinais.csv.bz2")
di2005 <- cleanDeaths(di2005)
##write.csv(di2005, bzfile("clean-data/mortality05.csv.bz2"), row.names = FALSE)
dbWriteTable(con, "mortality", di2005, append = TRUE, row.names = FALSE)
di2005 <- subset(di2005, !injury.intent %in% c("Natural Death"))

di2006 <- read.csv("clean-data/di2006.sinais.csv.bz2")
di2006 <- cleanDeaths(di2006)
dbWriteTable(con, "mortality", di2006, append = TRUE, row.names = FALSE)
##write.csv(di2006, bzfile("clean-data/mortality06.csv.bz2"), row.names = FALSE)
di2006 <- subset(di2006, !injury.intent %in% c("Natural Death"))

di2007 <- read.csv("clean-data/di2007.sinais.csv.bz2")
di2007 <- cleanDeaths(di2007)
##write.csv(di2007, bzfile("clean-data/mortality07.csv.bz2"), row.names = FALSE)
dbWriteTable(con, "mortality", di2007, append = TRUE, row.names = FALSE)
di2007 <- subset(di2007, !injury.intent %in% c("Natural Death"))

di2008 <- read.csv("clean-data/di2008.sinais.csv.bz2")
di2008 <- cleanDeaths(di2008)
dbWriteTable(con, "mortality", di2008, append = TRUE, row.names = FALSE)
##write.csv(di2008, bzfile("clean-data/mortality08.csv.bz2"), row.names = FALSE)
di2008 <- subset(di2008, !injury.intent %in% c("Natural Death"))

di2009 <- read.csv("clean-data/di2009.sinais.csv.bz2")
di2009 <- cleanDeaths(di2009)
dbWriteTable(con, "mortality", di2009, append = TRUE, row.names = FALSE)
##write.csv(di2009, bzfile("clean-data/mortality09.csv.bz2"), row.names = FALSE)
di2009 <- subset(di2009, !injury.intent %in% c("Natural Death"))

di2010 <- read.csv("clean-data/di2010.sinais.csv.bz2")
di2010 <- cleanDeaths(di2010)
dbWriteTable(con, "mortality", di2010, append = TRUE, row.names = FALSE)
##write.csv(di2010, bzfile("clean-data/mortality10.csv.bz2"), row.names = FALSE)
di2010 <- subset(di2010, !injury.intent %in% c("Natural Death"))



dbDisconnect(con)

#Add new data.frames for 2010 here:
deaths <- rbind(di2004,
                di2005,
                di2006,
                di2007,
                di2008,
                di2009,
                di2010)

rm(di2004)
rm(di2005)
rm(di2006)
rm(di2007)
rm(di2008)
rm(di2009)
rm(di2010)

#figure out the last year for which data is available
last.year <- max(subset(deaths, year.of.death < 2100)$year.of.death)
deaths <- subset(deaths, year.of.death %in% c("0", "0000", 2004:last.year))

##temp <- deaths
##names(deaths)

#subset(icd.10, str_length(Code) == 7)

write.csv(deaths, file = bzfile(file.path("clean-data", "injury-intent.csv.bz2")),
          row.names = FALSE)

##"subcategory",
