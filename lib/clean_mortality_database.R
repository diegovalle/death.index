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
                                1 = 'No Schooling';
                                2 = 'Grade School Incomplete';
                                3 = 'Grade School Completed';
                                4 = 'Secundaria Incomplete';
                                5 = 'Secundaria Completed';
                                6 = 'Preparatoria';
                                7 = 'Tertiary Education';
                                8 = 'Not App. (< 6 yrs old)'")

  deaths$OCUPACION <- car::recode(deaths$OCUPACION, "0 = NA;
    2 = 'Inactive';
    11 = 'Professional';
    12 = 'Technician';
    13 = 'Education';
    14 = 'Arts and Sports';
    21 = 'Public, Private and Social Sectors';
    41 = 'Agropecuary';
    51 = 'Industrial Activities (Foremen)';
    52 = 'Industrial Production (Workers)';
    53 = 'Industrial Production (Machine Operators)';
    54 = 'Industrial Production (Helpers)';
    55 = 'Transportation';
    61 = 'Administrators';
    62 = 'Helpers in Administrative Activities';
    71 = 'Sales';
    72 = 'Street Salesman';
    81 = 'Personal Services';
    82 = 'Domestic Services';
    83 = 'Armed Forces, Protection and Private Security';
    98 = 'Not App. (< 12 years old)';
    99 = 'Insufficiently Specified';")

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
                                      8 = 'Natural Death';")

  deaths$CERTIF <- car::recode(deaths$CERTIF, "0 = NA;
                                      1 = 'Attending Physician';
                                      2 = 'Forensic Doctor';
                                      3 = 'Other Doctor';
                                      4 = 'SSA Personnel';
                                      5 = 'Civil Authority';
                                      8 = 'Other';")


  deaths$NACION <- car::recode(deaths$NACION, "1 = 'Mexican';
                                               2 = 'Foreigner';")

  deaths$TAMLOCRH[deaths$TAMLOCRH == 0] <- NA
  deaths$TAMLOCRH[deaths$TAMLOCRH == 1] <- 999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 2] <- 1999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 3] <- 2999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 4] <- 4999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 5] <- 9999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 6] <- 14999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 7] <- 19999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 8] <- 29999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 9] <- 39999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 10] <- 49999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 11] <- 74999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 12] <- 99999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 13] <- 249999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 14] <- 499999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 15] <- 99999999
  deaths$TAMLOCRH[deaths$TAMLOCRH == 18] <- 0

  deaths$TAMLOCOC[deaths$TAMLOCOC == 0] <- NA
  deaths$TAMLOCOC[deaths$TAMLOCOC == 1] <- 999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 2] <- 1999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 3] <- 2999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 4] <- 4999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 5] <- 9999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 6] <- 14999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 7] <- 19999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 8] <- 29999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 9] <- 39999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 10] <- 49999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 11] <- 74999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 12] <- 99999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 13] <- 249999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 14] <- 499999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 15] <- 99999999
  deaths$TAMLOCOC[deaths$TAMLOCOC == 18] <- 0
  
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
                                                 8 = 'Not Homicide';")
  
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
                                        8 = 'Not App.';")
  deaths$COMPLICARO <- car::recode(deaths$COMPLICARO, "
                                        1 = 'Yes';
                                        2 ='No';
                                        8 = 'Not App.';")
  
  deaths$MUNOCU[which(deaths$MUNOCU == 0)] <- NA
  deaths$MUNRH[which(deaths$MUNRH == 0)] <- NA
  deaths$LOCOCU[which(deaths$LOCOCU == 0)] <- NA
  deaths$LOCRH[which(deaths$LOCRH == 0)] <- NA
  

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
  deaths <- join(deaths, metropolitan.areas, by = "id")
  
  #deaths$Block <- NA
  #blocks <- data.frame()
  #for(i in 1:nrow(icd.blocks)) {
  #  sequence <- ICDseq(icd.blocks[i,1], icd.blocks[i,2])
  #  x <- data.frame(CAUSADEF = sequence, Block = icd.blocks[i,3])
  #  blocks <- rbind(blocks, x)
  #}

  #join(deaths, blocks)

  
  
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

  
  deaths$CAUSADEF <- NULL
  deaths$DESDOBLA <- NULL
  deaths$SITIO_LES <- NULL
  
  names(deaths) <- c("yob", "mob", "dob", "sex",
                 "age_unit", "age_in_units", "nationality", "marital", "state_res",
                 "mun_res", "loc_res", "loc_res_size", "job", "edu",
                 "insurance", "state_death", "mun_death", "loc_death", "loc_death_size",
                 "yod", "mod", "dod", "hod",
                 "minod", "med_help",  "intent", "during_job", 
                 "place_injury", "domestic_v", "autopsy", "certifier", "state_reg",
                 "mun_reg", "year_reg", "mon_reg", "day_reg", "weight",
                 "year_cert", "mon_cert", "day_cert", "pregnancy_condition", "pregnancy_related",
                 "pregnancy_complication",
                 "icd_title", "icd4", "cause", 
"cause_detail", "mv_detail", "mun_death2", "fips", "metro_area", "age")

  try({deaths$date <- with(deaths,
                          as.Date(str_c(yod, mod, dod,
                                            sep = "-"),
                      "%Y-%m-%d"))

  ## date.reg <- with(deaths, parse_date(str_c(year_reg, mon_reg, day_reg,
  ##                                            sep = "-"),
  ##                       c("%Y", "%m", "%d"), seps = "-"))
  
  ## mean.diff <- median(deaths$date - date.reg, na.rm = TRUE)
  ## mean.diff / 3600 /24

  deaths$date2 <- deaths$date
  deaths$date2[which(is.na(deaths$date))] <-
    with(deaths[which(is.na(deaths$date)),],
         as.Date(str_c(year_reg, mon_reg, day_reg,
                        sep = "-") ,
                 "%Y-%m-%d")) 
  
##Invalid dates (e.g February 31) are assumed as having happened on the 15th of each month
  deaths$date2[which(is.na(deaths$date))] <-
    with(deaths[which(is.na(deaths$date)),],
         as.Date(str_c(year_reg, mon_reg, "15",
                       sep = "-"),
                 "%Y-%m-%d"))

  })

  deaths$date_birth <- with(deaths,
                          as.Date(str_c(yob, mob, dob,
                                            sep = "-"),
                      "%Y-%m-%d"))
  

  deaths$date <- as.character(deaths$date)
  deaths$date_birth <- as.character(deaths$date_birth)
  deaths$date2 <- as.character(deaths$date2)

  deaths$yob[which(deaths$yob == 0)] <- NA
  deaths$mob[which(deaths$mob == 0)] <- NA
  deaths$dob[which(deaths$dob == 0)] <- NA

  deaths$yod[which(deaths$yod == 0)] <- NA
  deaths$dod[which(deaths$dod == 0)] <- NA
  deaths$hod[which(deaths$hod == 99)] <- NA
  deaths$minod[which(deaths$minod == 99)] <- NA

  
  deaths$marital <- as.factor(deaths$marital)
  deaths$nationality <- as.factor(deaths$nationality)
  deaths$icd4 <- as.factor(deaths$icd4)
  deaths$sex <- as.factor(deaths$sex)
  deaths$job <- as.factor(deaths$job)
  deaths$during_job <- as.factor(deaths$during_job)
  deaths$edu <- as.factor(deaths$edu)
  deaths$insurance <- as.factor(deaths$insurance)
  deaths$place_injury <- as.factor(deaths$place_injury)
  deaths$med_help <- as.factor(deaths$med_help)
  deaths$intent <- as.factor(deaths$intent)
  deaths$job <- as.factor(deaths$job)
  deaths$domestic_v <- as.factor(deaths$domestic_v)
  deaths$autopsy <- as.factor(deaths$autopsy)
  deaths$certifier <- as.factor(deaths$certifier)
  deaths$weight <- as.factor(deaths$weight)
  deaths$pregnancy_condition <- as.factor(deaths$pregnancy_condition)
  deaths$pregnancy_related <- as.factor(deaths$pregnancy_related)
  deaths$pregnancy_complication <- as.factor(deaths$pregnancy_complication)
  deaths$icd_title <- as.factor(deaths$icd_title)
  deaths$cause <- as.factor(deaths$cause)
  deaths$cause_detail <- as.factor(deaths$cause_detail)
  deaths$mv_detail <- as.factor(deaths$mv_detail)
  deaths$metro_area <- as.factor(deaths$metro_area)

  ##deaths$icd.title <- NULL
  return(deaths)
}

readAndClean <- function(file.name, con){
  message(str_c("\nCleaning ", str_replace(file.name, "di", ""), "\n"))
  if(!file.exists(file.path("cache", str_c(file.name, ".RData")))) {
    df <- read.csv(str_c("clean-data/", file.name, ".sinais.csv.bz2"))
    df <- cleanDeaths(df)
    save(df, file = file.path("cache", str_c(file.name, ".RData")))
  } else {
    load(file.path("cache", str_c(file.name, ".RData")))
  }
  ##write.csv(df, bzfile("clean-data/mortality04.csv.bz2"), row.names = FALSE)
  dbWriteTable(con, "mortality", df, append = TRUE, row.names = FALSE)
  ##df <- subset(df, !injury.intent %in% c("Natural Death"))
  ##return(df)
  NULL
}


##SQLite
drv <- dbDriver("SQLite")
morttfile <- file.path("clean-data", "mortality-database.sqlite")
if(file.exists(morttfile))
  unlink(morttfile)
con <- dbConnect(drv, dbname = morttfile)

message("Adding fields to the mortality databases...\n")
readAndClean("di2004", con)
readAndClean("di2005", con)
readAndClean("di2006", con)
readAndClean("di2007", con)
readAndClean("di2008", con)
readAndClean("di2009", con)
readAndClean("di2010", con)

##dbDisconnect(con)


#Add new data.frames for 2010 here:
## deaths <- rbind(di2004,
##                 di2005,
##                 di2006,
##                 di2007,
##                 di2008,
##                 di2009,
##                 di2010)

## rm(di2004)
## rm(di2005)
## rm(di2006)
## rm(di2007)
## rm(di2008)
## rm(di2009)
## rm(di2010)


##drv <- dbDriver("SQLite")
##mortfile <- file.path("clean-data/mortality-database.sqlite")
##con <- dbConnect(drv, dbname = tfile)
sql.query <- "select * from mortality 
              where intent != 'Natural Death' OR intent IS NULL"
message("subsetting injury intent data")
deaths <- dbGetQuery(con, sql.query)

dbDisconnect(con)


deaths$date <- as.Date(deaths$date)
deaths$date2 <- as.Date(deaths$date2)
deaths$date_birth<- as.Date(deaths$date_birth)

#figure out the last year for which data is available
last.year <- max(year(deaths$date), na.rm = TRUE)
deaths <- subset(deaths, year(deaths$date2) %in% 2004:last.year)
deaths <- subset(deaths, !state_death %in% c(33, 34, 35))



message("Saving injury intent data to csv and RData")
write.csv(deaths, file = bzfile(file.path("clean-data", "injury-intent.csv.bz2")),
          row.names = FALSE)
save(deaths, file = file.path("clean-data", "injury-intent.RData"))



