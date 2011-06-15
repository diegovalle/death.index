ICDseq <- function(start, end) {
  letter <- str_sub(start, 1, 1)
  number1 <-  as.numeric(str_sub(start, 2, length(str) + 2))
  number2 <- as.numeric(str_sub(end, 2, length(str) + 2))
  str_c(letter, gsub(" ", "0",
                     format(number1:number2, width = 2)))
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

di2004 <- read.csv("clean-data/di2004.csv")
di2005 <- read.csv("clean-data/di2005.csv")
di2006 <- read.csv("clean-data/di2006.csv")
di2007 <- read.csv("clean-data/di2007.csv")
di2008 <- read.csv("clean-data/di2008.csv")
di2009 <- read.csv("clean-data/di2009.csv")

di2004 <- subset(di2004, PRESUNTO %in% c(1:5))
di2005 <- subset(di2005, PRESUNTO %in% c(1:5))
di2006 <- subset(di2006, PRESUNTO %in% c(1:5))
di2007 <- subset(di2007, PRESUNTO %in% c(1:5))
di2008 <- subset(di2008, PRESUNTO %in% c(1:5))
di2009 <- subset(di2009, PRESUNTO %in% c(1:5))

#Add new data.frames for 2010 here:
deaths <- rbind(di2004,
                di2005,
                di2006,
                di2007,
                di2008,
                di2009)

rm(di2004)
rm(di2005)
rm(di2006)
rm(di2007)
rm(di2008)
rm(di2009)

#figure out the last year for which data is available
last.year <- max(subset(deaths, ANIODEF < 2100)$ANIODEF)
deaths <- subset(deaths, ANIODEF %in% c(0, 2004:last.year))


#Max year and min year
kminy <- min(deaths$ANIODEF)
kmaxy <- max(deaths$ANIODEF)
last.day <- as.Date(str_c(kmaxy, "12", "31", sep = "-"))


#Nice variable names
deaths$SEXOtxt <- car::recode(deaths$SEXO, "1 = 'Male'; 2 = 'Female'")

deaths$LUGLEStxt <- car::recode(deaths$LUGLES,  "0 = 'Home';
                                   1 = 'Residential Institution';
                                   2 = 'School or Office';
                                   3 = 'Sporting Area';
                                   4 = 'Public Street';
                                   5 = 'Commercial Area';
                                   6 = 'Industrial Area';
                                   7 = 'Farm';
                                   8 = 'Other';
                                   9 = 'Unknown';
                                   88 = 'Natural Death';")

deaths$ESCOLtxt <- car::recode(deaths$ESCOL, "0 = 'Unknown';
                                1 = 'No schooling';
                                2 = 'Grade School Incomplete';
                                3 = 'Grade School Completed';
                                4 = 'Secundaria Incomplete';
                                5 = 'Secundaria Completed';
                                6 = 'Preparatoria';
                                7 = 'College';
                                8 = 'NA (less than 6 years old)'")

deaths$OCUPACIONtxt <- car::recode(deaths$OCUPACION, "0 = 'Unknown';
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
    98 = 'NA (less than 12 years old)';
    99 = 'Insufficiently specified';")

deaths$EDOCIVILtxt <- car::recode(deaths$EDOCIVIL, "0 = 'Unknown';
                                      1 = 'Single';
                                      2 = 'Widow';
                                      3 = 'Divorced';
                                      4 = 'Living Together';
                                      5 = 'Married';
                                      8 = 'NA';")


deaths$NECROPCIAtxt <- car::recode(deaths$NECROPCIA, "0 = 'Unknown';
                                      1 = 'Yes';
                                      2 = 'No';")

deaths$PRESUNTOtxt <- car::recode(deaths$PRESUNTO, "1 = 'Accident';
                                      2 = 'Homicide';
                                      3 = 'Suicide';
4 = 'Unknown';
5 = 'Legal intervention, operations of war, military operations, and terrorism';
8 = 'Natural death';")

deaths$CERTIFtxt <- car::recode(deaths$CERTIF, "0 = 'Unknown';
                                      1 = 'Attending Physician';
                                      2 = 'Forensic Doctor';
                                      3 = 'Other Doctor';
                                      4 = 'SSA Personnel';
                                      5 = 'Civil Authority';
                                      8 = 'Other';")

addAbbrv <- function(df) {
  df$ABBRV <- car::recode(df$ENTOCU, "1 = 'Ags';
                                 2 = 'BC';
                                 3 = 'BCS';
                                 4 = 'Camp';
                                 5 = 'Coah';
                                 6 = 'Col';
                                 7 = 'Chis';
                    8 = 'Chih';
                    9 = 'DF';
                    10 = 'Dgo';
                    11 ='Gto';
                    12 ='Gro';
                    13 ='Hgo';
                    14 ='Jal';
                    15 ='Mex';
                    16 ='Mich';
                    17 ='Mor';
                    18 ='Nay';
                    19 ='NL';
                    20 ='Oax';
                    21 ='Pue';
                    22 ='Qro';
                    23 ='QR';
                    24 ='SLP';
                    25 ='Sin';
                    26 ='Son';
                    27 ='Tab';
                    28 ='Tamps';
                    29 ='Tlax';
                    30 ='Ver';
                    31 ='Yuc';
                    32 = 'Zac'")
  df
}
deaths <- addAbbrv(deaths)
deaths <- subset(deaths, ENTOCU != 33)

#Assign each municipality a code based on the state and muncipality id
deaths$id <- with(deaths, str_c(format(ENTOCU, digits = 2),
                                        format(MUNOCU, digits = 3)))
deaths$id <- as.numeric(gsub(" ", "0", deaths$id))

metropolitan.areas <- read.csv("data/metropolitan-areas.csv")
deaths <- join(deaths, metropolitan.areas)
deaths$date <- with(deaths, as.Date(str_c(ANIODEF, MESDEF, DIADEF,
                                          sep = "-")))



icd.10 <- subset(icd.10, str_length(Code) == 3)
deaths <- merge(deaths, icd.10, by.x = "CAUSADEF", by.y = "Code")

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

write.csv(deaths, file = bzfile(file.path("clean-data", "violent-deaths.csv.bz2")),
          row.names = FALSE)

ddply(deaths, .(CAUSE), nrow)

ddply(deaths, .(PRESUNTOtxt, ANIODEF), nrow)

ddply(hom, .(CAUSE), nrow)

levels(factor(subset(deaths, is.na(CAUSE))$CAUSADEF))
levels(factor(subset(deaths, is.na(CAUSE))$ICDTitle.x))
