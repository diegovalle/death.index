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
  ##kminy <- min(deaths$ANIODEF)
  ##kmaxy <- max(deaths$ANIODEF)
  ##last.day <- as.Date(str_c(kmaxy, "12", "31", sep = "-"))


  ##Nice variable names
    deaths$SEXO <- car::recode(deaths$SEXO, "9 = NA;
                                              1 = 'Male';
                                              2 = 'Female';")
    deaths$LENGUA <- car::recode(deaths$LENGUA, "9 = NA;
                                              1 = 'Yes';
                                              2 = 'No';")
    deaths$COND_ACT <- car::recode(deaths$COND_ACT, "9 = NA;
                                              1 = 'Yes';
                                              2 = 'No';
                                              8 = 'Not App. (<5)';")
    
    deaths$SITIO_OCUR <- car::recode(deaths$SITIO_OCUR,  "99 = NA;
                                   1 = 'Secretaria de Salud';
                                   2 = 'IMSS Oportunidades';
                                   3 = 'IMSS';
                                   4 = 'ISSSTE';
                                   5 = 'PEMEX';
                                   6 = 'SEDENA';
                                   7 = 'SEMAR';
                                   8 = 'Other';
                                   9 = 'Private Hospital';
                                   10 = 'Public Street';
                                   11 = 'Home';
                                   12 = 'Other';")
    deaths$LUGAR_OCUR <- car::recode(deaths$LUGAR_OCUR,  "0 = 'Home';
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
    
    deaths$DERECHOHAB <- car::recode(deaths$DERECHOHAB, "99 = NA;
                                1 = 'None';
                                2 = 'IMSS';
                                3 = 'ISSSTE';
                                4 = 'PEMEX';
                                5 = 'SEDENA';
                                6 = 'SEMAR';
                                7 = 'Seguro Popular';
                                8 = 'Other'")
    if(deaths$ANIO_REGIS[1] == 2012){
        deaths$ESCOLARIDA <- car::recode(deaths$ESCOLARIDA, "99 = NA;
                                1 = 'No Schooling';
                                2 = 'Preschool';
                                3 = 'Primaria Incomplete';
                                4 = 'Primaria Completed';
                                5 = 'Secundaria Incomplete';
                                6 = 'Secundaria Complete';
                                7 = 'Preparatoria Incomplete';
                                8 = 'Preparatoria Complete';
                                9 = 'Profesional';
                                10 = 'Graduate Degree';
                                88 = 'Not App. (< 3 yrs old)'")
    } else {  
        deaths$ESCOLARIDA <- car::recode(deaths$ESCOLARIDA, "9 = NA;
                                1 = 'No Schooling';
                                2 = 'Less than 3 years';
                                3 = '3 to 5 years';
                                4 = 'Primaria Completed';
                                5 = 'Secundaria';
                                6 = 'Preparatoria';
                                7 = 'Profesional';
                                8 = 'Not App. (< 6 yrs old)'")
    }
    
    deaths$OCUPACION <- car::recode(deaths$OCUPACION, "99 = NA;
    2 = 'Inactive';
    11 = 'Professional';
    12 = 'Technician';
    13 = 'Education';
    14 = 'Arts and Sports';
    21 = 'Directors';
    41 = 'Agropecuary';
    51 = 'Industrial Activities (Foremen)';
    52 = 'Industrial Production (Workers)';
    53 = 'Industrial Production (Machine Operators)';
    54 = 'Industrial Production (Helpers)';
    55 = 'Transportation';
    61 = 'Administrators (intermediate)';
    62 = 'Administrators (inferior)';
    71 = 'Sales';
    72 = 'Street Salesman';
    81 = 'Personal Services';
    82 = 'Domestic Services';
    83 = 'Armed Forces, Protection and Private Security';
    97 = 'Not App. (< 12 years old)';
    98 = 'Insufficiently Specified';")
    
    deaths$EDO_CIVIL <- car::recode(deaths$EDO_CIVIL, "9 = NA;
                                      1 = 'Single';
                                      2 = 'Widow';
                                      3 = 'Divorced';
                                      4 = 'Living Together';
                                      5 = 'Married';
                                      8 = 'Not App. (< 12 yrs old)';")
    
    
    deaths$NECROPSIA <- car::recode(deaths$NECROPSIA, "9 = NA;
                                      1 = 'Yes';
                                      2 = 'No';")
    
    deaths$ASIST_MEDI <- car::recode(deaths$ASIST_MEDI, "9 = NA;
                                      1 = 'Yes';
                                      2 = 'No';")
    
    ## deaths$PESO <- car::recode(deaths$PESO, "0 = NA;
  ##                                     8888 = 'Not App. (Age > 1 day)';")

  deaths$PRESUNTO <- car::recode(deaths$PRESUNTO, "1 = 'Accident';
                                      2 = 'Homicide';
                                      3 = 'Suicide';
                                      4 = NA;
                                      5 = 'Legal Intervention';
                                      8 = 'Natural Death';")

  deaths$COND_CERT <- car::recode(deaths$COND_CERT, "9 = NA;
                                      1 = 'Attending Physician';
                                      2 = 'Forensic Doctor';
                                      3 = 'Other Doctor';
                                      4 = 'SSA Personnel';
                                      5 = 'Civil Authority';
                                      8 = 'Other';")


  deaths$NACIONALID <- car::recode(deaths$NACIONALID, "1 = 'Mexican';
                                               2 = 'Foreigner';
                                               9 = NA;")

  deaths$TLOC_RESID[deaths$TLOC_RESID == 99] <- NA
  deaths$TLOC_RESID[deaths$TLOC_RESID == 1] <- '1-999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 2] <- '1000-1999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 3] <- '2000-2999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 4] <- '3000-4999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 5] <- '5000-9999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 6] <- '10000-14999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 7] <- '15000-19999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 8] <- '20000-29999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 9] <- '30000-39999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 10] <- '40000-49999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 11] <- '50000-74999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 12] <- '75000-99999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 13] <- '100000-249999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 14] <- '250000-499999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 15] <- '500000-999999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 16] <- '1000000-1499999'
  deaths$TLOC_RESID[deaths$TLOC_RESID == 17] <- '>1500000'

    deaths$TLOC_OCURR[deaths$TLOC_OCURR == 99] <- NA
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 1] <- '1-999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 2] <- '1000-1999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 3] <- '2000-2999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 4] <- '3000-4999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 5] <- '5000-9999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 6] <- '10000-14999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 7] <- '15000-19999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 8] <- '20000-29999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 9] <- '30000-39999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 10] <- '40000-49999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 11] <- '50000-74999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 12] <- '75000-99999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 13] <- '100000-249999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 14] <- '250000-499999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 15] <- '500000-999999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 16] <- '1000000-1499999'
  deaths$TLOC_OCURR[deaths$TLOC_OCURR == 17] <- '>1500000'

  
  ## deaths$ASIST <- car::recode(deaths$ASIST, "1 = 'Yes';
  ##                                            2 = 'No';
  ##                                            0 = NA;")
  deaths$OCURR_TRAB <- car::recode(deaths$OCURR_TRAB, "8 = 'Natural Death';
                                                 2 = 'No';
                                                 1 = 'Yes';
                                                 9 = NA;")
  deaths$VIO_FAMI <- car::recode(deaths$VIO_FAMI, "9 = NA;
                                                 1 = 'Yes';
                                                 2 ='No';
                                                 8 = 'Not Homicide';")
  deaths$AREA_UR <- car::recode(deaths$AREA_UR, "9 = NA;
                                                 1 = 'Urban';
                                                 2 ='Rural';")

  ## deaths$EDAD_AGRU <- car::recode(deaths$EDAD_AGRU, "30 = NA;
  ##                                                1 = '<1';
  ##                                                2 ='1';
  ##                                                3 = '2'")

  
  deaths$EMBARAZO <- car::recode(deaths$EMBARAZO, "
                                      1 = 'Pregnancy';
                                      2 = 'Birth';
                                      3 = 'Puerperium';
                                      4 = '43 days to 11 months after giving birth';
                                      5 = 'No pregancy for 11 months';
                                      6 = 'Pregnant within year';
                                      8 = 'Not Applicable'")
  deaths$REL_EMBA <- car::recode(deaths$REL_EMBA, "
                                        1 = 'Yes';
                                        2 ='No';
                                        8 = 'Not App.';")
  deaths$COMPLICARO <- car::recode(deaths$COMPLICARO, "
                                        1 = 'Yes';
                                        2 ='No';
                                        8 = 'Not App.';")

  deaths$LENGUA <- car::recode(deaths$LENGUA, "
                                        1 = 'Yes';
                                        2 ='No';
                                        8 = NA;")

  deaths$COND_ACT <- car::recode(deaths$COND_ACT, "
                                        1 = 'Yes';
                                        2 ='No';
                                        8 = 'Not Applicable';
                                        9 = NA;")
  
  deaths$MUN_RESID[which(deaths$MUNOCU == 999)] <- NA
  deaths$MUN_OCURR[which(deaths$MUNOCU == 999)] <- NA
  deaths$MUN_REGIS[which(deaths$MUNRH == 999)] <- NA
  deaths$MUN_OCULES[which(deaths$MUNRH == 999)] <- NA

  deaths$ENT_RESID[which(deaths$MUNOCU == 99)] <- NA
  deaths$ENT_OCURR[which(deaths$MUNOCU == 99)] <- NA
  deaths$ENT_REGIS[which(deaths$MUNRH == 99)] <- NA
  deaths$ENT_OCULES[which(deaths$MUNRH == 99)] <- NA

  
  ##deaths$LOCOCU[which(deaths$LOCOCU == 999)] <- NA
  ##deaths$LOCRH[which(deaths$LOCRH == 999)] <- NA
  

  ##icd.10 <- subset(icd.10, str_length(Code) == 3)
  ##deaths <- merge(deaths, icd.10, by.x = "CAUSADEF", by.y = "Code", all.x = TRUE)

  deaths$CDEATH <- deaths$CAUSA_DEF
  deaths$CAUSADEF <- str_sub(deaths$CAUSA_DEF, 1,3) 










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
                   "Fire/hot object or substance",
                   "Firearm",
                   "Machinery",
                   "All Transport",
                   "Natural/environmental",
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

sequence <- c("X76", "X97", "Y26" , ICDseq("X00", "X09"))
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Fire/flame",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Fire/flame",
      silent = TRUE)

sequence <- c(ICDseq("X10", "X19"), "X77", "X98", "Y27")
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Hot object/substance",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Hot object/substance",
      silent = TRUE)

sequence <- c("Y880" , ICDseq("Y40", "Y59"))
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Adverse effects - Drugs",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Adverse effects - Drugs",
      silent = TRUE)

sequence <- c(ICDseq("Y60", "Y84"), CDeathSeq1("Y88", 1, 3))
  try(deaths[deaths$CAUSADEF %in% sequence,]$CAUSECDC <-  "Adverse effects - Medical care",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$CAUSECDC <-  "Adverse effects - Medical care",
      silent = TRUE)


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




  deaths$motor_vehicle_traffic <- NA
  sequence <- c(CDeathSeq("V30", "V79", 4, 9),
                CDeathSeq("V83", "V86", 0, 3)
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$motor_vehicle_traffic <-  "MVT - Occupant",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$motor_vehicle_traffic <-  "MVT - Occupant",
      silent = TRUE)


  sequence <- c(CDeathSeq("V20", "V28", 3, 9),
                CDeathSeq1("V29", 4, 9)
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$motor_vehicle_traffic <-  "MVT - Motorcyclist",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$motor_vehicle_traffic <-  "MVT - Motorcyclist",
      silent = TRUE)


  sequence <- c(CDeathSeq("V12", "V14", 3, 9),
                CDeathSeq1("V19", 4, 6)
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$motor_vehicle_traffic <-  "MVT - Pedal cyclist",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$motor_vehicle_traffic <-  "MVT - Pedal cyclist",
      silent = TRUE)


  sequence <- c(str_c(ICDseq("V02", "V04"), 1),
                str_c(ICDseq("V02", "V04"), 9),
                "V092")
  try(deaths[deaths$CAUSADEF %in% sequence,]$motor_vehicle_traffic <-  "MVT - Pedestrian",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$motor_vehicle_traffic <-  "MVT - Pedestrian",
      silent = TRUE)


  sequence <- c(CDeathSeq1("V80", 3, 5),
                "V811", "V821"
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$motor_vehicle_traffic <-  "MVT - Other",
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$motor_vehicle_traffic <-  "MVT - Other",
      silent = TRUE)


  sequence <- c(CDeathSeq1("V87", 0, 8),
                "V892"
                )
  try(deaths[deaths$CAUSADEF %in% sequence,]$motor_vehicle_traffic <-  NA,
      silent = TRUE)
  try(deaths[deaths$CDEATH %in% sequence,]$motor_vehicle_traffic <-  NA,
      silent = TRUE)





  deaths$MUNOCU2 <- deaths$MUN_OCURR
  ##Tulum was created from Solidaridad
  try(deaths[which(deaths$ENT_OCURR == 23 & deaths$MUN_OCURR == 9),]$MUNOCU2 <- 008,
      silent = TRUE)
  ##Bacalar was created from Othón P Blanco
  try(deaths[which(deaths$ENT_OCURR == 23 & deaths$MUN_OCURR == 10),]$MUNOCU2 <- 004,
      silent = TRUE)
  ##Replace San Ignacio Cerro Gordo with Arandas to match the population data
  try(deaths[which(deaths$ENT_OCURR == 14 & deaths$MUN_OCURR == 125),]$MUNOCU2 <- 008,
      silent = TRUE)

  ##Assign each municipality a code based on the state and muncipality id
  ## deaths$id <- with(deaths, str_c(format(ENTOCU, width = 2),
  ##                                 format(MUNOCU2, width = 3)))
  ## deaths$id <- as.numeric(gsub(" ", "0", deaths$id))
  ## deaths <- join(deaths, metropolitan.areas200k, by = "id")
  
  #deaths$Block <- NA
  #blocks <- data.frame()
  #for(i in 1:nrow(icd.blocks)) {
  #  sequence <- ICDseq(icd.blocks[i,1], icd.blocks[i,2])
  #  x <- data.frame(CAUSADEF = sequence, Block = icd.blocks[i,3])
  #  blocks <- rbind(blocks, x)
  #}

  #join(deaths, blocks)
  deaths$CDEATH <- NULL
  deaths$CAUSADEF <- NULL
  
  
  ##Age in years
  deaths$EDAD_ANIOS <- deaths$EDAD
  deaths[which(deaths$EDAD_ANIOS == 4998),]$EDAD_ANIOS <- NA
  deaths$EDAD_ANIOS <- deaths$EDAD_ANIOS - 4000
  deaths$EDAD_ANIOS[deaths$EDAD_ANIOS < 0] = 0



  deaths$ANIO_REGIS[which(deaths$ANIO_REGIS == 99)] <- NA
  deaths$MES_REGIS[which(deaths$MES_REGIS == 99)] <- NA
  deaths$DIA_REGIS[which(deaths$DIA_REGIS == 99)] <- NA

  deaths$ANIO_OCUR[which(deaths$ANIO_OCUR == 9999)] <- NA
  deaths$MES_OCURR[which(deaths$MES_OCURR == 99)] <- NA
  deaths$DIA_OCURR[which(deaths$DIA_OCURR == 99)] <- NA
  deaths$HORAS[which(deaths$HORAS == 99)] <- NA
  deaths$MINUTOS[which(deaths$MINUTOS == 99)] <- NA

  deaths$DIA_NACIM[which(deaths$DIA_NACIM == 99)] <- NA
  deaths$MES_NACIM[which(deaths$MES_NACIM == 99)] <- NA
  deaths$ANIO_NACIM[which(deaths$ANIO_NACIM == 9999)] <- NA

  ## Día en que se realiza el acto que proporciona certeza legal al acontecimiento y a las circunstancias en que ocurrió
  deaths$DIA_CERT[which(deaths$DIA_CERT == 99)] <- NA
  deaths$MES_CERT[which(deaths$MES_CERT == 99)] <- NA
  deaths$ANIO_CERT[which(deaths$ANIO_CERT == 9999)] <- NA
  #deaths$CAUSADEF <- NULL
  #deaths$DESDOBLA <- NULL
  ##deaths$SITIO_LES <- NULL
  
  ## names(deaths) <- c("year_birth", "month_birth", "day_birth", "sex",
  ##                "age_unit", "age_in_units", "nationality", "marital", "state_res",
  ##                "mun_res", "loc_res", "loc_res_size", "job", "edu",
  ##                "insurance", "state_occur", "mun_occur", "loc_occur", "loc_occur_size",
  ##                "place_occur", "year_occur", "month_occur", "day_occur", "hour_occur",
  ##                "min_occur", "med_help",  "intent", "during_job", 
  ##                "place_injury", "domestic_v", "autopsy", "certifier", "state_reg",
  ##                "mun_reg", "year_reg", "month_reg", "day_reg", "weight",
  ##                "year_cert", "month_cert", "day_cert", "pregnancy_condition",
  ##                    "pregnancy_related",
  ##                "pregnancy_complication",
  ##                "icd_title", "icd4", "cause", 
  ##                    "cause_detail", "motor_vehicle_traffic",
  ##                    "mun_occur2", "fips", "metro_area", "age")
  
  ## try({deaths$date_occur<- with(deaths,
  ##                         as.Date(str_c(year_occur, month_occur, day_occur,
  ##                                           sep = "-"),
  ##                     "%Y-%m-%d"))

       
  
  ##      deaths$date_occur[which(is.na(deaths$date_occur))] <-
  ##        with(deaths[which(is.na(deaths$date_occur)),],
  ##             as.Date(str_c(year_occur, month_reg, day_reg,
  ##                           sep = "-") ,
  ##                     "%Y-%m-%d")) 
  
  ##      ##Invalid dates (e.g February 31) are assumed as having happened on the 15th of each month
  ##      deaths$date_occur[which(is.na(deaths$date_occur))] <-
  ##        with(deaths[which(is.na(deaths$date_occur)),],
  ##             as.Date(str_c(year_occur, month_reg, "15",
  ##                           sep = "-"),
  ##                     "%Y-%m-%d"))

  ##    })

  ## try({deaths$date_reg <- with(deaths,
  ##                         as.Date(str_c(year_reg, month_reg, day_reg,
  ##                                           sep = "-"),
  ##                     "%Y-%m-%d"))

  
 
  ##      ##Invalid dates (e.g February 31) are assumed as having happened on the 15th of each month
  ##      deaths$date_reg[which(is.na(deaths$date_reg))] <-
  ##        with(deaths[which(is.na(deaths$date_reg)),],
  ##             as.Date(str_c(year_reg, month_reg, "15",
  ##                           sep = "-"),
  ##                     "%Y-%m-%d"))

  ##    })

 
  
  
  ## deaths$date_occur <- as.character(deaths$date_occur)
  ## deaths$date_reg <- as.character(deaths$date_reg)

  

  deaths$CAUSE <- car::recode(deaths$CAUSE, "'Unspecified' = NA;")
  deaths$CAUSECDC <- car::recode(deaths$CAUSECDC, "'Unspecified' = NA;")
  ##deaths$cause <- ifelse(deaths$cause == "Unspecified",
    ##                     NA,
    ##                     deaths$cause)
  ##deaths$cause_detail <- ifelse(deaths$cause_detail == "Unspecified",
    ##                            NA,
      ##                          deaths$cause_detail)
  
  #deaths <- convertFactors(deaths)

  ##deaths$icd.title <- NULL
  return(deaths)
}

convertFactors <- function(df) {
  df$age_unit <- as.factor(df$age_unit)
  df$marital <- as.factor(df$marital)
  df$nationality <- as.factor(df$nationality)
  df$icd4 <- as.factor(df$icd4)
  df$sex <- as.factor(df$sex)
  df$job <- as.factor(df$job)
  df$during_job <- as.factor(df$during_job)
  df$edu <- as.factor(df$edu)
  df$insurance <- as.factor(df$insurance)
  df$place_injury <- as.factor(df$place_injury)
  df$place_occur <- as.factor(df$place_occur)
  df$med_help <- as.factor(df$med_help)
  df$intent <- as.factor(df$intent)
  df$job <- as.factor(df$job)
  df$domestic_v <- as.factor(df$domestic_v)
  df$autopsy <- as.factor(df$autopsy)
  df$certifier <- as.factor(df$certifier)
  df$weight <- as.factor(df$weight)
  df$pregnancy_condition <- as.factor(df$pregnancy_condition)
  df$pregnancy_related <- as.factor(df$pregnancy_related)
  df$pregnancy_complication <- as.factor(df$pregnancy_complication)
  df$icd_title <- as.factor(df$icd_title)
  df$cause <- as.factor(df$cause)
  df$cause_detail <- as.factor(df$cause_detail)
  df$motor_vehicle_traffic <- as.factor(df$motor_vehicle_traffic)
  df$metro_area <- as.factor(df$metro_area)
  df
}

readAndClean <- function(file.name, con){
  message(str_c("\nCleaning ", file.name, "\n"))
  if(!file.exists(file.path("cache", str_c(file.name, ".RData")))) {
    df <- read.csv(str_c("clean-data/", file.name, ".inegi.csv.bz2"))
    df <- cleanDeaths(df)
    save(df, file = file.path("cache", str_c(file.name, ".RData")))
  } else {
    message("Loading from cache ('make clean-cache' if you don't want this to happen)'")
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

message("Adding fields to the mortality database...\n")
readAndClean("2004", con)
readAndClean("2005", con)
readAndClean("2006", con)
readAndClean("2007", con)
readAndClean("2008", con)
readAndClean("2009", con)
readAndClean("2010", con)
readAndClean("2011", con)
readAndClean("2012", con)

sql.query <- "select * from mortality
              where PRESUNTO != 'Natural Death' OR PRESUNTO IS NULL"
message("subsetting injury intent data")
deaths <- dbGetQuery(con, sql.query)
dbDisconnect(con)

deaths <- subset(deaths, !state_occur %in% c(33, 34, 35))
deaths$date_occur <- as.Date(deaths$date_occur)
deaths$date_reg <- as.Date(deaths$date_reg)
##deaths$date_birth<- as.Date(deaths$date_birth)

#figure out the last year for which data is available
##last.year <- max(year(deaths$date), na.rm = TRUE)
##deaths <- subset(deaths, year(deaths$date2) %in% 2004:last.year)


deaths <- convertFactors(deaths)
