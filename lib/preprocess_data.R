
#Add new data.frames for 2009 here:
deaths <- rbind(di2006,
             di2007,
             di2008,
             di2009)
rm(di2006)
rm(di2007)
rm(di2008)
rm(di2009)
#Deaths with no year of occurance, narco-mines,
#too decomposed, etc
#Assume the deaths occured on the year they were registered
deaths[which(deaths$ANIODEF == 0),]$ANIODEF <- deaths[which(deaths$ANIODEF == 0),]$ANIOREG

#figure out the last year for which data is available
last.year <- max(subset(deaths, ANIODEF < 2100)$ANIODEF)
deaths <- subset(deaths, ANIODEF %in% c(2006:last.year))

#Max year and min year
kminy <- min(deaths$ANIODEF)
kmaxy <- max(deaths$ANIODEF)
last.day <- as.Date(str_c(kmaxy, "12", "31", sep = "-"))


#Nice variable names
deaths$SEXOtxt <- car::recode(deaths$SEXO, "1 = 'Males'; 2 = 'Females'")

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

#Assign each municipality a code based on the state and muncipality id
deaths$id <- with(deaths, str_c(format(ENTOCU, digits = 2),
                                        format(MUNOCU, digits = 3)))
deaths$id <- as.numeric(gsub(" ", "0", deaths$id))

metropolitan.areas <- read.csv("data/metropolitan-areas.csv")
deaths <- join(deaths, metropolitan.areas)
