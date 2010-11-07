
#Add new data.frames for 2009 here:
hom <- rbind(di2006,
             di2007,
             di2008)

#Deaths with no year of occurance, narco-mines,
#too decomposed, etc
#Assume the deaths occured on the year they were registered
hom[which(hom$ANIODEF == 0),]$ANIODEF <- hom[which(hom$ANIODEF == 0),]$ANIOREG

#figure out the last year for which data is available
last.year <- max(subset(hom, ANIODEF < 2100)$ANIODEF)
hom <- subset(hom, ANIODEF %in% c(2006:last.year))


#Nice variable names
hom$SEXOtxt <- car::recode(hom$SEXO, "1 = 'Males'; 2 = 'Females'")

hom$LUGLEStxt <- car::recode(hom$LUGLES,  "0 = 'Home';
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

hom$ESCOLtxt <- car::recode(hom$ESCOL, "0 = 'Unknown';
                                1 = 'No schooling';
                                2 = 'Grade School Incomplete';
                                3 = 'Grade School Completed';
                                4 = 'Secundaria Incomplete';
                                5 = 'Secundaria Completed';
                                6 = 'Preparatoria';
                                7 = 'College';
                                8 = 'NA (less than 6 years old)'")

hom$OCUPACIONtxt <- car::recode(hom$OCUPACION, "0 = 'Unknown';
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

hom$EDOCIVILtxt <- car::recode(hom$EDOCIVIL, "0 = 'Unknown';
                                      1 = 'Single';
                                      2 = 'Widow';
                                      3 = 'Divorced';
                                      4 = 'Living Together';
                                      5 = 'Married';
                                      8 = 'NA';")
