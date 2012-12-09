clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  variable.name <- gsub('(^[0-9]+$)', 'X\\1', variable.name, perl = TRUE)
  return(variable.name)
}

stateToAbbrev <- function(v) {
  car::recode(v, "
                    0 = NA;
                    1 = 'Ags';
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
                    32 = 'Zac';
                    33 = 'USA';
                    34 = 'LATAM';
                    35 = 'Other';")
}
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
                    32 = 'Zac';;
                    33 = 'USA';
                    34 = 'LATAM';
                    35 = 'Other';")
  df
}


stateToAbbrev <- function(v) {
  car::recode(v, "
                    0 = NA;
                    1 = 'Ags';
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
                    32 = 'Zac';
                    33 = 'USA';
                    34 = 'LATAM';
                    35 = 'Other';")
}

