.packs <- c("testthat", 
            "yaml", 
            "foreign", 
            "reshape", 
            "plyr", 
            "stringr",
            "ggplot2", 
            "scales",
            "maptools",
            "directlabels",
            "fields",
            "car",
            "Cairo",
            "xtable",
            "stringr",
            "RColorBrewer",
            "shape",
            "Hmisc",
            "zoo",
            "stringr",
            "akima",
            "lubridate",
            "pscl",
            "rgdal",
            "maps",
            "maptools",
            "mapdata",
            "mapproj",
            "MASS",
            "spatstat",
            "compiler",
            "rbenchmark",
            "geosphere",
            "RSQLite",
            "xts",
            "caret",
            "VIM",
            "tools",
            "data.table",
            "ranger",
            "doMC",
            "svglite")
.success <- suppressWarnings(sapply(.packs, require, character.only = TRUE))
if (length(names(.success)[!.success])) {
  install.packages(names(.success)[!.success])
  sapply(names(.success)[!.success], require, character.only = TRUE)
}

options(stringsAsFactors = FALSE)



