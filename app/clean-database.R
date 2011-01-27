library(foreign)
library(plyr)
library(maptools)

########################################################
#These are the files you have to download
########################################################

#http://www.sinais.salud.gob.mx/descargas/zip/def2006.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2007.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2008.zip

checkFiles <- function(zipfile, dbfile, dir) {
        if(!file.exists(zipfile)) {
            cat("You need to download the death index files as described in README.md\n")            
            #Sys.sleep(100)
        }
        zip.file.extract(dbfile,
                         zipname = zipfile,
                         dir = gsub("/", "", dir))
        print(dbfile)
    

}

checkFile <- function(name) {
  file.exists(paste("data", name, sep = "/"))
}


dir <- "ssa-database/"
dbfiles <- c("BASE_DEF.DBF",
             "DEF-SSA07.dbf",
             "DEF-SSA08.DBF",
             "SALUD09.DBF")
zipfiles <- c("def2006.zip",
              "def2007.zip",
              "def2008.zip",
              "def2009.zip")
zipfiles <- paste(dir, zipfiles, sep = "")


if(!(checkFile("di2008.csv") &
   checkFile("di2007.csv") &
   checkFile("di2006.csv") &
   checkFile("di2009.csv"))) {
  mapply(checkFiles, zipfiles, dbfiles, dir)

  #Write the csv files with all deaths
  x <- read.dbf("ssa-database/DEF-SSA08.DBF", as.is = TRUE)
  write.csv(x, "data/di2008.csv", row.names = FALSE)

  x <- read.dbf("ssa-database/DEF-SSA07.dbf", as.is = TRUE)
  write.csv(x, "data/di2007.csv", row.names = FALSE)

  x <- read.dbf("ssa-database/BASE_DEF.DBF", as.is = TRUE)
  write.csv(x, "data/di2006.csv", row.names = FALSE)
  
  x <- read.dbf("ssa-database/SALUD09.DBF", as.is = TRUE)
  write.csv(x, "data/di2009.csv", row.names = FALSE)
}



