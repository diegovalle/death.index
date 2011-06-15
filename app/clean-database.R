library(foreign)
library(plyr)
library(maptools)

########################################################
#These are the files you have to download
########################################################
#http://www.sinais.salud.gob.mx/descargas/zip/def2006.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2004.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2005.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2007.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2008.zip
#http://www.sinais.salud.gob.mx/descargas/zip/def2009.zip

extractFiles <- function(zipfile, dbfile, dir) {
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
  file.exists(paste("clean-data", name, sep = "/"))
}


dir <- "ssa-database/"
dbfiles <- c("INEGI_04_def_difusion.dbf",
             "inegi05.dbf",
             "BASE_DEF.DBF",
             "DEF-SSA07.dbf",
             "DEF-SSA08.DBF",
             "def_09.DBF"
             )
zipfiles <- c("def2004.zip",
              "def2005.zip",
              "def2006.zip",
              "def2007.zip",
              "def2008.zip",
              "def2009.zip")
zipfiles <- paste(dir, zipfiles, sep = "")


if(!(checkFile("di2004.csv") &
   checkFile("di2005.csv") &
   checkFile("di2006.csv") &
   checkFile("di2007.csv") &
   checkFile("di2008.csv") &
   checkFile("di2009.csv"))) {
  mapply(extractFiles, zipfiles, dbfiles, dir)

  #Write the csv files with all deaths
  x <- read.dbf("ssa-database/DEF-SSA08.DBF", as.is = TRUE)
  write.csv(x, "clean-data/di2008.csv", row.names = FALSE)

  x <- read.dbf("ssa-database/DEF-SSA07.dbf", as.is = TRUE)
  write.csv(x, "clean-data/di2007.csv", row.names = FALSE)

  x <- read.dbf("ssa-database/BASE_DEF.DBF", as.is = TRUE)
  write.csv(x, "clean-data/di2006.csv", row.names = FALSE)
  
  x1 <- read.dbf("ssa-database/def_09.DBF", as.is = TRUE)
  names <- names(x1) #save the column names for files before 2006
  write.csv(x1, "clean-data/di2009.csv", row.names = FALSE)

  x <- read.dbf("ssa-database/inegi05.dbf", as.is = TRUE)
  x$CAUSABAS <- NULL
  x$GBD <- NULL
  names(x) <- names(x1)
  write.csv(x, "clean-data/di2005.csv", row.names = FALSE)

  x <- read.dbf("ssa-database/INEGI_04_def_difusion.dbf", as.is = TRUE)
  names(x) <- names(x1)
  write.csv(x, "clean-data/di2004.csv", row.names = FALSE)
}



