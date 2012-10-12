
##Extract the dbf file from the compressed file
extractFiles <- function(zipfile, dbfile, dir) {
        if(!file.exists(zipfile)) {
            cat("You need to download the death index files as described in README.md\n")            
            #Sys.sleep(100)
        }
        unzip(files = dbfile,
              zipfile = zipfile,
              exdir = gsub("/", "", dir))
        print(dbfile)
    

}

##Don't unnecessarily extract a file twice
checkFile <- function(name) {
  file.exists(paste("clean-data", name, sep = "/"))
}


##Extract the files
local({

  ##The directory where we stored the files
  dir <- "ssa-database/"
  ##The names of the database files
  dbfiles <- c("INEGI_04_def_difusion.dbf",
               "inegi05.dbf",
               "BASE_DEF.DBF",
               "DEF-SSA07.dbf",
               "DEF-SSA08.DBF",
               "def_09.DBF",
               "def_10.dbf"
               )
  ##Name of the compressed files that contain the previous list of dbf's
  zipfiles <- c("def2004.zip",
                "def2005.zip",
                "def2006.zip",
                "def2007.zip",
                "def2008.zip",
                "def2009.zip",
                "def2010.zip")
  zipfiles <- paste(dir, zipfiles, sep = "")
  
  
  if(!(checkFile("di2004.sinais.csv.bz2") &
       checkFile("di2005.sinais.csv.bz2") &
       checkFile("di2006.sinais.csv.bz2") &
       checkFile("di2007.sinais.csv.bz2") &
       checkFile("di2008.sinais.csv.bz2") &
       checkFile("di2009.sinais.csv.bz2") &
       checkFile("di2010.sinais.csv.bz2"))) {
    message("unziping files from SINAIS...\n")
    mapply(extractFiles, zipfiles, dbfiles, dir)
    
    
    message("converting the SINAIS .dbf to .csv...\n")
    ##Write the csv files with all deaths
    di2008 <- read.dbf("ssa-database/DEF-SSA08.DBF", as.is = TRUE)
    write.csv(di2008, bzfile("clean-data/di2008.sinais.csv.bz2"), row.names = FALSE)
    ##save(di2008, file = "clean-data/di2008.RData")
    
    di2007 <- read.dbf("ssa-database/DEF-SSA07.dbf", as.is = TRUE)
    write.csv(di2007, bzfile("clean-data/di2007.sinais.csv.bz2"), row.names = FALSE)
    ##save(di2007, file = "clean-data/di2007.RData")
    
    di2006 <- read.dbf("ssa-database/BASE_DEF.DBF", as.is = TRUE)
    write.csv(di2006, bzfile("clean-data/di2006.sinais.csv.bz2"), row.names = FALSE)
    ##save(di2006, file = "clean-data/di2006.RData")

    di2009 <- read.dbf("ssa-database/def_09.DBF", as.is = TRUE)
    names <- names(di2009) #save the column names for files before 2006
    write.csv(di2009, bzfile("clean-data/di2009.sinais.csv.bz2"), row.names = FALSE)
    ##save(di2009, file = "clean-data/di2009.RData")

    di2010 <- read.dbf("ssa-database/def_10.dbf", as.is = TRUE)
    #names <- names(di2009) #save the column names for files before 2006
    write.csv(di2010, bzfile("clean-data/di2010.sinais.csv.bz2"), row.names = FALSE)

    di2005 <- read.dbf("ssa-database/inegi05.dbf", as.is = TRUE)
    di2005$CAUSABAS <- NULL
    di2005$GBD <- NULL
    names(di2005) <- names
    write.csv(di2005, bzfile("clean-data/di2005.sinais.csv.bz2"), row.names = FALSE)
    ##save(di2005, file = "clean-data/di2005.RData")

    di2004 <- read.dbf("ssa-database/INEGI_04_def_difusion.dbf", as.is = TRUE)
    names(di2004) <- names
    write.csv(di2004, bzfile("clean-data/di2004.sinais.csv.bz2"), row.names = FALSE)
    ##save(di2004, file = "clean-data/di2004.RData")
  }
})


