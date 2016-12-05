
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
  colNames <- c("ENT_REGIS", "MUN_REGIS", "ENT_RESID", "MUN_RESID", "TLOC_RESID", 
                "LOC_RESID",
                "ENT_OCURR", "MUN_OCURR", "TLOC_OCURR", 
                "LOC_OCURR",
                "CAUSA_DEF", "LISTA_MEX", 
                "SEXO", "EDAD", "DIA_OCURR", "MES_OCURR", "ANIO_OCUR",
                "DIA_REGIS", 
                "MES_REGIS", "ANIO_REGIS", "DIA_NACIM", "MES_NACIM", "ANIO_NACIM", 
                "OCUPACION", "ESCOLARIDA", "EDO_CIVIL", "PRESUNTO", "OCURR_TRAB", 
                "LUGAR_OCUR", "NECROPSIA", "ASIST_MEDI", "SITIO_OCUR",
                "COND_CERT", 
                "NACIONALID", "DERECHOHAB", "EMBARAZO", "REL_EMBA", "HORAS", 
                "MINUTOS", "CAPITULO", "GRUPO", "LISTA1", "GR_LISMEX", "VIO_FAMI", 
                "AREA_UR", "EDAD_AGRU", "COMPLICARO", "DIA_CERT", "MES_CERT", 
                "ANIO_CERT", "MATERNAS", "LENGUA", "COND_ACT", "PAR_AGRE",
                "ENT_OCULES", 
                "MUN_OCULES", "LOC_OCULES", "RAZON_M", "DIS_RE_OAX", "PESO")
  addPath <- function(str) {
      str <- file.path("ssa-database", str)
  }
  toCSV <- function(i, di, year.sub) {
      ## Read a .dbf and save it to csv
      ## i = year to read and save
      message("writing year: ")
      message(i)
      di$PESO <- 8888
      df <- read.dbf(addPath(dbfiles[i-2003]), as.is = TRUE)
      ## Each year the INEGI adds new columns as they add more information
      ## use rbind.fill to add the missing columns as NA
      ## and then subset the 2012 data
      df <- rbind.fill(di, df)
      ##print(names(df))
      
      df <- subset(df, ANIO_REGIS != year.sub)
      df$PESO <- 8888
      #df$LOC_OCULES <- NA
      ## Discard columns that aren't in the first or last db
      ## browser()
      df <- df[,colNames]
      write.csv(df, bzfile(file.path("clean-data",
                                         str_c(i, ".inegi.csv.bz2"))),
                row.names = FALSE)
  }
  ##The directory where we stored the files
  dir <- "ssa-database/"
  ##The names of the database files
  dbfiles <- c(str_c("DEFUN0", 4:9, ".dbf"),
               str_c("DEFUN", 10:((last.year) %% 2000), ".dbf"))
  # The 2013 file is in lowercase
  #dbfiles <- str_replace(dbfiles, "DEFUN14.dbf", "DEFUN14.DBF")
  dbfiles <- str_replace(dbfiles, "DEFUN15.dbf", "DEFUN15.DBF")
 
  ##Names of the compressed files that contain the previous list of dbf's
  zipfiles <- paste0(2004:last.year, ".zip")
  zipfiles <- paste(dir, zipfiles, sep = "")
  
  if(!all(checkFile(str_c("clean-data/", 2004:last.year, ".sinais.csv.bz2")))) {
    message("unziping files from SINAIS...\n")
    mapply(extractFiles, zipfiles, dbfiles, dir)
    
    message("converting the SINAIS .dbf to .csv...\n")
    ##Write the csv files with all deaths
    diBegin <- read.dbf(addPath(dbfiles[1]), as.is = TRUE)
    diLast <- read.dbf(addPath(dbfiles[length(dbfiles)]), as.is = TRUE)
    
    for(i in 2004:(last.year-1)) {
        toCSV(i, diLast, last.year)
    }
    ## Write the last year
    toCSV(last.year, diBegin, 2004)
  }
})


