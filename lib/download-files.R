library(stringr)
library(plyr)
library(utils)
library(foreign)

########################################################
#These are the files to download
########################################################

files.url <- list("http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2004/Defunciones_Base_datos_2004.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2005/Defunciones_Base_datos_2005.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2006/Defunciones_Base_datos_2006.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2007/Defunciones_Base_datos_2007.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2008/Defunciones_Base_datos_2008.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2009/Defunciones_Base_datos_2009.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2010/Defunciones_Base_datos_2010.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2011/Defunciones_Base_datos_2011.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2012/Defunciones_Base_datos_2012.zip&ht=02")



##Download the mortality database
ldply(files.url, function(f) {
  file <- str_extract(f, "[0-9\\.a-z]+\\.zip")
  if(!file.exists(file.path("ssa-database", file))) {
    message("Downloading mortality files from INEGI...\n")
    tryCatch(
      download.file(f, file.path("ssa-database", file)),
      error = function(e) {
        unlink(file.path("ssa-database", file))
        e
        stop("Failed to download file. Try Again")
      }
    )
    Sys.sleep(20)
  } else {
      message("already downloaded")
      print(file)
  }
})

##Extract the zip files we just downloaded
## ldply(files.url, function(f) {
##   file <- str_extract(f, "[0-9\\.a-z]+$")
##   message(str_c("Extracting zip file: ", file))
##   unzip(file.path("ssa-database", file), exdir = "ssa-database")
##   NULL
## })

