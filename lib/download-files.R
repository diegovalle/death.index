library(stringr)
library(plyr)
library(utils)
library(foreign)

########################################################
#These are the files to download
########################################################

files.url <- list("http://www.sinais.salud.gob.mx/descargas/zip/def2004.zip",
               "http://www.sinais.salud.gob.mx/descargas/zip/def2005.zip",
               "http://www.sinais.salud.gob.mx/descargas/zip/def2006.zip",
               "http://www.sinais.salud.gob.mx/descargas/zip/def2007.zip",
               "http://www.sinais.salud.gob.mx/descargas/zip/def2008.zip",
               "http://www.sinais.salud.gob.mx/descargas/zip/def2009.zip",
               "http://www.sinais.salud.gob.mx/descargas/zip/def2010.zip",
                  "http://www.sinais.salud.gob.mx/descargas/zip/def2011.zip")



##Download the mortality database
ldply(files.url, function(f) {
  file <- str_extract(f, "[0-9\\.a-z]+$")
  if(!file.exists(file.path("ssa-database", file))) {
    message("Downloading mortality files from SINAIS...\n")
    tryCatch(
      download.file(f, file.path("ssa-database", file)),
      error = function(e) {
        unlink(file.path("ssa-database", file))
        e
        stop("Failed to download file. Try Again")
      }
    )
    Sys.sleep(20)
  }
})

##Extract the zip files we just downloaded
## ldply(files.url, function(f) {
##   file <- str_extract(f, "[0-9\\.a-z]+$")
##   message(str_c("Extracting zip file: ", file))
##   unzip(file.path("ssa-database", file), exdir = "ssa-database")
##   NULL
## })

