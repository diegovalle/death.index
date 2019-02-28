.packs <- c("foreign", "plyr", "utils", "stringr")
.success <- suppressWarnings(sapply(.packs, require, character.only = TRUE))
if (length(names(.success)[!.success])) {
  install.packages(names(.success)[!.success], repos = "https://cloud.r-project.org")
  sapply(names(.success)[!.success], require, character.only = TRUE)
}


########################################################
#These are the files to download
########################################################

files.url <- list("http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2004/defunciones_base_datos_2004.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2005/defunciones_base_datos_2005.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2006/defunciones_base_datos_2006.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2007/defunciones_base_datos_2007.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2008/defunciones_base_datos_2008.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2009/defunciones_base_datos_2009.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2010/defunciones_base_datos_2010.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2011/defunciones_base_datos_2011.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=Microdatos_archivos/vitales/Mortalidad/Defunciones/2012/defunciones_base_datos_2012.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/descargas.aspx?sr=microdatos_archivos/vitales/mortalidad/defunciones/2013/defunciones_base_datos_2013.zip&ht=02",
                  "http://www3.inegi.org.mx/sistemas/microdatos/descargas.aspx?sr=microdatos_archivos/vitales/mortalidad/defunciones/2014/defunciones_base_datos_2014.zip&ht=02",
                  "http://www.beta.inegi.org.mx/contenidos/proyectos/registros/vitales/mortalidad/microdatos/defunciones/2015/defunciones_base_datos_2015_dbf.zip",
                  "http://www.beta.inegi.org.mx/contenidos/proyectos/registros/vitales/mortalidad/microdatos/defunciones/2016/defunciones_base_datos_2016_dbf.zip",
                  "https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/2017/defunciones_base_datos_2017_dbf.zip")



##Download the mortality database
ldply(files.url, function(f) {
  file <- str_c(str_extract(f, "[0-9]{4}"), ".zip")
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
    print(file)
    message("already downloaded")
  }
})

##Extract the zip files we just downloaded
## ldply(files.url, function(f) {
##   file <- str_extract(f, "[0-9\\.a-z]+$")
##   message(str_c("Extracting zip file: ", file))
##   unzip(file.path("ssa-database", file), exdir = "ssa-database")
##   NULL
## })

