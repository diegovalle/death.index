This program cleans up the mortality database from the SSA/INEGI

run_all.R ##downloads and cleans the mortality files

* Upload zip of all the zip files in the ssa-database directory
* Upload zip of all the RData files in the cache directory

Cleaned Data
-------------

Once the program is run you'll find a  bunch of files in the clean-data directory:

* mortality-database.sqlite : *All* registered deaths in Mexico from 2004 to 2010 in sqlite format
* injury-intent.csv.bz2: Injury intent (accidents, suicides, homicides, legal interventions and deaths of unspecified injury intent) deaths in csv format
* di[year].sinais.csv.bz: The raw 'uncleaned' mortality files  (see below) in csv format

Marital status was recorded differently in 2012 (A separated category was added)
Insurance was recorded differently starting in 2012 (IMSS oportunidades added)
Education was recorded differently starting in 2012

Data Sources
------------

The mortality files are from the SSA/INEGI available at

[SINAIS](http://sinais.salud.gob.mx/basesdedatos/index.html#estatica)

If for some reason the files are not downloaded automatically you can download them manually and place the files in the "ssa-database" directory. The files you need to dowload are:

[2004](http://www.sinais.salud.gob.mx/descargas/zip/def2004.zip)

[2005](http://www.sinais.salud.gob.mx/descargas/zip/def2005.zip)

[2006](http://www.sinais.salud.gob.mx/descargas/zip/def2006.zip)

[2007](http://www.sinais.salud.gob.mx/descargas/zip/def2007.zip)

[2008](http://www.sinais.salud.gob.mx/descargas/zip/def2008.zip)

[2009](http://www.sinais.salud.gob.mx/descargas/zip/def2009.zip)

[2010](http://www.sinais.salud.gob.mx/descargas/zip/def2010.zip)

Author: Diego Valle-Jones
