########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Fri Oct 12 19:07:31 2012
## Email: diegovalle at gmail.com
## Purpose: Clean the Mexican Mortality Database 
## Copyright (c) Diego Valle-Jones. All rights reserved

source("lib/download-files.R") ##Download the Mortality DBs 2004-2012
source("lib/extract-convert.R") ##Unzip and convert the dbf's to csv
source("lib/boot.R")  ## Cleanup the data: recode variables and nice
                      ## columns names

source("src/classify.R")  ##Impute deaths of unknown intent
source("src/save.R") ##Save to a csv and RData
test_dir("test1.R", reporter = "summary")


library(mxmortality)

