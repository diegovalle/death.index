########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Fri Oct 12 19:07:31 2012
## Email: diegovalle at gmail.com
## Purpose: Clean the Mexican Mortality Database 
## Copyright (c) Diego Valle-Jones. All rights reserved

source("lib/download-files.R") ##Download the Mortality DB 2004-2010
source("lib/extract-convert.R") ##Unzip and convert the dbf's to csv
source("lib/boot.R")  ##Cleanup
test_dir('tests', reporter = 'summary')

ddply(subset(deaths, intent =="Homicide"), .(year(date_occur)), nrow)
ddply(subset(deaths, intent =="Homicide"), .(year(date_reg)), nrow)

names(deaths)
head(deaths)
levels(deaths$cause_detail)
levels(deaths$motor_vehicle_traffic)
levels(deaths$cause)
levels(as.factor(deaths$yod))


head(deaths[is.na(deaths$date_reg),])

