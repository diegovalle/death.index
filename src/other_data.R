extractStateCode <- function(id){
    id <- str_pad(id, 5, "left", pad = "0")
    as.numeric(str_sub(id, 1, 2))
}

extractMunCode <- function(id){
    id <- str_pad(id, 5, "left", pad = "0")
    as.numeric(str_sub(id, 3))
}

rObjects <- function(df, file.name){
    save(file.name,
         compress = "bzip2",
         file = file.path("clean-data", str_c(file.name, ".RData")))
    
}

convertCSV <- function(file.name, col.names, var = file.name){
    df <- read.csv(file.path("data", str_c(file.name, ".csv")))
    names(df) <- col.names
    write.csv(df, file.path("clean-data", str_c(file.name, ".csv")),
              row.names = FALSE)
    df
}

local({
    aggressor.relation.code <- convertCSV("aggressor.relation.code",
                                          c("aggressor_relation_code",
                                            "relationship"))
    ## aggressor.relation.code$relationship[aggressor.relation.code$relationship == "No especificado"] <- NA
    save(aggressor.relation.code,
         compress = "bzip2",
         file = file.path("clean-data", str_c("aggressor.relation.code",
             ".RData")))
    prompt(aggressor.relation.code,
           filename = file.path("rd", str_c("aggressor.relation.code", ".rd")))

    mex.list.group <- convertCSV("mex.list.group",
                                 c("mex_list_group", "mex_list_group_name"))
    save(mex.list.group,
         compress = "bzip2",
         file = file.path("clean-data", str_c("mex.list.group",
             ".RData")))
    prompt(mex.list.group,
           filename = file.path("rd", str_c("mex.list.group", ".rd")))


    mex.list <- convertCSV("mex.list",
                           c("mex_list", "mex_list_name"))
    save(mex.list,
         compress = "bzip2",
         file = file.path("clean-data", str_c("mex.list",
             ".RData")))
    prompt(mex.list,
           filename = file.path("rd", str_c("mex.list", ".rd")))

    icd.103 <- convertCSV("icd.103",
                          c("icd_103", "icd_103_name"))
    save(icd.103,
         compress = "bzip2",
         file = file.path("clean-data", str_c("icd.103",
             ".RData")))
    prompt(icd.103,
           filename = file.path("rd", str_c("icd.103", ".rd")))

    geo.codes <- convertCSV("geo.codes",
                            c("state_code","mun_code", "name"))
    save(geo.codes,
         compress = "bzip2",
         file = file.path("clean-data", str_c("geo.codes",
             ".RData")))
    prompt(geo.codes,
           filename = file.path("rd", str_c("geo.codes", ".rd")))



    ma <- read.csv(file.path("data", "metropolitan-areas.csv"))
    pop <- read.csv(file.path("data", "municipio-population2010-2030.csv"))
    pop <- subset(pop, Year == 2010 & Sex == "Total")
    ma <- merge(ma, pop, all = TRUE, by.x = "fips", by.y = "Code")
    ma$Sex <- NULL
    ma$Year <- NULL

    non.ma <- subset(ma, is.na(MA) & Population >= 109000)
    non.ma$MA <- NULL
    names(non.ma) <- c("fips", "population")
    non.ma$state_code <- extractStateCode(non.ma$fips)
    non.ma$mun_code <- extractMunCode(non.ma$fips)
    ## non.ma <- merge(non.ma, ENTMUN, all.x = TRUE)
    non.ma$fips <- NULL
    non.ma <- non.ma[order(-non.ma$population),]

    ma <- subset(ma, !is.na(MA))
    ma <- ddply(ma, .(MA), transform, metro_population_2010 = sum(Population))
    names(ma) <- c("fips", "metro_area", "mun_population_2010",
                   "metro_population_2010")
    ma$state_code <- extractStateCode(ma$fips)
    ma$mun_code <- extractMunCode(ma$fips)
    ma$fips <- NULL

    metro.areas <- ma
    big.municipios <- non.ma
    write.csv(ma, file.path("clean-data", "metro.areas.csv"), row.names = FALSE)
    write.csv(non.ma,
              file.path("clean-data", "big.municipios.csv"), row.names = FALSE)

    save(metro.areas,
         compress = "bzip2",
         file = file.path("clean-data", str_c("metro.areas",
             ".RData")))
    prompt(metro.areas,
           filename = file.path("rd", str_c("metro.areas", ".rd")))
    save(big.municipios,
         compress = "bzip2",
         file = file.path("clean-data", str_c("big.municipios",
             ".RData")))
    prompt(big.municipios,
           filename = file.path("rd", str_c("big.municipios", ".rd")))
    })

