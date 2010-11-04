########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Fri Jul 16 17:08:16 2010
########################################################
#Analyze the death index files from the SSA/INEGI

bumpChart <- function(df, name, f = last.points, directlabel = FALSE, title = "", xlab = "", scale = "") {
    hom.count <- ddply(df, c("ANIODEF", name), nrow)
    hom.count <- ddply(hom.count, c("ANIODEF"), transform,
                        per = V1 / sum(V1))
    hom.count$ANIODEF <- as.factor(hom.count$ANIODEF)
    hom.count[[name]] <- as.factor(hom.count[[name]])
    hom.count <- ddply(hom.count, c(name), transform,

                       order = per[ANIODEF == 2008])
    hom.count[[name]] <- reorder(hom.count[[name]], -hom.count$order)
    p <- ggplot(hom.count,
                aes_string(x = "ANIODEF",
                           y = "per",
                           group = name,
                           color = name)) +
        geom_line(size = 1.2) +
        scale_y_continuous(formatter = "percent") +
        opts(title = title) +
        ylab("percentage") +
        xlab(xlab) +
        scale_colour_hue(scale)

    if (directlabel == TRUE) {
        direct.label(p, f)

    } else {
        p
    }
}


dotPlot <- function(df, name, xlab = "", ylab = "", title = "") {
    df <- ddply(df, c(name), nrow)
    df[[name]] <- reorder(df[[name]], df$V1)
    ggplot(df, aes_string(y = name, x = "V1")) +
           geom_point() +
           xlab(xlab) +
           ylab(ylab) +
           opts(title = title) +
           xlab("number of homicides") 
}

########################################################
#Statistics Age, Sex, etc
########################################################

#Number of murders in 2006:last.year
totalHomicides <- function(df, title = ""){
  years <- 2006:last.year
  homicides <-  sapply(years,
                     function (x) nrow(subset(df, ANIODEF == x)))

  qplot(as.factor(years), homicides, geom = "line", group = 1) +
    scale_y_continuous(limits = c(0,max(homicides)), formatter = "comma") +
    xlab("year") +
    ylab("number of homicides") +
    opts(title = title)
}

########################################################
#Daily Homicides in Juarez
########################################################
formatDaily <- function(df){
    hom.count <- ddply(df, .(ANIODEF, MESDEF, DIADEF), nrow)
    hom.count$date <-  as.Date(paste(hom.count$ANIODEF,
                                     hom.count$MESDEF,
                                     hom.count$DIADEF,
                                     sep = "/"),
                                     "%Y/%m/%d")
    dates <- data.frame(date = seq(as.Date("2006-01-01"),
                    as.Date("2008-12-31"),
                    by="day"))
    hom.count <- merge(hom.count, dates, by= "date", all.y = TRUE)
    hom.count[is.na(hom.count)] <- 0
    hom.count
}

daily <- function(df, title = ""){
    ggplot(df, aes(date, V1)) +
        scale_x_date(minor = "month") +
        geom_area(fill = "darkred") +
        opts(title = title) +
        ylab("number of homicides")
}

    #geom_vline(aes(xintercept = op.chi), alpha = .7) +
    #geom_text(aes(x,y, label = "Joint Operation Chihuahua"),
    #        data = data.frame(x = op.chi, y = 17),
    #        size = 4, hjust = 1.01, vjust = 0)

op.chi <- as.Date("2008-03-27")


########################################################
#Weekly homicides in Ciudad Juarez
########################################################
weekly <- function(hom.count, title = ""){
    hom.count$week <- format(hom.count$date, "%Y %W")
    hom.count$week <- c(0, rep(1:nrow(hom.count), each = 7)[1:1095])
    hom.w <- ddply(hom.count, .(week), function(df) sum(df$V1))
    hom.w <- subset(hom.w, week != 0)

    hom.w$date <- seq(as.Date("2006-01-02"),
                    as.Date("2008-12-31"),
                    by="week")

    ggplot(hom.w, aes(date, V1)) +
#        geom_line(color = "darkred", size = 1.2) +
        geom_area(fill = "darkred") +
#        geom_line(color = "darkred", size = 1.2) +
        scale_x_date(minor = "month") +
    #geom_vline(aes(xintercept = op.chi), alpha = .7) +
    #geom_vline(aes(xintercept = op.mich), alpha = .7) +
        xlab("date") + ylab("number of homicides") +
        opts(title = title)
    #geom_text(aes(x,y, label = "Joint Operation Chihuahua"),
    #        data = data.frame(x = op.chi, y = 55),
    #        size = 4, hjust = 1.01, vjust = 0) +
    #geom_text(aes(x,y, label = "Start of the Drug War"),
    #        data = data.frame(x = op.mich, y = 55),
    #        size = 4, hjust = 1.01, vjust = 0)
}

monthly <- function(hom.count, title = ""){
    hom.count$month <- format(hom.count$date, "%Y%m")
    hom.w <- ddply(hom.count, .(month), function(df) sum(df$V1))

    hom.w$date <- seq(as.Date("2006-01-15"),
                    as.Date("2008-12-15"),
                    by="month")

    ggplot(hom.w, aes(date, V1)) +
        geom_line(color = "darkred", size = 1.2) +
        scale_x_date(minor = "month") +
        xlab("date") + ylab("number of homicides") +
        opts(title = title)
}


########################################################
#Age
########################################################
ageDensitySex <- function(df, title = "") {
    df <- subset(df, SEXO %in% c(1, 2))
    df$SEXO <- car::recode(df$SEXO, "1 = 'Males'; 2 = 'Females'")
    ggplot(subset(df, EDADVALOR < 900),
           aes(EDADVALOR, group = SEXO))+
        geom_density(aes(fill = SEXO), alpha = .5) +
        xlab("age at death") +
        opts(title = title)
}

ageDensity <- function(df, title = "") {
  ggplot(subset(df, EDADVALOR < 900), aes(EDADVALOR)) +
    geom_density(fill = "darkred") +
        xlab("age at death") +
        opts(title = title)
}


ageDensityYear <- function(df, title = "") {
ggplot(subset(df, EDADVALOR < 900),
       aes(EDADVALOR, group = ANIODEF,
       fill = as.factor(ANIODEF))) +
    geom_density(alpha =.5) +
    scale_fill_brewer("Year") + #values = c("#FDC086",
                        #"#7FC97F", "purple"))+
    xlab("age at death") +
    opts(title = title) 
}


########################################################
#Age Percentage
########################################################

plotAgeBump <- function(df, age.groups, title = "") {
  homa <- subset(df, EDADVALOR < 900)
  homa$age.group <- cut(homa$EDADVALOR, age.groups)
  bumpChart(homa, "age.group", title = title, xlab = "year",
            scale = "age")
}

plotAgeDot <- function(df, age.groups, year, title = ""){
  hom.year <- subset(df, ANIODEF == year)
  homa.year <- subset(hom.year, EDADVALOR < 900)
  homa.year$age.group <- cut(homa.year$EDADVALOR, age.groups)
  dotPlot(homa.year, "age.group", title = title)
}


########################################################
#Hours when people are most likely to die
########################################################
plotHours <- function(df, year, fix = FALSE, title = "") {
  df <- subset(df, ANIODEF == year)
  hours <- count(subset(df, HORADEF < 24)$HORADEF)
  if(fix)
    hours[["freq"]][1] <- (hours[["freq"]][2] + hours[["freq"]][24]) / 2
  hours$x <- factor(hours$x, levels = c(6:23,0:5))

  ggplot(hours, aes(x, freq, group = 1)) +
    geom_line() +
    opts(title = title) +
    ylab("number of homicides") + xlab("time of day") +
    scale_x_discrete(breaks = c("6","12","18","0"),
                       labels = c("6:00 AM", "Noon", "6:00 PM",
                                  "Midnight"))
}


########################################################
#Percentage of Homicides with a Firearm
########################################################

plotFirearmPer <- function(df, title = "") {
  fir <- subset(df, CAUSADEF %in% c("X93", "X94", "X95"))
  totfir <- ddply(fir, .(ANIODEF), nrow)
  tothom <- ddply(df, .(ANIODEF), nrow)
  totfir$prop <- totfir$V1 / tothom$V1

  ggplot(totfir, aes(as.factor(ANIODEF), prop, group = 1)) +
    geom_line() +
    scale_y_continuous(limits = c(0, max(totfir$prop)),
                       formatter = "percent") +
    opts(title = title) +
    ylab("percentage") + xlab("year")
}



#llages <- data.frame(sapply(2006:2008, function(x) stats(subset(hom, ANIODEF == x & EDADVALOR < 900)$EDADVALOR)))
#ames(allages) <- 2006:2008
#ownames(allages) <- c("N", "mean", "Std.Dev.", "min", "Q1", "median",
#                      "Q3", "max", "missing values")


#apply(2006:2008, function(x) stats(subset(hom, ANIODEF == x & EDADVALOR < 900 & SEXO == "Males")$EDADVALOR))
#apply(2006:2008, function(x) stats(subset(hom, ANIODEF == x & EDADVALOR < 900 & SEXO == "Females")$EDADVALOR))
