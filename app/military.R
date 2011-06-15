
Military <- function(df, dates, title = "") {
  dates <- c(as.Date("1970-01-01"), as.Date(dates))
  
  hom.lag <- formatDaily(df)
  hom.lag$V1 <- hom.lag$V1 + 0.5
  hom.lag$group <- cut(hom.lag$date,
                       breaks = dates)
  hom.lag <- subset(hom.lag, date < as.Date(str_c(kmaxy, "12", "01",
                                              sep = "-")))
  #llist <- dlply(hom.lag, .(group),
   #              function(df) zeroinfl(data = df, V1 ~ date))
  #plot(predict(llist[[2]]))
  ops <- as.numeric(dates[2:length(dates)])
  ggplot(hom.lag, aes(date, V1, group = group)) +
    geom_point(size = 3, alpha = .5) +
    geom_smooth() +
    #geom_smooth(method = glm.nb, formula = "y ~ x + I(x^2)", se = TRUE)  +
    #coord_trans(ytrans = "sqrt")
    scale_y_log10() +
    #geom_smooth(method = zeroinfl, se = FALSE) +
    geom_vline(xintercept = ops) +
    #coord_cartesian(ylim = c(0, 2.1)) +
    scale_x_date() +
    ylab("log(number of homicides)") +
    opts(title = title)
}

MilitaryW <- function(df, dates, title = "") {
  dates <- c(as.Date("1970-01-01"), as.Date(dates))
  hom.w <- formatWeekly(formatDaily(df))
  hom.w$group <- cut(hom.w$date,
                       breaks = dates)
  hom.w$V1 <- hom.w$V1 + 0.5
  #remove the last month since it has incomplete data
  hom.w <- subset(hom.w, date < as.Date(str_c(kmaxy, "12", "01",
                                              sep = "-")))
  
  ops <- as.numeric(dates[2:length(dates)])
  ggplot(hom.w, aes(date, V1, group = group)) +
    geom_point(size = 3, alpha = .8) +
    geom_vline(xintercept = ops) +
    ylab("log(weekly number of homicides)") +
    scale_x_date(minor = "month") +
    #geom_smooth(method = glm, family = "quasipoisson") +
    geom_smooth() +
    scale_y_log10() +
    #coord_trans(ytrans = "log") +
    opts(title = title)
    #geom_smooth(method = glm, family = "poisson")
}

hom.chima <- subset(hom, MA == "Chihuahua")


hom.border <- subset(hom, ABBRV == "Chih" &
                    MUNOCU %in% c(35, 5 ,53, 28, 52, 42))

hom.cg <- subset(hom, ENTOCU == 12 & MUNOCU %in% c(068, 16, 73,
                           38, 22, 50, 27, 64, 67, 7, 3, 57, 54, 11, 14, 007))

hom.son <- subset(hom, ABBRV == "Son")
hom.nogales <- subset(hom, ABBRV == "Son" & MUNOCU == 043)


hom.cuer <- subset(hom, MA == "Cuernavaca")


hom.gua <- subset(hom, MA == "Guadalajara")





hom.tam <- subset(hom, ABBRV == "Tamps")


hom.nlaredo <- subset(hom, MA == "Nuevo Laredo")


hom.ag <- subset(hom, ABBRV == "Ags")


hom.poor <- subset(hom, ABBRV %in% c("Chis", "Camp", "Tab"))


params <- list(laguna = list(hom.lag, c("2007-06-14"), "La Laguna"),
          mich.mun = list(hom.mich.mun, c("2006-12-11", "2009-07-01"),
            "Michoacán (regions of Lázaro Cardenas, Uruapan and the southern part of the Tierra Calienta)"),
          juarez = list(hom.j, c("2008-03-28", "2009-03-01"),
            "Ciudad Juárez"),
          list(hom.tj, c("2007-01-03"), "Tijuana (MA)"),
          list(hom.chima, c("2008-03-28"), "Chihuahua (MA)"),
          list(hom.border, c("2008-03-28"),
        "Chihuahua's Border Municipalities (Excluding Ciudad Juárez)"),
          list(hom.son, c("2008-01-22"), "Sonora"),
          list(hom.nogales, c("2008-01-22"), "Nogales"),
          list(hom.cuer, c("2008-01-22"), "Cuernavaca (MA)"),
          list(hom.gua, c("2008-01-11"), "Guadalajara"),
          list(hom.ver, c("2007-05-14"), "Veracruz (State)"),
          list(hom.mon, c("2007-02-19"), "Monterrey (MA)"),
          list(hom.tam, c("2007-02-19"), "Tamaulipas"),
          list(hom.nlaredo, c("2007-02-19"), "Nuevo Laredo"),
          list(hom.ag, c("2007-10-19"), "Aguascalientes (State)"),
          list(hom.poor, c("2007-10-19"),
                    "Chiapas, Campeche, and Tabasco"),
          list(hom.slp, c("2007-09-19"), "San Luís Potosí (State)"),    
          list(hom.cul, c("2008-05-13"), "Culiacán and Navolato"),
          list(hom.maz, c("2008-07-15"), "Mazatlán"),
          list(hom.aca, c("2007-01-15", "2008-01-22"), "Acapulco"),
          list(hom.cg, c("2007-01-15", "2008-01-22"),
               "Costa Grande Guerrero"),
          list(hom.can, c("2009-02-09"), "Cancún")
          )
llplots <- llply(params,
                 function(df) {
                   p <- Military(df[[1]], df[[2]], df[[3]])
                   saveAAPlot(p, file.path("graphs", "military",
                                           str_c(df[[3]], ".png")),
                              w = 800)
                 }
                 )

formatStart <- function(df, name, fun = formatDaily) {
  df <- fun(df)
  df$name <- name
  return(df)
}

mich <- formatStart(hom.mich, "Michoacán")
aca <- formatStart(hom.aca, "Acapulco (MA)")
nlaredo <- formatStart(hom.nlaredo, "Nuevo Laredo (MA)")

fir.nlaredo <- subset(hom.nlaredo, CAUSADEF %in% c("X93", "X94", "X95"))
awb <- formatWeekly(formatDaily(fir.nlaredo))
awb <- subset(nlaredo,
            date <= as.Date("2005-06-11"))
awb$group <- cut(awb$date, breaks = as.Date(c("2000-01-01","2004-09-14")))
fits <- dlply(awb, .(group), function(df) zeroinfl(data = df, V1 ~ date))
fits2 <- dlply(awb, .(group), function(df) hurdle(data = df, V1 ~ date))
fits.nb <- dlply(awb, .(group), function(df) glm.nb(data = df, V1 ~ date))
vuong(fits[[2]], fits.nb[[2]])
#plot(fits[[1]])
vuong(fits[[1]], fits.nb[[1]])

ggplot(awb, aes(date, V1, group = group)) +
  geom_point() +
  geom_smooth(method = glm.nb, dist = "negbin",
              size = 1.2)  +
  geom_vline(xintercept = as.numeric(as.Date("2004-09-14")))+
  #coord_trans(ytrans = "sqrt") +
  scale_x_date(format = "%b") +
  coord_cartesian(ylim = c(0, 2.6))

x1 <- rbind(mich, aca, nlaredo)
x <- subset(x1, date >= as.Date("2005-07-01") &
            date <= as.Date("2008-07-01"))
#df <- ddply(x, .(ABBRV, date), nrow)
x$group <- c(
  cut(subset(x, name == "Michoacán")$date, as.Date(c("2000-01-01", "2006-12-11"))),
cut(subset(x, name == "Acapulco (MA)")$date, as.Date(c("2000-01-01", "2007-01-15"))),
cut(subset(x, name == "Nuevo Laredo (MA)")$date, as.Date(c("2000-01-01", "2007-02-19")))
                 )
x$group[is.na(x$group)] <-"2"
x$name <- factor(x$name, levels = c("Michoacán",
                           "Acapulco (MA)",
                            "Nuevo Laredo (MA)"))
lops <- data.frame(
                   ops = as.numeric(as.Date(c("2006-12-11",
                     "2007-01-15", "2007-02-19"))),
                   name = factor(levels(x$name)),
                   group = "1",
                   opname = c("J.O. Michoacán", "J.O. Acapulco",
                     "J.O. Nuevo León-Tamaulipas"))

ggplot(x, aes(date, V1, group = group)) +
  geom_point(size = 1.5, alpha = .6) +
  geom_smooth(method = glm.nb, formula = "y ~ x + I(x^2)", se = TRUE,
              size = 1.2)  +
  #coord_trans(ytrans = "sqrt") +
  scale_x_date(format = "%b-%Y") +
  coord_cartesian(ylim = c(0, 2.1)) +
  geom_text(data = lops, aes(label = opname, x = ops), y = 1.5,
              hjust = -0.05, size = 3.5) +
  geom_vline(data = lops, aes(xintercept = ops)) + 
  #geom_smooth(method = glm, family = "poisson") +
  facet_wrap(~ name, ncol = 1) +
  scale_colour_hue("Before or\nAfter\nCalderon's\nMilitary\nOperations",
                   breaks = c("1", "2"), labels = c("before", "after")) +
  ylab("daily number of homicides") 
saveAAPlot(last_plot(), file.path("graphs", "military", "start.png"),
           w = 600, h = 600)

mich <- formatStart(hom.mich.mun, "Michoacán", formatWeekly)
aca <- formatStart(hom.aca, "Acapulco (MA)", formatWeekly)
nlaredo <- formatStart(hom.nlaredo, "Nuevo Laredo", formatWeekly)

ggplot(formatWeekly(formatDaily(hom.aca)), aes(date, V1)) +
       geom_area() +
  geom_area(data = formatWeekly(formatDaily(hom.nlaredo)), aes(date, V1),
            fill = "red")

x2 <- subset(x1, date <= as.Date("2006-12-11"))
ggplot(x2, aes(date, V1, group = name, color = name)) +
  geom_smooth() +
  coord_cartesian(ylim = c(0, 2)) 

