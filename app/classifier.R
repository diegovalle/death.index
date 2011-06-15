library(ipred)
library(randomForest)

hom.juarez <- subset(deaths, MA == "Juárez" &
                     PRESUNTOtxt != "Legal intervention, operations of war, military operations, and terrorism")
ddply(hom.juarez, .(PRESUNTOtxt), nrow)

hom.cause <- ddply(hom.juarez, .(CAUSE, PRESUNTOtxt), nrow)
hom.cause <- ddply(hom.cause, .(PRESUNTOtxt), transform,
                   per = V1 / sum(V1))
hom.cause$CAUSE <- reorder(hom.cause$CAUSE, hom.cause$per)

Cairo("graphs/classifier/percent.png", width = 500, height = 600)
ggplot(hom.cause,
       aes(per, CAUSE, group= PRESUNTOtxt, color = PRESUNTOtxt)) +
       geom_point(size = 3, alpha = .8) +
  scale_x_continuous(formatter = "percent") +
  ylab("Cause") + xlab("percent") +
  scale_color_hue("type of\ndeath")
dev.off()

ddply(subset(ddply(hom.juarez, .(PRESUNTOtxt, EDADVALOR), nrow), EDADVALOR < 900),
      .(PRESUNTOtxt),
      function(df) format(wtd.mean(df$EDADVALOR, df$V1), digits = 3))

ddply(subset(ddply(hom.juarez, .(CAUSE, EDADVALOR), nrow), EDADVALOR < 900),
      .(CAUSE),
      function(df) wtd.mean(df$EDADVALOR, df$V1))

ggplot(ddply(hom.juarez, .(PRESUNTOtxt, EDADVALOR), nrow),
       aes(EDADVALOR, fill = PRESUNTOtxt)) +
  geom_density(alpha = .1) +
  scale_x_continuous(limits = c(0,80))


classify <- function() {
  hom.juarez$EDADVALOR <- ifelse(hom.juarez$EDADVALOR == 998, NA, hom.juarez$EDADVALOR)
  formula <- PRESUNTOtxt ~ EDADVALOR + CAUSE + ANIODEF + LUGLEStxt

  hom.juarez$OCUPACIONtxt <- factor(hom.juarez$OCUPACIONtxt)
  hom.juarez$SEXOtxt <- factor(hom.juarez$SEXOtxt)
  hom.juarez$EDOCIVILtxt <- factor(hom.juarez$EDOCIVILtxt)
  hom.juarez$ESCOLtxt <- factor(hom.juarez$ESCOLtxt)
  hom.juarez$LUGLEStxt <- factor(hom.juarez$LUGLEStxt)
  hom.juarez$CAUSE <- factor(hom.juarez$CAUSE)
  hom.train <- na.omit(subset(hom.juarez, PRESUNTOtxt != "Unknown"))
  hom.train$PRESUNTOtxt <- factor(hom.train$PRESUNTOtxt)

  ind <- sample(2, nrow(hom.train), replace = TRUE, prob=c(2/3, 1/3))
  fit.rf <- randomForest(formula, data = hom.train[ind == 1,], ntree = 1000)
  print(fit.rf)
  fit.pred <- predict(fit.rf, hom.train[ind == 2,])
  hom.train$pred <- NA
  hom.train[ind == 2,]$pred <- as.character(fit.pred)
  #hom.train[ind == 2, c("PRESUNTOtxt", "pred")]

  #Specifity
  message("Specifity: ",
          mean(na.omit(hom.train[hom.train$PRESUNTOtxt == "Homicide",]$pred == "Homicide")))
  #Sensitivity
  message("Sensitivity: ",
          mean(na.omit(hom.train[hom.train$PRESUNTOtxt != "Homicide",]$pred != "Homicide")))


  hom.unknown <- na.omit(subset(hom.juarez, PRESUNTOtxt == "Unknown"))
  fit.unknown <- predict(fit.rf, hom.unknown)
  print(table(fit.unknown))
  hom.unknown$PRESUNTOtxt <- as.character(fit.unknown)
  return(hom.unknown)
}

hom.unknown <- classify()

hom.juarez.pred <- rbind(subset(hom.juarez, PRESUNTOtxt != "Unknown"), hom.unknown)

ddply(subset(hom.juarez.pred, PRESUNTOtxt == "Homicide"), .(ANIODEF), nrow)
ddply(subset(hom.juarez, PRESUNTOtxt == "Homicide"), .(ANIODEF), nrow)

m <- merge(ddply(subset(hom.juarez.pred, PRESUNTOtxt == "Homicide"), .(ANIODEF, MESDEF), nrow),
ddply(subset(hom.juarez, PRESUNTOtxt == "Homicide"), .(ANIODEF, MESDEF), nrow),
      by = c("ANIODEF", "MESDEF"))

m <- subset(m, MESDEF != 0)
m$date <- as.Date(str_c(m$ANIODEF, m$MESDEF, "01", sep = "-"))
m <- subset(m, date >= as.Date("2006-06-01") & date <= as.Date("2008-03-01"))
mm <- melt(m, id = c("date", "ANIODEF", "MESDEF"))
mm$variable <- reorder(mm$variable, mm$value)

Cairo("graphs/classifier/juarez-correct.png", width = 800, height = 500)
ggplot(mm, aes(date, value, group = variable, linetype = variable)) +
  geom_line() +
  #geom_line(aes(date, V1.y), linetype = 1) +
  ylim(0,135) +
  ylab("number of homicides") +
  scale_linetype("type of death",
                 labels = c("Homicides and unknown deaths classified as homicides",
                   "Homicides"),
                 breaks = c("V1.x", "V1.y")) +
  xlab("month") +
  opts(legend.position = "bottom",
       title = "Classifying unknown deaths in Ciudad Juárez")
dev.off()


error <- errorest(formula,
          data = hom.train,
          model = randomForest,estimator = "cv",
                  est.para=control.errorest(predictions=TRUE))
#Specifity
mean(error$predictions[hom.train$PRESUNTOtxt == "Homicide"] == "Homicide")
#Sensitivity
mean(error$predictions[hom.train$PRESUNTOtxt != "Homicide"] != "Homicide")

set.seed(131)
error.RF <- numeric(10)
for(i in 1:10)
  error.RF[i] <- errorest(formula,
                          data = hom.train,
                          model = randomForest,estimator = "cv")$error
summary(error.RF)

library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10)
  error.SVM[i] <-errorest(formula, data = hom.train,
                          model = svm)$error
summary(error.SVM)

par(mfrow = c(1, 1))
for (i in 1:1)
  plot(sort(fit.rf$importance[,i], dec = TRUE),
       type = "h", main = paste("Measure", i))

library(forecast)


t <- ddply(subset(deaths, PRESUNTOtxt == "Homicide"), .(date), nrow)
t <- subset(t, !is.na(date))
tts <- ts(t$V1, start = 2004,f = 12)
seasonplot(tts)
t[which(t$V1 == max(t$V1)),]
t$wday <- factor(wday(t$date))
t$yday <- factor(yday(t$date))
ddply(t, .(wday), function(df) sum(df$V1))
t$newyear <- factor(ifelse(t$yday == 1, 1, 0))

fit <- aov(V1 ~ wday + newyear, data = t)
print(summary(aov(fit)))
plot(TukeyHSD(fit, "newyear"))
library(lubridate)

ggplot(t, aes(wday, V1, group = wday)) +
        geom_boxplot(fill = "transparent", color = "red") +
        geom_jitter(fill = "darkred", alpha = .7) +
        ylab("number of homicides in a day") +
        xlab("day of week") +
#        scale_x_discrete(labels = days, breaks = days) +
        opts(title = "")


fir <- subset(deaths, CAUSE == "Firearm" & PRESUNTOtxt == "Homicide")
ddply(subset(fir, ABBRV %in% c("Son", "NL", "Coah", "BC")), .(ANIODEF), nrow)

ddply(subset(hom, ABBRV %in% c("Mich") & CAUSE == "Firearm"), .(ANIODEF), nrow)


ddply(subset(deaths, MA %in% c("Valle de México") & PRESUNTOtxt == "Suicide"), .(ANIODEF, MESDEF), nrow)

