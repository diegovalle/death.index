
deaths$abbrev <- stateToAbbrev(deaths$state_death)


classify <- function(df, states) {
  print(states)
  
  df <- subset(deaths, abbrev %in% states)
  df$intent.nolegal <- df$intent
  df$intent.nolegal <- car::recode(df$intent.nolegal, "'Legal Intervention' = 'Homicide'")

  df$month <- as.yearmon(df$date_reg)
  df$month <- as.numeric(df$month)
  if(states == "Sin") {
    df$intent.nolegal[((df$month <=  2008.999 & df$month >= 2007) & (year(df$date_reg) %in% 2007:2008)) &
              (df$intent.nolegal == "Accident"  & df$abbrev == "Sin")] <- NA
    t <- subset(deaths, cause == "Firearm" & abbrev != "Sin" & intent == "Accident")
    t$intent.nolegal <- t$intent
    t$month <- as.numeric(as.yearmon(t$date_reg))
    df <- rbind(df, t)
    rm(t)  
    ##print(ddply(subset(df, intent == "Accident"), .(cause, year_reg), nrow))
    ##print(ddply(df, .(intent, year_reg), nrow))
  }
  if(states == c("DF", "Mor")){
    df$intent.nolegal[((df$month <=  2007.1 & df$month >= 2007) & (year(df$date_reg) == 2007)) &
              (df$intent.nolegal == "Accident" & df$abbrev == "DF")] <- NA
    print(ddply(df, .(intent, year_reg), nrow))
  }
  if(states == c("Chis")){
    ##df$intent.nolegal[(df$intent.nolegal == "Accident" & is.na(df$cause) & df$abbrev == "Chis" &
      ##         df$year_reg == 2007)] <- NA
    ##df$intent.nolegal[(df$intent.nolegal == "Accident" & df$cause == "Firearm" & df$abbrev == "Chis")] <- NA
    ##print(ddply(df, .(intent, year_reg), nrow))
  }
  if(states == c("BC")){
    df$intent.nolegal[(df$intent.nolegal == "Accident" & df$cause == "Firearm" & df$abbrev == "BC" &
               df$year_reg == 2007)] <- NA
    print(ddply(df, .(intent, year_reg), nrow))
  }
  df <- droplevels(df)
  
  
  df$month <- as.factor(month(df$date_reg))
  ##df$intent.nolegal[((df$month == 1 | df$month == 2) & (year(df$date_reg) == 2007)) &
  ##          (df$intent.nolegal == "Homicide" | df$intent.nolegal == "Accident")] <- NA
  ##ddply(subset(df, intent == "Homicide"),
  ##      .(year(date_reg), month(date_reg)), nrow)
  
  
  y <- c("intent.nolegal")
  x <- c("cause", "age", "sex")
           ##"marital", "place_injury", "edu", "month", "yod", "autopsy")
  formula <-  intent.nolegal ~ age + cause * sex ##+ marital + place_injury + autopsy + month + yod
  
  algo <- "lda"
  if(states %in% c("Ver", "Mex", "Chih", "BC")) {
    algo <- "knn"
  }
  if(states == c("Mex")) {
    algo <- "knn"
  }
  if(states == c("Son", "Dgo")) {
    algo <- "knn"
  }
  if(states == c("Chis", "Oax")) {
    algo <- "knn"
  }
  if(states == c("Jal", "Col", "Nay")) {
    algo <- "knn"
  }
  if(states == c("DF")) {
    algo <- "knn"
    x <- c("age", "sex", "cause",
           "month", "place_injury", "job")
    formula <-  intent.nolegal ~ age + cause * sex
  }
  if(states == c("Sin")) {
    algo <- "glmnet"
    x <- c("cause", "age", "sex",
           "place_injury")
    formula <-  intent.nolegal ~ age + cause * sex * place_injury 
  }
  
  ##subset all the deaths that are of unknown injury intent
  df.train <- df[!is.na(df$intent.nolegal),]
  ##Get an idea of how many accidents by transportation there are
  ddply(df, .(year(date_reg), intent), nrow)
  
  ##Use kNN to impute missing data
  ##df.train <- cbind(kNN(df.train[,c(x)])[1:length(x)], intent = df.train$intent.nolegal)
  

  ##ddply(df.train, .(cause), nrow))
  ##head(df.train)
  
  ##divide the data set into training and test
  inTrain <- createDataPartition(1:nrow(df.train), p = 4/5, list = FALSE)
  
  train <- na.omit(df.train[inTrain,c(x,y)])
  test <- na.omit(df.train[-inTrain,c(x,y)])
  


  bootControl <- trainControl(#method = "repeatedcv",
                              number = 3,
                              repeats = 1,
                              returnData = FALSE)
  ## Estimate class probabilities
  ##classProbs = TRUE,
  ## Evaluate performance using
  ## the following function
  ##summaryFunction = twoClassSummary)
  
  
  rpartFit <- train(formula,
                    data = train,
                    preProcess = c("center", "scale"),
                    method = algo,
                    trControl = bootControl)
  ##save(nbFit, file = "dfnb.RData")
  ##load("dfnb.RData")
  ##test <- cbind(kNN(test[,c(x)])[1:length(x)], intent = test$intent.nolegal)
  fit.pred.rpart <- predict(rpartFit, test)
  print(confusionMatrix(fit.pred.rpart, test$intent.nolegal))
  
  
  
  df <- subset(df, abbrev %in% states)
  
  ##Impute missing data for the deaths of unknown intent
  hom.unknown <- df[is.na(df$intent.nolegal) & !is.na(df$cause),]
  inTrain <- createDataPartition(1:nrow(hom.unknown), p = 4/5, list = FALSE)
  hom.unknown[-inTrain, "cause"] <- NA

  message("\nComputing optimal number of groups for knn:")
  k <- 1
  min <- 0
  for(i in 1:60) {
    temp <- confusionMatrix(kNN(hom.unknown[,c(x)], k = i )$cause,
                    df[is.na(df$intent.nolegal) & !is.na(df$cause),]$cause)$overall[1]
    if(temp > min) {
      min <- temp
      k <- i
    }
  }
  hom.unknown <- df[is.na(df$intent.nolegal),]
  hom.unknown[,c(x)] <- kNN(hom.unknown[,c(x)], k = k)[1:length(x)]
  
  fit.unknown <- predict(rpartFit, hom.unknown[,c(x)])
  print(table(fit.unknown))
  hom.unknown$intent.imputed <- fit.unknown
  
  df.pred <- rbind.fill(df[!is.na(df$intent.nolegal),], hom.unknown)
  df.pred$intent.imputed <- with(df.pred,
                                 ifelse(is.na(intent),
                                        as.character(intent.imputed),
                                        as.character(intent)))
  df.pred$intent.imputed <- as.factor(df.pred$intent.imputed)


conf <<- rbind(conf,
                 data.frame(sen = confusionMatrix(fit.pred.rpart, test$intent.nolegal)$byClass[2, 1],
                            spe = confusionMatrix(fit.pred.rpart, test$intent.nolegal)$byClass[2, 2],
                            state = paste(states, sep=", ", collapse=", ") ,
                            num = table(fit.unknown)[2],
                            accu = confusionMatrix(fit.pred.rpart, test$intent.nolegal)$overall[[1]]))
  
  df.pred$intent.nolegal <- NULL
  
  return(df.pred)
  ## original <- ddply(df.pred, .(year(date_reg), month(date_reg), intent), nrow)
  
  ## imputed <- ddply(df.pred, .(year(date_reg), month(date_reg), intent.imputed), nrow)

  
  
  ## ddply(df.pred, .(year(date_reg), intent), nrow)
  ## ##print(ddply(df.pred, .(year(date_reg), intent.imputed), nrow))
  ## ret <- ddply(df.pred, .(year(date_reg), month(date_reg), intent.imputed), nrow)
  ## ret <- subset(ret, intent.imputed == "Homicide")
  ## names(ret) <-  c("year", "month", "intent", "homicides")
  ## ret$state <- paste(states, sep=", ", collapse=", ")
  ## return(ret)
}

##library(doMC)
##registerDoMC()
set.seed(1)

## dput(levels(factor(deaths$abbrev)))
## classify(deaths,  c("Tamps", "SLP", "Coah", "Zac", "NL"))
##classify(subset(deaths, cause == "Firearm"),  c("Sin"))
##classify(deaths, c("BC"))
## t <- classify(deaths, c("Chis"))
## ddply(subset(t, intent.imputed == "Homicide"& abbrev == "Chis"),
##       .(year(date_reg)), nrow)
## ddply(subset(t, is.na(intent) & abbrev == "Chis"),
##       .(year(date_reg)), nrow)
## ddply(subset(t, is.na(cause) & abbrev == "Chis"),
##       .(year(date_reg)), nrow)
## ddply(subset(t, intent == "Accident" & cause == "Firearm" & abbrev == "Chis"),
##       .(year(date_reg)), nrow)
##classify(deaths, c("Tlax"))


conf <- data.frame(sen = numeric, spe = numeric, state = character, num = numeric,
                   accu = numeric)
class <- ldply(list(c("Son", "Dgo"),
                    c("QR", "Camp", "Yuc", "Tlax", "Qro", "Tab", "Pue", "BCS", "Ags"),
                    "BC", "Chih", "Gro", 
                    c("DF", "Mor"), "Gto", "Hgo",
                    c("Jal", "Col", "Nay"), "Mex", "Mich",
                    c("Oax", "Chis"), c("Sin"), 
                    c("Tamps", "SLP", "Coah", "Zac", "NL"), "Ver"),
               function(x) classify(deaths, x))
class$abbrev <- NULL
deaths$abbrev <- NULL

test_that(nrow(subset(deaths,!intent %in% class$intent)), equals(0))
##test_that(all(deaths == class), equals(TRUE))
deaths <- class
rm(class)



conf <- melt(conf, id = c("state", "num"))

conf <- ddply(conf, .(state), transform, order = value[3])
conf$state <- as.factor(conf$state)
conf$state <- reorder(conf$state, conf$order)
conf$variable <- car::recode(conf$variable, "'sen' = 'Sensitivity (Homicides)';
                                             'spe' = 'Specificity (Homicides)';
                                             'accu' = 'Accuracy (Overall)'")


##Cairo("sen-spe.png", width = 610, height = 650)
ggplot(conf, aes(value,state, group = variable)) +
  geom_point(aes(size = num)) +
  scale_size_area("number of\nimputed\nhomicides",
             breaks = c(350, 650, 950)) +
  scale_x_continuous("percent", label = percent) +
  scale_y_discrete(breaks = c("QR, Camp, Yuc, Tlax, Qro, Tab, Pue, BCS, Ags",
                    "BC", "Chih", 
                    "Gro", "DF, Mor", "Gto", "Hgo",
                    "Jal, Col, Nay", "Mex", "Mich",
                    "Oax, Chis","Sin","Son, Dgo",
                    "Tamps, SLP, Coah, Zac, NL", "Ver"),
                   labels = c("QR, Camp, Yuc\nTlax, Qro, Tab\nPue, BCS, Ags",
                    "BC", "Chih", 
                    "Gro", "DF, Mor", "Gto", "Hgo",
                    "Jal, Col, Nay", "Mex", "Mich",
                    "Oax, Chis","Sin","Son, Dgo",
                    "Tamps, SLP, Coah\nZac, NL", "Ver")) +
  geom_vline(xintercept = .5, linetype = 2) +
  annotate("text", x = .52, y = "Mich",
           label = "random guessing", angle=90,
           hjust = 0, size = 4) +
  facet_wrap(~variable) +
  theme_bw() +
  labs(title = "Sensitivity and Specificity when Classifying\nDeaths of Unknown Intent as Homicides") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
##dev.off()
ggsave("sen-spe.svg",width = 7.50, height = 6.50, dpi = 100)
