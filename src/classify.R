
deaths$abbrev <- stateToAbbrev(deaths$state_occur_death)
sum(!is.na(deaths$abbrev)) == nrow(deaths)
sum(deaths$abbrev == "Other")
sum(deaths$abbrev == "LATAM")
sum(deaths$abbrev == "USA")

classify <- function(df, states) {
    print(states)
       
    df <- subset(deaths, abbrev %in% states)
    df$intent.nolegal <- NULL
    df$intent.nolegal <- df$intent
    df$intent.nolegal <- car::recode(df$intent.nolegal,
                                     "'Legal Intervention' = 'Homicide'")
    df$month <- as.factor(month(df$date_reg))
    ## Error in Sinaloa Firearm accidents 2007 and 2008
    if(identical(states,"Sin")) {
        df$intent.nolegal[(year(df$date_reg)) %in% 2007:2008 &
                          (df$intent.nolegal == "Accident"  &
                           df$abbrev == "Sin") &
                          !is.na(df$intent.nolegal)] <- NA
        t <- subset(deaths, mechanism == "Firearm" & abbrev != "Sin" & intent == "Accident")
        t$intent.nolegal <- t$intent
        t$month <- as.numeric(as.yearmon(t$date_reg))
        df <- rbind.fill(df, t)
        rm(t)   
    }
    ## Error in 2007 Feb and Jan DF Homicides
    if(identical(states, c("DF"))){
        df$intent.nolegal[((df$month %in% 1:2) &
                           (year(df$date_reg) == 2007)) &
                          (df$intent.nolegal == "Accident" &
                           df$abbrev == "DF") &
                          !is.na(df$intent.nolegal)] <- NA
    }
    ## Error in 2007 BC firearm accidents
    if(identical(states, c("BC"))){
        df$intent.nolegal[(df$intent.nolegal == "Accident" &
                           df$mechanism == "Firearm" & df$abbrev == "BC" &
                           df$year_reg == 2007)] <- NA
        
    }
    df <- droplevels(df)
  
    y <- c("intent.nolegal")
    x <- c("mechanism", "age_years", "sex")
    formula <-  intent.nolegal ~ age_years + mechanism * sex 
  
    algo <- "knn"
    ## penalized regression works better in Sinaloa
    if(states == c("Sin")) {
        algo <- "ranger"
        x <- c("mechanism", "age_years", "sex",
               "place_occur")
        formula <-  intent.nolegal ~ age_years+ mechanism * sex * place_occur
    }
    if(states == c("Mex")) {
      algo <- "ranger"
      x <- c("mechanism", "age_years", "sex",
             "place_occur")
      formula <-  intent.nolegal ~ age_years+ mechanism * sex * place_occur
    }
    if(states == c("DF")) {
      algo <- "ranger"
      x <- c("mechanism", "age_years", "sex",
             "place_occur")
      formula <-  intent.nolegal ~ age_years+ mechanism * sex * place_occur
    }
    if(states == c("BC")) {
      algo <- "ranger"
      x <- c("mechanism", "age_years", "sex",
             "place_occur")
      formula <-  intent.nolegal ~ age_years+ mechanism * sex * place_occur
    }
  
    ##subset all the deaths that are of unknown injury intent
    df.train <- df[!is.na(df$intent.nolegal),]
    ##Get an idea of how many accidents by transportation there are
    ddply(df, .(year(date_reg), intent), nrow)
    
    ##divide the data set into training and test
    inTrain <- createDataPartition(1:nrow(df.train), p = 4/5, list = FALSE)
    
    train <- na.omit(df.train[inTrain,c(x,y)])
    test <- na.omit(df.train[-inTrain,c(x,y)])
    
    
    
    bootControl <- trainControl(#method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                returnData = FALSE)
  
  
    rpartFit <- train(formula,
                      data = train,
                      preProcess = c("center", "scale"),
                      method = algo,
                      trControl = bootControl)
  
    fit.pred.rpart <- predict(rpartFit, test)
    print(confusionMatrix(fit.pred.rpart, test$intent.nolegal))
    
    
    df <- subset(df, abbrev %in% states)
  
  
    ##Impute missing data for the deaths of unknown intent
    hom.unknown <- df[is.na(df$intent.nolegal) & !is.na(df$mechanism),]
    inTrain <- createDataPartition(1:nrow(hom.unknown), p = 4/5, list = FALSE)
    hom.unknown[-inTrain, "mechanism"] <- NA
    
    message("\nComputing optimal number of groups for knn:")
    k <- 1
    min <- 0
    if(identical(states,"Chih")) {
        k <-  9
    } else {
        for(i in 1:60) {
            capture.log <- capture.output(temp <- confusionMatrix(kNN(hom.unknown[,c(x)],
                                                                      k = i,
                                                                      trace = FALSE)$mechanism,
                                                                  df[is.na(df$intent.nolegal) & !is.na(df$mechanism),]$mechanism)$overall[1])
            if(temp > min) {
                min <- temp
                k <- i
            }
        }
    }
    hom.unknown <- df[is.na(df$intent.nolegal),]
    unimputed <- hom.unknown[,c(x)]
    hom.unknown[,c(x)] <- kNN(hom.unknown[,c(x)], k = k,
                              trace = FALSE)[1:length(x)]
    ##df <- subset(deaths, abbrev %in% states)
    fit.unknown <- predict(rpartFit, hom.unknown[,c(x)])
    hom.unknown$intent.imputed <- fit.unknown
    ##print(table(fit.unknown))
    hom.unknown[,c(x)] <- unimputed
    subset(hom.unknown,intent == "Accident"  & mechanism == "Firearm" )
    df.pred <- rbind.fill(df[!is.na(df$intent.nolegal),], hom.unknown)
    df.pred$intent.imputed <- with(df.pred,
                                   ifelse(is.na(intent.nolegal),
                                          as.character(intent.imputed),
                                          as.character(intent.nolegal)))
    df.pred$intent.imputed <- as.factor(df.pred$intent.imputed)
    
        
    conf <<- rbind(conf,
                   data.frame(sen = confusionMatrix(fit.pred.rpart,
                                  test$intent.nolegal)$byClass[2, 1],
                              spe = confusionMatrix(fit.pred.rpart,
                                  test$intent.nolegal)$byClass[2, 2],
                              state = paste(states, sep=", ", collapse=", ") ,
                              num = table(fit.unknown)[2],
                              accu = confusionMatrix(fit.pred.rpart,
                                  test$intent.nolegal)$overall[[1]]))

    df.pred$intent.nolegal <- NULL
    return(df.pred)
}


set.seed(1)


## b=nrow(subset(deaths, abbrev %in% c("Son", "Dur")))
## df <- classify(deaths, c("Other", "LATAM", "USA"))
## df$abbrev <- NULL
## df$month <- NULL
## df$date_reg <- NULL
## df$date_occur <- NULL
## deaths <- rbind(deaths, df)
## b==nrow(df)



message("Classifying deaths of unknown intent (this part takes hours)")
conf <- data.frame(sen = numeric, spe = numeric, state = character, num = numeric,
                   accu = numeric)
class <- ldply(list("Mor", "Sin", c("Son", "Dgo"),
                    c("QR", "Camp", "Yuc", "Tlax", "Qro",
                      "Tab", "Pue", "BCS", "Ags"),
                    "BC", "Chih", "Gro", 
                    "DF", "Gto", "Hgo",
                    c("Jal", "Col", "Nay"), "Mex", "Mich",
                    c("Oax", "Chis"), 
                    c("Tamps", "SLP", "Coah", "Zac", "NL"), "Ver",
                    c("Other", "LATAM", "USA")),
               function(x) classify(deaths, x))

class$abbrev <- NULL
deaths$abbrev <- NULL

test_that("Classifier doens't modify rows", {
            expect_that(nrow(subset(class, intent =="Accident" &
                       mechanism == "Firearm" )), equals(nrow(subset(class,
                           intent =="Accident" &
                                mechanism_detail== "Firearm" ))))
         })


for(i in levels(as.factor(deaths$abbrev))) {
  print(str_c(, "Testing state:", i))
  test_that("Classifier doens't modify rows", {
             expect_that(nrow(subset(deaths, intent =="Accident" & abbrev == i &
                        mechanism == "Firearm" )), equals(nrow(subset(class,
                            intent =="Accident" &
                                 mechanism == "Firearm" & abbrev == i))))
          })
  }
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
  scale_y_discrete(breaks = c("Mor", "QR, Camp, Yuc, Tlax, Qro, Tab, Pue, BCS, Ags",
                    "BC", "Chih", 
                    "Gro", "DF", "Gto", "Hgo",
                    "Jal, Col, Nay", "Mex", "Mich",
                    "Oax, Chis","Sin","Son, Dgo",
                    "Tamps, SLP, Coah, Zac, NL", "Ver"),
                   labels = c("Mor", "QR, Camp, Yuc, Tlax, Qro, Tab, Pue, BCS, Ags",
                              "BC", "Chih", 
                              "Gro", "DF", "Gto", "Hgo",
                              "Jal, Col, Nay", "Mex", "Mich",
                              "Oax, Chis","Sin","Son, Dgo",
                              "Tamps, SLP, Coah, Zac, NL", "Ver")) +
  geom_vline(xintercept = .5, linetype = 2) +
  annotate("text", x = .52, y = "Mich",
           label = "random guessing", angle=90,
           hjust = 0, size = 4) +
  facet_wrap(~variable) +
  theme_bw() +
  labs(title = "Sensitivity and Specificity when Classifying\nDeaths of Unknown Intent as Homicides") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
##dev.off()
ggsave("sen-spe.svg",width = 12.50, height = 9.50, dpi = 100)
