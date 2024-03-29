
deaths$abbrev <- stateToAbbrev(deaths$state_reg)
expect_equal (sum(!is.na(deaths$abbrev)), nrow(deaths))
expect_equal (sort(unique(deaths$abbrev)), 
             c("Ags", "BC", "BCS", "Camp", "Chih", "Chis", "Coah", "Col", 
               "DF", "Dgo", "Gro", "Gto", "Hgo", "Jal", "Mex", "Mich", "Mor", 
               "Nay", "NL", "Oax", "Pue", "QR", "Qro", "Sin", "SLP", "Son", 
               "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac"))
sum(deaths$abbrev == "Other")
sum(deaths$abbrev == "LATAM")
sum(deaths$abbrev == "USA")

classify <- function(deaths, states) {
    print(states)
       
    df <- subset(deaths, abbrev %in% states)
    df$intent.nolegal <- NULL
    df$intent.nolegal <- df$intent
    df$intent.nolegal <- car::recode(df$intent.nolegal,
                                     "'Legal Intervention' = 'Homicide'")
    df$month <- as.factor(month(df$date_reg))
    ## Error in Sinaloa Firearm accidents 2007 and 2008
    if("Sin" %in% states) {
        df$intent.nolegal[(year(df$date_reg)) %in% 2007:2008 &
                          (df$intent.nolegal == "Accident"  &
                             df$mechanism %in% "Firearm" &
                           df$abbrev == "Sin") &
                          !is.na(df$intent.nolegal)] <- NA
    }
    ## Error in 2007 Feb and Jan DF Homicides
    if("DF" %in% states){
        df$intent.nolegal[(
          df$month %in% 1:2 &
            year(df$date_reg) == 2007 & 
            df$intent.nolegal %in% "Accident" & 
            df$abbrev == "DF")] <- NA
    }
    ## Error in 2007 BC firearm accidents
    if("BC" %in% states){
        df$intent.nolegal[(df$intent.nolegal %in% "Accident" &
                           df$mechanism %in%"Firearm" & df$abbrev == "BC" &
                           df$year_reg == 2007)] <- NA
        
    }
    df <- droplevels(df)
    df$mechanism_recode <- car::recode(df$mechanism, "'Cut/pierce'='Cut/pierce';
                                 'Drowning'='Other';
                                 'Fall'='Fall';
                                 'Fire/hot object or substance'='Other';
                                 'Firearm'='Firearm';
                                 'Machinery'='Other';
                                 'All Transport'='All Transport';
                                 'Natural/environmental'='Other';
                                 'Overexertion'='Other';
                                 'Poisoning'='Other';
                                 'Struck by or against'='Struck by or against';
                                 'Suffocation'='Suffocation';
                                 'Other specified, classifiable'='Other';
                                 'Other specified, nec'='Other';
                                 'Unspecified'=NA;
                                 'Adverse effects'='Other';")
  
    y <- c("intent.nolegal")
    x <- c("mechanism_recode", "age_years", "sex")
    formula <-  intent.nolegal ~ age_years + mechanism_recode + sex
    algo <- "ranger"
   
    ##subset all deaths that are of known injury intent
    df.train <- df[!is.na(df$intent.nolegal),]
    ##divide the data set into training and test
    inTrain <- createDataPartition(1:nrow(df.train), p = 4/5, list = FALSE)
    train <- na.omit(df.train[inTrain,c(x,y)])
    test <- na.omit(df.train[-inTrain,c(x,y)])
    bootControl <- trainControl(#method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                returnData = FALSE)
  
    tgrid <- expand.grid(
      .mtry = 2:4,
      .splitrule = "gini",
      .min.node.size = c(10, 20)
    )
    rpartFit <- train(formula,
                      data = train,
                      preProcess = c("center", "scale"),
                      method = algo,
                      trControl = bootControl,
                      tuneGrid = tgrid,
                      num.trees = fit.num.trees,
                      importance = "permutation")
    fit.pred.rpart <- predict(rpartFit, test)
    print(confusionMatrix(fit.pred.rpart, test$intent.nolegal))
    print(varImp(rpartFit))
  
    
    dfImputed <- missRanger(df[, c(y, x)], 
                            pmm.k = 3, 
                            verbose = 0, 
                            num.trees = miss.num.trees,
                            num.threads = num.cores)
    
    fit.unknown <- predict(rpartFit, dfImputed)
    df$intent.imputed2 <- fit.unknown
    df$intent.imputed <- ifelse(is.na(df$intent.nolegal),
                                     as.character(df$intent.imputed2),
                                     as.character(df$intent.nolegal))
    df$mechanism_recode <- NULL
    df$intent.imputed2 <- NULL
    df$intent.nolegal <- NULL
    df$intent.imputed <- as.factor(df$intent.imputed)
    return(df)
}


set.seed(1)
message("Classifying deaths of unknown intent (this part takes hours)")
conf <- data.frame(sen = numeric, spe = numeric, state = character, num = numeric,
                   accu = numeric)

registerDoMC(num.cores)
gc()

# class <- ldply(list(c("Mex",
#                     "DF",  "Mor", "Sin",
#                     "Son", "Dgo",
#                     "BC", "Chih",
#                     "Gro",
#                     "QR", "Camp", "Yuc", "Tlax", "Qro",
#                     "Tab", "Pue", "BCS", "Ags",
#                     "Gto", "Hgo",
#                     "Jal", "Col", "Nay", "Mich",
#                     "Oax", "Chis",
#                     "Tamps", "SLP", "Coah", "Zac", "NL", "Ver"
#               )),
#                function(x) classify(deaths, x))
# save(class,
#      compress = "xz",
#      file = file.path("cache", "class.RData"))

#Mex
class1 <- ldply(list("DF"),
               function(x) classify(deaths, x))
save(class1,
     compress = "xz",
     file = file.path("cache", "class1.RData"))

gc()
class1a <- ldply(list("DF",  "Mor", "Sin"),
                function(x) classify(deaths, x))
save(class1a,
     compress = "xz",
     file = file.path("cache", "class1a.RData"))

gc()
class2 <- ldply(list(c("Son", "Dgo"),
                    c("QR", "Camp", "Yuc", "Tlax", "Qro",
                      "Tab", "Pue", "BCS", "Ags"),
                    "BC", "Chih"),
               function(x) classify(deaths, x))
save(class2,
     compress = "xz",
     file = file.path("cache", "class2.RData"))
gc()
class3 <- ldply(list("Gro", 
                    c("Gto", "Hgo"),
                    c("Jal", "Col", "Nay"), "Mich",
                    c("Oax", "Chis"), 
                    c("Tamps", "SLP", "Coah", "Zac", "NL"), "Ver"),
               function(x) classify(deaths, x))
save(class3,
     compress = "xz",
     file = file.path("cache", "class3.RData"))
gc()

class <- rbind(class1, class1a, class2, class3)
rm(class1);rm(class1a);rm(class2);rm(class3)

expect_equal(nrow(class), nrow(deaths))

class$abbrev <- NULL
deaths$abbrev <- NULL

test_that("Classifier doens't modify rows", {
            expect_that(nrow(subset(class, intent =="Accident" &
                       mechanism == "Firearm" )), equals(nrow(subset(class,
                           intent =="Accident" &
                                mechanism_detail== "Firearm" ))))
         })


for(i in levels(as.factor(deaths$abbrev))) {
  print(str_c("Testing state:", i))
  test_that("Classifier doens't modify rows", {
             expect_that(nrow(subset(deaths, intent =="Accident" & abbrev == i &
                        mechanism == "Firearm" )), equals(nrow(subset(class,
                            intent =="Accident" &
                                 mechanism == "Firearm" & abbrev == i))))
          })
  }
deaths <- class
rm(class)


# 
# conf <- melt(conf, id = c("state", "num"))
# 
# conf <- ddply(conf, .(state), transform, order = value[3])
# conf$state <- as.factor(conf$state)
# conf$state <- reorder(conf$state, conf$order)
# conf$variable <- car::recode(conf$variable, "'sen' = 'Sensitivity (Homicides)';
#                                              'spe' = 'Specificity (Homicides)';
#                                              'accu' = 'Accuracy (Overall)'")
# 
# 
# ##Cairo("sen-spe.png", width = 610, height = 650)
# ggplot(conf, aes(value,state, group = variable)) +
#   geom_point(aes(size = num)) +
#   scale_size_area("number of\nimputed\nhomicides",
#              breaks = c(350, 650, 950)) +
#   scale_x_continuous("percent", label = percent) +
#   scale_y_discrete(breaks = c("Mor", "QR, Camp, Yuc, Tlax, Qro, Tab, Pue, BCS, Ags",
#                     "BC", "Chih", 
#                     "Gro", "DF", "Gto", "Hgo",
#                     "Jal, Col, Nay", "Mex", "Mich",
#                     "Oax, Chis","Sin","Son, Dgo",
#                     "Tamps, SLP, Coah, Zac, NL", "Ver"),
#                    labels = c("Mor", "QR, Camp, Yuc, Tlax, Qro, Tab, Pue, BCS, Ags",
#                               "BC", "Chih", 
#                               "Gro", "DF", "Gto", "Hgo",
#                               "Jal, Col, Nay", "Mex", "Mich",
#                               "Oax, Chis","Sin","Son, Dgo",
#                               "Tamps, SLP, Coah, Zac, NL", "Ver")) +
#   geom_vline(xintercept = .5, linetype = 2) +
#   annotate("text", x = .52, y = "Mich",
#            label = "random guessing", angle=90,
#            hjust = 0, size = 4) +
#   facet_wrap(~variable) +
#   theme_bw() +
#   labs(title = "Sensitivity and Specificity when Classifying\nDeaths of Unknown Intent as Homicides") +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# ##dev.off()
# ggsave("sen-spe.svg",width = 12.50, height = 9.50, dpi = 100)
