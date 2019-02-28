ddply(subset(injury.intent, intent =="Homicide"),
                    .(year_reg), nrow)

ddply(subset(injury.intent, mechanism =="Firearm" & is.na(intent)),
                    .(year_reg), nrow)
ddply(subset(injury.intent, mechanism =="Firearm"),
                    .(year_reg, intent), nrow)

ddply(subset(injury.intent, intent == "Homicide" & year_reg== 2012),
                    .(state_occur_death, state_reg), nrow)

ddply(subset(injury.intent, mechanism =="Firearm" & is.na(intent)),
                    .(year_reg, state_occur_death), nrow)

ddply(subset(injury.intent, intent == "Homicide" & mun_occur_death == 999),
                    .(year_reg), nrow)

tail(ddply(injury.intent,
                    .(mun_occur_death2), nrow))
head(injury.intent)
