# Example Testing Script

expect_that(1, equals(1))
##test_that(wday(formatWeekly(deaths, min, max)$date_occur), equals(rep(1, nrow(deaths))))
##test_that("", {expect_that(deaths$date_occur[which(is.na(deaths$date_occur))], equals(character(0)))})
test_that("maximum age", {
    expect_that(max(injury.intent$age_years, na.rm = TRUE), equals(120))
})
#Substract those homicides which occurred outside of Mexico and compare with the data
#available at https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/mortalidadgeneral.asp?s=est&c=11144&proy=mortgral_mg

suicides <- c(4117, 4315, 4277, 4395, 4681, 5190, 5012, 5718, 5550, 5909,6337,
              6425,6370,6559, 6808,7225	)
na <- c(2957, 2932, 2793, 2376, 2567, 2920, 3594, 5630, 4375,4198,4376, 4122,
        4393,5427, 5567,6399	)
homicides <- c(9330, 9926, 10454,
                    8868, 14007, 19804,
                    25757, 27213, 25967, 23063, 20013, 20763,24560,32082, 36687,36662)
accidents <- c(34880, 35865, 36282,
                    39343, 38880, 39461,
                    38120, 36694, 37729, 36295, 35817, 37190,37429,36220,34591,33525)
legi <- c(39,
          72,
          48,
          47,
          39,
          34,
          37,
          65,
          115,
          120,
          97,
          77,
          69,
          112,
          96,
          71
)

test_that("number of registered homicides", {
  expect_that(ddply(subset(injury.intent, intent =="Homicide" & state_reg %in% 1:32),
                    .(year_reg), nrow)$V1,
              equals(
                  homicides ))})


test_that("number of registered accidents", {
  expect_that(ddply(subset(injury.intent, intent =="Accident"),
                    .(year_reg), nrow)$V1,
              equals(
                  accidents ))})


test_that("number of registered suicides", {
  expect_that(ddply(subset(injury.intent, intent =="Suicide"),
                    .(year_reg), nrow)$V1,
              equals(
                  suicides ))})

test_that("number of registered na", {
  expect_that(ddply(subset(injury.intent, is.na(intent)),
                    .(year_reg), nrow)$V1,
              equals(
                  na ))})

test_that("number of registered legal interventions", {
  expect_that(ddply(subset(injury.intent, intent == "Legal Intervention"),
                    .(year_reg), nrow)$V1,
              equals(
                legi ))})


ddply(injury.intent,
      .(year_reg, intent), nrow)
