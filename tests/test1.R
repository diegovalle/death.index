# Example Testing Script

expect_that(1, equals(1))
##test_that(wday(formatWeekly(deaths, min, max)$date_occur), equals(rep(1, nrow(deaths))))
##test_that("", {expect_that(deaths$date_occur[which(is.na(deaths$date_occur))], equals(character(0)))})
test_that("maximum age", {
    expect_that(max(deaths$age_years, na.rm = TRUE), equals(120))
})
#Substract those homicides which occurred outside of Mexico and compare with the data
#available at http://www.inegi.org.mx/est/contenidos/espanol/proyectos/continuas/vitales/bd/mortalidad/MortalidadGeneral.asp?s=est&c=11144

suicides <- c(4117, 4315, 4277, 4395, 4681, 5190, 5012, 5718, 5550, 5909,6337,6425)
na <- c(2957, 2932, 2793, 2376, 2567, 2920, 3594, 5630, 4375,4198,4334, 4122)
homicides <- c(9330, 9926, 10454,
                    8868, 14007, 19804,
                    25757, 27213, 25967, 23063, 20010, 20763)
accidents <- c(34880, 35865, 36282,
                    39343, 38880, 39461,
                    38120, 36694, 37729, 36295, 35337, 37190)

test_that("number of registered homicides", {
  expect_that(ddply(subset(injury.intent, intent =="Homicide"),
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

ddply(injury.intent,
      .(year_reg, intent), nrow)
