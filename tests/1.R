# Example Testing Script

expect_that(1, equals(1))
##test_that(wday(formatWeekly(deaths, min, max)$date_occur), equals(rep(1, nrow(deaths))))
##test_that("", {expect_that(deaths$date_occur[which(is.na(deaths$date_occur))], equals(character(0)))})
test_that("maximum age", {expect_that(max(deaths$age, na.rm = TRUE), equals(120))})
#Substract those homicides which occurred outside of Mexico and compare with the data
#available at http://www.inegi.org.mx/est/contenidos/espanol/proyectos/continuas/vitales/bd/mortalidad/MortalidadGeneral.asp?s=est&c=11144
test_that("number of registered homicides", {
  expect_that(ddply(subset(deaths, intent =="Homicide"), .(year(date_reg)), nrow)$V1, equals(
c(9330, 9926, 10454, 8868, 14007, 19804, 25757) -c(2, 8, 5, 4, 2, 0, 0) ))})
