# Example Testing Script

expect_that(1, equals(1))
test_that(wday(formatWeekly(deaths, min, max)$date), equals(rep(1, nrow(deaths))))
test_that(deaths$date2[which(is.na(deaths$date2))], equals(character(0)))
