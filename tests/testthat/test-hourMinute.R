library(lubridate)

test_that("HH:MM gets parsed correctly", {
    expect_equal(hourMinute("12:34"),754)
})

test_that("HHMM gets parsed correctly", {
    expect_equal(hourMinute("1234"),754)
})

test_that("AH:MM gives NA", {
    expect_true(is.na(hourMinute("A2:34")))
})
