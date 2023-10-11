library(lubridate)

test_that("valid clock times are returned", {
  expect_equal(deltaToClockTime("08:00", c(7,60,135,1320)), c("08:07", "09:00", "10:15", "06:00"))
})

# TODO check usage of "none", here and in server.R

test_that("valid clock times are returned", {
  expect_equal(deltaToClockTime("none", c(7,540,615,360)), c(7,540,615,360))
})
