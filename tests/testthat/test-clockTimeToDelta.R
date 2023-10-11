library(lubridate)

test_that("valid delta times are returned", {
  expect_equal(clockTimeToDelta("08:00", c("7", "09:00", "10:15", "06:00")), c(7,60,135,1320))
})

test_that("valid delta times are returned", {
  expect_equal(clockTimeToDelta("none", c("7", "09:00"," 10:15", "06:00")), c(7,540,615,360))
})

test_that("valid delta times are returned", {
  expect_equal(clockTimeToDelta("none", c("7", "360")), c(7,360))
})
