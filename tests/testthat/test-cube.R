test_that("solution of 3-comp model is correct", {
  expect_equal(cube(0.01,0.02,0.03,0.04,0.05), c(0.101857993,0.043642958,0.004499049))
})

## note that cube.R tests only for k31

test_that("solution of 2-comp model is correct", {
  expect_equal(cube(0.01,0.02,0,0.04,0), c(0.063722813,0.006277187,0))
})

## note that cube.R tests only for k21

test_that("solution of 1-comp model is correct", {
  expect_equal(cube(0.01,0,0,0,0), c(0.01,0,0))
})
