# phenytoin: see the header of R/drugs_phenytoin.R for the Odani 1996
# Michaelis-Menten model, the salt and PE conversions, and the CYP2C9 term.
# The engine itself is tested in test-michaelis-menten.R.  Expected values are
# the published numbers written out, not taken from the code under test.

test_that("the reference patient gets Odani's parameters, switch off", {
  a <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE)
  s <- 42 * (70 / 42)^0.463               # weight term at total body weight
  expect_equal(a$PK$default$v1, 1.23 * s, tolerance = 1e-8)
  expect_equal(a$michaelisMenten$vmax, 9.80 * s / (24 * 60), tolerance = 1e-8)
  expect_equal(a$michaelisMenten$km, 9.19)
  expect_equal(a$PK$default$cl1, a$michaelisMenten$vmax / a$michaelisMenten$km)
  expect_equal(a$tPeak, 0)
  expect_equal(a$MEAC, 0)
  expect_equal(a$michaelisMenten$saltFactor$IV, 252.27 / 274.25, tolerance = 1e-8)
  expect_equal(a$michaelisMenten$saltFactor$liquid, 1)
  expect_equal(unname(a$michaelisMenten$kConversion), log(2) / 15)
})

test_that("CYP2C9 diplotype lowers Vmax and nothing else", {
  n <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1")
  i <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*3")
  p <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE, cyp2c9 = "*3/*3")
  expect_equal(n$michaelisMenten$vmax,
               phenytoin(70, 170, 40, "male", adjustToFFM = FALSE,
                         cyp2c9 = "*1/*2")$michaelisMenten$vmax)
  expect_equal(i$michaelisMenten$vmax, n$michaelisMenten$vmax * 0.67)
  expect_equal(p$michaelisMenten$vmax, n$michaelisMenten$vmax * 0.5)
  expect_equal(i$PK$default$v1, n$PK$default$v1)
  expect_error(phenytoin(70, 170, 40, "male", cyp2c9 = "normal"))
})

test_that("it scales to fat-free mass", {
  a <- phenytoin(120, 170, 50, "male", adjustToFFM = TRUE)
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  s <- 42 * (size$pkWeight / 42)^0.463
  expect_equal(a$PK$default$v1, 1.23 * s, tolerance = 1e-8)
})
