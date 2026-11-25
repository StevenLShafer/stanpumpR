# nalbuphine: see R/drugs_nalbuphine.R (Nie 2023, Table 4).

test_that("the published parameters, switch off", {
  pk <- nalbuphine(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(pk$v1, pk$v2), c(32.5, 83.5))
  expect_equal(c(pk$cl1, pk$cl2), c(32.9, 245) / 60)
  expect_equal(c(pk$v3, pk$cl3), c(1, 0))
  # the same for a woman and a child: fixed published values
  pk2 <- nalbuphine(50, 160, 70, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(pk2$cl1, pk$cl1)
})

test_that("the terminal half-life of the published model", {
  pk <- nalbuphine(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  beta <- ((k10 + k12 + k21) - sqrt((k10 + k12 + k21)^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / beta / 60, 2.618, tolerance = 0.002)
})

test_that("the switch on is anchored at the study's cohort", {
  # the geometric mean of the cohort's man and woman, weighted by their numbers
  # (12 and 15), has a factor of exactly 1
  co <- NALBUPHINE_COHORT
  m <- nalbuphine(co$weight, co$height, co$age, "male", TRUE)$PK$default
  f <- nalbuphine(co$weight, co$height, co$age, "female", TRUE)$PK$default
  gm <- function(a, b) exp((12 * log(a) + 15 * log(b)) / 27)
  expect_equal(gm(m$v1, f$v1), 32.5, tolerance = 1e-8)
  expect_equal(gm(m$v2, f$v2), 83.5, tolerance = 1e-8)
  expect_equal(gm(m$cl1, f$cl1), 32.9 / 60, tolerance = 1e-8)
  expect_equal(gm(m$cl2, f$cl2), 245 / 60, tolerance = 1e-8)
  # a larger patient has larger volumes and clearances
  big <- nalbuphine(120, 170, 50, "male", TRUE)$PK$default
  expect_gt(big$v1, m$v1)
  expect_gt(big$cl1, m$cl1)
})

test_that("no pregnancy argument: nothing is applied", {
  expect_false("pregnant" %in% names(formals(nalbuphine)))
})
