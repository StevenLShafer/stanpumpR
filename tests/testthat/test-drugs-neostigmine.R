# neostigmine: see the header of R/drugs_neostigmine.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- neostigmine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 0.7602,
        v2 = 7.7579385,
        v3 = 1,
        cl1 = 0.4006254,
        cl2 = 0.3025596,
        cl3 = 0
      )
    ),
    tPeak = 4.6, MEAC = 0, typical = 100, upperTypical = 300, lowerTypical = 30,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- neostigmine(120, 170, 50, "male")
  expected <- list(
    v1 = 0.99199003,
    v2 = 10.123385,
    v3 = 1,
    cl1 = 0.48912861,
    cl2 = 0.36939883,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Calvey patient 1 is reproduced, with the half-times the paper reports", {
  x <- neostigmine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1 / 70, 0.01086)                       # 10.86 mL/kg
  expect_equal(x$cl1 / x$v1, 0.527); expect_equal(x$cl2 / x$v1, 0.398)
  expect_equal(x$cl2 / x$v2, 0.039)
  # Illustrative patient-1 macroparameters in the specification, per kg
  expect_equal(x$v2 / 70, 0.1108277, tolerance = 1e-6)
  expect_equal(x$cl1 * 60 / 70, 0.3433932, tolerance = 1e-6)
  expect_equal(x$cl2 * 60 / 70, 0.2593368, tolerance = 1e-6)
  # Fast half-time under a minute, slow one at the top of Calvey's 15-32 min
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  expect_equal(sort(log(2) / r[r > 0]), c(0.7356799, 31.77509), tolerance = 1e-5)
  # The effect site is live, from Heier's 4.6 min
  expect_gt(getDrugPK("neostigmine", 70, 171, 50, "male")$PK$default$ke0, 0)
})
