# neostigmine: see the header of R/drugs_neostigmine.R for what the model is and what it
# leaves out.  Pins were worked out from Heier 2002 Table 3 and the
# fat-free-mass factors by hand, not from the code under test.

test_that("returns Heier's Table 3 unscaled with the fat-free-mass switch off", {
  actual <- neostigmine(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 5.59,
        v2 = 15.0,
        v3 = 110,
        cl1 = 0.696,
        cl2 = 1.480,
        cl3 = 0.556
      )
    ),
    tPeak = 4.6, MEAC = 0, typical = 100, upperTypical = 300, lowerTypical = 30,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # The source is not weight-normalised: with the switch off, weight is ignored
  expect_identical(neostigmine(55, 160, 30, "female", adjustToFFM = FALSE)$PK,
                   actual$PK)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.2209126.
  actual <- neostigmine(120, 170, 50, "male")
  expected <- list(
    v1 = 7.2944285,
    v2 = 19.573601,
    v3 = 143.53974,
    cl1 = 0.84975517,
    cl2 = 1.8069506,
    cl3 = 0.67882741
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Heier's model gives the derived half-lives", {
  x <- neostigmine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Vss 130.6 L; half-lives 1.3, 16.5 and 256 min, derived from Table 3
  expect_equal(x$v1 + x$v2 + x$v3, 130.59)
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, x$cl3 / x$v1, x$cl2 / x$v2, x$cl3 / x$v3)
  expect_equal(sort(log(2) / r), c(1.26529, 16.5306, 256.417), tolerance = 1e-5)
  # The effect site is live, from Heier's 4.6 min
  expect_gt(getDrugPK("neostigmine", 70, 171, 50, "male")$PK$default$ke0, 0)
})
