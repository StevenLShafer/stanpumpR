# neostigmine: see the header of R/drugs_neostigmine.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand, not from the code under test.

test_that("returns Heier's published parameters with the fat-free-mass switch off", {
  # Heier 2002 Table 3, normothermia; not weight-related, so the same for
  # every patient with the switch off.  Heier's model since 2026-10-28, by
  # decision of Steven L. Shafer (Calvey 1979 patient 1 before).
  actual <- neostigmine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 5.59,
        v2 = 15.0,
        v3 = 110,
        cl1 = 0.696,
        cl2 = 1.48,
        cl3 = 0.556
      )
    ),
    tPeak = 3.5, MEAC = 0, typical = 100, upperTypical = 300, lowerTypical = 30,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  expect_equal(neostigmine(110, 171, 50, "male", adjustToFFM = FALSE)$PK$default,
               actual$PK$default)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.2209126.
  actual <- neostigmine(120, 170, 50, "male")
  expected <- list(
    v1 = 7.2944285,
    v2 = 19.5736,
    v3 = 143.53974,
    cl1 = 0.8497552,
    cl2 = 1.8069506,
    cl3 = 0.6788274
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Heier's half-lives, and the effect site of his two-minute infusion", {
  x <- neostigmine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, x$cl3 / x$v1, x$cl2 / x$v2, x$cl3 / x$v3)
  # 1.3, 16.5 and 256 min (derived from Table 3)
  expect_equal(sort(log(2) / r), c(1.27, 16.5, 256), tolerance = 0.01)
  # tPeak 3.5 solves to ke0 about 0.199 /min, and with it a 2-min infusion
  # (Heier's 70 mcg/kg) peaks at the effect site at about 4.6 min from its
  # start, Heier's median onset.
  pk <- getDrugPK("neostigmine", 70, 171, 50, "male")$PK$default
  expect_equal(pk$ke0, 0.199, tolerance = 0.01)
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "neostigmine", Time = c(0, 2), Dose = c(4.9 * 30, 0),
               Units = c("mg/hr", "mg/hr")),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", 30, FALSE)$neostigmine$wide
  expect_equal(w$Time[which.max(w$"Effect Site")], 4.6, tolerance = 0.06)
})
