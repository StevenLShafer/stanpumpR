# metronidazole: see the header of R/drugs_metronidazole.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- metronidazole(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 37.866665,
        v2 = 1,
        v3 = 1,
        cl1 = 0.053666667,
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.022266667,
        bioavailability_PO = 0.841,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 8, upperTypical = 25, lowerTypical = 4,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- metronidazole(120, 170, 50, "male")
  expected <- list(
    v1 = 49.018278,
    v2 = 1,
    v3 = 1,
    cl1 = 0.065522311,
    cl2 = 0,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("volume follows Devine adjusted body weight with the switch off", {
  # Devine 171 cm male: 50 + 2.3 x (67.32 - 60) = 66.84 kg; adjusted 68.10 kg
  expect_equal(idealBodyWeightDevine(171, "male"), 50 + 2.3 * (171 / 2.54 - 60))
  expect_equal(idealBodyWeightDevine(160, "female"), 45.5 + 2.3 * (160 / 2.54 - 60))
  x <- metronidazole(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1, 0.556 * adjustedBodyWeight(70, 171, "male"))
  expect_equal(x$cl1 * 60, 3.22)
  # Half-time at the illustrative TBW = AJBW = 70 kg: 8.378 h
  expect_equal(log(2) / (3.22 / (0.556 * 70)), 8.37804, tolerance = 1e-5)
  # Oral: F 0.841, ka 1.336 /h
  expect_equal(x$bioavailability_PO, 0.841); expect_equal(x$ka_PO * 60, 1.336)
})
