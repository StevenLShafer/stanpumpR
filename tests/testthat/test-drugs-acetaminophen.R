# Acetaminophen: Morse 2022 intravenous and fasted-tablet disposition with
# clearance on normal fat mass, and ke0 from Anderson 2001.  See the header of
# R/drugs_acetaminophen.R.  Pins were worked out from the published numbers by
# hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # Clearance uses normal fat mass in either switch position (70 kg, 171 cm
  # male: NFM 67.17 kg against Morse's standard 67.45 kg), so it is not
  # exactly 24 L/h here.
  actual <- acetaminophen(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 43.7,
        v2 = 29.7,
        v3 = 1,
        cl1 = 0.398876879,
        cl2 = 0.725,
        cl3 = 0,
        ka_PO = 0.0456808881,
        bioavailability_PO = 0.86,
        tlag_PO = 0
      )
    ),
    tPeak = 0,
    ke0 = 0.0130782487,
    MEAC = 0,
    typical = 10,
    upperTypical = 20,
    lowerTypical = 5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg.  Clearance follows its own
  # normal-fat-mass covariate, 24 x ((71.09 + 0.816 x 48.91) / 67.45)^0.75
  # L/h; the volumes see the pharmacokinetic weight 91.34 kg; Q takes the
  # library clearance factor 1.2209126.
  actual <- acetaminophen(120, 170, 50, "male")
  expected <- list(
    v1 = 57.024420436,
    v2 = 38.75572739,
    v3 = 1,
    cl1 = 0.581202027,
    cl2 = 0.885161648,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Morse's vector is recovered for her standard 70 kg, 176 cm man", {
  x <- acetaminophen(70, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 24.0)
  expect_equal(x$v1, 43.7); expect_equal(x$v2, 29.7); expect_equal(x$cl2 * 60, 43.5)
  # The library's reference man is a little fatter: 23.92 L/h
  y <- acetaminophen(70, 170, 35, "male")$PK$default
  expect_equal(y$cl1, 0.398647084, tolerance = 1e-8)
  expect_equal(y$v1, 43.7); expect_equal(y$cl2 * 60, 43.5)
  # With the switch off, volumes and Q follow total weight as published
  z <- acetaminophen(140, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(z$v1, 87.4); expect_equal(z$v2, 59.4)
  expect_equal(z$cl2 * 60, 43.5 * 2^0.75)
})

test_that("the oral lag is folded into ka with the same mean input time", {
  x <- acetaminophen(70, 170, 35, "male")$PK$default
  expect_equal(x$tlag_PO, 0)
  expect_equal(1 / x$ka_PO, 5.3 + 11.5 / log(2))   # 21.89 min
  expect_equal(x$bioavailability_PO, 0.86)
})

test_that("ke0 is Anderson's 53 min equilibration half-time", {
  PK <- getDrugPK("acetaminophen", 70, 170, 35, "male",
                  getDrugDefaults("acetaminophen"))$PK$default
  expect_equal(log(2) / PK$ke0, 53)
})

test_that("1 g intravenous and oral match an independent integration", {
  # Reference man, switch on.  Expected values from a fourth-order Runge-Kutta
  # integration of the two-compartment model with an effect site (Python,
  # step 0.01 min), not from the closed-form engine.
  iv <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg"),
    noEvents, 70, 170, 35, "male", 360, TRUE)$acetaminophen$wide
  at <- function(w, t, col) w[[col]][which.min(abs(w$Time - t))]
  expect_equal(at(iv, 60,  "Plasma"),      9.0226288, tolerance = 1e-4)
  expect_equal(at(iv, 240, "Plasma"),      3.3938961, tolerance = 1e-4)
  expect_equal(at(iv, 60,  "Effect Site"), 6.9876389, tolerance = 1e-4)
  expect_equal(at(iv, 240, "Effect Site"), 4.8834844, tolerance = 1e-4)

  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 360, TRUE)$acetaminophen$wide
  expect_equal(at(po, 60,  "Plasma"),      9.0753163, tolerance = 1e-4)
  expect_equal(at(po, 240, "Plasma"),      3.2732578, tolerance = 1e-4)
  expect_equal(at(po, 60,  "Effect Site"), 4.7794932, tolerance = 1e-4)
  # Fasted 1 g tablet peaks near 10 mg/L at about 35 min
  expect_equal(po$Time[which.max(po$Plasma)], 35, tolerance = 0.05)
  expect_gt(max(po$Plasma), 9.5)
  expect_lt(max(po$Plasma), 10.5)
})
