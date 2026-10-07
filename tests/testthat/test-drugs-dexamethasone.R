# dexamethasone: see the header of R/drugs_dexamethasone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- dexamethasone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 41.6,
        v2 = 44.454902,
        v3 = 1,
        cl1 = 0.30166667,
        cl2 = 0.75573333,
        cl3 = 0,
        ka_PO = 0.0156,
        bioavailability_PO = 0.81,
        tlag_PO = 0,
        ka_IM = 0.0076666667,
        bioavailability_IM = 0.77884615,
        tlag_IM = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 50, upperTypical = 100, lowerTypical = 20,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- dexamethasone(120, 170, 50, "male")
  expected <- list(
    v1 = 54.284116,
    v2 = 58.009497,
    v3 = 1,
    cl1 = 0.36830864,
    cl2 = 0.92268436,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Hong's microconstants give the published macroparameters", {
  x <- dexamethasone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 18.1); expect_equal(x$v1, 41.6)
  expect_equal(x$cl2 * 60, 45.344, tolerance = 1e-9)        # Q = k12 Vc
  expect_equal(x$v2, 44.454902, tolerance = 1e-7)            # Vp = Q / k21
  expect_equal(x$cl2 / x$v1 * 60, 1.09); expect_equal(x$cl2 / x$v2 * 60, 1.02)
  # Eigenvalue half-times 0.294 and 3.68 h
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  expect_equal(sort(log(2) / r[r > 0] / 60), c(0.29411, 3.68096), tolerance = 1e-4)
  # Routes: oral F 0.81 with ka 0.936 /h; intramuscular F 0.81/1.04 with ka 0.460 /h
  expect_equal(x$bioavailability_PO, 0.81); expect_equal(x$ka_PO * 60, 0.936)
  expect_equal(x$bioavailability_IM, 0.81 / 1.04); expect_equal(x$ka_IM * 60, 0.460)
})
