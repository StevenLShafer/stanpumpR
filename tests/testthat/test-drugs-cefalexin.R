# cefalexin: see the header of R/drugs_cefalexin.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- cefalexin(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 26.63,
        v2 = 1,
        v3 = 1,
        cl1 = 0.233,
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.029833333,
        bioavailability_PO = 1,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 4.7, upperTypical = 4.7, lowerTypical = 2.4,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- cefalexin(120, 170, 50, "male")
  expected <- list(
    v1 = 34.749664,
    v2 = 1,
    v3 = 1,
    cl1 = 0.28447264,
    cl2 = 0,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("cefalexin is offered orally only, because the parameters are apparent", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- dd$Units[dd$Drug == "cefalexin"]
  expect_true(all(grepl("PO", strsplit(units, ",")[[1]])))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "cefalexin"]))
  expect_equal(cefalexin(70, 171, 50, "male")$PK$default$bioavailability_PO, 1)
})

test_that("the published allometry on total weight is reproduced with the switch off", {
  x <- cefalexin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  y <- cefalexin(35, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 13.98); expect_equal(x$v1, 26.63)
  expect_equal(y$cl1 / x$cl1, 0.5^0.75); expect_equal(y$v1 / x$v1, 0.5)
  # half-time at 70 kg, 1.32 h
  expect_equal(log(2) / (x$cl1 / x$v1) / 60, 1.32035, tolerance = 1e-5)
})

test_that("pregnancy: apparent clearance x 1.5, by analogy with cephradine", {
  # Philipson 1987 (cephradine, CL x 1.64) and the 50% rise in filtration.
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.
  on  <- getDrugPK("cefalexin", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("cefalexin", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("cefalexin", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default; b <- off$PK$default
  expect_equal(a$cl1 / b$cl1, CEFALEXIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(CEFALEXIN_PREGNANCY_CL_FACTOR, 1.5)
  expect_equal_rounded(0.2973287274, a$cl1)
  expect_identical(a[c("v1", "v2", "v3", "cl2", "cl3", "ka_PO",
                       "bioavailability_PO", "tlag_PO")],
                   b[c("v1", "v2", "v3", "cl2", "cl3", "ka_PO",
                       "bioavailability_PO", "tlag_PO")])
  expect_match(on$reference, "Philipson", fixed = TRUE)
  expect_no_match(off$reference, "Philipson", fixed = TRUE)
})
