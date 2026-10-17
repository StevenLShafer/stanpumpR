# Enoxaparin: Berges 2007 subcutaneous two-compartment anti-Xa model (elderly).
# See the header of R/drugs_enoxaparin.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the reference parameters with the switch off", {
  # 65 kg (reference weight), 82 y female; clearance uses the simplified-MDRD
  # creatinine clearance, so build the expected CL from egfrMDRD().
  scr <- 1.0
  actual <- enoxaparin(65, 160, 82, "female", adjustToFFM = FALSE, creatinine = scr)
  crcl <- egfrMDRD(82, "female", adultEquivalentCreatinine(scr, 82, "female"))
  cl <- 0.70 * (65 / 65)^0.78 * (crcl / 69)^0.25
  expected <- list(
    PK = list(default = list(
      v1 = 6.43, v2 = 8.18, v3 = 1,
      cl1 = cl / 60, cl2 = 0.34 / 60, cl3 = 0,
      ka_SC = 0.63 / 60, tlag_SC = 0, bioavailability_SC = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("clearance and central volume carry their weight exponents", {
  a <- enoxaparin(65, 160, 82, "female", adjustToFFM = FALSE, creatinine = 1.0)
  b <- enoxaparin(90, 160, 82, "female", adjustToFFM = FALSE, creatinine = 1.0)
  expect_equal(b$PK$default$cl1 / a$PK$default$cl1, (90 / 65)^0.78, tolerance = 1e-8)
  expect_equal(b$PK$default$v1  / a$PK$default$v1,  (90 / 65)^1.25, tolerance = 1e-8)
})

test_that("clearance rises with creatinine clearance (exponent 0.25)", {
  good <- enoxaparin(65, 160, 82, "female", adjustToFFM = FALSE, creatinine = 0.7)
  poor <- enoxaparin(65, 160, 82, "female", adjustToFFM = FALSE, creatinine = 1.6)
  expect_gt(good$PK$default$cl1, poor$PK$default$cl1)
})

test_that("the plotted output is anti-Xa activity only, with no effect site", {
  r <- getDrugPK("enoxaparin", 65, 160, 82, "female")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
})

test_that("a subcutaneous IU dose gives a sensible peak anti-Xa activity", {
  # 4000 IU SC in a typical elderly patient peaks near 0.3-0.5 IU/mL.
  dose <- data.frame(Drug = "enoxaparin", Time = 0, Dose = 4000, Units = "IU SC")
  r <- simulateDrugsWithCovariates(dose, noEvents, 65, 160, 82, "female",
                                   1440, TRUE, creatinine = 1.0,
                                   adjustToFFM = FALSE)$enoxaparin
  pl <- r$results[r$results$Site == "Plasma", ]
  peak <- max(pl$Y)
  expect_gt(peak, 0.2)
  expect_lt(peak, 0.6)
})

test_that("a mass unit is refused for this activity drug", {
  dose <- data.frame(Drug = "enoxaparin", Time = 0, Dose = 40, Units = "mg SC")
  expect_error(
    simulateDrugsWithCovariates(dose, noEvents, 65, 160, 82, "female", 1440, TRUE,
                                creatinine = 1.0, adjustToFFM = FALSE),
    "international units"
  )
})
