# psilocybin: see the header of R/drugs_psilocybin.R.  Holze 2023's
# one-compartment oral model, nominal 15 mg arm, fitted on a
# psilocin-equivalent dose basis and rescaled here so that the dose entered is
# psilocybin and the curve is free plasma psilocin.  The reproduction tests
# are the paper's published half-life (1.8 h), peak time (about 2 h) and peak
# free psilocin concentration (11 ng/mL, 95% CI 10-13).

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the rescaled apparent parameters with the fat-free-mass switch off", {
  actual <- psilocybin(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 623.41019,       # 448 L / 0.718628
        v2 = 1,
        v3 = 1,
        cl1 = 3.9890831,      # 172 L/h / 0.718628 / 60
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.014833333,  # 0.89 /h
        bioavailability_PO = 1,
        tlag_PO = 18          # 0.30 h
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("the molecular weight ratio is stoichiometry, not a bioavailability", {
  expect_equal(PSILOCIN_PER_PSILOCYBIN, 204.27 / 284.25)
  expect_equal(PSILOCIN_PER_PSILOCYBIN, 0.718628, tolerance = 1e-6)
  x <- psilocybin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # The conversion is carried by the rescaled parameters, so bioavailability
  # stays 1: the apparent scale already contains the real bioavailability and
  # applying another would double-discount the exposure.
  expect_equal(x$bioavailability_PO, 1)
  expect_equal(x$v1 * PSILOCIN_PER_PSILOCYBIN, 448)
  expect_equal(x$cl1 * 60 * PSILOCIN_PER_PSILOCYBIN, 172)
})

test_that("rescaling the dose basis leaves the predicted concentration unchanged", {
  x <- psilocybin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # A dose of 13.8 mg psilocybin is 9.9 mg psilocin-equivalent.  The source
  # parameters acting on the psilocin-equivalent dose and the rescaled
  # parameters acting on the psilocybin dose give the same concentration,
  # because only the ratio dose/volume matters.
  psilocybinDose <- 13.8
  equivalentDose <- psilocybinDose * PSILOCIN_PER_PSILOCYBIN
  expect_equal(equivalentDose, 9.91707, tolerance = 1e-4)
  expect_equal(psilocybinDose / x$v1, equivalentDose / 448)
  # The elimination rate constant is untouched by the rescaling
  expect_equal(x$cl1 / x$v1 * 60, 172 / 448)
})

test_that("the parameters reproduce the paper's published half-life", {
  x <- psilocybin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  halfLife <- log(2) / (x$cl1 / x$v1) / 60
  expect_equal(halfLife, 1.805, tolerance = 2e-3)   # h; paper 1.8 (95% CI 1.7-2.0)
  expect_gt(halfLife, 1.7)
  expect_lt(halfLife, 2.0)
})

test_that("the 15 mg arm's assayed content reproduces its published peak", {
  # Holze's nominal 15 mg capsules assayed 13.8 mg psilocybin.  The paper
  # reports a peak free psilocin concentration of 11 ng/mL (95% CI 10-13)
  # reached after an average of 2 hours.
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "psilocybin", Time = 0, Dose = 13.8, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$psilocybin$wide
  expect_gt(max(w$Plasma), 10)
  expect_lt(max(w$Plasma), 13)
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 1.98, tolerance = 0.03)
})

test_that("nothing reaches plasma during the lag", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "psilocybin", Time = 0, Dose = 25, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$psilocybin$wide
  expect_true(all(w$Plasma[w$Time < 18] == 0))
  expect_true(all(w$Plasma >= 0))
})

test_that("psilocybin is offered orally only, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "psilocybin", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(doseRoute(units), ROUTE_PO)
  expect_setequal(units, "mg PO")
  expect_true(is.na(row$Bolus.Units))
  expect_true(is.na(row$Infusion.Units))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  expect_equal(getDrugPK("psilocybin", 70, 171, 50, "male")$PK$default$ke0, 0)
})
