# Dacarbazine (R/drugs_dacarbazine.R): Breithaupt 1982, two compartments
# derived per patient from CL, Vss and both half-lives.  Drafted by Claude
# Code, 2026-11-13.

noEvents <- data.frame(Time = double(), Event = character())

test_that("each patient's derived model returns his own CL and Vss", {
  p <- DACARBAZINE_BREITHAUPT
  for (i in seq_len(nrow(p))) {
    m <- twoCompartmentFromClVss(p$tAlpha[i], p$tBeta[i], p$cl[i] / 1000, p$vss[i])
    expect_equal(m$v1 + m$v2, p$vss[i])
    expect_equal(m$cl, p$cl[i] / 1000)
    # and his two half-lives
    k10 <- m$cl / m$v1; k12 <- m$q / m$v1; k21 <- m$q / m$v2
    s <- k10 + k12 + k21
    ex <- (s + c(1, -1) * sqrt(s^2 - 4 * k10 * k21)) / 2
    expect_equal(log(2) / ex, c(p$tAlpha[i], p$tBeta[i]))
  }
})

test_that("the mean model, and the checks the derivation did not use", {
  m <- dacarbazineBreithaupt()
  expect_equal(round(c(m$v1, m$v2), 3), c(0.178, 0.454))
  expect_equal(round(c(m$cl, m$q) * 1000, 1), c(15.4, 21.9))
  expect_equal(round(m$v1 + m$v2, 3), 0.632)                       # the paper's mean Vss
  # V1 against Table 2's Ve (Dose / C0), patient by patient
  expect_equal(m$each$v1, DACARBAZINE_BREITHAUPT$ve, tolerance = 0.45)
  expect_equal(round(mean(m$each$v1), 3), 0.178)
  expect_equal(round(mean(DACARBAZINE_BREITHAUPT$ve), 3), 0.202)
  # the fast phase's share of the area: the text's 28 +/- 7%, range 21-35%
  expect_equal(round(mean(m$each$fastShare), 2), 0.30)
})

test_that("an impossible half-life set is refused", {
  expect_error(twoCompartmentFromClVss(3, 40, 0.0154, 0.05), "two-compartment")
})

test_that("per kilogram, at the pharmacokinetic weight with the switch on", {
  m <- dacarbazineBreithaupt()
  off <- dacarbazine(80, 175, 50, "male", FALSE)$PK$default
  expect_equal(c(off$v1, off$v2, off$cl1, off$cl2), 80 * c(m$v1, m$v2, m$cl, m$q))
  on <- dacarbazine(80, 175, 50, "male")$PK$default
  w <- pkSizeFactors(80, 175, 50, "male", TRUE)$pkWeight
  expect_equal(on$cl1, w * m$cl)
})

test_that("a 250 mg/m2 bolus: the terminal half-life is the paper's 41 min", {
  dose <- data.frame(Drug = "dacarbazine", Time = 0, Dose = 450, Units = "mg")
  w <- simulateDrugsWithCovariates(dose, noEvents, 70, 175, 50, "male", 6 * 60, FALSE,
                                   adjustToFFM = FALSE)$dacarbazine$wide
  r <- approx(w$Time, w$Plasma, c(180, 220))$y
  expect_equal(round(40 * log(2) / log(r[1] / r[2]), 1), 41.3)          # paper 41.4
  expect_equal_rounded(approx(w$Time, w$Plasma, 1)$y, 29.35171)          # mcg/mL
})

test_that("dacarbazine is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "dacarbazine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("dacarbazine", 70, 170, 50, "male")$reference, "Breithaupt")
  expect_identical(therapeuticRange("dacarbazine")$band_status, "not_applicable")
})
