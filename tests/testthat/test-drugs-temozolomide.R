# Temozolomide (R/drugs_temozolomide.R): Ostermann 2004, oral, with a CSF
# compartment.  Drafted by Claude Code, 2026-11-13.

noEvents <- data.frame(Time = double(), Event = character())
# a patient whose Du Bois BSA is the pooled mean, so dBSA = 0
refHeight <- function(w) uniroot(function(h) bsaDuBois(w, h) - TEMOZOLOMIDE_OSTERMANN$bsaMean,
                                  c(100, 250))$root

test_that("Table 4: CL 10.0 L/h (11.9 in a man) and VD 30.3 L at the mean BSA", {
  h <- refHeight(70)
  f <- temozolomide(70, h, 50, "female", FALSE)$PK$default
  m <- temozolomide(70, h, 50, "male", FALSE)$PK$default
  expect_equal_rounded(c(f$cl1 * 60, f$v1), c(10.0, 30.3))
  expect_equal(round(m$cl1 * 60, 1), 11.9)
  expect_equal(m$v1, f$v1)
  expect_equal(f$ka_PO * 60, 5.8)
  expect_equal(f$bioavailability_PO, 1)
})

test_that("doubling the BSA (dBSA = 1) gives the text's 14.2 L/h and 50.2 L", {
  p <- TEMOZOLOMIDE_OSTERMANN
  w <- uniroot(function(w) bsaDuBois(w, 200) - 2 * p$bsaMean, c(50, 400))$root
  d <- temozolomide(w, 200, 50, "female", FALSE)$PK$default
  expect_equal(round(c(d$cl1 * 60, d$v1), 1), c(14.2, 50.2))
})

test_that("the derived terminal half-life is 2.1 h, and the CSF compartment holds about 20%", {
  d <- temozolomide(70, refHeight(70), 50, "female", FALSE)$PK$default
  expect_equal(round(log(2) / (d$cl1 / d$v1) / 60, 1), 2.1)
  p <- TEMOZOLOMIDE_OSTERMANN
  expect_equal(round(p$k23 * d$v1 / (p$k32 * 0.14), 2), 0.21)   # CSF / plasma at equilibrium
  expect_equal(d$cl2 * 60, p$k23 * d$v1)
})

test_that("a 200 mg/m2 dose: peak about 30 min, then the 2.1-h decline", {
  h <- refHeight(70)
  dose <- data.frame(Drug = "temozolomide", Time = 0, Dose = 200 * bsaDuBois(70, h),
                     Units = "mg PO")
  w <- simulateDrugsWithCovariates(dose, noEvents, 70, h, 50, "female", 24 * 60, FALSE,
                                   adjustToFFM = FALSE)$temozolomide$wide
  expect_equal(w$Time[which.max(w$Plasma)], 29, tolerance = 2)
  expect_equal_rounded(max(w$Plasma), 10.0331)                     # mcg/mL
  late <- approx(w$Time, w$Plasma, c(360, 360 + 126))$y        # one half-life apart
  expect_equal(late[2] / late[1], 0.5, tolerance = 0.02)
})

test_that("stays positive and finite at any size, both switch positions pinned", {
  for (w in c(3, 300)) {
    d <- temozolomide(w, 170, 40, "male")$PK$default
    expect_true(all(is.finite(unlist(d))) && d$cl1 > 0 && d$v1 > 0)
  }
  on <- temozolomide(100, 170, 40, "male")$PK$default
  off <- temozolomide(100, 170, 40, "male", FALSE)$PK$default
  s <- pkSizeFactors(100, 170, 40, "male", TRUE)
  expect_equal(on$v1, 30.3 + 19.9 * (bsaDuBois(s$pkWeight, 170) / TEMOZOLOMIDE_OSTERMANN$bsaMean - 1))
  expect_equal(off$v1, 30.3 + 19.9 * (bsaDuBois(100, 170) / TEMOZOLOMIDE_OSTERMANN$bsaMean - 1))
})

test_that("temozolomide is an oral chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "temozolomide", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Default.Units, "mg PO")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("temozolomide", 70, 170, 50, "male")$reference, "Ostermann")
  expect_identical(therapeuticRange("temozolomide")$band_status, "not_applicable")
})
