# Bleomycin (R/drugs_bleomycin.R): Yee 1983, two compartments per m2, doses in
# USP units.  Drafted by Claude Code, 2026-11-16.

noEvents <- data.frame(Time = double(), Event = character())
heightFor <- function(w, bsa) uniroot(function(h) bsaMosteller(w, h) - bsa, c(30, 300))$root

test_that("the closure returns Table II's CL, Vc, Vss and terminal half-life", {
  m <- bleomycinYee()
  expect_equal(m$cl, 51.8 * 60 / 1000)
  expect_equal(m$v1, 4.3)
  expect_equal(m$v1 + m$v2, 9.9)
  k10 <- m$cl / m$v1; k12 <- m$q / m$v1; k21 <- m$q / m$v2
  s <- k10 + k12 + k21; p <- k10 * k21
  beta <- (s - sqrt(s^2 - 4 * p)) / 2
  expect_equal(log(2) / beta, 3.2)
  expect_equal_rounded(c(m$v2, m$q), c(5.6, 2.740015))
})

test_that("the fast half-life, the check, is 0.42 h against the printed 0.3 +/- 0.1", {
  m <- bleomycinYee()
  expect_equal_rounded(m$tAlpha, 0.424545)
  expect_lt(abs(m$tAlpha - 0.3) / 0.1, 1.3)               # within 1.3 SEM
})

test_that("against adults the model was not fitted to (Alberts 1978)", {
  m <- bleomycinYee()
  expect_equal(m$cl * 1000 / 60, 50.6, tolerance = 0.03)  # mL/min/m2
  expect_equal(m$tAlpha * 60, 24.4, tolerance = 0.06)     # min
})

test_that("per m2 of BSA, both switch positions pinned", {
  h <- heightFor(70, 1.8)
  off <- bleomycin(70, h, 40, "male", FALSE)$PK$default
  expect_equal(c(off$v1, off$cl1 * 60), c(4.3, 3.108) * 1.8)
  on <- bleomycin(100, 175, 40, "female")$PK$default
  s <- pkSizeFactors(100, 175, 40, "female", TRUE)
  expect_equal(on$v1, 4.3 * bsaMosteller(s$pkWeight, 175))
  for (w in c(0.5, 300)) {
    d <- bleomycin(w, 170, 40, "male")$PK$default
    expect_true(all(is.finite(unlist(d))) && d$cl1 > 0 && d$v1 > 0)
  }
})

test_that("a 30 U/m2 bolus: concentrations in mcU/mL, area Dose/CL", {
  h <- heightFor(70, 1.8)
  dose <- data.frame(Drug = "bleomycin", Time = 0, Dose = 54, Units = "U")
  w <- simulateDrugsWithCovariates(dose, noEvents, 70, h, 40, "male", 36 * 60, FALSE,
                                   adjustToFFM = FALSE)$bleomycin$wide
  # at time 0 the dose is in V1: 54 U = 54000 mU over 4.3 x 1.8 L, mU/L = mcU/mL
  expect_equal(max(w$Plasma), 54000 / (4.3 * 1.8), tolerance = 0.01)
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)   # mcU min/mL
  expect_equal(auc, 54000 / (51.8 * 1.8 / 1000), tolerance = 0.02)
})

test_that("bleomycin is a chemotherapy drug dosed in units, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "bleomycin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "mcU")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("bleomycin", 70, 170, 40, "male")$reference, "Yee")
  expect_identical(therapeuticRange("bleomycin")$band_status, "not_applicable")
})
