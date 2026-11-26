# Temozolomide in children (R/drugs_temozolomidePanetta.R): Panetta 2003, oral,
# one compartment, BSA on clearance and volume; and the temozolomide group
# (R/modelSelection.R).  Drafted by Claude Code, 2026-11-16.

noEvents <- data.frame(Time = double(), Event = character())
# a patient whose Mosteller BSA is a given value
heightFor <- function(w, bsa) uniroot(function(h) bsaMosteller(w, h) - bsa, c(30, 300))$root

test_that("Table 3's equations at the median BSA, 1.1 m2, give Table 2's ke 0.39 /h", {
  h <- heightFor(30, 1.1)
  d <- temozolomidePanetta(30, h, 7, "male", FALSE)$PK$default
  expect_equal_rounded(c(d$cl1 * 60, d$v1), c(4.99282, 12.667))      # L/h, L
  expect_equal(round(d$cl1 / d$v1 * 60, 2), 0.39)
  expect_equal(d$ka_PO * 60, 2.4)
  expect_equal(c(d$bioavailability_PO, d$tlag_PO), c(1, 0))
  # sex does not enter
  expect_equal(temozolomidePanetta(30, h, 7, "female", FALSE)$PK$default$cl1, d$cl1)
})

test_that("at an adult's 1.8 m2 the equations meet Ostermann's adult model", {
  m <- temozolomidePanettaClV(1.8)
  expect_equal_rounded(c(m$cl, m$v), c(10.6334, 27.1669))
  expect_true(m$cl > 10.0 && m$cl < 11.9)            # Ostermann's woman and man
})

test_that("outside the studied 0.45-2.1 m2, clearance and volume are proportional to BSA", {
  lo <- temozolomidePanettaClV(0.45)
  hi <- temozolomidePanettaClV(2.1)
  expect_equal(temozolomidePanettaClV(0.2)$cl, lo$cl * 0.2 / 0.45)
  expect_equal(temozolomidePanettaClV(0.2)$v, lo$v * 0.2 / 0.45)
  expect_equal(temozolomidePanettaClV(3)$cl, hi$cl * 3 / 2.1)
  # continuous at both limits
  expect_equal(temozolomidePanettaClV(0.45 - 1e-9)$cl, lo$cl, tolerance = 1e-8)
  expect_equal(temozolomidePanettaClV(2.1 + 1e-9)$cl, hi$cl, tolerance = 1e-8)
  for (w in c(0.5, 3, 300)) {
    d <- temozolomidePanetta(w, 170, 5, "male")$PK$default
    expect_true(all(is.finite(unlist(d))) && d$cl1 > 0 && d$v1 > 0)
  }
})

test_that("both switch positions pinned, and they agree", {
  on <- temozolomidePanetta(40, 140, 10, "female")$PK$default
  off <- temozolomidePanetta(40, 140, 10, "female", FALSE)$PK$default
  # BSA comes from TOTAL weight either way (Shafer, 2026-11-25, Group B item 11), so the
  # fat-free-mass switch cannot reach this drug.  Until that day the switch-on case used
  # pkSizeFactors()$pkWeight, which applied the size adjustment twice: Panetta fitted his
  # BSA covariate against the children's actual weight.
  expect_equal(on$v1, temozolomidePanettaClV(bsaMosteller(40, 140))$v)
  expect_equal(off$v1, temozolomidePanettaClV(bsaMosteller(40, 140))$v)
  expect_equal(on$v1, off$v1)
  expect_equal(on$cl1, off$cl1)
  # and the switch is inert at a size where FFM differs most from total weight
  heavy_on <- temozolomidePanetta(95, 150, 16, "female")$PK$default
  heavy_off <- temozolomidePanetta(95, 150, 16, "female", FALSE)$PK$default
  expect_equal(heavy_on$v1, heavy_off$v1)
  expect_equal(heavy_on$cl1, heavy_off$cl1)
})

test_that("a 200 mg/m2 dose in a child of 1.1 m2", {
  h <- heightFor(30, 1.1)
  dose <- data.frame(Drug = "temozolomidePanetta", Time = 0, Dose = 220, Units = "mg PO")
  w <- simulateDrugsWithCovariates(dose, noEvents, 30, h, 7, "male", 12 * 60, FALSE,
                                   adjustToFFM = FALSE)$temozolomidePanetta$wide
  # the one-compartment oral peak, ln(ka/ke)/(ka-ke) h, and its height
  ka <- 2.4; ke <- 4.99282 / 12.667
  tmax <- log(ka / ke) / (ka - ke)
  cmax <- 220 * ka / (12.667 * (ka - ke)) * (exp(-ke * tmax) - exp(-ka * tmax))
  expect_equal(max(w$Plasma), cmax, tolerance = 0.005)
  expect_equal(approx(w$Time, w$Plasma, tmax * 60)$y, cmax, tolerance = 0.005)
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  expect_equal(auc, 220 / 4.99282, tolerance = 0.02)                # Dose / CL, mg h/L
})

test_that("the group chooses Panetta for a child and Ostermann for an adult", {
  pick <- function(age, weight = 70) temozolomide(weight, 170, age, "male")$modelChosen
  expect_identical(pick(7, 25), "temozolomidePanetta")
  expect_identical(pick(50), "temozolomideOstermann")
  expect_match(getDrugPK("temozolomide", 25, 120, 7, "male")$reference, "Panetta")
  expect_match(getDrugPK("temozolomide", 70, 170, 50, "male")$reference, "Ostermann")
  # the age ranges do not overlap: the nearer one wins between them
  expect_identical(pick(25.9), "temozolomidePanetta")
  expect_identical(pick(26.1), "temozolomideOstermann")
})

test_that("the candidates are not offered; the group is", {
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Category[dd$Drug == "temozolomide"], CHEMOTHERAPY_DRUG_CATEGORY)
  expect_true(is.na(dd$Category[dd$Drug == "temozolomidePanetta"]) ||
              dd$Category[dd$Drug == "temozolomidePanetta"] == "")
  expect_identical(therapeuticRange("temozolomidePanetta")$band_status, "not_applicable")
})
