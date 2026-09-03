# Tests for advanceClosedFormMetabolite().
#
# Two things must hold.  First, adding a metabolite must not change the parent:
# Cp and Ce have to come out bit-identical to advanceClosedForm0().  Second, the
# metabolite columns have to agree with the closed form derived in
# metaboliteCoefficients.R, and with an independent integration.

pkWithMetabolite <- function(parentDrug = "hydromorphone",
                             metaboliteDrug = "morphine",
                             fraction = 0.08,
                             mwRatio = 1) {
  parent <- getDrugPK(parentDrug, 70, 170, 50, "male",
                      getDrugDefaults(parentDrug))$PK$default
  met    <- getDrugPK(metaboliteDrug, 70, 170, 50, "male",
                      getDrugDefaults(metaboliteDrug))$PK$default
  parent$metabolite <- list(
    name  = metaboliteDrug,
    coefs = metaboliteCoefficients(parent, met, fraction, mwRatio),
    ke0   = met$ke0
  )
  parent
}

doseTable <- function() data.frame(
  Time  = c(0, 0, 30),
  Dose  = c(2, 0.02, 0),
  Bolus = c(TRUE, FALSE, FALSE)
)


test_that("the parent columns are unchanged by the presence of a metabolite", {
  pk <- pkWithMetabolite()
  DT <- doseTable()

  withMet <- advanceClosedFormMetabolite(DT, pk, 120, FALSE, pk$endCe)
  plain   <- advanceClosedForm0(DT, pk, 120, FALSE, pk$endCe)

  expect_equal(withMet$Time, plain$Time)
  expect_equal(withMet$Cp, plain$Cp, tolerance = 1e-12)
  expect_equal(withMet$Ce, plain$Ce, tolerance = 1e-12)
})


test_that("the metabolite starts at zero, rises, then falls", {
  pk <- pkWithMetabolite()
  r  <- advanceClosedFormMetabolite(doseTable(), pk, 480, FALSE, pk$endCe)

  expect_equal(r$CpMetabolite[1], 0, tolerance = 1e-12)
  expect_true(all(r$CpMetabolite >= 0))
  expect_gt(max(r$CpMetabolite), 0)

  # It peaks after the parent does, because it has to be formed first
  tPeakParent <- r$Time[which.max(r$Cp)]
  tPeakMet    <- r$Time[which.max(r$CpMetabolite)]
  expect_gt(tPeakMet, tPeakParent)

  # And it is falling by the end of a long simulation
  n <- nrow(r)
  expect_lt(r$CpMetabolite[n], max(r$CpMetabolite))
})


test_that("the simulated metabolite matches the closed form after a bolus", {
  pk <- pkWithMetabolite()
  DT <- data.frame(Time = 0, Dose = 5, Bolus = TRUE)

  r <- advanceClosedFormMetabolite(DT, pk, 240, FALSE, pk$endCe)

  # Direct evaluation of the closed form at the same times
  expected <- metaboliteAfterBolus(pk$metabolite$coefs, 5, r$Time)
  expect_equal(r$CpMetabolite, pmax(expected, 0), tolerance = 1e-9)
})


test_that("the metabolite effect site lags the metabolite concentration", {
  pk <- pkWithMetabolite()
  r  <- advanceClosedFormMetabolite(doseTable(), pk, 480, FALSE, pk$endCe)

  expect_equal(r$CeMetabolite[1], 0, tolerance = 1e-12)
  tPeakCm  <- r$Time[which.max(r$CpMetabolite)]
  tPeakCem <- r$Time[which.max(r$CeMetabolite)]
  expect_gte(tPeakCem, tPeakCm)
  expect_lt(max(r$CeMetabolite), max(r$CpMetabolite))
})


test_that("metabolite exposure scales with the pathway fraction", {
  DT <- data.frame(Time = 0, Dose = 5, Bolus = TRUE)

  low  <- advanceClosedFormMetabolite(DT, pkWithMetabolite(fraction = 0.05),
                                      240, FALSE, 0)
  high <- advanceClosedFormMetabolite(DT, pkWithMetabolite(fraction = 0.10),
                                      240, FALSE, 0)

  expect_equal(high$CpMetabolite, 2 * low$CpMetabolite, tolerance = 1e-9)
  # ...while the parent is untouched by it
  expect_equal(high$Cp, low$Cp, tolerance = 1e-12)
})


test_that("an infusion of parent produces a metabolite plateau", {
  pk <- pkWithMetabolite()
  DT <- data.frame(Time = 0, Dose = 0.05, Bolus = FALSE)

  r <- advanceClosedFormMetabolite(DT, pk, 1440, FALSE, pk$endCe)
  at <- function(t) stats::approx(r$Time, r$CpMetabolite, t)$y

  # Rising monotonically toward steady state
  expect_true(all(diff(r$CpMetabolite) >= -1e-12))

  # And flattening.  The comparison has to be per MINUTE: the timeline is
  # geometric, so comparing by row index compares wildly different durations.
  rateEarly <- (at(60)   - at(0))    / 60
  rateLate  <- (at(1440) - at(1080)) / 360
  expect_gt(rateEarly, 0)
  expect_lt(rateLate, rateEarly / 10)
})


test_that("recovery is computed for the parent and unaffected by the metabolite", {
  pk <- pkWithMetabolite()
  DT <- doseTable()

  withMet <- advanceClosedFormMetabolite(DT, pk, 240, TRUE, 0.5)
  plain   <- advanceClosedForm0(DT, pk, 240, TRUE, 0.5)

  expect_true(all(is.finite(withMet$Recovery)))
  expect_equal(withMet$Recovery, plain$Recovery, tolerance = 1e-12)

  off <- advanceClosedFormMetabolite(DT, pk, 240, FALSE, 0.5)
  expect_true(all(off$Recovery == 0))
})


test_that("a pkSet without a metabolite is refused", {
  pk <- getDrugPK("morphine", 70, 170, 50, "male",
                  getDrugDefaults("morphine"))$PK$default
  expect_error(
    advanceClosedFormMetabolite(doseTable(), pk, 60, FALSE, 0),
    "metabolite"
  )
})


test_that("a pure prodrug has no effect site of its own", {
  # Codeine and tramadol are prodrugs only, so they carry no tPeak and
  # getDrugPK leaves ke0 at zero.  calculateCe() divides by ke0 and would
  # otherwise return NaN at every point.
  pk <- pkWithMetabolite()
  pk$ke0 <- 0

  r <- advanceClosedFormMetabolite(doseTable(), pk, 240, FALSE, 0)

  expect_true(all(is.na(r$Ce)))
  expect_false(any(is.nan(r$Ce)))   # NA, so the plot drops it; not NaN
  expect_true(all(is.finite(r$Cp)))
  expect_gt(max(r$Cp), 0)

  # The metabolite is untouched: its effect is the one that matters
  expect_gt(max(r$CpMetabolite), 0)
  expect_gt(max(r$CeMetabolite), 0)
  expect_true(all(is.finite(r$CpMetabolite)))
  expect_true(all(is.finite(r$CeMetabolite)))
})


test_that("advanceClosedForm0 also survives a drug with no tPeak", {
  # The same latent divide-by-ke0.  No drug in the library reaches it today,
  # which is why it had never surfaced.
  pk <- getDrugPK("morphine", 70, 170, 50, "male",
                  getDrugDefaults("morphine"))$PK$default
  pk$ke0 <- 0

  r <- advanceClosedForm0(data.frame(Time = 0, Dose = 5, Bolus = TRUE),
                          pk, 120, FALSE, 0)

  expect_true(all(is.finite(r$Cp)))
  expect_gt(max(r$Cp), 0)
  expect_true(all(is.na(r$Ce)))
  expect_false(any(is.nan(r$Ce)))
})
