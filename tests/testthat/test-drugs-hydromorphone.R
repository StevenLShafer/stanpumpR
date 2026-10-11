test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- hydromorphone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 11.2,
        v2 = 112,
        v3 = 943.1579,
        cl1 = 1.2992,
        cl2 = 3.36,
        cl3 = 0.896,
        ka_PO = 0.01,
        bioavailability_PO = 0.225,
        tlag_PO = 0,
        # Corrected 2026-10-06.  These three used to copy the oral values and
        # add lags of 90 and 180 min, which put the intranasal plasma peak at
        # 226 min against a measured 15 to 25.  See R/drugs_hydromorphone.R.
        ka_IM = 0.0128241366,
        bioavailability_IM = 1.0,
        tlag_IM = 0,
        ka_IN = 0.0149015907,
        bioavailability_IN = 0.55,
        tlag_IN = 0
      )
    ),
    tPeak = 19.6,
    MEAC = 0.0015,
    typical = 0.0018,
    upperTypical = 0.0012,
    lowerTypical = 0.003,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})


# The extravascular routes, corrected 2026-10-06.  Before that they reused the
# oral absorption constant and bioavailability and added lags of 90 and 180
# min, so intranasal hydromorphone peaked at 226 min in a model of a route
# chosen because it works quickly.

test_that("the intranasal route reproduces the measured peak and exposure", {
  # Coda 2003: 24 healthy volunteers, absolute bioavailability 52.4% and
  # 57.5%, median time to peak 20 and 25 min.
  x <- hydromorphone(70, 171, 50, "male")$PK$default
  expect_equal(x$tlag_IN, 0)
  expect_gte(x$bioavailability_IN, 0.52)
  expect_lte(x$bioavailability_IN, 0.58)

  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "hydromorphone", Time = 0, Dose = 2, Units = "mg IN"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", 720, FALSE)
  w <- o$hydromorphone$wide
  expect_equal(w$Time[which.max(w$Plasma)], 20, tolerance = 1.5)
  # Davis 2004 measured 3.02 to 3.56 ng/mL after 2 mg
  expect_gt(max(w$Plasma), 2.7)
  expect_lt(max(w$Plasma), 3.7)
})


test_that("the intramuscular route is complete and faster than oral", {
  x <- hydromorphone(70, 171, 50, "male")$PK$default
  expect_equal(x$tlag_IM, 0)
  # No first pass, and the labelling gives the same dose by either route
  expect_equal(x$bioavailability_IM, 1.0)
  # Faster than oral, slower than nasal
  expect_gt(x$ka_IM, x$ka_PO)
  expect_lt(x$ka_IM, x$ka_IN)
})


test_that("time until threshold counts down from the first minutes on every route", {
  # The lags used to leave the engine with no effect-site state at all, so
  # recovery read exactly zero for the first 90 min after an intramuscular
  # dose and three hours after an intranasal one, which reads as "already
  # recovered" when the truth is "not yet absorbed".
  ev <- data.frame(Time = numeric(0), Event = character(0))
  # 2 mg by each parenteral route; 8 mg by mouth, because at the oral
  # bioavailability of 0.225 (Lohela 2021, since 2026-10-10) 2 or 4 mg oral
  # never brings the effect site up to the 1.5 ng/mL threshold, so there is
  # no time to count down.
  doses <- c("mg" = 2, "mg IM" = 2, "mg IN" = 2, "mg PO" = 8)
  for (units in names(doses)) {
    o <- simulateDrugsWithCovariates(
      data.frame(Drug = "hydromorphone", Time = 0, Dose = doses[[units]], Units = units),
      ev, 70, 171, 50, "male", 720, TRUE)
    es <- o$hydromorphone$equiSpace
    at10 <- stats::approx(es$Time, es$Recovery, 10)$y
    expect_gt(at10, 0, label = paste("recovery at 10 min for", units))
    # and it counts down rather than sitting still
    at60 <- stats::approx(es$Time, es$Recovery, 60)$y
    expect_lt(at60, at10, label = paste("recovery falls by 60 min for", units))
  }
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- hydromorphone(120, 170, 50, "male")
  expected <- list(
        v1 = 14.614954,
        v2 = 146.14954,
        v3 = 1230.733,
        cl1 = 1.5862097,
        cl2 = 4.1022664,
        cl3 = 1.0939377
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("oral liquid is the tablet's absorption, entered as prescribed", {
  # The DILAUDID label: the 8 mg tablet and an equal dose of the oral
  # solution are bioequivalent, so the liquid is not a separate formulation.
  PK <- getDrugPK("hydromorphone", 20, 115, 6, "female")
  expect_null(PK$oralFormulations)
  events <- data.frame(Time = numeric(0), Event = character(0))
  run <- function(dose, units)
    simCpCe(data.frame(Drug = "hydromorphone", Time = 0, Dose = dose,
                       Units = units),
            events, PK, 12 * 60, plotRecovery = FALSE)$results
  expect_equal(run(2, "mg PO liquid"), run(2, "mg PO"))
  # 0.1 mg/kg in a 20 kg child is 2 mg.
  expect_equal(run(0.1, "mg/kg PO liquid"), run(2, "mg PO"))
  units <- getDrugDefaults("hydromorphone")$Units[[1]]
  expect_true(all(c("mg PO liquid", "mg/kg PO liquid", "mg PO liquid qid",
                    "mg/kg PO liquid qid") %in% units))
})

test_that("oral hydromorphone reproduces Lohela 2021's apparent clearance and curve", {
  # Lohela TJ et al., Anesth Analg 2021;133:423-434: 2.6 mg immediate-release
  # hydromorphone HCl, 12 fasted volunteers, placebo phase.  Apparent oral
  # clearance 5.78 L/min (Table 1); mean curve from Figure 1A, read from the
  # plot, so compared loosely.
  PK <- getDrugPK("hydromorphone", 70, 170, 40, "male")
  set <- PK$PK$default
  expect_equal(set$cl1 / set$bioavailability_PO, 5.78, tolerance = 0.005)
  sim <- simCpCe(
    data.frame(Drug = "hydromorphone", Time = 0, Dose = 2.6, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)), PK, 12 * 60, FALSE)
  r <- sim$results[sim$results$Site == "Plasma", ]
  hours <- c(0.5, 1, 1.5, 2, 3, 4, 6, 8)
  lohela <- c(1.18, 1.30, 1.13, 0.90, 0.61, 0.45, 0.25, 0.17)   # ng/mL
  model <- stats::approx(r$Time, r$Y, xout = hours * 60)$y
  expect_true(all(abs(model / lohela - 1) < 0.2))
  # Peak at 46 min on a fine grid; the engine's plotted grid puts it within
  # a few minutes of that.
  expect_lt(abs(r$Time[which.max(r$Y)] - 46), 6)
})

test_that("hydrocodone's formed hydromorphone does not use hydromorphone's oral F", {
  # Hydromorphone is hydrocodone's metabolite.  Its formation runs through
  # the metabolite link on the PARENT's absorption, so recalibrating
  # hydromorphone's own oral bioavailability (0.6 -> 0.225, 2026-10-10) must
  # leave it unchanged.  Pinned before that change: 10 mg oral hydrocodone,
  # 70 kg reference man, peak formed hydromorphone 0.07444011 (ng/mL scale).
  PK <- getDrugPK("hydrocodone", 70, 170, 40, "male")
  sim <- simCpCe(
    data.frame(Drug = "hydrocodone", Time = 0, Dose = 10, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)), PK, 720, FALSE)
  expect_equal(max(sim$metaboliteSeries$Cp), 0.07444011, tolerance = 1e-6)
})
