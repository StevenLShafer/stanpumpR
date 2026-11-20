# Irbesartan (R/drugs_irbesartan.R), from Karatza and Karalis 2020.
# Drafted by Claude Code, 2026-11-20, at the request of Steven L. Shafer.
#
# PROVISIONAL.  The source's absorption is a DELAY differential equation,
# which this package discretises into pulses of its default oral form, and the
# stand-in misses the published curve by up to 4.7% of the peak.  The test
# that earns its keep checks the plotted curve against an independent
# integration of the exact delay system, written out here without reading the
# pulse fractions, so that the cost stays measured rather than asserted.

events <- data.frame(Time = numeric(0), Event = character(0))

refPK <- function(weight = 70, height = 175, age = 30, sex = "male",
                  ffm = FALSE) {
  getDrugPK("irbesartan", weight, height, age, sex, adjustToFFM = ffm)
}

simIrb <- function(dose = 150, units = "mg PO", minutes = 24 * 60,
                   weight = 70, height = 175, age = 30, sex = "male",
                   ffm = FALSE) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "irbesartan", Time = 0, Dose = dose, Units = units),
    events, weight, height, age, sex, minutes, FALSE,
    adjustToFFM = ffm)$irbesartan$wide
}

# The exact published system, per Karatza 2020 Table 2, in hours and litres:
#
#     dA0/dt = -ka A0(t - T)                    (the depot, delayed)
#     dA1/dt =  ka A0(t - T) + k21 A2 - (k10 + k12) A1
#     dA2/dt =  k12 A1 - k21 A2
#
# run with the dose held in the depot before time zero (Monolix's history),
# which is what reproduces the paper's own Table 3.  Midpoint steps of
# 1/1000 h, so that the delay is a whole number of steps.  The disposition is
# passed in, so the same integration checks a scaled patient.
exactIrbCp <- function(times, dose, v1, v2, cl1, cl2, h = 1 / 1000) {
  ka <- IRBESARTAN_DDE_KA
  lag <- round(IRBESARTAN_DDE_T / h)
  k10 <- cl1 / v1; k12 <- cl2 / v1; k21 <- cl2 / v2
  n <- ceiling(max(times) / h) + 1
  A0 <- numeric(n + 1); A0[1] <- dose
  delayed <- function(i) if (i <= lag) dose else A0[i - lag]
  A1 <- 0; A2 <- 0; t <- 0
  out <- numeric(length(times)); j <- 1
  for (i in seq_len(n)) {
    r0 <- ka * delayed(i)
    r1 <- ka * delayed(i + 1)
    rm <- (r0 + r1) / 2
    A0[i + 1] <- A0[i] - h * rm
    A1m <- A1 + h / 2 * (r0 + k21 * A2 - (k10 + k12) * A1)
    A2m <- A2 + h / 2 * (k12 * A1 - k21 * A2)
    A1  <- A1 + h * (rm + k21 * A2m - (k10 + k12) * A1m)
    A2  <- A2 + h * (k12 * A1m - k21 * A2m)
    t <- t + h
    while (j <= length(times) && t >= times[j] - h / 2) {
      out[j] <- A1 / v1 * 1000; j <- j + 1               # ng/mL
    }
  }
  out
}

test_that("the published parameters are carried unchanged", {
  set <- refPK()$PK[[PK_EVENT_DEFAULT]]
  expect_equal_rounded(13.8, set$v1)
  expect_equal_rounded(85.8, set$v2)
  expect_equal_rounded(13.5 / 60, set$cl1)
  expect_equal_rounded(17.7 / 60, set$cl2)
  expect_equal(set$v3, 1)
  expect_equal(set$cl3, 0)
  # Apparent model: F is not applied a second time, and the delay lives in
  # the pulses rather than in tlag_PO
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$tlag_PO, 0)
  expect_equal_rounded(IRBESARTAN_KA / 60, set$ka_PO, tolerance = 1e-6)
  # The delay equation the pulses stand for is recorded
  expect_equal(IRBESARTAN_DDE_KA, 0.304)
  expect_equal(IRBESARTAN_DDE_T, 1.68)
})

test_that("plasma only, and marked provisional", {
  PK <- refPK()
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
  expect_equal(PK$upperTypical, 0)
  expect_equal(PK$lowerTypical, 0)
  expect_match(PK$reference, "PROVISIONAL", fixed = TRUE)
  expect_match(PK$reference, "4.7% of the peak", fixed = TRUE)
})

test_that("the dose is given as twelve pulses of the default oral form", {
  pulses <- refPK()$oralPulses
  expect_named(pulses, ORAL_PULSE_DEFAULT_FORM)
  p <- pulses[[ORAL_PULSE_DEFAULT_FORM]]
  expect_equal(sum(p$fraction), 1, tolerance = 1e-6)
  expect_true(all(p$fraction > 0))
  expect_equal(p$delay, sort(p$delay))
  expect_gt(p$delay[1], 0)
  expect_lt(max(p$delay), 240)                 # all inside four hours
  expect_equal(length(p$fraction), 12)
  expect_equal(length(p$delay), 12)
})

test_that("with the switch off, body size changes nothing", {
  # No size covariate was retained and the cohort's demographics are not
  # printed, so the reference anchor stands
  light <- refPK(weight = 50)$PK[[PK_EVENT_DEFAULT]]
  heavy <- refPK(weight = 110)$PK[[PK_EVENT_DEFAULT]]
  for (x in c("v1", "v2", "cl1", "cl2"))
    expect_equal_rounded(light[[x]], heavy[[x]])
  for (ffm in c(FALSE, TRUE)) {
    set <- refPK(ffm = ffm)$PK[[PK_EVENT_DEFAULT]]
    expect_true(all(vapply(set[c("v1", "v2", "cl1", "cl2", "ka_PO")],
                           function(x) is.finite(x) && x > 0, logical(1))),
                info = as.character(ffm))
  }
})

test_that("the pulses reproduce the exact delay system to the stated error", {
  # THE TEST THIS DRUG EXISTS FOR.  The bounds are the stated cost, not a
  # loose fence: the header and the help page say 4.7% of the peak at the
  # worst point and a peak 2.3% low, so the curve is held to 5% and the peak
  # to 2.5%.  Nine times the library's 0.53% standard, which is why the
  # model is provisional.
  w <- simIrb()
  w <- w[w$Time > 0, ]
  ex <- exactIrbCp(w$Time / 60, 150, IRBESARTAN_VC_F, IRBESARTAN_VP_F,
                   IRBESARTAN_CL_F, IRBESARTAN_Q_F)
  peak <- max(ex)
  # The integration itself against the paper's Table 3B, 1614 ng/mL at
  # 150 mg (the history convention lands 1.8% low; a zero history lands far
  # lower, which is how the convention was chosen)
  expect_lt(abs(peak - 1614) / 1614, 0.025)
  expect_lt(max(abs(w$Plasma - ex)) / peak, 0.050)
  expect_lt(abs(max(w$Plasma) - peak) / peak, 0.025)
  # and it errs low at the peak rather than high
  expect_lt(max(w$Plasma), peak)
  auc   <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  aucEx <- sum(diff(w$Time) * (head(ex, -1) + tail(ex, -1)) / 2)
  expect_lt(abs(auc - aucEx) / aucEx, 0.01)
})

test_that("the error holds for a small woman and a large man with the switch on", {
  # The pulses were fitted at the published disposition; fat-free-mass
  # scaling moves it, and the error moves with it.  Checked with the same
  # independent integration, at the package's own scaled parameters.
  for (pt in list(list(weight = 45, height = 155, age = 30, sex = SEX_FEMALE),
                  list(weight = 120, height = 185, age = 50, sex = "male"))) {
    set <- refPK(pt$weight, pt$height, pt$age, pt$sex, ffm = TRUE)$PK[[PK_EVENT_DEFAULT]]
    w <- simIrb(weight = pt$weight, height = pt$height, age = pt$age,
                sex = pt$sex, ffm = TRUE)
    w <- w[w$Time > 0, ]
    ex <- exactIrbCp(w$Time / 60, 150, set$v1, set$v2, set$cl1 * 60,
                     set$cl2 * 60)
    peak <- max(ex)
    expect_lt(max(abs(w$Plasma - ex)) / peak, 0.055,
              label = paste("curve", pt$weight))
  }
})

test_that("the curve agrees with the label, which the model was not fitted to", {
  w <- simIrb()
  tmax <- w$Time[which.max(w$Plasma)] / 60
  # Labelled 1.5 to 2 h; the exact source peaks at 1.9 h and the stand-in
  # about 0.1 h later
  expect_gt(tmax, 1.5)
  expect_lt(tmax, 2.1)
  # Total exposure is dose over apparent clearance: the fractions sum to one
  long <- simIrb(minutes = 7 * 1440)
  auc <- sum(diff(long$Time) *
               (head(long$Plasma, -1) + tail(long$Plasma, -1)) / 2) / 60
  expect_lt(abs(auc - 150 * 1000 / IRBESARTAN_CL_F) /
              (150 * 1000 / IRBESARTAN_CL_F), 0.02)
})

test_that("once daily, the peak accumulates about 6% over a week", {
  # The figure the header and the help page quote.  The label says under
  # 20% at steady state; a single 13 h half-life would predict about 39%.
  w <- simIrb(units = "mg PO qd", minutes = 7 * 1440)
  day1 <- max(w$Plasma[w$Time <= 1440])
  day7 <- max(w$Plasma[w$Time >= 6 * 1440])
  expect_lt(abs(day7 / day1 - 1.06), 0.02)
  day6 <- max(w$Plasma[w$Time >= 5 * 1440 & w$Time < 6 * 1440])
  expect_lt(abs(day7 - day6) / day6, 0.01)       # steady state, not a ramp
})

test_that("pregnancy changes nothing", {
  on  <- getDrugPK("irbesartan", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("irbesartan", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(on$PK, off$PK)
  expect_identical(on$oralPulses, off$oralPulses)
})

test_that("the registry quotes the fractions the drug uses", {
  pars <- utils::read.csv(system.file("extdata", "antihypertensiveParameters.csv",
                                      package = "stanpumpR"),
                          stringsAsFactors = FALSE)
  id <- "irbesartan__tablet__oral__healthy_asian_men__concentration__karatza2020"
  note <- pars$notes[pars$model_id == id &
                       pars$parameter == "discretised release fractions"]
  expect_length(note, 1)
  for (f in sprintf("%.6f", IRBESARTAN_PULSE_FRACTION))
    expect_match(note, f, fixed = TRUE)
  for (d in sprintf("%.4f", IRBESARTAN_PULSE_DELAY))
    expect_match(note, d, fixed = TRUE)
  err <- pars$value[pars$model_id == id &
                      pars$parameter == "discretisation error, worst point on the curve"]
  expect_equal(round(err, 1), IRBESARTAN_PULSE_ERROR_PERCENT)
})
