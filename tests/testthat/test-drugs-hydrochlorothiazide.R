# Hydrochlorothiazide (R/drugs_hydrochlorothiazide.R), from Van Wart 2013.
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer.
#
# The parameter pins here are routine.  The test that earns its keep is the
# last one: the source's oral input is a WEIBULL function, which this package
# discretises into pulses of its default oral form, so the curve is checked
# against a NUMERICAL INTEGRATION of the exact Weibull system, written out
# here independently of the package, at every renal function the paper
# tabulates.  That is the same standard the zero-order release mode was held
# to in test-oral-zero-order.R: an approximation is only allowed if what it
# costs is measured.

events <- data.frame(Time = numeric(0), Event = character(0))

refPK <- function(creatinine = NULL, weight = 70, age = 50, sex = "male",
                  ffm = FALSE) {
  getDrugPK("hydrochlorothiazide", weight, 175, age, sex,
            adjustToFFM = ffm, creatinine = creatinine)
}

test_that("the published parameters are carried unchanged", {
  set <- refPK()$PK[[PK_EVENT_DEFAULT]]
  expect_equal_rounded(97.3, set$v1)
  expect_equal_rounded(178, set$v2)
  expect_equal_rounded(17.2 / 60, set$cl2)
  expect_equal(set$v3, 1)
  expect_equal(set$cl3, 0)
  # Apparent model: the dose is not multiplied by F a second time, and the
  # delay lives in the pulse structure rather than in tlag_PO
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$tlag_PO, 0)
  # Plasma only
  PK <- refPK()
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
  expect_equal(PK$upperTypical, 0)
  expect_equal(PK$lowerTypical, 0)
})

test_that("the depot rate constant is the Weibull's exact asymptote", {
  # Not a fitted number: 1 - exp(-(ka t)^gamma) tends to 1 as t grows, so the
  # source's own depot rate constant tends to 1 per hour.  getDrugPK() may
  # nudge it to keep it off an eigenvalue (separateEigenvalues()), so this is
  # checked against the constant and on the published value.
  expect_equal(HCTZ_KA, 1.0)
  expect_equal_rounded(1.0 / 60, refPK()$PK[[PK_EVENT_DEFAULT]]$ka_PO,
                       tolerance = 1e-6)
  # The Weibull the discretisation stands for is recorded, and is NOT the
  # rate constant: its shape is well away from 1, which is why it needed
  # discretising at all.
  expect_equal(HCTZ_WEIBULL_SCALE, 1.34)
  expect_equal(HCTZ_WEIBULL_SHAPE, 2.84)
  expect_gt(HCTZ_WEIBULL_SHAPE, 2)
})

test_that("the dose is given as pulses of the default oral form", {
  pulses <- refPK()$oralPulses
  expect_named(pulses, ORAL_PULSE_DEFAULT_FORM)
  p <- pulses[[ORAL_PULSE_DEFAULT_FORM]]
  # The whole dose is given, only its timing is changed
  expect_equal(sum(p$fraction), 1)
  expect_true(all(p$fraction > 0))
  # Delays are in minutes, ordered, and all inside the first couple of hours
  expect_equal(p$delay, sort(p$delay))
  expect_gt(p$delay[1], 0)
  expect_lt(max(p$delay), 180)
  expect_equal(length(p$fraction), length(p$delay))
})

test_that("renal clearance follows the paper's own ladder", {
  # Table 2's footnote: CL_R = 18.3 x (CLcr/120)^1.38 L/h, and the paper
  # quotes 18.3, 10.5, 5.47 and 2.70 at creatinine clearances of 120, 80, 50
  # and 30 mL/min.  The creatinine that produces each clearance is solved
  # from Cockcroft-Gault for this patient, so this tests the whole chain from
  # the Patient Profile field to cl1.
  patient <- list(weight = 70, age = 50, sex = "male")
  scrFor <- function(crcl)
    (140 - patient$age) * patient$weight / (72 * crcl)
  for (i in seq_along(c(120, 80, 50, 30))) {
    crcl  <- c(120, 80, 50, 30)[i]
    paper <- c(18.3, 10.5, 5.47, 2.70)[i]
    set <- refPK(creatinine = scrFor(crcl))$PK[[PK_EVENT_DEFAULT]]
    clTotal <- set$cl1 * 60                       # L/h
    # Non-renal clearance is the floor and carries no covariate
    expect_equal_rounded(paper + 2.44, clTotal, tolerance = 0.01)
  }
  # Even at the highest creatinine the Patient Profile accepts -- 15 mg/dL,
  # a Cockcroft-Gault clearance of 5.8 mL/min for this patient -- the
  # non-renal floor remains, so the half-life lengthens a long way but never
  # without limit.
  anuric <- refPK(creatinine = 15)$PK[[PK_EVENT_DEFAULT]]$cl1 * 60
  expect_gt(anuric, 2.44)
  expect_lt(anuric, 3.0)
  # Which is a near-sixfold fall in TOTAL clearance from an assumed normal
  # creatinine (the renal term alone falls fiftyfold; the floor is what keeps
  # the total from following it)
  normal <- refPK()$PK[[PK_EVENT_DEFAULT]]$cl1 * 60
  expect_gt(normal / anuric, 5.5)
  expect_lt(normal / anuric, 6.5)

  # TWO RANGES THAT ARE EASY TO CONFUSE, pinned because a help page did
  # confuse them (CodeRabbit on PR #532).  The sixfold figure above is this
  # program's own range, from an assumed normal creatinine to the highest
  # value the Patient Profile accepts.  Across the range the PAPER tabulates,
  # 120 down to 30 mL/min, total clearance falls only about FOURfold, because
  # the non-renal floor does not move; it is the RENAL term that falls almost
  # sevenfold there.  Any text quoting one of these numbers has to say which.
  totalAt <- function(crcl)
    refPK(creatinine = (140 - 50) * 70 / (72 * crcl))$PK[[PK_EVENT_DEFAULT]]$cl1 * 60
  renalAt <- function(crcl) HCTZ_CL_R * (crcl / HCTZ_CRCL_REF)^HCTZ_CL_R_EXP
  expect_lt(abs(totalAt(120) / totalAt(30) - 4.03), 0.05)
  expect_lt(abs(renalAt(120) / renalAt(30) - 6.77), 0.05)
})

test_that("a blank creatinine is an assumed normal value, not impairment", {
  # getDrugPK() passes NULL when the field is empty, which the renal helpers
  # read as the assumed normal creatinine for age and sex.  A patient entered
  # with that same creatinine must land on the same curve.
  blank <- refPK(creatinine = NULL)$PK[[PK_EVENT_DEFAULT]]$cl1
  typed <- refPK(creatinine = SCR_ASSUMED_MALE)$PK[[PK_EVENT_DEFAULT]]$cl1
  expect_equal_rounded(blank, typed)
  # and it is NOT an impaired patient's clearance
  impaired <- refPK(creatinine = 3.0)$PK[[PK_EVENT_DEFAULT]]$cl1
  expect_gt(blank, 2 * impaired)
})

test_that("the micro constants reproduce the paper's reported half-lives", {
  # 1.6 and 14.8 h for the typical individual at a creatinine clearance of
  # 120 mL/min.  This is the internal consistency check the transcription had
  # to pass: four Table 2 numbers imply two half-lives the paper prints
  # elsewhere, and nothing in the transcription used them.
  set <- refPK(creatinine = (140 - 50) * 70 / (72 * 120))$PK[[PK_EVENT_DEFAULT]]
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  s <- k10 + k12 + k21
  l1 <- (s + sqrt(s^2 - 4 * k10 * k21)) / 2
  l2 <- (s - sqrt(s^2 - 4 * k10 * k21)) / 2
  expect_lt(abs(log(2) / l1 / 60 - 1.6) / 1.6, 0.03)
  expect_lt(abs(log(2) / l2 / 60 - 14.8) / 14.8, 0.03)
})

test_that("both switch positions give a usable parameter set", {
  for (ffm in c(FALSE, TRUE)) {
    set <- refPK(ffm = ffm)$PK[[PK_EVENT_DEFAULT]]
    expect_true(all(vapply(set[c("v1", "v2", "cl1", "cl2", "ka_PO")],
                           function(x) is.finite(x) && x > 0, logical(1))),
                info = as.character(ffm))
  }
  # With the switch off the published values stand exactly: the source fitted
  # no body-size covariate, so a heavier patient's volumes are unchanged and
  # only the Cockcroft-Gault clearance moves.
  light <- refPK(weight = 60)$PK[[PK_EVENT_DEFAULT]]
  heavy <- refPK(weight = 110)$PK[[PK_EVENT_DEFAULT]]
  expect_equal_rounded(light$v1, heavy$v1)
  expect_equal_rounded(light$v2, heavy$v2)
  expect_gt(heavy$cl1, light$cl1)              # through creatinine clearance
})

test_that("the pulses reproduce the exact Weibull system at every renal function", {
  # THE TEST THIS DRUG EXISTS FOR.  An independent midpoint integration of
  #
  #     dA1/dt = -(1 - exp(-(ka t)^gamma)) A1
  #     dA2/dt =  (1 - exp(-(ka t)^gamma)) A1 + k32 A3 - (k20 + k23) A2
  #     dA3/dt =  k23 A2 - k32 A3
  #
  # with Table 2's own numbers, against what the package actually plots.
  # Nothing here reads the drug's pulse fractions: if a later change to them
  # or to expandOralPulses() degrades the curve, this fails.
  exactCp <- function(times, clR, dose = 25, h = 1/1000) {
    ka <- HCTZ_WEIBULL_SCALE; gam <- HCTZ_WEIBULL_SHAPE
    cl <- clR + HCTZ_CL_NR
    k20 <- cl / HCTZ_V1; k23 <- HCTZ_Q / HCTZ_V1; k32 <- HCTZ_Q / HCTZ_V2
    A1 <- dose; A2 <- 0; A3 <- 0; t <- 0
    out <- numeric(length(times)); j <- 1
    for (i in seq_len(ceiling(max(times) / h))) {
      w  <- 1 - exp(-((ka * t)^gam))
      w2 <- 1 - exp(-((ka * (t + h / 2))^gam))
      A1m <- A1 + h / 2 * (-w * A1)
      A2m <- A2 + h / 2 * (w * A1 + k32 * A3 - (k20 + k23) * A2)
      A3m <- A3 + h / 2 * (k23 * A2 - k32 * A3)
      A1n <- A1 + h * (-w2 * A1m)
      A2n <- A2 + h * (w2 * A1m + k32 * A3m - (k20 + k23) * A2m)
      A3  <- A3 + h * (k23 * A2m - k32 * A3m)
      A1 <- A1n; A2 <- A2n; t <- t + h
      while (j <= length(times) && t >= times[j] - h / 2) {
        out[j] <- A2 / HCTZ_V1 * 1000; j <- j + 1      # ng/mL
      }
    }
    out
  }
  # One patient per rung of the paper's clearance ladder, each entered through
  # the serum creatinine that Cockcroft-Gault maps to that rung
  for (i in seq_along(c(120, 80, 50, 30))) {
    crcl <- c(120, 80, 50, 30)[i]
    clR  <- HCTZ_CL_R * (crcl / HCTZ_CRCL_REF)^HCTZ_CL_R_EXP
    scr  <- (140 - 50) * 70 / (72 * crcl)
    w <- simulateDrugsWithCovariates(
      data.frame(Drug = "hydrochlorothiazide", Time = 0, Dose = 25,
                 Units = "mg PO"),
      events, 70, 175, 50, "male", 24 * 60, FALSE,
      adjustToFFM = FALSE, creatinine = scr)$hydrochlorothiazide$wide
    w <- w[w$Time > 0, ]
    ex <- exactCp(w$Time / 60, clR)
    peak <- max(ex)
    # The peak concentration and the area are reproduced to well under a
    # tenth of a percent; the worst point anywhere on the curve, which falls
    # on the rising limb where the pulse structure is visible, to under 1.5%
    # of the peak.  The measured figures are in docs/antihypertensives.md.
    expect_lt(abs(max(w$Plasma) - peak) / peak, 0.005, label = paste("peak", crcl))
    expect_lt(max(abs(w$Plasma - ex)) / peak, 0.015, label = paste("curve", crcl))
    auc    <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
    aucEx  <- sum(diff(w$Time) * (head(ex, -1) + tail(ex, -1)) / 2)
    expect_lt(abs(auc - aucEx) / aucEx, 0.002, label = paste("AUC", crcl))
  }
})

test_that("a once-daily dose accumulates, and the repeats are pulsed too", {
  # expandScheduledDoses() runs before expandOralPulses(), so each repeat is
  # given as its own pulses; the check is that the sequence is still daily and
  # that steady state is reached rather than the dose being given once.
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "hydrochlorothiazide", Time = 0, Dose = 25,
               Units = "mg PO qd"),
    events, 70, 175, 50, "male", 10 * 1440, FALSE,
    adjustToFFM = FALSE)$hydrochlorothiazide$wide
  day1 <- max(w$Plasma[w$Time <= 1440])
  day9 <- max(w$Plasma[w$Time >= 8 * 1440 & w$Time <= 9 * 1440])
  expect_gt(day9, day1)                       # it accumulates
  expect_lt(day9 / day1, 2)                   # but not without limit
  # and day 10 is within a percent of day 9: steady state, not a ramp
  day10 <- max(w$Plasma[w$Time >= 9 * 1440])
  expect_lt(abs(day10 - day9) / day9, 0.01)
})

test_that("the curve agrees with the label, which the model was not fitted to", {
  # Checked against the LABEL rather than the paper: reproducing a source's
  # own numbers tests the transcription, and says nothing about whether the
  # curve is right.  The label's descriptors are in the registry's catalog
  # row for this product.
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "hydrochlorothiazide", Time = 0, Dose = 25,
               Units = "mg PO"),
    events, 70, 175, 50, "male", 24 * 60, FALSE,
    adjustToFFM = FALSE)$hydrochlorothiazide$wide
  tmax <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(tmax, 1)                 # labelled time to peak is 1 to 5 h
  expect_lt(tmax, 5)
  # Total exposure is dose over apparent clearance, which it must be for a
  # linear model and which the pulse discretisation must not disturb: the
  # fractions sum to one, so the whole dose is still given.
  long <- simulateDrugsWithCovariates(
    data.frame(Drug = "hydrochlorothiazide", Time = 0, Dose = 25,
               Units = "mg PO"),
    events, 70, 175, 50, "male", 7 * 1440, FALSE,
    adjustToFFM = FALSE)$hydrochlorothiazide$wide
  auc <- sum(diff(long$Time) *
               (head(long$Plasma, -1) + tail(long$Plasma, -1)) / 2) / 60
  cl <- getDrugPK("hydrochlorothiazide", 70, 175, 50, "male",
                  adjustToFFM = FALSE)$PK[[PK_EVENT_DEFAULT]]$cl1 * 60
  expect_lt(abs(auc - 25 * 1000 / cl) / (25 * 1000 / cl), 0.02)
})

test_that("impaired kidneys raise the trough far more than the peak", {
  # The numbers the help page quotes, pinned: on 25 mg daily the peak rises
  # about 16% over a week at a normal creatinine and about 68% at 2.5 mg/dL,
  # while the trough rises four and a half fold.  That the trough moves much
  # more than the peak is the signature of impaired elimination rather than
  # altered absorption, and it is what the renal term is for.
  weekOf <- function(scr) {
    w <- simulateDrugsWithCovariates(
      data.frame(Drug = "hydrochlorothiazide", Time = 0, Dose = 25,
                 Units = "mg PO qd"),
      events, 70, 175, 50, "male", 7 * 1440, FALSE,
      adjustToFFM = FALSE, creatinine = scr)$hydrochlorothiazide$wide
    list(day1 = max(w$Plasma[w$Time <= 1440]),
         day7 = max(w$Plasma[w$Time >= 6 * 1440]),
         trough = w$Plasma[which.min(abs(w$Time - 7 * 1440))])
  }
  normal   <- weekOf(0.9)
  impaired <- weekOf(2.5)
  expect_lt(abs(normal$day7 / normal$day1 - 1.16), 0.05)
  expect_lt(abs(impaired$day7 / impaired$day1 - 1.68), 0.08)
  # The trough moves far more than the peak
  expect_lt(abs(impaired$trough / normal$trough - 4.5), 0.5)
  expect_lt(impaired$day7 / normal$day7, 2)
})
