# "Time until threshold" in the three intravenous engines, checked against the
# definition: stop all delivery at time t, simulate on, and see when the effect
# site finally comes down through the threshold.
#
# This is the test that the feature lacked.  It is how the defects fixed on
# 2026-10-05 were found: no time reported while the effect site was still
# climbing after a bolus or an oral dose, and the time-varying-PK engine timing
# the plasma instead of the effect site.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05; run on R 4.6.1.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

pkFor <- function(drug, weight = 70, height = 170, age = 50, sex = "male") {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK(drug = drug, weight = weight, height = height, age = age,
                  sex = sex, drugDefaults = dd[dd$Drug == drug, ])
  PK$endCe <- dd$endCe[dd$Drug == drug]
  PK
}

# Stop every infusion at t, give nothing after it, and time the effect site.
bruteRecovery <- function(DT, ET, PK, t, horizon = 720) {
  d <- DT[DT$Time <= t, , drop = FALSE]
  for (u in unique(d$Units[grepl("min|hr", d$Units)]))
    d <- rbind(d, data.frame(Drug = d$Drug[1], Time = t, Dose = 0, Units = u))
  r <- simCpCe(d[order(d$Time), ], ET, PK, t + horizon, FALSE)$results
  ce <- r[r$Site == "Effect Site" & r$Time >= t, ]
  above <- which(ce$Y > PK$endCe)
  if (length(above) == 0) return(0)
  i <- max(above)
  ce$Time[i] + (PK$endCe - ce$Y[i]) * (ce$Time[i + 1] - ce$Time[i]) /
    (ce$Y[i + 1] - ce$Y[i]) - t
}

# The value the app would show at t.  Taken from the grid point at or just
# before t, since the curve jumps at every dose.
shown <- function(sim, t) {
  es <- sim$equiSpace
  es$Recovery[max(which(es$Time <= t + 1e-9))]
}

# The grid time that value was actually read FROM, so that a brute-force run
# can be compared against it at the same instant rather than at t.  Where
# recovery is changing steeply -- on bypass it grows about ten minutes per
# minute of continued infusion -- a grid point under a minute away from t is
# worth ten minutes of difference, which is sampling offset rather than
# anything the engine did.  (Diagnosed by the weight-adjustment session,
# 2026-10-06, while scaling the same test's PK to fat-free mass.)
shownTime <- function(sim, t) {
  es <- sim$equiSpace
  es$Time[max(which(es$Time <= t + 1e-9))]
}

# `tolerance` is in MINUTES.  The plotted series is the engine's own time line
# interpolated onto 100 points, so where the curve has a corner -- reaching zero
# as the effect site crosses the threshold -- it can be out by up to the spacing
# of the points either side.
expectMatchesBrute <- function(DT, ET, PK, maximum, tolerance = 1) {
  sim <- simCpCe(DT, ET, PK, maximum, TRUE)
  grid <- sim$equiSpace$Time
  # Sample the plotted grid, skipping points within a grid step after a dose,
  # where the plotted line is joining two sides of a jump.
  step <- grid[2] - grid[1]
  ok <- vapply(grid, function(t) all(t - DT$Time < 0 | t - DT$Time > step), logical(1))
  for (t in grid[ok][seq(2, sum(ok), length.out = 12)]) {
    expect_lt(abs(shown(sim, t) - bruteRecovery(DT, ET, PK, t)), tolerance,
              label = paste(PK$drug, "at", round(t, 1), "min: difference in minutes"))
  }
}


test_that("bolus plus infusion: the plain intravenous engine matches stopping delivery", {
  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60), Dose = c(140, 100, 0),
                   Units = c("mg", "mcg/kg/min", "mcg/kg/min"))
  expectMatchesBrute(DT, noEvents, pkFor("propofol"), 120)

  DT <- data.frame(Drug = "fentanyl", Time = c(0, 30), Dose = c(100, 50), Units = "mcg")
  expectMatchesBrute(DT, noEvents, pkFor("fentanyl"), 90)
})


test_that("a time is reported while the effect site is still climbing after a bolus", {
  PK <- pkFor("fentanyl")
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  # A ten-minute simulation, so that the plotted grid is fine enough to catch
  # the first half minute.
  sim <- simCpCe(DT, noEvents, PK, 10, TRUE)
  es <- sim$equiSpace
  early <- es[es$Time > 0 & es$Time <= 1.5, ]
  # Still below the threshold on the way up for part of this window...
  expect_true(any(early$Ce < PK$endCe))
  # ...but it is going above it, so the time until threshold is not zero.
  expect_true(all(early$Recovery > 10))
})


test_that("an oral dose: time is reported through the absorption phase", {
  PK <- pkFor("oxycodone")
  DT <- data.frame(Drug = "oxycodone", Time = c(0, 240), Dose = c(10, 10), Units = "mg PO")
  expectMatchesBrute(DT, noEvents, PK, 480, tolerance = 6)   # 4.8 min grid

  sim <- simCpCe(DT, noEvents, PK, 480, TRUE)
  es <- sim$equiSpace
  rising <- es[es$Time > 0 & es$Ce < PK$endCe & es$Time < 25, ]
  expect_gt(nrow(rising), 0)
  expect_true(all(rising$Recovery > 100))
})


test_that("time-varying PK: the effect site is timed, not the plasma", {
  # Dexmedetomidine in an infant (age <= 1) switches PK on cardiopulmonary bypass, which
  # routes it through advanceClosedForm1().
  PK <- pkFor("dexmedetomidine", weight = 7, height = 65, age = 0.5)
  skip_if(length(PK$pkEvents) < 2, "dexmedetomidine has no PK events for this patient")
  ET <- data.frame(Time = 30, Event = "CPB Start")
  DT <- data.frame(Drug = "dexmedetomidine", Time = c(0, 0), Dose = c(1, 0.7),
                   Units = c("mcg/kg", "mcg/kg/hr"))
  sim <- simCpCe(DT, ET, PK, 120, TRUE)
  expect_gt(max(sim$equiSpace$Recovery), 0)
  # After the last change in PK, so that "the PK in force now" is also the PK
  # the brute-force run goes on using.
  for (t in c(45, 60, 90, 110)) {
    # Compared at the grid instant the shown value was read from, not at t.
    # Comparing at t folded a sub-minute sampling offset into the assertion and
    # needed a 3% tolerance to absorb it, which made the test sensitive to the
    # covariate model rather than to the engine -- scaling this patient's PK to
    # fat-free mass pushed it to 3.2% and the test failed for no engine reason.
    # Matched instants agree to well under 1%, so the limit is 1%, or three
    # quarters of a minute where the time itself is only a minute or two.
    at   <- shownTime(sim, t)
    want <- bruteRecovery(DT, ET, PK, at, horizon = 1440)
    expect_lt(abs(shown(sim, t) - want), max(0.75, 0.01 * want),
              label = paste("at", round(at, 2), "min: difference in minutes"))
  }
})


# ---------------------------------------------------------------------------
# A drug that arrives partly as another drug's active metabolite
# ---------------------------------------------------------------------------
#
# Three pairs: codeine forms morphine, hydrocodone forms hydromorphone, and
# oxycodone forms oxymorphone.  Each receiving drug carries opioid the patient
# was given and opioid formed from the parent, so its time until threshold has
# to be the time for the sum.  Recovery does not superpose, so it is solved
# again from the underlying effect-site states; these tests check that against
# the definition, the same way the ones above do for a single drug.
#
# Not a jointly simulated washout, which is what the inhaled gases need
# (gasCoupledRecovery()): the intravenous path is linear end to end, so summing
# the states is exact.
#
# (Claude Code, Claude Opus 5, 2026-10-05; run on R 4.6.1.)

refSim <- function(DT, maximum, plotRecovery = TRUE)
  simulateDrugsWithCovariates(DT, noEvents, 70, 171, 50, "male", maximum,
                              plotRecovery)

# A zero dose adds points to an engine's time line and changes nothing else,
# which is how the brute-force run is given a grid fine enough to find the
# crossing.  Without it the metabolite engine's geometric line is over an hour
# wide out where the crossing falls, and that spacing, not the quantity under
# test, is what the comparison would measure.
densify <- function(d, drugs, from, to, by = 2)
  rbind(d, do.call(rbind, lapply(drugs, function(g)
    data.frame(Drug = g, Time = seq(from, to, by = by), Dose = 0, Units = "mg"))))

# Stop all delivery at t -- of the parent as well, though the parent already in
# the body goes on forming metabolite -- and time the receiving row's combined
# effect site.
bruteMetabolite <- function(DT, target, t, endCe, horizon = 2000) {
  d <- DT[DT$Time <= t, , drop = FALSE]
  for (i in which(grepl("min|hr", d$Units)))
    d <- rbind(d, data.frame(Drug = d$Drug[i], Time = t, Dose = 0,
                             Units = d$Units[i]))
  d <- densify(d, unique(DT$Drug), t, t + horizon)
  w <- refSim(d[order(d$Time), ], t + horizon, FALSE)[[target]]$wide
  w <- w[w$Time >= t, ]
  above <- which(w$"Effect Site" > endCe)
  if (length(above) == 0) return(0)
  i <- max(above)
  if (i == nrow(w)) return(MINS_PER_DAY)
  w$Time[i] + (endCe - w$"Effect Site"[i]) * (w$Time[i + 1] - w$Time[i]) /
    (w$"Effect Site"[i + 1] - w$"Effect Site"[i]) - t
}

# Sampled on the merged series' own time line, so that what is shown is read
# where it was computed rather than interpolated across a corner.
expectFoldMatchesBrute <- function(DT, target, maximum, horizon = 2000,
                                   tolerance = 0.5, n = 5) {
  o <- refSim(DT, maximum)
  w <- o[[target]]$wide
  endCe <- o[[target]]$endCe
  for (t in w$Time[round(seq(3, nrow(w) - 8, length.out = n))])
    expect_lt(abs(w$Recovery[w$Time == t] -
                    bruteMetabolite(DT, target, t, endCe, horizon)),
              tolerance,
              label = paste(target, "at", round(t, 1),
                            "min: difference in minutes"))
}


test_that("formed metabolite alone: the row matches stopping delivery", {
  # Before this fix the receiving row reported no time at all, because its
  # recovery was taken from doses that were never given.
  #
  # Oxycodone is the case that matters clinically: 40 mg is an ordinary dose,
  # and the oxymorphone formed from it clears oxymorphone's own threshold.
  DT <- data.frame(Drug = "oxycodone", Time = 0, Dose = 40, Units = "mg PO")
  o <- refSim(DT, 1440)
  expect_gt(max(o$oxymorphone$wide$"Effect Site"), o$oxymorphone$endCe)
  expect_gt(max(o$oxymorphone$wide$Recovery), 0)
  expectFoldMatchesBrute(DT, "oxymorphone", 1440)

  # Codeine needs a dose far above any clinical one, morphine's threshold being
  # very low relative to what codeine forms; it is kept because it is the
  # original worked example and the only pure prodrug among the three.
  expectFoldMatchesBrute(
    data.frame(Drug = "codeine", Time = 0, Dose = 600, Units = "mg PO"),
    "morphine", 1440)
})


test_that("given and formed together: the row times the sum", {
  # Ordinary doses of both, which is where the understatement was worst: the
  # row showed the time for the injected oxymorphone alone.
  DT <- data.frame(Drug  = c("oxycodone", "oxymorphone"),
                   Time  = c(0, 0),
                   Dose  = c(20, 1),
                   Units = c("mg PO", "mg"))
  expectFoldMatchesBrute(DT, "oxymorphone", 1440)

  o     <- refSim(DT, 1440)
  alone <- refSim(DT[DT$Drug == "oxymorphone", ], 1440)
  expect_gt(max(o$oxymorphone$wide$Recovery),
            max(alone$oxymorphone$wide$Recovery) + 60)

  DT <- data.frame(Drug  = c("codeine", "morphine"),
                   Time  = c(0, 0),
                   Dose  = c(600, 10),
                   Units = c("mg PO", "mg"))
  expectFoldMatchesBrute(DT, "morphine", 1440)
  o     <- refSim(DT, 1440)
  alone <- refSim(DT[DT$Drug == "morphine", ], 1440)
  expect_gt(max(o$morphine$wide$Recovery), max(alone$morphine$wide$Recovery))
})


test_that("hydrocodone's hydromorphone, through the extravascular engine", {
  # Hydromorphone given orally goes through advanceClosedFormPO_IM_IN, whose
  # state set has seven terms, while the formed contribution has seven of its
  # own over a different eigenvalue set.  This is the case that checks two
  # differently shaped sets concatenate.
  #
  # Orally rather than intranasally, which carries a three-hour absorption lag.
  # The engine reports no time at all until a lagged dose starts, having no
  # effect-site state to report one from until then, while the brute-force run
  # replays the lag and disagrees.  That is the single-drug engine's own
  # behaviour with no metabolite anywhere in sight, so it is not this fold's to
  # settle.
  DT <- data.frame(Drug  = c("hydrocodone", "hydromorphone"),
                   Time  = c(0, 0),
                   Dose  = c(30, 2),
                   Units = c("mg PO", "mg PO"))
  expectFoldMatchesBrute(DT, "hydromorphone", 1440)

  o     <- refSim(DT, 1440)
  alone <- refSim(DT[DT$Drug == "hydromorphone", ], 1440)
  expect_gt(max(o$hydromorphone$wide$Recovery),
            max(alone$hydromorphone$wide$Recovery))
})


test_that("an infusion of the metabolite drug alongside the parent", {
  # The states carry an infusion as well as a bolus, so the merged row has to
  # work while the receiving drug is still running.
  expectFoldMatchesBrute(
    data.frame(Drug  = c("oxycodone", "oxymorphone"),
               Time  = c(0, 0),
               Dose  = c(20, 0.2),
               Units = c("mg PO", "mg/hr")),
    "oxymorphone", 720)

  expectFoldMatchesBrute(
    data.frame(Drug  = c("codeine", "morphine"),
               Time  = c(0, 0),
               Dose  = c(600, 2),
               Units = c("mg PO", "mg/hr")),
    "morphine", 720)
})


test_that("doses at different times, through both engines", {
  # Codeine intravenously -- so the parent goes through the bolus branch of
  # advanceClosedFormMetabolite -- and morphine an hour later, which puts the
  # two time lines out of step and makes the merge interpolate.
  expectFoldMatchesBrute(
    data.frame(Drug  = c("codeine", "morphine"),
               Time  = c(0, 60),
               Dose  = c(300, 5),
               Units = c("mg", "mg")),
    "morphine", 1440)
})


test_that("a drug with no metabolite is untouched by the fold", {
  # The receiving drug's own recovery still comes from its own engine, and
  # nothing about carrying the states out changed it.
  PK <- pkFor("morphine", height = 171)
  DT <- data.frame(Drug = "morphine", Time = 0, Dose = 10, Units = "mg")
  direct <- simCpCe(DT, noEvents, PK, 1440, TRUE)
  folded <- refSim(DT, 1440)$morphine
  expect_equal(folded$wide$Recovery, direct$wide$Recovery)
  expect_null(folded$formedFrom)
})


test_that("exactly the plasma-only drugs have no effect site", {
  # The set has moved repeatedly while this was being written, so it is pinned:
  # a drug losing or gaining an effect site changes which branch of the fold it
  # takes.  If this fails, the set has changed and the NA paths want rechecking
  # rather than the number updating on its own.
  dd <- getDrugDefaultsGlobal()
  noCe <- character(0)
  for (drug in dd$Drug[!isGasDrug(dd$Drug)])
  {
    PK <- tryCatch(getDrugPK(drug, 70, 171, 50, "male", dd[dd$Drug == drug, ]),
                   error = function(e) NULL)
    if (is.null(PK)) next
    if (all(vapply(PK$PK, function(s) s$ke0 == 0, logical(1))))
      noCe <- c(noCe, drug)
  }
  # Was three until 2026-10-06, when desmetramadol was given a tPeak and so
  # an effect site, then two (codeine, tramadol).  The antibiotics, the
  # steroids, sugammadex and glycopyrrolate joined on 2026-10-06 as
  # plasma-only drugs by design (no equilibration model to attach; see each
  # header).  That brought a REAL pair whose receiving drug has no effect
  # site, prednisone -> prednisolone, so the NA fold path that
  # test-metabolite-merge.R constructs is now also exercised by a real drug:
  # see "oral prednisone folds onto a prednisolone row with no effect site"
  # below.  Mannitol joined too: plotted as serum osmolality, no published
  # ke0, and it neither forms nor receives a metabolite, so it never reaches
  # the fold.
  # Gabapentin joined on 2026-10-07: no estimated human equilibration delay
  # yet (GABAPENTIN_TPEAK in R/drugs_gabapentin.R).  Amiodarone and
  # desethylamiodarone joined on 2026-10-07: an ACTIVE parent with no effect
  # site (no human ke0 for the antiarrhythmic effect), forming a metabolite
  # with none either.  The fold path is prednisone's, driven by a
  # constant-rate input instead of an oral dose; rechecked in the next test.
  # AmiodaroneIV joined on 2026-10-08: active, no effect site and no
  # metabolite, so it never reaches the fold either.  Clonazepam, zolpidem and
  # temazepam (direct effects, no effect site) joined on 2026-10-09, with no
  # metabolite either.  Methylphenidate and lisdexamfetamine joined on
  # 2026-10-10: no calibrated concentration-effect model, no metabolite.
  # Bupivacaine, ropivacaine and mepivacaine (systemic
  # plasma only, regional anesthesia) joined on 2026-10-10, with no metabolite.
  # The antidepressants joined on 2026-10-10: response
  # lags weeks and no ke0 exists; bupropion forms hydroxybupropion, which has
  # no effect site either, so the fold takes the NA path, as for amiodarone.
  # Ondansetron, aprepitant and fosaprepitant (no equilibration model for
  # antiemesis) joined on 2026-10-10, with no metabolite.
  # Diclofenac, meloxicam and ketorolac (no published ke0) joined the same
  # day, with no metabolite.
  expect_setequal(noCe, c(
    "codeine", "tramadol", "prednisone",
    "cefazolin", "clindamycin", "cefalexin", "ceftriaxone", "vancomycin",
    "metronidazole", "gentamicin",
    "hydrocortisone", "methylprednisolone", "dexamethasone", "prednisolone",
    "sugammadex", "glycopyrrolate", "mannitol", "gabapentin",
    "amiodarone", "desethylamiodarone", "amiodaroneIV",
    "clonazepam", "zolpidem", "temazepam",
    "methylphenidate", "lisdexamfetamine",
    "bupivacaine", "ropivacaine", "mepivacaine",
    "escitalopram", "citalopram", "sertraline", "paroxetine", "duloxetine",
    "mirtazapine", "bupropion", "hydroxybupropion", "fluoxetine", "norfluoxetine",
    "ondansetron", "aprepitant", "fosaprepitant",
    "diclofenac", "meloxicam", "ketorolac"
  ))
})


test_that("amiodarone folds onto a desethylamiodarone row with no effect site", {
  # The same path as prednisone's below, from a constant-rate input
  # ("mg/day PO").  The receiving row has no effect site and no threshold, so
  # it reads zero; the parent's own threshold (1.0 mg/L, the bottom of the
  # therapeutic window) is timed on its plasma.  (Claude Code, 2026-10-07.)
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "amiodarone", Time = 0, Dose = 1600, Units = "mg/day PO"),
    noEvents, 70, 170, 50, "male", 1440, TRUE)
  dea <- o$desethylamiodarone
  expect_equal(dea$formedFrom, "amiodarone")
  expect_gt(max(dea$wide$Plasma), 0)
  expect_true(all(is.na(dea$wide$"Effect Site")))
  expect_true(all(dea$equiSpace$Recovery == 0))
  amio <- o$amiodarone$wide
  expect_true(all(is.na(amio$"Effect Site")))
  expect_gt(amio$Recovery[nrow(amio)], 0)
})


test_that("oral prednisone folds onto a prednisolone row with no effect site", {
  # The receiving drug has ke0 = 0, so the fold has no effect-site states to
  # solve a time until threshold from: the plasma fold must still happen, the
  # effect site stays NA, and the recovery column reads zero throughout,
  # which is the convention every effect-site-free row (codeine's own row
  # included) already follows.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "prednisone", Time = 0, Dose = 20, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 170, 35, "male", 720, TRUE)
  pl <- o$prednisolone
  expect_equal(pl$formedFrom, "prednisone")
  expect_gt(max(pl$wide$Plasma, na.rm = TRUE), 0)
  expect_true(all(is.na(pl$wide$"Effect Site")))
  expect_true(all(pl$equiSpace$Recovery == 0))
})


# The plasma at u, by simulating to u and reading the last point, which is
# always at the end of the run: nothing interpolated.
plasmaAt <- function(DT, PK, u) {
  r <- simCpCe(DT, noEvents, PK, u, FALSE)$wide
  r$Plasma[nrow(r)]
}

# The engine says that, given nothing more after t, the plasma comes down
# through PK$endCe at `at`.  Then it must be above the threshold `within`
# minutes before that instant and below it `within` minutes after.  `DT`
# holds the doses given up to t.  recoveryCalc() solved to uniroot()'s
# tolerance of 0.01 min when this was written (RECOVERY_TOL, 1e-6 min, since
# 2026-10-09), and the largest error measured in these tests was
# 2.5e-3 min (by uniroot() on simulations run to the crossing), so the
# callers use 0.02.  (Claude Code, 2026-10-07, mutation review.)
expectPlasmaCrossesAt <- function(DT, PK, at, within, label) {
  expect_gt(plasmaAt(DT, PK, at - within), PK$endCe,
            label = paste(label, ": plasma just before the reported instant"))
  expect_lt(plasmaAt(DT, PK, at + within), PK$endCe,
            label = paste(label, ": plasma just after the reported instant"))
}


test_that("a drug with no effect site is timed on its plasma", {
  # The antibiotics have no effect site.  Their threshold is the MIC, and the
  # time until threshold is the time until the PLASMA falls to it if no more
  # is given -- including, for an oral dose, drug still in the gut, which has
  # been given and keeps arriving.  Until 2026-10-07 these drugs carried
  # all-zero effect-site states and so always read zero.  The thresholds are
  # set here rather than read from the library, so that the test is about the
  # engines and not about the defaults.
  bruteP <- function(DT, PK, t, horizon = 1440) {
    r <- simCpCe(DT[DT$Time <= t, , drop = FALSE], noEvents, PK, t + horizon, FALSE)$wide
    r <- r[r$Time >= t, ]
    above <- which(r$Plasma > PK$endCe)
    if (length(above) == 0) return(0)
    j <- max(above)
    # log-linear: the decline is exponential and the points are far apart
    r$Time[j] + log(r$Plasma[j] / PK$endCe) / log(r$Plasma[j] / r$Plasma[j + 1]) *
      (r$Time[j + 1] - r$Time[j]) - t
  }

  # Intravenous: the plain engine
  PK <- pkFor("cefazolin")
  PK$endCe <- 2
  DT <- data.frame(Drug = "cefazolin", Time = c(0, 240), Dose = 2000, Units = "mg")
  sim <- simCpCe(DT, noEvents, PK, 480, TRUE)
  expect_gt(max(sim$equiSpace$Recovery), 0)
  for (t in c(10, 60, 200, 300, 450)) {
    at <- shownTime(sim, t)
    expect_lt(abs(shown(sim, t) - bruteP(DT, PK, at)), 0.5,
              label = paste("cefazolin at", round(at, 1), "min: difference in minutes"))
  }

  # Oral: the extravascular engine, through the absorption phase
  PK <- pkFor("cefalexin")
  PK$endCe <- 2
  DT <- data.frame(Drug = "cefalexin", Time = c(0, 360), Dose = 500, Units = "mg PO")
  sim <- simCpCe(DT, noEvents, PK, 720, TRUE)
  for (t in c(5, 30, 120, 365, 500)) {
    at <- shownTime(sim, t)
    expect_lt(abs(shown(sim, t) - bruteP(DT, PK, at)), 0.5,
              label = paste("cefalexin at", round(at, 1), "min: difference in minutes"))
  }
  # Straight after the first dose the plasma is below the MIC, but the drug in
  # the gut is going to take it over: a time, not zero.  A 30-minute run, so
  # that the plotted grid is fine enough to see the first minutes.
  sim <- simCpCe(DT[1, ], noEvents, PK, 30, TRUE)
  es <- sim$equiSpace
  cp <- stats::approx(sim$wide$Time, sim$wide$Plasma, es$Time)$y
  rising <- es[es$Time > 0 & cp < PK$endCe, ]
  expect_gt(nrow(rising), 0)
  expect_true(all(rising$Recovery > 60))

  # The absorption term itself.  At 2 mg/L the crossing comes hours after the
  # dose, by which time the term at ka has decayed to nothing, so the checks
  # above pass whether or not the plasma states include it.  Two thresholds
  # either side of the peak do not:
  #  - just ABOVE the peak the plasma never gets there, so the time is zero
  #    throughout.  Leave the ka term out and what remains, the disposition
  #    term alone, starts at more than twice the peak and reads hours.
  #  - just BELOW it, straight after the dose, the plasma is far below the
  #    threshold and the time is to the far side of the peak, which depends
  #    on how fast the gut empties.  Checked at the engine's own points over
  #    the first half hour by simulating to the instant reported.
  # (Claude Code, 2026-10-07, mutation review.)
  DT1  <- DT[1, ]
  peak <- max(simCpCe(DT1, noEvents, PK, 720, FALSE)$wide$Plasma)
  PK$endCe <- 1.05 * peak
  expect_true(all(simCpCe(DT1, noEvents, PK, 720, TRUE)$wide$Recovery == 0))
  PK$endCe <- 0.9 * peak
  w <- simCpCe(DT1, noEvents, PK, 720, TRUE)$wide
  early <- which(w$Time > 0 & w$Time <= 30)
  expect_true(all(w$Plasma[early] < PK$endCe))
  for (i in early)
    expectPlasmaCrossesAt(DT1, PK, w$Time[i] + w$Recovery[i], 0.02,
                          paste("cefalexin at 0.9 x peak, from", round(w$Time[i], 2), "min"))
})


test_that("a lagged dose of a drug with no effect site is masked, not timed", {
  # The plasma branch of advanceClosedFormPO_IM_IN() carries its own copy of
  # the not-yet-absorbing mask (see R/recoveryStates.R), and nothing tested it:
  # every lag test was on a drug with an effect site.  No drug in the library
  # has a lag, so cefalexin is given one by hand.  The second dose is given
  # while the first is well above the threshold, so an unmasked answer there
  # would be a plausible time rather than zero.
  # (Claude Code, 2026-10-07, mutation review.)
  PK <- pkFor("cefalexin")
  PK$PK$default$tlag_PO <- 20
  DT <- data.frame(Drug = "cefalexin", Time = c(0, 120), Dose = 500, Units = "mg PO")
  w <- simCpCe(DT, noEvents, PK, 480, TRUE)$wide
  expect_true(all(w$Plasma[w$Time < 20] == 0))
  pending <- w$Time < 20 | (w$Time >= 120 & w$Time < 140)
  expect_true(all(c(0, 20, 120, 140) %in% w$Time))
  expect_true(all(is.na(w$Recovery[pending])))
  expect_false(anyNA(w$Recovery[!pending]))
  expect_gt(w$Recovery[max(which(w$Time < 120))], 60)
  # Once the second dose has landed the time is right again.
  for (i in which(w$Time >= 140 & w$Time <= 200))
    expectPlasmaCrossesAt(DT, PK, w$Time[i] + w$Recovery[i], 0.02,
                          paste("lagged cefalexin, from", round(w$Time[i], 2), "min"))
})


test_that("a pure prodrug is timed on its own plasma, through its absorption", {
  # Codeine has no effect site of its own, so a threshold set on it is timed
  # on its plasma, through advanceClosedFormMetabolite(), whose parent plasma
  # states carry the absorption term at ka_PO.  The same two thresholds
  # either side of the peak as for cefalexin above, for the same reason.
  # Codeine's shipped threshold is zero; these are set by hand.
  # (Claude Code, 2026-10-07, mutation review.)
  PK <- pkFor("codeine", height = 171)
  DT <- data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO")
  peak <- max(simCpCe(DT, noEvents, PK, 720, FALSE)$wide$Plasma)
  PK$endCe <- 1.05 * peak
  expect_true(all(simCpCe(DT, noEvents, PK, 720, TRUE)$wide$Recovery == 0))
  PK$endCe <- 0.9 * peak
  w <- simCpCe(DT, noEvents, PK, 720, TRUE)$wide
  early <- which(w$Time > 0 & w$Time <= 30)
  expect_true(all(w$Plasma[early] < PK$endCe))
  for (i in early)
    expectPlasmaCrossesAt(DT, PK, w$Time[i] + w$Recovery[i], 0.02,
                          paste("codeine at 0.9 x peak, from", round(w$Time[i], 2), "min"))
})
