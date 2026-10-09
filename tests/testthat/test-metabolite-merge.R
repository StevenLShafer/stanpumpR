# Tests for unit scaling between a parent and its metabolite, and for summing a
# metabolite contribution into the metabolite drug's own row.
#
# The merge works in the pipeline's own wide shape -- Time, Plasma, Effect Site
# and Recovery -- because that is what processdoseTable() holds and what
# finishDrugSeries() needs to rebuild the receiving drug's plotted series.  The
# contribution arriving from a parent is in Cp/Ce terms, which is what
# simCpCe() lifts out of advanceClosedFormMetabolite().

wideSeries <- function(Time, Plasma, Ce, Recovery = rep(0, length(Time))) {
  data.frame(Time = Time, Plasma = Plasma, `Effect Site` = Ce,
             Recovery = Recovery, check.names = FALSE)
}

# A drug entry carrying the minimum finishDrugSeries() needs
drugEntry <- function(name, wideOwn = NULL) {
  list(drug = name, MEAC = 0, wideOwn = wideOwn)
}


test_that("internal dose units follow Concentration.Units", {
  # simCpCe carries doses in mg for a drug reported in mcg/mL, and in mcg for
  # one reported in ng/mL.
  expect_equal(internalDoseScale("mcg"), 1)
  expect_equal(internalDoseScale("ng"), 1e-3)
  expect_error(internalDoseScale("kg"), "Unsupported")
})


test_that("metaboliteUnitScale catches a mismatched parent and metabolite", {
  # Same units on both sides needs no correction
  expect_equal(metaboliteUnitScale("mcg", "mcg"), 1)
  expect_equal(metaboliteUnitScale("ng", "ng"), 1)

  # A ng-reported parent carried in mcg, forming a mcg-reported metabolite
  # carried in mg, is a thousandfold conversion
  expect_equal(metaboliteUnitScale("ng", "mcg"), 1e-3)
  expect_equal(metaboliteUnitScale("mcg", "ng"), 1e3)

  # The real pairings this exists for
  dd <- getDrugDefaultsGlobal(FALSE)
  hydro <- dd$Concentration.Units[dd$Drug == "hydromorphone"]
  morph <- dd$Concentration.Units[dd$Drug == "morphine"]
  cod   <- dd$Concentration.Units[dd$Drug == "codeine"]
  expect_equal(hydro, "ng")
  expect_equal(morph, "mcg")
  expect_equal(cod,   "ng")
  expect_equal(metaboliteUnitScale(hydro, morph), 1e-3)
  expect_equal(metaboliteUnitScale(cod,   morph), 1e-3)
})


test_that("the unit scale carries straight into the coefficients", {
  parent <- getDrugPK("hydromorphone", 70, 170, 50, "male",
                      getDrugDefaults("hydromorphone"))$PK$default
  met    <- getDrugPK("morphine", 70, 170, 50, "male",
                      getDrugDefaults("morphine"))$PK$default

  plain  <- metaboliteCoefficients(parent, met, 0.01)
  scaled <- metaboliteCoefficients(parent, met, 0.01, unitScale = 1e-3)

  expect_equal(scaled$bolus, plain$bolus * 1e-3, tolerance = 1e-15)
  expect_equal(scaled$K, plain$K * 1e-3, tolerance = 1e-15)
  # Still starts at zero
  expect_equal(sum(scaled$bolus), 0, tolerance = 1e-15)

  expect_error(metaboliteCoefficients(parent, met, 0.01, unitScale = 0))
})


test_that("mergeMetaboliteSeries adds two series on a shared timeline", {
  a <- wideSeries(c(0, 10, 20), c(0, 4, 2), c(0, 2, 3))
  b <- data.frame(Time = c(0, 10, 20), Cp = c(0, 1, 1), Ce = c(0, 1, 1))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Time, c(0, 10, 20))
  expect_equal(m$Plasma, c(0, 5, 3))
  expect_equal(m$"Effect Site", c(0, 3, 4))
})


test_that("mergeMetaboliteSeries interpolates onto the union of two timelines", {
  # The two drugs build their own timelines around their own dose times, so the
  # grids do not line up.
  a <- wideSeries(c(0, 20), c(0, 20), c(0, 10))
  b <- data.frame(Time = c(0, 10, 20), Cp = c(0, 5, 0), Ce = c(0, 1, 0))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Time, c(0, 10, 20))
  # a is linear 0..20, so it interpolates to 10 at t = 10
  expect_equal(m$Plasma, c(0, 15, 20))
  expect_equal(m$"Effect Site", c(0, 6, 10))
})


test_that("recovery is not summed by the merge", {
  # Recovery is not a concentration and does not superpose, so the merge does
  # not add the two columns.  It carries the receiving drug's own through as a
  # placeholder; foldMetabolites() solves the combined time from the underlying
  # effect-site states, which is tested below and in test-recovery-engines.R.
  a <- wideSeries(c(0, 10), c(0, 4), c(0, 2), Recovery = c(0, 7))
  b <- data.frame(Time = c(0, 10), Cp = c(0, 1), Ce = c(0, 1))

  expect_equal(mergeMetaboliteSeries(a, b)$Recovery, c(0, 7))
})


test_that("a missing side of the merge is handled", {
  a <- wideSeries(c(0, 10), c(0, 4), c(0, 2))
  contribution <- data.frame(Time = c(0, 10), Cp = c(0, 4), Ce = c(0, 2))
  empty <- data.frame(Time = numeric(0), Cp = numeric(0), Ce = numeric(0))

  # Nothing to add leaves the base alone
  expect_equal(mergeMetaboliteSeries(a, NULL), a)
  expect_equal(mergeMetaboliteSeries(a, empty), a)

  # No base at all means the metabolite drug was never given directly, so the
  # contribution becomes the whole series
  made <- mergeMetaboliteSeries(NULL, contribution)
  expect_equal(made$Time, c(0, 10))
  expect_equal(made$Plasma, c(0, 4))
  expect_equal(made$"Effect Site", c(0, 2))
  expect_equal(made$Recovery, c(0, 0))

  # A one-row contribution is kept as it is (approx() needs two points; a
  # review of the F19 change found the merge had started to stop on it)
  one <- mergeMetaboliteSeries(NULL, data.frame(Time = 5, Cp = 1, Ce = 0.5))
  expect_equal(one$Time, 5)
  expect_equal(one$Plasma, 1)
  expect_equal(one$"Effect Site", 0.5)
})


test_that("a prodrug's NA effect site survives the merge", {
  # Interpolating an all-NA column would error; it has to pass through.
  a <- wideSeries(c(0, 10), c(0, 4), c(NA_real_, NA_real_))
  b <- data.frame(Time = c(0, 10), Cp = c(0, 1), Ce = c(0, 1))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Plasma, c(0, 5))
  expect_true(all(is.na(m$"Effect Site")))
})


test_that("foldMetabolites sums a contribution into the metabolite's own row", {
  # morphine given directly, and morphine formed from codeine
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10, 20),
                                    Cp = c(0, 1, 2), Ce = c(0, 0.5, 1))
    )),
    morphine = drugEntry("morphine", wideSeries(c(0, 10, 20),
                                                c(0, 4, 3), c(0, 2, 2)))
  )

  out <- foldMetabolites(drugs, maximum = 20)

  expect_equal(out$morphine$wide$Plasma, c(0, 5, 5))
  expect_equal(out$morphine$wide$"Effect Site", c(0, 2.5, 3))
  expect_equal(out$morphine$formedFrom, "codeine")
  # The plotted series is rebuilt from the sum
  plasma <- out$morphine$results
  expect_equal(plasma$Y[plasma$Site == "Plasma"], c(0, 5, 5))
  expect_equal(out$morphine$max$Cp, 5)
  # The parent is left alone
  expect_equal(out$codeine$metaboliteSeries$Cp, c(0, 1, 2))
})


test_that("folding twice does not count the contribution twice", {
  # processdoseTable() folds on every reactive invalidation, so the operation
  # has to start from the receiving drug's own simulation each time.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    morphine = drugEntry("morphine", wideSeries(c(0, 10), c(0, 4), c(0, 2)))
  )

  once  <- foldMetabolites(drugs, maximum = 10)
  twice <- foldMetabolites(once,  maximum = 10)

  expect_equal(once$morphine$wide$Plasma, c(0, 6))
  expect_equal(twice$morphine$wide$Plasma, c(0, 6))
})


test_that("foldMetabolites creates the row when the metabolite was not given", {
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    morphine = drugEntry("morphine")   # resolved, but never dosed
  )
  out <- foldMetabolites(drugs, maximum = 10)

  expect_equal(out$morphine$wide$Plasma, c(0, 2))
  expect_equal(out$morphine$formedFrom, "codeine")
})


test_that("two parents forming the same metabolite both contribute", {
  # Codeine and hydrocodone are both prodrugs; nothing stops a table holding
  # both once hydrocodone is added.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    hydrocodone = c(drugEntry("hydrocodone"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 3), Ce = c(0, 1))
    )),
    morphine = drugEntry("morphine")
  )
  out <- foldMetabolites(drugs, maximum = 10)

  expect_equal(out$morphine$wide$Plasma, c(0, 5))
  expect_setequal(out$morphine$formedFrom, c("codeine", "hydrocodone"))
})


test_that("a contribution with nowhere to go is skipped rather than guessed", {
  # The metabolite drug has no resolved PK, so its row cannot be finished.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    ))
  )
  expect_equal(foldMetabolites(drugs, maximum = 10), drugs)
})


test_that("foldMetabolites leaves a drug list with no metabolites alone", {
  drugs <- list(morphine = drugEntry("morphine", wideSeries(0, 0, 0)))
  expect_equal(foldMetabolites(drugs, maximum = 10), drugs)
  expect_null(foldMetabolites(NULL, maximum = 10))
  expect_equal(foldMetabolites(list(), maximum = 10), list())
})


# ---------------------------------------------------------------------------
# The folded time until threshold
# ---------------------------------------------------------------------------
#
# foldMetabolites() solves it again from the effect-site states of every
# contribution, because recovery times do not add.  These tests work on
# hand-built state sets, so that what is being checked is the fold's
# bookkeeping rather than any drug's kinetics; the end-to-end check against
# stopping delivery in the simulation is in test-recovery-engines.R.
#
# (Claude Code, Claude Opus 5, 2026-10-05; run on R 4.6.1.)

# A single decaying exponential, which makes the expected time arithmetic:
# amplitude A at rate k reaches target T after log(A / T) / k minutes.
decaySet <- function(times, A, k)
  recoveryStateSet(times, matrix(A * exp(-k * times), ncol = 1), k)

timeToFall <- function(A, k, target) log(A / target) / k


test_that("the fold solves recovery from the combined state", {
  k <- 0.01
  times <- c(0, 10, 20)
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = times, Cp = 3 * exp(-k * times),
                                    Ce = 3 * exp(-k * times)),
      metaboliteRecoveryStates = decaySet(times, 3, k)
    )),
    morphine = c(drugEntry("morphine", wideSeries(times, 5 * exp(-k * times),
                                                  5 * exp(-k * times))),
                 list(endCe = 1, recoveryStatesOwn = decaySet(times, 5, k)))
  )

  out <- foldMetabolites(drugs, maximum = 20, plotRecovery = TRUE)

  # Both contributions decay at the same rate here, so the combined effect site
  # is a single exponential of amplitude 8 and the answer is exact arithmetic.
  want <- timeToFall(8 * exp(-k * times), k, 1)
  expect_equal(out$morphine$wide$Recovery, want, tolerance = 1e-3)

  # Longer than either part alone, which is the whole point
  expect_gt(out$morphine$wide$Recovery[1], timeToFall(5, k, 1))
  expect_gt(out$morphine$wide$Recovery[1], timeToFall(3, k, 1))
})


test_that("the fold reports a time for a drug that was never given", {
  k <- 0.02
  times <- c(0, 25, 50)
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = times, Cp = 4 * exp(-k * times),
                                    Ce = 4 * exp(-k * times)),
      metaboliteRecoveryStates = decaySet(times, 4, k)
    )),
    morphine = c(drugEntry("morphine"), list(endCe = 1))
  )

  out <- foldMetabolites(drugs, maximum = 50, plotRecovery = TRUE)
  expect_equal(out$morphine$wide$Recovery,
               timeToFall(4 * exp(-k * times), k, 1), tolerance = 1e-3)
})


test_that("two parents feeding one metabolite are both in the time", {
  k <- 0.01
  times <- c(0, 10)
  contribution <- function(A) list(
    metaboliteName   = "morphine",
    metaboliteSeries = data.frame(Time = times, Cp = A * exp(-k * times),
                                  Ce = A * exp(-k * times)),
    metaboliteRecoveryStates = decaySet(times, A, k)
  )
  drugs <- list(
    codeine     = c(drugEntry("codeine"),     contribution(2)),
    hydrocodone = c(drugEntry("hydrocodone"), contribution(3)),
    morphine    = c(drugEntry("morphine"), list(endCe = 1))
  )

  out <- foldMetabolites(drugs, maximum = 10, plotRecovery = TRUE)
  expect_equal(out$morphine$wide$Recovery,
               timeToFall(5 * exp(-k * times), k, 1), tolerance = 1e-3)
})


test_that("the fold interpolates states onto the merged time line", {
  # The two drugs build their own time lines, so the receiving drug's states
  # have to be carried onto the union before they can be added.
  k <- 0.01
  own    <- c(0, 20)
  formed <- c(0, 10, 20)
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = formed, Cp = 3 * exp(-k * formed),
                                    Ce = 3 * exp(-k * formed)),
      metaboliteRecoveryStates = decaySet(formed, 3, k)
    )),
    morphine = c(drugEntry("morphine", wideSeries(own, 5 * exp(-k * own),
                                                  5 * exp(-k * own))),
                 list(endCe = 1, recoveryStatesOwn = decaySet(own, 5, k)))
  )

  out <- foldMetabolites(drugs, maximum = 20, plotRecovery = TRUE)
  expect_equal(out$morphine$wide$Time, formed)
  # Exact at t = 10 even though the receiving drug has no point there, because
  # the state is decayed rather than interpolated
  expect_equal(out$morphine$wide$Recovery,
               timeToFall(8 * exp(-k * formed), k, 1), tolerance = 1e-3)
})


test_that("the fold falls back on the own column when states are missing", {
  # A caller that built the drug list by hand, or a run with plotRecovery
  # FALSE, gets the old behaviour rather than a wrong one.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    morphine = c(drugEntry("morphine",
                           wideSeries(c(0, 10), c(0, 4), c(0, 2),
                                      Recovery = c(0, 7))),
                 list(endCe = 1))
  )
  expect_equal(foldMetabolites(drugs, maximum = 10,
                               plotRecovery = TRUE)$morphine$wide$Recovery,
               c(0, 7))

  # And recovery is not solved at all when it was not asked for
  withStates <- drugs
  withStates$codeine$metaboliteRecoveryStates <-
    decaySet(c(0, 10), 3, 0.01)
  withStates$morphine$recoveryStatesOwn <- decaySet(c(0, 10), 5, 0.01)
  expect_equal(foldMetabolites(withStates, maximum = 10,
                               plotRecovery = FALSE)$morphine$wide$Recovery,
               c(0, 7))
})


test_that("no threshold means no time, not an error", {
  k <- 0.01
  times <- c(0, 10)
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = times, Cp = c(0, 2), Ce = c(0, 1)),
      metaboliteRecoveryStates = decaySet(times, 3, k)
    )),
    morphine = drugEntry("morphine")          # no endCe at all
  )
  out <- foldMetabolites(drugs, maximum = 10, plotRecovery = TRUE)
  expect_equal(out$morphine$wide$Recovery, c(0, 0))
})


test_that("a metabolite drug with no effect site is timed on its plasma", {
  # Since 2026-10-07 a drug with no effect site has its time until threshold
  # timed on its PLASMA (see "Which concentration is timed" in
  # R/recoveryStates.R): that is how the antibiotics are timed against their
  # MIC.  The formed contribution then carries plasma states, and the fold
  # solves the receiving row's time from them.  Until then there were no
  # states to fold and the row read zero, which this test used to pin.
  #
  # The condition is CONSTRUCTED, which is why it lives in this file rather
  # than with the real-drug folds in test-recovery-engines.R: morphine's ke0 is
  # forced to zero, and the parent's metabolite link zeroed to match, standing
  # in for a receiving drug that has no effect site of its own.
  #
  # It used to be a real pair.  Tramadol into desmetramadol was the only one
  # whose RECEIVING drug lacked an effect site, and desmetramadol was given one
  # on 2026-10-06, so no pair in the library has the property any more.  The
  # coverage was reconstructed rather than deleted, by the active-metabolites
  # session, which made the change -- test-recovery-engines.R pins the set of
  # drugs with no effect site and says to recheck these paths rather than
  # update the number on its own, and that is what happened.
  #
  # Unlike the other tests here this one drives the real pipeline,
  # recalculatePK() into processdoseTable(), because the branch under test is
  # reached through getDrugPK's resolved metabolite rather than through a
  # hand-built drug entry.  Only the ke0 values are synthetic.
  dd <- getDrugDefaultsGlobal()
  DT <- data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO")

  drugs <- recalculatePK(NULL, dd, DT, 50, 70, 171, "male")
  # The lever the branch actually reads is the parent's metabolite ke0, which
  # advanceClosedFormMetabolite() checks to decide whether the metabolite has
  # an effect site at all.  Morphine's own ke0 is zeroed too, not because
  # anything reads it here -- morphine is never dosed in this table -- but so
  # that the constructed drug is self-consistent rather than a drug with an
  # effect site whose metabolite link says otherwise.
  for (ev in names(drugs$morphine$PK)) drugs$morphine$PK[[ev]]$ke0 <- 0
  drugs$codeine$PK$default$metabolite$ke0 <- 0
  drugs <- processdoseTable(DT, data.frame(Time = numeric(0), Event = character(0)),
                            drugs, 1440, TRUE)

  expect_equal(drugs$morphine$formedFrom, "codeine")
  expect_gt(drugs$morphine$endCe, 0)      # morphine's shipped threshold
  expect_true(all(is.na(drugs$morphine$wide$"Effect Site")))
  expect_gt(max(drugs$morphine$wide$Plasma), 0)

  # Plasma states are offered, and they add up to the formed plasma
  # concentration.
  formed <- drugs$codeine$metaboliteRecoveryStates
  expect_false(is.null(formed))
  expect_equal(rowSums(formed$state), drugs$codeine$metaboliteSeries$Cp,
               tolerance = 1e-9)
  expect_false(anyNA(drugs$morphine$wide$Recovery))

  # With the threshold at half the formed peak, the time is the time until the
  # formed plasma comes back down through it.  Checked by simulating the parent
  # to the instant the row reports and reading the formed plasma at the last
  # point, which is always the end of the run: it must be above the threshold
  # 0.02 min before that instant and below it 0.02 min after.  recoveryCalc()
  # solved to uniroot()'s 0.01 min until 2026-10-09, and to RECOVERY_TOL
  # (1e-6 min) since; the largest error measured here was 3e-4 min before
  # that change.  Checked on the way up, at the peak and on the way down.
  #
  # The reference used to be the last point of the row's own time line above
  # the threshold, with one step of that line as the tolerance.  Out there the
  # steps are 68 minutes, and timing the formed plasma 20% too fast, a
  # 52-minute error, passed.  (Claude Code, 2026-10-07, mutation review.)
  w <- drugs$morphine$wide
  drugs$morphine$endCe <- max(w$Plasma) / 2
  drugs <- processdoseTable(DT, data.frame(Time = numeric(0), Event = character(0)),
                            drugs, 1440, TRUE)
  w <- drugs$morphine$wide
  thr <- drugs$morphine$endCe
  formedAt <- function(u) {
    m <- simCpCe(DT, data.frame(Time = numeric(0), Event = character(0)),
                 drugs$codeine, u, FALSE)$metaboliteSeries
    m$Cp[nrow(m)]
  }
  peak <- which.max(w$Plasma)
  # The same curve the row plots
  expect_equal(formedAt(w$Time[peak]), w$Plasma[peak], tolerance = 1e-12)
  for (i in c(5, 10, 20, peak, peak + 5)) {
    expect_gt(w$Recovery[i], 0)
    at <- w$Time[i] + w$Recovery[i]
    expect_gt(formedAt(at - 0.02), thr,
              label = paste("formed plasma just before the instant reported at", round(w$Time[i], 1)))
    expect_lt(formedAt(at + 0.02), thr,
              label = paste("formed plasma just after the instant reported at", round(w$Time[i], 1)))
  }
  # And nothing once it is below for good
  down <- max(which(w$Plasma > thr))
  expect_true(all(w$Recovery[w$Time > w$Time[down + 1]] == 0))
})


# The union of two time lines keeps no point inside either line's pre-dose
# interval, the PRE_DOSE_OFFSET that ends at a dose, where interpolation would
# spread the dose back over the point (audit finding F19).
test_that("the fold's time line keeps out of every pre-dose interval", {
  bolusLine <- c(0, 10, 19.99, 20, 30)        # a bolus at 20
  oralLine  <- c(0, 5, 19.985, 19.995, 25)    # an oral dose at 19.995
  expect_equal(metaboliteTimeLine(list(bolusLine, oralLine)),
               c(0, 5, 10, 19.985, 20, 25, 30))
  # Elsewhere it is the plain union, and a missing line is skipped
  expect_equal(metaboliteTimeLine(list(c(0, 1, 2), c(0, 1.5, 3), NULL)),
               c(0, 1, 1.5, 2, 3))
  expect_equal(metaboliteTimeLine(list(c(0, 9.99, 10))), c(0, 9.99, 10))
})

# The audit's case.  At 19.995 the morphine row read 0.286 mcg/mL, half of a
# bolus given 0.3 seconds later, and a time until threshold of 197 minutes,
# where the morphine present (all of it formed from codeine) was 0.000535
# mcg/mL and never reaches the 0.008 mcg/mL threshold.
test_that("a dose of one contributor is not spread back onto another's point", {
  local_mocked_bindings(outputComments = function(...) {})
  dose <- data.frame(Drug = c("codeine", "codeine", "morphine"),
                     Time = c(0, 19.995, 20), Dose = c(30, 15, 10),
                     Units = c("mg PO", "mg PO", "mg"))
  events <- data.frame(Time = numeric(0), Event = character(0))
  X <- simulateDrugsWithCovariates(dose, events, 70, 170, 40, "male",
                                   maximum = 60, plotRecovery = TRUE)
  wide <- X$morphine$wide
  before <- wide[wide$Time < 20, ]
  # no point between the last one before the bolus and the bolus
  expect_true(all(wide$Time <= 19.985 | wide$Time >= 20))
  expect_true(all(before$Plasma < 0.001))
  expect_true(all(before$Recovery == 0))
  # and the bolus is all there at 20
  alone <- simulateDrugsWithCovariates(dose[dose$Drug == "codeine", ], events,
                                       70, 170, 40, "male", maximum = 60,
                                       plotRecovery = FALSE)
  given <- simulateDrugsWithCovariates(dose[dose$Drug == "morphine", ], events,
                                       70, 170, 40, "male", maximum = 60,
                                       plotRecovery = FALSE)
  # 20 is a point of the morphine-alone line; the formed curve has no dose
  # there, so it is read off its own line by interpolation
  at20 <- function(Y) stats::approx(Y$morphine$wide$Time, Y$morphine$wide$Plasma, 20)$y
  expect_equal(wide$Plasma[match(20, wide$Time)], at20(alone) + at20(given),
               tolerance = 1e-6)
  expect_gt(wide$Recovery[match(20, wide$Time)], 60)
})
