# Oral, intramuscular and intranasal doses in advanceClosedForm1(), the engine
# that switches PK set on a clinical event.
#
# Until 2026-10-07 that engine had no extravascular route: anything that was
# not a bolus was taken as an infusion rate, so "10 mg PO" ran as 10 mg/min
# until the next rate change.  No drug in the library had both a PK event and
# an extravascular route, so the drugs here are given a PK event by hand: a
# second PK set taken from a heavier patient, switched in at 50 minutes.
#
# The reference is an independent solution of the same system -- absorption
# depots feeding a three-compartment model and an effect site, amounts carried
# across the switch -- by matrix exponential, which shares no code with the
# engines.  Writing it exposed a second, older defect: convertState() kept a
# ONE-compartment model's concentration continuous across a change in V1
# instead of its amount.
#
# (Claude Code, 2026-10-07, at the request of Steven L. Shafer; run on R 4.3.3.)
#
# A mutation review the same day found that every dose here went by mouth,
# that the two PK sets never differed in ka, bioavailability or lag, and that
# no effect-site drug was absorbed across a real change in PK, so changes to
# which set supplies what, or which routes are processed, went unnoticed.  The
# reference now carries all three depots and the effect site, and the tests
# below marked "mutation review" close those gaps.
# (Claude Code, 2026-10-07; run on R 4.3.3.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

pkWith <- function(drug, weight = 70) {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK(drug, weight, 170, 50, "male", dd[dd$Drug == drug, ])
  PK$endCe <- dd$endCe[dd$Drug == drug]
  PK
}

# The drug's own PK until 50 minutes, then a 130 kg patient's.  `same` keeps
# the first set on both sides, so that only the engine changes.
switchedPK <- function(drug, same = FALSE) {
  PK <- pkWith(drug)
  PK$PK$Switch <- if (same) PK$PK$default else pkWith(drug, 130)$PK$default
  PK$pkEvents <- c(PK$pkEvents, "Switch")
  PK
}
switchAt50 <- data.frame(Time = 50, Event = "Switch")
setsOf <- function(PK) list(PK$PK$default, PK$PK$Switch)

# exp(M) by scaling and squaring a Taylor series; M is small and well scaled.
expmTaylor <- function(M) {
  s <- max(0, ceiling(log2(max(abs(M)) * 4 + 1e-300)))
  A <- M / 2^s
  E <- term <- diag(nrow(M))
  for (k in 1:20) { term <- term %*% A / k; E <- E + term }
  for (i in seq_len(s)) E <- E %*% E
  E
}

# The reference, from first principles.  State: the three depots (PO, IM, IN)
# and the central, second and third compartments, all AMOUNTS; the effect
# site, a CONCENTRATION obeying dCe/dt = ke0 (Cp - Ce) and so continuous
# across a change in V1; and a constant 1 that carries the infusion rate.
#
# Which PK set supplies what is the engine's stated contract (the header of
# R/advanceClosedForm1.R), written out again here without its code: ka and
# ke0 come from the set in force over each step, bioavailability from the set
# in force when the dose LANDS, the lag from the set in force when it is
# GIVEN, and a set with no ka for a route (ka = 0) uses the default set's
# ka, bioavailability and lag for that route.  sets[[1]] is the default set,
# sets[[2]] the one switched in at `switchAt`.
#
# Doses here are mg; `scale` converts mg/L to plotted units.
# (Extended from the plasma and the oral route alone by Claude Code,
# 2026-10-07, mutation review.)
exactRoutes <- c("PO", "IM", "IN", "SL")
# State layout: one depot per route, then central, second and third
# compartments, the effect site and the constant 1.  (SL added 2026-10-10.)
nDepot <- length(exactRoutes)
iC <- nDepot + 1; i2 <- nDepot + 2; i3 <- nDepot + 3; iE <- nDepot + 4; i1 <- nDepot + 5
nState <- nDepot + 5

exactRoute <- function(s, r, sets) {
  ka <- s[[paste0("ka_", r)]]
  if (is.null(ka) || ka <= 0) s <- sets[[1]]
  get <- function(x) if (is.null(s[[x]])) 0 else s[[x]]
  list(ka = get(paste0("ka_", r)), F = get(paste0("bioavailability_", r)),
       tlag = get(paste0("tlag_", r)))
}

exactSystem <- function(s, R, sets) {
  A <- matrix(0, nState, nState)
  for (j in seq_len(nDepot)) {
    ka <- exactRoute(s, exactRoutes[j], sets)$ka
    A[j, j]  <- -ka
    A[iC, j] <-  ka
  }
  A[iC, iC] <- -(s$k10 + s$k12 + s$k13)
  A[iC, i2] <-  s$k21; A[i2, iC] <- s$k12; A[i2, i2] <- -s$k21
  A[iC, i3] <-  s$k31; A[i3, iC] <- s$k13; A[i3, i3] <- -s$k31
  A[iE, iC] <-  s$ke0 / s$v1; A[iE, iE] <- -s$ke0
  A[iC, i1] <-  R
  A
}

# Cp and Ce at `times`, and the whole state there.
exactRun <- function(DT, sets, switchAt, times, weight = 70, scale = 1) {
  setAt  <- function(t) if (t < switchAt) sets[[1]] else sets[[2]]
  perKg  <- ifelse(grepl("kg", DT$Units), weight, 1)
  isRate <- grepl("hr", DT$Units)
  route  <- vapply(DT$Units, function(u) {
    r <- exactRoutes[vapply(exactRoutes, grepl, logical(1), x = u)]
    if (length(r) == 0) "" else r
  }, character(1), USE.NAMES = FALSE)
  lands <- DT$Time
  for (d in which(route != ""))
    lands[d] <- DT$Time[d] + exactRoute(setAt(DT$Time[d]), route[d], sets)$tlag
  rateAt <- function(t) {
    i <- which(isRate & DT$Time <= t)
    if (length(i) == 0) 0 else DT$Dose[max(i)] * perKg[max(i)] / 60
  }
  breaks <- sort(unique(c(0, DT$Time, lands, switchAt, times)))
  x <- c(rep(0, nState - 1), 1)
  n <- length(times)
  out <- list(Cp = numeric(n), Ce = numeric(n), state = matrix(0, n, nState))
  for (k in seq_along(breaks)) {
    t <- breaks[k]
    s <- setAt(t)
    for (d in which(lands == t & !isRate)) {
      amount <- DT$Dose[d] * perKg[d]
      if (route[d] == "") {
        x[iC] <- x[iC] + amount
      } else {
        j <- match(route[d], exactRoutes)
        x[j] <- x[j] + amount * exactRoute(s, route[d], sets)$F
      }
    }
    for (i in which(times == t)) {
      out$Cp[i] <- x[iC] / s$v1 * scale
      out$Ce[i] <- x[iE] * scale
      out$state[i, ] <- x
    }
    if (k < length(breaks))
      x <- as.vector(expmTaylor(exactSystem(s, rateAt(t), sets) * (breaks[k + 1] - t)) %*% x)
  }
  out
}

exactCp <- function(...) exactRun(...)$Cp

# Time until threshold by its definition, on the exact solution: give nothing
# after t, stop any infusion, let the depots go on draining, and find when the
# effect site (site = "Ce") or the plasma (site = "Cp") comes down through
# `thr` for the last time, with the PK in force at t throughout -- the
# engine's own assumption, so it is only compared after the last change in
# PK.  Stepped by one matrix exponential every `h` minutes out to `horizon`,
# then refined by uniroot() to far below anything asserted.  Meaningless at a
# time when a dose has been given but has not begun to be absorbed.
exactRecovery <- function(DT, sets, switchAt, t, thr, site = "Ce", scale = 1,
                          horizon = MINS_PER_DAY, h = 1, weight = 70) {
  x0 <- exactRun(DT[DT$Time <= t, , drop = FALSE], sets, switchAt, t,
                 weight = weight)$state[1, ]
  x0[i1] <- 0
  s <- if (t < switchAt) sets[[1]] else sets[[2]]
  A <- exactSystem(s, 0, sets)
  read <- function(x) scale * if (site == "Ce") x[iE] else x[iC] / s$v1
  E <- expmTaylor(A * h)
  X <- matrix(0, ceiling(horizon / h) + 1, nState)
  X[1, ] <- x0
  for (k in seq_len(nrow(X) - 1)) X[k + 1, ] <- E %*% X[k, ]
  above <- which(apply(X, 1, read) > thr)
  if (length(above) == 0) return(0)
  k <- max(above)
  if (k == nrow(X)) return(horizon)
  (k - 1) * h + stats::uniroot(function(u) read(expmTaylor(A * u) %*% X[k, ]) - thr,
                               c(0, h), tol = 1e-9)$root
}

# The engine's time until threshold against exactRecovery(), at every point of
# its own time line from `from` on, so nothing is interpolated.  `from` must be
# at or after the switch, and after any lag window.  `tolerance` in minutes.
expectRecoveryExact <- function(w, DT, sets, thr, site, tolerance, from = 50,
                                scale = 1, label = "") {
  rows <- which(w$Time >= from)
  expect_false(anyNA(w$Recovery[rows]), label = paste(label, "recovery missing"))
  err <- vapply(rows, function(i)
    w$Recovery[i] - exactRecovery(DT, sets, 50, w$Time[i], thr, site, scale),
    numeric(1))
  expect_lt(max(abs(err)), tolerance,
            label = paste(label, "largest difference in time until threshold, minutes"))
  invisible(err)
}

# Engine output at its own time points, so nothing is interpolated.
engineCp <- function(sim, from = 0) {
  w <- sim$wide
  w[w$Time >= from, c("Time", "Plasma")]
}

# Largest relative error over the points where the reference is not
# vanishingly small.
relErr <- function(got, ref, floor = 1e-6) {
  keep <- ref > max(ref) * floor
  max(abs(got[keep] - ref[keep]) / ref[keep])
}

# Tolerances used below, from what was measured on R 4.3.3:
#
#  - Plasma: exact, measured 3e-13 relative; asserted at 1e-8.
#  - Effect site: advanceClosedForm1() derives it from the plasma curve with
#    calculateCe(), an approximation (plasma linear or log-linear within each
#    step), unlike the other engines, which carry its exponential states.
#    The error is largest in the first minute after a dose (8%) and decays;
#    from the switch on it measured at most 9e-4 relative for hydromorphone,
#    so the effect site is compared from the switch on, at 3e-3.
#  - Time until threshold timed on the PLASMA (no effect site) is exact but
#    for recoveryCalc()'s root tolerance, 0.01 min when this was written
#    (RECOVERY_TOL, 1e-6 min, since 2026-10-09): measured at most 2.4e-3 min;
#    asserted at 0.02 min.
#  - Timed on the EFFECT SITE it inherits calculateCe()'s error, damped over
#    the hours to the crossing: measured at most 0.016 min after a switch,
#    0.019 with ka on an eigenvalue and 0.024 just after a lagged dose lands;
#    asserted at 0.05 min.


# Hydromorphone's oral doses below are multiplied by HM_PO, so that each one
# puts into the circulation exactly what it did when hydromorphone's oral
# bioavailability was 0.6.  These tests check the engines, not that
# calibration, and their tolerances were measured for effect-site
# concentrations well above the threshold; at the recalibrated 0.225
# (Lohela 2021, 2026-10-10) the same milligrams sit near it, where time until
# threshold is less well conditioned.  The engine is linear, so the scaled
# doses reproduce the original concentrations exactly.
HM_PO <- 0.6 / 0.225

test_that("an oral, intramuscular or intranasal dose is absorbed, not infused, across a change in PK", {
  # Each route of hydromorphone has its own ka and bioavailability (PO 0.01 /
  # 0.225, IM 0.0128 / 1, IN 0.0149 / 0.55), so a route that borrowed another's
  # absorption would show.  Until the mutation review only "mg PO" was run.
  cases <- list(c("clindamycin", "PO"), c("hydromorphone", "PO"),
                c("hydromorphone", "IM"), c("hydromorphone", "IN"))
  for (case in cases) {
    drug <- case[1]
    unit <- paste("mg", case[2])
    scale <- if (drug == "hydromorphone") 1000 else 1      # ng/mL from mg/L
    mg    <- if (drug == "hydromorphone") c(4, 1, 0.003, 3, 0) else c(600, 300, 0.5, 450, 0)
    DT <- data.frame(Drug = drug, Time = c(0, 20, 30, 50, 100), Dose = mg,
                     Units = c(unit, "mg", "mg/kg/hr", unit, "mg/kg/hr"))
    PK  <- switchedPK(drug)
    sim <- simCpCe(DT, switchAt50, PK, 300, FALSE)
    got <- engineCp(sim)
    ref <- exactCp(DT, setsOf(PK), 50, got$Time, scale = scale)
    # Exact, at every point including the switch.  (The step into a switch
    # used to decay states with the new set's eigenvalues, a 2e-4 error that
    # persisted; fixed 2026-10-07.)
    expect_lt(relErr(got$Plasma, ref), 1e-8,
              label = paste(drug, unit, "largest relative error against the exact solution"))
  }
})


test_that("two depots at once, and a third route at the switch, are all absorbed", {
  # Oral and intramuscular together at the start, intranasal exactly at the
  # switch: three depots draining at three rates, each carried across the
  # change in PK.  A route left unprocessed once another has been would show
  # here.  (Claude Code, 2026-10-07, mutation review.)
  PK <- switchedPK("hydromorphone")
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 0, 50), Dose = c(4, 1, 2),
                   Units = c("mg PO", "mg IM", "mg IN"))
  w   <- simCpCe(DT, switchAt50, PK, 300, FALSE)$wide
  ref <- exactRun(DT, setsOf(PK), 50, w$Time, scale = 1000)
  expect_lt(relErr(w$Plasma, ref$Cp), 1e-8, label = "plasma, largest relative error")
  after <- w$Time >= 50
  expect_lt(max(abs(w$"Effect Site"[after] / ref$Ce[after] - 1)), 3e-3,
            label = "effect site from the switch on, largest relative error")
})


test_that("ka and bioavailability come from the set in force over the step and when the dose lands", {
  # Until the mutation review both PK sets always carried the same ka and F,
  # so nothing showed which set supplies them.  Here the switched set absorbs
  # three times as fast and has half the bioavailability, with one dose before
  # the switch and one exactly at it.  The first dose must drain at the old ka
  # up to 50 minutes (including the 0.01-minute step into the switch) and at
  # the new ka after; the second must take the NEW set's F; and the time until
  # threshold at the switch instant itself must assume the new ka.
  # (Claude Code, 2026-10-07, mutation review.)
  PK <- switchedPK("clindamycin")
  PK$PK$Switch$ka_PO <- 3 * PK$PK$default$ka_PO
  PK$PK$Switch$bioavailability_PO <- 0.5
  DT <- data.frame(Drug = "clindamycin", Time = c(0, 50), Dose = 600, Units = "mg PO")
  w <- simCpCe(DT, switchAt50, PK, 480, TRUE)$wide
  expect_lt(relErr(w$Plasma, exactCp(DT, setsOf(PK), 50, w$Time)), 1e-8,
            label = "plasma, largest relative error")
  expect_true(50 %in% w$Time)
  expect_gt(w$Recovery[w$Time == 50], 60)
  expectRecoveryExact(w, DT, setsOf(PK), PK$endCe, "Cp", 0.02,
                      label = "clindamycin, switched ka and F")
})


test_that("with the same PK on both sides of an event it matches the oral engine", {
  for (drug in c("hydromorphone", "clindamycin", "cefalexin")) {
    po <- "mg PO"
    DT <- data.frame(Drug = drug, Time = c(0, 90, 240), Dose = c(4, 4, 4) * HM_PO, Units = po)
    if (drug != "hydromorphone") DT$Dose <- c(500, 500, 500)
    if (drug == "hydromorphone")
      DT <- rbind(DT, data.frame(Drug = drug, Time = 20, Dose = 1, Units = "mg"))
    PK <- pkWith(drug)
    if (PK$endCe == 0) PK$endCe <- 2         # no default threshold yet: time something
    one <- simCpCe(DT, noEvents, PK, 480, TRUE)

    PK2 <- switchedPK(drug, same = TRUE)
    PK2$endCe <- PK$endCe
    two <- simCpCe(DT, switchAt50, PK2, 480, TRUE)

    a <- one$wide; b <- two$wide
    t <- intersect(a$Time, b$Time)
    expect_equal(b$Plasma[match(t, b$Time)], a$Plasma[match(t, a$Time)],
                 tolerance = 1e-10, label = paste(drug, "plasma"))
    # Time until threshold.  The effect site in this engine comes from
    # calculateCe(), an approximation the oral engine no longer uses, so for
    # hydromorphone the times agree closely rather than exactly; the plotted
    # grid also interpolates across two different time lines.  The largest
    # difference measured was 0.008 min (hydromorphone; cefalexin 0.007 from
    # the interpolation alone), so 0.03 min.  It was 1 min, which a 125-fold
    # error would have passed.  (Tightened 2026-10-07, mutation review.)
    expect_lt(max(abs(one$equiSpace$Recovery - two$equiSpace$Recovery)), 0.03,
              label = paste(drug, "time until threshold, minutes"))
    # At the points the two time lines share nothing is interpolated: exact
    # for the drugs timed on their plasma (measured 3e-13 min).
    if (drug != "hydromorphone")
      expect_lt(max(abs(b$Recovery[match(t, b$Time)] - a$Recovery[match(t, a$Time)])), 1e-6,
                label = paste(drug, "time until threshold at shared points, minutes"))
  }
})


test_that("time until threshold counts drug still being absorbed after a change in PK", {
  # Clindamycin is timed on its plasma (no effect site).  Checked from the
  # switch on, so that "the PK in force now" is also the PK the reference goes
  # on using, at every point of the engine's own time line against the exact
  # solution.  (Until the mutation review this was checked against the engine
  # itself, interpolated log-linearly across its sparse late points, to half a
  # minute.)  Two thresholds: 2 mg/L, which both doses cross, and 6 mg/L, which
  # only the second does.
  PK <- switchedPK("clindamycin")
  DT <- data.frame(Drug = "clindamycin", Time = c(0, 120), Dose = c(600, 600),
                   Units = "mg PO")
  for (thr in c(2, 6)) {
    PK$endCe <- thr
    w <- simCpCe(DT, switchAt50, PK, 480, TRUE)$wide
    expectRecoveryExact(w, DT, setsOf(PK), thr, "Cp", 0.02,
                        label = paste("clindamycin, threshold", thr))
  }
  # Straight after the second dose the plasma is still below 6 mg/L, but the
  # drug in the gut is going to take it over the threshold: a time, not zero.
  # Until 2026-10-07 this was asserted at a threshold of 2 mg/L, which the
  # plasma (4 to 4.6 mg/L there) was already above, so it held whether or not
  # the depot was counted.  (Mutation review.)
  early <- w[w$Time > 120 & w$Time <= 130, ]
  expect_true(all(early$Plasma < PK$endCe))
  expect_gt(nrow(early), 5)
  expect_true(all(early$Recovery > 60))
  # Just before it, at the same plasma, the first dose's remnant in the gut
  # cannot take it to 6 mg/L: none.
  expect_equal(w$Recovery[max(which(w$Time < 120))], 0)
})


test_that("an absorption lag is applied, and masked, across a change in PK", {
  PK <- switchedPK("clindamycin")
  PK$endCe <- 2
  PK$PK$Switch$tlag_PO <- 30          # the set in force when the dose is given
  DT <- data.frame(Drug = "clindamycin", Time = 60, Dose = 600, Units = "mg PO")
  sim <- simCpCe(DT, switchAt50, PK, 240, TRUE)
  w <- sim$wide
  expect_true(all(w$Plasma[w$Time < 90] == 0))
  expect_gt(w$Plasma[w$Time > 95][1], 0)
  # Given but not yet absorbing: no time, rather than zero
  expect_true(all(is.na(w$Recovery[w$Time >= 60 & w$Time < 90])))
  expect_false(anyNA(w$Recovery[w$Time >= 90]))
})


test_that("an absorption lag is taken from the set in force when the dose is GIVEN", {
  # The test above puts the lag in the set that is in force both when the dose
  # is given and from then on, so it cannot tell "the set in force when given"
  # from "the last set" or "the set in force when it lands".  Here the default
  # set has a 30-minute lag and the switched set none, and the second dose is
  # given at 40 minutes, under the default set, and so lands at 70, after the
  # switch.  (Claude Code, 2026-10-07, mutation review.)
  PK <- switchedPK("clindamycin")
  PK$endCe <- 2
  PK$PK$default$tlag_PO <- 30
  PK$PK$Switch$tlag_PO  <- 0
  DT <- data.frame(Drug = "clindamycin", Time = c(0, 40), Dose = 600, Units = "mg PO")
  w <- simCpCe(DT, switchAt50, PK, 480, TRUE)$wide
  expect_true(all(w$Plasma[w$Time < 30] == 0))
  expect_lt(relErr(w$Plasma, exactCp(DT, setsOf(PK), 50, w$Time)), 1e-8,
            label = "plasma, largest relative error")
  # Not computable from exactly the instant each dose is given until it starts
  # absorbing, and computable everywhere else -- including the point just
  # before the second dose is given, when the first is in and above.
  pending <- w$Time < 30 | (w$Time >= 40 & w$Time < 70)
  expect_true(all(c(40, 70) %in% w$Time))
  expect_true(all(is.na(w$Recovery[pending])))
  expect_false(anyNA(w$Recovery[!pending]))
  expect_gt(w$Recovery[max(which(w$Time < 40))], 60)
  expectRecoveryExact(w, DT, setsOf(PK), 2, "Cp", 0.02, from = 70,
                      label = "clindamycin, after the lagged dose lands")
})


test_that("an effect-site drug absorbed across a real change in PK", {
  # Hydromorphone's effect site, after oral and intramuscular doses, across a
  # switch to a heavier patient's disposition (and so a different ke0), with a
  # dose exactly at the switch and one after it.  Until the mutation review no
  # effect-site drug was absorbed across a real change in PK at all.  The
  # effect site, and the time until it falls to the threshold, are compared
  # with the exact solution from the switch on, within the tolerances measured
  # above for calculateCe().  (Claude Code, 2026-10-07.)
  PK <- switchedPK("hydromorphone")
  for (route in c("PO", "IM")) {
    DT <- data.frame(Drug = "hydromorphone", Time = c(0, 50, 120),
                     Dose = if (route == "PO") c(4, 2, 2) * HM_PO else c(2, 1, 1),
                     Units = paste("mg", route))
    w   <- simCpCe(DT, switchAt50, PK, 300, TRUE)$wide
    ref <- exactRun(DT, setsOf(PK), 50, w$Time, scale = 1000)
    expect_lt(relErr(w$Plasma, ref$Cp), 1e-8, label = paste(route, "plasma"))
    after <- w$Time >= 50
    expect_lt(max(abs(w$"Effect Site"[after] / ref$Ce[after] - 1)), 3e-3,
              label = paste(route, "effect site from the switch on, largest relative error"))
    expect_gt(min(w$Recovery[after]), 60)
    expectRecoveryExact(w, DT, setsOf(PK), PK$endCe, "Ce", 0.05, scale = 1000,
                        label = paste("hydromorphone", route))
  }
})


test_that("ka equal to an eigenvalue of the switched set does not wreck the time until threshold", {
  # The depot's term in the recovery states divides by lambda_i - ka, so the
  # engine nudges ka by a part in a million when they coincide.  Here the
  # switched set's ka_PO is set to its own lambda_2 and the answer is compared
  # with the exact solution, which has no such singularity.
  # (Claude Code, 2026-10-07, mutation review.)
  PK <- switchedPK("hydromorphone")
  PK$PK$Switch$ka_PO <- PK$PK$Switch$lambda_2
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 60), Dose = 4 * HM_PO, Units = "mg PO")
  w <- simCpCe(DT, switchAt50, PK, 300, TRUE)$wide
  expect_lt(relErr(w$Plasma, exactCp(DT, setsOf(PK), 50, w$Time, scale = 1000)), 1e-8,
            label = "plasma, largest relative error")
  expectRecoveryExact(w, DT, setsOf(PK), PK$endCe, "Ce", 0.05, scale = 1000,
                      label = "hydromorphone, ka on lambda_2")
})


test_that("a lagged dose of an effect-site drug is masked across a change in PK", {
  # The effect-site branch of the engine's recovery carries its own mask; the
  # lag tests above are all on clindamycin, which takes the plasma branch.  An
  # oral dose at the start keeps the time nonzero, so that an unmasked answer
  # would be a plausible number rather than zero.
  # (Claude Code, 2026-10-07, mutation review.)
  PK <- switchedPK("hydromorphone")
  PK$PK$Switch$tlag_IM <- 15
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 70), Dose = c(4 * HM_PO, 1),
                   Units = c("mg PO", "mg IM"))
  w <- simCpCe(DT, switchAt50, PK, 300, TRUE)$wide
  expect_lt(relErr(w$Plasma, exactCp(DT, setsOf(PK), 50, w$Time, scale = 1000)), 1e-8,
            label = "plasma, largest relative error")
  pending <- w$Time >= 70 & w$Time < 85
  expect_true(all(c(70, 85) %in% w$Time))
  expect_true(all(is.na(w$Recovery[pending])))
  expect_false(anyNA(w$Recovery[!pending]))
  expect_gt(w$Recovery[max(which(w$Time < 70))], 60)
  expectRecoveryExact(w, DT, setsOf(PK), PK$endCe, "Ce", 0.05, scale = 1000, from = 85,
                      label = "hydromorphone, after the lagged dose lands")
})


test_that("convertState carries a one-compartment AMOUNT across a change in V1", {
  old <- list(lambda_2 = 0, v1 = 10)
  new <- list(lambda_2 = 0, v1 = 25)
  expect_equal(convertState(c(5, 0, 0), old, new), c(2, 0, 0))
  # Same volume, nothing changes
  expect_equal(convertState(c(5, 0, 0), old, old), c(5, 0, 0))
})


test_that("depotInput is continuous through lambda == ka", {
  ka <- 0.05; G <- 100; dt <- 3
  exact <- depotInput(0.05, ka, G, dt)
  expect_equal(exact, ka * G * dt * exp(-ka * dt))
  expect_equal(depotInput(0.05 * (1 + 1e-7), ka, G, dt), exact, tolerance = 1e-6)
  expect_equal(depotInput(0.2, ka, 0, dt), 0)
  expect_equal(depotInput(0.2, 0, G, dt), 0)
})


test_that("an event set that leaves a route out keeps the default set's absorption", {
  # Same disposition on both sides, but the event set carries no oral route:
  # the gut must go on absorbing as before, not freeze.
  PK <- switchedPK("clindamycin", same = TRUE)
  PK$PK$Switch$ka_PO <- 0
  PK$PK$Switch$bioavailability_PO <- 0
  DT <- data.frame(Drug = "clindamycin", Time = c(0, 60), Dose = 600, Units = "mg PO")
  a <- simCpCe(DT, noEvents, pkWith("clindamycin"), 300, FALSE)$wide
  b <- simCpCe(DT, switchAt50, PK, 300, FALSE)$wide
  t <- intersect(a$Time, b$Time)
  expect_equal(b$Plasma[match(t, b$Time)], a$Plasma[match(t, a$Time)], tolerance = 1e-10)

  # Its lag too: a stray tlag on a set without the route is not applied.  The
  # 60-minute dose is given under the event set and must start at once.
  PK$PK$Switch$tlag_PO <- 45
  c2 <- simCpCe(DT, switchAt50, PK, 300, FALSE)$wide
  t <- intersect(a$Time, c2$Time)
  expect_equal(c2$Plasma[match(t, c2$Time)], a$Plasma[match(t, a$Time)], tolerance = 1e-10)
})


test_that("ka equal to ke0 does not wreck the time until threshold", {
  PK <- switchedPK("hydromorphone", same = TRUE)
  for (ev in names(PK$PK)) PK$PK[[ev]]$ka_PO <- PK$PK[[ev]]$ke0
  DT <- data.frame(Drug = "hydromorphone", Time = 0, Dose = 8, Units = "mg PO")
  w <- simCpCe(DT, switchAt50, PK, 480, TRUE)$wide
  expect_true(all(is.finite(w$Recovery)))
  expect_gt(max(w$Recovery), 60)
})


test_that("a dose its lag pushes past the end of the run leaves no stray points", {
  PK <- switchedPK("clindamycin")
  PK$PK$Switch$tlag_PO <- 30
  PK$endCe <- 2
  DT <- data.frame(Drug = "clindamycin", Time = c(0, 380), Dose = 600, Units = "mg PO")
  w <- simCpCe(DT, switchAt50, PK, 400, TRUE)$wide
  expect_lte(max(w$Time), 400)
  expect_gt(w$Plasma[nrow(w)], 0)
})


# Buprenorphine scales each sublingual dose (mg) by its own fraction absorbed
# before the engine sees it (sublingualSaturation); the reference is given the
# scaled doses, so that what is compared is the engine.
slScaled <- function(DT, PK) {
  sl <- grepl(" SL", DT$Units)
  DT$Dose[sl] <- DT$Dose[sl] * oralSaturationFraction(DT$Dose[sl], PK$sublingualSaturation)
  DT
}

test_that("a sublingual dose is absorbed across the switch like the other routes", {
  # The sublingual route (buprenorphine; added 2026-10-10) against the
  # matrix-exponential reference, given before and exactly at the switch, with
  # an intranasal dose alongside so two depots drain at once.
  PK <- switchedPK("buprenorphine")
  DT <- data.frame(Drug = "buprenorphine", Time = c(0, 50, 20), Dose = c(8, 4, 0.3),
                   Units = c("mg SL", "mg SL", "mg IN"))
  w   <- simCpCe(DT, switchAt50, PK, 600, FALSE)$wide
  ref <- exactRun(slScaled(DT, PK), setsOf(PK), 50, w$Time, scale = 1000)
  expect_lt(relErr(w$Plasma[w$Time > 0], ref$Cp[w$Time > 0]), 1e-8,
            label = "plasma, largest relative error")
})

test_that("with one PK set the sublingual route matches the reference exactly", {
  PK <- switchedPK("buprenorphine", same = TRUE)
  DT <- data.frame(Drug = "buprenorphine", Time = c(0, 30), Dose = c(16, 2),
                   Units = c("mg SL", "mg"))
  single <- simCpCe(DT, noEvents, pkWith("buprenorphine"), 600, FALSE)$wide
  ref <- exactRun(slScaled(DT, PK), setsOf(PK), 50, single$Time, scale = 1000)
  expect_lt(relErr(single$Plasma[single$Time > 0], ref$Cp[single$Time > 0]), 1e-8)
  expect_lt(relErr(single$"Effect Site"[single$Time > 0], ref$Ce[single$Time > 0]), 1e-8)
})
