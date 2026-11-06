# -----------------------------------------------------------------------------
# Exact linear engine: a biliary store that fills, then empties once
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, for
# oxaliplatin (R/drugs_oxaliplatin.R; Bastian, Barrail & Urien 2003), whose
# enterohepatic recirculation is a gallbladder store filled from the central
# compartment and emptied, once, as a bolus.  Shafer decided (2026-11-06) to
# implement the model AS PUBLISHED: the store fills only until TLAG.
# Verified by tests/testthat/test-drugs-oxaliplatin.R against an independent
# fine-step Runge-Kutta integration of the same equations.
#
# THE MODEL
# =========
# For each course (see below), with amounts A1 (central), A2 and A3 (the PK
# set's peripheral compartments, where it has them), B (the store: the
# gallbladder) and G (the gut):
#
#   before the course's start + tlag ("filling"):
#     dA1/dt = R(t) - (k10 + k12 + k13 + k1B) A1 + k21 A2 + k31 A3
#     dB/dt  = k1B A1                          (the store only fills)
#   at the course's start + tlag:  G <- G + B,  B <- 0   (the bolus emptying)
#   after it ("emptied"):
#     dA1/dt = R(t) - (k10 + k12 + k13) A1 + k21 A2 + k31 A3 + ka G
#     dG/dt  = -ka G                           (nothing more is stored)
#
# with the peripheral equations the usual mammillary ones.  k10 is the PK
# set's (the clearance alone); k1B, tlag and ka are the block's.
#
# WHY THE STORE STOPS FILLING
# ===========================
# Bastian 2003 cites Plusquellec & Houin 1992 (J Biomed Eng 14:521-526) for
# its integrated equations.  In Plusquellec's model the transfer into the
# store is continuous -- K1 = k10 + k12 + k1B in their equation (5) holds for
# all t -- and the store is emptied at each of n chosen times T1..Tn; after
# the LAST emptying it goes on filling and is never emptied again, so with
# one emptying it is a sink and AUC(0-inf) falls below Dose/CL (their AUC
# expression).  Bastian's own numbers contradict that reading: their AUC
# example and Table 4 take AUC = Dose/CL, and their terminal half-lives (10.5
# h in men, 14.4 h in women) are the two-compartment system's alone (10.3 and
# 15.5 h at the typical values), which is what a store that stops filling at
# TLAG gives.  Read as a sink, the terminal half-life would be 4.4 h and the
# area 35% of Dose/CL.  So the model the paper's results describe fills until
# TLAG and then stops; Shafer decided (2026-11-06) to implement that.
#
# COURSES, AND WHY THE RESPONSES ADD
# ==================================
# The switch is fixed in time relative to a dose, so the system is linear but
# not time-invariant, and one switch cannot serve a dose table.  Each dose
# therefore carries its own store and its own switch: a COURSE is an
# intravenous bolus, or an infusion episode -- from a rate row above zero
# that follows no rate (or a zero rate) until the next zero rate -- and its
# switch is at the episode's START plus tlag (Bastian measures TLAG from the
# start of the infusion; Plusquellec's emptying times are from the start of
# the infusion).  A change of rate within an episode does not start a new
# course.  Each course is simulated as its own system and the courses are
# added: the model is linear in the dose, so the response to several courses
# is the sum of each one's response.  For a single course, which is the
# paper's whole design (one course every two or three weeks), this is exactly
# Bastian's model; for overlapping courses it is the natural extension, each
# dose's drug stored and released on that dose's own clock.
#
# A consequence a user will see: a 4-h infusion of the same dose peaks HIGHER
# than a 2-h one (1.75 against 1.38 mcg/mL for 230 mg in the typical man),
# because the store stops taking drug at 2 h, half-way through the longer
# infusion.  That is the published structure, not an error of this engine.
#
# HOW THE ADVANCE IS DONE
# =======================
# As in advanceTransitChain(): between grid points the matrix is constant, so
# the exact advance is x(t + dt) = E(dt) x(t) + R g(dt), with E and g taken
# from one matrix exponential of the system augmented by a state held at 1
# (expmScaleSquare()), memoised on the phase and the step length.  Every
# course's switch time is a point of the time line, so no step straddles a
# switch.  The courses' states are carried side by side as the columns of
# one matrix, so a step is two matrix products whatever the number of
# courses.  Exact to rounding: there is no step-size error.
#
# TIME UNTIL THRESHOLD
# ====================
# After every course has emptied its store the system is time-invariant (the
# "emptied" matrix), and the summed state's future without further input is
# c' exp(A s) x, which transitChainRecovery() solves exactly.  Before the
# last switch the future depends on switches still to come, which that solver
# cannot carry, so the time until threshold is NA there, as it is inside an
# oral lag.  Oxaliplatin has no threshold, so for it the column is zero.
# -----------------------------------------------------------------------------


#' Validate a model's biliaryStore block
#'
#' @param bs the block a drug model returned, or NULL: `k1B` (the transfer
#'   from the central compartment into the store), `tlag` (minutes from the
#'   start of a course to the store's emptying) and `ka` (the return from the
#'   gut to the central compartment), the rate constants per minute
#' @param drug the drug's name, for the error message
#' @returns `bs`, unchanged, if valid
#' @keywords internal
validateBiliaryStore <- function(bs, drug)
{
  if (is.null(bs)) return(NULL)
  positive <- function(v) is_valid_number(v) && is.finite(v) && v > 0
  if (!is.list(bs) || !positive(bs$k1B) || !positive(bs$tlag) || !positive(bs$ka))
    stop("Invalid biliaryStore for ", drug, ": needs a positive k1B and ka ",
         "(per minute) and a positive tlag (minutes).")
  bs
}


#' The courses of a biliary-store drug's dose table
#'
#' @param t0 dose times, minutes
#' @param D doses in the engine's units (amounts, or amounts per minute)
#' @param bolus,rate which rows are intravenous boluses and rates
#' @returns a data frame of `start`, `end` (Inf for a bolus or an infusion
#'   that never stops; a bolus's rate is zero anyway) and `bolus` (the
#'   course's bolus amount, 0 for an infusion), one row per course
#' @keywords internal
biliaryStoreCourses <- function(t0, D, bolus, rate)
{
  courses <- data.frame(start = t0[bolus & D > 0], end = t0[bolus & D > 0],
                        bolus = D[bolus & D > 0])
  if (any(rate))
  {
    o <- order(t0[rate])
    tr <- t0[rate][o]
    dr <- D[rate][o]
    # The last row at each time is the one in force (rateIn() reads it so).
    keep <- !duplicated(tr, fromLast = TRUE)
    tr <- tr[keep]; dr <- dr[keep]
    on <- dr > 0
    starts <- which(on & c(TRUE, !on[-length(on)]))
    for (s in starts)
    {
      stop <- which(!on & seq_along(on) > s)
      end <- if (length(stop)) tr[stop[1]] else Inf
      courses <- rbind(courses, data.frame(start = tr[s], end = end, bolus = 0))
    }
  }
  courses[order(courses$start), , drop = FALSE]
}


#' Advance a biliary-store drug, exactly, by matrix exponential
#'
#' @param dose the classified dose table `simCpCe()` builds, in the engine's
#'   amount unit
#' @param PK the whole resolved PK, for `PK$biliaryStore` and the default set
#' @param maximum the end of the plot, minutes
#' @param plotRecovery should the time until threshold be computed?  Zeros
#'   otherwise, as the closed-form engines give
#' @param emerge the threshold, on the plasma (the drug has no effect site)
#' @returns a data frame of `Time`, `Cp`, `Ce` (NA: no effect site),
#'   `Recovery`, `C2`, `C3` and `Depot` (the drug in the store and the gut,
#'   still to return), as the other engines return
#' @keywords internal
advanceBiliaryStore <- function(dose, PK, maximum, plotRecovery = FALSE,
                                emerge = 0)
{
  bs <- PK$biliaryStore
  pkSet <- PK$PK[[PK_EVENT_DEFAULT]]
  orZero <- function(v, zero = 0) if (is.null(v) || is.na(v)) zero else v
  V   <- pkSet$v1
  k10 <- pkSet$k10
  k12 <- orZero(pkSet$k12); k21 <- orZero(pkSet$k21)
  k13 <- orZero(pkSet$k13); k31 <- orZero(pkSet$k31)
  V2  <- orZero(pkSet$v2, 1); V3 <- orZero(pkSet$v3, 1)

  # ---- State layout: central, the peripherals it has, store, gut ----
  iC <- 1L
  i2 <- if (k12 > 0) 2L else NA_integer_
  i3 <- if (k13 > 0) 2L + (k12 > 0) else NA_integer_
  iB <- 2L + (k12 > 0) + (k13 > 0)
  iG <- iB + 1L
  nS <- iG
  iOne <- nS + 1L

  system <- function(filling) {
    A <- matrix(0, iOne, iOne)
    kB <- if (filling) bs$k1B else 0
    A[iC, iC] <- -(k10 + k12 + k13 + kB)
    if (k12 > 0) { A[iC, i2] <- k21; A[i2, iC] <- k12; A[i2, i2] <- -k21 }
    if (k13 > 0) { A[iC, i3] <- k31; A[i3, iC] <- k13; A[i3, i3] <- -k31 }
    A[iB, iC] <- kB
    A[iG, iG] <- -bs$ka
    A[iC, iG] <- bs$ka
    A[iC, iOne] <- 1            # a unit intravenous rate
    A
  }
  Afill  <- system(TRUE)
  Aempty <- system(FALSE)

  # ---- The inputs ----
  units <- as.character(dose$Units)
  isRate <- isRateUnit(units)
  route <- doseRoute(units)
  t0 <- as.numeric(dose$Time)
  D  <- as.numeric(dose$Dose)
  ivBolus <- route == ROUTE_IV & !isRate
  ivRate  <- route == ROUTE_IV & isRate
  other <- !(ivBolus | ivRate)
  if (any(other & D != 0))
    stop("The biliary-store engine takes intravenous doses only: ",
         paste(unique(units[other & D != 0]), collapse = ", "))

  courses <- biliaryStoreCourses(t0, D, ivBolus, ivRate)
  nC <- nrow(courses)
  switchAt <- courses$start + bs$tlag

  # ---- The time line ----
  # Every dose time and every switch, and the instant before each, so that
  # neither a jump nor a change of matrix lands inside a step.
  knots <- unique(c(t0, switchAt))
  timeLine <- simulationTimeGrid(c(knots, knots - PRE_DOSE_OFFSET), maximum,
                                 gridStart(0))
  timeLine <- timeLine[timeLine >= 0]
  L <- length(timeLine)
  if (any(is.na(match(switchAt[switchAt <= maximum], timeLine))) ||
      any(is.na(match(t0[t0 <= maximum], timeLine))))
    stop("Internal error: a dose or switch time is missing from the time line.")

  # The rate in force at each point, the last rate row at or before it, and
  # each course's share of it: the part inside its own episode.
  rateLine <- if (!any(ivRate)) rep(0, L) else {
    o <- order(t0[ivRate])
    c(0, D[ivRate][o])[findInterval(timeLine, t0[ivRate][o]) + 1]
  }
  courseRate <- matrix(0, L, max(nC, 1))
  for (k in seq_len(nC))
    if (courses$bolus[k] == 0)
      courseRate[, k] <- rateLine * (timeLine >= courses$start[k] &
                                       timeLine < courses$end[k])
  bolusAt <- match(courses$start, timeLine)
  switchIdx <- match(switchAt, timeLine)

  # ---- The advance ----
  cache <- list()
  propagator <- function(filling, dt) {
    key <- sprintf("%d %.14e", filling, dt)
    got <- cache[[key]]
    if (is.null(got))
    {
      E <- expmScaleSquare((if (filling) Afill else Aempty) * dt)
      E[E < 0] <- 0
      got <- list(E = E[seq_len(nS), seq_len(nS), drop = FALSE],
                  g = E[seq_len(nS), iOne])
      cache[[key]] <<- got
    }
    got
  }

  X <- matrix(0, nS, max(nC, 1))   # one column per course
  total <- matrix(0, L, nS)
  for (i in seq_len(L))
  {
    if (i > 1 && nC > 0)
    {
      dt <- timeLine[i] - timeLine[i - 1]
      # A course is filling over the step if the step starts before its
      # switch; the switch is a grid point, so the whole step is.
      filling <- timeLine[i - 1] < switchAt
      for (phase in c(TRUE, FALSE))
      {
        cols <- which(filling == phase)
        if (!length(cols)) next
        p <- propagator(phase, dt)
        X[, cols] <- p$E %*% X[, cols, drop = FALSE] +
          outer(p$g, courseRate[i - 1, cols])
      }
    }
    for (k in which(bolusAt == i))
      X[iC, k] <- X[iC, k] + courses$bolus[k]
    for (k in which(switchIdx == i))
    {
      X[iG, k] <- X[iG, k] + X[iB, k]
      X[iB, k] <- 0
    }
    total[i, ] <- rowSums(X)
  }

  Cp <- total[, iC] / V
  C2 <- if (k12 > 0) total[, i2] / V2 else rep(0, L)
  C3 <- if (k13 > 0) total[, i3] / V3 else rep(0, L)
  depot <- total[, iB] + total[, iG]

  # ---- Time until threshold ----
  recovery <- rep(0, L)
  if (plotRecovery && length(emerge) == 1 && !is.na(emerge) && emerge > 0 &&
      nC > 0)
  {
    cvec <- numeric(nS)
    cvec[iC] <- 1 / V
    settled <- timeLine >= max(switchAt)
    if (any(settled))
      recovery[settled] <- transitChainRecovery(
        total[settled, , drop = FALSE],
        Aempty[seq_len(nS), seq_len(nS), drop = FALSE], cvec, emerge,
        recoveryHorizonPlasma(maximum))
    recovery[!settled & timeLine >= min(courses$start)] <- NA_real_
  }

  data.frame(
    Time = timeLine,
    Cp = Cp,
    Ce = rep(NA_real_, L),
    Recovery = recovery,
    C2 = C2,
    C3 = C3,
    Depot = depot
  )
}
