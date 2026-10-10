# Target-controlled infusion (TCI)
#
# Shafer SL, Gregg KM. Algorithms to rapidly achieve and maintain stable drug
# concentrations at the site of drug effect with a computer-controlled infusion
# pump. J Pharmacokinet Biopharm 1992;20:147-169, as implemented in the original
# STANPUMP program (STANPUMP.C: model(), find_peak(), virtual_model(),
# calculate_udfs()).
#
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at the request of
# Steven L. Shafer; verified on R 4.6.1 by tests/testthat/test-tci.R.
#
# A target row in the dose table ("Plasma target" or "Effect site target", the
# dose being the target concentration) starts a controller that recomputes the
# infusion rate every TCI_INTERVAL minutes:
#
#   Plasma target.  Decay the plasma states through one interval with the pump
#   off; the rate is the remaining deficit divided by the plasma concentration
#   that a unit rate produces by the end of the interval (Bailey & Shafer,
#   IEEE Trans Biomed Eng 1991;38:522-525).  Zero if already above target.
#
#   Effect site target.  The largest rate that reaches the target without
#   overshoot: over the next tPeak the effect site follows B(tau) + R * E(tau),
#   where B is the decay of the current effect-site states with the pump off
#   and E is the response to a unit-rate infusion lasting one interval.  So
#   R = min over tau of (CT - B(tau)) / E(tau), and the minimum is the moment
#   the curve just touches the target (Appendix A of the paper; STANPUMP
#   iterates find_peak() to the same point).  Negative means the pump is off.
#
#   The hand-off.  Once the effect site is within TCI_PLASMA_SWITCH of the
#   target, the controller holds the PLASMA at the target instead.  At steady
#   state Cp == Ce, so this holds Ce exactly.  The effect-site solution is
#   ill-conditioned there: the residual CT - B(dt) is a rounding error whose
#   sign decides whether the touching point is the next interval (a huge rate)
#   or tPeak away (a tiny rate), so the rate aliases between maximum and zero.
#   Both the paper and STANPUMP switch at 5%.
#
# Everything is the exact closed form on the eigenvalues and coefficients that
# getDrugPK() already provides; nothing is integrated numerically.  Time is in
# minutes and amounts are in the "base" mass unit of simCpCe() (mg for drugs
# measured in mcg/ml, mcg for drugs measured in ng/ml), so a rate is base/min
# and a concentration is base/L.
#
# Rules from the specification (Shafer, 2026-10-06):
#   - A target of 0 stops the TCI infusion.
#   - A manual infusion row stops the TCI infusion (the schedule gets a zero
#     row there); a TCI target zeroes any manual infusion that is running.
#     When both are entered at the same time the target wins.
#   - Manual boluses are allowed during TCI; the controller sees them and
#     gives no drug until the concentration is back at the target.
#   - Oral, IM and IN doses are not in the controller's model (as an unmodelled
#     input they are, to the pump, invisible); the simulation itself does
#     include them.
#   - Only the default PK set drives the controller.  No TCI drug has
#     event-dependent PK.

# Which drugs in the dose table are under TCI
isTciUnit <- function(units) as.character(units) %in% tciUnits

# The rate the pump should run for the coming interval, in base/min.
#
# p, e: the three plasma and four effect-site state variables (base/L), each
#       the amplitude of one exponential, so that Cp = sum(p), Ce = sum(e).
# CT:   target concentration (base/L).  site: "plasma" or "effect".
# dt:   the coming interval (min).  K: the kinetic constants from tciKinetics().
tciRate <- function(p, e, CT, site, dt, K)
{
  if (CT <= 0) return(list(rate = 0, effectMode = FALSE))

  Ce <- sum(e)
  effectMode <- site == "effect" && K$ke0 > 0 &&
    abs(Ce - CT) >= TCI_PLASMA_SWITCH * CT

  if (!effectMode) {
    decay <- exp(-K$lambda3 * dt)
    Pd <- sum(p * decay)
    Pu <- sum(K$pinf * (1 - decay))
    rate <- (CT - Pd) / Pu
  } else {
    # tau from the end of the coming interval out to tPeak, one second apart.
    tau <- unique(c(dt, K$tau[K$tau > dt]))
    M <- exp(-outer(K$lambda4, tau))                   # 4 x J
    B <- as.vector(e %*% M)
    E <- as.vector((K$einf * (exp(K$lambda4 * dt) - 1)) %*% M)
    ok <- E > 0
    rate <- if (any(ok)) min((CT - B[ok]) / E[ok]) else 0
  }
  list(rate = max(0, rate), effectMode = effectMode)
}

# Pull the constants the controller needs out of a drug's PK object.
tciKinetics <- function(PK)
{
  k <- PK$PK[[1]]
  lambda3 <- c(k$lambda_1, k$lambda_2, k$lambda_3)
  ke0 <- k$ke0
  tPeak <- if (is.null(PK$tPeak) || ke0 <= 0) 0 else PK$tPeak
  list(
    lambda3 = lambda3,
    lambda4 = c(lambda3, ke0),
    ke0     = ke0,
    tPeak   = tPeak,
    pbolus  = c(k$p_coef_bolus_l1, k$p_coef_bolus_l2, k$p_coef_bolus_l3),
    pinf    = c(k$p_coef_infusion_l1, k$p_coef_infusion_l2, k$p_coef_infusion_l3),
    ebolus  = c(k$e_coef_bolus_l1, k$e_coef_bolus_l2, k$e_coef_bolus_l3, k$e_coef_bolus_ke0),
    einf    = c(k$e_coef_infusion_l1, k$e_coef_infusion_l2, k$e_coef_infusion_l3, k$e_coef_infusion_ke0),
    # The effect site after any input peaks within tPeak of that input, so
    # the touching point is never further away than tPeak (plus a margin).
    tau     = seq_len(ceiling(tPeak * 60) + 10) / 60
  )
}

# Build the pump's infusion schedule for one drug.
#
# dose:    that drug's rows of the dose table AFTER simCpCe() has converted
#          them to base units and flagged Bolus / PO / IM / IN / RA.  Target rows
#          carry the target concentration in Dose.
# PK:      the drug's PK object from getDrugPK() (default PK set, tPeak, weight).
# maximum: end of the simulation (min).
#
# Returns a list:
#   dose    the dose table to simulate: target rows replaced by the TCI
#           infusion rows, manual infusions dropped where a target overrides them
#   rates   data.frame(Time, Rate, Dt, Bolus): one row per rate change, Rate in
#           base/min, Dt the time the rate is held, Bolus TRUE for the rapid
#           loading infusion that starts a target increase
#   boluses data.frame(Time, Amount): the loading infusions, in base units
tciSchedule <- function(dose, PK, maximum,
                        interval = TCI_INTERVAL, maxRate = TCI_MAX_RATE)
{
  K <- tciKinetics(PK)
  dose$Target <- isTciUnit(dose$Units)
  dose$Bolus[dose$Target] <- FALSE

  targets   <- dose[dose$Target & dose$Time >= 0 & dose$Time < maximum, ]
  manual    <- which(!dose$Target & !dose$Bolus & !dose$PO & !dose$IM & !dose$IN &
                     !dose$RA & !dose$RAslow & !dose$PO2)
  bolusRows <- dose[dose$Bolus & !dose$Target, ]

  # Several targets at one time: the last one entered wins.
  targets <- targets[order(targets$Time), ]
  targets <- targets[!duplicated(targets$Time, fromLast = TRUE), ]
  targets$Site <- ifelse(targets$Units == TCI_UNIT_EFFECT, "effect", "plasma")

  # A manual infusion entered at the same time as a non-zero target is
  # overridden by the target.
  drop <- manual[dose$Time[manual] %in% targets$Time[targets$Dose > 0]]
  manualRows <- dose[setdiff(manual, drop), ]

  eventTimes <- sort(unique(c(targets$Time, manualRows$Time, bolusRows$Time)))
  eventTimes <- eventTimes[eventTimes >= 0 & eventTimes < maximum]

  dtMax <- max(interval, min(10, maximum / 2000))

  p <- numeric(3); e <- numeric(4)
  mode <- "manual"; manualRate <- 0; CT <- 0; site <- "plasma"
  t <- 0; dt <- interval; tLastChange <- -Inf
  n <- 0L
  rTime <- rRate <- rDt <- numeric(0)
  changeTimes <- numeric(0)      # times a non-zero target took effect

  advance <- function(dt, rate) {
    d3 <- exp(-K$lambda3 * dt); d4 <- exp(-K$lambda4 * dt)
    p <<- p * d3 + K$pinf * rate * (1 - d3)
    e <<- e * d4 + K$einf * rate * (1 - d4)
  }
  # One row per interval, even when the rate repeats (the pump off while the
  # effect site rises to its peak): the schedule is the pump's programme, one
  # decision per update interval.
  emit <- function(time, rate, held) {
    n <<- n + 1L
    rTime[n] <<- time; rRate[n] <<- rate; rDt[n] <<- held
  }

  for (te in c(eventTimes, maximum)) {
    # Bring the model from t to te under the current mode.
    if (mode == "manual") {
      if (te > t) advance(te - t, manualRate)
      t <- te
    } else {
      while (t < te - 1e-9) {
        step <- min(dt, te - t)
        x <- tciRate(p, e, CT, site, step, K)
        rate <- min(x$rate, maxRate)
        emit(t, rate, step)
        advance(step, rate)
        t <- t + step
        if (rate > 0 && !x$effectMode && t - tLastChange > K$tPeak) {
          dt <- min(dt * 1.1, dtMax)
        } else {
          dt <- interval
        }
      }
      t <- te
    }
    if (te >= maximum) break

    # Apply what happens at te.
    amt <- sum(bolusRows$Dose[bolusRows$Time == te])
    if (amt > 0) {
      p <- p + K$pbolus * amt
      e <- e + K$ebolus * amt
      tLastChange <- te; dt <- interval
    }
    m <- manualRows[manualRows$Time == te, ]
    if (nrow(m) > 0) {
      # A manual infusion stops the controller.  The schedule says so with a
      # zero row, as for a target of 0; without it the rate panel, its hover
      # and the export carried the last TCI rate on to the end of the plot.
      # The engine adds the zero to the manual rate set at the same time.
      if (mode == "tci") emit(te, 0, 0)
      mode <- "manual"; manualRate <- sum(m$Dose)
    }
    tg <- targets[targets$Time == te, ]
    if (nrow(tg) > 0) {
      if (tg$Dose > 0) {
        mode <- "tci"; CT <- tg$Dose; site <- tg$Site
        tLastChange <- te; dt <- interval
        changeTimes <- c(changeTimes, te)
      } else {
        # Target 0: the pump stops and stays off.
        if (mode == "tci") emit(te, 0, 0)
        mode <- "manual"; manualRate <- 0; CT <- 0
      }
    }
  }

  rates <- data.frame(Time = rTime, Rate = rRate, Dt = rDt, Bolus = FALSE)

  # The loading dose: the interval(s) after a target increase that deliver
  # far more than the interval that follows.  With the effect site targeted
  # the pump is then off until the peak; with the plasma targeted the
  # maintenance rate is an order of magnitude lower.  A pump ceiling spreads
  # the loading dose over several intervals at the same (maximum) rate, which
  # are taken together.
  bTime <- bAmount <- numeric(0)
  for (tc in changeTimes) {
    k <- which(rates$Time == tc)
    if (length(k) != 1 || rates$Rate[k] <= 0) next
    j <- k
    while (j < nrow(rates) && rates$Rate[j + 1] == rates$Rate[k]) j <- j + 1
    nextRate <- if (j < nrow(rates)) rates$Rate[j + 1] else 0
    if (nextRate == 0 || rates$Rate[k] > 3 * nextRate) {
      rates$Bolus[k:j] <- TRUE
      bTime <- c(bTime, tc)
      bAmount <- c(bAmount, sum(rates$Rate[k:j] * rates$Dt[k:j]))
    }
  }
  boluses <- data.frame(Time = bTime, Amount = bAmount)

  # The dose table the engine simulates.
  keep <- dose[!dose$Target & !(seq_len(nrow(dose)) %in% drop), ]
  if (nrow(rates) > 0) {
    tciRows <- dose[rep(1, nrow(rates)), , drop = FALSE]
    tciRows$Time  <- rates$Time
    tciRows$Dose  <- rates$Rate
    tciRows$Units <- "TCI"
    tciRows$Bolus <- FALSE
    tciRows$PO <- tciRows$IM <- tciRows$IN <- tciRows$RA <- FALSE
    if (!is.null(tciRows$RAslow)) tciRows$RAslow <- FALSE
    if (!is.null(tciRows$PO2)) tciRows$PO2 <- FALSE
    tciRows$Target <- FALSE
    keep <- rbind(keep, tciRows)
  }
  if (nrow(keep) == 0) {
    keep <- dose[1, , drop = FALSE]
    keep$Time <- 0; keep$Dose <- 0; keep$Bolus <- TRUE; keep$Target <- FALSE
  }
  keep$Target <- NULL
  rownames(keep) <- NULL

  list(dose = keep, rates = rates, boluses = boluses)
}

# The TCI schedule in the units shown to the user: rates per kilogram per
# minute, and the loading doses as a mass.  Drugs measured in mcg/ml dose in
# mg, so their rates are mg/kg/min; drugs measured in ng/ml dose in mcg.
tciDisplay <- function(schedule, PK)
{
  if (is.null(schedule) || nrow(schedule$rates) == 0) return(NULL)
  mass <- if (PK$Concentration.Units == "ng") "mcg" else "mg"
  weight <- PK$weight
  rates <- data.frame(
    Drug  = PK$drug,
    Time  = schedule$rates$Time,
    Rate  = schedule$rates$Rate / weight,
    Units = paste0(mass, "/kg/min"),
    Bolus = schedule$rates$Bolus
  )
  boluses <- data.frame(
    Drug   = PK$drug,
    Time   = schedule$boluses$Time,
    Amount = schedule$boluses$Amount,
    Units  = mass
  )
  list(rates = rates, boluses = boluses)
}

# The dose table with every drug's TCI infusion rows merged in, for export.
# The rows the user typed are kept as typed; the TCI rows follow in the units
# of the rate graph.
tciMergeDoseTable <- function(DT, drugs)
{
  if (is.null(DT)) return(DT)
  extra <- lapply(drugs, function(d) {
    r <- d$tci$rates
    if (is.null(r) || nrow(r) == 0) return(NULL)
    data.frame(Drug = r$Drug, Time = r$Time, Dose = signif(r$Rate, 4), Units = r$Units)
  })
  extra <- do.call(rbind, Filter(Negate(is.null), extra))
  if (is.null(extra) || nrow(extra) == 0) return(DT)
  out <- DT[, c("Drug", "Time", "Dose", "Units")]
  out$Time <- as.numeric(out$Time)
  out <- rbind(out, extra)
  out <- out[order(out$Drug, out$Time), ]
  rownames(out) <- NULL
  out
}
