# Suggest Dosing: work backwards from target effect-site concentrations to a
# dose table.  inst/help/models/suggest-algorithm.md describes the method for
# the user; the steps here follow it.
#
#   1. Clean the targets: suggestTargets().
#   2. Lay out the regimen, a bolus at each target time and up to two infusion
#      rates in each interval, with every rate change inside the requested
#      window: suggestRegimen().
#   3. Seed the doses with ten rounds of proportional correction, reading the
#      effect site at the actual time of the next rate change (or the end).
#   4. Refine with nlm(), minimising the squared effect-site error alone.
#      Both steps read the effect site as a sum of each row's unit-dose curve,
#      simulated once, because the engine is linear in the doses.
#   5. Close the regimen with a single zero-rate row at the end time, so that
#      nothing runs past it.
#
# Only a drug with an effect site and intravenous bolus and infusion units can
# be targeted (suggestSupported()); the dialog offers no others
# (suggestDrugChoices()).

# nlm()'s iteration limit.  Its default of 100 stops a fit of many targets
# short; with the objective a matrix product, 1000 iterations take well under
# a second.
SUGGEST_ITERATION_LIMIT <- 1000L

suggest <- function(
    targetDrug,
    targetTable,
    endTime,
    drugs,
    drugList,
    eventTable,
    referenceTime)
{
  PK <- drugs[[targetDrug]]
  if (!targetDrug %in% drugList || is.null(PK))
    stop(shiny::safeError(paste("Suggest Dosing: unknown drug", targetDrug)))
  if (!suggestSupported(PK))
    stop(shiny::safeError(paste(
      "Suggest Dosing needs a drug with an effect site and intravenous bolus",
      "and infusion units, which", targetDrug, "does not have."
    )))

  cleaned <- suggestTargets(targetTable, endTime, referenceTime)
  if (is.null(cleaned))
  {
    outputComments("Target table is empty")
    return(NULL)
  }
  targets <- cleaned$targets
  endTime <- cleaned$endTime
  outputComments("End Time =", endTime)
  outputComments("Cleaned targets")
  outputComments(targets)

  # Work from the first target, which becomes time 0.  Nothing of this drug is
  # given before it, because the regimen replaces the drug's previous rows.
  offsetTime <- targets$Time[1]
  targets$Time <- targets$Time - offsetTime
  endTime <- endTime - offsetTime

  regimen <- suggestRegimen(targets, endTime, PK)
  outputComments("Structure of the regimen")
  outputComments(regimen)

  # The effect site is read at two sets of times: at each dose's successor,
  # the next later rate change or the end (nextTime, for the proportional
  # correction), and on the grid the fit is judged over (sampleTimes).
  nextTime <- vapply(
    regimen$Time,
    function(t) min(c(regimen$Time[regimen$Time > t], endTime)),
    numeric(1)
  )
  sampleTimes <- seq(0, endTime, length.out = RESOLUTION)
  readTimes <- c(nextTime, sampleTimes)
  simulateCe <- function(Dose)
  {
    DT <- data.frame(Time = regimen$Time, Dose = Dose, Units = regimen$Units)
    wide <- simCpCe(DT, eventTable, PK, endTime, plotRecovery = FALSE)$wide
    stats::approx(wide$Time, wide$"Effect Site", xout = readTimes, rule = 2)$y
  }

  # The kinetics are linear in the doses, so the effect site of any regimen of
  # this shape is the sum of each row's own curve -- that row at a unit dose,
  # every other row at zero -- scaled by the row's dose.  An infusion row's
  # unit rate runs until the next infusion row sets its zero, as its fitted
  # rate will.  Every curve is simulated with all the rows present, so every
  # dose time is on the engine's timeline and the effect site is read there
  # exactly; the grid times are interpolated from that timeline, as the plot
  # is.  One simulation per row, after which every evaluation in the search is
  # a matrix product rather than a simulation.
  rows <- seq_len(nrow(regimen))
  basis <- matrix(
    vapply(rows, function(j) simulateCe(as.numeric(rows == j)), numeric(length(readTimes))),
    ncol = length(rows)
  )
  basisNext   <- basis[seq_along(nextTime), , drop = FALSE]
  basisSample <- basis[length(nextTime) + seq_along(sampleTimes), , drop = FALSE]

  targetAt <- function(times) targets$Target[findInterval(times, targets$Time)]

  # Proportional correction.  Each dose is scaled by the ratio of the target in
  # force at its time to the effect site at its successor: a bolus is judged
  # where the infusion takes over, an infusion where the next rate replaces
  # it.  A dose whose effect site reads zero there is left alone rather than
  # divided by zero.
  seedTarget <- targetAt(regimen$Time)
  for (round in 1:10)
  {
    ce <- drop(basisNext %*% regimen$Dose)
    scale <- is.finite(ce) & ce > 0
    regimen$Dose[scale] <- regimen$Dose[scale] * seedTarget[scale] / ce[scale]
  }

  # Non-linear regression.  The objective is the squared difference between
  # the effect site and the target in force, summed over RESOLUTION evenly
  # spaced times from the first target to the end: a concentration and
  # nothing else.  It is divided by the number of times and the square of the
  # highest target, which leaves the minimum where it is but makes the
  # objective the same size for every drug and unit, so that nlm()'s stopping
  # rules mean the same thing for each.  typsize puts each dose on the scale
  # of its seed, because a bolus in mg and a rate in mcg/kg/min can differ by
  # orders of magnitude.  A negative dose counts as zero, and the gradient,
  # which the matrix form makes exact, is supplied.
  sampleTarget <- targetAt(sampleTimes)
  scaleTarget <- max(targets$Target)
  objective <- function(Dose)
  {
    given <- Dose > 0
    error <- (drop(basisSample %*% (Dose * given)) - sampleTarget) / scaleTarget
    value <- sum(error^2) / length(error)
    attr(value, "gradient") <-
      2 / length(error) / scaleTarget * drop(crossprod(basisSample, error)) * given
    value
  }
  seed <- regimen$Dose
  typsize <- ifelse(seed > 0, seed, 1)
  seedObjective <- as.numeric(objective(seed))

  outputComments("About to run nlm")
  fit <- stats::nlm(objective, seed, typsize = typsize, iterlim = SUGGEST_ITERATION_LIMIT)
  fitted <- signif(pmax(fit$estimate, 0), 3)

  # The rounded regimen as the engine itself simulates it, which also checks
  # the sum of curves the search used
  ceFitted <- simulateCe(fitted)[length(nextTime) + seq_along(sampleTimes)]
  fittedObjective <- sum(((ceFitted - sampleTarget) / scaleTarget)^2) / length(sampleTimes)
  outputComments("nlm code", fit$code, "after", fit$iterations, "iterations; objective",
                 seedObjective, "->", fit$minimum, "; rounded and simulated", fittedObjective)

  suggested <- data.frame(
    Time  = regimen$Time + offsetTime,
    Dose  = fitted,
    Units = regimen$Units
  )
  # Close the regimen with one zero-rate row at the requested end.  The engine
  # adds the rates of infusion rows at the same time, so this stops the drug
  # only if no other rate is set there.  suggestRegimen() puts every rate
  # change before the end; this also holds it at the dose table's time
  # precision, where a change within TIME_SNAP_DIGITS of the end would land on
  # it.
  requestedEnd <- cleaned$endTime
  late <- suggested$Units == PK$Infusion.Units &
    round(suggested$Time, TIME_SNAP_DIGITS) >= round(requestedEnd, TIME_SNAP_DIGITS)
  suggested <- rbind(
    suggested[!late, ],
    data.frame(Time = requestedEnd, Dose = 0, Units = PK$Infusion.Units)
  )
  rownames(suggested) <- NULL
  suggested$Drug <- targetDrug
  # The search's diagnostics, for the debug log and the tests: nlm()'s code
  # and iterations, and the objective at the seed, at nlm()'s minimum, and for
  # the rounded regimen as simulated
  attr(suggested, "optimizer") <- list(
    code       = fit$code,
    iterations = fit$iterations,
    seed       = seedObjective,
    minimum    = fit$minimum,
    rounded    = fittedObjective
  )
  suggested
}

# Clean the target table.
#
# Times and targets are parsed as the dose table parses them, and the end time
# and target times are put in minutes from the reference time.  A row is
# dropped when its time is blank, its target is zero or blank, or it is at or
# after the end time; a target at time 0 is kept.  Of several rows at the same
# time the last one entered is kept, as for TCI targets.  The rest are sorted,
# and a target lower than the one before is raised to it: decreasing targets
# are not supported.
#
# Returns list(targets = data.frame(Time, Target), endTime), or NULL when no
# target is left.
suggestTargets <- function(targetTable, endTime, referenceTime)
{
  time <- as.character(targetTable$Time)
  # A row with no time is not a target.  Told apart before validateTime(),
  # which turns a blank into "0": dropping the rows whose cleaned time was 0
  # also dropped a target at time 0, and so, once the app passes times in
  # minutes (it does since there are time units), one at the procedure start.
  noTime <- is.na(time) | !nzchar(trimws(time))
  time <- vapply(time, validateTime, character(1), USE.NAMES = FALSE)
  target <- vapply(as.character(targetTable$Target), validateDose, character(1),
                   USE.NAMES = FALSE)
  if (referenceTime == REFERENCE_TIME_NONE)
  {
    time <- suppressWarnings(as.numeric(time))
    endTime <- suppressWarnings(as.numeric(endTime))
  } else {
    time <- clockTimeToDelta(referenceTime, time)
    endTime <- clockTimeToDelta(referenceTime, endTime)
  }
  if (length(endTime) != 1 || !is.finite(endTime)) return(NULL)
  # To the dose table's time precision, so that two times it would show as one
  # are one target
  targets <- data.frame(
    Time   = round(as.numeric(time), TIME_SNAP_DIGITS),
    Target = suppressWarnings(as.numeric(target))
  )
  targets <- targets[
    !noTime & is.finite(targets$Time) & is.finite(targets$Target) &
      targets$Target > 0 & targets$Time < endTime, ]
  targets <- targets[!duplicated(targets$Time, fromLast = TRUE), ]
  if (nrow(targets) == 0) return(NULL)
  targets <- targets[order(targets$Time), ]
  targets$Target <- cummax(targets$Target)
  rownames(targets) <- NULL
  list(targets = targets, endTime = endTime)
}

# The shape of the regimen, every dose 1.
#
# A bolus at each target time.  In the interval from each target time to the
# next (or to the end time), an infusion from the target time plus tPeak and a
# second rate a fifth of the way from there to the end of the interval, both
# in whole minutes from the first target.  A rate change that would fall at or
# beyond the end of its interval is left out, so every row is inside the
# requested window and no interval's rates overlap the next one's; an interval
# shorter than tPeak gets the bolus alone.  The closing zero-rate row is added
# by suggest() after the fit, because it is not a free parameter.
suggestRegimen <- function(targets, endTime, PK)
{
  intervalEnd <- c(targets$Time[-1], endTime)
  infusionTimes <- numeric(0)
  for (i in seq_len(nrow(targets)))
  {
    start <- targets$Time[i]
    first <- max(start, round(start + PK$tPeak))
    second <- round(first + (intervalEnd[i] - first) / 5)
    if (first < intervalEnd[i]) infusionTimes <- c(infusionTimes, first)
    if (second > first && second < intervalEnd[i]) infusionTimes <- c(infusionTimes, second)
  }
  regimen <- data.frame(
    Time  = c(targets$Time, infusionTimes),
    Dose  = 1,
    Units = c(rep(PK$Bolus.Units, nrow(targets)),
              rep(PK$Infusion.Units, length(infusionTimes)))
  )
  regimen <- regimen[order(regimen$Time), ]  # stable: a bolus before its infusion
  rownames(regimen) <- NULL
  regimen
}

# Can Suggest Dosing target this drug?  It needs an effect site (a drug without
# one -- the antibiotics, mannitol, the prodrugs codeine and tramadol -- has no
# effect-site concentration to fit) and a bolus unit and an infusion unit that
# are both intravenous, which is what the regimen is made of.  `PK` is the
# drug's entry from getDrugPK().
suggestSupported <- function(PK)
{
  suggestUnitsSupported(PK$Bolus.Units, PK$Infusion.Units) && pkHasEffectSite(PK)
}

suggestUnitsSupported <- function(bolusUnits, infusionUnits)
{
  units <- c(as.character(bolusUnits), as.character(infusionUnits))
  if (length(units) != 2 || any(is.na(units) | !nzchar(units))) return(FALSE)
  # Told apart as simCpCe() tells them apart: a rate is per minute or per hour
  rate <- grepl("min|hr", units)
  all(doseRoute(units) == ROUTE_IV) && !rate[1] && rate[2]
}

# An effect site in every PK set: ke0 > 0.  A drug without one has ke0 = 0 and
# tPeak = 0, and its effect-site column is NA.
pkHasEffectSite <- function(PK)
{
  length(PK$PK) > 0 &&
    all(vapply(PK$PK, function(set) isTRUE(set$ke0 > 0), logical(1)))
}

# Whether a drug's model has an effect site.  A property of the model, not the
# patient, so it is read once from a reference adult and remembered.
drugHasEffectSite <- memoise::memoise(function(drug)
{
  PK <- tryCatch(
    getDrugPK(drug, weight = 70, height = 170, age = 40, sex = SEX_MALE),
    error = function(e) NULL
  )
  !is.null(PK) && pkHasEffectSite(PK)
})

# The drugs the Suggest Dosing dialog offers, in library order: not a gas, and
# a supported pair of units and an effect site as suggestSupported() requires.
suggestDrugChoices <- function(drugDefaults = getDrugDefaultsGlobal())
{
  keep <- vapply(seq_len(nrow(drugDefaults)), function(i)
  {
    drug <- drugDefaults$Drug[i]
    !isGasDrug(drug) &&
      suggestUnitsSupported(drugDefaults$Bolus.Units[i], drugDefaults$Infusion.Units[i]) &&
      drugHasEffectSite(drug)
  }, logical(1))
  drugDefaults$Drug[keep]
}
