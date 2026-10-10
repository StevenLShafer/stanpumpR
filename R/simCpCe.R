
#' Turn a simulated plasma and effect-site series into the shapes the app plots
#'
#' Splits out of \code{simCpCe()} so that a drug whose series changes after it
#' was first simulated -- because it received a contribution from a parent
#' drug's active metabolite -- can be finished the same way, rather than having
#' the normalisation, the equispaced grid and the maxima recomputed by hand in
#' two places.
#'
#' @param wide a data frame of \code{Time}, \code{Plasma}, \code{Effect Site}
#'   and \code{Recovery}
#' @param PK PK parameters from \code{getDrugPK(drug)}
#' @param maximum maximum length of simulation in minutes
#' @param plotRecovery should recovery be kept in the plotted series?
#'
#' @returns a list of \code{results}, \code{equiSpace} and \code{max}
#' @keywords internal
finishDrugSeries <- function(wide, PK, maximum, plotRecovery)
{
  results <- wide
  maxCp <- max(results$Plasma)

  # A pure prodrug -- codeine, tramadol -- has no effect site of its own, so
  # its effect-site column is NA at every point.  That is right for the plotted
  # series, because simulationPlot() drops NA rows and the drug is then drawn
  # as plasma only.  It is not right for anything derived, which is why every
  # scalar below is guarded: max() would return NA and approx() would refuse to
  # interpolate a column with fewer than two non-NA values.
  ceAllNA <- all(is.na(results$"Effect Site"))
  maxCe <- if (ceAllNA) 0 else max(results$"Effect Site", na.rm = TRUE)

  # An osmotic agent's plasma column is serum osmolality, baseline included.
  # Normalised to its peak that would read about 95% before any dose, so the
  # rise above the baseline is normalised instead.  The absolute values, which
  # the unnormalised plot shows, are left alone.
  plasmaRise <- results$Plasma
  if (!is.null(PK$osmotic)) plasmaRise <- plasmaRise - PK$osmotic$baseline
  maxRise <- max(plasmaRise)
  results$CpNormCp <- if (maxRise > 0) plasmaRise          / maxRise * 100 else 0
  results$CeNormCp <- if (maxCp > 0) results$"Effect Site" / maxCp * 100 else 0
  results$CpNormCe <- if (maxCe > 0) results$Plasma        / maxCe * 100 else 0
  results$CeNormCe <- if (maxCe > 0) results$"Effect Site" / maxCe * 100 else 0

  # Calculate equispaced output
  xout <- seq(from = 0, to = maximum, length.out = RESOLUTION)
  equiSpaceCe <- if (ceAllNA) {
    # Zero, not NA.  A prodrug has no effect of its own and its MEAC is zero,
    # and both the hover readout and the total-opioid MEAC sum read equiSpace.
    # The metabolite's row is where the effect actually appears.
    rep(0, length(xout))
  } else {
    stats::approx(x = results$Time, y = results$"Effect Site", xout = xout)$y
  }
  # Recovery is missing wherever it could not be computed -- a dose given that
  # has not begun to be absorbed; see pendingDoseTimes().  na.rm = FALSE so
  # that such a stretch stays missing instead of being interpolated across,
  # which would invent a number for exactly the interval that has none.  It
  # costs up to one grid step of extra blank at each end of the stretch, the
  # bracketing interval being partly unknown, and erring blank is the right way
  # round.  approx() refuses a column with fewer than two known values, so the
  # all-missing case -- a dose that never starts within the run -- is handled
  # before it gets there.
  equiSpaceRecovery <- if (sum(!is.na(results$Recovery)) < 2) {
    rep(NA_real_, length(xout))
  } else {
    stats::approx(x = results$Time, y = results$Recovery, xout = xout,
                  na.rm = FALSE)$y
  }
  equiSpace <- data.frame(
    Drug = PK$drug,
    Time = xout,
    Ce = equiSpaceCe,
    Time = xout,
    Recovery = equiSpaceRecovery
  )

  equiSpace$Ce[1] <- 0  # Approx tends to make it a very small negative number
  if (PK$MEAC == 0)
  {
    equiSpace$MEAC <- 0
  } else {
    equiSpace$MEAC <- equiSpace$Ce / PK$MEAC * 100
  }
  # na.rm, for the same reason, and -Inf rather than a maximum if every point
  # is missing.  The plot reads this to scale the recovery axis and treats zero
  # as "no axis to draw", which is what an all-missing column should give.
  maxRecovery <- suppressWarnings(max(results$Recovery, na.rm = TRUE))
  if (!is.finite(maxRecovery)) maxRecovery <- 0
  max <- data.frame(
    Drug = PK$drug,
    Recovery = maxRecovery,
    Cp = maxCp,
    Ce = maxCe
  )
  if (!plotRecovery) results$Recovery <- NULL
  results <- tidyr::gather(results, "Site", "Y", -Time)
  results$Drug <- PK$drug
  results <- results[, c(4, 1, 2, 3)]
  # Structure of results
  # Four columns: Drug, Time, Site, Y
  # 7 Sites: Plasma, Effect Site, CpNormCp, CeNormCp, CpNormCE, CeNormCe, and MEAC
  # These will be subset in simulation plot as needed.

  list(
    results = results,
    equiSpace = equiSpace,
    max = max
  )
}


#' Cut an engine's output at the end of the window
#'
#' Drops the rows after \code{maximum} from an engine's wide output, and the
#' same times from the effect-site state sets it carries as attributes
#' (\code{recoveryStates}, \code{metaboliteRecoveryStates}).  Every engine's
#' time line has a point at \code{maximum} itself, so the cut series still
#' ends there.
#'
#' @param results an engine's output: a data frame with a \code{Time} column
#' @param maximum end of the window, minutes
#'
#' @returns \code{results}, unchanged when nothing lies after \code{maximum}
#' @keywords internal
clipToWindow <- function(results, maximum)
{
  keep <- results$Time <= maximum
  if (all(keep)) return(results)
  sets <- c("recoveryStates", "metaboliteRecoveryStates")
  states <- lapply(sets, function(a) clipStateSet(attr(results, a), maximum))
  results <- results[keep, , drop = FALSE]
  rownames(results) <- NULL
  for (i in seq_along(sets)) attr(results, sets[i]) <- states[[i]]
  results
}

#' Cut a state set from \code{recoveryStateSet()} at the end of the window
#'
#' @param set a state set, or NULL
#' @param maximum end of the window, minutes
#'
#' @returns the set without its times after \code{maximum}
#' @keywords internal
clipStateSet <- function(set, maximum)
{
  if (is.null(set)) return(set)
  keep <- set$time <= maximum
  if (all(keep)) return(set)
  set$time  <- set$time[keep]
  set$state <- set$state[keep, , drop = FALSE]
  if (is.matrix(set$lambda)) set$lambda <- set$lambda[keep, , drop = FALSE]
  if (!is.null(set$pending)) {
    set$pending <- set$pending[keep]
    # as recoveryStateSet() does: nothing pending is no mask
    if (!any(set$pending)) set$pending <- NULL
  }
  set
}


#' Add engine results run on the same time line
#'
#' Each part is one engine's output for some of a drug's doses, all on the
#' same time line.  The concentrations add; the time until threshold does not,
#' and is solved again from the summed effect-site states.
#'
#' @param parts a list of engine outputs (\code{Time}, \code{Cp}, \code{Ce},
#'   \code{Recovery}), the first being the run that carries every
#'   non-oral dose
#' @param plotRecovery was recovery asked for?  The parts then carry their
#'   \code{recoveryStates}.
#' @param emerge the threshold, in the units the engines simulate
#'
#' @returns one engine output, the sum of the parts
#' @keywords internal
superposeEngineResults <- function(parts, plotRecovery, emerge)
{
  out <- parts[[1]]
  for (part in parts[-1])
  {
    if (!identical(part$Time, out$Time))
      stop("Cannot add engine runs on different time lines.")
    for (col in setdiff(names(out), c("Time", "Recovery")))
      out[[col]] <- out[[col]] + part[[col]]
  }
  if (plotRecovery)
  {
    states <- mergeStateSets(out$Time, lapply(parts, attr, "recoveryStates"))
    out$Recovery <- recoveryFromStates(states, emerge)
    attr(out, "recoveryStates") <- states
  }
  out
}


#' Simulate plasma and effect site concentration from time 0 to maximum
#'
#' See \code{vignette("stanpumpR-single-PK", package = "stanpumpR")} for an example
#'
#' The result covers the window from 0 to \code{maximum}, and only that: a
#' dose at or after \code{maximum} is not simulated (it cannot change the
#' curves inside the window), and the returned series, the maxima in
#' \code{max} and the normalised series (CpNormCp and the rest, scaled to those
#' maxima) all stop at \code{maximum}.  A dose given before \code{maximum} is
#' simulated in full, including an oral dose whose absorption starts after it.
#' The app chooses \code{maximum} long enough to take in every dose before it
#' calls this.
#'
#' @param dose table of individual doses
#' @param events table of events
#' @param PK PK parameters from \code{getDrugPK(drug)}
#' @param maximum end of the simulation, in minutes: the window is 0 to
#'   \code{maximum}, and doses at or after it are ignored
#' @param plotRecovery should the "time until threshold" be calculated?  For
#'   each time point, how long the effect site would take to fall to
#'   \code{PK$endCe} if all delivery stopped at that moment; returned as the
#'   \code{Recovery} column of \code{equiSpace}.  Checked against stopping
#'   delivery in the simulation itself by
#'   \code{tests/testthat/test-recovery-engines.R} (2026-10-05).
#'
#' @returns a list of data frames with the output of the a single drug
#'   simulation, plus \code{recoveryStates}: the effect-site exponential states
#'   behind the \code{Recovery} column, or NULL when recovery was not asked
#'   for.  A drug that forms an active metabolite additionally carries
#'   \code{metaboliteSeries}, \code{metaboliteName} and
#'   \code{metaboliteRecoveryStates}; \code{foldMetabolites()} adds that
#'   contribution, and its recovery, to the metabolite drug's own row.
#'   \code{tci} holds the TCI schedule, if any, and \code{scheduled} the
#'   repeats of any qd/bid/tid/qid dose (Drug, Time, Dose, Units), or NULL.
#'
#' @export
simCpCe <- function(dose, events, PK, maximum, plotRecovery)
  {
    # dose <- doseTable
    # pK <- PK
    # maximum <- max

    # The result covers 0 to maximum, however this is called.  A dose at or
    # after maximum cannot change anything inside that window, so it is not
    # simulated: the rule the scheduled repeats and the TCI controller already
    # follow ("no dose is given at or after it").  Simulated, such a dose ran
    # the time line on past maximum, so the returned curves ran on to it and
    # the maxima, and so the normalised curves, were taken from a peak outside
    # the window: 1 mg of propofol at 0 and 100 mg at 120, run to 60, peaked
    # at 1% (audit finding F20, October 2026).  The app lengthens its plot to
    # take in every dose (plotInfo() in R/app_server.R) before calling, so
    # there this matters only when the plot is already its unit's longest,
    # and the app then says the dose falls after the end.
    late <- suppressWarnings(as.numeric(dose$Time)) >= maximum
    late[is.na(late)] <- FALSE
    if (any(late)) dose <- dose[!late, , drop = FALSE]

    # Scheduled doses (qd, bid, tid, qid): expand each into its repeats out to
    # the end of the plot, while the dose is still in the user's units.  The
    # repeats are also returned on their own, for export; see scheduled.R.
    expanded <- expandScheduledDoses(dose, maximum)
    dose <- expanded$dose

    # Convert all doses to base units
    switch(
      PK$Concentration.Units,  # Units (per ml)
      mcg = {                  # 1 mcg/ml = 1000 mg/L
        mg_Conv  <- 1          # 1 mcg/ml = 1 mg / L
        mcg_Conv <- 1000       # 1 mcg/ml = 1,000 mcg / L
        ng_Conv  <- 1000000    # 1 mcg/ml = 1,000 mcg / L = 1,000,000 ng / L
      },
      ng = {                   # 1 ng/ml = 1000 mcg/L
        mg_Conv  <- .001       # 1 ng/ml = 0.001 mcg/ml = 0.001 mg/L80
        mcg_Conv <- 1        # Native unit
        ng_Conv  <- 1000
      },
      mOsm = {                 # An osmotic agent: amounts in mOsm, Cp in mOsm/L
        # 1 mOsm is molecularWeight mg of a solute that does not dissociate,
        # so a dose in mg divided by mg_Conv is a dose in mOsm.
        mg_Conv  <- PK$osmotic$molecularWeight
        mcg_Conv <- PK$osmotic$molecularWeight * 1000
        ng_Conv  <- PK$osmotic$molecularWeight * 1000000
      },
      stop("Unsupported Concentration.Units: ", PK$Concentration.Units)
    )

    # Grams.  Anchored, because "mg" and every other mass unit contain a "g".
    use <- grep("^g( |/|$)", dose$Units)
    dose$Dose[use] <- dose$Dose[use] * 1000 / mg_Conv
    use <- grep("mg",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / mg_Conv
    use <- grep("mcg",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / mcg_Conv
    use <- grep("ng",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / ng_Conv

    # Convert dose per kg  to absolute dose
    use <- grep("kg",dose$Units)
    dose$Dose[use] <- dose$Dose[use] * PK$weight

    # Convert dose per hour or per day to dose per minute
    use <- grep("hr",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / 60
    use <- grep("/day", dose$Units)
    dose$Dose[use] <- dose$Dose[use] / MINS_PER_DAY

    # Identify extravascular (PO, IM, IN, RA) and IV bolus doses.  A rate unit
    # (isRateUnit(), R/routes.R) is an input rate whatever its route word, so
    # it is neither: a "mg/day PO" row, the constant-rate oral input of
    # poRateUnits, becomes an infusion row on the drug's apparent oral
    # parameters, with no absorption constant and no bioavailability (the
    # apparent scale already contains F; getDrugPK() sets bioavailability_PO
    # to zero for a drug without ka_PO, so it must not be applied).  Every
    # other unit is classified exactly as before.  (Claude Code, 2026-10-07,
    # at the request of Steven L. Shafer.)
    route <- doseRoute(dose$Units)
    rate  <- isRateUnit(dose$Units)
    dose$PO <- route == ROUTE_PO & !rate
    dose$IM <- route == ROUTE_IM & !rate
    dose$IN <- route == ROUTE_IN & !rate
    dose$RA <- route == ROUTE_RA & !rate
    # A drug with a slow second RA depot (ka_RA_slow in its model; see
    # getDrugPK()) absorbs each RA dose through two parallel depots.  The
    # dose rows are duplicated, the copy flagged as the internal route
    # "RAslow"; the split of the dose is carried by the two bioavailabilities,
    # so each copy keeps the whole dose.
    dose$RAslow <- rep(FALSE, nrow(dose))
    hasSlowRA <- any(vapply(PK$PK, function(s) isTRUE(s$ka_RAslow > 0), logical(1)))
    if (hasSlowRA && any(dose$RA))
    {
      slowRows <- dose[dose$RA, , drop = FALSE]
      slowRows$RA <- FALSE
      slowRows$RAslow <- TRUE
      dose <- rbind(dose, slowRows)
      dose <- dose[order(dose$Time), , drop = FALSE]
    }
    dose$Bolus <- route == ROUTE_IV & !rate

    # Saturable oral absorption (gabapentin): each oral dose is scaled by the
    # fraction absorbed at its own size, in mg per administration.  The dose is
    # in the base unit here, and the base unit times mg_Conv is mg for every
    # concentration unit.  Scaled once, here, each dose is then an ordinary
    # input to whichever engine runs; see oralSaturationFraction().
    if (!is.null(PK$oralSaturation) && any(dose$PO))
      dose$Dose[dose$PO] <- dose$Dose[dose$PO] *
        oralSaturationFraction(dose$Dose[dose$PO] * mg_Conv, PK$oralSaturation)

    # A drug with a second oral depot (ka_PO2 in its model; see getDrugPK())
    # absorbs each oral dose through two parallel depots, each with its own
    # lag.  As for the slow RA depot, the dose rows are duplicated, the copy
    # flagged as the internal route "PO2", and the split of the dose is
    # carried by the two bioavailabilities.  After the saturation scaling, so
    # that both copies carry the scaled dose.
    dose$PO2 <- rep(FALSE, nrow(dose))
    hasPO2 <- any(vapply(PK$PK, function(s) isTRUE(s$ka_PO2 > 0), logical(1)))
    if (hasPO2 && any(dose$PO))
    {
      secondRows <- dose[dose$PO, , drop = FALSE]
      secondRows$PO <- FALSE
      secondRows$PO2 <- TRUE
      dose <- rbind(dose, secondRows)
      dose <- dose[order(dose$Time), , drop = FALSE]
    }

    # Target-controlled infusion.  A "Plasma target" or "Effect site target"
    # row (Dose = the target concentration, which is already in the units Cp
    # and Ce come out in) is replaced by the infusion rows the TCI controller
    # would run; see tci.R.  The schedule is also returned, in display units,
    # for the rate panel of the plot and for export.
    tci <- NULL
    if (any(isTciUnit(dose$Units)))
    {
      # The controller inverts one system; it cannot target a sum of them.
      if (!is.null(PK$parallelSystems))
        stop("Target-controlled infusion is not available for a drug ",
             "simulated as several parallel systems.")
      schedule <- tciSchedule(dose, PK, maximum)
      dose <- schedule$dose
      tci <- tciDisplay(schedule, PK)
    }

    events <- events[,c(1,2)]

    pkSets <- PK$PK
    pkEvents <- PK$pkEvents

    events$Event <- gsub(" ","", events$Event)
    events <- events[events$Event %in% pkEvents,]

    # A drug that forms an active metabolite takes its own route, because the
    # metabolite is advanced alongside the parent over the union of the two
    # drugs' eigenvalues.
    hasMetabolite <- !is.null(pkSets[[1]]$metabolite)

    # The threshold, in the units the engines simulate.  An osmotic agent is
    # plotted as serum osmolality, baseline + fraction x Cp (below), and its
    # threshold is set on that axis, so it is mapped back to a concentration.
    # One at or below the baseline can never be reached and is treated as no
    # threshold.
    emerge <- PK$endCe
    if (!is.null(PK$osmotic) && length(emerge) == 1 && !is.na(emerge) && emerge > 0)
      emerge <- max(0, (emerge - PK$osmotic$baseline) / PK$osmotic$fraction)

    # The engines, for one dose table and one drug's PK sets.
    runEngines <- function(dose, pkSets)
    {
      if (length(pkEvents) == 1 | nrow(events) == 0)
      {
        if (hasMetabolite)
        {
          results <- advanceClosedFormMetabolite(dose, pkSets[[1]], maximum, plotRecovery, emerge)
        } else if (sum(dose$PO) + sum(dose$IM) + sum(dose$IN) + sum(dose$RA) + sum(dose$RAslow) + sum(dose$PO2) == 0)
        {
          results <- advanceClosedForm0(dose,pkSets[[1]], maximum, plotRecovery, emerge)
        } else {
          results <- advanceClosedFormPO_IM_IN(dose,pkSets[[1]], maximum, plotRecovery, emerge)
        }
      } else {
        if (hasMetabolite)
          stop("A drug with an active metabolite cannot yet switch kinetics on a ",
               "clinical event; advanceClosedForm1() carries no metabolite ",
               "coefficients.")
        # Process Events
        defaultEvent <- data.frame(
          Time = 0,
          Event = PK_EVENT_DEFAULT
        )
        if (events$Time[1] > 0)
          events <- rbind(defaultEvent,events)
        events <- events[events$Time < maximum,]
        events <- rbind(events, events[nrow(events),])
        events$Time[nrow(events)] <- maximum
        results <- advanceClosedForm1(dose, events, pkSets, maximum, plotRecovery, emerge)
      }
      results
    }

    # A drug with more than one oral formulation (morphine tablets and
    # liquid; see oralFormulationSet() in getDrugPK.R) runs once for each.
    # The default run carries every dose except the other formulations' oral
    # ones, which it gives as zero; each further run carries only its own
    # formulation's oral doses, on PK sets that differ only in their oral
    # absorption.  Zeroing rather than dropping rows keeps every run on the
    # same time line (the formulations share their lag), so the series add
    # point by point, which is exact because the disposition is linear.  The
    # effect-site states add the same way, and the time until threshold is
    # solved once from their sum.  (Claude Code, 2026-10-10, at the request of
    # Steven L. Shafer.)
    formulation <- doseFormulation(dose$Units)
    others <- names(PK$oralFormulations)
    other <- dose$PO & formulation %in% others
    # A drug plotted as the sum of parallel systems (ketorolac's S and R
    # enantiomers; see parallelSystemSets() in getDrugPK.R) runs once for each,
    # every dose scaled by that system's share, and the runs are added.  The
    # systems share the doses, their lags and the effect site, so every run is
    # on the same time line.  getDrugPK() refuses them alongside further oral
    # formulations.
    shareOf <- function(dose, fraction) {
      if (!is.null(fraction)) dose$Dose <- dose$Dose * fraction
      dose
    }
    if (!is.null(PK$parallelSystems))
    {
      parts <- list(runEngines(shareOf(dose, PK$doseFraction), pkSets))
      for (sys in PK$parallelSystems)
        parts[[length(parts) + 1]] <- runEngines(shareOf(dose, sys$doseFraction), sys$PK)
      results <- superposeEngineResults(parts, plotRecovery, emerge)
    } else if (!any(other))
    {
      results <- runEngines(shareOf(dose, PK$doseFraction), pkSets)
    } else {
      dose <- shareOf(dose, PK$doseFraction)
      base <- dose
      base$Dose[other] <- 0
      parts <- list(runEngines(base, pkSets))
      for (f in intersect(others, formulation[other]))
      {
        own <- dose
        own$Dose[!(other & formulation == f)] <- 0
        parts[[length(parts) + 1]] <- runEngines(own, PK$oralFormulations[[f]])
      }
      results <- superposeEngineResults(parts, plotRecovery, emerge)
    }

  # A lagged oral, IM, IN or RA dose given before maximum still puts a point of
  # the time line where its absorption starts, which can be after maximum
  # (simulationTimeGrid()): gabapentin's lag is 19 minutes.  The series is cut
  # at maximum, with the effect-site states that ride along, so that nothing
  # returned lies outside the window either.
  results <- clipToWindow(results, maximum)

  # Lift the metabolite out into a series of its own before the parent's
  # columns are renamed.  It is folded into the metabolite drug's own row
  # later, once every drug has been simulated, because the contribution
  # crosses from one drug's entry into another's.
  metaboliteSeries <- NULL
  if (hasMetabolite)
  {
    metaboliteSeries <- data.frame(
      Time = results$Time,
      Cp   = results$CpMetabolite,
      Ce   = results$CeMetabolite
    )
    results$CpMetabolite <- NULL
    results$CeMetabolite <- NULL
  }

  # The effect-site states behind the Recovery column, which the engines carry
  # out as attributes.  foldMetabolites() needs them: a drug that also receives
  # an active metabolite has to solve for its time until threshold from the
  # combined effect site, because recovery times do not add.  Lifted off rather
  # than left on `wide`, which is copied around the pipeline.
  recoveryStates           <- attr(results, "recoveryStates")
  metaboliteRecoveryStates <- attr(results, "metaboliteRecoveryStates")
  attr(results, "recoveryStates")           <- NULL
  attr(results, "metaboliteRecoveryStates") <- NULL

  names(results) <- c("Time", "Plasma","Effect Site", "Recovery")

  # An osmotic agent is shown as the serum osmolality it produces: the
  # patient's baseline plus the net rise its plasma concentration causes.  See
  # R/drugs_mannitol.R.  Such a drug has no effect site, so only the plasma
  # column changes.
  if (!is.null(PK$osmotic))
  {
    results$Plasma <- PK$osmotic$baseline + PK$osmotic$fraction * results$Plasma
  }

  out <- finishDrugSeries(results, PK, maximum, plotRecovery)
  out$wide             <- results
  out$metaboliteSeries <- metaboliteSeries
  out$metaboliteName   <- PK$metaboliteName
  out$recoveryStates           <- recoveryStates
  out$metaboliteRecoveryStates <- metaboliteRecoveryStates

  out$tci              <- tci
  out$scheduled        <- expanded$scheduled

  return(out)
}
