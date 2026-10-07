
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


#' Simulate plasma and effect site concentration from time 0 to maximum
#'
#' See \code{vignette("stanpumpR-single-PK", package = "stanpumpR")} for an example
#'
#' @param dose table of individual doses
#' @param events table of events
#' @param PK PK parameters from \code{getDrugPK(drug)}
#' @param maximum maximum length of simulation in minutes
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

    # Identify extravascular (PO, IM, IN) and IV bolus doses.  A rate unit
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
    dose$Bolus <- route == ROUTE_IV & !rate

    # Target-controlled infusion.  A "Plasma target" or "Effect site target"
    # row (Dose = the target concentration, which is already in the units Cp
    # and Ce come out in) is replaced by the infusion rows the TCI controller
    # would run; see tci.R.  The schedule is also returned, in display units,
    # for the rate panel of the plot and for export.
    tci <- NULL
    if (any(isTciUnit(dose$Units)))
    {
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

    if (length(pkEvents) == 1 | nrow(events) == 0)
    {
      if (hasMetabolite)
      {
        results <- advanceClosedFormMetabolite(dose, pkSets[[1]], maximum, plotRecovery, emerge)
      } else if (sum(dose$PO) + sum(dose$IM) + sum(dose$IN) == 0)
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
