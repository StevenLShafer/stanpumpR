# stanpumpR viewer (the Visualizer) — the run file
#
# Author:    Dr Charles Minto
# Assisted by: Claude.ai (Anthropic)
# Adapted to stanpumpR's pipeline by Claude Code (Anthropic), 2026-11-05.
#
# Copyright (c) 2026 Charles Minto and Thomas Schnider
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# One drug's run, as the viewer (inst/www/stanpumpR-viewer.html, built from
# tools/viewer/src) plays it: the concentrations in V1, V2, V3 and the effect
# site, the bioavailable drug still in the depot, the drug that has entered
# the body, the intravenous rate and target in force, and the boluses, at even
# times from the plot's Min time to its Max time.
#
# The viewer computes no pharmacokinetics, and the run is the plot's own
# (Steven L. Shafer, 2026-11-05): viewerPlotEntry() starts from the drug's
# entry in the app's simulation, drugs(), and re-solves nothing while the
# plot's points are close enough to animate (viewerGridFine()). Where they
# are not -- a plot's time line is dense only near its doses and kinks, with
# gaps of an hour on an oral run -- it re-solves the same drug on the
# viewer's times with the plot's own PK (viewerSimulate()), no covariate
# recomputed. Everything is in the concentration domain: a drug plotted as an
# ACT (heparinCPB) is re-solved as the anti-Xa activity the ACT is computed
# from. So the vessels show exactly what stanpumpR computes, for any model it
# runs: linear or saturable, closed form, transit chain or Runge-Kutta.
#
# The flows are model-agnostic too: the viewer draws each arrow from the
# change in the amounts, not from clearances. "given" is the cumulative drug
# that has entered the body (intravenous rates and boluses, the apparent-oral
# rates that run straight into V1, and the drug absorbed from the depot, read
# off the fall of stanpumpR's Depot column); the drug eliminated is then
# given less the drug in V1 + V2 + V3, whatever the elimination is.
#
# Time is in seconds in the file (minutes in stanpumpR). Concentrations are
# in the drug's own unit per mL; amounts and rates in the unit `amount` names
# (mg for a drug in mcg/mL, mcg for one in ng/mL, IU for one in IU/mL, U for
# one in mcU/mL), a rate per minute. The run's `amountScale` turns the
# concentration x volume (L) into that unit: stanpumpR's own amounts are
# per 1000 IU for an IU drug and in mU for a mcU drug.

# The concentration and amount units for each Concentration.Units the viewer
# draws, and the factor from stanpumpR's internal amount to the one shown
# (depotUnits() in R/compartments.R uses the same). Other drugs (those plotted
# as mOsm, INR or L/min) are not offered; the inhaled anesthetics, plotted in
# %, are drawn from their own states by viewerGasRun().
VIEWER_UNITS <- list(
  mcg  = list(conc = "mcg/mL",  amount = "mg",   scale = 1),
  ng   = list(conc = "ng/mL",   amount = "mcg",  scale = 1),
  pg   = list(conc = "pg/mL",   amount = "ng",   scale = 1),
  IU   = list(conc = "IU/mL",   amount = "IU",   scale = 1000),
  mcU  = list(conc = "mcU/mL",  amount = "U",    scale = 1 / 1000),
  mmol = list(conc = "mmol/L",  amount = "mmol", scale = 1))

# At most about this many times in a run: the step is 1 s, or longer on a
# long plot.
VIEWER_MAX_TIMES <- 7200

viewerUnits <- function(concUnits)
{
  u <- VIEWER_UNITS[[concUnits]]
  if (is.null(u))
    stop("The viewer draws drug concentrations (mcg/mL, ng/mL, pg/mL, IU/mL, mcU/mL, mmol/L); ",
         "this drug is plotted in ", concUnits, ".")
  u
}

#' Why the viewer cannot draw a simulated drug
#'
#' @param entry one drug's entry of the app's simulation (`drugs()`), or of
#'   `processdoseTable()`; the PK fields alone (`recalculatePK()`) suffice,
#'   except for a drug of parallel systems, which needs its dose rows (`$DT`).
#' @return NULL when the viewer can draw it, else one sentence saying why not.
#' @noRd
viewerProblem <- function(entry)
{
  if (is.null(entry)) return("There is no simulation of this drug to show.")
  # An inhaled anesthetic is drawn from its own states (viewerGasRun()); oxygen
  # has no tissue states, and MAC is a sum over agents, not a gas.
  if (isTRUE(entry$isGas) || isTRUE(isGasDrug(entry$drug))) {
    if (is.null(entry$gasRun))
      return("The Visualizer draws the inhaled anesthetics, not oxygen or the summed MAC.")
    return(NULL)
  }
  if (!is.null(entry$inr))
    return("This drug is plotted as an INR, and stanpumpR keeps no drug concentration for it to show.")
  if (!is.null(entry$osmotic))
    return("This drug is plotted as a serum osmolality, not a drug concentration.")
  if (!is.null(entry$endogenousActivity))
    return(paste("This drug is plotted as the patient's own clotting-factor activity plus",
                 "the activity infused, not as a drug concentration."))
  units <- if (!is.null(entry$act)) "IU" else entry$Concentration.Units
  if (is.null(VIEWER_UNITS[[units]]))
    return("The viewer draws drug concentrations (mcg/mL, ng/mL, pg/mL, IU/mL, mcU/mL, mmol/L).")
  if (is.null(viewerSystem(entry)))
    return(paste("This drug's doses reach more than one of its separate systems (two",
                 "enantiomers, or two routes each with its own fit, such as oral and",
                 "intravenous meloxicam), which one set of compartments cannot show."))
  NULL
}

#' The PK sets of the one system a drug's doses reach
#'
#' A drug built from parallel systems (`parallelSystems`, `simCpCe()`) is
#' drawn only while its doses reach one of them: meloxicam's oral fit takes
#' only oral doses and its intravenous fit (ANJESO) only intravenous ones, so
#' a table of one route runs one ordinary system, whose concentrations are the
#' plotted ones (the other system's being zero). Ketorolac's enantiomers both
#' take every dose, so it is never drawn.
#'
#' @param entry one drug's entry, with its dose rows (`$DT`) when it has
#'   parallel systems.
#' @return the system's PK sets (the drug's own `$PK` when it has no
#'   parallel systems), or NULL when the doses reach more than one, or the
#'   dose rows are not there to say which.
#' @noRd
viewerSystem <- function(entry)
{
  if (length(entry$parallelSystems) == 0) return(entry$PK)
  d <- entry$DT
  if (is.null(d)) return(NULL)
  units <- as.character(d$Units[suppressWarnings(as.numeric(d$Dose)) != 0])
  units <- units[!is.na(units)]
  if (length(units) == 0) return(NULL)
  routes <- unique(doseRoute(units))
  systems <- c(list(list(doseFraction = entry$doseFraction, routes = entry$routes, PK = entry$PK)),
               entry$parallelSystems)
  reached <- Filter(function(sys)
    (is.null(sys$doseFraction) || sys$doseFraction > 0) &&
      (is.null(sys$routes) || any(routes %in% sys$routes)),
    systems)
  if (length(reached) != 1) return(NULL)
  reached[[1]]$PK
}

# The run's times, in minutes: from Min time to Max time, 1 s apart or
# longer on a long plot; the last time is Max time, a shorter step if need be.
viewerTimes <- function(minimum, maximum)
{
  if (!is.finite(minimum) || minimum < 0) minimum <- 0
  if (!(maximum > minimum)) stop("Max time must be after Min time.")
  step <- max(1, ceiling((maximum - minimum) * 60 / VIEWER_MAX_TIMES)) / 60
  times <- seq(minimum, maximum, by = step)
  if (maximum - utils::tail(times, 1) > 1e-6) times <- c(times, maximum)
  times
}

# A dose in its units (mg, mcg/kg, mg/kg/hr, IU, U/hr, ...) to the amount
# the run shows (u$amount, or u$amount per minute). Mass doses as simCpCe()
# converts them; an activity or potency dose is already in the amount shown.
# NA for a unit the viewer cannot read (a salt, mmol), which leaves the drug
# given unknown.
VIEWER_MASS <- c(g = 1e3, mg = 1, mcg = 1e-3, ng = 1e-6)

viewerBaseDose <- function(dose, units, weight, u, height = NULL)
{
  units <- as.character(units)
  f <- rep(NA_real_, length(units))
  if (u$amount %in% names(VIEWER_MASS)) {
    shown <- VIEWER_MASS[[u$amount]]
    f[grepl("^g( |/|$)", units)] <- VIEWER_MASS[["g"]] / shown
    f[grepl("mg", units)]  <- VIEWER_MASS[["mg"]] / shown
    f[grepl("mcg", units)] <- VIEWER_MASS[["mcg"]] / shown
    f[grepl("ng", units)]  <- VIEWER_MASS[["ng"]] / shown
  }
  if (u$amount == "IU") f[grepl("^IU( |/|$)", units)] <- 1
  if (u$amount == "U")  f[grepl("^U( |/|$)", units)] <- 1
  out <- dose * f
  out <- out * bodySizeDoseFactor(units, weight, height)
  out[grepl("hr", units)]   <- out[grepl("hr", units)] / 60
  out[grepl("/day", units)] <- out[grepl("/day", units)] / MINS_PER_DAY
  out
}

# The rate in force at each of `times` from a table of rate changes (Time,
# Rate), already in the order in which a later row wins at one time.
rateInForce <- function(steps, times)
{
  if (nrow(steps) == 0) return(rep(0, length(times)))
  i <- findInterval(times + 1e-9, steps$Time)
  out <- rep(0, length(times))
  out[i > 0] <- steps$Rate[i[i > 0]]
  out
}

# The integral from 0 of the rate in force, at each of `times`.
cumulativeRate <- function(steps, times)
{
  if (nrow(steps) == 0) return(rep(0, length(times)))
  knots <- sort(unique(c(0, steps$Time[steps$Time <= max(times)], times)))
  r <- rateInForce(steps, knots)
  cum <- c(0, cumsum(r[-length(knots)] * diff(knots)))
  cum[match(times, knots)]
}

# The anti-Xa activity (IU/mL) behind an ACT (s): the inverse of R/act.R's
# ACT = act0 + emax C / (c50 + C); 0 where the ACT is outside the link's range.
viewerActActivity <- function(actSeconds, act)
{
  if (!is.numeric(actSeconds) || length(actSeconds) != 1 || !is.finite(actSeconds)) return(0)
  x <- actSeconds - act$act0
  if (x <= 0 || x >= act$emax) return(0)
  act$c50 * x / (act$emax - x)
}

# Whether a drug's plotted concentration includes what a parent in the table
# forms (foldMetabolites(): $wide is $wideOwn plus the formed metabolite, and
# $formedFrom names the parents). A metabolite with no dose row of its own
# (morphine from codeine alone) has no $wideOwn: it is all formed. Read as
# not formed, it was "re-solved" without its parent, which simulated nothing
# and left the plot's own points, minutes apart, to be drawn as the run
# (Carbon's third UI audit, 2026-11-15, H8).
viewerFormed <- function(entry)
{
  all <- entry$wide$Plasma
  if (is.null(all)) return(FALSE)
  own <- entry$wideOwn$Plasma
  if (is.null(own)) return(length(entry$formedFrom) > 0)
  !isTRUE(all.equal(own, all))
}

# The name of the PK set in force at each of `times` (minutes): the last of
# the drug's clinical events at or before it, as advanceClosedForm1() reads
# them (an event at or after Max time is not reached), or NULL for a drug
# that keeps one set. `sets` are the PK sets of the system drawn
# (viewerSystem()).
viewerSetsInForce <- function(entry, sets, times, maximum)
{
  ev <- entry$ET
  if (length(sets) < 2) return(NULL)
  if (is.null(ev)) ev <- data.frame(Time = numeric(0), Event = character(0))
  ev <- data.frame(Time = suppressWarnings(as.numeric(ev$Time)),
                   Event = gsub(" ", "", as.character(ev$Event)), stringsAsFactors = FALSE)
  ev <- ev[!is.na(ev$Time) & ev$Time < maximum & ev$Event %in% names(sets), , drop = FALSE]
  ev <- ev[order(ev$Time), , drop = FALSE]
  # Physiology events (R/physiologyEvents.R): the merged sets, on the time
  # line simCpCe() switches them on
  physiology <- entry[["physiologyEvents"]]
  if (viewerFollowsPhysiologyEvents(entry)) {
    ev <- physiologyEventTimeline(ev, physiology)
    ev <- ev[ev$Time < maximum, , drop = FALSE]
  }
  if (nrow(ev) == 0) return(NULL)
  i <- findInterval(times, ev$Time)
  out <- rep(PK_EVENT_DEFAULT, length(times))
  out[i > 0] <- ev$Event[i[i > 0]]
  out
}

# Whether the plot's own points are close enough to animate: no gap inside
# the window, nor at its edges, longer than VIEWER_GAP_STEPS of the viewer's
# steps. The plot's time line is dense only near doses and kinks, so a run
# usually fails this and is re-solved (viewerSimulate()).
VIEWER_GAP_STEPS <- 2

viewerGridFine <- function(entry, minimum, maximum)
{
  times <- viewerTimes(minimum, maximum)
  step <- if (length(times) > 1) times[2] - times[1] else maximum - minimum
  lines <- list(entry$wide$Time, entry$compartments$Time)
  all(vapply(lines, function(tt) {
    if (is.null(tt)) return(TRUE)
    inside <- sort(unique(c(tt[tt > minimum & tt < maximum], minimum, maximum)))
    if (!any(abs(tt - minimum) < 1e-9) || !any(abs(tt - maximum) < 1e-9)) return(FALSE)
    max(diff(inside)) <= VIEWER_GAP_STEPS * step + 1e-9
  }, logical(1)))
}

#' The drug's entry the viewer reads: the plot's own, or re-solved
#'
#' @param drug the drug's name.
#' @param plotEntry the drug's entry in the app's simulation (`drugs()`).
#' @param doseTable,events,drugs,antiseizureInteractions as for
#'   `viewerSimulate()`; `drugs` the plot's own entries, whose PK is reused.
#' @param minimum,maximum the plot's Min and Max time, minutes.
#' @return the entry, with `$viewerResolved` TRUE when it was re-solved.
#' @noRd
viewerPlotEntry <- function(drug, plotEntry, doseTable, events, drugs, minimum, maximum,
                            antiseizureInteractions = FALSE)
{
  problem <- viewerProblem(plotEntry)
  if (!is.null(problem)) stop(problem)
  if (is.null(plotEntry$act) && viewerGridFine(plotEntry, minimum, maximum)) {
    plotEntry$viewerResolved <- FALSE
    return(plotEntry)
  }
  # A drug that is also formed from a parent in the table (morphine from
  # codeine) needs that parent simulated too, as the plot does.
  entry <- viewerSimulate(drug, doseTable, events, drugs, minimum, maximum, antiseizureInteractions,
                          allDrugs = viewerFormed(plotEntry))
  entry$viewerResolved <- TRUE
  entry
}

#' Simulate the dose table for the viewer
#'
#' The app's own pipeline, as `drugs()` runs it, but with the viewer's
#' times added to every time line, so the run is sampled from the very
#' solution the plot draws. A drug plotted as an ACT is solved as the anti-Xa
#' activity (IU/mL) its ACT is computed from: the viewer shows concentrations.
#'
#' @param drug the drug's name, as in the dose table.
#' @param doseTable the cleaned dose table (Time in minutes).
#' @param events the cleaned event table.
#' @param drugs the drugs' PK, from `recalculatePK()` (the app's `drugPK()`),
#'   or the plot's own simulated entries, which carry it.
#' @param minimum,maximum the plot's Min and Max time, minutes.
#' @param antiseizureInteractions as for `processdoseTable()`; with it off
#'   only `drug`'s rows are simulated.
#' @return the drug's simulated entry.
#' @noRd
viewerSimulate <- function(drug, doseTable, events, drugs, minimum, maximum,
                           antiseizureInteractions = FALSE, allDrugs = FALSE)
{
  if (!isTRUE(antiseizureInteractions) && !isTRUE(allDrugs)) {
    doseTable <- doseTable[doseTable$Drug == drug, , drop = FALSE]
    drugs <- drugs[names(drugs) == drug]
  }
  if (is.null(drugs[[drug]])) stop("No simulation for ", drug, " in this dose table.")
  if (!is.null(drugs[[drug]]$act)) {
    act <- drugs[[drug]]$act
    drugs[[drug]]$act <- NULL
    drugs[[drug]]$Concentration.Units <- "IU"
    # The typical level (the height scale) is an ACT in seconds; the viewer
    # needs the anti-Xa activity that gives it.
    drugs[[drug]]$typical <- viewerActActivity(drugs[[drug]]$typical, act)
  }
  times <- viewerTimes(minimum, maximum)
  X <- withOutputTimes(times, processdoseTable(
    doseTable, events, drugs, maximum, FALSE,
    plotMinimum = minimum, antiseizureInteractions = antiseizureInteractions))
  X[[drug]]
}

#' One drug's run for the viewer
#'
#' @param entry the drug's simulated entry, from `viewerSimulate()`.
#' @param minimum,maximum the plot's Min and Max time, minutes: the run's
#'   first and last times.
#' @param category the drug's category (sets the liquid's colour), or NA.
#' @return a list; jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA,
#'   null = "null") is the file the viewer reads.
#' @noRd
viewerRun <- function(entry, minimum, maximum, category = NA_character_,
                      physiology = NULL, physiologyEvents = NULL)
{
  problem <- viewerProblem(entry)
  if (!is.null(problem)) stop(problem)
  # A drug the physiology events do not change is drawn at normal physiology
  # throughout, and says so (ChatGPT's review of PR #796, finding 7: the run
  # showed the events of a drug they had not changed)
  if (is.data.frame(physiologyEvents) && nrow(physiologyEvents) > 0 &&
      !viewerFollowsPhysiologyEvents(entry)) {
    physiologyEvents <- NULL
    physiology <- NULL
  }
  times <- viewerTimes(minimum, maximum)
  minimum <- times[1]
  u <- viewerUnits(entry$Concentration.Units)
  # The system the doses reach (meloxicam's oral or intravenous fit).
  sys <- viewerSystem(entry)
  # A Michaelis-Menten peripheral compartment declared as k23/k32 (GHB) has
  # V2 = V k23 / k32 and Q = k23 V (advanceMichaelisMenten()), not the PK
  # set's placeholders.
  mm <- entry$michaelisMenten
  if (!is.null(mm$k23) && !is.null(mm$k32))
    sys <- lapply(sys, function(s) { s$v2 <- s$v1 * mm$k23 / mm$k32; s$cl2 <- mm$k23 * s$v1; s })
  # The PK set in force at each time. A model that switches sets at a
  # clinical event (the infant dexmedetomidine model's bypass sets) changes
  # its volumes there, the amounts carried across (convertState()): the
  # drug in each compartment is its concentration times the volume IN FORCE.
  # With the first set's volumes throughout, the drug in the body fell 8% the
  # instant bypass started, drawn as 23.5 mcg/min eliminated while clearance
  # had fallen 17-fold, and the elimination drawn on bypass was then that of
  # the wrong volumes (Carbon's third UI audit, 2026-11-15, H8).
  # So the run carries the set in force at its first time, and, where the
  # volumes or CL1 change inside the window, each time's own (pkAt).
  inForce <- viewerSetsInForce(entry, sys, times, maximum)
  k <- sys[[if (is.null(inForce)) 1 else inForce[1]]]
  pkAt <- NULL
  if (!is.null(inForce)) {
    used <- unique(inForce)
    pick <- match(inForce, used)
    field <- function(name) vapply(used, function(s) {
      x <- sys[[s]][[name]]
      if (is.null(x) || !is.finite(x)) 0 else x
    }, numeric(1), USE.NAMES = FALSE)[pick]
    pkAt <- list(v1 = field("v1"), v2 = field("v2"), v3 = field("v3"), cl1 = field("cl1"))
    if (!any(vapply(pkAt, function(x) any(x != x[1]), logical(1)))) pkAt <- NULL
  }
  # Whether the model has a compartment in any set in force.
  anySet <- function(name) if (is.null(inForce)) isTRUE(k[[name]] > 0) else
    any(vapply(unique(inForce), function(s) isTRUE(sys[[s]][[name]] > 0), logical(1)))
  weight <- entry$weight
  height <- entry$height

  # Linear between the simulation's own points; at a time given twice (a
  # bolus), the later value.
  at <- function(tt, v) stats::approx(tt, v, times, rule = 2,
                                      ties = list("ordered", function(x) x[length(x)]))$y
  w <- entry$wide
  cp <- at(w$Time, w$Plasma)
  ce <- if (is.null(w$`Effect Site`) || all(is.na(w$`Effect Site`))) NULL else at(w$Time, w$`Effect Site`)
  cm <- entry$compartments
  c2 <- if (!is.null(cm$V2)) at(cm$Time, cm$V2) else NULL
  c3 <- if (!is.null(cm$V3)) at(cm$Time, cm$V3) else NULL
  # A 2-compartment drug has V3's column only when another drug in the
  # simulation does (it is all zeros then): no V3 for a model without one.
  if (!is.null(c3) && !anySet("v3")) c3 <- NULL
  if (!is.null(c2) && !anySet("v2")) { c2 <- NULL; c3 <- NULL }
  if (!is.null(c3) && !anySet("cl3")) c3 <- NULL
  if (!is.null(c2) && !anySet("cl2") && is.null(entry$michaelisMenten)) { c2 <- NULL; c3 <- NULL }

  # The drug's dose rows, scheduled doses expanded into their repeats.
  d <- entry$DT
  if (is.null(d)) d <- data.frame(Drug = character(), Time = double(), Dose = double(), Units = character())
  d$Time <- as.numeric(d$Time)
  d <- expandScheduledDoses(d, maximum)$dose
  # Doses in units of a drug plotted by mass (oxytocin) as micrograms, as
  # simCpCe() reads them.
  converted <- potencyToMass(d$Dose, d$Units, entry$unitPotency)
  d$Dose <- converted$Dose
  d$Units <- converted$Units
  # In time order (stably: at one time the later row still wins), and only
  # doses before Max time, which the simulation's last point has not taken in.
  d <- d[order(d$Time), , drop = FALSE]
  d <- d[d$Time < maximum - 1e-9, , drop = FALSE]
  units <- as.character(d$Units)
  route <- doseRoute(units)
  rate <- isRateUnit(units)
  tci <- isTciUnit(units)
  enteral <- units %in% enteralRateUnits

  # The intravenous rate in force: the latest of the manual infusion rows and
  # the TCI controller's schedule (which has a zero row where a manual
  # infusion stops it, and gives its loading dose as a short rate). At one
  # time the TCI row wins if it runs the pump (a target entered with an
  # infusion wins), else the manual row.
  # A prodrug in equivalents (fosphenytoin, mg PE) enters a conversion
  # compartment, not V1 (advanceMichaelisMenten()): it is no IV input here.
  pe <- isEquivalentUnit(units)
  manual <- d[route == ROUTE_IV & rate & !tci & !pe, , drop = FALSE]
  # A Michaelis-Menten drug dosed as a salt (phenytoin sodium) enters its
  # model as the active drug: advanceMichaelisMenten() multiplies IV boluses
  # and rates by saltFactor$IV, and so does the drug given here, or the salt's
  # mass would read as drug eliminated at the moment of the dose.
  ivSalt <- entry$michaelisMenten$saltFactor$IV
  if (is.null(ivSalt)) ivSalt <- 1
  steps <- data.frame(Time = manual$Time,
                      Rate = ivSalt * viewerBaseDose(manual$Dose, manual$Units, weight, u, height) *
                        doseProductFactor(manual$Units, entry$doseProducts),
                      rank = rep(1, nrow(manual)))
  tciRates <- entry$tci$rates
  if (!is.null(tciRates) && nrow(tciRates) > 0) {
    tr <- viewerBaseDose(tciRates$Rate, tciRates$Units, weight, u, height)
    # A TCI row ranks above a manual row at one time when it runs the pump,
    # or when a non-zero target is entered at that time (the target wins,
    # R/tci.R, even if the controller gives no drug then); a zero row there
    # is otherwise the stop a manual row makes, so the manual row wins.
    targetTimes <- d$Time[tci & d$Dose > 0]
    atTarget <- vapply(tciRates$Time, function(x) any(abs(targetTimes - x) < 1e-9), logical(1))
    steps <- rbind(steps, data.frame(Time = tciRates$Time, Rate = tr,
                                     rank = ifelse(tr > 0 | atTarget, 2, 0)))
  }
  steps <- steps[order(steps$Time, steps$rank), , drop = FALSE]
  rateAt <- rateInForce(steps, times)
  rateAt[is.na(rateAt)] <- 0

  # Apparent-oral rates ("mg/day PO"): simCpCe() runs them straight into V1
  # on the apparent parameters, so they enter the body as they are given.
  oral <- d[rate & !tci & route != ROUTE_IV & !(route %in% TISSUE_ROUTES) & !enteral, , drop = FALSE]
  oralSteps <- data.frame(Time = oral$Time, Rate = viewerBaseDose(oral$Dose, oral$Units, weight, u, height))
  oralSteps <- oralSteps[order(oralSteps$Time), , drop = FALSE]

  # IV boluses, in base amount.
  b <- d[route == ROUTE_IV & !rate & !tci & !pe, , drop = FALSE]
  # A dose named as a product (aminophylline for theophylline) counts as the
  # plotted drug it carries, as simCpCe() enters it (doseProductFactor()).
  bAmount <- ivSalt * viewerBaseDose(b$Dose, b$Units, weight, u, height) *
    doseProductFactor(b$Units, entry$doseProducts)
  # A dose in a unit the viewer cannot read (a salt, mmol) leaves the drug
  # given unknown; it is drawn as nothing rather than refused.
  unreadable <- anyNA(c(steps$Rate, oralSteps$Rate, bAmount))
  steps$Rate[is.na(steps$Rate)] <- 0
  oralSteps$Rate[is.na(oralSteps$Rate)] <- 0
  bAmount[is.na(bAmount)] <- 0
  # d is in time order, so the boluses up to each time are a prefix of b.
  bolusGiven <- c(0, cumsum(bAmount))[findInterval(times + 1e-9, b$Time) + 1]

  # The depot: the bioavailable drug still to be absorbed. What it loses is
  # what has been absorbed; what it gains is a dose arriving (stanpumpR puts a
  # point just before and at each arrival, so a rise holds no absorption to
  # speak of).
  depot <- NULL
  absorbedFromDepot <- rep(0, length(times))
  if (!is.null(cm$Depot) && any(cm$Depot > 0)) {
    depot <- pmax(0, at(cm$Time, cm$Depot))
    fall <- c(0, cumsum(pmax(0, -diff(cm$Depot))))
    absorbedFromDepot <- at(cm$Time, fall)
  }
  # That fall is the drug absorbed only while nothing fills the depot as it
  # empties and nothing passes it by: a rate into a tissue depot (a patch, a
  # perineural or pump infusion), an enteral infusion or a zero-order release
  # into the depot raises the Depot column while it is absorbed, so its fall
  # is far less than the absorption (626-fold at 10 h for lidocaine 100 mg RA
  # with 20 mg/hr RA), and a zero-order share run straight into V1 (the
  # insulins, amantadine ER) or exenatide's saturable input never enters it.
  # Then no absorption is drawn at all rather than a wrong one (Carbon's
  # third UI audit, 2026-11-15, H8): the amounts card says the input is
  # rewritten (inputKnown below).
  depotIsAbsorption <- is.null(entry$oralZeroOrder) && is.null(entry$zeroOrderSC) &&
    is.null(entry$michaelisMenten$saturableSC) && !any(enteral) &&
    !any(rate & route %in% TISSUE_ROUTES)
  absorbed <- absorbedFromDepot + cumulativeRate(oralSteps, times)
  given <- cumulativeRate(steps[, c("Time", "Rate")], times) + bolusGiven + absorbed

  # Some inputs are rewritten by the engine in ways the dose rows do not
  # show (zero-order releases, an enteral infusion, saturable SC input), and
  # a metabolite's input is its formation: the drug given is then unknown,
  # and the viewer says so rather than draw a wrong elimination.
  inputKnown <- depotIsAbsorption && !isTRUE(entry$enteralInfusion) &&
    is.null(entry$molar) && nrow(d) > 0 &&
    # Drug bound to its target (linagliptin, vildagliptin) is neither the
    # concentrations times the volumes nor eliminated.
    is.null(entry$michaelisMenten$targetBinding) && is.null(entry$michaelisMenten$rapidBinding) &&
    # A metabolite formed from a parent in the table enters as it is formed.
    !viewerFormed(entry) &&
    # A prodrug in equivalents (fosphenytoin, mg PE) waits in a conversion
    # compartment that is neither the body nor the Depot column.
    !any(isEquivalentUnit(units)) &&
    !unreadable

  # The target and the mode in force: a target row starts TCI (a target of 0
  # stops it), and a manual infusion row strictly after it stops it; one at
  # the target's own time does not, the target winning (R/tci.R).
  # By lookup, not a loop over the times reading a data frame row at each
  # (a quarter of a second at 7200 times; Gemini, 6b52133). d is in time
  # order, so the target in force is the last target row at or before each
  # time, and a manual row stops it if one lies after it and at or before
  # the time.
  targets <- d[tci, , drop = FALSE]
  stops <- sort(manual$Time)
  modeAt <- rep("manual", length(times)); targetAt <- numeric(length(times))
  last <- findInterval(times + 1e-9, targets$Time)
  has <- last > 0
  if (any(has)) {
    tTime <- targets$Time[last[has]]
    stopped <- findInterval(times[has] + 1e-9, stops) > findInterval(tTime + 1e-9, stops)
    on <- targets$Dose[last[has]] > 0 & !stopped
    j <- which(has)[on]
    modeAt[j] <- ifelse(targets$Units[last[j]] == TCI_UNIT_EFFECT, "effect", "plasma")
    targetAt[j] <- targets$Dose[last[j]]
  }

  # The boluses the run shows (those inside the window), in base amount.
  shown <- which(b$Time >= minimum - 1e-9 & b$Time <= maximum)
  boluses <- lapply(shown, function(i) list(t = b$Time[i] * 60, amount = bAmount[i]))

  list(
    format = "stanpumpR-viewer", version = 2,
    drug = entry$drug,
    model = trimws(sub("https?://.*$", "", paste(entry$reference, collapse = " "))),
    category = if (length(category) == 1) category else NA_character_,
    typical = if (is.numeric(entry$typical) && length(entry$typical) == 1) entry$typical else 0,
    units = list(conc = u$conc, amount = u$amount, rate = paste0(u$amount, "/min")),
    amountScale = u$scale,
    patient = list(weight = weight, height = entry$height, age = entry$age, sex = entry$sex),
    # The physiology panel the plot was drawn at, shown under the title
    physiology = viewerPhysiology(physiology),
    # With physiology events, the settings in force from each time (seconds)
    physiologySteps = viewerPhysiologySteps(physiologyEvents, minimum, end = maximum),
    pk = list(v1 = k$v1, v2 = k$v2, v3 = k$v3, cl1 = k$cl1, cl2 = k$cl2, cl3 = k$cl3),
    # The volumes (L) and CL1 (L/min) in force at each time, where a clinical
    # event changes them inside the window; else absent, and `pk` holds.
    pkAt = pkAt,
    t = times * 60, cp = cp, ce = ce, c2 = c2, c3 = c3,
    rate = rateAt, target = targetAt, mode = modeAt,
    boluses = boluses,
    depot = depot,
    absorbed = if (depotIsAbsorption && any(absorbed > 0)) absorbed else NULL,
    given = if (inputKnown) given else NULL,
    inputKnown = inputKnown,
    # Whether elimination is CL1 x Cp: the viewer's only stand-in for it
    # when the drug given is unknown. A saturable drug's is not, and its
    # elimination arrow is then not drawn.
    eliminationLinear = is.null(entry$michaelisMenten),
    source = "stanpumpR")
}

#' One inhaled anesthetic's run for the viewer
#'
#' Shafer, 2026-11-09: the partial pressure of the agent in the breathing
#' circuit (what the patient inspires), the lungs (end-tidal), the vessel-rich
#' group (the effect site), the muscle group and the fat group, read from the
#' gas engine's own states for the plot (`gasDrugEntries()` keeps them as
#' `$gasRun`): the run is the plot's own, interpolated onto the viewer's
#' times, nothing re-solved.
#'
#' Partial pressures are in % of an atmosphere, as stanpumpR plots them. A
#' compartment's capacity is its volume times its tissue:gas partition
#' coefficient (the gas phase's is its volume), so the agent it holds, in mL
#' of vapour at one atmosphere, is 10 x capacity (L) x partial pressure (%):
#' the run's `amountScale`. The ideal circuit, stanpumpR's, has no volume and
#' holds none; GasMan's semi-closed circuit holds its 8 L.
#'
#' `delivered` is the agent the fresh gas has carried into the circuit since
#' time 0, in mL of vapour, from the settings in force (the engine reads them
#' at the start of each interval, as here). The viewer derives every flow from
#' the amounts (player.js): into each tissue its capacity times the rise in its
#' partial pressure, out of the lungs into the blood their sum, from the
#' circuit into the lungs that plus the lungs' own rise, and out through the
#' pop-off valve the agent delivered less the agent held.
#'
#' The blood (Shafer, 2026-11-17, from GasMan's display, which shows ART and
#' VEN): `venous` is the mixed venous partial pressure, the tissues' tensions
#' weighted by their shares of the cardiac output (`gasMixedVenous()`, the
#' engine's own); arterial blood is at the alveolar (end-tidal) partial
#' pressure in the engine, so it is not sent twice. `cardiacOutput` is the
#' cardiac output the engine ran on (the Physiology panel's slider included),
#' and `alveolarVentilation` the minute ventilation less the dead space it
#' took. Blood holds no agent in the engine (mixed venous is computed at each
#' moment), so the viewer draws the blood as bars, as it draws the ideal
#' circuit. `greenhouse` is `gasGreenhouse()`: what the viewer needs to turn
#' the agent delivered into kg of CO2 equivalent.
#'
#' @param entry the agent's entry in the app's simulation (`drugs()`), with
#'   `$gasRun`.
#' @param minimum,maximum the plot's Min and Max time, minutes.
#' @param category the agent's category, or NA.
#' @return a list, as `viewerRun()`'s, with `kind = "gas"`.
#' @noRd
viewerGasRun <- function(entry, minimum, maximum, category = NA_character_,
                         physiology = NULL)
{
  problem <- viewerProblem(entry)
  if (!is.null(problem)) stop(problem)
  gr <- entry$gasRun
  g <- entry$drug
  times <- viewerTimes(minimum, maximum)
  body <- gr$body
  state <- gr$state
  # Physiology events (R/physiologyEvents.R): a body in force from each step.
  # A step in cardiac output or altitude changes flows and pressures only,
  # which the run carries per time.  A step in the intubation (a tube pulled
  # back out of a bronchus, or slipping into one) changes one capacity, the
  # lungs', the ventilated lung holding a share of the alveolar gas
  # (R/endobronchial.R): the run carries it per time as `lungsCapacity`, and
  # the tissues' are unchanged.  A step in temperature changes every tissue's
  # capacity (its tissue:gas coefficient), which the viewer draws as one width
  # for the run, so such a run is not drawn.  Only the bodies in force inside
  # the window drawn count (ChatGPT's round-2 gas audit, E-01: a cooling event
  # after the window, or one before it, refused a window whose temperature
  # never changed, and a tube in force only outside the window sent the
  # arterial values): the one in force at its start, then each later step
  # before its end.
  steps <- gr$bodySteps
  inWindow <- NULL
  rowBody <- function(i) body
  if (!is.null(steps)) {
    first <- max(1L, findInterval(minimum, steps$time))
    last <- max(first, findInterval(maximum, steps$time, left.open = TRUE))
    inWindow <- first:last
    body <- steps$body[[first]]
    same <- vapply(steps$body[inWindow],
                   function(b) identical(b$temperature, body$temperature), TRUE)
    if (!all(same))
      stop("The Visualizer cannot yet draw an inhaled agent whose body temperature ",
           "changes during the run (physiology events); it can draw one ",
           "whose cardiac output, altitude or intubation changes.")
    rowBody <- function(i) steps$body[[gr$bodyRow[[i]]]]
  }
  # The tissues' capacities at the temperature of the window drawn
  props <- gasPropertiesAt(body$temperature)
  pr <- props[props$gas == g, ]
  ltg <- gasPartitionTissueGas(pr)
  bodyAt <- function(t) if (is.null(steps)) body else
    steps$body[[max(1L, findInterval(t, steps$time))]]
  # The alveolar gas volume in force at each of the viewer's times: one lung's
  # share while a tube is endobronchial.  The lungs are drawn at the largest
  # (both lungs, where the tube is tracheal at any time in the window).
  alveolar <- if (is.null(steps)) NULL else vapply(times, function(t) bodyAt(t)$V_alveolar, 0)
  alveolarSteps <- !is.null(alveolar) && diff(range(alveolar)) > 0
  shunted <- gasShunt(body) > 0 ||
    (!is.null(steps) && any(vapply(steps$body[inWindow], gasShunt, 0) > 0))
  # The cardiac-output slider splits the brain and heart (state 3) from the
  # rest of the vessel-rich group (state 6; gasTissueShares()).  The viewer
  # draws the whole group: its volume, and the partial pressure that holds the
  # agent both parts hold (one partition coefficient, so volume-weighted).
  # Mixed venous blood, from every state, before the vessel-rich group's two
  # parts are merged for drawing.
  venous <- vapply(seq_len(nrow(state)), function(i) gasMixedVenous(state[i, ], rowBody(i)), 0)
  # Arterial blood: end-tidal, unless an endobronchial tube shunts part of the
  # cardiac output past the ventilated lung (gasArterial(), R/endobronchial.R)
  arterial <- vapply(seq_len(nrow(state)), function(i) gasArterial(state[i, ], rowBody(i)), 0)
  vrgVolume <- body$V_brain
  if (!is.null(body$V_vrgRest) && ncol(state) >= 6) {
    vrgVolume <- body$V_brain + body$V_vrgRest
    state[, 3] <- (body$V_brain * state[, 3] + body$V_vrgRest * state[, 6]) / vrgVolume
  }
  vol <- c(circuit = if (identical(gr$circuit, "semi-closed")) body$V_circuit else 0,
           lungs = if (is.null(alveolar)) body$V_alveolar else max(alveolar),
           vrg = vrgVolume, mg = body$V_muscle, fg = body$V_fat)
  lambda <- c(circuit = 1, lungs = 1, vrg = ltg[["brain"]], mg = ltg[["muscle"]], fg = ltg[["fat"]])
  capacity <- vol * lambda

  # Linear between the engine's own points, as the plot draws them.
  at <- function(v) pmax(0, stats::approx(gr$time, v, times, rule = 2, ties = "ordered")$y)
  p <- stats::setNames(lapply(seq_len(5), function(j) at(state[, j])), names(vol))

  # The settings in force: each a step that changes only at a dose row.
  rows <- gr$gasDose
  rows <- data.frame(Time = as.numeric(rows$Time), Drug = as.character(rows$Drug),
                     Dose = as.numeric(rows$Dose), stringsAsFactors = FALSE)
  rows <- rows[!is.na(rows$Time) & !is.na(rows$Dose), , drop = FALSE]
  rows <- rows[order(rows$Time), , drop = FALSE]
  bySetting <- split(rows, rows$Drug)
  knots <- sort(unique(c(0, rows$Time[rows$Time > 0], steps$time[steps$time > 0])))
  deadSpace <- if (is.null(gr$deadSpace)) GAS_DEAD_SPACE_FRACTION else gr$deadSpace
  # Partial pressures at the body's altitude (R/altitude.R), so the vapour
  # delivered is in sea-level millilitres, as the amounts are
  settings <- lapply(knots, function(k) gasSettingsAt(bySetting, k, deadSpace,
                                                      gasPressureRatio(bodyAt(k)),
                                                      gasFreshGasExpansion(bodyAt(k))))
  # `flow` is what the flowmeters read: the engine takes the fresh gas in the
  # lungs' litres (gasFreshGasExpansion()), but the Visualizer shows the
  # setting, and counts every amount of vapour in litres at the machine
  # (gasMachineLitres(), the run's amountScale), so that the agent delivered
  # is the flow times the vaporiser's setting, as a clinician reckons it
  expansion <- vapply(knots, function(k) gasFreshGasExpansion(bodyAt(k)), numeric(1))
  flow <- vapply(settings, function(s) s$Q, numeric(1)) / expansion
  fresh <- vapply(settings, function(s) unname(s$Ffgf[[g]]), numeric(1))
  mv <- vapply(settings, function(s) s$MV, numeric(1))
  va <- vapply(settings, function(s) s$VA, numeric(1))
  # mL of vapour a minute: L/min x % / 100 x 1000 mL/L.
  rate <- 10 * flow * fresh
  i <- findInterval(times + 1e-9, knots)
  deliveredAtKnot <- c(0, cumsum(rate[-length(knots)] * diff(knots)))
  delivered <- deliveredAtKnot[i] + rate[i] * (times - knots[i])

  mac <- macForAge(pr$MAC40, gr$age)
  list(
    format = "stanpumpR-viewer", version = 2, kind = "gas",
    drug = g,
    model = "GasMan (Philip 1986), as stanpumpR solves it",
    category = if (length(category) == 1) category else NA_character_,
    typical = mac, mac = mac,
    units = list(conc = "%", amount = "mL", rate = "mL/min"),
    amountScale = 10 * gasMachineLitres(body),
    patient = list(weight = gr$weight, age = gr$age),
    # The physiology panel; altitude, intubation, temperature and cardiac
    # output as the gas engine ran (its body)
    physiology = viewerPhysiology(physiology, body = body),
    # With physiology events, the settings in force from each time (seconds)
    physiologySteps = if (!is.null(steps))
      viewerPhysiologySteps(gr$physiologyEvents, minimum, body = body, end = maximum),
    circuit = gr$circuit,
    volume = as.list(vol), partition = as.list(lambda), capacity = as.list(capacity),
    t = times * 60, p = p, venous = at(venous),
    arterial = if (shunted) at(arterial) else NULL,
    # The lungs' capacity in force at each time, where the intubation steps
    # (their volume: the gas phase's coefficient is 1); else NULL, and
    # `capacity$lungs` holds throughout
    lungsCapacity = if (alveolarSteps) alveolar else NULL,
    # The arterial saturation and PaO2 where an endobronchial tube shunts
    # blood (R/arterialOxygen.R; Shafer, 2026-11-19), shown under the title
    saturation = if (shunted && !is.null(gr$arterialOxygen)) at(gr$arterialOxygen$SaO2) else NULL,
    paO2 = if (shunted && !is.null(gr$arterialOxygen)) at(gr$arterialOxygen$PaO2) else NULL,
    # One number, or with physiology events the output in force at each time
    cardiacOutput = if (is.null(steps)) (if (is.null(gr$Qco)) body$Q_cardiac else gr$Qco)
                    else if (!is.null(gr$cardiacOutput)) gr$cardiacOutput
                    else vapply(times, function(t) bodyAt(t)$Q_cardiac, 0),
    fresh = fresh[i], freshFlow = flow[i], ventilation = mv[i], alveolarVentilation = va[i],
    delivered = delivered,
    greenhouse = gasGreenhouse(g),
    source = "stanpumpR")
}

#' The plot's Visualizer button
#'
#' Beside Add new drug (Devin's stress test of 2026-11-08, on Shafer's
#' decision), so the Visualizer is found without knowing to double-click a
#' panel: hidden when no plotted drug can be drawn, one button for one drug,
#' a list that drops up for several.  Plain buttons, as in the Edit doses
#' dialog: inst/www/viewer-launch.js opens the pop-up on the click itself
#' (one opened later would be blocked) and only then sends the drug
#' (\code{input$openViewerDrug}).
#'
#' @param drugs the drugs the Visualizer can draw now
#' @returns a tag, or NULL
#' @noRd
viewerPlotButton <- function(drugs)
{
  if (length(drugs) == 0) return(NULL)
  label <- list(shiny::icon("cube"), "Visualizer")
  if (length(drugs) == 1)
    return(htmltools::tags$button(
      type = "button", class = "btn btn-sm btn-outline-primary viewer-plot-button",
      `data-viewer-drug` = drugs, title = paste("Watch", drugs, "in the Visualizer"), label))
  htmltools::div(class = "btn-group dropup viewer-plot-button",
    htmltools::tags$button(type = "button", class = "btn btn-sm btn-outline-primary dropdown-toggle",
                           `data-bs-toggle` = "dropdown", `aria-expanded` = "false", label),
    htmltools::tags$ul(class = "dropdown-menu",
      lapply(drugs, function(d) htmltools::tags$li(htmltools::tags$button(
        type = "button", class = "dropdown-item", `data-viewer-drug` = d, d)))))
}

#' Whether a drug's plotted curve follows the physiology events
#' @param entry the drug's entry
#' @keywords internal
viewerFollowsPhysiologyEvents <- function(entry)
{
  e <- entry[["physiologyEvents"]]
  is.data.frame(e) && nrow(e) > 0 && is.null(entry$physiologyEventsIgnored) &&
    is.null(entry$interactionEvents)
}

#' The physiology steps a run carries with physiology events
#'
#' @param events physiologyEventsFrom()'s data frame
#' @param start the run's first time, minutes
#' @param body a gas run's body, to add the altitude and the intubation
#' @param end the run's last time, minutes: a step at or after it is not in
#'   the run (ChatGPT's round-2 gas audit, E-01)
#' @returns a list of settings with `time`, the first at `start`, or NULL
#' @keywords internal
viewerPhysiologySteps <- function(events, start, body = NULL, end = Inf)
{
  if (!is.data.frame(events) || nrow(events) == 0) return(NULL)
  states <- physiologyEventStates(events)
  times <- c(0, events$Time)
  # The step in force at the run's start, then each later one before its end
  first <- max(which(times <= start))
  keep <- seq_along(times) >= first & (seq_along(times) == first | times < end)
  lapply(which(keep), function(i) {
    x <- states[[i]]
    step <- lapply(x[c(PHYSIOLOGY_FACTORS, "temperature")], unname)
    # Seconds, as the run's own times
    step$time <- 60 * (if (i == first) start else times[[i]])
    if (!is.null(body)) {
      step$altitude <- unname(x[["altitude"]])
      step$pressure <- round(barometricPressure(step$altitude))
      step$endobronchial <- x[["endobronchial"]] == 1
      # The shunt fraction, percent (R/endobronchial.R)
      if (step$endobronchial) step$shunt <- round(100 * unname(x[["shunt"]]), 1)
    }
    step
  })
}

#' The physiology panel as a run carries it to the Visualizer
#'
#' The five controls of every run (the four organ multiples and the body
#' temperature) and, for an inhaled agent, the altitude and endobronchial
#' intubation, read from the gas body the engine ran on.  Shafer, 2026-11-19:
#' "the status of all physiology sliders should appear on 2D and 3D graphs in
#' the visualizer".
#'
#' @param physiology the physiology factors (\code{PHYSIOLOGY_DEFAULT}'s
#'   names), or NULL for normal
#' @param body a gas body, or NULL for an intravenous drug's run
#' @return a named list of numbers (and a logical for the tube)
#' @keywords internal
viewerPhysiology <- function(physiology = NULL, body = NULL)
{
  p <- tryCatch(checkPhysiology(physiology), error = function(e) PHYSIOLOGY_DEFAULT)
  out <- lapply(p[c(PHYSIOLOGY_FACTORS, "temperature")], function(x) unname(x))
  if (!is.null(body)) {
    # What the engine ran on wins over the panel's reading
    if (!is.null(body$temperature)) out$temperature <- body$temperature
    out$altitude <- if (is.null(body$altitude)) ALTITUDE_DEFAULT else body$altitude
    out$pressure <- round(barometricPressure(out$altitude))
    out$endobronchial <- isTRUE(body$endobronchial)
    # The shunt fraction, percent (R/endobronchial.R; Shafer, 2026-11-25)
    if (out$endobronchial) out$shunt <- round(100 * gasShunt(body), 1)
  }
  out
}
