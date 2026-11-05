# stanpumpR viewer (the Minto Visualizer) — the run file
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
# The viewer computes no pharmacokinetics. viewerSimulate() runs the app's own
# pipeline (recalculatePK() -> processdoseTable(), with every covariate the
# plot used) at the viewer's times, and viewerRun() reads the result. So the
# vessels show exactly what stanpumpR computes, for any model it runs: linear
# or saturable, closed form, transit chain or Runge-Kutta.
#
# The flows are model-agnostic too: the viewer draws each arrow from the
# change in the amounts, not from clearances. "given" is the cumulative drug
# that has entered the body (intravenous rates and boluses, the apparent-oral
# rates that run straight into V1, and the drug absorbed from the depot, read
# off the fall of stanpumpR's Depot column); the drug eliminated is then
# given less the drug in V1 + V2 + V3, whatever the elimination is.
#
# Time is in seconds in the file (minutes in stanpumpR). Amounts and rates
# are in stanpumpR's base unit for the drug (mg for a drug in mcg/mL, mcg
# for one in ng/mL), a rate per minute, a concentration per L (= the
# drug's own unit per mL).

# The base amount and concentration units for each Concentration.Units the
# viewer draws. Other drugs (gases, and those measured in IU, mmol, INR, ...)
# are not offered.
VIEWER_UNITS <- list(
  mcg = list(conc = "mcg/mL", amount = "mg",  amountPerConc = c(mg = 1,    mcg = 1000, ng = 1e6)),
  ng  = list(conc = "ng/mL",  amount = "mcg", amountPerConc = c(mg = 1e-3, mcg = 1,    ng = 1000)),
  pg  = list(conc = "pg/mL",  amount = "ng",  amountPerConc = c(mg = 1e-6, mcg = 1e-3, ng = 1)))

# At most about this many times in a run: the step is 1 s, or longer on a
# long plot.
VIEWER_MAX_TIMES <- 7200

viewerUnits <- function(concUnits)
{
  u <- VIEWER_UNITS[[concUnits]]
  if (is.null(u))
    stop("The viewer draws drugs measured in mcg/mL, ng/mL or pg/mL; this one is in ",
         concUnits, "/mL.")
  u
}

#' Why the viewer cannot draw a simulated drug
#'
#' @param entry one drug's entry of the app's simulation (`drugs()`), or of
#'   `processdoseTable()`; the PK fields alone (`recalculatePK()`) suffice.
#' @return NULL when the viewer can draw it, else one sentence saying why not.
#' @noRd
viewerProblem <- function(entry)
{
  if (is.null(entry)) return("There is no simulation of this drug to show.")
  if (isTRUE(isGasDrug(entry$drug))) return("The viewer does not draw inhaled agents.")
  if (is.null(VIEWER_UNITS[[entry$Concentration.Units]]))
    return("The viewer draws drugs measured in mcg/mL, ng/mL or pg/mL.")
  if (length(entry$parallelSystems) > 0)
    return(paste("This drug is the sum of separate systems (such as two enantiomers),",
                 "which one set of compartments cannot show."))
  NULL
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

# A dose in its units (mg, mcg/kg, mg/kg/hr, ...) to base units (amount, or
# amount per minute), as simCpCe() converts it.
viewerBaseDose <- function(dose, units, weight, u)
{
  units <- as.character(units)
  f <- rep(NA_real_, length(units))
  f[grepl("^g( |/|$)", units)] <- 1000 / u$amountPerConc[["mg"]]
  f[grepl("mg", units)]  <- 1 / u$amountPerConc[["mg"]]
  f[grepl("mcg", units)] <- 1 / u$amountPerConc[["mcg"]]
  f[grepl("ng", units)]  <- 1 / u$amountPerConc[["ng"]]
  out <- dose * f
  out[grepl("kg", units)]   <- out[grepl("kg", units)] * weight
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

#' Simulate the dose table for the viewer
#'
#' The app's own pipeline, as `drugs()` runs it, but with the viewer's
#' times added to every time line, so the run is sampled from the very
#' solution the plot draws.
#'
#' @param drug the drug's name, as in the dose table.
#' @param doseTable the cleaned dose table (Time in minutes).
#' @param events the cleaned event table.
#' @param drugs the drugs' PK, from `recalculatePK()` (the app's `drugPK()`).
#' @param minimum,maximum the plot's Min and Max time, minutes.
#' @param antiseizureInteractions as for `processdoseTable()`; with it off
#'   only `drug`'s rows are simulated.
#' @return the drug's simulated entry.
#' @noRd
viewerSimulate <- function(drug, doseTable, events, drugs, minimum, maximum,
                           antiseizureInteractions = FALSE)
{
  if (!isTRUE(antiseizureInteractions)) {
    doseTable <- doseTable[doseTable$Drug == drug, , drop = FALSE]
    drugs <- drugs[names(drugs) == drug]
  }
  if (is.null(drugs[[drug]])) stop("No simulation for ", drug, " in this dose table.")
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
viewerRun <- function(entry, minimum, maximum, category = NA_character_)
{
  problem <- viewerProblem(entry)
  if (!is.null(problem)) stop(problem)
  times <- viewerTimes(minimum, maximum)
  minimum <- times[1]
  u <- viewerUnits(entry$Concentration.Units)
  k <- entry$PK[[1]]
  weight <- entry$weight

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
  if (!is.null(c3) && !isTRUE(k$v3 > 0)) c3 <- NULL
  if (!is.null(c2) && !isTRUE(k$v2 > 0)) { c2 <- NULL; c3 <- NULL }

  # The drug's dose rows, scheduled doses expanded into their repeats.
  d <- entry$DT
  if (is.null(d)) d <- data.frame(Drug = character(), Time = double(), Dose = double(), Units = character())
  d$Time <- as.numeric(d$Time)
  d <- expandScheduledDoses(d, maximum)$dose
  d <- d[d$Time <= maximum, , drop = FALSE]
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
  manual <- d[route == ROUTE_IV & rate & !tci, , drop = FALSE]
  steps <- data.frame(Time = manual$Time,
                      Rate = viewerBaseDose(manual$Dose, manual$Units, weight, u),
                      rank = rep(1, nrow(manual)))
  tciRates <- entry$tci$rates
  if (!is.null(tciRates) && nrow(tciRates) > 0) {
    tr <- viewerBaseDose(tciRates$Rate, tciRates$Units, weight, u)
    steps <- rbind(steps, data.frame(Time = tciRates$Time, Rate = tr,
                                     rank = ifelse(tr > 0, 2, 0)))
  }
  steps <- steps[order(steps$Time, steps$rank), , drop = FALSE]
  rateAt <- rateInForce(steps, times)

  # Apparent-oral rates ("mg/day PO"): simCpCe() runs them straight into V1
  # on the apparent parameters, so they enter the body as they are given.
  oral <- d[rate & !tci & route != ROUTE_IV & !(route %in% TISSUE_ROUTES) & !enteral, , drop = FALSE]
  oralSteps <- data.frame(Time = oral$Time, Rate = viewerBaseDose(oral$Dose, oral$Units, weight, u))
  oralSteps <- oralSteps[order(oralSteps$Time), , drop = FALSE]

  # IV boluses, in base amount.
  b <- d[route == ROUTE_IV & !rate & !tci, , drop = FALSE]
  bAmount <- viewerBaseDose(b$Dose, b$Units, weight, u)
  bolusGiven <- vapply(times, function(t) sum(bAmount[b$Time <= t + 1e-9]), numeric(1))

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
  absorbed <- absorbedFromDepot + cumulativeRate(oralSteps, times)
  given <- cumulativeRate(steps[, c("Time", "Rate")], times) + bolusGiven + absorbed

  # Some inputs are rewritten by the engine in ways the dose rows do not
  # show (zero-order releases, an enteral infusion, saturable SC input), and
  # a metabolite's input is its formation: the drug given is then unknown,
  # and the viewer says so rather than draw a wrong elimination.
  inputKnown <- is.null(entry$oralZeroOrder) && is.null(entry$zeroOrderSC) &&
    !isTRUE(entry$enteralInfusion) && is.null(entry$michaelisMenten$saturableSC) &&
    !any(enteral) && nrow(d) > 0

  # The target and the mode in force: a target row starts TCI (a target of 0
  # stops it), and a later manual infusion row stops it.
  targets <- d[tci, , drop = FALSE]
  stops <- manual$Time
  modeAt <- character(length(times)); targetAt <- numeric(length(times))
  for (j in seq_along(times)) {
    t <- times[j]
    i <- which(targets$Time <= t + 1e-9)
    if (!length(i)) { modeAt[j] <- "manual"; next }
    last <- targets[max(i), ]
    if (last$Dose <= 0 || any(stops > last$Time - 1e-9 & stops <= t + 1e-9)) { modeAt[j] <- "manual"; next }
    modeAt[j] <- if (last$Units == TCI_UNIT_EFFECT) "effect" else "plasma"
    targetAt[j] <- last$Dose
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
    patient = list(weight = weight, height = entry$height, age = entry$age, sex = entry$sex),
    pk = list(v1 = k$v1, v2 = k$v2, v3 = k$v3, cl1 = k$cl1, cl2 = k$cl2, cl3 = k$cl3),
    t = times * 60, cp = cp, ce = ce, c2 = c2, c3 = c3,
    rate = rateAt, target = targetAt, mode = modeAt,
    boluses = boluses,
    depot = depot,
    absorbed = if (any(absorbed > 0)) absorbed else NULL,
    given = if (inputKnown) given else NULL,
    inputKnown = inputKnown,
    source = "stanpumpR")
}
