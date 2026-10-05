# =============================================================================
# Five scenarios demonstrating the app's inhaled-gas engine against Gas Man
# =============================================================================
#
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
# Steven L. Shafer, after reading the six weeks of correspondence with Richard
# Epstein about the Gas Man validation.  Run on R 4.6.1; its output is
# gasman_engine_scenarios_results.csv and gasman_engine_scenarios_settings.csv
# in this directory, and tests/testthat/test-gas-scenarios.R holds a fast subset
# as a regression test.
#
# What this is for
# ----------------
# The earlier five-case grid (gasman_validation_grid.R) established that
# advanceGasManBaseline() -- Gas Man's stepping scheme restated in R -- IS Gas
# Man, to the limit of Gas Man's float32 arithmetic (worst 6.2e-04, in FAT).
# What the app actually runs is a different routine, advanceClosedFormGas().
# These scenarios put THAT routine, at the resolution the app uses, beside Gas
# Man.
#
# Three numbers are reported for every compartment and time:
#
#   gasman   Gas Man as it runs: the baseline at its native 6-second tick.
#   limit    What Gas Man's equations converge to as the tick shrinks, by
#            Richardson extrapolation of the baseline (it converges first order,
#            so limit ~ 2 f(h/2) - f(h)).  This is the answer both programs are
#            approximating.
#   engine   advanceClosedFormGas() at the app's own resolution (601 points).
#
# The engine and Gas Man do not agree digit for digit at a fixed step and are
# not meant to: Gas Man splits each tick into sequential updates, the engine
# advances each step exactly.  The demonstration is that the engine lands on
# the limit, and that the gap between the engine and Gas Man is Gas Man's own
# distance from that limit.
#
# Why these five
# --------------
#   1. Sevoflurane wash-in at high flow.  The anchor: Epstein has already run
#      this in Gas Man (his Scenario 1).
#   2. The same with 70% nitrous oxide.  The second gas effect; also already run
#      in Gas Man (his Scenario 2).  The flows are chosen so that DELIVERED
#      nitrous oxide is exactly 70% after the vaporiser displaces 2% of carrier.
#   3. A whole anaesthetic over three hours: a target, a step up, a step down at
#      low flow, then vaporiser off at high flow.  Epstein proposed this on
#      2026-09-06; every earlier run was wash-in at constant settings, so this is
#      the first to exercise setting changes and EMERGENCE, where the uptake
#      coupling changes sign.
#   4. Desflurane, wash-in at 4 L/min then low-flow maintenance at 0.5 L/min.
#      The circuit equation dominates and rebreathing matters most.
#   5. A 100 kg patient at Gas Man's own weight-scaled defaults.  Nothing other
#      than 70 kg has been compared before, and the allometric defaults adopted
#      on 2026-10-05 are live here.  Same dial and flow as the old grid's case 3,
#      so the difference from that case is the weight scaling alone.
#
# Usage
# -----
#   devtools::load_all(".")
#   source("inst/validation/gasman_engine_scenarios.R")
#   out <- runGasEngineScenarios()        # a few minutes; writes the two CSVs
#   summariseGasEngineScenarios(out)
#
# Sourcing the file only defines functions; nothing runs until asked.
# =============================================================================


# One row per setting, in the app's own dose-table vocabulary.
gasScenarioRows <- function(...)
{
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r)
    data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]],
               stringsAsFactors = FALSE)))
}


#' The five scenarios
#'
#' Each has the dose table exactly as the engine receives it, the patient
#' weight, the cardiac output and ventilation in force, the agents to report,
#' and the times at which to report them.
gasEngineScenarios <- function()
{
  # Gas Man's defaults at a given weight: VA 4 and CO 5 at 70 kg, scaled by
  # (weight / 70)^0.75.  Written out rather than taken from getGasBody() so the
  # scenario does not silently move if the app's defaults are changed later.
  allo <- function(x70, weight) x70 * (weight / 70)^0.75

  # Scenario 2: flows giving exactly 70% delivered nitrous oxide.  The vaporiser
  # displaces 2% of the carrier, so Q_N2O / Q * 0.98 = 0.70.
  n2o <- 8 * 0.70 / 0.98

  list(
    list(
      id = 1, name = "Sevoflurane wash-in, high flow",
      weight = 70, CO = 5, maximum = 30,
      agents = "sevoflurane",
      times = c(1, 2, 5, 10, 15, 20, 30),
      DT = gasScenarioRows(
        list(0, "oxygen", 8), list(0, "ventilation", 4),
        list(0, "sevoflurane", 2))
    ),
    list(
      id = 2, name = "Second gas effect: sevoflurane with 70% nitrous oxide",
      weight = 70, CO = 5, maximum = 30,
      agents = c("sevoflurane", "nitrousOxide"),
      times = c(1, 2, 5, 10, 15, 20, 30),
      DT = gasScenarioRows(
        list(0, "oxygen", 8 - n2o), list(0, "nitrousOxide", n2o),
        list(0, "ventilation", 4), list(0, "sevoflurane", 2))
    ),
    list(
      id = 3, name = "Whole anaesthetic: step up, step down, emergence",
      weight = 70, CO = 5, maximum = 180,
      agents = "sevoflurane",
      times = c(5, 15, 30, 35, 45, 60, 65, 90, 120, 150, 151, 155, 160, 180),
      DT = gasScenarioRows(
        list(0,   "ventilation", 4),
        list(0,   "oxygen", 6),  list(0,   "sevoflurane", 2),
        list(30,  "sevoflurane", 3),
        list(60,  "oxygen", 2),  list(60,  "sevoflurane", 1.5),
        list(150, "oxygen", 10), list(150, "sevoflurane", 0))
    ),
    list(
      id = 4, name = "Desflurane: wash-in at 4 L/min, then low flow at 0.5 L/min",
      weight = 70, CO = 5, maximum = 60,
      agents = "desflurane",
      times = c(1, 2, 5, 10, 11, 15, 20, 30, 45, 60),
      DT = gasScenarioRows(
        list(0,  "ventilation", 4),
        list(0,  "oxygen", 4),   list(0,  "desflurane", 6),
        list(10, "oxygen", 0.5), list(10, "desflurane", 8))
    ),
    list(
      id = 5, name = "100 kg patient at Gas Man's weight-scaled defaults",
      weight = 100, CO = allo(5, 100), maximum = 30,
      agents = "isoflurane",
      times = c(1, 2, 5, 10, 15, 20, 30),
      DT = gasScenarioRows(
        list(0, "oxygen", 2), list(0, "ventilation", allo(4, 100)),
        list(0, "isoflurane", 1.2))
    )
  )
}


# The engine's five states, named as Gas Man names them.  Gas Man's VEN has no
# counterpart: the engine mixes venous blood instantaneously.
GAS_SCENARIO_COMPARTMENTS <- c("CKT", "ALV", "VRG", "MUS", "FAT")


#' Run the app engine on one scenario
#'
#' @param s one element of \code{gasEngineScenarios()}
#' @param resolution number of output points; 601 is what the app uses
#' @returns data frame of Agent, Time and the five compartments
gasScenarioEngine <- function(s, resolution = 601)
{
  sim <- advanceClosedFormGas(s$DT, weight = s$weight, maximum = s$maximum,
                              cardiacOutput = s$CO, resolution = resolution)
  do.call(rbind, lapply(s$agents, function(g) {
    m <- sim$state[[g]][, 1:5, drop = FALSE]
    out <- as.data.frame(lapply(seq_len(5), function(j)
      stats::approx(sim$timeLine, m[, j], s$times, ties = "ordered")$y))
    names(out) <- GAS_SCENARIO_COMPARTMENTS
    cbind(Agent = g, Time = s$times, out)
  }))
}


#' Run the Gas Man baseline on one scenario
#'
#' Nitrogen starts at the engine's 78.07% rather than Gas Man's 80%, so that
#' the comparison is of the integration and not of that documented difference.
#'
#' @param s one element of \code{gasEngineScenarios()}
#' @param dt tick in minutes; 0.1 is Gas Man's 6000 ms
#' @returns data frame of Agent, Time and the five compartments
gasScenarioBaseline <- function(s, dt = 0.1)
{
  # Record on the tick grid itself, so that reported values are states the
  # baseline actually computed rather than held-over ones.
  b <- advanceGasManBaseline(s$DT, weight = s$weight, maximum = s$maximum,
                             cardiacOutput = s$CO, dt = dt,
                             resolution = round(s$maximum / dt) + 1,
                             nitrogenAmbient = AIR_FRACTION_N2 * 100)
  r <- b$results
  do.call(rbind, lapply(s$agents, function(g) {
    out <- as.data.frame(lapply(GAS_SCENARIO_COMPARTMENTS, function(cmp) {
      d <- r[r$Drug == g & r$Site == cmp, ]
      stats::approx(d$Time, d$Y, s$times, ties = "ordered")$y
    }))
    names(out) <- GAS_SCENARIO_COMPARTMENTS
    cbind(Agent = g, Time = s$times, out)
  }))
}


#' The limit Gas Man's equations converge to as the tick shrinks
#'
#' Richardson extrapolation of the first-order baseline.
#'
#' @param s one element of \code{gasEngineScenarios()}
#' @param refine the two ticks used are 0.1 / refine and 0.1 / (2 * refine)
gasScenarioLimit <- function(s, refine = 16)
{
  coarse <- gasScenarioBaseline(s, dt = 0.1 / refine)
  fine   <- gasScenarioBaseline(s, dt = 0.1 / (2 * refine))
  out <- fine
  out[GAS_SCENARIO_COMPARTMENTS] <-
    2 * fine[GAS_SCENARIO_COMPARTMENTS] - coarse[GAS_SCENARIO_COMPARTMENTS]
  out
}


#' The settings to enter in Gas Man for each scenario
#'
#' Gas Man takes DELIVERED tensions, where the app takes flowmeter and vaporiser
#' settings, so each segment's settings are converted here.  One row per
#' scenario, segment and agent.
gasScenarioSettings <- function(scenarios = gasEngineScenarios())
{
  gasManName <- c(sevoflurane = "Sevoflurane", isoflurane = "Isoflurane",
                  desflurane = "Desflurane", nitrousOxide = "Nitrous Oxide",
                  nitrogen = "Nitrogen")
  do.call(rbind, lapply(scenarios, function(s) {
    DT <- s$DT[order(s$DT$Time), ]
    bySetting <- split(DT, DT$Drug)
    starts <- sort(unique(DT$Time))
    ends   <- c(starts[-1], s$maximum)
    do.call(rbind, lapply(seq_along(starts), function(i) {
      st <- gasSettingsAt(bySetting, starts[i])
      # NITROGEN MUST BE ADDED AS AN AGENT IN GAS MAN, at its delivered value
      # (0 unless air is flowing).  The app engine always carries nitrogen, and
      # its washout feeds the uptake coupling.  Gas Man only does so if nitrogen
      # is one of the agents in the run; Epstein's September runs did not
      # include it, which by itself moves alveolar sevoflurane by 0.3-0.5%.
      do.call(rbind, lapply(c(s$agents, "nitrogen"), function(g) data.frame(
        Scenario = s$id, Name = s$name, Weight_kg = s$weight,
        Circuit = "Semi-closed", Uptake = "on", Return = "on", dt_ms = 6000,
        From_min = starts[i], To_min = ends[i],
        Agent = gasManName[[g]], DEL_percent = round(st$Ffgf[[g]], 4),
        FGF_L_min = round(st$Q, 4), VA_L_min = round(st$VA, 4),
        CO_L_min = round(s$CO, 4),
        stringsAsFactors = FALSE)))
    }))
  }))
}


#' Run all five scenarios and write the results
#'
#' @param outdir where to write the two CSV files; NULL to write nothing
#' @param refine passed to \code{gasScenarioLimit()}
#' @returns long data frame: one row per scenario, agent, time and compartment
runGasEngineScenarios <- function(outdir = "inst/validation", refine = 16)
{
  scenarios <- gasEngineScenarios()
  long <- do.call(rbind, lapply(scenarios, function(s) {
    message("Scenario ", s$id, ": ", s$name)
    eng <- gasScenarioEngine(s)
    gm  <- gasScenarioBaseline(s, dt = 0.1)
    lim <- gasScenarioLimit(s, refine)
    do.call(rbind, lapply(GAS_SCENARIO_COMPARTMENTS, function(cmp) {
      d <- data.frame(
        Scenario = s$id, Agent = eng$Agent, Minute = eng$Time, Compartment = cmp,
        engine = eng[[cmp]], gasman = gm[[cmp]], limit = lim[[cmp]],
        stringsAsFactors = FALSE)
      # Differences as a percentage of that compartment's peak in the run, so
      # that a near-zero value early in a wash-in does not manufacture a large
      # relative error out of nothing.
      peak <- stats::ave(abs(d$limit), d$Agent, FUN = max)
      d$engine_vs_limit_pct  <- 100 * (d$engine - d$limit)  / peak
      d$gasman_vs_limit_pct  <- 100 * (d$gasman - d$limit)  / peak
      d$engine_vs_gasman_pct <- 100 * (d$engine - d$gasman) / peak
      d
    }))
  }))

  if (!is.null(outdir))
  {
    utils::write.csv(format(long, digits = 10),
                     file.path(outdir, "gasman_engine_scenarios_results.csv"),
                     row.names = FALSE, quote = FALSE)
    utils::write.csv(gasScenarioSettings(scenarios),
                     file.path(outdir, "gasman_engine_scenarios_settings.csv"),
                     row.names = FALSE)
  }
  long
}


#' Worst disagreement per scenario and compartment
#'
#' @param long output of \code{runGasEngineScenarios()}
#' @returns data frame of the largest absolute percentage differences
summariseGasEngineScenarios <- function(long)
{
  worst <- function(x) max(abs(x))
  agg <- stats::aggregate(
    cbind(engine_vs_limit_pct, gasman_vs_limit_pct, engine_vs_gasman_pct) ~
      Scenario + Agent + Compartment, data = long, FUN = worst)
  agg <- agg[order(agg$Scenario, agg$Agent,
                   match(agg$Compartment, GAS_SCENARIO_COMPARTMENTS)), ]
  rownames(agg) <- NULL
  agg
}
