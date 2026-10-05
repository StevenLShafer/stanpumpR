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


# The two circuit models, and what Gas Man calls them in a scenario file.  The
# app engine defaults to "ideal" (since 2026-10-05); Gas Man defaults to
# "semi-closed".  Every comparison below is like with like: the same circuit on
# both sides.
GAS_SCENARIO_CIRCUITS <- c(ideal = "Ideal", "semi-closed" = "Semi")

# The engine's five states, named as Gas Man names them.  Gas Man's VEN has no
# counterpart: the engine mixes venous blood instantaneously.
GAS_SCENARIO_COMPARTMENTS <- c("CKT", "ALV", "VRG", "MUS", "FAT")


#' Run the app engine on one scenario
#'
#' @param s one element of \code{gasEngineScenarios()}
#' @param resolution number of output points; 601 is what the app uses
#' @param circuit "ideal" or "semi-closed"
#' @returns data frame of Agent, Time and the five compartments
gasScenarioEngine <- function(s, resolution = 601, circuit = "ideal")
{
  # deadSpace = 0: the scenarios give ALVEOLAR ventilation, as Gas Man takes
  # it.  The app's own default treats the ventilation row as minute ventilation
  # with a 30% dead space, which Gas Man has no counterpart for.
  # oxygenUptake = FALSE: Gas Man has no oxygen, so none of its volume is lost
  # to oxygen consumption.
  sim <- advanceClosedFormGas(s$DT, weight = s$weight, maximum = s$maximum,
                              cardiacOutput = s$CO, resolution = resolution,
                              circuit = circuit, deadSpace = 0,
                              oxygenUptake = FALSE)
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
#' @param circuit "ideal" or "semi-closed"
#' @returns data frame of Agent, Time and the five compartments
gasScenarioBaseline <- function(s, dt = 0.1, circuit = "ideal")
{
  # Record on the tick grid itself, so that reported values are states the
  # baseline actually computed rather than held-over ones.
  b <- advanceGasManBaseline(s$DT, weight = s$weight, maximum = s$maximum,
                             cardiacOutput = s$CO, dt = dt,
                             resolution = round(s$maximum / dt) + 1,
                             nitrogenAmbient = AIR_FRACTION_N2 * 100,
                             circuit = circuit)
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
#' @param circuit "ideal" or "semi-closed"
gasScenarioLimit <- function(s, refine = 16, circuit = "ideal")
{
  coarse <- gasScenarioBaseline(s, dt = 0.1 / refine, circuit = circuit)
  fine   <- gasScenarioBaseline(s, dt = 0.1 / (2 * refine), circuit = circuit)
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
gasScenarioSettings <- function(scenarios = gasEngineScenarios(), circuit = "ideal")
{
  gasManName <- GAS_SCENARIO_GASMAN_NAMES
  do.call(rbind, lapply(scenarios, function(s) {
    DT <- s$DT[order(s$DT$Time), ]
    bySetting <- split(DT, DT$Drug)
    starts <- sort(unique(DT$Time))
    ends   <- c(starts[-1], s$maximum)
    do.call(rbind, lapply(seq_along(starts), function(i) {
      st <- gasSettingsAt(bySetting, starts[i], deadSpace = 0)
      # NITROGEN MUST BE ADDED AS AN AGENT IN GAS MAN, at its delivered value
      # (0 unless air is flowing).  The app engine always carries nitrogen, and
      # its washout feeds the uptake coupling.  Gas Man only does so if nitrogen
      # is one of the agents in the run; Epstein's September runs did not
      # include it, which by itself moves alveolar sevoflurane by 0.3-0.5%.
      do.call(rbind, lapply(c(s$agents, "nitrogen"), function(g) data.frame(
        Scenario = s$id, Name = s$name, Weight_kg = s$weight,
        Circuit = if (circuit == "ideal") "Ideal" else "Semi-closed",
        Uptake = "on", Return = "on", dt_ms = 6000,
        From_min = starts[i], To_min = ends[i],
        Agent = gasManName[[g]], DEL_percent = round(st$Ffgf[[g]], 4),
        FGF_L_min = round(st$Q, 4), VA_L_min = round(st$VA, 4),
        CO_L_min = round(s$CO, 4),
        stringsAsFactors = FALSE)))
    }))
  }))
}


# -----------------------------------------------------------------------------
# Running Gas Man itself
# -----------------------------------------------------------------------------
# Gas Man's C++ engine is published at github.com/rasman/gasmanonline (GPL-3.0).
# Its gasman_api directory builds a self-contained command-line runner,
# gasman_run, which reads a sections-based CSV scenario and gasman.ini.  Built
# here on 2026-10-05 from commit d3a2dd3 with the Rtools 4.5 g++ and cmake:
#
#   cmake --fresh -S gasman_api -B build -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release
#   cmake --build build
#
# Neither the source nor the binary is part of this package.  Pass the path to
# gasman_run and to its gasman.ini to runGasEngineScenarios() to add a
# `gasman_cpp` column; without them everything else still runs.

GAS_SCENARIO_GASMAN_NAMES <- c(
  sevoflurane = "Sevoflurane", isoflurane = "Isoflurane",
  desflurane = "Desflurane", nitrousOxide = "Nitrous Oxide",
  nitrogen = "Nitrogen")


#' One scenario as a Gas Man CSV scenario file
#'
#' Nitrogen is always included as the last agent, so that its washout feeds the
#' uptake coupling as it does in the app engine.  dt_ms is pinned at 6000:
#' left alone, Gas Man derives its tick from the patient's weight.
#'
#' @param s one element of \code{gasEngineScenarios()}
#' @param circuit "ideal" or "semi-closed"
#' @returns character vector, one element per line
gasScenarioGasManCsv <- function(s, circuit = "ideal")
{
  agents <- c(s$agents, "nitrogen")
  DT <- s$DT[order(s$DT$Time), ]
  bySetting <- split(DT, DT$Drug)
  hms <- function(m) sprintf("%02d:%02d:%02d", m %/% 60, m %% 60, 0)
  num <- function(x) format(x, digits = 10, scientific = FALSE, trim = TRUE)

  rows <- vapply(sort(unique(DT$Time)), function(t0) {
    st <- gasSettingsAt(bySetting, t0, deadSpace = 0)
    del <- unlist(lapply(agents, function(g) c(num(st$Ffgf[[g]]), "0")))
    paste(c(num(st$VA), num(st$Q), GAS_SCENARIO_CIRCUITS[[circuit]], hms(t0),
            num(s$CO), del), collapse = ",")
  }, character(1))

  header <- paste(c("va", "fgf", "circuit", "time", "co",
                    paste0(rep(c("del", "inject"), length(agents)),
                           rep(seq_along(agents), each = 2))), collapse = ",")
  c(paste0("# Scenario ", s$id, ": ", s$name, "  [", circuit, " circuit]"),
    "# Generated by gasman_engine_scenarios.R -- do not hand-edit.",
    "# Per-agent constants are omitted on purpose so gasman.ini supplies them.",
    "", "[patient]", paste0("weight_kg,", num(s$weight)), "dt_ms,6000", "",
    unlist(lapply(agents, function(g)
      c("[agent]", paste0("name,", GAS_SCENARIO_GASMAN_NAMES[[g]]), ""))),
    "[settings]", header, rows)
}


#' Run one scenario through Gas Man's own gasman_run
#'
#' @param s one element of \code{gasEngineScenarios()}
#' @param exe path to gasman_run
#' @param ini path to gasman.ini
#' @param nitrogenAmbient starting nitrogen.  Gas Man's own is 80; the default
#'   here matches the app engine's room air, by writing a private copy of the
#'   ini with \code{Ambient} changed.  That is a setting, not a code change.
#' @param workdir where the scenario, ini copy and raw output are written
#' @param circuit "ideal" or "semi-closed"
#' @returns data frame of Agent, Time and the five compartments at \code{s$times}
gasScenarioGasManCpp <- function(s, exe, ini,
                                 nitrogenAmbient = AIR_FRACTION_N2 * 100,
                                 workdir = tempfile("gasman"), circuit = "ideal")
{
  dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
  workdir <- normalizePath(workdir)
  iniLines <- readLines(ini, warn = FALSE)
  amb <- grep("^Ambient=", iniLines)
  stopifnot(length(amb) == 1)        # only [Nitrogen] has one
  iniLines[amb] <- paste0("Ambient=", format(nitrogenAmbient, digits = 10))
  iniCopy <- file.path(workdir, "gasman.ini")
  writeLines(iniLines, iniCopy)

  infile  <- file.path(workdir, sprintf("scenario_%d.csv", s$id))
  outfile <- file.path(workdir, sprintf("scenario_%d_gasman.csv", s$id))
  writeLines(gasScenarioGasManCsv(s, circuit), infile)
  # Run from the work directory with bare file names: gasman_run looks for
  # gasman.ini in the current directory (as built 2026-10-05 it does so even
  # when --ini is given), and this way it finds the edited copy either way.
  exe <- normalizePath(exe)
  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)
  status <- system2(exe, c(basename(infile), "--end", s$maximum * 60,
                           "--every", 6, "--ini", "gasman.ini",
                           "--output", basename(outfile)),
                    stdout = TRUE, stderr = TRUE)
  if (!file.exists(outfile)) stop("gasman_run failed: ", paste(status, collapse = " "))

  raw <- utils::read.csv(outfile, stringsAsFactors = FALSE)
  hms <- do.call(rbind, lapply(strsplit(raw$Time, ":"), as.numeric))
  raw$Minute <- hms[, 1] * 60 + hms[, 2] + hms[, 3] / 60
  do.call(rbind, lapply(s$agents, function(g) {
    d <- raw[raw$Agent == GAS_SCENARIO_GASMAN_NAMES[[g]], ]
    out <- as.data.frame(lapply(GAS_SCENARIO_COMPARTMENTS, function(cmp)
      stats::approx(d$Minute, d[[cmp]], s$times, ties = "ordered")$y))
    names(out) <- GAS_SCENARIO_COMPARTMENTS
    cbind(Agent = g, Time = s$times, out)
  }))
}


#' Run all five scenarios and write the results
#'
#' @param outdir where to write the CSV files; NULL to write nothing
#' @param refine passed to \code{gasScenarioLimit()}
#' @param gasmanExe,gasmanIni optional paths to Gas Man's gasman_run and its
#'   gasman.ini.  When given, Gas Man itself is run and reported as
#'   \code{gasman_cpp}.
#' @param circuits which circuit models to run: both by default
#' @returns long data frame: one row per circuit, scenario, agent, time and
#'   compartment
runGasEngineScenarios <- function(outdir = "inst/validation", refine = 16,
                                  gasmanExe = NULL, gasmanIni = NULL,
                                  circuits = c("ideal", "semi-closed"))
{
  scenarios <- gasEngineScenarios()
  useCpp <- !is.null(gasmanExe) && !is.null(gasmanIni)
  long <- do.call(rbind, lapply(circuits, function(circuit)
    do.call(rbind, lapply(scenarios, function(s) {
    message("Scenario ", s$id, " [", circuit, "]: ", s$name)
    eng <- gasScenarioEngine(s, circuit = circuit)
    gm  <- gasScenarioBaseline(s, dt = 0.1, circuit = circuit)
    lim <- gasScenarioLimit(s, refine, circuit = circuit)
    cpp <- if (useCpp) gasScenarioGasManCpp(s, gasmanExe, gasmanIni, circuit = circuit) else NULL
    do.call(rbind, lapply(GAS_SCENARIO_COMPARTMENTS, function(cmp) {
      d <- data.frame(
        Circuit = circuit,
        Scenario = s$id, Agent = eng$Agent, Minute = eng$Time, Compartment = cmp,
        engine = eng[[cmp]], gasman = gm[[cmp]], limit = lim[[cmp]],
        gasman_cpp = if (useCpp) cpp[[cmp]] else NA_real_,
        stringsAsFactors = FALSE)
      # Differences as a percentage of that compartment's peak in the run, so
      # that a near-zero value early in a wash-in does not manufacture a large
      # relative error out of nothing.
      peak <- stats::ave(abs(d$limit), d$Agent, FUN = max)
      d$engine_vs_limit_pct  <- 100 * (d$engine - d$limit)  / peak
      d$gasman_vs_limit_pct  <- 100 * (d$gasman - d$limit)  / peak
      d$engine_vs_gasman_pct <- 100 * (d$engine - d$gasman) / peak
      # Against Gas Man itself: how faithfully the R baseline restates it, and
      # how far the app engine is from the real program.
      d$baseline_vs_cpp_pct  <- 100 * (d$gasman - d$gasman_cpp) / peak
      d$engine_vs_cpp_pct    <- 100 * (d$engine - d$gasman_cpp) / peak
      d
    }))
  }))))

  if (!is.null(outdir))
  {
    utils::write.csv(format(long, digits = 10),
                     file.path(outdir, "gasman_engine_scenarios_results.csv"),
                     row.names = FALSE, quote = FALSE)
    utils::write.csv(do.call(rbind, lapply(circuits, function(circuit)
                       gasScenarioSettings(scenarios, circuit))),
                     file.path(outdir, "gasman_engine_scenarios_settings.csv"),
                     row.names = FALSE)
    # The scenario files in Gas Man's own CSV format, ready for gasman_run.
    scdir <- file.path(outdir, "scenarios_engine")
    dir.create(scdir, showWarnings = FALSE)
    for (circuit in circuits) for (s in scenarios)
      writeLines(gasScenarioGasManCsv(s, circuit),
                 file.path(scdir, sprintf("scenario_%d_%s.csv", s$id,
                                          sub("-", "", circuit))))
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
  cols <- c("engine_vs_limit_pct", "gasman_vs_limit_pct", "engine_vs_gasman_pct",
            "baseline_vs_cpp_pct", "engine_vs_cpp_pct")
  cols <- cols[vapply(cols, function(k) !all(is.na(long[[k]])), logical(1))]
  agg <- stats::aggregate(long[cols],
                          long[c("Circuit", "Scenario", "Agent", "Compartment")],
                          FUN = worst)
  agg <- agg[order(agg$Circuit, agg$Scenario, agg$Agent,
                   match(agg$Compartment, GAS_SCENARIO_COMPARTMENTS)), ]
  rownames(agg) <- NULL
  agg
}
