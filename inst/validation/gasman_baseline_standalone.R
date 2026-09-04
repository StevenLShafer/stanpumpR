# =============================================================================
# Gas Man baseline -- self-contained R implementation for independent testing
# =============================================================================
#
# Written for Rich Epstein, 2026-09-03, at the request of Steven L. Shafer.
# Drafted by Claude Code (Claude Opus 5).
#
# WHAT THIS IS
# ------------
# A restatement in R of Gas Man's own integration scheme, transcribed from
# GasDoc.cpp::Calc and GasDoc.cpp::CalcUptake in the Gas Man source released
# under GPL-3.0 at github.com/rasman/gasmanonline.  No C++ code is copied; the
# algorithm is re-expressed from the published source.
#
# The point is NOT to be a better simulator.  It is to be the SAME simulator,
# so that any disagreement with Gas Man is a real disagreement rather than a
# difference of method.  Where Gas Man does something questionable it is
# reproduced here and the oddity is noted, not corrected.
#
# HOW TO USE IT
# -------------
#   1. source("gasman_baseline_standalone.R")   in RStudio.  Base R only, no
#      packages required.
#   2. Run the same case through the Gas Man API, exporting its CSV.
#   3. Compare:
#
#        ours   <- gasman_simulate(
#                    agents = list(list(name = "Sevoflurane",   del = 2.0),
#                                  list(name = "Nitrous Oxide", del = 70)),
#                    fgf = 8, va = 4, co = 5, weight = 70, minutes = 30)
#
#        theirs <- read.csv("gasman_export.csv")
#        cmp    <- gasman_compare(ours, theirs)
#        print(cmp$summary)
#
# The interface deliberately matches Gas Man's own inputs -- a delivered tension
# per agent, with fresh gas flow, alveolar ventilation and cardiac output shared
# -- rather than the flowmeter-based dose table stanpumpR uses.  The flowmeter
# layer is stanpumpR's addition and would confound this comparison.
#
# WHAT TO LOOK AT FIRST
# ---------------------
#   * ALV early (0-5 min), where the integration scheme matters most.
#   * The Uptake and Delivered columns, which integrate the whole trajectory
#     and so accumulate any systematic error.
#   * VA.  Gas Man reports INSPIRED ventilation, the setting plus the volume
#     drawn in to replace uptake, and this code now does the same.  It is worth
#     comparing because it is the only column exposing the INSTANTANEOUS uptake
#     rate -- Uptake itself is cumulative, so a rate error there is smeared.
#   * A case with nitrous oxide AND a volatile, which exercises the uptake
#     ("second gas") coupling between gases.
#   * dt_ms.  Gas Man's m_fdt is m_cMSec_dx/60000.  The value is 6000 ms, which
#     is Gas Man's BREATH PERIOD -- its own comment says Calc is called
#     "2^nVerner times per breath to advance one full breath period", and 6000 ms
#     is 10 breaths per minute.  Their scenario template sets dt_ms,6000.  An
#     earlier version of this file defaulted to 1000, which was simply wrong.
#
#     Note dt does NOT multiply ventilation.  VA is a separate parameter in
#     L/min and enters as a flow; dt only sets how stale the targets are between
#     sub-steps.  Measured sensitivity for sevoflurane 2%, FGF 8, VA 4, CO 5:
#     dt 1000 against dt 6000 differs by 6.4% at 1 min, 1.3% at 5 min and under
#     0.2% by 20 min, and the curves converge as dt shrinks.  That is a bounded
#     splitting error, not a rate error, so dt alone cannot explain a large
#     disagreement in uptake.
#
# KNOWN GAPS, stated up front so they are not mistaken for findings
# ------------------------------------------------------------------
#   * ART.  Gas Man's export has an arterial column.  This code reports
#     ART = ALV, on the assumption that blood leaving the lung equilibrates
#     with alveolar gas.  That is an ASSUMPTION and has not been checked
#     against GetART().  If ART disagrees but ALV agrees, this is why.
#   * m_bVapEnb is CONFIRMED false by default (InitDocument sets
#     m_bVapEnb = false), so `vaporizer_effect = FALSE` here is right.
#   * Weight.  CORRECTED 2026-09-03.  An earlier version of this file said Gas
#     Man does not scale compartment volumes by weight.  It does, but not by
#     scaling m_fVolume -- which is why it was missed.  ComputeTerms divides the
#     ALVEOLAR and TISSUE rate constants by fWtFactor = weight/70, which is
#     equivalent to multiplying those effective volumes by it.  The CIRCUIT is
#     not scaled, correctly, since it is machine rather than patient.  Uptake is
#     separately multiplied by fWtFactor, consistently.
#
#     Note the DEFAULT VA and CO scale allometrically instead, by
#     (weight/70)^0.75, but only as defaults -- an explicit scenario overrides
#     them, so that does not enter a comparison that sets va and co.
# =============================================================================


# -----------------------------------------------------------------------------
# Agent parameters, read from [<Agent>] sections of gasman.ini
# -----------------------------------------------------------------------------
# Lambda            blood:gas partition coefficient
# VRG / MUS / FAT   tissue:GAS partition coefficients (NOT tissue:blood)
# MAC               minimum alveolar concentration, % of 1 atm
# Ambient           starting tension, % of 1 atm
#
# NOTE ON NITROGEN MAC: gasman.ini gives 200, i.e. 2 atmospheres.  Eger put the
# MAC of nitrogen at 110 ATMOSPHERES.  Gas Man's figure is low by about 55-fold.
# It is carried here unchanged so that this code matches Gas Man; it is not used
# in any calculation below.
# -----------------------------------------------------------------------------

GASMAN_AGENTS <- data.frame(
  name    = c("Desflurane", "Enflurane", "Halothane", "Isoflurane",
              "Nitrogen", "Nitrous Oxide", "Sevoflurane"),
  lambda  = c(0.42,  1.9,   2.4,   1.3,   0.014, 0.47,  0.65),
  vrg     = c(0.54,  2.6,   4.8,   2.1,   0.010, 0.42,  1.1),
  mus     = c(0.97,  3.4,   8.4,   4.5,   0.014, 0.54,  2.4),
  fat     = c(13,    69,    130,   70,    0.070, 1.08,  34),
  mac     = c(6.0,   1.7,   0.75,  1.1,   200,   110,   2.1),
  ambient = c(0,     0,     0,     0,     80,    0,     0),
  stringsAsFactors = FALSE
)

# [Volumes] in gasman.ini, litres
GASMAN_VOLUME <- c(CKT = 8, ALV = 2.5, VRG = 6, MUS = 33, FAT = 14.5, VEN = 1)

# [Ratio] in gasman.ini: fraction of cardiac output to each tissue group
GASMAN_RATIO <- c(VRG = 0.76, MUS = 0.18, FAT = 0.06)

GASMAN_MAX_VERNIER   <- 5     # MAX_VERNIER
GASMAN_VERNIER_TICKS <- 3     # VERNIER_TICKS
GASMAN_STD_WEIGHT    <- 70    # STD_WEIGHT

GASMAN_COMPARTMENTS <- c("CKT", "ALV", "VRG", "MUS", "FAT", "VEN")


# -----------------------------------------------------------------------------
# Per-compartment solubilities, as GasAnes.cpp sets m_fSolubility
# -----------------------------------------------------------------------------
# Circuit and alveolus are gas phase, so 1.  Tissues carry tissue:gas.  The
# venous entry carries blood:gas.
gasman_solubility <- function(agent_row) {
  c(CKT = 1, ALV = 1,
    VRG = agent_row$vrg, MUS = agent_row$mus, FAT = agent_row$fat,
    VEN = agent_row$lambda)
}


# -----------------------------------------------------------------------------
# Uptake rate for one agent -- after GasDoc.cpp::CalcUptake
# -----------------------------------------------------------------------------
# Returns litres of agent per minute.
gasman_uptake <- function(state, sol, lambda, exp_tissue, subdt,
                          weight, co, recirculation) {
  tis <- c("VRG", "MUS", "FAT")

  # Volume of agent moved into each tissue during this sub-step, divided by the
  # sub-step length to give a rate.  The /100 is because tensions are percent.
  moved <- sum(GASMAN_VOLUME[tis] * sol[tis] *
                 (state[["ALV"]] - state[tis]) * (1 - exp_tissue[tis]))
  uptake <- moved * (weight / GASMAN_STD_WEIGHT) / subdt / 100

  if (!recirculation) {
    # With no venous return, agent carried off in blood never comes back and so
    # counts as uptake.
    mixed_venous <- sum(state[tis] * GASMAN_RATIO[tis])
    uptake <- uptake + co * lambda * mixed_venous / 100
  }
  unname(uptake)
}


# -----------------------------------------------------------------------------
# Advance one agent by one sub-step -- after GasDoc.cpp::Calc
# -----------------------------------------------------------------------------
# Every target is computed from the state at the START of the sub-step, so the
# update is simultaneous rather than sequential.
#
# Returns the new state and a flag; FALSE means the sub-step was numerically
# dubious and the whole tick should be retried at twice the resolution.
gasman_calc <- function(state, sol, lambda, del, fgf, va, co,
                        exp_tissue, subdt, tot_uptake,
                        circuit, uptake_effect, recirculation,
                        wt_factor = 1, check_fast_decay = TRUE) {

  eff_ckt    <- fgf * sol[["CKT"]]
  eff_alv    <- va  * sol[["ALV"]]
  blood_flow <- lambda * co

  target    <- state
  fixed_ckt <- FALSE

  if (circuit == "open") {
    # Delivered gas determines the circuit outright.
    state[["CKT"]] <- del
    fixed_ckt <- TRUE

  } else if (circuit == "ideal") {
    # New mixture displaces exhaled gas without mixing.  Note the explicit
    # threshold: at or above FGF = VA the circuit simply is the delivered gas.
    if (eff_ckt < eff_alv) {
      f <- eff_ckt / eff_alv
      state[["CKT"]] <- f * del + (1 - f) * state[["ALV"]]
    } else {
      state[["CKT"]] <- del
    }
    fixed_ckt <- TRUE

  } else {
    # Semi-closed: the circuit gets a target and relaxes toward it.
    g <- eff_ckt + eff_alv
    if (g != 0) {
      f <- eff_ckt * del + eff_alv * state[["ALV"]]
      if (uptake_effect && tot_uptake < 0)
        f <- f - tot_uptake * state[["ALV"]]
      target[["CKT"]] <- f / g
    } else {
      target[["CKT"]] <- state[["CKT"]]
    }
  }

  # Alveolar target.  The uptake term here is Gas Man's "Correct for constant
  # lung capacity": gas taken up by blood leaves a volume deficit that draws
  # replacement in from the circuit on induction, and pushes alveolar gas out on
  # emergence.  tot_uptake is summed over ALL agents, so nitrous oxide's uptake
  # augments a volatile's alveolar tension.  That is the second gas effect.
  g <- eff_alv + blood_flow
  if (g != 0) {
    f <- eff_alv * state[["CKT"]] + blood_flow * state[["VEN"]]
    if (uptake_effect) {
      if (tot_uptake > 0) f <- f + state[["CKT"]] * tot_uptake
      else                f <- f + state[["ALV"]] * tot_uptake
    }
    target[["ALV"]] <- f / g
  } else {
    target[["ALV"]] <- state[["ALV"]]
  }

  # Tissues chase the current alveolar tension.
  target[["VRG"]] <- target[["MUS"]] <- target[["FAT"]] <- state[["ALV"]]

  # P(t+dt) = target*(1 - exp(-k dt)) + P(t)*exp(-k dt),
  #   k = effective_flow / (volume * solubility)
  f_ckt <- exp(-subdt / (GASMAN_VOLUME[["CKT"]] * sol[["CKT"]]) * (eff_ckt + eff_alv))
  # ALV and the tissues carry the weight factor; the circuit does not, because
  # the circuit is machine rather than patient.  Gas Man does this by dividing
  # the rate constant by fWtFactor (ComputeTerms), which is the same thing as
  # multiplying the effective volume by it.
  f_alv <- exp(-subdt / (GASMAN_VOLUME[["ALV"]] * sol[["ALV"]] * wt_factor) *
                 (eff_alv + blood_flow))
  decay <- c(CKT = f_ckt, ALV = f_alv,
             VRG = exp_tissue[["VRG"]], MUS = exp_tissue[["MUS"]],
             FAT = exp_tissue[["FAT"]])

  ok <- TRUE
  for (cmp in c("CKT", "ALV", "VRG", "MUS", "FAT")) {
    if (cmp == "CKT" && fixed_ckt) next
    state[[cmp]] <- target[[cmp]] * (1 - decay[[cmp]]) + state[[cmp]] * decay[[cmp]]
    if (state[[cmp]] < 0) ok <- FALSE          # no negative tensions
  }

  # More than 90% of the circuit or alveolar value gone in one sub-step means
  # the step is too coarse to trust.  Gas Man applies this test ONLY within
  # VERNIER_TICKS ticks of a settings change -- see the guard in Calc:
  #   int i = VERNIER_TICKS - int((dwTime - samp.m_dwBaseTime) / m_cMSec_dx);
  #   if (i > 0) { if (fExpCKT < 0.1F || fExpALV < 0.1F) bMyRet = false; }
  # so once a setting has been in force a while it stops sub-stepping on this
  # criterion even if the step is coarse.  Applying it unconditionally would
  # make this code MORE accurate than Gas Man, which is the wrong direction for
  # a baseline.
  if (check_fast_decay && (f_ckt < 0.1 || f_alv < 0.1)) ok <- FALSE

  # Venous is algebraic, not differential: the blood-flow-weighted mean of the
  # NEW tissue tensions.  The alveolar target above therefore used the PREVIOUS
  # sub-step's value, a one-sub-step lag that is Gas Man's, not an oversight.
  state[["VEN"]] <- if (recirculation)
    sum(state[c("VRG", "MUS", "FAT")] * GASMAN_RATIO) else 0

  list(state = state, ok = ok)
}


# -----------------------------------------------------------------------------
# Driver
# -----------------------------------------------------------------------------
#' @param agents list of list(name = <as in GASMAN_AGENTS$name>, del = <percent>)
#' @param fgf    fresh gas flow, L/min
#' @param va     alveolar ventilation, L/min
#' @param co     cardiac output, L/min
#' @param weight kg
#' @param minutes length of simulation
#' @param circuit "semi-closed", "open" or "ideal"
#' @param uptake_effect   Gas Man's m_bUptEnb; default TRUE as Gas Man does
#' @param recirculation   Gas Man's m_bRtnEnb; default TRUE as Gas Man does
#' @param vaporizer_effect Gas Man's m_bVapEnb; default FALSE, see KNOWN GAPS
#' @param dt_ms  base tick in milliseconds; Gas Man's m_cMSec_dx
#' @param every_seconds output row spacing, to match the Gas Man export
#'
#' @return data frame with one row per agent per output time, columns matching
#'   Gas Man's CSV export
gasman_simulate <- function(agents,
                            fgf = 8, va = 4, co = 5, weight = 70,
                            minutes = 30,
                            circuit = "semi-closed",
                            uptake_effect = TRUE,
                            recirculation = TRUE,
                            vaporizer_effect = FALSE,
                            dt_ms = 6000,
                            every_seconds = 1) {

  stopifnot(is.list(agents), length(agents) > 0)
  dt <- dt_ms / 60000                       # minutes, as Gas Man's m_fdt

  names_in <- vapply(agents, function(a) a$name, character(1))
  unknown  <- setdiff(names_in, GASMAN_AGENTS$name)
  if (length(unknown))
    stop("Unknown agent(s): ", paste(unknown, collapse = ", "),
         ". Known: ", paste(GASMAN_AGENTS$name, collapse = ", "))

  rows <- lapply(names_in, function(n) GASMAN_AGENTS[GASMAN_AGENTS$name == n, ])
  sol  <- lapply(rows, gasman_solubility)
  lam  <- vapply(rows, function(r) r$lambda, numeric(1))
  del  <- vapply(agents, function(a) a$del, numeric(1))

  # Starting tensions: each agent's ambient (80 for nitrogen, 0 otherwise).
  state <- lapply(rows, function(r)
    stats::setNames(rep(r$ambient, length(GASMAN_COMPARTMENTS)), GASMAN_COMPARTMENTS))

  cum_uptake    <- rep(0, length(agents))    # litres, Gas Man's fResults[UPT]
  cum_delivered <- rep(0, length(agents))    # litres, Gas Man's fResults[DEL]

  out_times <- seq(0, minutes, by = every_seconds / 60)
  n_out     <- length(out_times)
  rec <- lapply(seq_along(agents), function(i)
    matrix(NA_real_, n_out, length(GASMAN_COMPARTMENTS) + 2,
           dimnames = list(NULL, c(GASMAN_COMPARTMENTS, "Uptake", "Delivered"))))
  for (i in seq_along(agents)) rec[[i]][1, ] <- c(state[[i]], 0, 0)

  # VA is RECORDED as the run proceeds, not reconstructed afterwards.  Gas Man's
  # GetVA is (sum over gases of UPT(t) - UPT(t - one tick)) / dt + m_fVA, i.e.
  # the uptake rate over the LAST TICK.  An earlier version recovered that by
  # interpolating cumulative uptake on the OUTPUT grid, which made the reported
  # number depend on how often output was written -- 4.0110 at one-second output
  # against 4.0022 at five-second output for the same run.  The tick is the only
  # correct window, so record it here.
  va_rec    <- rep(va, n_out)     # Gas Man returns the setting on tick 1

  n_ticks   <- max(1, ceiling(minutes / dt))
  next_out  <- 2
  max_vern  <- 0

  for (tick in seq_len(n_ticks)) {
    cum_before <- sum(cum_uptake)      # for this tick's VA
    # Settings are constant here, so the only change is at time zero.
    check_fast_decay <- (tick - 1) < GASMAN_VERNIER_TICKS
    nv_level <- 0
    repeat {
      saved_state <- state
      saved_upt   <- cum_uptake
      saved_del   <- cum_delivered

      n_sub <- 2^nv_level
      subdt <- dt / n_sub

      wt_factor <- weight / GASMAN_STD_WEIGHT
      exp_tissue <- lapply(seq_along(agents), function(i) {
        eff <- lam[i] * co * GASMAN_RATIO
        exp(-eff * subdt / (GASMAN_VOLUME[c("VRG", "MUS", "FAT")] *
                              sol[[i]][c("VRG", "MUS", "FAT")] * wt_factor))
      })

      ok <- TRUE
      for (sub in seq_len(n_sub)) {

        # Uptake for every agent from the CURRENT state, then summed, exactly as
        # Gas Man's caller does before advancing any of them.
        upt <- vapply(seq_along(agents), function(i)
          gasman_uptake(state[[i]], sol[[i]], lam[i], exp_tissue[[i]], subdt,
                        weight, co, recirculation), numeric(1))
        tot_uptake <- sum(upt)

        for (i in seq_along(agents)) {
          eff_fgf <- fgf
          if (vaporizer_effect && del[i] < 99)
            eff_fgf <- fgf / ((100 - del[i]) / 100)

          r <- gasman_calc(state[[i]], sol[[i]], lam[i], del[i],
                           eff_fgf, va, co, exp_tissue[[i]], subdt,
                           tot_uptake, circuit, uptake_effect, recirculation,
                           wt_factor, check_fast_decay)
          state[[i]] <- r$state
          if (!r$ok) ok <- FALSE

          cum_uptake[i]    <- cum_uptake[i] + upt[i] * subdt
          cum_delivered[i] <- cum_delivered[i] + del[i] * eff_fgf * subdt / 100
        }
        if (!ok) break
      }

      if (ok || nv_level + 1 >= GASMAN_MAX_VERNIER) break
      state         <- saved_state       # reject the tick, retry finer
      cum_uptake    <- saved_upt
      cum_delivered <- saved_del
      nv_level      <- nv_level + 1
    }
    max_vern <- max(max_vern, nv_level)

    # Averaged over the tick, exactly the window GetVA uses.
    va_now <- if (uptake_effect) va + (sum(cum_uptake) - cum_before) / dt else va

    t_end <- tick * dt
    while (next_out <= n_out && out_times[next_out] <= t_end + 1e-12) {
      for (i in seq_along(agents))
        rec[[i]][next_out, ] <- c(state[[i]], cum_uptake[i], cum_delivered[i])
      va_rec[next_out] <- va_now
      next_out <- next_out + 1
    }
  }
  for (i in seq_along(agents)) {
    miss <- is.na(rec[[i]][, 1])
    if (any(miss))
      rec[[i]][miss, ] <- matrix(c(state[[i]], cum_uptake[i], cum_delivered[i]),
                                 sum(miss), ncol(rec[[i]]), byrow = TRUE)
  }

  # Gas Man's VA column is INSPIRED ventilation, not the setting.  GetVA returns
  #     (sum over gases of UPT(t) - UPT(t - one tick)) / dt  +  m_fVA
  # when uptake is enabled: the expired setting plus the extra volume drawn in to
  # replace what blood took up.  Reporting the setting instead would make the
  # column look like a disagreement when it is only a difference of definition.
  # Note this is REPORTING only -- the model is untouched either way.
  va_rep <- va_rec        # recorded per tick in the loop above, see the note there

  res <- do.call(rbind, lapply(seq_along(agents), function(i) data.frame(
    Time = out_times, Agent = names_in[i],
    FGF = fgf, VA = va_rep, CO = co,
    CKT = rec[[i]][, "CKT"], ALV = rec[[i]][, "ALV"],
    ART = rec[[i]][, "ALV"],          # ASSUMPTION -- see KNOWN GAPS
    VRG = rec[[i]][, "VRG"], MUS = rec[[i]][, "MUS"], FAT = rec[[i]][, "FAT"],
    VEN = rec[[i]][, "VEN"],
    Uptake = rec[[i]][, "Uptake"], Delivered = rec[[i]][, "Delivered"],
    stringsAsFactors = FALSE)))

  attr(res, "max_vernier") <- max_vern
  res
}


# -----------------------------------------------------------------------------
# Comparison against a Gas Man CSV export
# -----------------------------------------------------------------------------
# Gas Man writes Time as "H:M:S".  Pass `time_parser` if the export differs.
gasman_compare <- function(ours, theirs,
                           columns = c("CKT", "ALV", "VRG", "MUS", "FAT", "VEN",
                                       "Uptake", "Delivered", "VA"),
                           time_parser = NULL) {

  if (is.null(time_parser)) {
    time_parser <- function(x) {
      if (is.numeric(x)) return(x)
      p <- do.call(rbind, lapply(strsplit(as.character(x), ":"), as.numeric))
      p[, 1] * 60 + p[, 2] + p[, 3] / 60          # H:M:S -> minutes
    }
  }
  theirs$TimeMin <- time_parser(theirs$Time)
  ours$TimeMin   <- ours$Time

  columns <- intersect(columns, intersect(names(ours), names(theirs)))
  agents  <- intersect(unique(ours$Agent), unique(theirs$Agent))
  if (!length(agents))
    stop("No agent names in common. Ours: ", paste(unique(ours$Agent), collapse = ", "),
         " / theirs: ", paste(unique(theirs$Agent), collapse = ", "))

  detail <- do.call(rbind, lapply(agents, function(a) {
    o <- ours[ours$Agent == a, ]
    t <- theirs[theirs$Agent == a, ]
    do.call(rbind, lapply(columns, function(cl) {
      # Interpolate ours onto their times, so grids need not match.
      mine <- stats::approx(o$TimeMin, o[[cl]], t$TimeMin, rule = 2)$y
      data.frame(Agent = a, Column = cl, Time = t$TimeMin,
                 Ours = mine, Theirs = t[[cl]],
                 Diff = mine - t[[cl]], stringsAsFactors = FALSE)
    }))
  }))

  summary <- do.call(rbind, lapply(split(detail, list(detail$Agent, detail$Column),
                                         drop = TRUE), function(d) {
    scale <- max(abs(d$Theirs), na.rm = TRUE)
    data.frame(Agent = d$Agent[1], Column = d$Column[1],
               n = nrow(d),
               MaxAbsDiff = max(abs(d$Diff), na.rm = TRUE),
               RMSE = sqrt(mean(d$Diff^2, na.rm = TRUE)),
               MaxPctOfScale = if (scale > 0)
                 100 * max(abs(d$Diff), na.rm = TRUE) / scale else NA_real_,
               stringsAsFactors = FALSE)
  }))
  rownames(summary) <- NULL
  summary <- summary[order(-summary$MaxPctOfScale), ]

  list(summary = summary, detail = detail)
}


# -----------------------------------------------------------------------------
# Example -- delete or edit freely
# -----------------------------------------------------------------------------
if (identical(environment(), globalenv()) && !exists("GASMAN_QUIET")) {
  cat("Gas Man baseline loaded.\n\n")
  demo <- gasman_simulate(
    agents  = list(list(name = "Sevoflurane",   del = 2.0),
                   list(name = "Nitrous Oxide", del = 70)),
    fgf = 8, va = 4, co = 5, weight = 70, minutes = 30)
  show <- demo[demo$Agent == "Sevoflurane" &
                 demo$Time %in% c(1, 2, 5, 10, 20, 30),
               c("Time", "CKT", "ALV", "VRG", "VEN", "Uptake", "Delivered")]
  cat("Sevoflurane 2% with 70% nitrous oxide, FGF 8, VA 4, CO 5, 70 kg:\n")
  print(round(show, 4), row.names = FALSE)
  cat("\nmax vernier level used:", attr(demo, "max_vernier"),
      " (0 means the base tick was never rejected)\n")
  cat("\nNext: run the same case through the Gas Man API, then\n")
  cat("  cmp <- gasman_compare(demo, read.csv(\"gasman_export.csv\"))\n")
  cat("  print(cmp$summary)\n")
}


# =============================================================================
# Scenario generation and grid verification
# =============================================================================
#
# The point of this section is that ONE scenario definition drives BOTH engines.
# gasman_scenario_csv() emits the Gas Man scenario CSV, and gasman_simulate()
# takes the same arguments, so the two cannot drift apart through a
# transcription slip.
#
# TWO THINGS TO KNOW BEFORE RUNNING A GRID
# ----------------------------------------
# 1. PER-AGENT CONSTANTS ARE DELIBERATELY OMITTED from the generated scenario,
#    so that Gas Man falls back to gasman.ini.  Their own template documents
#    lambdaVrg / lambdaMus / lambdaFat as "VRG/blood", "muscle/blood" and
#    "fat/blood" partition coefficients, but the values it supplies for
#    sevoflurane (1.1, 2.4, 34) are the ini's tissue:GAS numbers, not
#    tissue:blood.  Passing them would make it ambiguous which convention the
#    engine applies, and a factor of lambdaBlood is exactly the size of error
#    that would produce.  Letting the ini supply them removes the question.
#
# 2. HOLD WEIGHT AT 70 to begin with.  Reading GasDoc.cpp suggests compartment
#    volumes are NOT weight-scaled, weight entering only as a multiplier on
#    uptake; the API README says weight "scales its default compartment sizes".
#    Those cannot both be right.  At 70 kg the question does not arise, so
#    settle the integration scheme first and come back to weight as its own
#    experiment.
# =============================================================================


#' Build a Gas Man scenario CSV from the same arguments gasman_simulate() takes
#'
#' @param schedule optional data frame of setting changes with columns
#'   time_min, and any of va, fgf, co, and del1..delN.  The first row is the
#'   state at time zero; without a schedule the settings are constant.
#' @return character vector of CSV lines
gasman_scenario_csv <- function(agents, fgf = 8, va = 4, co = 5, weight = 70,
                                circuit = "semi-closed", dt_ms = 6000,
                                schedule = NULL) {

  ckt <- switch(circuit,
                "semi-closed" = "Semi",
                "ideal"       = "Ideal",
                "closed"      = "Closed",
                stop("Gas Man's scenario format accepts Semi, Closed or Ideal. ",
                     "Circuit '", circuit, "' has no documented mapping."))

  hhmmss <- function(minutes) {
    s  <- round(minutes * 60)
    sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
  }

  del <- vapply(agents, function(a) a$del, numeric(1))
  n   <- length(agents)

  if (is.null(schedule))
    schedule <- data.frame(time_min = 0)
  if (!"time_min" %in% names(schedule))
    stop("schedule needs a time_min column")

  colFor <- function(row, nm, fallback) {
    if (nm %in% names(schedule) && !is.na(schedule[[nm]][row])) schedule[[nm]][row] else fallback
  }

  delNames <- if (n == 1) "del" else paste0("del", seq_len(n))
  injNames <- if (n == 1) "inject" else paste0("inject", seq_len(n))
  header   <- paste(c("va", "fgf", "circuit", "time", "co",
                      as.vector(rbind(delNames, injNames))), collapse = ",")

  rows <- vapply(seq_len(nrow(schedule)), function(r) {
    dels <- vapply(seq_len(n), function(i)
      colFor(r, delNames[i], del[i]), numeric(1))
    paste(c(colFor(r, "va", va), colFor(r, "fgf", fgf), ckt,
            hhmmss(schedule$time_min[r]), colFor(r, "co", co),
            as.vector(rbind(dels, rep(0, n)))), collapse = ",")
  }, character(1))

  c("# Generated by gasman_baseline_standalone.R -- do not hand-edit.",
    "# Per-agent constants are omitted on purpose so gasman.ini supplies them.",
    "",
    "[patient]",
    paste0("weight_kg,", weight),
    paste0("dt_ms,", dt_ms),
    "",
    unlist(lapply(agents, function(a) c("[agent]", paste0("name,", a$name), ""))),
    "[settings]",
    header,
    rows)
}


#' Write scenario files for a grid, and run our baseline over the same grid
#'
#' @param grid data frame with one row per case.  Recognised columns: agent1,
#'   del1, agent2, del2, fgf, va, co, weight, minutes, circuit, dt_ms.  Missing
#'   columns take the defaults.
#' @param outdir directory to write scenario CSVs into
#' @return list of `ours` (our results for every case) and `files`
gasman_grid <- function(grid, outdir = "gasman_scenarios",
                        every_seconds = 1) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  g <- function(row, nm, fallback)
    if (nm %in% names(grid) && !is.na(grid[[nm]][row])) grid[[nm]][row] else fallback

  files <- character(nrow(grid))
  ours  <- list()

  for (r in seq_len(nrow(grid))) {
    agents <- list(list(name = g(r, "agent1", "Sevoflurane"), del = g(r, "del1", 2)))
    if ("agent2" %in% names(grid) && !is.na(grid$agent2[r]) && nzchar(grid$agent2[r]))
      agents[[2]] <- list(name = grid$agent2[r], del = g(r, "del2", 0))

    args <- list(agents = agents,
                 fgf = g(r, "fgf", 8), va = g(r, "va", 4), co = g(r, "co", 5),
                 weight = g(r, "weight", 70), circuit = g(r, "circuit", "semi-closed"),
                 dt_ms = g(r, "dt_ms", 6000))

    files[r] <- file.path(outdir, sprintf("case_%03d.csv", r))
    writeLines(do.call(gasman_scenario_csv, args), files[r])

    mine <- do.call(gasman_simulate,
                    c(args, list(minutes = g(r, "minutes", 30),
                                 every_seconds = every_seconds)))
    mine$Case <- r
    ours[[r]] <- mine
  }

  list(ours = do.call(rbind, ours), files = files, grid = grid)
}


#' Compare a grid run against Gas Man CSV outputs
#'
#' @param gridrun the value returned by gasman_grid()
#' @param gasman_files character vector of Gas Man CSV outputs, in the same
#'   order as gridrun$files
#' @param tolerance_pct flag any column whose worst disagreement exceeds this
#'   percentage of that column's range
gasman_verify <- function(gridrun, gasman_files, tolerance_pct = 1) {
  stopifnot(length(gasman_files) == length(gridrun$files))

  per_case <- lapply(seq_along(gasman_files), function(r) {
    theirs <- utils::read.csv(gasman_files[r], stringsAsFactors = FALSE)
    mine   <- gridrun$ours[gridrun$ours$Case == r, ]
    s <- gasman_compare(mine, theirs)$summary
    s$Case <- r
    s
  })
  summary <- do.call(rbind, per_case)
  summary$Flag <- ifelse(summary$MaxPctOfScale > tolerance_pct, "***", "")
  summary <- summary[order(-summary$MaxPctOfScale), ]
  rownames(summary) <- NULL

  cat("Cases:", length(gasman_files),
      "  columns compared:", length(unique(summary$Column)), "\n")
  cat("Worst disagreement:", round(max(summary$MaxPctOfScale, na.rm = TRUE), 3),
      "% of column range\n")
  cat("Columns over", tolerance_pct, "%:",
      sum(summary$MaxPctOfScale > tolerance_pct, na.rm = TRUE), "\n\n")
  print(utils::head(summary, 20), row.names = FALSE)
  invisible(summary)
}
