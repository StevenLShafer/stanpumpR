# =============================================================================
# Gas Man validation grid -- driver script
# =============================================================================
#
# Run this after sourcing gasman_baseline_standalone.R.  It defines a five-case
# grid, runs our advance routine over it, and writes one Gas Man scenario file
# per case so the identical cases can be pushed through the Gas Man API.
#
#   1. Put both files in the same directory and open this one in RStudio.
#   2. Run it.  It writes scenarios/case_001.csv ... case_005.csv and holds our
#      results in `run$ours`.
#   3. Convert and run each scenario through Gas Man, saving its CSV export.
#      Either route works:
#
#        gasman_convert scenarios/case_001.csv --format json --output c1.json
#        # then GasManJsonToCsv(c1.json, ..., startSecond=0, endSecond=1800,
#        #                      everySeconds=1)
#
#      or drive the shared library from R directly using the helpers in the
#      repository's examples/r_example.R.
#
#   4. Compare:
#
#        theirs <- sprintf("gasman_out/case_%03d.csv", 1:5)
#        gasman_verify(run, theirs)
#
# gasman_verify() prints the worst disagreement per column as a percentage of
# that column's range, flags anything over the tolerance, and returns the full
# table for inspection.
#
# WHY THESE FIVE CASES
# --------------------
# They are chosen to separate the things that could differ, rather than to be
# realistic anaesthetics:
#
#   1. Plain sevoflurane, high flow.  The simplest possible case.  If this
#      disagrees, the disagreement is in the core stepping and nothing else.
#   2. The same case plus 70% nitrous oxide.  The ONLY difference from case 1
#      is the cross-gas uptake coupling, so case 2 minus case 1 isolates the
#      second gas effect.  This is the most informative single comparison.
#   3. Isoflurane at low flow.  A soluble agent where rebreathing dominates,
#      exercising the circuit equation rather than the tissue equations.
#   4. Desflurane at 0.5 L/min.  Near-closed circuit, the hardest case for the
#      circuit target, and the least soluble agent.
#   5. Low cardiac output with raised ventilation and nitrous oxide.  Pushes the
#      alveolar equation away from the defaults in both terms at once.
#
# START WITH CASE 1.  If it does not match, nothing later will, and the cause
# will be easier to see there than anywhere else.
#
# THINGS THAT ARE NOT YET PINNED DOWN, so do not read them as findings
# --------------------------------------------------------------------
#   * dt_ms is now 6000, Gas Man's breath period, matching their scenario
#     template.  An earlier version guessed 1000.  Note this is NOT a large
#     effect: measured, dt 1000 against 6000 differs by 6.4% at 1 min and under
#     0.2% by 20 min, and the curves converge as dt shrinks.  dt does not
#     multiply ventilation -- VA is separate, in L/min.
#   * ART.  We report ART = ALV.  Unverified.  If ART disagrees while ALV
#     agrees, that assumption is the reason and nothing is actually wrong.
#   * Weight is held at 70 in every case on purpose.  The API README says
#     weight scales compartment volumes; the source appears not to.  Settle the
#     stepping first, then make weight its own experiment.
# =============================================================================

if (!exists("gasman_simulate")) {
  GASMAN_QUIET <- TRUE      # suppress the baseline file's own demo output
  source("gasman_baseline_standalone.R")
}

DT_MS   <- 6000     # base tick, ms -- Gas Man's breath period (10 breaths/min)
MINUTES <- 30       # length of every case
OUTDIR  <- "scenarios"


gasman_validation_grid <- function() data.frame(
  case    = 1:5,
  label   = c("sevoflurane, high flow",
              "sevoflurane + 70% N2O (isolates the second gas effect)",
              "isoflurane, low flow (soluble agent, rebreathing dominates)",
              "desflurane, 0.5 L/min (near-closed circuit)",
              "sevoflurane + N2O, low cardiac output, raised ventilation"),

  agent1  = c("Sevoflurane", "Sevoflurane", "Isoflurane", "Desflurane", "Sevoflurane"),
  del1    = c(2.0,            2.0,           1.2,          6.0,          2.0),

  agent2  = c("",             "Nitrous Oxide", "",         "",           "Nitrous Oxide"),
  del2    = c(0,              70,             0,           0,            70),

  fgf     = c(8.0,            8.0,           2.0,          0.5,          2.0),
  va      = c(4,              4,             4,            4,            6),
  co      = c(5.0,            5.0,           5.0,          5.0,          2.5),

  weight  = 70,
  minutes = MINUTES,
  circuit = "semi-closed",
  dt_ms   = DT_MS,
  stringsAsFactors = FALSE
)


grid <- gasman_validation_grid()

cat("Gas Man validation grid\n")
cat("=======================\n\n")
print(grid[, c("case", "agent1", "del1", "agent2", "del2", "fgf", "va", "co")],
      row.names = FALSE)
cat("\n")

run <- gasman_grid(grid, outdir = OUTDIR, every_seconds = 1)

cat("Wrote", length(run$files), "scenario files to", normalizePath(OUTDIR), "\n\n")

# Our answers, for reference while running the Gas Man side.
cat("Our alveolar tensions (percent of 1 atm):\n\n")
for (r in seq_len(nrow(grid))) {
  o <- run$ours[run$ours$Case == r & run$ours$Agent == grid$agent1[r], ]
  at <- function(t) stats::approx(o$Time, o$ALV, t)$y
  cat(sprintf("  case %d  %-11s  1min %6.3f   5min %6.3f   15min %6.3f   30min %6.3f\n",
              r, grid$agent1[r], at(1), at(5), at(15), at(30)))
}

cat("\nCase 2 minus case 1 is the second gas effect on sevoflurane:\n")
c1 <- run$ours[run$ours$Case == 1 & run$ours$Agent == "Sevoflurane", ]
c2 <- run$ours[run$ours$Case == 2 & run$ours$Agent == "Sevoflurane", ]
for (t in c(1, 2, 5, 10, 20, 30)) {
  a <- stats::approx(c2$Time, c2$ALV, t)$y
  b <- stats::approx(c1$Time, c1$ALV, t)$y
  cat(sprintf("  %2d min   with N2O %6.3f   without %6.3f   ratio %5.3f\n",
              t, a, b, a / b))
}

# ---------------------------------------------------------------------------
# FIRST DIAGNOSTIC: Delivered has no model in it
# ---------------------------------------------------------------------------
# Gas Man accumulates fResults[DEL] += fDEL * fFGF * subdt / 100, and
# GetDelivered returns it unchanged.  So the Delivered column is a pure function
# of the dial, the fresh gas flow and elapsed time -- no compartments, no
# solubilities, no integration scheme, no weight.  It separates two very
# different problems:
#
#   Delivered AGREES, Uptake disagrees  -> the inputs are being read the same way
#                                          on both sides, and the disagreement is
#                                          in the model or its parameters.
#   Delivered DISAGREES                 -> FGF or the dial is not what we think
#                                          it is, and everything downstream
#                                          follows from that.  CHECK THIS FIRST.
cat("\nDelivered at 30 min -- model-free, should match Gas Man exactly:\n")
for (r in seq_len(nrow(grid))) {
  o <- run$ours[run$ours$Case == r & run$ours$Agent == grid$agent1[r], ]
  expected <- grid$del1[r] * grid$fgf[r] * MINUTES / 100
  cat(sprintf("  case %d  %-11s  ours %7.4f L   dial x FGF x t / 100 = %7.4f L\n",
              r, grid$agent1[r], o$Delivered[nrow(o)], expected))
}

cat("\nNext:\n")
cat("  theirs <- sprintf(\"gasman_out/case_%03d.csv\", 1:5)\n")
cat("  gasman_verify(run, theirs)\n")
