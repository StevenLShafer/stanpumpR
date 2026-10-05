# The five scenarios that demonstrate the app's gas engine against Gas Man.
#
# The scenarios, and the reasoning behind each, live in
# inst/validation/gasman_engine_scenarios.R; the full run (every compartment,
# a finer extrapolation, the three-hour case against the limit) is
# runGasEngineScenarios() there and takes a few minutes.  This file is the fast
# subset that guards the result:
#
#   * against Gas Man as it runs, at its native 6-second tick, the engine is
#     within a few percent of each compartment's peak -- the gap being Gas Man's
#     own operator-splitting error, largest in the first minutes of a change;
#   * against the limit Gas Man's equations converge to as the tick shrinks, the
#     engine at the APP'S resolution agrees to about a thousandth of the peak.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05; run on R 4.6.1.)

gasScenarioEnv <- function() {
  f <- system.file("validation", "gasman_engine_scenarios.R", package = "stanpumpR")
  if (!nzchar(f)) f <- "../../inst/validation/gasman_engine_scenarios.R"
  if (!file.exists(f)) skip("gas engine scenarios not available")
  # Parent is the package namespace so the scenario code can see the engine's
  # internals (gasSettingsAt, AIR_FRACTION_N2) as well as its exports.
  env <- new.env(parent = asNamespace("stanpumpR"))
  sys.source(f, envir = env)
  env
}

pctOfPeak <- function(a, b, ref) 100 * max(abs(a - b)) / max(abs(ref))


test_that("there are five scenarios, each well formed", {
  env <- gasScenarioEnv()
  sc <- env$gasEngineScenarios()
  expect_length(sc, 5)
  for (s in sc) {
    expect_true(all(c("Time", "Drug", "Dose") %in% names(s$DT)))
    expect_true(all(isGasDrug(s$DT$Drug)))
    # Ventilation is set, and positive, from time zero in every scenario.
    v <- s$DT[s$DT$Drug == "ventilation", ]
    expect_true(nrow(v) > 0 && min(v$Time) == 0 && all(v$Dose > 0))
    expect_true(all(s$times <= s$maximum))
  }
  # Scenario 2 delivers exactly 70% nitrous oxide once the vaporiser has
  # displaced 2% of the carrier, matching the Gas Man run it is compared with.
  set <- env$gasScenarioSettings(sc)
  expect_equal(set$DEL_percent[set$Scenario == 2 & set$Agent == "Nitrous Oxide"], 70)
  # Scenario 5 uses Gas Man's allometric defaults at 100 kg.
  expect_equal(unique(set$VA_L_min[set$Scenario == 5]), 5.2268)
  expect_equal(unique(set$CO_L_min[set$Scenario == 5]), 6.5335)
})


test_that("the engine tracks Gas Man at its native tick in all five scenarios", {
  env <- gasScenarioEnv()
  for (s in env$gasEngineScenarios()) {
    eng <- env$gasScenarioEngine(s)
    gm  <- env$gasScenarioBaseline(s, dt = 0.1)
    for (g in s$agents) for (cmp in env$GAS_SCENARIO_COMPARTMENTS) {
      e <- eng[eng$Agent == g, cmp]; m <- gm[gm$Agent == g, cmp]
      # Worst seen when written: 2.9% (nitrous oxide, alveolar, scenario 2).
      expect_lt(pctOfPeak(e, m, m), 4)
    }
  }
})


test_that("the engine lands on the limit Gas Man converges to", {
  env <- gasScenarioEnv()
  sc <- env$gasEngineScenarios()
  # The 70 kg anchor and the 100 kg case: between them the plain wash-in and
  # the weight scaling.  A coarser extrapolation than the full run, for speed.
  for (s in sc[c(1, 5)]) {
    eng <- env$gasScenarioEngine(s)
    lim <- env$gasScenarioLimit(s, refine = 4)
    for (cmp in env$GAS_SCENARIO_COMPARTMENTS)
      expect_lt(pctOfPeak(eng[[cmp]], lim[[cmp]], lim[[cmp]]), 0.1)
  }
})


test_that("emergence in the whole-anaesthetic scenario behaves", {
  env <- gasScenarioEnv()
  s <- env$gasEngineScenarios()[[3]]
  eng <- env$gasScenarioEngine(s)
  alv <- stats::setNames(eng$ALV, eng$Time)
  # Step up at 30 min raises it, step down at 60 lowers it.
  expect_gt(alv[["60"]], alv[["30"]])
  expect_lt(alv[["150"]], alv[["60"]])
  # Vaporiser off at 150 min: alveolar falls monotonically, fast at first.
  after <- alv[c("150", "151", "155", "160", "180")]
  expect_true(all(diff(after) < 0))
  expect_lt(alv[["155"]], 0.5 * alv[["150"]])
  # Fat is still holding agent when the alveoli have nearly emptied.
  expect_gt(eng$FAT[eng$Time == 180], 0)
  expect_true(all(eng[env$GAS_SCENARIO_COMPARTMENTS] >= 0))
})
