# The five scenarios that demonstrate the app's gas engine against GasMan.
#
# The scenarios, and the reasoning behind each, live in
# inst/validation/gasman_engine_scenarios.R; the full run (every compartment,
# a finer extrapolation, the three-hour case against the limit) is
# runGasEngineScenarios() there and takes a few minutes.  This file is the fast
# subset that guards the result:
#
#   * against GasMan as it runs, at its native 6-second tick, the engine is
#     within a few percent of each compartment's peak -- the gap being mostly
#     GasMan's own operator-splitting error, largest in the first minutes of a
#     change;
#   * against the limit GasMan's equations converge to as the tick shrinks, the
#     engine at the APP'S resolution agrees to about a thousandth of the peak,
#     in scenarios 1 and 5, which the test below checks.  Scenario 4's ideal
#     circuit does not converge to GasMan's: at fresh gas flow equal to the
#     ventilation it rebreathes to make up the uptake, which GasMan's ideal
#     circuit does not (inst/validation/VALIDATION.md; ChatGPT's round-2 gas
#     audit, A-06).
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

# How far the engine may sit from GasMan at GasMan's own 6-second tick, as a
# percentage of each compartment's peak.  The gap is GasMan's distance from
# the limit of its own equations, largest just after a setting changes.
GAS_SCENARIO_NATIVE_TOLERANCE <- 4


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
  # displaced 2% of the carrier, matching the GasMan run it is compared with.
  set <- env$gasScenarioSettings(sc)
  expect_equal(unique(set$Circuit), "Ideal")
  expect_equal(unique(env$gasScenarioSettings(sc, "semi-closed")$Circuit), "Semi-closed")
  expect_equal(set$DEL_percent[set$Scenario == 2 & set$Agent == "Nitrous Oxide"], 70)
  # Scenario 5 uses GasMan's allometric defaults at 100 kg.
  expect_equal(unique(set$VA_L_min[set$Scenario == 5]), 5.2268)
  expect_equal(unique(set$CO_L_min[set$Scenario == 5]), 6.5335)
})


# GasMan's own output (gasman_run, built from gasmanonline commit d3a2dd3, run
# on 2026-10-05), frozen in inst/validation/gasman_cpp_fixture.csv.  Until
# 2026-11-23 the tests held the engine to the R baseline only, so a number
# mistranscribed into both R restatements would have passed (ChatGPT's
# gas-engine audit of 2026-11-23, A-04).  The baseline restates GasMan to
# 0.062% of a compartment's peak (fat, GasMan's float32 accumulation; the
# rest under 0.004%).
GAS_SCENARIO_BASELINE_CPP_TOLERANCE <- 0.1

gasCppFixtureFile <- function() {
  f <- system.file("validation", "gasman_cpp_fixture.csv", package = "stanpumpR")
  if (!nzchar(f)) f <- "../../inst/validation/gasman_cpp_fixture.csv"
  if (!file.exists(f)) skip("GasMan C++ fixture not available")
  f
}

test_that("the engine tracks GasMan at its native tick in all five scenarios, in both circuits", {
  env <- gasScenarioEnv()
  fx <- env$gasCppFixture(gasCppFixtureFile())
  expect_equal(nrow(fx), 520)
  for (circuit in c("ideal", "semi-closed")) for (s in env$gasEngineScenarios()) {
    eng <- env$gasScenarioEngine(s, circuit = circuit)
    gm  <- env$gasScenarioBaseline(s, dt = 0.1, circuit = circuit)
    for (g in s$agents) for (cmp in env$GAS_SCENARIO_COMPARTMENTS) {
      e <- eng[eng$Agent == g, cmp]; m <- gm[gm$Agent == g, cmp]
      what <- paste(circuit, "scenario", s$id, g, cmp, "percent of peak")
      expect_lt(pctOfPeak(e, m, m), GAS_SCENARIO_NATIVE_TOLERANCE, label = what)
      # Against GasMan itself, at the same minutes
      cpp <- env$gasCppFixtureAt(fx, data.frame(
        Circuit = circuit, Scenario = s$id, Agent = g,
        Minute = eng$Time[eng$Agent == g], Compartment = cmp))
      expect_false(anyNA(cpp), label = paste(what, "has every fixture row"))
      expect_lt(pctOfPeak(m, cpp, cpp), GAS_SCENARIO_BASELINE_CPP_TOLERANCE,
                label = paste("baseline", what, "against GasMan C++"))
      expect_lt(pctOfPeak(e, cpp, cpp), GAS_SCENARIO_NATIVE_TOLERANCE,
                label = paste("engine", what, "against GasMan C++"))
    }
  }
})


test_that("the engine lands on the limit GasMan converges to", {
  env <- gasScenarioEnv()
  sc <- env$gasEngineScenarios()
  # The 70 kg anchor and the 100 kg case: between them the plain wash-in and
  # the weight scaling.  A coarser extrapolation than the full run, for speed.
  for (circuit in c("ideal", "semi-closed")) for (s in sc[c(1, 5)]) {
    eng <- env$gasScenarioEngine(s, circuit = circuit)
    lim <- env$gasScenarioLimit(s, refine = 4, circuit = circuit)
    for (cmp in env$GAS_SCENARIO_COMPARTMENTS)
      expect_lt(pctOfPeak(eng[[cmp]], lim[[cmp]], lim[[cmp]]), 0.1,
                label = paste(circuit, "scenario", s$id, cmp, "percent of peak"))
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
