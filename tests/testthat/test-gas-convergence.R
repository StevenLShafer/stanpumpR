# Cross-validation of the two inhaled-gas engines.
#
# There are two implementations of the same physiology in this package:
#
#   advanceGasManBaseline()  Gas Man's own stepping scheme restated in R.  Its
#                            purpose is to BE Gas Man, splitting error and all.
#                            Validated against Gas Man itself to 6e-04, limited
#                            by Gas Man's float32 -- see inst/validation.
#
#   advanceClosedFormGas()   the engine the app actually runs.  Solves the same
#                            equations with a matrix exponential that is exact
#                            within each sub-step.
#
# They must NOT agree digit-for-digit at any fixed step size.  The baseline
# carries Gas Man's operator-splitting error on purpose, and the closed form
# does not, so at dt = 6000 ms they differ by around 1%.  What must be true is
# that they converge to the SAME limit as dt shrinks, because that is the
# statement that they integrate the same equations.  Convergence to a common
# limit is what closes the chain to Gas Man transitively.
#
# This test is why the blood-flow fractions in getGasBody() are Gas Man's
# 0.76/0.18/0.06 rather than the undocumented 0.75/0.20/0.05 they used to be:
# that difference showed up here as a residual that did not shrink with dt,
# which is the signature of a structural difference rather than a numerical one.

skip_if_no_baseline <- function() {
  f <- system.file("validation", "gasman_baseline_standalone.R",
                   package = "stanpumpR")
  if (!nzchar(f)) f <- "../../inst/validation/gasman_baseline_standalone.R"
  if (!file.exists(f)) skip("standalone baseline not available")
  f
}

# The two engines take different inputs by design: the baseline takes delivered
# tensions, as Gas Man does, while the engine takes flowmeter settings and
# derives the tensions.  Matching them requires care, and getting it wrong looks
# exactly like a modelling disagreement -- so the mapping is spelled out here.
#
#   FGF 8 L/min as 2.4 O2 + 5.6 N2O, with sevoflurane at 2%.
#   The vaporiser displaces 2% of the carrier, so carrier = 0.98 and
#   F_fgf,N2O = 100 * 0.98 * 5.6/8 = 68.6%, NOT 70%.
#   Nitrogen must be present on both sides, starting at the same ambient.
#   Cardiac output must be forced equal: getGasBody() uses 75 mL/kg = 5.25 L/min
#   at 70 kg, while the Gas Man scenarios use 5.0.
matchedDoseTable <- function() data.frame(
  Time = c(0, 0, 0, 0),
  Drug = c("oxygen", "nitrousOxide", "ventilation", "sevoflurane"),
  Dose = c(2.4, 5.6, 4, 2),
  stringsAsFactors = FALSE
)

alvAt <- function(res, drug, t) {
  d <- res$results[res$results$Drug == drug & res$results$Site == "Alveolar", ]
  stats::approx(d$Time, d$Y, t)$y
}


test_that("the closed-form engine converges as the step shrinks", {
  DT <- matchedDoseTable()
  v <- vapply(c(301, 601, 1201, 2401), function(r)
    alvAt(advanceClosedFormGas(DT, weight = 70, maximum = 30, resolution = r,
                               cardiacOutput = 5), "sevoflurane", 30),
    numeric(1))

  # Successive differences must shrink monotonically toward zero.
  d <- abs(diff(v))
  expect_true(all(diff(d) < 0))
  expect_lt(d[length(d)], 1e-4)
})


test_that("the two engines converge to the same limit", {
  f <- skip_if_no_baseline()
  env <- new.env()
  assign("GASMAN_QUIET", TRUE, envir = env)
  sys.source(f, envir = env)

  # Match nitrogen's starting tension to the engine's air composition.
  env$GASMAN_AGENTS$ambient[env$GASMAN_AGENTS$name == "Nitrogen"] <-
    AIR_FRACTION_N2 * 100

  agents <- list(list(name = "Sevoflurane",   del = 2.0),
                 list(name = "Nitrous Oxide", del = 68.6),
                 list(name = "Nitrogen",      del = 0))

  baselineAt <- function(dtms, t) {
    b <- env$gasman_simulate(agents, fgf = 8, va = 4, co = 5, weight = 70,
                             minutes = 30, dt_ms = dtms, every_seconds = 1)
    b <- b[b$Agent == "Sevoflurane", ]
    stats::approx(b$Time, b$ALV, t)$y
  }

  # The baseline converges FIRST ORDER in dt, so extrapolate: limit ~ 2f(h/2)-f(h).
  lim <- 2 * baselineAt(188, 30) - baselineAt(375, 30)

  eng <- alvAt(advanceClosedFormGas(matchedDoseTable(), weight = 70,
                                    maximum = 30, resolution = 4801,
                                    cardiacOutput = 5), "sevoflurane", 30)

  # Same limit to well under a tenth of a percent.  If this fails, something
  # STRUCTURAL differs between the engines -- a coefficient, a flow fraction, a
  # missing term -- not a step-size artefact.  Check getGasBody() against
  # gasman.ini first; that is what it was last time.
  expect_lt(abs(eng - lim) / lim, 5e-4)
})


test_that("the second gas effect is present, and vanishes when uncoupled", {
  DT <- matchedDoseTable()
  on  <- advanceClosedFormGas(DT, weight = 70, maximum = 30, uptakeEffect = TRUE)
  off <- advanceClosedFormGas(DT, weight = 70, maximum = 30, uptakeEffect = FALSE)

  # Nitrous oxide's uptake augments sevoflurane's alveolar tension.
  for (t in c(1, 2, 5, 10)) {
    expect_gt(alvAt(on, "sevoflurane", t), alvAt(off, "sevoflurane", t))
  }
  # The effect is large enough to matter clinically on induction.
  expect_gt(alvAt(on, "sevoflurane", 5) / alvAt(off, "sevoflurane", 5), 1.10)
})


test_that("without a second gas the coupling still acts on the agent itself", {
  # This is the concentration effect proper: a gas taken up in bulk concentrates
  # ITSELF.  With sevoflurane alone the uptake is small, so the effect is small
  # but must still be positive and must vanish when uncoupled.
  DT <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(8, 4, 2), stringsAsFactors = FALSE)

  on  <- advanceClosedFormGas(DT, weight = 70, maximum = 30, uptakeEffect = TRUE)
  off <- advanceClosedFormGas(DT, weight = 70, maximum = 30, uptakeEffect = FALSE)
  expect_gt(alvAt(on, "sevoflurane", 5), alvAt(off, "sevoflurane", 5))

  # And nitrogen washing out gives NEGATIVE uptake, so early on the coupling can
  # act in either direction; what must hold is that turning it off changes
  # something, and that nothing goes non-finite.
  expect_true(all(is.finite(on$results$Y)))
})


test_that("uncoupled, the gases do not influence one another at all", {
  # With uptakeEffect off, adding nitrous oxide must leave sevoflurane exactly
  # unchanged.  This is what isolates the coupling as the mechanism: if the
  # gases still interacted here, some other term would be doing it.
  withN2O <- matchedDoseTable()
  without <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(8, 4, 2), stringsAsFactors = FALSE)

  a <- advanceClosedFormGas(withN2O, weight = 70, maximum = 20,
                            uptakeEffect = FALSE)
  b <- advanceClosedFormGas(without, weight = 70, maximum = 20,
                            uptakeEffect = FALSE)

  # Not identical, because the fresh gas composition differs (N2O displaces
  # oxygen, and the vaporiser dilution is the same either way), but sevoflurane
  # is delivered at 2% in both and must follow the same trajectory.
  expect_equal(alvAt(a, "sevoflurane", 10), alvAt(b, "sevoflurane", 10),
               tolerance = 1e-12)
})


test_that("the blood flow fractions are Gas Man's, and sum to one", {
  body <- getGasBody(70)
  # Pinned deliberately: these are gasman.ini [Ratio].  Changing them breaks the
  # convergence test above, which is the point -- see the note in getGasBody().
  expect_equal(body$f_brain,  0.76)
  expect_equal(body$f_muscle, 0.18)
  expect_equal(body$f_fat,    0.06)
  expect_equal(body$f_brain + body$f_muscle + body$f_fat, 1)
})
