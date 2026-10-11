# Tests for R/advanceClosedForm1.R: the time-varying PK engine, reached through
# simCpCe() whenever a drug has more than one PK event set AND the event table
# contains at least one event matching PK$pkEvents. In practice the only such
# drug is dexmedetomidine for age <= 1 year (Zuppa cardiopulmonary-bypass model:
# default + CPBStart/CPB36/.../CPB31/CPBEnd), so that model is used throughout.
#
# What is covered:
#   * The correctness anchor: with every event mapped to an IDENTICAL PK set,
#     advanceClosedForm1 must reproduce (a) the analytic closed-form
#     superposition solution computed independently below, and (b)
#     advanceClosedForm0, pointwise at shared time-grid points. This exercises
#     both the 3-compartment and the 2-compartment (lambda_3 == 0) branches of
#     convertState().
#   * End-to-end simCpCe with real CPB events: central-compartment mass balance
#     across event boundaries, bolus rises using the ACTIVE PK set's v1,
#     effect-site continuity, non-negativity/finiteness, event at t = 0, and
#     routing (non-matching events fall back to advanceClosedForm0).
#
# KNOWN LIMITATION / pinned quirk (advanceClosedForm1.R line 18):
#   start <- min(0.693/pkSets$default$lambda_4 / 4, 1)
#   getDrugPK() never creates a "lambda_4" field (the roots are lambda_1..3 and
#   the effect-site constant is ke0), so pkSets$default$lambda_4 is NULL,
#   0.693/NULL is numeric(0), and min(numeric(0), 1) silently evaluates to 1.
#   The intended field is almost certainly ke0 -- advanceClosedForm0.R line 17
#   uses min(0.693/pkSet$ke0 / 4, 1) for exactly this purpose. Consequence: the
#   exponential gap-fill grid always starts at 1 minute, so the fine sampling
#   near t = 0 / dose times degrades (for infant dexmedetomidine ke0 = 1.87/min,
#   the intended start is 0.0926 min -- about 10x finer). Concentrations at the
#   reported times remain exact (closed form); only grid density suffers. This
#   is pinned in the first test below; fixing the code to use ke0 should
#   deliberately update that test.
#
# Also noted while testing (not asserted here): simCpCe passes PK$endCe as the
# 'emerge' argument, but getDrugPK returns the field under the name 'emerge'
# (itself drugDefaults$Emerge, which is also NULL because the CSV column is
# named endCe). So 'emerge' arrives as NULL; harmless today because it is only
# consumed by recoveryCalc when plotRecovery = TRUE, which is documented broken.
#
# Cp is deliberately DISCONTINUOUS across an event boundary whenever v1
# changes: convertState() conserves compartment amounts (a1 = Cp * v1), so Cp
# jumps by v1_old/v1_new. Continuity is therefore asserted on the central
# compartment AMOUNT, and the Cp jump is asserted to match the v1 ratio.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (advanceClosedForm1 / time-varying PK engine).
# Expected values derived from first principles: analytic superposition of the
# closed-form bolus/infusion exponential solutions (analyticCp below),
# central-compartment mass balance (Cp * v1) across event boundaries, and the
# dose/v1 identity for instantaneous bolus rises (sum of the p_coef_bolus
# coefficients is algebraically 1/v1). No expected value was pasted back from
# the code's own output except where explicitly marked as a pinned quirk.
# Verified by running against the working tree (byte-identical to master for
# the files under test) on R 4.6.1 / testthat 3.3.2.

# ---------------------------------------------------------------------------
# Shared fixtures (deterministic; computed once for the whole file)
# ---------------------------------------------------------------------------

dd <- getDrugDefaults("dexmedetomidine")

# Infant (age <= 1 year): the multi-event CPB model, 2-compartment (cl3 = 0).
# Concentration.Units is "ng" so "mcg" doses pass through unconverted and
# concentrations come out in ng/mL (volumes are liters; mcg/L == ng/mL).
PKinf <- getDrugPK("dexmedetomidine", weight = 5, height = 60, age = 0.5,
                   sex = "female", drugDefaults = dd)

# Adult (age > 1): single "default" event set, full 3-compartment model.
PKad <- getDrugPK("dexmedetomidine", weight = 70, height = 170, age = 50,
                  sex = "male", drugDefaults = dd)

v1s <- sapply(PKinf$PK, function(x) x$v1)   # named by event

# Extract one Site's rows from simCpCe's long-format results
siteRows <- function(out, site) {
  r <- out$results
  r[r$Site == site, ]
}

# Value at an exact grid time (the timeline is deterministic, so grid times can
# be matched with a tight absolute fuzz for floating point)
atT <- function(df, t) {
  i <- which(abs(df$Time - t) < 1e-9)
  stopifnot(length(i) == 1)
  df$Y[i]
}

# Independent expected value: Cp(t) by superposition of closed-form solutions.
# For a linear mammillary model the unit disposition function is
#   sum_j p_coef_bolus_lj * exp(-lambda_j * t),
# so a bolus D at tb contributes D * sum_j p_coef_bolus_lj e^{-lambda_j (t-tb)}
# and a constant-rate infusion R over [t0, t1] contributes (per exponential,
# using p_coef_infusion_lj = p_coef_bolus_lj / lambda_j):
#   R * p_coef_infusion_lj * (1 - e^{-lambda_j (te - t0)}) * e^{-lambda_j (t - te)}
# with te = min(t, t1) -- the standard rise-then-washout expression obtained by
# integrating the disposition function against the input. This uses only the
# coefficients from getDrugPK plus hand algebra; the time propagation is
# independent of the advanceState/advanceClosedForm* recursions under test.
analyticCp <- function(tvec, pk, bolusTab, infTab) {
  sapply(tvec, function(t) {
    cp <- 0
    for (j in 1:3) {
      lam  <- pk[[paste0("lambda_", j)]]
      pb   <- pk[[paste0("p_coef_bolus_l", j)]]
      pinf <- pk[[paste0("p_coef_infusion_l", j)]]
      for (b in seq_len(nrow(bolusTab))) {
        if (t >= bolusTab$Time[b])
          cp <- cp + bolusTab$Dose[b] * pb * exp(-lam * (t - bolusTab$Time[b]))
      }
      for (r in seq_len(nrow(infTab))) {
        t0 <- infTab$Start[r]; t1 <- infTab$End[r]; R <- infTab$Rate[r]
        if (t > t0) {
          te <- min(t, t1)
          cp <- cp + R * pinf * (1 - exp(-lam * (te - t0))) * exp(-lam * (t - te))
        }
      }
    }
    cp
  })
}

# --- End-to-end fixture A: single CPB event at t = 30 -----------------------
# 5 mcg bolus at t=0, 1 mcg/kg/hr infusion 10->60, 3 mcg bolus at t=45 (after
# the event boundary). "CPB Start" tests simCpCe's space-stripping: it becomes
# "CPBStart", which is in pkEvents. The extra Fill column mimics the app's
# event table; simCpCe keeps only the first two columns.
doseSingle <- data.frame(
  Time  = c(0, 10, 60, 45),
  Dose  = c(5, 1, 0, 3),
  Units = c("mcg", "mcg/kg/hr", "mcg/kg/hr", "mcg")
)
evSingle <- data.frame(Time = 30, Event = "CPB Start", Fill = "x")
outSingle <- simCpCe(doseSingle, evSingle, PKinf, maximum = 90, plotRecovery = FALSE)
cpSingle <- siteRows(outSingle, "Plasma")
ceSingle <- siteRows(outSingle, "Effect Site")

# --- End-to-end fixture B: three CPB events (Start -> 34C -> End) -----------
doseMulti <- data.frame(
  Time  = c(0, 5, 70, 55),
  Dose  = c(5, 1, 0, 2),
  Units = c("mcg", "mcg/kg/hr", "mcg/kg/hr", "mcg")
)
evMulti <- data.frame(
  Time  = c(20, 35, 50),
  Event = c("CPB Start", "CPB 34", "CPB End"),
  Fill  = c("a", "b", "c")
)
outMulti <- simCpCe(doseMulti, evMulti, PKinf, maximum = 90, plotRecovery = FALSE)
cpMulti <- siteRows(outMulti, "Plasma")
ceMulti <- siteRows(outMulti, "Effect Site")

# ---------------------------------------------------------------------------
# Pinned quirk: pkSets$default$lambda_4 does not exist (line 18)
# ---------------------------------------------------------------------------

test_that("lambda_4 is absent from getDrugPK output and the fine-grid start degrades to 1 (pinned quirk)", {
  # The field the code asks for does not exist...
  expect_false("lambda_4" %in% names(PKinf$PK$default))
  # ...but ke0 (the field advanceClosedForm0 uses for the same purpose) does.
  expect_true("ke0" %in% names(PKinf$PK$default))
  expect_gt(PKinf$PK$default$ke0, 0)

  # The exact expression from advanceClosedForm1.R line 18: NULL arithmetic
  # yields numeric(0), and min(numeric(0), 1) is 1 -- silently, no warning.
  expect_equal(min(0.693 / PKinf$PK$default$lambda_4 / 4, 1), 1)

  # Had the code used ke0 (as advanceClosedForm0 does), the infant model would
  # get a much finer grid: 0.693/1.87/4 = 0.0926 min.
  expect_lt(min(0.693 / PKinf$PK$default$ke0 / 4, 1), 0.1)

  # Observable consequence, pinned: for the SAME infant PK, the single-set
  # route (advanceClosedForm0, ke0-based start) places grid points inside
  # (0, 1) min, while the event route (advanceClosedForm1, start stuck at 1)
  # places none. Fixing line 18 to use ke0 should flip the second expectation
  # -- update this test deliberately when that happens.
  evIgnored <- data.frame(Time = 20, Event = "Incision", Fill = "x")  # not a pkEvent
  outACF0 <- simCpCe(doseSingle, evIgnored, PKinf, maximum = 90, plotRecovery = FALSE)
  t0 <- siteRows(outACF0, "Plasma")$Time
  t1 <- cpSingle$Time
  expect_true(any(t0 > 0 & t0 < 1))    # fine ke0-based grid near t = 0
  expect_false(any(t1 > 0 & t1 < 1))   # pinned quirk: coarse grid, start = 1
})

# ---------------------------------------------------------------------------
# Correctness anchor: identical PK sets == time-invariant closed form
# ---------------------------------------------------------------------------

test_that("with identical PK sets advanceClosedForm1 reproduces the analytic solution and advanceClosedForm0", {
  # Direct engine calls, mimicking simCpCe's post-processed inputs: doses in
  # absolute base units with Bolus flags; events with the default row at t = 0
  # and the sentinel duplicate of the last event at t = maximum.
  dose <- data.frame(
    Time  = c(0, 10, 60),
    Dose  = c(100, 1, 0),      # 100 mcg bolus at 0; 1 mcg/min from 10 to 60
    Bolus = c(TRUE, FALSE, FALSE)
  )
  bolusTab <- data.frame(Time = 0, Dose = 100)
  infTab   <- data.frame(Start = 10, End = 60, Rate = 1)
  events <- data.frame(
    Time = c(0, 30, 120),
    Event = c("default", "middle", "middle"),
    stringsAsFactors = FALSE
  )

  # -- Adult set: full 3-compartment, exercises the general convertState path
  pk <- PKad$PK$default
  r1 <- advanceClosedForm1(dose, events, list(default = pk, middle = pk),
                           maximum = 120, plotRecovery = FALSE, emerge = 1)
  r0 <- advanceClosedForm0(dose, pk, maximum = 120, plotRecovery = FALSE, emerge = 1)

  # Output structure; Recovery is all zero when plotRecovery = FALSE
  expect_named(r1, c("Time", "Cp", "Ce", "Recovery"))
  expect_true(all(r1$Recovery == 0))

  # The recursion is algebraically exact for boluses + piecewise-constant
  # infusions, and with identical sets convertState must be the identity, so
  # agreement with the independent superposition is pure floating point
  # (measured ~7e-17 relative; asserted at 1e-10).
  a1 <- analyticCp(r1$Time, pk, bolusTab, infTab)
  a0 <- analyticCp(r0$Time, pk, bolusTab, infTab)
  expect_lt(max(abs(r1$Cp - a1)), 1e-10 * max(a1))
  expect_lt(max(abs(r0$Cp - a0)), 1e-10 * max(a0))

  # Pointwise equality with advanceClosedForm0 at shared grid times (the two
  # timelines share the dose anchors and the gap-fill points ahead of the
  # first synthetic event; no interpolation needed at shared points).
  common <- which(sapply(r0$Time, function(t) any(abs(r1$Time - t) < 1e-9)))
  idx1 <- sapply(r0$Time[common], function(t) which.min(abs(r1$Time - t)))
  expect_gte(length(common), 40)                       # a meaningful overlap
  expect_lt(max(abs(r0$Cp[common] - r1$Cp[idx1])), 1e-10 * max(r0$Cp))

  # Ce agreement is looser BY CONSTRUCTION: calculateCe is a grid-dependent
  # quadrature and the two grids differ after the first event anchor
  # (measured max deviation 3.2e-4 of max Ce; asserted at 1e-3).
  expect_lt(max(abs(r0$Ce[common] - r1$Ce[idx1])), 1e-3 * max(r0$Ce))

  # -- Infant set: 2-compartment (lambda_3 == 0), exercises the two-compartment
  # branch of convertState. Same identical-sets argument, same FP tolerance.
  pki <- PKinf$PK$default
  r1i <- advanceClosedForm1(dose, events, list(default = pki, middle = pki),
                            maximum = 120, plotRecovery = FALSE, emerge = 1)
  a1i <- analyticCp(r1i$Time, pki, bolusTab, infTab)
  expect_lt(max(abs(r1i$Cp - a1i)), 1e-10 * max(a1i))
})

# ---------------------------------------------------------------------------
# Real PK transition: mass balance across the event boundary
# ---------------------------------------------------------------------------

test_that("across a CPB event the central-compartment amount is continuous and Cp jumps by the v1 ratio", {
  # Sanity: t = 0 bolus appears instantly at dose/v1 of the default set
  # (sum of p_coef_bolus coefficients is algebraically 1/v1).
  expect_equal_rounded(5 / v1s[["default"]], atT(cpSingle, 0))

  # The timeline inserts eventTime - 0.01, so the pair (29.99, 30) straddles
  # the boundary. convertState conserves amounts, so Cp * v1 is continuous up
  # to 0.01 min of genuine kinetics (elimination + intercompartmental transfer
  # + infusion input at rate constants ~0.05/min => bound ~1e-3; measured
  # ~2e-5). Tolerance 2e-3.
  amtBefore <- atT(cpSingle, 29.99) * v1s[["default"]]
  amtAfter  <- atT(cpSingle, 30)    * v1s[["CPBStart"]]
  expect_equal(amtAfter, amtBefore, tolerance = 2e-3)

  # Cp itself jumps -- by design -- by v1_default / v1_CPBStart = 1.148
  # (amount conserved in a smaller central volume => higher concentration).
  expect_equal(atT(cpSingle, 30) / atT(cpSingle, 29.99),
               v1s[["default"]] / v1s[["CPBStart"]], tolerance = 2e-3)
  # ...and the PK change is genuine, not a no-op:
  expect_gt(abs(atT(cpSingle, 30) / atT(cpSingle, 29.99) - 1), 0.1)

  # The effect site is driven through ke0 and must stay continuous even though
  # its input (Cp) jumps: the step across 0.01 min stays below 1% of max Ce
  # (measured ~8e-4 of max Ce).
  ceJump <- abs(atT(ceSingle, 30) - atT(ceSingle, 29.99))
  expect_lt(ceJump, 0.01 * max(ceSingle$Y))
})

test_that("a bolus given after an event boundary rises by dose/v1 of the ACTIVE PK set", {
  # 3 mcg bolus at t = 45, during CPB: the instantaneous rise across the
  # (44.99, 45) pair must be dose / v1_CPBStart, NOT dose / v1_default.
  # Tolerance 2e-3 covers 0.01 min of decay of the background concentration
  # (measured relative error ~7e-5).
  rise <- atT(cpSingle, 45) - atT(cpSingle, 44.99)
  expect_equal(rise, 3 / v1s[["CPBStart"]], tolerance = 2e-3)
  # Distinguishable from the wrong volume (the v1s differ by ~15%)
  expect_gt(abs(rise - 3 / v1s[["default"]]), 0.04 * rise)

  # Same check on the multi-event run: bolus at t = 55 lands after "CPB End",
  # so the CPBEnd volume (11.07 L) must be in effect.
  riseEnd <- atT(cpMulti, 55) - atT(cpMulti, 54.99)
  expect_equal(riseEnd, 2 / v1s[["CPBEnd"]], tolerance = 2e-3)
})

# ---------------------------------------------------------------------------
# Multi-event simulation: mass balance at every boundary + well-formed output
# ---------------------------------------------------------------------------

test_that("a three-event CPB simulation conserves mass at each boundary and stays finite and non-negative", {
  # Amount continuity at all three transitions:
  #   default -> CPBStart (t=20), CPBStart -> CPB34 (t=35), CPB34 -> CPBEnd (t=50)
  transitions <- data.frame(
    t     = c(20, 35, 50),
    from  = c("default", "CPBStart", "CPB34"),
    to    = c("CPBStart", "CPB34", "CPBEnd"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(transitions))) {
    amtBefore <- atT(cpMulti, transitions$t[i] - 0.01) * v1s[[transitions$from[i]]]
    amtAfter  <- atT(cpMulti, transitions$t[i])        * v1s[[transitions$to[i]]]
    # measured relative differences 6e-5, 5e-5, 5e-4; bound 2e-3 (0.01 min of
    # kinetics, largest for CPBEnd whose clearance is highest)
    expect_equal(amtAfter, amtBefore, tolerance = 2e-3)
    # and Cp jumps by exactly the volume ratio at each boundary
    expect_equal(atT(cpMulti, transitions$t[i]) / atT(cpMulti, transitions$t[i] - 0.01),
                 v1s[[transitions$from[i]]] / v1s[[transitions$to[i]]],
                 tolerance = 2e-3)
  }

  # Finite and non-negative everywhere, for both plasma and effect site
  expect_true(all(is.finite(cpMulti$Y)))
  expect_true(all(cpMulti$Y >= 0))
  expect_true(all(is.finite(ceMulti$Y)))
  expect_true(all(ceMulti$Y >= 0))

  # The simulation spans the full requested window
  expect_equal(min(cpMulti$Time), 0)
  expect_equal(max(cpMulti$Time), 90)

  # Well-formed downstream products: equiSpace on the RESOLUTION grid with a
  # clean t = 0 (simCpCe zeroes the first Ce), and a consistent max table
  expect_equal(nrow(outMulti$equiSpace), RESOLUTION)
  expect_true(all(is.finite(outMulti$equiSpace$Ce)))
  expect_true(all(outMulti$equiSpace$Ce >= 0))
  expect_equal(outMulti$equiSpace$Ce[1], 0)
  expect_equal(outMulti$max$Cp, max(cpMulti$Y))
  expect_equal(outMulti$max$Ce, max(ceMulti$Y))
})

# ---------------------------------------------------------------------------
# Event at t = 0 and event-name routing
# ---------------------------------------------------------------------------

test_that("an event at t = 0 replaces the default PK set from the very start", {
  # With "CPB Start" at t = 0, simCpCe does NOT prepend a default row, so the
  # t = 0 bolus must distribute into the CPBStart central volume.
  ev0 <- data.frame(Time = 0, Event = "CPB Start", Fill = "x")
  out0 <- simCpCe(doseSingle, ev0, PKinf, maximum = 90, plotRecovery = FALSE)
  cp0 <- siteRows(out0, "Plasma")
  expect_equal_rounded(5 / v1s[["CPBStart"]], atT(cp0, 0))
  # ...which is measurably different (15%) from the default-volume answer
  expect_gt(abs(atT(cp0, 0) - 5 / v1s[["default"]]), 0.05 * atT(cp0, 0))
  # and the simulation still runs to the requested maximum, finite throughout
  expect_equal(max(cp0$Time), 90)
  expect_true(all(is.finite(cp0$Y)))
})

test_that("events not in pkEvents are ignored and the single-set engine is used", {
  # "Incision" is not a dexmedetomidine pkEvent, so after filtering the event
  # table is empty and simCpCe must route to advanceClosedForm0. Verify by
  # reproducing the result with a direct advanceClosedForm0 call on
  # hand-converted doses (mcg pass through; 1 mcg/kg/hr * 5 kg / 60 min).
  evIgnored <- data.frame(Time = 20, Event = "Incision", Fill = "x")
  outACF0 <- simCpCe(doseSingle, evIgnored, PKinf, maximum = 90, plotRecovery = FALSE)
  cpACF0 <- siteRows(outACF0, "Plasma")
  doseConv <- data.frame(
    Time  = c(0, 10, 60, 45),
    Dose  = c(5, 1 * 5 / 60, 0, 3),
    Bolus = c(TRUE, FALSE, FALSE, TRUE)
  )
  r0 <- advanceClosedForm0(doseConv, PKinf$PK$default, maximum = 90,
                           plotRecovery = FALSE, emerge = 1)
  ord <- order(cpACF0$Time)
  # identical grid (including the ke0-based fine start) and identical values
  expect_equal(cpACF0$Time[ord], r0$Time, tolerance = 1e-12)
  expect_equal(cpACF0$Y[ord], r0$Cp, tolerance = 1e-12)
})
