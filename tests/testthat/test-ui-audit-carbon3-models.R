# Carbon's third UI audit (Gemini, of PR #730 at 4164743, 2026-11-15),
# findings H3 and M1: the model-level defects.
#
# H3: four getDrugPK() stops on legal inputs, each of which took every drug
# off the plot (or failed a Bayesian update):
#   1. hydrocodone in a patient whose oral plasma curve peaks after its fixed
#      90-min tPeak: ke0FromTPeak() stopped.  It now returns the ke0 of the
#      nearest reachable peak (R/getDrugPK.R).
#   2. vancomycin below a creatinine clearance of 1.06 mL/min, where the
#      Thomson line gives no clearance: held at the lowest Thomson studied,
#      12 mL/min (R/drugs_vancomycin.R).
#   3. methadone at 0.1 kg: the racemic reduction's fit failed (an exponent of
#      1e10 /min, equal or negative exponents) and ke0FromTPeakBolus() then
#      stopped; a fit that is not a reduction is refitted from the slow phases
#      (R/drugs_methadone.R).
#   4. ke0FromTPeakBolus() (and the oral solve) at a Bayesian candidate of 20x
#      clearance and 1/20 volume: no bracket reaches the ke0; the nearest
#      bracket end is returned.
# M1: the turnover (warfarin) and indirect (fluindione, acenocoumarol) INR
# grew without limit while the inhibition sat at 1; held at INR_CEILING, 21
# (R/inr.R).
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer.

noEvents <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

# The oral plasma and effect-site peaks of a resolved PK set, rebuilt as
# getDrugPK() builds the oral curve for ke0FromTPeak()
oralPeaks <- function(set) {
  lambda <- c(set$lambda_1, set$lambda_2, set$lambda_3)
  coef <- c(set$p_coef_bolus_l1, set$p_coef_bolus_l2, set$p_coef_bolus_l3) *
    set$ka_PO / (set$ka_PO - lambda) * set$bioavailability_PO
  use <- lambda > 0
  oc <- c(coef[use], -sum(coef[use]))
  ol <- c(lambda[use], set$ka_PO)
  lag <- if (is.null(set$tlag_PO)) 0 else set$tlag_PO
  c(plasma = lag + stats::optimize(function(t) sum(oc * exp(-ol * t)), c(0, 4000),
                                   maximum = TRUE)$maximum,
    effect = lag + effectSitePeakTime(oc, ol, set$ke0),
    fastest = max(ol))
}

# ---- H3.1: hydrocodone, plasma peak after tPeak ----------------------------

test_that("H3.1: hydrocodone whose plasma peaks after 90 min gets an effect site that follows it", {
  # 90 years, 40 kg, 190 cm, a woman with a creatinine of 15: the oral plasma
  # curve peaks at 93.3 min, after the fixed tPeak of 90
  pk <- getDrugPK("hydrocodone", 40, 190, 90, "female", creatinine = 15)
  set <- pk$PK$default
  peaks <- oralPeaks(set)
  expect_gt(peaks[["plasma"]], 90)
  expect_true(is.finite(set$ke0))
  # The nearest reachable peak: ke0 = max(10, 100 x the fastest rate)
  expect_equal(set$ke0, max(10, KE0_FOLLOWS_PLASMA_FACTOR * peaks[["fastest"]]),
               tolerance = 1e-6)
  expect_lt(peaks[["effect"]] - peaks[["plasma"]], 0.5)
  expect_gte(peaks[["effect"]], peaks[["plasma"]] - 1e-3)
  # and the plot's simulation runs, with a finite effect site
  sim <- simCpCe(data.frame(Drug = "hydrocodone", Time = 0, Dose = 10, Units = "mg PO"),
                 noEvents, pk, 600, plotRecovery = TRUE)
  expect_true(all(is.finite(sim$wide$Plasma)))
  expect_true(all(is.finite(sim$wide$`Effect Site`)))
})

test_that("H3.1: the oral fallback is continuous with the solved ke0 at the boundary", {
  # One compartment, k = 0.01, ka = 0.05 /min: the plasma curve peaks at
  # log(ka / k) / (ka - k) = 40.24 min
  k <- 0.01; ka <- 0.05
  coef <- 1 * ka / (ka - k)
  plasmaPeak <- log(ka / k) / (ka - k)
  follows <- max(10, KE0_FOLLOWS_PLASMA_FACTOR * ka)
  # At or before the plasma peak: the effect site follows the plasma
  expect_equal(ke0FromTPeak(plasmaPeak - 5, coef, k, ka), follows)
  expect_equal(ke0FromTPeak(plasmaPeak, coef, k, ka), follows)
  peakAt <- function(ke0) effectSitePeakTime(c(coef, -coef), c(k, ka), ke0)
  # Just after it, nearer than an effect site at `follows` peaks (about
  # 1 / follows = 0.1 min after the plasma): the bracket's end, so ke0 rises
  # to `follows` and stays there, continuous across the boundary
  expect_equal(ke0FromTPeak(plasmaPeak + 0.05, coef, k, ka), follows)
  expect_lt(peakAt(follows) - plasmaPeak, 0.11)
  # A little later, within reach: solved, and the effect site peaks there
  ke0 <- ke0FromTPeak(plasmaPeak + 0.5, coef, k, ka)
  expect_lt(ke0, follows)
  expect_equal(peakAt(ke0), plasmaPeak + 0.5, tolerance = 1e-5)
  # Well after it, exactly as before
  expect_equal(peakAt(ke0FromTPeak(60, coef, k, ka)), 60, tolerance = 1e-5)

  # A fast absorption (ka 1 /min) puts `follows` at 100 /min, and a tPeak
  # 0.05 min after the plasma peak needs a ke0 near 20, above the old
  # bracket's fixed end of 10 /min, where uniroot() stopped: now solved
  ka <- 1
  coef <- 1 * ka / (ka - k)
  plasmaPeak <- log(ka / k) / (ka - k)
  ke0 <- ke0FromTPeak(plasmaPeak + 0.05, coef, k, ka)
  expect_gt(ke0, 10)
  expect_lt(ke0, KE0_FOLLOWS_PLASMA_FACTOR * ka)
  expect_equal(effectSitePeakTime(c(coef, -coef), c(k, ka), ke0), plasmaPeak + 0.05,
               tolerance = 1e-4)
})

test_that("H3.1: every oral tPeak in the library is reachable at the reference patient", {
  # What the stop in ke0FromTPeak() used to guard (a tPeak entered before the
  # drug's own plasma peak) is a property of the drug, checked here: the
  # effect site must peak AT tPeak, so the fallback is never what sets it.
  dd <- getDrugDefaultsGlobal()
  drugs <- dd$Drug[!is.na(dd$Category) & vapply(dd$Drug, exists, logical(1), mode = "function")]
  oral <- 0
  for (d in drugs) {
    pk <- getDrugPK(d, 70, 170, 35, "male")
    if (!identical(pk$tPeakRoute, ROUTE_PO) || !isTRUE(pk$tPeak > 0)) next
    oral <- oral + 1
    peaks <- oralPeaks(pk$PK$default)
    expect_lt(peaks[["plasma"]], pk$tPeak, label = d)
    expect_equal(peaks[["effect"]], pk$tPeak, tolerance = 1e-4, label = d)
  }
  expect_gte(oral, 2)   # hydrocodone and pregabalin at least
})

# ---- H3.2: vancomycin below 1.06 mL/min ------------------------------------

test_that("H3.2: vancomycin's creatinine clearance is held at 12 mL/min, the lowest Thomson studied", {
  floor <- 2.99 * (1 + 0.0154 * (THOMSON_MIN_CRCL - 66)) / 60    # L/min
  expect_equal(THOMSON_MIN_CRCL, 12)
  # 0.93 mL/min (90 y, 20 kg, 150 cm, creatinine 15, switch off): stopped before
  expect_equal(getDrugPK("vancomycin", 20, 150, 90, "male", creatinine = 15,
                         adjustToFFM = FALSE)$PK$default$cl1, floor)
  # A 0.1 kg newborn at an assumed creatinine (0.33 mL/min)
  expect_equal(getDrugPK("vancomycin", 0.1, 10, 0, "male")$PK$default$cl1, floor)
  # At and above 12 mL/min the line is Thomson's: a 70 kg man of 50 years
  crcl <- creatinineClearanceCG(70, 50, "male")
  expect_gt(crcl, 12)
  expect_equal(vancomycin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default$cl1,
               2.99 * (1 + 0.0154 * (crcl - 66)) / 60)
  # and the plot's simulation runs
  pk <- getDrugPK("vancomycin", 20, 150, 90, "male", creatinine = 15, adjustToFFM = FALSE)
  sim <- simCpCe(data.frame(Drug = "vancomycin", Time = 0, Dose = 1000, Units = "mg"),
                 noEvents, pk, 1440, plotRecovery = TRUE)
  expect_true(all(is.finite(sim$wide$Plasma)))
  expect_gt(max(sim$wide$Plasma), 0)
})

# ---- H3.3: methadone at 0.1 kg ---------------------------------------------

test_that("H3.3: methadone's racemic reduction holds at the smallest legal patients", {
  udf <- function(x, t) colSums(x$coef * exp(-outer(x$lambda, t)))
  t <- exp(seq(log(0.5), log(7 * 1440), length.out = 200))
  for (sex in c("female", "male")) {
    # 0 years, 0.1 kg, 10 cm: the female stopped in ke0FromTPeakBolus(), the
    # male gave an exponent of 3.5e9 /min and two equal ones without a word
    pk <- getDrugPK("methadone", 0.1, 10, 0, sex)
    set <- pk$PK$default
    lambda <- c(set$lambda_1, set$lambda_2, set$lambda_3)
    expect_true(all(is.finite(lambda)) && all(lambda > 0), label = sex)
    expect_true(is.finite(set$ke0) && set$ke0 > 0, label = sex)
    pkW <- pkSizeFactors(0.1, 10, 0, sex, TRUE)$pkWeight
    e <- methadoneEnantiomers(pkW)
    expect_lte(max(lambda), max(e$R$lambda, e$S$lambda))
    expect_gt(min(diff(sort(log(lambda)))), 1e-3)
    racemic <- methadoneRacemicPK(pkW)
    u <- methadoneUdf(racemic$v1, racemic$v2, racemic$v3, racemic$cl1, racemic$cl2, racemic$cl3)
    enantiomers <- 0.5 * (udf(e$R, t) + udf(e$S, t))
    expect_lt(max(abs(udf(u, t) / enantiomers - 1)), 0.01, label = sex)
  }
  # Over the whole range of pharmacokinetic weights the reduction's exponents
  # stay among the six it reduces
  for (w in c(0.05, 0.1, 0.12, 0.15, 0.2, 1, 10, 70, 300)) {
    e <- methadoneEnantiomers(w)
    racemic <- methadoneRacemicPK(w)
    u <- methadoneUdf(racemic$v1, racemic$v2, racemic$v3, racemic$cl1, racemic$cl2, racemic$cl3)
    expect_lte(max(u$lambda), max(e$R$lambda, e$S$lambda) * (1 + 1e-6), label = paste(w, "kg"))
  }
})

# ---- H3.4: the Bayesian search's corners -----------------------------------

test_that("H3.4: every drug resolves at the corners of the Bayesian search", {
  # BAYES_FACTOR_LIMIT either way on clearance and volume; at 20x clearance
  # and 1/20 volume oxymorphone, pregabalin, epinephrine and vasopressin
  # stopped in uniroot(), and hydrocodone at 1/20 clearance in ke0FromTPeak()
  dd <- getDrugDefaultsGlobal()
  drugs <- dd$Drug[!is.na(dd$Category) & vapply(dd$Drug, exists, logical(1), mode = "function")]
  f <- c(1 / BAYES_FACTOR_LIMIT, BAYES_FACTOR_LIMIT)
  for (d in drugs) {
    if (!isTRUE(getDrugPK(d, 70, 170, 35, "male")$tPeak > 0)) next
    for (cl in f) for (v in f) {
      r <- tryCatch({
        pk <- getDrugPK(d, 70, 170, 35, "male", individual = list(clearance = cl, volume = v))
        k <- vapply(pk$PK, function(set) set$ke0, numeric(1))
        if (all(is.finite(k) & k > 0)) "ok" else "ke0 not finite and positive"
      }, error = function(e) conditionMessage(e))
      expect_identical(r, "ok", label = sprintf("%s at clearance x%g, volume x%g", d, cl, v))
    }
  }
})

test_that("H3.4: a plasma curve too brief for any bracketed ke0 gets the smallest, and simulates", {
  for (d in c("oxymorphone", "pregabalin", "epinephrine", "vasopressin")) {
    pk <- getDrugPK(d, 70, 170, 35, "male", individual = list(clearance = 20, volume = 0.05))
    expect_lte(pk$PK$default$ke0, KE0_SOLVE_MIN, label = d)
  }
  pk <- getDrugPK("epinephrine", 70, 170, 35, "male", individual = list(clearance = 20, volume = 0.05))
  sim <- simCpCe(data.frame(Drug = "epinephrine", Time = 0, Dose = 10, Units = "mcg"),
                 noEvents, pk, 60, plotRecovery = TRUE)
  expect_true(all(is.finite(sim$wide$Plasma)))
  expect_true(all(is.finite(sim$wide$`Effect Site`)))
  # An effect site that slow holds almost nothing, as the exact ke0 would
  expect_lt(max(sim$wide$`Effect Site`), 1e-6 * max(sim$wide$Plasma))
})

test_that("H3.4: a bolus tPeak inside the bracket is solved exactly as before", {
  pk <- getDrugPK("fentanyl", 70, 170, 40, "male")$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  for (tPeak in c(0.5, 3.694, 150)) {
    ke0 <- ke0FromTPeakBolus(tPeak, coef, lambda)
    expect_gt(ke0, KE0_SOLVE_MIN)
    expect_lt(ke0, KE0_SOLVE_MAX)
    expect_equal(effectSitePeakTime(coef, lambda, ke0), tPeak, tolerance = 1e-5)
  }
})

# ---- M1: the INR ceiling ----------------------------------------------------

test_that("M1: the indirect and turnover INR are held at INR_CEILING, and unchanged below it", {
  expect_equal(INR_CEILING, 21)
  # Below the ceiling the INR is the reciprocal of the activity, bit for bit
  activity <- c(1, 0.5, 0.2, 0.1, 1 / 20, 1 / 21 + 1e-12)
  expect_identical(inrFromActivity(activity), 1 / activity)
  # At and beyond it, and for an activity rounded to zero or below
  expect_equal(inrFromActivity(c(1 / 21, 1e-12, 0, -1e-17)), rep(21, 4))
  # Fluindione held at a saturating concentration for 60 days: 1/INR decays at
  # kout (half-life 17.7 h), which made the INR 1e24
  inr <- fluindione(70, 170, 50, "male")$inr
  time <- seq(0, 60 * 1440, by = 60)
  out <- inrIndirect(time, rep(1e6, length(time)), inr)
  expect_true(all(is.finite(out)))
  expect_equal(max(out), INR_CEILING)
  # Warfarin's turnover form through the engine path (R-warfarin present)
  w <- warfarin(70, 170, 50, "male")$inr
  parts <- list(data.frame(Time = time, Cp = 1e6), data.frame(Time = time, Cp = 1e-3))
  res <- inrEngineResults(parts, w)
  expect_true(all(is.finite(res$Cp)))
  expect_equal(max(res$Cp), INR_CEILING)
})

test_that("M1: an absurd fluindione dose plots a bounded INR, a clinical one the model's own", {
  run <- function(dose, units, days) {
    x <- simulateDrugsWithCovariates(data.frame(Drug = "fluindione", Time = 0, Dose = dose, Units = units),
                                     noEvents, 70, 170, 50, "male", days * 1440, FALSE)
    r <- x$fluindione$results
    r$Y[r$Site == "Plasma"]
  }
  absurd <- run(1e6, "mg PO", 30)
  expect_true(all(is.finite(absurd)))
  expect_equal(max(absurd), INR_CEILING)
  # 20 mg daily for four weeks peaks near 3 (2.98 when this was written), far
  # below the ceiling, so the ceiling cannot have touched it
  clinical <- run(20, "mg PO qd", 28)
  expect_lt(max(clinical), 4)
  expect_gt(max(clinical), 2)
})
