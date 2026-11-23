# Methadone: Henthorn and Kharasch 2025, R(-) + S(+) summed to racemate and
# reduced to one three-compartment model per patient (see the file header of
# R/drugs_methadone.R).  The reduced parameters come out of an optimiser, so
# they are pinned to 1e-5 rather than the usual 1.5e-6, which leaves room for
# floating-point differences between platforms without hiding a real change.

# Henthorn's cohort (Table 1, p740) as the model anchors it, and its factors
# computed independently of pkSizeFactors() (helpers.R)
methadoneCohort <- cohortFactorsIndependent(list(groups = list(
  list(weight = 74, height = 177.3, age = 29, men = 34, women = 0),
  list(weight = 74, height = 163.4, age = 29, men = 0, women = 30))))
methadoneRacemicCL <- 1 / (0.5 / 0.103 + 0.5 / 0.09102) / (309.45 / 345.91)

test_that("reference man: reduced parameters, oral route and pharmacodynamics", {
  actual <- methadone(70, 170, 50, "male")

  # Since 2026-11-19 every parameter but V3 is scaled to fat-free mass and
  # anchored at Henthorn's cohort (the header's BODY SIZE section): the
  # reference man's Vc, V2 and clearances are Table 2's divided by the
  # cohort's factors (0.948 and 0.961), V3 is his own weight term's, and the
  # reduction is refitted.  Before, v1 34.6029236, v2 130.4042348, v3
  # 178.5251339, cl1 0.1080264692, cl2 12.9415885, cl3 0.4803807328.
  expect_equal(
    actual$PK$default,
    list(
      v1 = 36.50621513,
      v2 = 137.6094391,
      v3 = 178.7502506,
      cl1 = 0.1124566553,
      cl2 = 13.47302684,
      cl3 = 0.4992306531,
      ka_PO = 0.0100236846,
      bioavailability_PO = 0.70,
      tlag_PO = 0
    ),
    tolerance = 1e-5
  )
  # Kharasch 2004: peak miosis 10-15 min after IV methadone.  On Inturrisi
  # 1987's own disposition a 10 min tPeak is a t1/2 ke0 of 15.4 min, inside
  # Inturrisi's 1.3-23.1 min range (worked out by hand); see the header.
  expect_equal(actual$tPeak, 10)
  expect_equal(actual$MEAC, 0.06)
  expect_equal(actual$typical, 0.072)
  expect_equal(actual$lowerTypical, 0.048)
  expect_equal(actual$upperTypical, 0.12)
  expect_match(actual$reference, "Henthorn")
})

test_that("racemic clearance is the enantiomer clearances combined exactly", {
  # Worked from Henthorn Table 2 by hand: R(-) 0.024 + 0.026 + 0.053 = 0.103
  # L/min; S(+) 0.024 x 1.30 + 0.026 x 0.57 + 0.045 = 0.09102 L/min.  Half the
  # dose to each, so racemic clearance is the harmonic mean, and the
  # hydrochloride-to-base factor divides it.  With the switch on every
  # clearance of both enantiomers takes the anchored clearance factor, so the
  # racemic clearance takes it exactly; with it off, none.
  clR <- 0.103
  clS <- 0.09102
  racemic <- 1 / (0.5 / clR + 0.5 / clS)
  expected <- racemic / (309.45 / 345.91)
  for (w in c(50, 70, 120)) {
    factor <- pkSizeFactors(w, 170, 50, "male", TRUE)$clearance /
      methadoneCohort$clearance
    expect_equal(methadone(w, 170, 50, "male")$PK$default$cl1,
                 expected * factor, tolerance = 1e-9)
    expect_equal(methadone(w, 170, 50, "male", adjustToFFM = FALSE)$PK$default$cl1,
                 expected, tolerance = 1e-9)
  }
})

test_that("size: the cohort's typical patient gets Table 2, and FFM scales the rest", {
  # (a) A man at the cohort's height and age whose fat-free mass is the
  # cohort's pooled typical one (34 men and 30 women of 74 kg) receives
  # Henthorn's Table 2 values for every parameter but V3: Vc 41.0 L (S(+)
  # x 0.60), Q2 13.3 L/min (S(+) x 0.77), and the racemic clearance.
  wCohort <- uniroot(function(w) ffmAlSallami(w, 177.3, 29, "male") / FFM_REFERENCE -
                       methadoneCohort$volume, c(40, 120), tol = 1e-12)$root
  size <- pkSizeFactors(wCohort, 177.3, 29, "male", TRUE,
                        cohort = do.call(cohortSizeFactors, METHADONE_COHORT))
  expect_equal(c(size$volume, size$clearance), c(1, 1), tolerance = 1e-9)
  expect_equal(methadone(wCohort, 177.3, 29, "male")$PK$default$cl1,
               methadoneRacemicCL, tolerance = 1e-9)
  e <- methadoneEnantiomers(size$pkWeight, volume = size$volume,
                            clearance = size$clearance)
  expect_equal(1 / sum(e$R$coef), 41.0, tolerance = 1e-9)        # Vc, R(-)
  expect_equal(1 / sum(e$S$coef), 41.0 * 0.60, tolerance = 1e-9) # Vc, S(+)
  # The cohort's own man and woman sit either side of the pooled patient:
  # each one's factor is his or her FFM ratio over the pooled divisor
  for (sex in c("male", "female")) {
    h <- if (sex == "male") 177.3 else 163.4
    s <- pkSizeFactors(74, h, 29, sex, TRUE,
                       cohort = do.call(cohortSizeFactors, METHADONE_COHORT))
    expect_equal(s$volume, ffmAlSallami(74, h, 29, sex) / FFM_REFERENCE /
                   methadoneCohort$volume, tolerance = 1e-9, info = sex)
  }

  # (b) A man of twice the reference fat-free mass: Vc, V2, Q2, Q3 and the
  # clearances scale by the ratio (volumes) and its 0.75 power (clearances),
  # over the cohort's factors; V3 follows Henthorn's own weight term at his
  # pharmacokinetic weight (140 kg) and nothing else.
  w2 <- uniroot(function(w) ffmAlSallami(w, 190, 35, "male") - 2 * FFM_REFERENCE,
                c(80, 300), tol = 1e-12)$root
  big <- pkSizeFactors(w2, 190, 35, "male", TRUE,
                       cohort = do.call(cohortSizeFactors, METHADONE_COHORT))
  expect_equal(big$volume, 2 / methadoneCohort$volume, tolerance = 1e-9)
  expect_equal(big$clearance, 2^0.75 / methadoneCohort$clearance, tolerance = 1e-9)
  expect_equal(big$pkWeight, 140, tolerance = 1e-9)
  expect_equal(methadone(w2, 190, 35, "male")$PK$default$cl1,
               methadoneRacemicCL * 2^0.75 / methadoneCohort$clearance,
               tolerance = 1e-9)
  e2 <- methadoneEnantiomers(140, volume = big$volume, clearance = big$clearance)
  expect_equal(1 / sum(e2$R$coef), 41.0 * 2 / methadoneCohort$volume, tolerance = 1e-9)
  # The sum of the exponents is k10 + k12 + k13 + k21 + k31: written out from
  # Table 2 with Vc and V2 scaled, the clearances scaled, and V3 at Henthorn's
  # own term alone (208.1 L x (140/70)^1.23)
  sv <- big$volume; sc <- big$clearance
  v3 <- 208.1 * 2^1.23
  expect_equal(sum(e2$R$lambda),
               (0.103 + 13.3 + 0.50) * sc / (41.0 * sv) + 13.3 * sc / (155.3 * sv) +
                 0.50 * sc / v3, tolerance = 1e-9)

  # (c) The switch off ignores the cohort: Table 2 at any size, V3 at total
  # weight
  off <- methadone(w2, 190, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, methadoneRacemicCL, tolerance = 1e-9)
  expect_identical(methadoneRacemicPK(w2, 1, volume = 1, clearance = 1),
                   methadoneRacemicPK(w2))
})

test_that("the reduction tracks the two-enantiomer model for a week", {
  for (w in c(50, 70, 120)) {
    e <- methadoneEnantiomers(w)
    pk <- methadoneRacemicPK(w)
    u <- methadoneUdf(pk$v1, pk$v2, pk$v3, pk$cl1, pk$cl2, pk$cl3)
    t <- c(1, 5, 15, 60, 240, 720, 1440, 2880, 4320, 7 * 1440)
    udf <- function(x) colSums(x$coef * exp(-outer(x$lambda, t)))
    enantiomers <- 0.5 * (udf(e$R) + udf(e$S))
    expect_lt(max(abs(udf(u) / enantiomers - 1)), 0.1)
    if (w == 70) expect_lt(max(abs(udf(u) / enantiomers - 1)), 0.03)
  }
})

test_that("the enantiomer models reproduce Henthorn's clearance shares", {
  # R(-): 25% renal, 23% EDDP, 51% other; S(+): 16%, 34%, 50% (Results).
  r <- METHADONE_R
  s <- METHADONE_S_OVER_R
  clR <- r$clEddp + r$clRenal + r$clOther
  clS <- r$clEddp * s$clEddp + r$clRenal * s$clRenal + METHADONE_S_CL_OTHER
  expect_equal(round(100 * c(r$clRenal, r$clEddp, r$clOther) / clR), c(25, 23, 51))
  expect_equal(round(100 * c(r$clRenal * s$clRenal, r$clEddp * s$clEddp,
                             METHADONE_S_CL_OTHER) / clS), c(16, 34, 49))
  # Terminal half-life ratio S/R, 0.69 in the Discussion.
  e <- methadoneEnantiomers(70)
  expect_equal(e$R$lambda[3] / e$S$lambda[3], 0.69, tolerance = 0.01)
})

test_that("V3 on pharmacokinetic or total weight; the rest on FFM with the switch on", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 91.34 kg.  Henthorn's
  # V3 term at 120 kg exceeds that at 91.34 kg.  With the switch on the other
  # volumes and clearances take his FFM factors over the cohort's (since
  # 2026-11-19; before, v1 34.5982634, v2 130.2796169, v3 245.7331206, cl1
  # 0.1080264692, cl2 12.9400613, cl3 0.4841747711); with it off, nothing.
  on  <- methadone(120, 170, 50, "male")$PK$default
  off <- methadone(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(
    on[1:6],
    list(v1 = 47.61599952, v2 = 179.4435044, v3 = 246.8841445,
         cl1 = 0.1372996876, cl2 = 16.45149843, cl3 = 0.612249411),
    tolerance = 1e-5
  )
  expect_equal(
    off[1:6],
    list(v1 = 34.6022111, v2 = 130.2938735, v3 = 343.0648022,
         cl1 = 0.1080264692, cl2 = 12.9366231, cl3 = 0.4852550264),
    tolerance = 1e-5
  )
  ref <- methadone(70, 170, 50, "male")$PK$default
  expect_gt(off$v3, on$v3)
  expect_gt(on$v3, ref$v3)
  expect_equal(on$cl1 / ref$cl1,
               pkSizeFactors(120, 170, 50, "male", TRUE)$clearance /
                 pkSizeFactors(70, 170, 50, "male", TRUE)$clearance, tolerance = 1e-9)
})

test_that("an oral dose peaks at about 3 hours with 70% of the exposure", {
  ev <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  run <- function(units) {
    dose <- data.frame(Drug = "methadone", Time = 0, Dose = 10, Units = units)
    x <- simulateDrugsWithCovariates(dose, ev, 70, 170, 50, "male", 1440, FALSE)
    r <- x$methadone$results
    r[r$Site == "Plasma", ]
  }
  po <- run("mg PO")
  iv <- run("mg")
  tPeak <- po$Time[which.max(po$Y)]
  expect_gt(tPeak, 150)
  expect_lt(tPeak, 210)
  # By 24 h most of the oral dose is absorbed and distribution is complete,
  # so the oral curve sits close to 0.70 of the intravenous one.
  ratio <- approx(po$Time, po$Y, 1440)$y / approx(iv$Time, iv$Y, 1440)$y
  expect_gt(ratio, 0.65)
  expect_lt(ratio, 0.75)
})

test_that("oral liquid is the tablet's absorption, entered as prescribed", {
  PK <- getDrugPK("methadone", 20, 115, 6, "female")
  expect_null(PK$oralFormulations)
  events <- data.frame(Time = numeric(0), Event = character(0))
  run <- function(dose, units)
    simCpCe(data.frame(Drug = "methadone", Time = 0, Dose = dose, Units = units),
            events, PK, 24 * 60, plotRecovery = FALSE)
  tablet <- run(2, "mg PO")
  expect_equal(run(2, "mg PO liquid")$results, tablet$results)
  liquidKg <- run(0.1, "mg/kg PO liquid")
  expect_equal(liquidKg$results, tablet$results)
  units <- getDrugDefaults("methadone")$Units[[1]]
  expect_true(all(c("mg PO liquid", "mg/kg PO liquid", "mg PO liquid qid",
                    "mg/kg PO liquid qid") %in% units))
})

# --- CYP2B6 ------------------------------------------------------------------
# Kharasch ED, Regina KJ, Blood J, Friedel C, Anesthesiology 2015;123:1142-1153
# measured hepatic clearance in cohorts of CYP2B6 *1/*1, *1/*6 and *6/*6
# volunteers; the multipliers below are read from that paper, not from the code
# under test.  See the CYP2B6 section of R/drugs_methadone.R.

test_that("the CYP2B6 multiplier scales non-renal clearance only, per enantiomer", {
  # Henthorn Table 2, written out by hand (as the clearance test above does)
  renalR <- 0.026
  renalS <- 0.026 * 0.57
  hepaticR <- 0.024 + 0.053
  hepaticS <- 0.024 * 1.30 + 0.045
  # Kharasch 2015 Results: hepatic clearance 1.3 -> 1.0 (R) and 1.3 -> 0.8 (S)
  poor <- c(R = 1.0 / 1.3, S = 0.8 / 1.3)
  # Heterozygotes: the fraction of the homozygote effect his apparent oral
  # clearances show (R 25% against 35% lower, S 35% against 45%)
  inter <- c(R = 1 - (25 / 35) * (1 - poor[["R"]]),
             S = 1 - (35 / 45) * (1 - poor[["S"]]))

  clOf <- function(udf) sum(udf$coef / udf$lambda)^-1   # 1 / AUC of the unit curve
  expected <- function(g) c(R = g[["R"]] * hepaticR + renalR,
                            S = g[["S"]] * hepaticS + renalS)
  for (ph in CYP2B6_VALUES) {
    g <- switch(ph, poor = poor, intermediate = inter, c(R = 1, S = 1))
    e <- methadoneEnantiomers(70, cyp2b6 = ph)
    expect_equal(c(R = clOf(e$R), S = clOf(e$S)), expected(g), tolerance = 1e-8,
                 info = ph)
  }
  # The renal share is untouched, so a poor metaboliser's clearance falls by
  # less than the hepatic multiplier alone would give
  e <- methadoneEnantiomers(70, cyp2b6 = "poor")
  n <- methadoneEnantiomers(70, cyp2b6 = "normal")
  expect_gt(clOf(e$S) / clOf(n$S), poor[["S"]])
  expect_equal(clOf(e$S) / clOf(n$S), 0.678, tolerance = 0.002)
  expect_equal(clOf(e$R) / clOf(n$R), 0.827, tolerance = 0.002)
})

test_that("the reduced racemic model carries the phenotype, and normal is the default", {
  ref <- methadone(70, 170, 50, "male")
  expect_equal(ref, methadone(70, 170, 50, "male", cyp2b6 = "normal"))
  # Rapid and ultrarapid take the normal metaboliser's parameters (no
  # transportable estimate exists for the increased-function allele)
  for (ph in c("rapid", "ultrarapid")) {
    expect_equal(methadone(70, 170, 50, "male", cyp2b6 = ph), ref, info = ph)
  }
  cl <- function(ph) methadone(70, 170, 50, "male", cyp2b6 = ph)$PK$default$cl1
  expect_lt(cl("poor"), cl("intermediate"))
  expect_lt(cl("intermediate"), cl("normal"))
  # Racemic clearance relative to the normal metaboliser, from the enantiomer
  # clearances above: half the dose to each, so the harmonic mean
  racemic <- function(clR, clS) 1 / (0.5 / clR + 0.5 / clS)
  base <- racemic(0.103, 0.09102)
  expect_equal(cl("poor") / cl("normal"),
               racemic(0.827 * 0.103, 0.678 * 0.09102) / base, tolerance = 0.002)
  # Volumes do not move with phenotype; clearance does.  (The reduction is a
  # fit, so V1 shifts a little; it stays within a per cent.)
  v1 <- function(ph) methadone(70, 170, 50, "male", cyp2b6 = ph)$PK$default$v1
  expect_equal(v1("poor"), v1("normal"), tolerance = 0.01)
  expect_match(methadone(70, 170, 50, "male")$reference, "Anesthesiology 2015")
})

test_that("an invalid cyp2b6 fails loudly", {
  expect_error(methadone(70, 170, 50, "male", cyp2b6 = "*1/*6"), "Invalid cyp2b6")
  expect_error(methadone(70, 170, 50, "male", cyp2b6 = c("poor", "normal")), "Invalid cyp2b6")
  expect_error(methadone(70, 170, 50, "male", cyp2b6 = NA), "Invalid cyp2b6")
  expect_error(methadoneEnantiomers(70, cyp2b6 = "fast"), "Invalid cyp2b6")
})

test_that("a poor metaboliser accumulates higher on repeated oral dosing", {
  ev <- data.frame(Time = numeric(0), Event = character(0))
  trough <- function(ph, day) {
    dose <- data.frame(Drug = "methadone", Time = 0, Dose = 10, Units = "mg PO bid")
    x <- simulateDrugsWithCovariates(dose, ev, 70, 170, 40, "male", 14 * MINS_PER_DAY,
                                     FALSE, cyp2b6 = ph)
    r <- x$methadone$results
    r <- r[r$Site == "Plasma", ]
    approx(r$Time, r$Y, day * MINS_PER_DAY)$y
  }
  # The genotype effect grows with accumulation: small after a day, large at
  # steady state.  This is the point of the methadone-cyp2b6 scenario.
  day1 <- trough("poor", 1) / trough("normal", 1)
  day14 <- trough("poor", 14) / trough("normal", 14)
  expect_gt(day1, 1.05)
  expect_lt(day1, 1.2)
  expect_gt(day14, 1.3)
  expect_lt(day14, 1.45)
  expect_gt(day14, day1)
})

test_that("the Pregnant field applies the review's factor (R/pregnancy.R)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  Both enantiomers' CL x 24/14, then the racemic fit.
  # The values are those the model gives with the flag on; test-pregnancy.R
  # checks the ratio against the flag off and that nothing else moves.  Since
  # 2026-11-19 she is scaled to FFM and anchored at Henthorn's cohort; cl1 is
  # the old pin 0.1851882329 times her anchored clearance factor (checked
  # below), and the reduction is refitted (old pins: v1 34.698757, v2
  # 132.1350653, v3 144.2931424, cl2 12.92238908, cl3 0.4444147292).
  actual <- methadone(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(29.50680535, actual$v1)
  expect_equal_rounded(112.0460219, actual$v2)
  expect_equal_rounded(143.230113, actual$v3)
  expect_equal_rounded(0.1640052931, actual$cl1)
  expect_equal_rounded(11.44143663, actual$cl2)
  expect_equal_rounded(0.4035549047, actual$cl3)
  expect_equal(actual$cl1, 0.1851882329 *
                 pkSizeFactors(70, 165, 30, "female", TRUE)$clearance /
                 methadoneCohort$clearance, tolerance = 1e-8)
  expect_equal(methadone(70, 165, 30, "female")$PK$default,
               methadone(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(methadone(70, 165, 30, "female", pregnant = TRUE)$reference, "Ansari", fixed = TRUE)
  expect_no_match(methadone(70, 165, 30, "female")$reference, "Ansari")
})
