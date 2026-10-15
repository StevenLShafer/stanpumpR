# Methadone: Henthorn and Kharasch 2025, R(-) + S(+) summed to racemate and
# reduced to one three-compartment model per patient (see the file header of
# R/drugs_methadone.R).  The reduced parameters come out of an optimiser, so
# they are pinned to 1e-5 rather than the usual 1.5e-6, which leaves room for
# floating-point differences between platforms without hiding a real change.

test_that("reference man: reduced parameters, oral route and pharmacodynamics", {
  actual <- methadone(70, 170, 50, "male")

  expect_equal(
    actual$PK$default,
    list(
      v1 = 34.6029236,
      v2 = 130.4042348,
      v3 = 178.5251339,
      cl1 = 0.1080264692,
      cl2 = 12.9415885,
      cl3 = 0.4803807328,
      ka_PO = 0.0100236846,
      bioavailability_PO = 0.70,
      tlag_PO = 0
    ),
    tolerance = 1e-5
  )
  expect_equal(actual$tPeak, 11.3)
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
  # hydrochloride-to-base factor divides it.
  clR <- 0.103
  clS <- 0.09102
  racemic <- 1 / (0.5 / clR + 0.5 / clS)
  expected <- racemic / (309.45 / 345.91)
  for (w in c(50, 70, 120)) {
    expect_equal(methadone(w, 170, 50, "male")$PK$default$cl1, expected,
                 tolerance = 1e-9)
  }
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

test_that("weight enters V3 only, on pharmacokinetic weight or total weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 91.34 kg.  Henthorn's
  # V3 term at 120 kg exceeds that at 91.34 kg; V1 and clearance barely move.
  on  <- methadone(120, 170, 50, "male")$PK$default
  off <- methadone(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(
    on[1:6],
    list(v1 = 34.5982634, v2 = 130.2796169, v3 = 245.7331206,
         cl1 = 0.1080264692, cl2 = 12.9400613, cl3 = 0.4841747711),
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
  expect_equal(on$cl1, ref$cl1)
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
  # checks the ratio against the flag off and that nothing else moves.
  actual <- methadone(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(34.698757, actual$v1)
  expect_equal_rounded(132.1350653, actual$v2)
  expect_equal_rounded(144.2931424, actual$v3)
  expect_equal_rounded(0.1851882329, actual$cl1)
  expect_equal_rounded(12.92238908, actual$cl2)
  expect_equal_rounded(0.4444147292, actual$cl3)
  expect_equal(methadone(70, 165, 30, "female")$PK$default,
               methadone(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(methadone(70, 165, 30, "female", pregnant = TRUE)$reference, "Ansari", fixed = TRUE)
  expect_no_match(methadone(70, 165, 30, "female")$reference, "Ansari")
})
