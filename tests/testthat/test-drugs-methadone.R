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
  expect_equal(actual$upperTypical, 0.048)
  expect_equal(actual$lowerTypical, 0.12)
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
