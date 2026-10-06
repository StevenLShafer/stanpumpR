# Codeine: a prodrug whose analgesia is its metabolite's.
#
# Beyond pinning the returned values, these tests pin the two things in
# R/drugs_codeine.R that are easy to get wrong and expensive to notice:
# the rescaling of Ashraf 2024's formation fractions onto the Lotsch morphine
# clearance, and the observable predictions that rescaling produces.

refPatient <- list(weight = 70, height = 171, age = 50, sex = "male")

codeinePK <- function(cyp2d6 = CYP2D6_DEFAULT, weight = 70) {
  getDrugPK("codeine", weight, 171, 50, "male", getDrugDefaults("codeine"),
            cyp2d6 = cyp2d6)
}

oralCodeine <- function(mg = 60, cyp2d6 = CYP2D6_DEFAULT, maximum = 1440) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "codeine", Time = 0, Dose = mg, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", maximum, FALSE, cyp2d6 = cyp2d6
  )
}

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)


test_that("returns the correct calculations", {
  actual <- codeine(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 176.904, v2 = 1, v3 = 1,
      cl1 = 0.756, cl2 = 0, cl3 = 0,
      ka_PO = 0.0425953551,
      bioavailability_PO = 0.5,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 100,
    upperTypical = 150,
    lowerTypical = 50,
    reference = actual$reference,
    metabolite = list(
      name = "morphine",
      kFormation = 1.2415524e-04,
      firstPassFraction = 0.0015,
      mwRatio = 285.34 / 299.36
    )
  )

  expect_equal_rounded(actual, expected)
})


test_that("disposition follows the intravenous anchors", {
  x <- codeine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Persson 1992: 10.8 mL/min/kg
  expect_equal(x$cl1 / 70, 10.8 / 1000, tolerance = 1e-9)
  # Guay 1988: mean residence time 3.90 h, and Vss = CL * MRT for one compartment
  expect_equal(x$v1 / (x$cl1 * 60), 3.90, tolerance = 1e-9)
  # Commonly cited volume of distribution, about 2.6 L/kg
  expect_gt(x$v1 / 70, 2.4)
  expect_lt(x$v1 / 70, 2.7)
  # Scales linearly with weight
  y <- codeine(35, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$v1, x$v1 / 2)
  expect_equal(y$cl1, x$cl1 / 2)
})


test_that("codeine is a pure prodrug with no effect site of its own", {
  PK <- codeinePK()
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$MEAC, 0)

  # The plotted effect-site column is NA, which simulationPlot() drops, so
  # codeine is drawn as plasma only rather than as a flat line on the axis.
  o <- oralCodeine()
  expect_true(all(is.na(o$codeine$wide$"Effect Site")))
  # But nothing derived is allowed to be NA
  expect_false(any(is.na(o$codeine$equiSpace$Ce)))
  expect_equal(o$codeine$max$Ce, 0)
  expect_false(is.na(o$codeine$max$Cp))
})


test_that("the CYP2D6 phenotype weights are Ashraf's group medians as odds ratios", {
  # Ashraf 2024 section 3.3 median apparent formation fractions, per cent
  src <- c(poor = 0.55, intermediate = 6.82, normal = 13.8, ultrarapid = 19.9) / 100
  odds <- src / (1 - src)
  expect_equal(unname(CODEINE_CYP2D6_WEIGHT[names(src)]),
               unname(odds / odds["normal"]), tolerance = 1e-9)
  expect_equal(unname(CODEINE_CYP2D6_WEIGHT[["normal"]]), 1)
})


test_that("formation is ordered across the four phenotypes", {
  kf <- vapply(c(CYP2D6_POOR, CYP2D6_INTERMEDIATE, CYP2D6_NORMAL,
                 CYP2D6_ULTRARAPID),
               function(g) codeine(70, 171, 50, "male", g, adjustToFFM = FALSE)$metabolite$kFormation,
               numeric(1))
  expect_true(all(diff(kf) > 0))
  # Poor is small but not structurally zero; Ashraf's own estimate is nonzero
  expect_gt(kf[[1]], 0)
  # Ultrarapid forms about 45 times as much morphine as poor
  expect_equal(unname(kf[[4]] / kf[[1]]), 45.0, tolerance = 0.5)
})


test_that("total codeine clearance moves only slightly with phenotype", {
  # Formation is a branch of total clearance, so changing it changes the
  # total; but the CYP2D6 branch is only about 3% of codeine clearance, which
  # is why Yue 1991 and Chen 1991 found no significant difference between
  # extensive and poor metabolisers.
  cl <- vapply(CYP2D6_VALUES,
               function(g) codeine(70, 171, 50, "male", g, adjustToFFM = FALSE)$PK$default$cl1,
               numeric(1))
  expect_lt(max(cl) / min(cl) - 1, 0.06)
})


test_that("an unknown phenotype is refused", {
  expect_error(codeine(70, 171, 50, "male", "typical"), "Invalid cyp2d6")
  expect_error(codeinePK("rapid"), "Invalid cyp2d6")
})


test_that("the formation fraction is Ashraf rescaled to the Lotsch clearance", {
  # Only the ratio of formation to metabolite clearance is identifiable from a
  # formed metabolite, so Ashraf's fraction has to be rescaled by the ratio of
  # their morphine clearance to ours.  Left unrescaled it overpredicts morphine
  # by 357.5/75.3, which is 4.75 fold.
  expected <- (0.16 / 1.16) * 75.3 / 357.5
  expect_equal(expected, 0.0290523, tolerance = 1e-5)

  x <- codeine(70, 171, 50, "male", adjustToFFM = FALSE)
  p <- x$PK$default
  # kFormation * v1 is the formation clearance; against total clearance it is
  # the fraction of codeine eliminated as morphine.
  expect_equal(x$metabolite$kFormation * p$v1 / p$cl1, expected, tolerance = 1e-6)
})


test_that("oral codeine reproduces the observed codeine peak", {
  o <- oralCodeine(60)
  w <- o$codeine$wide
  # Chen 1991 0.97 h, Shah 1990 1.2 h
  expect_equal(w$Time[which.max(w$Plasma)], 60, tolerance = 2)
  # Shah 1990 saw 88 ng/mL after 60 mg of the phosphate salt, about 45 mg base
  expect_gt(max(w$Plasma), 100)
  expect_lt(max(w$Plasma), 170)
})


test_that("formed morphine appears on the morphine row, peaking 1-2 h", {
  o <- oralCodeine(60)
  expect_true("morphine" %in% names(o))
  expect_equal(o$morphine$formedFrom, "codeine")

  w <- o$morphine$wide
  # Lafolie 1996: every measured compound, morphine included, peaked 1-2 h.
  # The first-pass fraction is set at the top of the range that keeps this
  # true; raising it makes the curve bimodal and moves the peak to 20 min.
  tmax <- w$Time[which.max(w$Plasma)]
  expect_gt(tmax, 60)
  expect_lt(tmax, 120)

  # Shah 1990 saw a morphine peak of 2.7 ng/mL after about 45 mg of base.
  # Plasma is carried in mcg/mL for morphine, so convert.
  expect_gt(max(w$Plasma) * 1000, 1.0)
  expect_lt(max(w$Plasma) * 1000, 2.5)

  # Morphine has its own effect site, which is where codeine's analgesia shows
  expect_false(any(is.na(w$"Effect Site")))
  expect_gt(max(w$"Effect Site"), 0)
})


test_that("the morphine to codeine AUC ratio sits near the observed range", {
  # Shah 1990 reports 0.027 and Yue 1991 reports 0.020, both truncated.  The
  # model runs 30-50% below those, which is the documented residual
  # disagreement between Ashraf's rescaled fraction and the direct
  # observations; this test pins it rather than hiding it.
  o <- oralCodeine(60, maximum = 360)
  cod <- o$codeine$wide
  mor <- o$morphine$wide
  ratio <- trapz(mor$Time, mor$Plasma * 1000) / trapz(cod$Time, cod$Plasma)
  expect_gt(ratio, 0.010)
  expect_lt(ratio, 0.020)
})


test_that("morphine exposure is ordered across phenotypes", {
  peaks <- vapply(CYP2D6_VALUES,
                  function(g) max(oralCodeine(60, g, maximum = 720)$morphine$wide$Plasma),
                  numeric(1))
  expect_true(all(diff(peaks) > 0))
  # A poor metaboliser forms very little
  expect_lt(peaks[["poor"]] / peaks[["normal"]], 0.1)
  # Ashraf's simulated exposure ratio for ultrarapid against normal is 2.18;
  # peak concentration is a different quantity but should be the same order.
  expect_gt(peaks[["ultrarapid"]] / peaks[["normal"]], 1.2)
  expect_lt(peaks[["ultrarapid"]] / peaks[["normal"]], 2.0)
})


test_that("intravenous codeine gives the correct one-compartment peak", {
  # A regression test for getDrugPK()'s one-compartment branch, which returned
  # 1/(lambda_1 * v1) instead of 1/v1 and so inflated every concentration by
  # 1/k10.  Codeine is the first drug to reach that branch.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", 720, FALSE
  )
  v1 <- codeine(70, 171, 50, "male")$PK$default$v1
  # 60 mg into 176.9 L, reported in ng/mL: dose is carried in mcg
  expect_equal(max(o$codeine$wide$Plasma), 60 * 1000 / v1, tolerance = 1e-6)

  # And morphine is still formed by the systemic route alone
  expect_gt(max(o$morphine$wide$Plasma), 0)
})


test_that("a dose given both ways sums on the morphine row", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug  = c("codeine", "morphine"),
               Time  = c(0, 0),
               Dose  = c(60, 5),
               Units = c("mg PO", "mg")),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", 720, FALSE
  )
  expect_equal(o$morphine$formedFrom, "codeine")
  # The directly given morphine dominates, but the formed contribution is
  # added rather than replacing it or creating a second row.
  expect_equal(sum(names(o) == "morphine"), 1)

  # Compared on exposure, not on the peak: the intravenous bolus peaks at
  # time zero, where no codeine has been converted yet, so the two curves
  # share a maximum even though one is strictly larger afterwards.
  given <- trapz(o$morphine$wideOwn$Time, o$morphine$wideOwn$Plasma)
  total <- trapz(o$morphine$wide$Time,    o$morphine$wide$Plasma)
  expect_gt(total, given)

  # And the sum is exact: superposition holds because both contributions
  # distribute through the same linear morphine disposition.
  alone <- simulateDrugsWithCovariates(
    data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", 720, FALSE
  )
  formed <- trapz(alone$morphine$wide$Time, alone$morphine$wide$Plasma)
  expect_equal(total - given, formed, tolerance = 1e-3)
})


# "Time until threshold" on the morphine row.  The morphine a patient given
# codeine has is partly morphine nobody gave them, and recovery used to be
# taken from the receiving drug's own doses -- which for codeine alone meant no
# doses at all.
#
# Codeine is the pure prodrug of the three parent drugs, and the one whose
# metabolite has by far the lowest threshold relative to what is formed, so the
# doses here are deliberately extreme.  The clinically ordinary cases are
# oxycodone's and hydrocodone's; those are in test-recovery-engines.R, along
# with the check against stopping delivery in the simulation itself.
#
# (Claude Code, Claude Opus 5, 2026-10-05; run on R 4.6.1.)

recoveryRun <- function(DT, maximum = 1440)
  simulateDrugsWithCovariates(
    DT, data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", maximum, TRUE)


test_that("the morphine row reports a time for morphine formed from codeine", {
  # 600 mg is far above any clinical dose; it is the dose at which formed
  # morphine clears morphine's own threshold, which is what makes a time until
  # threshold exist to be reported.
  o <- recoveryRun(data.frame(Drug = "codeine", Time = 0, Dose = 600,
                              Units = "mg PO"))
  w <- o$morphine$wide

  expect_null(o$morphine$wideOwn)          # never given directly
  expect_equal(o$morphine$formedFrom, "codeine")
  expect_gt(max(w$"Effect Site"), o$morphine$endCe)
  # The reported time, which was identically zero before this was fixed
  expect_gt(max(w$Recovery), 60)
  expect_gt(max(o$morphine$equiSpace$Recovery), 60)

  # Nothing is given after time zero, so the moment the effect site comes down
  # through the threshold is fixed: every point before it must report the same
  # absolute time.
  rising <- w[w$Recovery > 0, ]
  expect_gt(nrow(rising), 5)
  absolute <- rising$Time + rising$Recovery
  expect_lt(max(absolute) - min(absolute), 0.5)

  # Codeine's own row still reports nothing, having no effect site of its own
  expect_true(all(o$codeine$wide$Recovery == 0))
})


test_that("a clinical dose of codeine stays below morphine's threshold", {
  # 60 mg forms morphine that never reaches the threshold, so no time is the
  # right answer -- the fix must not invent one.
  o <- recoveryRun(data.frame(Drug = "codeine", Time = 0, Dose = 60,
                              Units = "mg PO"))
  expect_lt(max(o$morphine$wide$"Effect Site"), o$morphine$endCe)
  expect_true(all(o$morphine$wide$Recovery == 0))
})


test_that("codeine lengthens the time until threshold for given morphine", {
  both <- recoveryRun(data.frame(Drug  = c("codeine", "morphine"),
                                 Time  = c(0, 0),
                                 Dose  = c(600, 10),
                                 Units = c("mg PO", "mg")))
  given <- recoveryRun(data.frame(Drug = "morphine", Time = 0, Dose = 10,
                                  Units = "mg"))

  # The row's own simulation is unchanged -- the understated answer is still
  # there to compare against -- but the row now reports the sum.
  expect_gt(max(both$morphine$wide$Recovery),
            max(both$morphine$wideOwn$Recovery))
  expect_equal(max(both$morphine$wideOwn$Recovery),
               max(given$morphine$wide$Recovery))
  # Formed morphine adds hours here, not minutes
  expect_gt(max(both$morphine$wide$Recovery) -
              max(given$morphine$wide$Recovery), 120)
})


test_that("the formed contribution's states are the metabolite's effect site", {
  # What the fold adds to the receiving drug is one amplitude per eigenvalue,
  # and they have to sum to the curve that is plotted or the time reported
  # would not belong to the curve shown.
  o <- recoveryRun(data.frame(Drug = "codeine", Time = 0, Dose = 600,
                              Units = "mg PO"))
  states <- o$codeine$metaboliteRecoveryStates
  expect_false(is.null(states))
  # Codeine's three eigenvalues collapse to one (it is one-compartment), plus
  # morphine's three, plus the absorption constant, plus morphine's ke0
  expect_equal(ncol(states$state), 6)
  expect_equal(rowSums(states$state), o$codeine$metaboliteSeries$Ce,
               tolerance = 1e-15)
  # And the fold's answer is what those states imply
  expect_equal(o$morphine$wide$Recovery,
               recoveryFromStates(states, o$morphine$endCe))
})


test_that("asking for no recovery leaves the morphine row's times at zero", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "codeine", Time = 0, Dose = 600, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 171, 50, "male", 1440, FALSE)
  expect_true(all(o$morphine$wide$Recovery == 0))
})


test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand).  The formation constant
  # is a clearance over a volume, so it is no longer weight-independent.
  x <- codeine(120, 170, 50, "male")
  expect_equal_rounded(x$PK$default$v1,  230.84321)
  expect_equal_rounded(x$PK$default$cl1, 0.92300994)
  expect_equal_rounded(x$metabolite$kFormation, 0.00011616364)
})
