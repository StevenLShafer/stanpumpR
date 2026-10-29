# Hydrocodone: oral only, with hydromorphone as its active metabolite.
#
# The two things worth guarding here are the ones that are easy to get wrong
# and expensive to notice: that the apparent disposition is never offered
# intravenously, and that formation reproduces the observed hydromorphone to
# hydrocodone exposure ratio rather than some formation clearance nobody
# measured.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

oralHydrocodone <- function(mg = 20, cyp2d6 = CYP2D6_DEFAULT, maximum = 10080) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "hydrocodone", Time = 0, Dose = mg, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", maximum, FALSE, cyp2d6 = cyp2d6
  )
}

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)


# Melhem's covariates worked by hand for the 70 kg, 171 cm, 50-year-old man at
# the assumed creatinine of 0.9 mg/dL, total body weight (switch off):
# Gehan-George BSA 0.0235 x 70^0.51456 x 171^0.42246, Cockcroft-Gault
# (140 - 50) x 70 / (72 x 0.9) = 97.222 mL/min, indexed x 1.73 / BSA.
handBsa  <- 0.0235 * 70^0.51456 * 171^0.42246
handClcr <- (140 - 50) * 70 / (72 * 0.9) * 1.73 / handBsa
handV1   <- 714 * (handBsa / 1.87)^0.486
handCl1  <- 64.4 / 60 * (handClcr / 91.48)^0.223

test_that("returns the correct calculations", {
  actual <- hydrocodone(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = handV1, v2 = 151, v3 = 1,
      cl1 = handCl1, cl2 = 0.910 / 60, cl3 = 0,
      ka_PO = 0.0448919972,
      bioavailability_PO = 1,
      tlag_PO = 0
    )),
    tPeak = 90,        # provisional, measured ORALLY; see the drug file
    tPeakRoute = ROUTE_PO,
    MEAC = 12,         # provisional, set equal to oxycodone's; see the drug file
    typical = 20,
    upperTypical = 30,
    lowerTypical = 10,
    reference = actual$reference,
    metabolite = list(
      name              = "hydromorphone",
      kFormation        = 3.18574e-07 * 70 * 714 / handV1,
      firstPassFraction = 0,
      mwRatio           = 285.34 / 299.36
    )
  )

  expect_equal_rounded(actual, expected)
})


test_that("the apparent disposition is the published one, with Melhem's covariates", {
  x <- hydrocodone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Melhem 2013 / FDA review: Q/F 0.910 L/h and Vp/F 151 L carry no covariate
  expect_equal(x$cl2 * 60, 0.910)      # 0.910, not 91.0
  expect_equal(x$v2, 151)

  # At the paper's reference values the published typical values come back.
  # Find the weight whose Gehan-George BSA is 1.87 m2 at 171 cm, and the
  # creatinine that puts its indexed Cockcroft-Gault clearance at 91.48.
  w <- (1.87 / (0.0235 * 171^0.42246))^(1 / 0.51456)
  scr <- (140 - 50) * w / (72 * 91.48 * 1.87 / 1.73)
  ref <- hydrocodone(w, 171, 50, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(ref$v1, 714)
  expect_equal(ref$cl1 * 60, 64.4)

  # Eq 11: CL/F goes with the 0.223 power of creatinine clearance, which is
  # inverse in the creatinine; Eq 12: Vc/F with the 0.486 power of BSA.
  impaired <- hydrocodone(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = 3.6)$PK$default
  expect_equal(impaired$cl1 / x$cl1, (0.9 / 3.6)^0.223)
  expect_equal(impaired$v1, x$v1)
  big <- hydrocodone(120, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(big$v1 / x$v1,
               (bsaGehanGeorge(120, 171) / bsaGehanGeorge(70, 171))^0.486)

  # Severe against normal renal function (CLCR 15 against 100 mL/min/1.73 m2):
  # exposure 53% higher, as Melhem reports.
  expect_equal((100 / 15)^0.223, 1.53, tolerance = 0.01)
})


test_that("the switch moves the covariates to the pharmacokinetic weight", {
  on  <- hydrocodone(120, 170, 50, "male")$PK$default
  pkW <- pkSizeFactors(120, 170, 50, "male")$pkWeight
  off <- hydrocodone(pkW, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(on$v1, off$v1)
  expect_equal(on$cl1, off$cl1)
  # The central volume grows less with weight with the switch on than off:
  # 7% and 14% from 70 to 120 kg at 170 cm, as the help page says.
  v1 <- function(w, ffm) hydrocodone(w, 170, 50, "male", adjustToFFM = ffm)$PK$default$v1
  expect_equal(v1(120, TRUE) / v1(70, TRUE), 1.07, tolerance = 0.005)
  expect_equal(v1(120, FALSE) / v1(70, FALSE), 1.14, tolerance = 0.005)
})


test_that("hydrocodone reads the serum creatinine", {
  pk <- function(cr) getDrugPK("hydrocodone", 70, 170, 50, "male",
                               getDrugDefaults("hydrocodone"), creatinine = cr)$PK$default
  expect_lt(pk(3)$cl1, pk(0.9)$cl1)
  expect_equal(pk(NULL)$cl1, pk(0.9)$cl1)
})


test_that("the model is nearly mono-exponential, matching the observed half-life", {
  x <- hydrocodone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  r <- sort(r[r > 0], decreasing = TRUE) * 60      # per hour
  # The two-compartment eigenvalues worked by hand from the covariate values
  # above (handV1, handCl1): the roots of l^2 - (k10 + k12 + k21) l + k10 k21.
  k10 <- handCl1 / handV1; k12 <- 0.910 / 60 / handV1; k21 <- 0.910 / 60 / 151
  s <- k10 + k12 + k21
  hand <- c(s + sqrt(s^2 - 4 * k10 * k21), s - sqrt(s^2 - 4 * k10 * k21)) / 2 * 60
  expect_equal(unname(r), hand, tolerance = 1e-6)
  expect_equal(unname(r), c(0.0924142, 0.00593678), tolerance = 1e-5)

  # The fast half-time, 7.5 h, is close to the 8.4 h Kapil 2015 observed; the
  # slow one is a 117 h model tail carrying under 2% of the area, and is not a
  # clinical elimination half-life.
  expect_equal(unname(log(2) / r[1]), 7.50, tolerance = 0.01)
  expect_gt(log(2) / r[2], 100)
})


test_that("hydrocodone is offered orally only, because the parameters are apparent", {
  # Apparent parameters predict oral concentrations correctly and intravenous
  # ones wrong by 1/F, so no intravenous unit may be offered.
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- dd$Units[dd$Drug == "hydrocodone"]
  units <- strsplit(units, ",")[[1]]
  expect_equal(units, c("mg PO", paste("mg PO", names(SCHEDULE_INTERVALS))))
  expect_true(all(doseRoute(units) == "PO"))
  expect_false(any(grepl("min|hr", units)))
  # Bioavailability is carried as 1: the apparent scale already contains it
  expect_equal(hydrocodone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default$bioavailability_PO, 1)
})


test_that("the oral plasma peak falls at 1.3 h and the effect site at 90 min", {
  # Checked analytically rather than off the plotted grid, whose geometric
  # fill is about 15 min wide here and cannot resolve 78 from 90.
  p <- hydrocodone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  PK <- getDrugPK("hydrocodone", 70, 171, 50, "male", getDrugDefaults("hydrocodone"))
  pk <- PK$PK$default

  lam <- c(pk$lambda_1, pk$lambda_2)
  a   <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2) * pk$ka_PO / (pk$ka_PO - lam)
  coef   <- c(a, -sum(a))
  lambda <- c(lam, pk$ka_PO)

  plasmaPeak <- stats::optimize(
    function(t) sum(coef * exp(-lambda * t)), c(1, 500), maximum = TRUE)$maximum
  expect_equal(plasmaPeak, 78, tolerance = 0.02)

  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 90, tolerance = 0.02)

  # And the effect site really does lag: solving this oral tPeak against a
  # bolus curve, as the engine does for every intravenous drug, would have put
  # the peak elsewhere.
  expect_gt(effectSitePeakTime(coef, lambda, pk$ke0), plasmaPeak)

  o <- oralHydrocodone(20, maximum = 1440)
  w <- o$hydrocodone$wide
  expect_gt(max(w$Plasma), 18)
  expect_lt(max(w$Plasma), 35)
})


test_that("formed hydromorphone reproduces the observed exposure ratio", {
  # Kapil 2015, placebo arm: hydromorphone AUC 3.8, hydrocodone AUC 325.3.
  # An exposure ratio does not depend on the input shape, so a ratio measured
  # on an extended-release product transfers to this immediate-release one.
  o <- oralHydrocodone(20, maximum = 43200)
  hc <- o$hydrocodone$wide
  hm <- o$hydromorphone$wide
  ratio <- trapz(hm$Time, hm$Plasma) / trapz(hc$Time, hc$Plasma)
  # Divided by the target, so that the tolerance is relative.  It was
  # expect_equal(ratio, 0.0117, tolerance = 0.15), and expect_equal() treats a
  # tolerance larger than the expected value as ABSOLUTE, so any ratio below
  # 0.16 passed.  Nor did the area mean much: until 2026-10-07 the series drew
  # one straight chord from 20 hours to 30 days, which made both AUCs six times
  # too large (hydrocodone 112,504 against 18,629 ng.min/mL) and this ratio
  # 8.7% high.  kFormation was not tuned against that: the closed-form ratio is
  # 0.0116815, Kapil's to five figures.  On the plot-scaled time line
  # (R/simulationTimeGrid.R) trapz() is within 0.1% of it.  (Claude Code.)
  expect_equal(ratio / (3.8 / 325.3), 1, tolerance = 0.02)

  expect_equal(o$hydromorphone$formedFrom, "hydrocodone")
  # Kapil's hydromorphone peak was 0.19 ng/mL
  expect_gt(max(hm$Plasma), 0.08)
  expect_lt(max(hm$Plasma), 0.35)
})


test_that("the exposure ratio does not drift with body weight", {
  # hydrocodone's apparent volumes are fixed while hydromorphone's clearance
  # scales with weight, so kFormation has to scale with weight to hold the
  # calibrated ratio.  Without that the ratio would be proportional to weight.
  ratioAt <- function(wt) {
    o <- simulateDrugsWithCovariates(
      data.frame(Drug = "hydrocodone", Time = 0, Dose = 20, Units = "mg PO"),
      noEvents, wt, 171, 50, "male", 20160, FALSE)
    trapz(o$hydromorphone$wide$Time, o$hydromorphone$wide$Plasma) /
      trapz(o$hydrocodone$wide$Time, o$hydrocodone$wide$Plasma)
  }
  # As a ratio, so that the tolerance is relative rather than absolute (see
  # above).  The closed-form ratio is the same at every weight; the 2.2% that
  # used to separate these two was the straight chord the old time line drew
  # across the second week, whose error depends on the curve's shape.
  expect_equal(ratioAt(50) / ratioAt(100), 1, tolerance = 0.02)
})


test_that("the CYP2D6 floor is hydrocodone's own, not codeine's", {
  w <- HYDROCODONE_CYP2D6_WEIGHT
  # Otton 1993: partial clearance to hydromorphone 3.4 vs 28.1 mL/h/kg
  expect_equal(unname(w[["poor"]]), 3.4 / 28.1, tolerance = 1e-4)
  expect_equal(unname(w[["normal"]]), 1)
  expect_true(all(diff(unname(w[c("poor", "intermediate", "normal", "ultrarapid")])) > 0))

  # Hydrocodone's poor metaboliser retains far more formation than codeine's,
  # which is exactly why codeine's multipliers must not be imported.
  expect_gt(w[["poor"]], 3 * CODEINE_CYP2D6_WEIGHT[["poor"]])
})


test_that("formed hydromorphone is ordered across phenotypes", {
  peaks <- vapply(CYP2D6_VALUES,
                  function(g) max(oralHydrocodone(20, g, maximum = 2880)$hydromorphone$wide$Plasma),
                  numeric(1))
  expect_true(all(diff(peaks) > 0))
  expect_equal(unname(peaks[["poor"]] / peaks[["normal"]]), 3.4 / 28.1, tolerance = 0.02)
})


test_that("an unknown phenotype is refused", {
  expect_error(hydrocodone(70, 171, 50, "male", "rapid"), "Invalid cyp2d6")
})


test_that("the provisional potency is consistent with the drug table", {
  # tPeak and MEAC are provisional values set by hand, not fitted; see the
  # constants at the top of R/drugs_hydrocodone.R.  This test exists so that
  # changing HYDROCODONE_MEAC without also changing the CSV fails loudly: the
  # plot and the opioid total read the CSV, not the drug function.
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_equal(dd$MEAC[dd$Drug == "hydrocodone"], HYDROCODONE_MEAC)
  # The emergence threshold has to move with MEAC.  Left at zero it makes the
  # time until threshold pin at the simulation length, which is how the
  # omission shows up.
  expect_equal(dd$endCe[dd$Drug == "hydrocodone"], HYDROCODONE_MEAC)

  # MEAC is set equal to oxycodone's (both rows in ng/mL); see the drug file.
  expect_equal(HYDROCODONE_MEAC, dd$MEAC[dd$Drug == "oxycodone"])
})


test_that("the effect site is live now that tPeak is set", {
  PK <- getDrugPK("hydrocodone", 70, 171, 50, "male", getDrugDefaults("hydrocodone"))
  expect_equal(PK$tPeak, 90)
  expect_equal(PK$tPeakRoute, ROUTE_PO)
  expect_gt(PK$PK$default$ke0, 0)

  o <- oralHydrocodone(20, maximum = 1440)
  w <- o$hydrocodone$wide
  expect_false(any(is.na(w$"Effect Site")))
  expect_gt(max(w$"Effect Site"), 0)
  # The effect site lags, but the plotted grid is about 15 min wide where the
  # two peaks sit 12 min apart, so the lag is checked where it is visible.
  expect_gte(w$Time[which.max(w$"Effect Site")], w$Time[which.max(w$Plasma)])
  early <- w[w$Time > 0 & w$Time < 40, ]
  expect_true(all(early$"Effect Site" < early$Plasma))
  # and it now contributes to the opioid total
  expect_gt(max(o$hydrocodone$equiSpace$MEAC), 0)
})

test_that("with fat-free mass scaling the covariates see the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y male.  The fat-free-mass ratio is 1.3049067 and its
  # 0.75 power 1.2209126 (worked out from the Al-Sallami formula by hand), so
  # the pharmacokinetic weight is 70 x 1.3049067 = 91.343469 kg.  Melhem's
  # covariates at that weight, by hand: Gehan-George BSA, Cockcroft-Gault at
  # the assumed 0.9 mg/dL indexed to 1.73 m2.  kFormation follows
  # hydromorphone's clearance factor and is divided by Vc/F's covariate
  # factor, so the Kapil AUC ratio is preserved.
  pkW  <- 70 * 1.3049067
  bsa  <- 0.0235 * pkW^0.51456 * 170^0.42246
  clcr <- (140 - 50) * pkW / (72 * 0.9) * 1.73 / bsa
  v1   <- 714 * (bsa / 1.87)^0.486
  x <- hydrocodone(120, 170, 50, "male")
  expect_equal_rounded(x$PK$default$v1, v1)
  expect_equal_rounded(x$PK$default$cl1 * 60, 64.4 * (clcr / 91.48)^0.223)
  expect_equal_rounded(x$metabolite$kFormation,
                       3.18574e-07 * 70 * 1.2209126 * 714 / v1)
  # Vp/F and Q/F carry no covariate and take the library factors
  expect_equal_rounded(x$PK$default$v2, 151 * 1.3049067)
  expect_equal_rounded(x$PK$default$cl2 * 60, 0.910 * 1.2209126)
  # and are the published values with the switch off, at any size
  off <- hydrocodone(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v2, 151)
  expect_equal(off$cl2 * 60, 0.910)
})
