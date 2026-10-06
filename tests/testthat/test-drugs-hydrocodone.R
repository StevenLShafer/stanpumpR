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


test_that("returns the correct calculations", {
  actual <- hydrocodone(70, 171, 50, "male")

  expected <- list(
    PK = list(default = list(
      v1 = 714, v2 = 151, v3 = 1,
      cl1 = 64.4 / 60, cl2 = 0.910 / 60, cl3 = 0,
      ka_PO = 0.0637451777,
      bioavailability_PO = 1,
      tlag_PO = 0
    )),
    tPeak = 60,        # provisional; see the drug file
    MEAC = 8,          # provisional, set equal to morphine's; see the drug file
    typical = 20,
    upperTypical = 30,
    lowerTypical = 10,
    reference = actual$reference,
    metabolite = list(
      name              = "hydromorphone",
      kFormation        = 3.18574e-07 * 70,
      firstPassFraction = 0,
      mwRatio           = 285.34 / 299.36
    )
  )

  expect_equal_rounded(actual, expected)
})


test_that("the apparent disposition is the published one", {
  x <- hydrocodone(70, 171, 50, "male")$PK$default
  # Melhem 2013 / FDA review: CL/F 64.4 L/h, Vc/F 714 L, Q/F 0.910 L/h, Vp/F 151 L
  expect_equal(x$cl1 * 60, 64.4)
  expect_equal(x$cl2 * 60, 0.910)      # 0.910, not 91.0
  expect_equal(x$v1, 714)
  expect_equal(x$v2, 151)

  # It carries no weight term, because the source covariate normalisers were
  # not recoverable.  Unusual for this package, and deliberate.
  y <- hydrocodone(35, 171, 50, "male")$PK$default
  expect_equal(y$v1, x$v1)
  expect_equal(y$cl1, x$cl1)
})


test_that("the model is nearly mono-exponential, matching the observed half-life", {
  x <- hydrocodone(70, 171, 50, "male")$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  r <- sort(r[r > 0], decreasing = TRUE) * 60      # per hour
  expect_equal(unname(r), c(0.0915604, 0.00593669), tolerance = 1e-5)

  # The fast half-time is close to the 8.4 h Kapil 2015 observed; the slow one
  # is a 117 h model tail carrying under 2% of the area, and is not a clinical
  # elimination half-life.
  expect_equal(unname(log(2) / r[1]), 7.57, tolerance = 0.01)
  expect_gt(log(2) / r[2], 100)
})


test_that("hydrocodone is offered orally only, because the parameters are apparent", {
  # Apparent parameters predict oral concentrations correctly and intravenous
  # ones wrong by 1/F, so no intravenous unit may be offered.
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- dd$Units[dd$Drug == "hydrocodone"]
  expect_equal(units, "mg PO")
  expect_false(grepl("min|hr", units))
  # Bioavailability is carried as 1: the apparent scale already contains it
  expect_equal(hydrocodone(70, 171, 50, "male")$PK$default$bioavailability_PO, 1)
})


test_that("the oral peak falls at an hour", {
  o <- oralHydrocodone(20, maximum = 1440)
  w <- o$hydrocodone$wide
  expect_equal(w$Time[which.max(w$Plasma)], 60, tolerance = 2)
  # Kapil 2015 saw 15.9 ng/mL from a 20 mg extended-release tablet; an
  # immediate-release input of the same dose peaks higher and earlier.
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
  expect_equal(ratio, 3.8 / 325.3, tolerance = 0.15)

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
  expect_equal(ratioAt(50), ratioAt(100), tolerance = 0.02)
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

  # MEAC is set equal to morphine's.  Morphine is reported in mcg/mL and
  # hydrocodone in ng/mL, so the two rows carry the same CONCENTRATION with
  # numbers a thousandfold apart.
  expect_equal(HYDROCODONE_MEAC, dd$MEAC[dd$Drug == "morphine"] * 1000)
})


test_that("the effect site is live now that tPeak is set", {
  PK <- getDrugPK("hydrocodone", 70, 171, 50, "male", getDrugDefaults("hydrocodone"))
  expect_equal(PK$tPeak, 60)
  expect_gt(PK$PK$default$ke0, 0)

  o <- oralHydrocodone(20, maximum = 1440)
  w <- o$hydrocodone$wide
  expect_false(any(is.na(w$"Effect Site")))
  expect_gt(max(w$"Effect Site"), 0)
  # The effect site lags the plasma peak
  expect_gt(w$Time[which.max(w$"Effect Site")], w$Time[which.max(w$Plasma)])
  # and it now contributes to the opioid total
  expect_gt(max(o$hydrocodone$equiSpace$MEAC), 0)
})
