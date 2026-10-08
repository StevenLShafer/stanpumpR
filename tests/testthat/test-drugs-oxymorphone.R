# Oxymorphone: one compartment, given directly or formed from oxycodone.
#
# The point to guard is that adding a metabolite to oxycodone leaves
# oxycodone's own curve untouched, and that the one-compartment reduction
# reproduces the three things the manufacturer summary actually reports.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

simDrug <- function(drug, dose, units, maximum = 1440, cyp2d6 = CYP2D6_DEFAULT) {
  simulateDrugsWithCovariates(
    data.frame(Drug = drug, Time = 0, Dose = dose, Units = units),
    noEvents, 70, 171, 50, "male", maximum, FALSE, cyp2d6 = cyp2d6
  )
}


test_that("returns the correct calculations", {
  actual <- oxymorphone(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 3.08 * 70, v2 = 1, v3 = 1,
      cl1 = 2.0, cl2 = 0, cl3 = 0,
      ka_PO = 0.0155970777,
      bioavailability_PO = 0.10,
      tlag_PO = 0
    )),
    tPeak = 20,        # provisional; see the drug file
    MEAC = 0.8,        # provisional, a tenth of morphine's; see the drug file
    typical = 2,
    upperTypical = 4,
    lowerTypical = 1,
    reference = actual$reference
  )

  expect_equal_rounded(actual, expected)
})


test_that("one compartment reproduces the reported intravenous summary", {
  x <- oxymorphone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Manufacturer summary: CL 2.0 L/min, Vss 3.08 L/kg, terminal half-life 1.3 h
  expect_equal(x$cl1, 2.0)
  expect_equal(x$v1 / 70, 3.08)
  expect_equal(log(2) * x$v1 / x$cl1 / 60, 1.3, tolerance = 0.06)
  # Mean residence time is exact by construction
  expect_equal(x$v1 / x$cl1 / 60, 3.08 * 70 / 120)

  # Both scale with weight, so the half-life does not
  y <- oxymorphone(35, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$v1 / y$cl1, x$v1 / x$cl1)
})


test_that("an intravenous bolus gives dose over the central volume", {
  # This is the one-compartment path in getDrugPK, which returned
  # 1/(lambda_1 * v1) instead of 1/v1 until it was fixed for codeine.
  o <- simDrug("oxymorphone", 1, "mg", maximum = 720)
  v1 <- oxymorphone(70, 171, 50, "male")$PK$default$v1
  expect_equal(max(o$oxymorphone$wide$Plasma), 1e6 / v1 / 1000, tolerance = 1e-6)
})


test_that("the oral peak matches the observed immediate-release peak", {
  # Adams and Ahdieh 2005: 10 mg of the hydrochloride, which is 8.92 mg of
  # base, gave Cmax 1.93 ng/mL and AUC 9.10 ng.h/mL.
  o <- simDrug("oxymorphone", 10 * 301.34 / 337.80, "mg PO", maximum = 1440)
  w <- o$oxymorphone$wide
  expect_equal(max(w$Plasma), 1.93, tolerance = 0.01)

  # Exposure comes out about 18% below the observed mean, inside one standard
  # deviation of it.  That is the documented disagreement between the
  # intravenous clearance anchor and what the oral data imply; the anchor is
  # kept rather than tuned away.
  auc <- trapz(w$Time, w$Plasma) / 60
  expect_gt(auc, 6.5)
  expect_lt(auc, 9.10)
})


test_that("oxycodone forms oxymorphone at the observed ratio", {
  # Agema 2021: oxymorphone plasma concentrations are about 2% of oxycodone's.
  o <- simDrug("oxycodone", 10, "mg PO", maximum = 2880)
  expect_true("oxymorphone" %in% names(o))
  expect_equal(o$oxymorphone$formedFrom, "oxycodone")

  ratio <- trapz(o$oxymorphone$wide$Time, o$oxymorphone$wide$Plasma) /
           trapz(o$oxycodone$wide$Time,   o$oxycodone$wide$Plasma)
  # Divided by the target, so that the 25% is relative: expect_equal() treats
  # a tolerance larger than the expected value as absolute, and 0.25 against
  # 0.02 passed any ratio below 0.27.  The closed-form ratio is 0.02003; the
  # straight chord the time line drew across the second day until 2026-10-07
  # read 0.02033.  (Claude Code.)
  expect_equal(ratio / 0.02, 1, tolerance = 0.25)
})


test_that("adding the metabolite leaves oxycodone's own curve untouched", {
  # Formation is an independent transfer and is not subtracted from the
  # parent, whose fitted clearance already subsumes it.  So oxycodone's plasma
  # and effect-site curves must be exactly what the published disposition
  # gives, with or without oxymorphone attached.
  PK <- getDrugPK("oxycodone", 70, 171, 50, "male", getDrugDefaults("oxycodone"))
  p  <- PK$PK$default
  dose <- data.frame(Time = 0, Dose = 10 / 0.001, Units = "mg PO",
                     Bolus = FALSE, PO = TRUE, IM = FALSE, IN = FALSE)

  withMetabolite <- advanceClosedFormMetabolite(dose, p, 1440, FALSE, 10)
  pStripped <- p; pStripped$metabolite <- NULL
  without <- advanceClosedFormPO_IM_IN(dose, pStripped, 1440, FALSE, 10)

  expect_equal(withMetabolite$Time, without$Time)
  expect_equal(withMetabolite$Cp, without$Cp)
  expect_equal(withMetabolite$Ce, without$Ce)
  # And oxycodone keeps the effect site it has always had
  expect_gt(max(withMetabolite$Ce), 0)
  expect_equal(PK$MEAC, 12)
  expect_equal(p$ke0, pStripped$ke0)
})


test_that("formed oxymorphone is ordered across CYP2D6 phenotypes", {
  peaks <- vapply(CYP2D6_VALUES,
                  function(g) max(simDrug("oxycodone", 10, "mg PO", 1440, g)$oxymorphone$wide$Plasma),
                  numeric(1))
  expect_true(all(diff(peaks) > 0))
  # Samer 2010: oxymorphone peak 62% lower in poor than extensive metabolisers
  expect_equal(unname(peaks[["poor"]] / peaks[["normal"]]), 0.38, tolerance = 0.02)
  # and 75% lower in poor than ultrarapid
  expect_equal(unname(peaks[["poor"]] / peaks[["ultrarapid"]]), 0.25, tolerance = 0.02)
})


test_that("an unknown phenotype is refused by oxycodone", {
  expect_error(oxycodone(70, 171, 50, "male", "slow"), "Invalid cyp2d6")
})


test_that("the provisional potency is consistent with the drug table", {
  # See the constants at the top of R/drugs_oxymorphone.R.  Changing
  # OXYMORPHONE_MEAC without updating the CSV would silently leave the plot
  # reading the old value.
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_equal(dd$MEAC[dd$Drug == "oxymorphone"], OXYMORPHONE_MEAC)
  expect_equal(dd$endCe[dd$Drug == "oxymorphone"], OXYMORPHONE_MEAC)

  # A tenth of morphine's, taking oxymorphone as ten times as potent.
  # Morphine is reported in mcg/mL and oxymorphone in ng/mL.
  expect_equal(OXYMORPHONE_MEAC, dd$MEAC[dd$Drug == "morphine"] * 1000 / 10)
})


test_that("the effect site is live, whether dosed directly or formed", {
  PK <- getDrugPK("oxymorphone", 70, 171, 50, "male", getDrugDefaults("oxymorphone"))
  expect_equal(PK$tPeak, 20)
  expect_gt(PK$PK$default$ke0, 0)

  direct <- simDrug("oxymorphone", 5, "mg PO", maximum = 720)
  w <- direct$oxymorphone$wide
  expect_false(any(is.na(w$"Effect Site")))
  # The effect site lags, but with a 4.7 min equilibration half-time against
  # slow oral absorption the two peaks can share a grid point, so the lag is
  # checked where it is visible rather than at the peak.
  expect_gte(w$Time[which.max(w$"Effect Site")], w$Time[which.max(w$Plasma)])
  early <- w[w$Time > 0 & w$Time < w$Time[which.max(w$Plasma)] / 2, ]
  expect_true(all(early$"Effect Site" < early$Plasma))

  # Arriving as oxycodone's metabolite, it must also carry a real effect site
  formed <- simDrug("oxycodone", 10, "mg PO", maximum = 1440)
  f <- formed$oxymorphone$wide
  expect_false(any(is.na(f$"Effect Site")))
  expect_gt(max(f$"Effect Site"), 0)
  # and now contribute to the opioid total rather than nothing
  expect_gt(max(formed$oxymorphone$equiSpace$MEAC), 0)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volume x 1.3049067, clearance x 1.2209126
  # (worked out from the Al-Sallami formula by hand).
  x <- oxymorphone(120, 170, 50, "male")$PK$default
  expect_equal_rounded(x$v1,  281.33787)
  expect_equal_rounded(x$cl1, 2.4418252)
})
