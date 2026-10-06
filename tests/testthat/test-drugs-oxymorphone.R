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
  actual <- oxymorphone(70, 171, 50, "male")

  expected <- list(
    PK = list(default = list(
      v1 = 3.08 * 70, v2 = 1, v3 = 1,
      cl1 = 2.0, cl2 = 0, cl3 = 0,
      ka_PO = 0.0155970777,
      bioavailability_PO = 0.10,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 2,
    upperTypical = 4,
    lowerTypical = 1,
    reference = actual$reference
  )

  expect_equal_rounded(actual, expected)
})


test_that("one compartment reproduces the reported intravenous summary", {
  x <- oxymorphone(70, 171, 50, "male")$PK$default
  # Manufacturer summary: CL 2.0 L/min, Vss 3.08 L/kg, terminal half-life 1.3 h
  expect_equal(x$cl1, 2.0)
  expect_equal(x$v1 / 70, 3.08)
  expect_equal(log(2) * x$v1 / x$cl1 / 60, 1.3, tolerance = 0.06)
  # Mean residence time is exact by construction
  expect_equal(x$v1 / x$cl1 / 60, 3.08 * 70 / 120)

  # Both scale with weight, so the half-life does not
  y <- oxymorphone(35, 171, 50, "male")$PK$default
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
  expect_equal(ratio, 0.02, tolerance = 0.25)
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


test_that("the placeholder potency is consistent with the drug table", {
  # See the header of R/drugs_oxymorphone.R.  Changing OXYMORPHONE_MEAC
  # without updating the CSV would silently leave the plot reading zero.
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_equal(dd$MEAC[dd$Drug == "oxymorphone"], OXYMORPHONE_MEAC)

  if (OXYMORPHONE_TPEAK == 0) {
    PK <- getDrugPK("oxymorphone", 70, 171, 50, "male", getDrugDefaults("oxymorphone"))
    expect_equal(PK$PK$default$ke0, 0)
    o <- simDrug("oxymorphone", 5, "mg PO", maximum = 720)
    expect_true(all(is.na(o$oxymorphone$wide$"Effect Site")))
    expect_false(any(is.na(o$oxymorphone$equiSpace$Ce)))

    # The same must hold when it arrives as a metabolite rather than a dose
    q <- simDrug("oxycodone", 10, "mg PO", maximum = 720)
    expect_true(all(is.na(q$oxymorphone$wide$"Effect Site")))
    expect_gt(max(q$oxymorphone$wide$Plasma), 0)
  }
})
