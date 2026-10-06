# Tramadol and its active metabolite desmetramadol.
#
# Tramadol is modelled as a prodrug on purpose: only the metabolite's
# mu-opioid activity is in scope, and the parent's monoaminergic action is
# not represented.  That is a scope decision, not pharmacology, and the tests
# below pin the consequences so nobody mistakes one for the other.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

oralTramadol <- function(mg = 100, cyp2d6 = CYP2D6_DEFAULT, maximum = 2880) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "tramadol", Time = 0, Dose = mg, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", maximum, FALSE, cyp2d6 = cyp2d6)
}

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)


test_that("tramadol returns the correct calculations", {
  actual <- tramadol(70, 171, 50, "male")

  expected <- list(
    PK = list(default = list(
      v1 = 90, v2 = 79, v3 = 1,
      cl1 = 28.9 / 60, cl2 = 105 / 60, cl3 = 0,
      ka_PO = 0.0254568712,
      bioavailability_PO = 0.70,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 300,
    upperTypical = 600,
    lowerTypical = 100,
    reference = actual$reference,
    metabolite = list(
      name              = "desmetramadol",
      kFormation        = (10.5 / 60) / 90,
      firstPassFraction = 0,
      mwRatio           = 249.38 / 263.38
    )
  )

  expect_equal_rounded(actual, expected)
})


test_that("desmetramadol returns the correct calculations", {
  actual <- desmetramadol(70, 171, 50, "male")

  expected <- list(
    PK = list(default = list(
      v1 = 78.9, v2 = 131, v3 = 1,
      cl1 = 84.2 / 60, cl2 = 274 / 60, cl3 = 0
    )),
    tPeak = 0,
    MEAC = 84,
    typical = 40,
    upperTypical = 80,
    lowerTypical = 20,
    reference = actual$reference
  )

  expect_equal_rounded(actual, expected)
})


test_that("both carry Holford's allometry", {
  # clearances (W/70)^0.75, volumes W/70
  for (fn in list(tramadol, desmetramadol)) {
    a <- fn(70, 171, 50, "male")$PK$default
    b <- fn(35, 171, 50, "male")$PK$default
    expect_equal(b$v1 / a$v1, 0.5, tolerance = 1e-9)
    expect_equal(b$v2 / a$v2, 0.5, tolerance = 1e-9)
    expect_equal(b$cl1 / a$cl1, 0.5^0.75, tolerance = 1e-9)
    expect_equal(b$cl2 / a$cl2, 0.5^0.75, tolerance = 1e-9)
  }
})


test_that("tramadol is a prodrug only because the parent's activity is out of scope", {
  # It has real monoaminergic analgesia and weak mu activity; neither is
  # modelled.  A tramadol patient is NOT fully described by the metabolite.
  PK <- getDrugPK("tramadol", 70, 171, 50, "male", getDrugDefaults("tramadol"))
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$MEAC, 0)

  o <- oralTramadol(100, maximum = 1440)
  expect_true(all(is.na(o$tramadol$wide$"Effect Site")))
  expect_false(any(is.na(o$tramadol$equiSpace$Ce)))
})


test_that("formation is CYP2D6 driven and ordered across the four phenotypes", {
  w <- TRAMADOL_CYP2D6_WEIGHT
  expect_equal(unname(w[["normal"]]), 1)
  expect_true(all(diff(unname(w[c("poor","intermediate","normal","ultrarapid")])) > 0))

  # Stamer 2007 early (+)-M1 AUC medians normalised to the two-gene group
  expect_equal(unname(w[["intermediate"]]), 38.6/66.5, tolerance = 1e-4)
  expect_equal(unname(w[["ultrarapid"]]), 149.7/66.5, tolerance = 1e-4)
  # Lee 2019 independently puts *10/*10 formation at 0.472 of its comparator,
  # within 20% of Stamer's intermediate group
  expect_lt(abs(w[["intermediate"]] - 0.472), 0.12)
  # Poor is deliberately NOT the literal zero median; a structural zero would
  # assert that no CYP2D6-independent route exists
  expect_gt(unname(w[["poor"]]), 0)
  expect_lt(unname(w[["poor"]]), 0.18)   # below Stamer's own third quartile

  peaks <- vapply(CYP2D6_VALUES,
                  function(g) max(oralTramadol(100, g, maximum = 1440)$desmetramadol$wide$Plasma),
                  numeric(1))
  expect_true(all(diff(peaks) > 0))
  expect_gt(peaks[["ultrarapid"]] / peaks[["poor"]], 10)
})


test_that("phenotype moves the parent's own clearance too", {
  # Formation is a branch of total clearance, so a poor metaboliser clears
  # tramadol more slowly and has higher parent concentrations.  This is the
  # clinically familiar direction.
  cl <- vapply(CYP2D6_VALUES,
               function(g) tramadol(70, 171, 50, "male", g)$PK$default$cl1 * 60,
               numeric(1))
  expect_equal(unname(cl[["normal"]]), 28.9, tolerance = 1e-6)
  expect_equal(unname(cl[["poor"]]), 18.4 + 10.5 * 0.10, tolerance = 1e-6)
  expect_true(all(diff(cl) > 0))

  peaks <- vapply(CYP2D6_VALUES,
                  function(g) max(oralTramadol(100, g, maximum = 1440)$tramadol$wide$Plasma),
                  numeric(1))
  # parent goes the other way from the metabolite
  expect_true(all(diff(peaks) < 0))
})


test_that("an unknown phenotype is refused", {
  expect_error(tramadol(70, 171, 50, "male", "extensive"), "Invalid cyp2d6")
})


test_that("the oral peak falls where Brvar's own model puts it", {
  # 0.99 h.  Brvar's inverse-Gaussian mean absorption time was NOT
  # transplanted: it was fitted against apparent volumes roughly twice
  # Holford's, and dropped onto this disposition it would peak at 0.85 h.
  o <- oralTramadol(100, maximum = 1440)
  w <- o$tramadol$wide
  expect_equal(w$Time[which.max(w$Plasma)], 59, tolerance = 6)
  # 100 mg oral gives a few hundred ng/mL
  expect_gt(max(w$Plasma), 250)
  expect_lt(max(w$Plasma), 450)
})


test_that("an intravenous dose gives dose over the central volume", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg"),
    noEvents, 70, 171, 50, "male", 1440, FALSE)
  expect_equal(max(o$tramadol$wide$Plasma), 100e6 / 90 / 1000, tolerance = 1e-6)
  expect_gt(max(o$desmetramadol$wide$Plasma), 0)
})


test_that("desmetramadol appears only as a metabolite, never as a dose", {
  # Holford fixed the metabolite central volume using dog data.  Formed
  # concentrations are invariant to that scale; a directly administered dose
  # would be dose over an unidentified volume, so no dosing unit is offered.
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- dd$Units[dd$Drug == "desmetramadol"]
  expect_true(is.na(units) || !nzchar(units))

  o <- oralTramadol(100, maximum = 1440)
  expect_true("desmetramadol" %in% names(o))
  expect_equal(o$desmetramadol$formedFrom, "tramadol")
  expect_gt(max(o$desmetramadol$wide$Plasma), 0)
})


test_that("the provisional potency is consistent with the drug table", {
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_equal(dd$MEAC[dd$Drug == "desmetramadol"], DESMETRAMADOL_MEAC)
  expect_equal(dd$endCe[dd$Drug == "desmetramadol"], DESMETRAMADOL_MEAC)
  # Lee 2019 cites 84 ug/L as the minimum effective concentration of M1
  expect_equal(DESMETRAMADOL_MEAC, 84)

  # tPeak is the remaining blank, and until it is set the MEAC is not read
  if (DESMETRAMADOL_TPEAK == 0) {
    PK <- getDrugPK("desmetramadol", 70, 171, 50, "male",
                    getDrugDefaults("desmetramadol"))
    expect_equal(PK$PK$default$ke0, 0)
    o <- oralTramadol(100, maximum = 1440)
    expect_true(all(is.na(o$desmetramadol$wide$"Effect Site")))
    expect_false(any(is.na(o$desmetramadol$equiSpace$Ce)))
  }
})
