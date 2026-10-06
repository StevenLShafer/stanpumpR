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
  actual <- tramadol(70, 171, 50, "male", adjustToFFM = FALSE)

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
      firstPassFraction = 0.10,
      mwRatio           = 249.38 / 263.38
    )
  )

  expect_equal_rounded(actual, expected)
})


test_that("desmetramadol returns the correct calculations", {
  actual <- desmetramadol(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 78.9, v2 = 131, v3 = 1,
      cl1 = 84.2 / 60, cl2 = 274 / 60, cl3 = 0
    )),
    # ke0 is supplied directly rather than solved from a tPeak; the peak is
    # observed against the metabolite curve after an ORAL PARENT dose, which
    # getDrugPK cannot build.  See the drug file.
    tPeak = 0,
    ke0 = 0.0287536942,
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
    a <- fn(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
    b <- fn(35, 171, 50, "male", adjustToFFM = FALSE)$PK$default
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
               function(g) tramadol(70, 171, 50, "male", g, adjustToFFM = FALSE)$PK$default$cl1 * 60,
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
    noEvents, 70, 171, 50, "male", 1440, FALSE, adjustToFFM = FALSE)
  # 90 L is the published 70 kg central volume, hence the legacy switch
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
})


test_that("peak analgesia falls at 2.5 h after an oral tramadol dose", {
  # This is the whole point of the metabolite model, and it is the one
  # number the equilibration was solved for.  Checked analytically, because
  # the plotted grid is far coarser than the tolerance worth asserting.
  #
  # The driving curve is the metabolite profile after an ORAL PARENT dose,
  # not a bolus and not the drug's own oral curve, so a change to tramadol's
  # absorption, formation or first-pass fraction invalidates the solved ke0.
  # This test is what makes that loud.
  p   <- getDrugPK("tramadol", 70, 171, 50, "male",
                   getDrugDefaults("tramadol"))$PK$default
  ke0 <- getDrugPK("desmetramadol", 70, 171, 50, "male",
                   getDrugDefaults("desmetramadol"))$PK$default$ke0
  co  <- p$metabolite$coefs

  e <- co$PO * ke0 / (ke0 - co$lambda)
  peak <- stats::optimize(
    function(t) sum(e * exp(-co$lambda * t)) - sum(e) * exp(-ke0 * t),
    c(1, 5000), maximum = TRUE)$maximum
  expect_equal(peak, DESMETRAMADOL_TPEAK_ORAL_PARENT, tolerance = 0.01)
  expect_equal(DESMETRAMADOL_TPEAK_ORAL_PARENT, 150)

  # and the effect site lags the metabolite concentration, as it must
  plasmaPeak <- stats::optimize(
    function(t) sum(co$PO * exp(-co$lambda * t)), c(1, 3000), maximum = TRUE)$maximum
  expect_lt(plasmaPeak, peak)
})


test_that("first pass is what makes that peak reachable at all", {
  # With systemic formation alone the metabolite peaked at about 4 h, and an
  # effect site cannot peak before the curve driving it, so 2.5 h was
  # impossible rather than merely unfitted.
  p  <- getDrugPK("tramadol", 70, 171, 50, "male",
                  getDrugDefaults("tramadol"))$PK$default
  mo <- getDrugPK("desmetramadol", 70, 171, 50, "male",
                  getDrugDefaults("desmetramadol"))$PK$default
  kF <- (10.5 / 60) / 90

  peakFor <- function(fp) {
    co <- metaboliteCoefficients(p, mo, kFormation = kF, mwRatio = 249.38 / 263.38,
                                 unitScale = 1, firstPassFraction = fp)
    stats::optimize(function(t) sum(co$PO * exp(-co$lambda * t)),
                    c(1, 3000), maximum = TRUE)$maximum
  }
  expect_gt(peakFor(0), 220)      # formation alone: about 4 h
  expect_lt(peakFor(0.10), 150)   # with first pass: early enough to work
})


test_that("first pass is CYP2D6 driven like the systemic route", {
  # Presystemic O-demethylation is the same reaction, so a poor metaboliser
  # must not receive a full first-pass contribution.  Leaving the fraction
  # constant was a bug: it put poor at 71% of normal instead of a tenth.
  fp <- vapply(CYP2D6_VALUES,
               function(g) tramadol(70, 171, 50, "male", g)$metabolite$firstPassFraction,
               numeric(1))
  expect_equal(unname(fp[["normal"]]), 0.10)
  expect_true(all(diff(fp) > 0))
  expect_equal(unname(fp[["poor"]] / fp[["normal"]]),
               unname(TRAMADOL_CYP2D6_WEIGHT[["poor"]]), tolerance = 1e-9)

  peaks <- vapply(CYP2D6_VALUES,
                  function(g) max(oralTramadol(100, g, maximum = 1440)$desmetramadol$wide$Plasma),
                  numeric(1))
  expect_gt(peaks[["ultrarapid"]] / peaks[["poor"]], 15)
})


test_that("the metabolite's effect site is live and carries tramadol's effect", {
  PK <- getDrugPK("desmetramadol", 70, 171, 50, "male",
                  getDrugDefaults("desmetramadol"))
  expect_gt(PK$PK$default$ke0, 0)

  o <- oralTramadol(100, maximum = 1440)
  w <- o$desmetramadol$wide
  expect_false(any(is.na(w$"Effect Site")))
  expect_gt(max(o$desmetramadol$equiSpace$MEAC), 0)

  # the parent still contributes nothing: its activity is out of scope
  expect_true(all(is.na(o$tramadol$wide$"Effect Site")))
  expect_equal(max(o$tramadol$equiSpace$MEAC), 0)
})


test_that("the opioid contribution is ordered across phenotypes", {
  meac <- vapply(CYP2D6_VALUES,
                 function(g) max(oralTramadol(100, g, maximum = 1440)$desmetramadol$equiSpace$MEAC),
                 numeric(1))
  expect_true(all(diff(meac) > 0))
  expect_lt(meac[["poor"]], 10)
  expect_gt(meac[["ultrarapid"]] / meac[["poor"]], 10)
})


test_that("only the two deliberate prodrugs now lack an effect site", {
  # codeine and tramadol, both by design.  Pinned because the count has been
  # got wrong by hand more than once.
  dd <- getDrugDefaultsGlobal(FALSE)
  blank <- Filter(function(d) {
    k <- tryCatch(getDrugPK(d, 70, 171, 50, "male",
                            getDrugDefaults(d))$PK$default$ke0,
                  error = function(e) NA_real_)
    !is.na(k) && k == 0
  }, dd$Drug[dd$Class == "IV"])
  expect_setequal(blank, c("codeine", "tramadol"))
})

test_that("the pair scales to fat-free mass identically for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand), the same factors for
  # both members of the jointly fitted pair.
  t <- tramadol(120, 170, 50, "male")$PK$default
  d <- desmetramadol(120, 170, 50, "male")$PK$default
  expect_equal_rounded(t$v1,  117.4416)
  expect_equal_rounded(t$v2,  103.08763)
  expect_equal_rounded(t$cl1, 0.58807291)
  expect_equal_rounded(t$cl2, 2.1365971)
  expect_equal_rounded(d$v1,  102.95713)
  expect_equal_rounded(d$v2,  170.94277)
  expect_equal_rounded(d$cl1, 1.7133474)
  expect_equal_rounded(d$cl2, 5.575501)
  # and the formation constant falls with size the same way under either switch
  f <- function(w, ...) tramadol(w, 170, 50, "male", ...)$metabolite$kFormation
  expect_equal(f(120) / f(70), f(120, adjustToFFM = FALSE) / f(70, adjustToFFM = FALSE) *
                 (1.30490665^-0.25) / ((120 / 70)^-0.25), tolerance = 1e-6)
})
