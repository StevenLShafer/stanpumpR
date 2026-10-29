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
  # Allegaert 2015 Table 3's base-scale parameters divided by 263.38 / 299.84
  # = 0.8784, so that a hydrochloride dose, as labelled, gives base
  # concentrations.  At 50 years every maturation factor is 1 to within
  # 1e-12 for the parent.
  f <- 263.38 / 299.84

  expected <- list(
    PK = list(default = list(
      v1 = 51 / f, v2 = 134 / f, v3 = 1,
      cl1 = (21.6 + 10.5) / 60 / f, cl2 = 227 / 60 / f, cl3 = 0,
      ka_PO = 0.0205398742,
      bioavailability_PO = 0.75,
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
      kFormation        = (10.5 / 60) / 51,
      firstPassFraction = 0.10,
      mwRatio           = 249.38 / 299.84
    )
  )

  expect_equal_rounded(actual, expected)
})


test_that("desmetramadol returns the correct calculations", {
  actual <- desmetramadol(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 78.9, v2 = 133, v3 = 1,
      # GFR-like maturation, 1 - 1.2e-6 at 50 years
      cl1 = 71.4 / 60 * tramadolMaturation(50)[["m1"]], cl2 = 562 / 60, cl3 = 0
    )),
    # ke0 is supplied directly rather than solved from a tPeak; the peak is
    # observed against the metabolite curve after an ORAL PARENT dose, which
    # getDrugPK cannot build.  See the drug file.
    tPeak = 0,
    ke0 = 0.0930286574,
    MEAC = 84,
    typical = 40,
    upperTypical = 80,
    lowerTypical = 20,
    reference = actual$reference
  )

  expect_equal_rounded(actual, expected)
})


test_that("both carry Allegaert's allometry", {
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
  f <- TRAMADOL_BASE_FRACTION
  expect_equal(unname(cl[["normal"]]) * f, 32.1, tolerance = 1e-6)
  expect_equal(unname(cl[["poor"]]) * f, 21.6 + 10.5 * 0.10, tolerance = 1e-6)
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
  # transplanted: it was fitted against apparent volumes roughly twice the
  # physical ones.
  o <- oralTramadol(100, maximum = 1440)
  w <- o$tramadol$wide
  expect_equal(w$Time[which.max(w$Plasma)], 59, tolerance = 6)
  # 100 mg of the hydrochloride by mouth gives a few hundred ng/mL of base:
  # 262 on Allegaert's disposition with F 0.75 (298 on Holford's with F
  # 0.70, which the library used until 2026-10-27; 339 with the dose read
  # as base, before that)
  expect_gt(max(w$Plasma), 200)
  expect_lt(max(w$Plasma), 450)
  expect_equal(max(w$Plasma), 261.80, tolerance = 0.01)
})


test_that("an intravenous dose gives dose over the central volume", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg"),
    noEvents, 70, 171, 50, "male", 1440, FALSE, adjustToFFM = FALSE)
  # 51 L is Allegaert's 70 kg central volume, hence the legacy switch.
  # 100 mg of the hydrochloride is 87.84 mg of base (Allegaert 2015's
  # 0.8784), so C0 is 1722 ng/mL, not the 1961 the base dose would give.
  expect_equal(max(o$tramadol$wide$Plasma), 87.84e6 / 51 / 1000, tolerance = 1e-4)
  expect_equal(round(87.84e3 / 51), 1722)
  expect_gt(max(o$desmetramadol$wide$Plasma), 0)
})


test_that("a hydrochloride dose forms M1 on the base molar ratio", {
  # The formed amount per milligram of the hydrochloride is the base molar
  # ratio times 0.8784: a 10% one-to-one conversion of 100 mg tramadol HCl is
  # 87.84 x 0.10 x 249.38/263.38 = 8.3171 mg of M1, not the 9.4685 mg a base
  # dose would give.
  m <- tramadol(70, 171, 50, "male")$metabolite
  expect_equal(100 * 0.10 * m$mwRatio, 87.84 * 0.10 * 249.38 / 263.38, tolerance = 1e-4)
  expect_equal(round(100 * 0.10 * m$mwRatio, 4), 8.3171)
  # Formation as a rate constant is untouched by the salt
  a <- tramadol(70, 171, 50, "male", adjustToFFM = FALSE)
  expect_equal(a$metabolite$kFormation, (10.5 / 60) / 51, tolerance = 1e-9)
})


test_that("desmetramadol appears only as a metabolite, never as a dose", {
  # Allegaert (as Holford) fixed the metabolite central volume using dog
  # data.  Formed
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
  # 84 ng/mL is Grond 1999's mean serum total M1 before PCA demands during
  # racemic tramadol, a mixed-exposure observation, not an isolated M1 MEC
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
  kF <- (10.5 / 60) / 51

  peakFor <- function(fp) {
    co <- metaboliteCoefficients(p, mo, kFormation = kF, mwRatio = 249.38 / 263.38,
                                 unitScale = 1, firstPassFraction = fp)
    stats::optimize(function(t) sum(co$PO * exp(-co$lambda * t)),
                    c(1, 3000), maximum = TRUE)$maximum
  }
  expect_gt(peakFor(0), 220)      # formation alone: about 4.6 h
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


test_that("exactly the drugs meant to be plasma-only lack an effect site", {
  # codeine, tramadol and prednisone are prodrugs by design; the antibiotics,
  # the other steroids, sugammadex and glycopyrrolate have no equilibration
  # model to attach (see each drug's header); mannitol is plotted as serum
  # osmolality and has no published ke0; amiodarone and desethylamiodarone
  # (2026-10-07) and amiodaroneIV (2026-10-08) are active but have no
  # published human ke0 for the antiarrhythmic effect; clonazepam's,
  # zolpidem's and temazepam's published effects are direct functions of
  # plasma concentration (2026-10-09); methylphenidate and lisdexamfetamine
  # have no calibrated concentration-effect model for any ADHD endpoint;
  # bupivacaine, ropivacaine and mepivacaine model systemic plasma
  # concentration, not the nerve block (2026-10-10).  The antidepressants
  # (2026-10-10) have no effect site: response lags weeks and no ke0 has been
  # estimated; ondansetron, aprepitant and fosaprepitant have no
  # equilibration model for antiemesis (2026-10-10).  Diclofenac, meloxicam
  # and ketorolac have no published ke0 (2026-10-10), nor diclofenacEC
  # (2026-10-11); naproxen's effect follows its plasma with no delay
  # (2026-10-11).  Pinned because the
  # count has been got wrong by hand more than once.
  dd <- getDrugDefaultsGlobal(FALSE)
  blank <- Filter(function(d) {
    k <- tryCatch(getDrugPK(d, 70, 171, 50, "male",
                            getDrugDefaults(d))$PK$default$ke0,
                  error = function(e) NA_real_)
    !is.na(k) && k == 0
  }, dd$Drug[dd$Class == "IV"])
  expect_setequal(blank, c(
    "codeine", "tramadol", "prednisone",
    "cefazolin", "clindamycin", "cefalexin", "ceftriaxone", "vancomycin",
    "metronidazole", "gentamicin",
    "hydrocortisone", "methylprednisolone", "dexamethasone", "prednisolone",
    "sugammadex", "glycopyrrolate", "mannitol", "gabapentin",
    "amiodarone", "desethylamiodarone", "amiodaroneIV",
    "clonazepam", "zolpidem", "temazepam",
    "methylphenidate", "lisdexamfetamine", "mixedAmphetamineSalts", "methylphenidatePediatric",
    "bupivacaine", "ropivacaine", "mepivacaine",
    "phenytoin", "carbamazepine", "valproate", "phenobarbital", "pentobarbital",
    "ethosuximide", "topiramate", "lacosamide", "lamotrigine", "zonisamide",
    "tiagabine", "levetiracetam", "eslicarbazepine", "cenobamate",
    "escitalopram", "citalopram", "sertraline", "paroxetine", "duloxetine",
    "mirtazapine", "bupropion", "hydroxybupropion", "fluoxetine", "norfluoxetine", "venlafaxine", "desvenlafaxine",
    "ondansetron", "aprepitant", "fosaprepitant",
    "diclofenac", "diclofenacEC", "meloxicam", "ketorolac", "naproxen",
    # aspirin and the salicylate it forms (2026-10-10): no ke0
    "aspirin", "salicylate",
    # diamorphine (2026-10-10): the opt-in illicit research model plots parent
    # plasma only, with 6-MAM and morphine not modelled, so it has no effect site.
    "diamorphine",
    # The opt-in illicit research models (2026-10-17) are plasma only: see
    # inst/extdata/illicitRouteRegistry.csv and each drug's header.
    "cocaine", "methamphetamine", "LSD", "psilocybin", "MDMA", "GHB",
    # carfentanil (2026-10-18): sufentanil's DISPOSITION only, so its
    # tPeak deliberately does not come with it (R/drugs_carfentanil.R)
    "carfentanil",
    # Antipsychotics (2026-10-09): occupancy is a direct function of plasma,
    # or no effect is established; aripiprazole and haloperidol have PET ke0s.
    "quetiapine", "risperidone", "hydroxyrisperidone", "dehydroaripiprazole",
    "olanzapine", "droperidol", "droperidolIM",
    # The antidiabetic drugs (2026-10-17): concentration only, no ke0
    # (inst/help/models/antidiabetics.md)
    "semaglutide", "tirzepatide", "liraglutide", "insulinRegular",
    "insulinRegularU500", "insulinNPH", "insulinHumanMix70", "insulinLispro",
    "insulinLisproMix75", "insulinLisproMix50", "insulinGlargine", "metformin",
    "empagliflozin", "ertugliflozin", "glyburide", "dulaglutide", "dapagliflozin",
    "pioglitazone", "pramlintideIV", "exenatide", "linagliptin", "vildagliptin", "liraglutideWatson", "dapagliflozinKobuchi", "canagliflozin", "lixisenatide",
    "semaglutideOral", "cetagliptin",
    # Intravenous calcium (2026-10-13): the ionized calcium itself is the
    # end point, no ke0 (R/drugs_calcium.R).
    "calcium",
    # Anticoagulants (2026-10-17): plasma activity or concentration, or the
    # INR; no effect site (R/drugs_heparin.R and the others).
    "heparin", "bivalirudin", "apixaban", "rivaroxaban",
    "edoxaban", "dabigatran", "dabigatranHip", "apixabanVTE", "heparinCPB", "argatroban", "enoxaparin", "fondaparinux",
    "warfarin", "fluindione", "acenocoumarol",
    # The antihypertensives joined on 2026-10-17: concentration only, no
    # published equilibration constant, and no blood pressure is plotted for
    # any drug in the library (docs/antihypertensives.md).
    "clevidipine", "fenoldopamPediatric", "carvedilol",
    "azilsartan", "chlorthalidone", "hydrochlorothiazide",
    # The renin-angiotensin drugs (2026-10-20): none of their sources fitted
    # any pharmacodynamic model, and valsartan's fitted one is a blood
    # pressure, which nothing in this library plots
    "lisinopril", "olmesartan", "valsartan", "benazepril",
    "enalapril", "enalaprilat", "fosinopril",
    # vasoactive and resuscitation drugs (2026-10-18): plasma only
    "magnesiumSulfate", "labetalol", "nitroglycerin",
    "procainamide", "dopamine",
    "digoxin",
    "milrinonePediatric",
    "digoxinIVPediatric",
    "methyleneBlue",
    "ephedrine"
  ))
})

test_that("the pair scales to fat-free mass identically for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand), the same factors for
  # both members of the jointly fitted pair.
  t <- tramadol(120, 170, 50, "male")$PK$default
  d <- desmetramadol(120, 170, 50, "male")$PK$default
  f <- 263.38 / 299.84   # hydrochloride doses (see R/drugs_tramadol.R)
  v <- 1.30490665; cl <- 1.2209126
  expect_equal_rounded(t$v1,  51 * v / f)
  expect_equal_rounded(t$v2,  134 * v / f)
  expect_equal_rounded(t$cl1, 32.1 / 60 * cl / f)
  expect_equal_rounded(t$cl2, 227 / 60 * cl / f)
  expect_equal_rounded(d$v1,  78.9 * v)
  expect_equal_rounded(d$v2,  133 * v)
  expect_equal_rounded(d$cl1, 71.4 / 60 * cl * tramadolMaturation(50)[["m1"]])
  expect_equal_rounded(d$cl2, 562 / 60 * cl)
  # and the formation constant falls with size the same way under either switch
  f <- function(w, ...) tramadol(w, 170, 50, "male", ...)$metabolite$kFormation
  expect_equal(f(120) / f(70), f(120, adjustToFFM = FALSE) / f(70, adjustToFFM = FALSE) *
                 (1.30490665^-0.25) / ((120 / 70)^-0.25), tolerance = 1e-6)
})


test_that("each clearance matures with postmenstrual age (Allegaert 2015 Table 8)", {
  # Fmat = PMA^Hill / (TM50^Hill + PMA^Hill), PMA = 40 + 52 x age (term
  # birth).  Worked out by hand from Table 8's original estimates: at term
  # (40 weeks) other pathways (40/39)^6.76 / (1 + (40/39)^6.76) = 0.5428,
  # formation (40/39.9)^9 / (1 + ...) = 0.5056, M1 elimination
  # (40/47.7)^3.4 / (1 + ...) = 0.3546.
  m0 <- tramadolMaturation(0)
  expect_equal(unname(m0[["other"]]),     0.5428, tolerance = 1e-3)
  expect_equal(unname(m0[["formation"]]), 0.5056, tolerance = 1e-3)
  expect_equal(unname(m0[["m1"]]),        0.3546, tolerance = 1e-3)
  # At one year (92 weeks) the parent's pathways are mature and the
  # metabolite's elimination, following filtration, is not yet
  m1 <- tramadolMaturation(1)
  expect_gt(min(m1[c("other", "formation")]), 0.99)
  expect_equal(unname(m1[["m1"]]), 0.9026, tolerance = 1e-3)
  # Monotonic, and 1 in adults
  ages <- c(0, 0.1, 0.5, 1, 2, 5, 18, 80)
  for (k in names(TRAMADOL_MATURATION))
    expect_true(all(diff(vapply(ages, function(a) tramadolMaturation(a)[[k]],
                                numeric(1))) >= 0), info = k)
  expect_equal(unname(tramadolMaturation(30)), c(1, 1, 1), tolerance = 1e-5)

  # In the model: a term neonate's clearances, scaled to 70 kg by
  # (W/70)^0.75, are the adult values times the maturation, and its first
  # pass matures with the formation clearance.  The size factors are
  # removed by comparing with the same weight at 30 years.
  neo   <- tramadol(3.5, 50, 0, "male", adjustToFFM = FALSE)
  adult <- tramadol(3.5, 50, 30, "male", adjustToFFM = FALSE)
  expect_equal(neo$PK$default$cl1 / adult$PK$default$cl1,
               (21.6 * m0[["other"]] + 10.5 * m0[["formation"]]) / 32.1,
               tolerance = 1e-6)
  expect_equal(neo$PK$default$v1, adult$PK$default$v1)
  expect_equal(neo$PK$default$cl2, adult$PK$default$cl2)
  expect_equal(neo$metabolite$firstPassFraction,
               0.10 * unname(m0[["formation"]]), tolerance = 1e-9)
  expect_equal(desmetramadol(3.5, 50, 0, "male", adjustToFFM = FALSE)$PK$default$cl1,
               71.4 / 60 * (3.5 / 70)^0.75 * unname(m0[["m1"]]), tolerance = 1e-9)
  # Allometrically scaled, a term neonate's tramadol clearance is 18.1 L/h
  # per 70 kg; Allegaert's introduction quotes 17.1 L/h/70 kg from the
  # earlier neonatal literature
  expect_equal(neo$PK$default$cl1 * 60 * TRAMADOL_BASE_FRACTION / (3.5 / 70)^0.75,
               32.1 * (21.6 * m0[["other"]] + 10.5 * m0[["formation"]]) / 32.1,
               tolerance = 1e-6)
})


test_that("the ULTRAM label comparison is what the header records", {
  # ULTRAM (tramadol hydrochloride) tablets, NDA 020281, revised March 2008,
  # Table 1, healthy adults: 100 mg single dose, tramadol 308 ng/mL and M1
  # 55.0 ng/mL; 100 mg qid at steady state, 592 and 110 ng/mL.  Neither
  # Allegaert nor Holford fitted these, and the model is known to run low
  # (clearance 32.1 L/h against the label's 6.4 mL/min/kg = 26.9 L/h).  The
  # shortfall is recorded, not corrected; this pins it so that a change in
  # either direction is seen.  Oral F is the label's own 0.75.
  expect_equal(tramadol(70, 175, 35, "male")$PK$default$bioavailability_PO, 0.75)
  sim <- function(times, maximum) {
    o <- simulateDrugsWithCovariates(
      data.frame(Drug = "tramadol", Time = times, Dose = 100, Units = "mg PO"),
      noEvents, 70, 175, 35, "male", maximum, FALSE)
    list(t = o$tramadol$wide, m = o$desmetramadol$wide)
  }
  sd <- sim(0, 2880)
  expect_equal(max(sd$t$Plasma) / 308,  0.83, tolerance = 0.03)
  expect_equal(max(sd$m$Plasma) / 55.0, 0.69, tolerance = 0.03)
  md <- sim((0:28) * 360, 29 * 360)
  last <- function(w) max(w$Plasma[w$Time >= 28 * 360])
  expect_equal(last(md$t) / 592, 0.75, tolerance = 0.03)
  expect_equal(last(md$m) / 110, 0.69, tolerance = 0.03)
  # The label's intravenous clearance in men against the model's
  expect_equal(6.4 * 70 * 60 / 1000, 26.88)
  expect_equal(tramadol(70, 175, 35, "male", adjustToFFM = FALSE)$PK$default$cl1 *
                 60 * TRAMADOL_BASE_FRACTION, 32.1, tolerance = 1e-6)
})
