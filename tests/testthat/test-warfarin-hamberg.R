# The Hamberg 2007 warfarin genotype model, a companion of warfarin drawn as a
# second INR line beside the library's Xue model.  See the headers of
# R/alternativeModels.R (warfarinHamberg) and R/inr.R (inrTransit), and the
# primary source Hamberg et al., Clin Pharmacol Ther 2007;81:529-538.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
warfarinDefaults <- getDrugDefaults("warfarin")

simulateINR <- function(entry, maximum = 56 * 1440) {
  dose <- data.frame(Drug = "warfarin", Time = 0, Dose = 5, Units = "mg PO qd",
                     stringsAsFactors = FALSE)
  X <- simCpCe(dose, noEvents, entry, maximum, FALSE)
  X$results
}

test_that("warfarinHamberg returns the published parameters at the reference patient", {
  # A 71-year-old wild-type (*1/*1), VKORC1 GG patient is the reference: the age
  # and genotype terms are both 1, so with the fat-free-mass switch off (no
  # size scaling) the pins are the Table 2 / Table 3 values.
  actual <- warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1", vkorc1 = "GG")
  ka <- 2 / 60
  expect_equal_rounded(actual$PK$default, list(
    v1 = 13.8, v2 = 6.59, v3 = 1,
    cl1 = 0.314 / 60, cl2 = 0.131 / 60, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  ))
  expect_equal_rounded(actual$parallelSystems[[1]]$PK$default, list(
    v1 = 12.8, v2 = 1, v3 = 1,
    cl1 = 0.139 / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  ))
  expect_equal(actual$doseFraction, 0.5)
  expect_equal(actual$parallelSystems[[1]]$name, "R-warfarin")
  expect_equal(actual$parallelSystems[[1]]$doseFraction, 0.5)
  expect_equal_rounded(actual$inr, list(
    type = "transit", c50 = 4.61, gamma = 0.424,
    mtt1 = 11.6 * 60, mtt2 = 120 * 60, n1 = 6L,
    baseline = 1, inrMax = 20, lambda = 3.61))
})

test_that("CYP2C9 and age act on the S-warfarin clearance only", {
  base <- warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1", vkorc1 = "GG")
  # The six CYP2C9 % reductions on CL_S (Table 2).
  reduction <- c("*1/*1" = 0, "*1/*2" = 0.315, "*1/*3" = 0.453,
                 "*2/*2" = 0.722, "*2/*3" = 0.690, "*3/*3" = 0.852)
  for (g in names(reduction)) {
    vk <- warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE, cyp2c9 = g, vkorc1 = "GG")
    expect_equal(vk$PK$default$cl1, 0.314 * (1 - reduction[[g]]) / 60,
                 info = g, tolerance = 1e-8)
    # R-warfarin clearance does not move with CYP2C9.
    expect_equal(vk$parallelSystems[[1]]$PK$default$cl1, base$parallelSystems[[1]]$PK$default$cl1)
  }
  # Age: 0.91%/year on CL_S, 0.98%/year on CL_R (both centred at 71).
  young <- warfarinHamberg(70, 176, 51, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1", vkorc1 = "GG")
  expect_equal(young$PK$default$cl1, 0.314 * (1 - 0.0091 * (51 - 71)) / 60, tolerance = 1e-8)
  expect_equal(young$parallelSystems[[1]]$PK$default$cl1,
               0.139 * (1 - 0.0098 * (51 - 71)) / 60, tolerance = 1e-8)
})

test_that("VKORC1 sets the EC50 only", {
  for (v in c(GG = 4.61, GA = 3.02, AA = 2.20)) {
    name <- names(which(c(GG = 4.61, GA = 3.02, AA = 2.20) == v))
    m <- warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1", vkorc1 = name)
    expect_equal(m$inr$c50, unname(v), info = name)
    # the PK is untouched by VKORC1
    expect_equal(m$PK$default$cl1, 0.314 / 60)
  }
})

test_that("the VKORC1 default gives the companion its lowest EC50", {
  # The app's default, AA (VKORC1_DEFAULT), is the most sensitive genotype in
  # this model as in the library's: Shafer, 2026-10-28.
  expect_identical(VKORC1_DEFAULT, "AA")
  m <- warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1")
  expect_equal(m$inr$c50, 2.20)
  expect_equal(m$inr$c50, min(vapply(VKORC1_VALUES, function(v)
    warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE,
                    cyp2c9 = "*1/*1", vkorc1 = v)$inr$c50, numeric(1))))
})

test_that("invalid genotypes are rejected", {
  expect_error(warfarinHamberg(70, 176, 71, "male", adjustToFFM = FALSE, cyp2c9 = "*9/*9"), "cyp2c9")
  expect_error(warfarinHamberg(70, 176, 71, "male", vkorc1 = "ZZ"), "vkorc1")
})

test_that("the companion entry is a warfarin INR companion with a transit block", {
  entry <- alternativeModelEntry("warfarin", 75, 172, 75, "male", warfarinDefaults,
                                 cyp2c9 = "*2/*3", vkorc1 = "AA")
  expect_true(isAlternativeModelEntry(entry))
  expect_equal(entry$companionOf, "warfarin")
  expect_equal(entry$drug, "warfarin (Hamberg)")
  expect_equal(entry$modelLabel, "Hamberg")
  expect_equal(entry$Concentration.Units, "INR")        # inherited from the row
  expect_false(is.null(entry$inr))
  expect_equal(entry$inr$type, "transit")
  expect_equal(entry$inr$c50, 2.20)                      # VKORC1 AA
  expect_equal(length(entry$parallelSystems), 1)
  # The companion is drawn on warfarin's INR, not added like a metabolite.
  expect_equal(companionHelpPage(entry), "models/hamberg")
})

test_that("the companion group machinery maps warfarin to the Hamberg box", {
  expect_equal(companionModelGroup("warfarin"), "hamberg")
  expect_equal(companionModelGroup("propofol"), "mintoSchnider")
  expect_true(is.na(companionModelGroup("morphine")))
  expect_setequal(companionGroupDrugs("mintoSchnider"), c("propofol", "remifentanil"))
  expect_equal(companionGroupDrugs("hamberg"), "warfarin")
  expect_equal(alternativeModelName("warfarin"), "warfarin (Hamberg)")
  expect_equal(alternativeModelParent("warfarin (Hamberg)"), "warfarin")
})

test_that("recalculatePK builds the Hamberg companion only when its group is on", {
  DT <- data.frame(Drug = "warfarin", Time = 0, Dose = 5, Units = "mg PO qd",
                   stringsAsFactors = FALSE)
  common <- list(drugs = NULL, drugDefaults = getDrugDefaultsGlobal(),
                 doseTable = DT, age = 75, weight = 75, height = 172,
                 sex = "male", cyp2c9 = "*2/*3", vkorc1 = "AA")
  off <- do.call(recalculatePK, c(common, list(alternativeModels = character(0))))
  expect_false("warfarin (Hamberg)" %in% names(off))
  on <- do.call(recalculatePK, c(common, list(alternativeModels = "hamberg")))
  expect_true("warfarin (Hamberg)" %in% names(on))
  expect_equal(on[["warfarin (Hamberg)"]]$companionOf, "warfarin")
  # The Minto / Schnider box does not build a warfarin companion.
  other <- do.call(recalculatePK, c(common, list(alternativeModels = "mintoSchnider")))
  expect_false("warfarin (Hamberg)" %in% names(other))
  # With the box on but warfarin absent from the dose table, no companion is
  # built -- which is what the export's "Hamberg model shown" reads, so a
  # ticked-but-disabled box does not claim a line that is not drawn.
  noWarf <- utils::modifyList(common, list(
    doseTable = data.frame(Drug = "propofol", Time = 0, Dose = 2, Units = "mg/kg",
                           stringsAsFactors = FALSE)))
  none <- do.call(recalculatePK, c(noWarf, list(alternativeModels = "hamberg")))
  expect_false(any(vapply(none, isAlternativeModelEntry, logical(1))))
})

test_that("the simulated INR starts at the baseline, rises, and has no effect site", {
  entry <- alternativeModelEntry("warfarin", 75, 172, 75, "male", warfarinDefaults,
                                 cyp2c9 = "*2/*3", vkorc1 = "AA")
  r <- simulateINR(entry)
  plasma <- r[r$Site == "Plasma", ]
  expect_equal(plasma$Y[1], 1, tolerance = 1e-6)          # baseline INR
  expect_gt(max(plasma$Y), 1.4)                           # it climbs
  # The rise is gradual (clotting-factor turnover), not a bolus spike: the INR
  # one day in is still well below its eventual maximum.
  day1 <- approx(plasma$Time, plasma$Y, 1440)$y
  expect_lt(day1, max(plasma$Y) - 1)
  ce <- r[r$Site == "Effect Site", ]
  expect_true(all(is.na(ce$Y)))                           # no effect site
})

test_that("a more sensitive genotype gives a higher INR on the same dose", {
  sensitive <- alternativeModelEntry("warfarin", 75, 172, 75, "male", warfarinDefaults,
                                     cyp2c9 = "*2/*3", vkorc1 = "AA")
  wildtype  <- alternativeModelEntry("warfarin", 75, 172, 75, "male", warfarinDefaults,
                                     cyp2c9 = "*1/*1", vkorc1 = "GG")
  endOf <- function(entry) {
    p <- simulateINR(entry)
    p <- p[p$Site == "Plasma", ]
    approx(p$Time, p$Y, 56 * 1440)$y
  }
  expect_gt(endOf(sensitive), endOf(wildtype))
  # VKORC1 AA alone raises the INR above GG at the same CYP2C9 and age.
  aa <- alternativeModelEntry("warfarin", 75, 172, 75, "male", warfarinDefaults,
                              cyp2c9 = "*1/*1", vkorc1 = "AA")
  expect_gt(endOf(aa), endOf(wildtype))
})

test_that("inrTransit agrees between a coarse and a fine integration step", {
  # A single 5 mg dose, S-warfarin falling, over four weeks.
  time <- seq(0, 28 * 1440, by = 180)
  inr <- list(type = "transit", c50 = 3.02, gamma = 0.424,
              mtt1 = 11.6 * 60, mtt2 = 120 * 60, n1 = 6L,
              baseline = 1, inrMax = 20, lambda = 3.61)
  # A plausible S-warfarin profile: rises with absorption, falls with clearance.
  cS <- 1.2 * (exp(-log(2) / (40 * 60) * time) - exp(-(2 / 60) * time))
  cS <- pmax(cS, 0)
  coarse <- inrTransit(time, cS, inr, dtMax = 10)
  fine   <- inrTransit(time, cS, inr, dtMax = 1)
  expect_lt(max(abs(coarse - fine)), 1e-3)
  expect_equal(coarse[1], 1, tolerance = 1e-9)            # starts at baseline
})

test_that("validateINR accepts the transit block and checks its fields", {
  good <- list(type = "transit", c50 = 4.61, gamma = 0.424,
               mtt1 = 696, mtt2 = 7200, n1 = 6L,
               baseline = 1, inrMax = 20, lambda = 3.61)
  one <- list(list(name = "R-warfarin"))
  expect_identical(validateINR(good, "warfarin", "INR", one), good)
  # a missing or non-positive field is rejected
  expect_error(validateINR(utils::modifyList(good, list(gamma = 0)), "warfarin", "INR", one),
               "transit INR model")
  expect_error(validateINR(utils::modifyList(good, list(n1 = 6.5)), "warfarin", "INR", one),
               "transit INR model")
  # the transit model still needs exactly one parallel system (R-warfarin)
  expect_error(validateINR(good, "warfarin", "INR", list()), "parallel system")
  # inrBaseline reads the transit baseline, not 1/pca0
  expect_equal(inrBaseline(good), 1)
})

test_that("the Hamberg teaching scenario is valid and turns the box on", {
  s <- helpScenarioById("warfarin-hamberg-genotype")
  expect_false(is.null(s))
  expect_equal(helpScenarioCheck(s), character(0))
  expect_true(s$options$hamberg)
  expect_equal(s$patient$vkorc1, "AA")
  expect_equal(s$patient$cyp2c9, "*2/*3")
})

test_that("the Hamberg companion scales to fat-free mass like any model without a size term", {
  # 120 kg, 170 cm, 50 y man: volumes x 1.3049067, clearances x 1.2209126 (the
  # factors pinned in test-drugs-alfentanil.R), on the published values at
  # 50 y: CL_S = 0.314 x (1 - 0.0091 x (50 - 71)) L/h, CL_R = 0.139 x
  # (1 - 0.0098 x (50 - 71)) L/h.  Worked out by hand.
  on <- warfarinHamberg(120, 170, 50, "male", cyp2c9 = "*1/*1", vkorc1 = "GG")
  expect_equal_rounded(on$PK$default[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 18.007712, v2 = 8.5993352,
                            cl1 = 0.0076104651, cl2 = 0.0026656592))
  expect_equal_rounded(on$parallelSystems[[1]]$PK$default[c("v1", "cl1")],
                       list(v1 = 16.702806, cl1 = 0.0034105420))
  # With the switch off the published values are used at any size
  off <- warfarinHamberg(120, 170, 50, "male", adjustToFFM = FALSE)
  expect_equal(off$PK$default$v1, 13.8)
  expect_equal(off$PK$default$cl1, 0.314 * (1 - 0.0091 * (50 - 71)) / 60)
  # And the companion entry built by the app passes the switch through
  dd <- getDrugDefaults("warfarin")
  entryOn  <- alternativeModelEntry("warfarin", 120, 170, 50, "male", dd, adjustToFFM = TRUE)
  entryOff <- alternativeModelEntry("warfarin", 120, 170, 50, "male", dd, adjustToFFM = FALSE)
  expect_equal(entryOn$PK$default$v1 / entryOff$PK$default$v1, 1.3049067, tolerance = 1e-6)
})
