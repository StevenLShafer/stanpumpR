# Intravenous calcium: Ansari 2025, parturients at cesarean delivery, plotted
# as the change in ionized calcium in mmol/L.  One drug for both salts; the
# units name the salt ("g chloride", "mg/min gluconate").
# Drafted by Claude Code, 2026-10-13, at the request of Steven L. Shafer.

calciumReference <- paste(
  "Ansari JR, Conti DJ, Michel G, Yarmosh A, Cole NM, Shafer SL.",
  "Bioequivalence and pharmacokinetics of intravenous calcium during cesarean delivery.",
  "Anesthesiology 2025;142:121-31 (parturients at cesarean delivery; plotted as the",
  "change in ionized calcium, mmol/L, from calcium chloride or calcium gluconate",
  "at the fitted ratio of 3.11 g per gram). https://doi.org/10.1097/ALN.0000000000005248"
)

# Grams of each salt per mole of calcium chloride dihydrate equivalent:
# 147.01 g/mol, and 147.01 x 3.11 for calcium gluconate
saltWeights <- c(chloride = 147.01, gluconate = 457.2011)

test_that("returns the published parameters with the switch off", {
  # Table 2 of Ansari 2025, unscaled whatever the patient's size.
  actual <- calcium(95, 160, 50, "female", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 10.9,
        v2 = 16.5,
        v3 = 1,
        cl1 = 0.183,
        cl2 = 1.25,
        cl3 = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = calciumReference,
    molar = list(molecularWeight = saltWeights)
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (the factors pinned in test-drugs-alfentanil.R, worked out by hand).  The
  # placeholders v3 = 1 and cl3 = 0 are left alone.
  actual <- calcium(120, 170, 50, "male")
  expected <- list(
    v1 = 14.223483,
    v2 = 21.530961,
    v3 = 1,
    cl1 = 0.22342701,
    cl2 = 1.5261408,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the reference man receives the published parameters", {
  published <- list(v1 = 10.9, v2 = 16.5, v3 = 1, cl1 = 0.183, cl2 = 1.25, cl3 = 0)
  expect_equal_rounded(calcium(70, 170, 35, "male")$PK$default, published)
  expect_equal_rounded(calcium(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default,
                       published)
})

test_that("the units name the salt and nothing else", {
  expect_equal(CALCIUM_SALTS, c("chloride", "gluconate"))
  expect_equal(calciumUnits, c(
    "g chloride", "mg chloride", "mg/kg chloride", "mg/min chloride",
    "mg/hr chloride", "mg/kg/hr chloride", "g/hr chloride",
    "g gluconate", "mg gluconate", "mg/kg gluconate", "mg/min gluconate",
    "mg/hr gluconate", "mg/kg/hr gluconate", "g/hr gluconate"
  ))
  expect_true(all(calciumUnits %in% allUnits))
  expect_false(any(calciumUnits %in% c(bolusUnits, infusionUnits, scheduledUnits)))
  # Intravenous, and a rate exactly where the base unit is one
  expect_true(all(doseRoute(calciumUnits) == ROUTE_IV))
  expect_equal(isRateUnit(calciumUnits), rep(isRateUnit(calciumBaseUnits), 2))
  expect_equal(doseProduct(calciumUnits, CALCIUM_SALTS), rep(CALCIUM_SALTS, each = 7))
  expect_equal(doseProduct(c("g", "mg/kg", "mg PO"), CALCIUM_SALTS), rep(NA_character_, 3))
  expect_error(doseProduct("g chloride gluconate", CALCIUM_SALTS), "two products")
})

test_that("the CSV row matches the model: molar units, salt units only, no effect site, no band", {
  defaults <- getDrugDefaults("calcium")
  expect_identical(defaults$Concentration.Units, "mmol")
  expect_identical(defaults$MEAC, 0)
  expect_identical(defaults$endCe, 0)
  expect_identical(c(defaults$Lower, defaults$Typical, defaults$Upper), c(0, 0, 0))
  expect_identical(as.character(defaults$Category), "Other")
  expect_setequal(defaults$Units[[1]], calciumUnits)
  expect_identical(defaults$Bolus.Units, "g chloride")
  expect_identical(defaults$Infusion.Units, "mg/min chloride")
  expect_identical(defaults$Default.Units, "g chloride")
  # The two salts are gone as drugs of their own
  expect_false(any(c("calciumChloride", "calciumGluconate") %in% getDrugDefaultsGlobal()$Drug))
})

test_that("getDrugPK carries one weight per salt and no effect site", {
  PK <- getDrugPK("calcium", 70, 170, 35, "male")
  expect_equal_rounded(PK$molar, list(molecularWeight = saltWeights))
  expect_identical(PK$Concentration.Units, "mmol")
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  # Other drugs carry no molar block
  expect_null(getDrugPK("propofol", 70, 170, 35, "male")$molar)
})

events <- data.frame(Time = double(), Event = character())

simulateCalcium <- function(dose, maximum = 240, adjustToFFM = FALSE,
                            weight = 70, height = 170, age = 35, sex = "male") {
  # By default the reference man, so the published parameters apply unscaled
  simulateDrugsWithCovariates(dose, events, weight, height, age, sex, maximum, FALSE,
                              adjustToFFM = adjustToFFM)$calcium$wide
}

infusion <- function(rate, units) {
  data.frame(Drug = "calcium", Time = c(0, 10), Dose = c(rate, 0), Units = units)
}

at <- function(wide, time) wide$Plasma[wide$Time == time]

test_that("1 g of calcium chloride over 10 minutes reproduces the paper's rise in ionized calcium", {
  # Ansari 2025, figure 5 and table 3: 1 g of calcium chloride over 10 min
  # raises ionized calcium by about 0.39 mmol/L at the end of the infusion,
  # and the rise halves by about 30 min after the start.  1 g is
  # 1000 / 147.01 = 6.802258 mmol.  The values below are from an independent
  # matrix-exponential solution of the two-compartment model (not this
  # code): 0.38877795 mmol/L at 10 min and 0.05030493 at 240 min; the peak
  # halves at 29.4 min.
  wide <- simulateCalcium(infusion(100, "mg/min chloride"))

  expect_equal(at(wide, 0), 0)
  expect_equal_rounded(at(wide, 10),  0.38877795)
  expect_equal_rounded(at(wide, 240), 0.05030493)
  expect_equal_rounded(max(wide$Plasma), 0.38877795)
  # The paper's practical numbers: a peak of 0.4 and half of it by 30 min
  expect_equal(round(max(wide$Plasma), 1), 0.4)
  after <- wide[wide$Time > 10, ]
  halfTime <- after$Time[which(after$Plasma <= max(wide$Plasma) / 2)[1]]
  expect_gt(halfTime, 28)
  expect_lt(halfTime, 32)
  # No effect site: plasma only
  expect_true(all(is.na(wide$"Effect Site")))
})

test_that("3.11 g of calcium gluconate is 1 g of calcium chloride", {
  # Bioequivalence as the model states it: the two curves are identical.
  gluconate <- simulateCalcium(infusion(311, "mg/min gluconate"))
  chloride  <- simulateCalcium(infusion(100, "mg/min chloride"))
  expect_equal(gluconate$Plasma, chloride$Plasma)
})

test_that("3 g of calcium gluconate over 10 minutes is 3 / 3.11 of the 1 g calcium chloride rise", {
  # Table 3: 3 g of calcium gluconate (300 mg/min for 10 min) is the usual
  # equivalent of 1 g of calcium chloride, 96% of it by the fitted ratio:
  # 0.38877795 x 3 / 3.11 = 0.37502696 mmol/L at 10 min.
  gluconate <- simulateCalcium(infusion(300, "mg/min gluconate"))
  chloride  <- simulateCalcium(infusion(100, "mg/min chloride"))
  expect_equal(gluconate$Plasma, chloride$Plasma * 3 / 3.11)
  expect_equal_rounded(at(gluconate, 10), 0.37502696)
})

test_that("the dosing error: 1 g of calcium gluconate gives a third of 1 g of calcium chloride", {
  gluconate <- simulateCalcium(infusion(100, "mg/min gluconate"))
  chloride  <- simulateCalcium(infusion(100, "mg/min chloride"))
  expect_equal(gluconate$Plasma, chloride$Plasma / 3.11)
  expect_equal_rounded(at(gluconate, 10), 0.12500899)
  expect_equal(round(at(gluconate, 10), 2), 0.13)
})

test_that("the two salts given together add into one curve", {
  both <- simulateCalcium(data.frame(
    Drug = "calcium", Time = c(0, 10, 0, 10), Dose = c(100, 0, 100, 0),
    Units = c("mg/min chloride", "mg/min chloride", "mg/min gluconate", "mg/min gluconate")
  ))
  chloride  <- simulateCalcium(infusion(100, "mg/min chloride"))
  gluconate <- simulateCalcium(infusion(100, "mg/min gluconate"))
  expect_equal(both$Plasma, chloride$Plasma + gluconate$Plasma)
})

test_that("gram, milligram, per-kilogram and infusion units agree, for each salt", {
  for (salt in CALCIUM_SALTS) {
    u <- function(base) paste(base, salt)
    grams <- simulateCalcium(data.frame(Drug = "calcium", Time = 0, Dose = 1, Units = u("g")))
    mg    <- simulateCalcium(data.frame(Drug = "calcium", Time = 0, Dose = 1000, Units = u("mg")))
    perKg <- simulateCalcium(data.frame(Drug = "calcium", Time = 0, Dose = 1000 / 70, Units = u("mg/kg")))
    expect_equal(mg$Plasma, grams$Plasma)
    expect_equal(perKg$Plasma, grams$Plasma)

    # A 1 g bolus is 1000 / weight mmol in a 10.9 L central volume
    expect_equal_rounded(max(grams$Plasma), 1000 / saltWeights[[salt]] / 10.9)

    perMin  <- simulateCalcium(infusion(100, u("mg/min")))
    perHour <- simulateCalcium(infusion(6, u("g/hr")))
    perKgHr <- simulateCalcium(infusion(6000 / 70, u("mg/kg/hr")))
    mgHr    <- simulateCalcium(infusion(6000, u("mg/hr")))
    expect_equal(perHour$Plasma, perMin$Plasma)
    expect_equal(perKgHr$Plasma, perMin$Plasma)
    expect_equal(mgHr$Plasma, perMin$Plasma)
  }
})

test_that("a dose that does not name its salt is refused, not guessed", {
  for (units in c("g", "mg", "mg/kg", "mg/min")) {
    expect_error(
      simulateCalcium(data.frame(Drug = "calcium", Time = 0, Dose = 1, Units = units)),
      "must name the product, one of chloride, gluconate"
    )
  }
  PK <- getDrugPK("calcium", 70, 170, 35, "male")
  expect_equal(molarWeightOfDoses(PK$molar, c("g gluconate", "mg/min chloride"), "calcium"),
               c(457.2011, 147.01))
  expect_error(molarWeightOfDoses(PK$molar, c("g chloride", "mg"), "calcium"),
               "for calcium must name the product.*: mg")
  # A single unnamed weight applies to every row, whatever the unit
  expect_equal(molarWeightOfDoses(list(molecularWeight = 10), c("g", "mg/kg")), c(10, 10))
})

test_that("the rise scales with the dose and with fat-free mass", {
  dose <- function(g) infusion(g * 100, "mg/min chloride")
  one <- simulateCalcium(dose(1))
  two <- simulateCalcium(dose(2))
  expect_equal(two$Plasma, 2 * one$Plasma)

  # 120 kg, 170 cm, 50 y man, switch on: V1 is 14.223483 L, so the peak of
  # the 1 g infusion is lower (0.30435980 from the independent solution
  # with the scaled volumes and clearances); switch off, the published
  # parameters, so the same as the reference man.
  heavyOn  <- simulateCalcium(dose(1), adjustToFFM = TRUE,  weight = 120, age = 50)
  heavyOff <- simulateCalcium(dose(1), adjustToFFM = FALSE, weight = 120, age = 50)
  expect_equal_rounded(at(heavyOn, 10), 0.30435980)
  expect_equal(heavyOff$Plasma, one$Plasma)
})

test_that("plots on a zero-anchored mmol/L axis, one panel for both salts", {
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  dose <- data.frame(
    Drug = "calcium", Time = c(0, 10, 30), Dose = c(100, 0, 1),
    Units = c("mg/min chloride", "mg/min chloride", "g gluconate")
  )
  drugs <- processdoseTable(
    dose, events, recalculatePK(NULL, defaults, dose, 35, 70, 170, "male"), 60, FALSE
  )
  out <- simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    plotEvents = FALSE, plotRecovery = FALSE
  )
  expect_length(unique(out$plotResults$Wrap), 1)
  expect_match(as.character(unique(out$plotResults$Wrap)), "mmol/L", fixed = TRUE)
  yRange <- ggplot2::ggplot_build(out$plotObject)$layout$panel_params[[1]]$y.range
  expect_lte(yRange[1], 0)
})

test_that("the API and the help name the units mmol/L and the route intravenous", {
  expect_identical(apiConcentrationUnits("mmol"), "mmol/L")
  expect_match(helpConcentrationUnits("mmol"), "mmol/L", fixed = TRUE)
  index <- helpDrugIndexHTML()
  expect_match(as.character(index), "Calcium", fixed = TRUE)
})

test_that("a molar block is validated against the concentration units", {
  expect_error(validateMolar(NULL, "x", "mmol"), "must return molar")
  expect_error(validateMolar(list(molecularWeight = 147), "x", "mcg"), "only a drug with")
  expect_error(validateMolar(list(molecularWeight = -1), "x", "mmol"), "positive")
  expect_error(validateMolar(list(), "x", "mmol"), "positive")
  expect_null(validateMolar(NULL, "x", "mcg"))
  expect_equal(validateMolar(list(molecularWeight = 147.01), "x", "mmol"),
               list(molecularWeight = 147.01))
  # Several products: named, single words, no two alike, all positive
  expect_equal(validateMolar(list(molecularWeight = saltWeights), "x", "mmol"),
               list(molecularWeight = saltWeights))
  expect_error(validateMolar(list(molecularWeight = c(147, 457)), "x", "mmol"), "named by the product")
  expect_error(validateMolar(list(molecularWeight = c(a = 147, a = 457)), "x", "mmol"), "no two alike")
  expect_error(validateMolar(list(molecularWeight = c("a b" = 147)), "x", "mmol"), "single word")
  expect_error(validateMolar(list(molecularWeight = c(a = 147, b = -1)), "x", "mmol"), "positive")
})
