# Intravenous calcium chloride: Ansari 2025, parturients at cesarean delivery,
# plotted as the change in ionized calcium in mmol/L.
# Drafted by Claude Code, 2026-10-13, at the request of Steven L. Shafer.

calciumReference <- paste(
  "Ansari JR, Conti DJ, Michel G, Yarmosh A, Cole NM, Shafer SL.",
  "Bioequivalence and pharmacokinetics of intravenous calcium during cesarean delivery.",
  "Anesthesiology 2025;142:121-31 (parturients at cesarean delivery; plotted as the",
  "change in ionized calcium, mmol/L). https://doi.org/10.1097/ALN.0000000000005248"
)

test_that("returns the published parameters with the switch off", {
  # Table 2 of Ansari 2025, unscaled whatever the patient's size.
  actual <- calciumChloride(95, 160, 50, "female", adjustToFFM = FALSE)

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
    # Calcium chloride dihydrate, 147.01 g/mol: 1 g is 6.80 mmol
    molar = list(molecularWeight = 147.01)
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (the factors pinned in test-drugs-alfentanil.R, worked out by hand).  The
  # placeholders v3 = 1 and cl3 = 0 are left alone.
  actual <- calciumChloride(120, 170, 50, "male")
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
  expect_equal_rounded(calciumChloride(70, 170, 35, "male")$PK$default, published)
  expect_equal_rounded(calciumChloride(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default,
                       published)
})

test_that("the CSV row matches the model: molar units, no effect site, no band", {
  defaults <- getDrugDefaults("calciumChloride")
  expect_identical(defaults$Concentration.Units, "mmol")
  expect_identical(defaults$MEAC, 0)
  expect_identical(defaults$endCe, 0)
  expect_identical(c(defaults$Lower, defaults$Typical, defaults$Upper), c(0, 0, 0))
  expect_identical(as.character(defaults$Category), "Other")
  # Intravenous only: no oral, TCI or scheduled units
  units <- defaults$Units[[1]]
  expect_true(all(units %in% c(bolusUnits, infusionUnits)))
  expect_true(all(c("g", "mg", "mg/kg", "mg/min") %in% units))
})

test_that("getDrugPK carries the molar block and no effect site", {
  PK <- getDrugPK("calciumChloride", 70, 170, 35, "male")
  expect_equal(PK$molar, list(molecularWeight = 147.01))
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
                              adjustToFFM = adjustToFFM)
}

at <- function(wide, time) wide$Plasma[wide$Time == time]

test_that("1 g over 10 minutes reproduces the paper's rise in ionized calcium", {
  # Ansari 2025, figure 5 and table 3: 1 g of calcium chloride over 10 min
  # raises ionized calcium by about 0.39 mmol/L at the end of the infusion,
  # and the rise halves by about 30 min after the start.  1 g is
  # 1000 / 147.01 = 6.802258 mmol.  The values below are from an independent
  # matrix-exponential solution of the two-compartment model (not this
  # code): 0.38877795 mmol/L at 10 min and 0.05030493 at 240 min; the peak
  # halves at 29.4 min.
  dose <- data.frame(Drug = "calciumChloride", Time = c(0, 10), Dose = c(100, 0), Units = "mg/min")
  wide <- simulateCalcium(dose)$calciumChloride$wide

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

test_that("gram, milligram, per-kilogram and infusion units agree", {
  grams <- simulateCalcium(data.frame(Drug = "calciumChloride", Time = 0, Dose = 1, Units = "g"))$calciumChloride$wide
  mg    <- simulateCalcium(data.frame(Drug = "calciumChloride", Time = 0, Dose = 1000, Units = "mg"))$calciumChloride$wide
  perKg <- simulateCalcium(data.frame(Drug = "calciumChloride", Time = 0, Dose = 1000 / 70, Units = "mg/kg"))$calciumChloride$wide
  expect_equal(mg$Plasma, grams$Plasma)
  expect_equal(perKg$Plasma, grams$Plasma)

  # A 1 g bolus is 6.802258 mmol in a 10.9 L central volume
  expect_equal_rounded(max(grams$Plasma), 1000 / 147.01 / 10.9)

  perMin  <- simulateCalcium(data.frame(Drug = "calciumChloride", Time = c(0, 10), Dose = c(100, 0), Units = "mg/min"))$calciumChloride$wide
  perHour <- simulateCalcium(data.frame(Drug = "calciumChloride", Time = c(0, 10), Dose = c(6, 0),   Units = "g/hr"))$calciumChloride$wide
  perKgHr <- simulateCalcium(data.frame(Drug = "calciumChloride", Time = c(0, 10), Dose = c(6000 / 70, 0), Units = "mg/kg/hr"))$calciumChloride$wide
  expect_equal(perHour$Plasma, perMin$Plasma)
  expect_equal(perKgHr$Plasma, perMin$Plasma)
})

test_that("the rise scales with the dose and with fat-free mass", {
  dose <- function(g) data.frame(Drug = "calciumChloride", Time = c(0, 10), Dose = c(g * 100, 0), Units = "mg/min")
  one <- simulateCalcium(dose(1))$calciumChloride$wide
  two <- simulateCalcium(dose(2))$calciumChloride$wide
  expect_equal(two$Plasma, 2 * one$Plasma)

  # 120 kg, 170 cm, 50 y man, switch on: V1 is 14.223483 L, so the peak of
  # the 1 g infusion is lower (0.30435980 from the independent solution
  # with the scaled volumes and clearances); switch off, the published
  # parameters, so the same as the reference man.
  heavyOn  <- simulateCalcium(dose(1), adjustToFFM = TRUE,  weight = 120, age = 50)$calciumChloride$wide
  heavyOff <- simulateCalcium(dose(1), adjustToFFM = FALSE, weight = 120, age = 50)$calciumChloride$wide
  expect_equal_rounded(at(heavyOn, 10), 0.30435980)
  expect_equal(heavyOff$Plasma, one$Plasma)
})

test_that("plots on a zero-anchored mmol/L axis", {
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  dose <- data.frame(Drug = "calciumChloride", Time = c(0, 10), Dose = c(100, 0), Units = "mg/min")
  drugs <- processdoseTable(
    dose, events, recalculatePK(NULL, defaults, dose, 35, 70, 170, "male"), 60, FALSE
  )
  out <- simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    plotEvents = FALSE, plotRecovery = FALSE
  )
  expect_match(as.character(unique(out$plotResults$Wrap)), "mmol/L", fixed = TRUE)
  yRange <- ggplot2::ggplot_build(out$plotObject)$layout$panel_params[[1]]$y.range
  expect_lte(yRange[1], 0)
})

test_that("the API and the help name the units mmol/L", {
  expect_identical(apiConcentrationUnits("mmol"), "mmol/L")
  expect_match(helpConcentrationUnits("mmol"), "mmol/L", fixed = TRUE)
})

test_that("a molar block is validated against the concentration units", {
  expect_error(validateMolar(NULL, "x", "mmol"), "must return molar")
  expect_error(validateMolar(list(molecularWeight = 147), "x", "mcg"), "only a drug with")
  expect_error(validateMolar(list(molecularWeight = -1), "x", "mmol"), "positive number")
  expect_error(validateMolar(list(), "x", "mmol"), "positive number")
  expect_null(validateMolar(NULL, "x", "mcg"))
  expect_equal(validateMolar(list(molecularWeight = 147.01), "x", "mmol"),
               list(molecularWeight = 147.01))
})
