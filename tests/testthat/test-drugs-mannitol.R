test_that("returns the published parameters with the switch off", {
  # Kaneda 2010 population means, high-dose clearance.  With the switch off
  # the volumes and distribution clearances are unscaled whatever the
  # patient's size; CL1 still carries renal function, Cockcroft-Gault on total
  # weight at the assumed creatinine (0.8 for a woman):
  #   (140 - 50) x 95 / (72 x 0.8) x 0.85 = 126.17 mL/min, against the
  #   reference man's (140 - 35) x 70 / (72 x 0.9) = 113.43, so
  #   CL1 = 0.07 x 1.112372.
  actual <- mannitol(95, 160, 50, "female", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 2.80,
        v2 = 8.86,
        v3 = 12.0,
        cl1 = 0.077866071,
        cl2 = 2.07,
        cl3 = 0.16
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 310,
    upperTypical = 320,
    lowerTypical = 300,
    reference = paste(
      "Kaneda K et al., J Clin Pharmacol 2010;50(5):536-543. https://pubmed.ncbi.nlm.nih.gov/20051588/",
      "Osmolality: Rudehill A et al., J Neurosurg Anesthesiol 1993;5(1):4-12. https://pubmed.ncbi.nlm.nih.gov/8431668/",
      "Renal function: stanpumpR assumption, CL1 scaled by Cockcroft-Gault creatinine clearance",
      "over the reference patient's, from the entered creatinine or an assumed normal one."
    ),
    osmotic = list(
      baseline = 280,
      # (310 - 292) / (5.91 mg/mL * 1000 / 182.17), worked out by hand
      fraction = 0.55483249,
      molecularWeight = 182.17
    )
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, distribution clearances
  # x 1.2209126 (the factors pinned in test-drugs-alfentanil.R, worked out by
  # hand).  CL1 is renal: Cockcroft-Gault at the pharmacokinetic weight
  # 70 x 1.3049067 = 91.343 kg, (140 - 50) x 91.343 / (72 x 0.9) = 126.87
  # mL/min, over the reference man's 113.43, so CL1 = 0.07 x 1.118491.
  actual <- mannitol(120, 170, 50, "male")
  expected <- list(
    v1 = 3.6537388,
    v2 = 11.561473,
    v3 = 15.658880,
    cl1 = 0.078294402,
    cl2 = 2.5272891,
    cl3 = 0.19534602
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the reference man receives the published parameters", {
  # Kaneda's means, unscaled: his fat-free mass is the reference, and his
  # Cockcroft-Gault clearance at the assumed creatinine of 0.9 mg/dL is the
  # reference 113.43 mL/min.
  published <- list(v1 = 2.80, v2 = 8.86, v3 = 12.0, cl1 = 0.07, cl2 = 2.07, cl3 = 0.16)
  actual <- mannitol(70, 170, 35, "male")
  expect_equal_rounded(actual$PK$default, published)
  expect_equal_rounded(
    actual$PK$default,
    mannitol(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  )
})

test_that("carries the baseline osmolality it is given", {
  expect_identical(mannitol(70, 170, 35, "male", osmolality = 305)$osmotic$baseline, 305)
})

test_that("the CSV row matches the model's units and band", {
  defaults <- getDrugDefaults("mannitol")
  expect_identical(defaults$Concentration.Units, "mOsm")
  # Infusion only: mannitol is run in over 15 to 60 minutes, never pushed, so
  # the library offers no bolus unit and no scheduled (qd, bid, ...) unit.
  expect_true(is.na(defaults$Bolus.Units) || !nzchar(defaults$Bolus.Units))
  expect_identical(defaults$Infusion.Units, "g/hr")
  expect_identical(defaults$Default.Units, "g/hr")
  expect_identical(defaults$Units[[1]], c("g/hr", "g/kg/hr"))
  expect_identical(defaults$MEAC, 0)
  expect_identical(c(defaults$Lower, defaults$Typical, defaults$Upper), c(300, 310, 320))
})

events <- data.frame(Time = double(), Event = character())

simulateMannitol <- function(dose, osmolality = OSMOLALITY_DEFAULT, maximum = 480) {
  # The reference man, so the published parameters apply unscaled.
  simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male", maximum, FALSE,
                              osmolality = osmolality)$mannitol$wide
}

at <- function(wide, time) wide$Plasma[wide$Time == time]

test_that("predicts serum osmolality after 1 g/kg over 30 minutes", {
  # 70 g over 30 minutes is 384.26 mOsm.  Plasma mannitol at 30 and 480
  # minutes, 26.622358 and 3.7288398 mOsm/L, from an independent matrix
  # exponential solution of the three-compartment model (not this code).
  dose <- data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(140, 0), Units = "g/hr")
  wide <- simulateMannitol(dose)

  # The default baseline, 280 mOsm/kg
  expect_equal(at(wide, 0), 280)
  expect_equal_rounded(at(wide, 30),  280 + 0.55483249 * 26.622358)  # 294.77
  expect_equal_rounded(at(wide, 480), 280 + 0.55483249 * 3.7288398)  # 282.07
  # No effect site: the drug is plotted as plasma only.
  expect_true(all(is.na(wide$"Effect Site")))
})

test_that("the baseline osmolality shifts the curve and nothing else", {
  dose <- data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(100, 0), Units = "g/hr")
  normal <- simulateMannitol(dose, osmolality = 290)
  high   <- simulateMannitol(dose, osmolality = 310)
  expect_equal(high$Plasma - normal$Plasma, rep(20, nrow(normal)))
})

test_that("per-hour and per-kilogram-per-hour units agree", {
  perHour <- simulateMannitol(data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(140, 0), Units = "g/hr"))
  perKgHr <- simulateMannitol(data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(2, 0),   Units = "g/kg/hr"))
  expect_equal(perKgHr$Plasma, perHour$Plasma)

  # 70 g over 30 minutes is 384.26 mOsm.  The osmolality peaks when the
  # infusion stops, 0.55483249 times the 26.622358 mOsm/L above (not at the
  # 137 mOsm/L a bolus of the same amount would give in the 2.8 L central
  # volume: mannitol is never pushed, and the library offers no bolus unit).
  expect_equal(which.max(perHour$Plasma), which(perHour$Time == 30))
  expect_equal_rounded(max(perHour$Plasma), 280 + 0.55483249 * 26.622358)
})

test_that("an invalid baseline osmolality is rejected", {
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", osmolality = 50), "Invalid osmolality")
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", osmolality = NA_real_), "Invalid osmolality")
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", osmolality = "290"), "Invalid osmolality")
})

test_that("other drugs ignore the baseline osmolality", {
  a <- getDrugPK("propofol", 70, 170, 35, "male", osmolality = 280)
  b <- getDrugPK("propofol", 70, 170, 35, "male", osmolality = 320)
  expect_identical(a, b)
  expect_null(a$osmotic)
})

test_that("plots on a serum osmolality axis that does not start at zero", {
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  dose <- data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(140, 0), Units = "g/hr")
  # The app's own path, with the patient's baseline osmolality.
  drugs <- processdoseTable(
    dose, events,
    recalculatePK(NULL, defaults, dose, 35, 70, 170, "male", osmolality = 300),
    240, FALSE
  )
  out <- simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    xBreaks = seq(0, 240, 60), xLabels = seq(0, 240, 60),
    plotEvents = FALSE, plotRecovery = FALSE, typical = "Range"
  )
  expect_equal(min(out$plotResults$Y), 300)
  expect_match(as.character(unique(out$plotResults$Wrap)), "mOsm/kg", fixed = TRUE)

  yRange <- ggplot2::ggplot_build(out$plotObject)$layout$panel_params[[1]]$y.range
  # The floor is 10 * floor((300 - 5) / 10) = 290, not zero.
  expect_gt(yRange[1], 280)
  expect_lt(yRange[1], 300)
})

test_that("every other panel still starts at zero", {
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  dose <- data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg")
  drugs <- processdoseTable(
    dose, events, recalculatePK(NULL, defaults, dose, 35, 70, 170, "male"), 60, FALSE
  )
  out <- simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    plotEvents = FALSE, plotRecovery = FALSE
  )
  yRange <- ggplot2::ggplot_build(out$plotObject)$layout$panel_params[[1]]$y.range
  expect_lte(yRange[1], 0)
})

test_that("peak normalisation scales the rise above the baseline, not the osmolality", {
  dose <- data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(140, 0), Units = "g/hr")
  out <- simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male", 240, FALSE)
  norm <- out$mannitol$results
  norm <- norm[norm$Site == "CpNormCp", ]
  # Zero before the dose, 100 at the peak, whatever the baseline
  expect_equal(norm$Y[norm$Time == 0], 0)
  expect_equal(max(norm$Y), 100)
  # The unnormalised series is still the absolute osmolality
  expect_equal(out$mannitol$max$Cp, max(out$mannitol$wide$Plasma))
  expect_gt(out$mannitol$max$Cp, 280)
})


test_that("clearance follows the entered serum creatinine", {
  # A 70 kg, 170 cm, 50 y man.  Cockcroft-Gault is inversely proportional to
  # creatinine, so CL1 is too, and nothing else changes.
  normal <- mannitol(70, 170, 50, "male")
  high   <- mannitol(70, 170, 50, "male", creatinine = 2.0)
  # Blank means the assumed value, 0.9 mg/dL for a man.
  expect_equal(mannitol(70, 170, 50, "male", creatinine = SCR_ASSUMED_MALE)$PK, normal$PK)
  expect_equal(mannitol(70, 170, 50, "male", creatinine = NA)$PK, normal$PK)
  # (140 - 50) x 70 / (72 x 2) = 43.75 mL/min over 113.43: CL1 = 0.07 x 0.385714
  expect_equal_rounded(high$PK$default$cl1, 0.027)
  expect_equal(high$PK$default[c("v1", "v2", "v3", "cl2", "cl3")],
               normal$PK$default[c("v1", "v2", "v3", "cl2", "cl3")])
})

test_that("a high creatinine prolongs the osmolality rise", {
  dose <- data.frame(Drug = "mannitol", Time = c(0, 30), Dose = c(140, 0), Units = "g/hr")
  run <- function(creatinine) {
    w <- simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male", 480, FALSE,
                                     creatinine = creatinine)$mannitol$wide
    w$Plasma[w$Time == 480]
  }
  # The reference man with a creatinine of 3.6 mg/dL, four times the assumed
  # 0.9, has a quarter of the reference creatinine clearance, so CL1 = 0.0175
  # L/min.  Eight hours after 1 g/kg over 30 minutes his plasma mannitol is
  # 10.954746 mOsm/L, from an independent matrix-exponential solution (not
  # this code), against 3.7288398 with normal kidneys.
  expect_equal_rounded(run(4 * SCR_ASSUMED_MALE), 280 + 0.55483249 * 10.954746)  # 286.08
  expect_gt(run(4 * SCR_ASSUMED_MALE), run(NULL))
})

test_that("an invalid creatinine is rejected and a blank one is not", {
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", creatinine = 0), "Invalid creatinine")
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", creatinine = 50), "Invalid creatinine")
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", creatinine = "1"), "Invalid creatinine")
  expect_identical(getDrugPK("mannitol", 70, 170, 35, "male", creatinine = NA),
                   getDrugPK("mannitol", 70, 170, 35, "male"))
  expect_error(getDrugPK("mannitol", 70, 170, 35, "male", creatinine = NaN), "Invalid creatinine")
})

test_that("pregnancy: creatinine clearance x 1.5 when the creatinine is blank", {
  # A woman of 30 years, 70 kg, 165 cm, creatinine blank: the Cockcroft-Gault
  # creatinine clearance in CL1's renal factor x PREGNANCY_GFR_FACTOR, so CL1 x
  # 1.5 (R/drugs_mannitol.R, PREGNANCY); volumes and Q unchanged.
  on  <- getDrugPK("mannitol", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("mannitol", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("mannitol", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, PREGNANCY_GFR_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  # A measured creatinine already carries the rise: no pregnancy change
  cr <- function(p) getDrugPK("mannitol", 70, 165, 30, SEX_FEMALE, pregnant = p,
                              creatinine = 0.6)
  expect_identical(cr(TRUE)$PK, cr(FALSE)$PK)
  expect_no_match(cr(TRUE)$reference, "Deferm", fixed = TRUE)
  expect_match(on$reference, "Deferm", fixed = TRUE)
  expect_no_match(off$reference, "Deferm", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- mannitol(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(2.257066149, actual$v1)
  expect_equal_rounded(7.142002171, actual$v2)
  expect_equal_rounded(9.673140638, actual$v3)
  expect_equal_rounded(0.0847911234, actual$cl1)
  expect_equal_rounded(1.761002763, actual$cl2)
  expect_equal_rounded(0.1361161556, actual$cl3)
})
