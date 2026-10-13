# Calcium gluconate: the calcium chloride disposition (Ansari 2025) reached
# through the fitted dose ratio of 3.11 g per gram of calcium chloride.
# Drafted by Claude Code, 2026-10-13, at the request of Steven L. Shafer.

test_that("shares the calcium chloride disposition and converts the dose by 3.11", {
  gluconate <- calciumGluconate(95, 160, 50, "female", adjustToFFM = FALSE)
  chloride  <- calciumChloride(95, 160, 50, "female", adjustToFFM = FALSE)

  expect_equal(gluconate$PK, chloride$PK)
  expect_equal(gluconate$PK$default,
               list(v1 = 10.9, v2 = 16.5, v3 = 1, cl1 = 0.183, cl2 = 1.25, cl3 = 0))
  expect_equal(gluconate$tPeak, 0)
  expect_equal(gluconate$MEAC, 0)
  expect_equal(gluconate$reference, chloride$reference)
  # 147.01 g/mol x 3.11: grams of calcium gluconate per mole of calcium
  # chloride dihydrate equivalent
  expect_equal_rounded(gluconate$molar, list(molecularWeight = 457.2011))
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- calciumGluconate(120, 170, 50, "male")
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

test_that("the CSV row matches calcium chloride's", {
  g <- getDrugDefaults("calciumGluconate")
  c <- getDrugDefaults("calciumChloride")
  expect_identical(g$Concentration.Units, "mmol")
  expect_identical(g$Units[[1]], c$Units[[1]])
  expect_identical(c(g$Lower, g$Typical, g$Upper, g$MEAC, g$endCe), c(0, 0, 0, 0, 0))
  expect_identical(as.character(g$Category), "Other")
  expect_false(identical(g$Color, c$Color))
})

events <- data.frame(Time = double(), Event = character())

simulateSalt <- function(drug, dose, units = "mg/min", maximum = 240) {
  # The reference man, so the published parameters apply unscaled
  d <- data.frame(Drug = drug, Time = c(0, 10), Dose = c(dose, 0), Units = units)
  simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", maximum, FALSE)[[drug]]$wide
}

test_that("3.11 g of calcium gluconate is 1 g of calcium chloride", {
  # Bioequivalence as the model states it: the two curves are identical.
  gluconate <- simulateSalt("calciumGluconate", 311)
  chloride  <- simulateSalt("calciumChloride", 100)
  expect_equal(gluconate$Plasma, chloride$Plasma)
})

test_that("3 g of calcium gluconate over 10 minutes is 3 / 3.11 of the 1 g calcium chloride rise", {
  # Table 3: 3 g of calcium gluconate (300 mg/min for 10 min) is the
  # practical equivalent of 1 g of calcium chloride.  3 / 3.11 of
  # 0.38877795 is 0.37502697 mmol/L at 10 min (independent solution).
  gluconate <- simulateSalt("calciumGluconate", 300)
  chloride  <- simulateSalt("calciumChloride", 100)
  expect_equal_rounded(gluconate$Plasma[gluconate$Time == 10], 0.37502697)
  expect_equal(gluconate$Plasma, chloride$Plasma * 3 / 3.11)
  expect_equal(round(max(gluconate$Plasma), 1), 0.4)
  expect_true(all(is.na(gluconate$"Effect Site")))
})

test_that("gram and milligram units agree", {
  grams <- simulateSalt("calciumGluconate", 1, units = "g/hr")
  mg    <- simulateSalt("calciumGluconate", 1000, units = "mg/hr")
  expect_equal(mg$Plasma, grams$Plasma)
  # A 1 g bolus: 1000 / 457.2011 = 2.1872 mmol in 10.9 L
  bolus <- simulateDrugsWithCovariates(
    data.frame(Drug = "calciumGluconate", Time = 0, Dose = 1, Units = "g"),
    events, 70, 170, 35, "male", 60, FALSE)$calciumGluconate$wide
  expect_equal_rounded(max(bolus$Plasma), 1000 / (147.01 * 3.11) / 10.9)
})
