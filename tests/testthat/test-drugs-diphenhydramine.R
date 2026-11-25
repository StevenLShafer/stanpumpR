# Diphenhydramine (R/drugs_diphenhydramine.R): Meredith 1984's healthy men,
# two compartments, a clinical time to peak effect of 2 min and Gengo 1989's
# band.  Drafted by Claude Code, 2026-11-25.

diphenhydraminePlasma <- function(dose, weight, maximum, times, height = 175,
                                  age = 35, sex = "male", adjustToFFM = FALSE)
{
  wide <- simulateDrugsWithCovariates(
    dose, data.frame(Time = double(), Event = character()),
    weight, height, age, sex, maximum, FALSE,
    adjustToFFM = adjustToFFM)$diphenhydramine$wide
  stats::approx(wide$Time, wide$Plasma, xout = times)$y
}

test_that("the model keeps Meredith's V1, clearance and both exponents", {
  set <- diphenhydramine(70, 175, 35, "male", adjustToFFM = FALSE)$PK$default
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  s <- k10 + k12 + k21
  lambda <- (s + c(1, -1) * sqrt(s^2 - 4 * k10 * k21)) / 2
  # Table II and the text: alpha 0.7292 /h, beta 0.079 /h
  expect_equal(lambda * 60, c(0.7292, 0.079), tolerance = 1e-10)
  expect_equal(set$v1 / 70, 3.3, tolerance = 1e-10)
  expect_equal(set$cl1 / 70 * 1000, 9.78, tolerance = 1e-10)
  # Vss 6.42 L/kg against the printed mean 6.52 (a mean of individual fits)
  vss <- (set$v1 + set$v2) / 70
  expect_lt(abs(vss / 6.52 - 1), 0.02)
  # Cp0 after 0.8 mg/kg: 242 ng/mL against the printed mean 256.7
  expect_lt(abs(0.8 / 3.3 * 1000 / 256.7 - 1), 0.06)
})

test_that("the paper's V1 is read on the hydrochloride, not the base", {
  # 0.8 mg/kg / 256.7 ng/mL: 3.12 L/kg on the salt, 2.73 on the base,
  # against the printed 3.3
  salt <- 0.8 / 256.7 * 1000
  base <- salt * 255.36 / 291.82
  expect_lt(abs(salt - 3.3), abs(base - 3.3))
})

test_that("returns the correct calculations", {
  actual <- diphenhydramine(70, 175, 35, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 231, v2 = 218.48797, v3 = 1,
      cl1 = 0.6846, cl2 = 1.1797062, cl3 = 0
    )),
    tPeak = 2, MEAC = 0,
    typical = 41.5, upperTypical = 74.4, lowerTypical = 30.4
  )
  expect_equal_rounded(actual[names(expected)], expected)
  expect_match(actual$reference, "Meredith")
})

test_that("scales to fat-free mass for a 120 kg man, linearly with the switch off", {
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  on <- diphenhydramine(120, 170, 50, "male")$PK$default
  expect_equal(on$v1, 231 * size$volume, tolerance = 1e-6)
  expect_equal(on$cl1, 0.6846 * size$clearance, tolerance = 1e-6)
  off <- diphenhydramine(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 231 * 120 / 70, tolerance = 1e-6)
  expect_equal(off$cl2, 1.1797062 * 120 / 70, tolerance = 1e-6)
})

test_that("the effect site peaks 2 min after a bolus", {
  pk <- getDrugPK("diphenhydramine", 70, 175, 35, "male")$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2)
  lambda <- c(pk$lambda_1, pk$lambda_2)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 2, tolerance = 1e-4)
})

test_that("Carruthers' measured levels are matched at 1 h, overpredicted at 8 h", {
  # not fitted: 50 mg hydrochloride IV over 1 min, mean 70 kg;
  # 144 +- 17 ng/mL (SE) at 1 h, 37 +- 3 at 8 h
  dose <- data.frame(Drug = "diphenhydramine", Time = c(0, 1),
                     Dose = c(50, 0), Units = "mg/min")
  cp <- diphenhydraminePlasma(dose, 70, 600, c(60, 480))
  expect_lt(abs(cp[1] - 144), 17)
  # the model is 19% high at 8 h (Carruthers' subjects cleared faster);
  # pinned so a change is seen
  expect_equal(round(cp, 0), c(141, 44))
})

test_that("Meredith's normal-subject clearance is above Blyden's, as recorded", {
  # Blyden 1986: 6.2 mL/min/kg; the model's 9.78 is Meredith's
  set <- diphenhydramine(70, 175, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_gt(set$cl1 / 70 * 1000, 6.2 * 1.5)
})

test_that("diphenhydramine is plotted in ng/mL, with Gengo's band, in hypnotics", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "diphenhydramine", ]
  expect_identical(row$Category, "Hypnotics and sedatives")
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(30.4, 41.5, 74.4))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 41.5)
})
