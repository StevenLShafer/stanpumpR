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
  # pinned as numbers, not through pkSizeFactors(), which the model calls:
  # every volume x 1.3049066, every clearance x 1.2209126 at this patient
  on <- diphenhydramine(120, 170, 50, "male")
  expect_equal_rounded(on$PK$default, list(
    v1 = 301.43344, v2 = 285.10640, v3 = 1,
    cl1 = 0.83583678, cl2 = 1.4403182, cl3 = 0
  ))
  # with the switch off, linearly in weight (x 120/70)
  off <- diphenhydramine(120, 170, 50, "male", adjustToFFM = FALSE)
  expect_equal_rounded(off$PK$default, list(
    v1 = 396, v2 = 374.55080, v3 = 1,
    cl1 = 1.1736, cl2 = 2.0223535, cl3 = 0
  ))
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

test_that("Albert 1975's two men: clearance agrees, the end of infusion is underpredicted", {
  # not fitted: 50 mg hydrochloride (assayed 43.7 mg base) over 1 h, Table II
  measured <- list(c(120, 222, 258, 150, 132, 83.8, 61.6, 19.4, 6.49),
                   c(31.0, 89.1, 179, 170, 147, 80.3, 41.5, 18.2, 8.19))
  times <- c(0.25, 0.5, 1, 1.5, 2, 4, 6, 12, 24) * 60
  weights <- c(76, 95)
  rate <- 43.7 * 291.82 / 255.36 / 60          # mg/min of the hydrochloride
  dose <- data.frame(Drug = "diphenhydramine", Time = c(0, 60),
                     Dose = c(rate, 0), Units = "mg/min")
  ratios <- lapply(1:2, function(s)
    diphenhydraminePlasma(dose, weights[s], 1500, times, height = 178, age = 27) /
      measured[[s]])
  # at the end of the infusion the model is a third low (V1 back-extrapolated
  # from Meredith's 15-min first sample); pinned so a change is seen
  expect_equal(round(c(ratios[[1]][3], ratios[[2]][3]), 2), c(0.62, 0.72))
  # from 2 to 6 h, within 0.55-1.0 of what was measured
  expect_true(all(unlist(lapply(ratios, `[`, 5:7)) > 0.55 &
                  unlist(lapply(ratios, `[`, 5:7)) < 1.0))
  # Albert's clearances (footnote 5, on the base) on the hydrochloride
  clAlbert <- c(37.0, 41.1) * 291.82 / 255.36 / 60 / weights * 1000
  expect_equal(round(clAlbert, 1), c(9.3, 8.2))
  expect_lt(max(abs(clAlbert / DIPHENHYDRAMINE_CL - 1)), 0.2)
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
