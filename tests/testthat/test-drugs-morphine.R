test_that("returns Lotsch's published parameters with the switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off gives Model A as published: absolute volumes and
  # clearances, and Q3 = 19.5 L/h x (BSA / 1.83)^4 with Du Bois BSA 1.8174 m2
  # for 70 kg and 171 cm, so 0.325 x 0.97279 = 0.31615 L/min.
  actual <- morphine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 17.5,
        v2 = 85.82865,
        v3 = 195.646,
        cl1 = 1.2333333,
        cl2 = 2.2283333,
        cl3 = 0.31615473,
        ka_PO = 0.013,
        bioavailability_PO = 0.29,
        tlag_PO = 17.6
      )
    ),
    tPeak = 93.8,
    MEAC = 0.008,
    typical = 0.0096,
    upperTypical = 0.016,
    lowerTypical = 0.0064,
    reference = paste0(
      "Lotsch J et al., Clin Pharmacol Ther 2002;72(2):151-162. ",
      "https://pubmed.ncbi.nlm.nih.gov/12189362/ (intravenous, Model A, ",
      "Q3 x (BSA/1.83)^4; tPeak and MEAC are library values, not the paper's); ",
      "Atrux-Tallau N et al., Clin Drug Investig 2022;42:1101-1112. ",
      "https://pubmed.ncbi.nlm.nih.gov/36331670/ (oral tablet and liquid: ",
      "pivotal tablet Cmax, tmax and AUC; liquid absorption rate from the ",
      "pilot Cmax, same bioavailability)"
    ),
    oralFormulations = list(
      liquid = list(ka_PO = 0.01887, bioavailability_PO = 0.29, tlag_PO = 17.6)
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and CL and Q2 x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).  Q3
  # keeps the paper's covariate at the body surface area bounded to 2.2 m2
  # (Du Bois BSA is 2.2756 m2): 0.325 x (2.2/1.83)^4 = 0.325 x 2.08875 =
  # 0.67884 L/min.
  actual <- morphine(120, 170, 50, "male")
  expected <- list(
        v1 = 22.835866,
        v2 = 111.99838,
        v3 = 255.29977,
        cl1 = 1.5057922,
        cl2 = 2.7206003,
        cl3 = 0.67884360
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

# Oral tablet and liquid, against the source they were calibrated to:
# Atrux-Tallau 2022, 30 mg morphine sulfate in fasted adults.  Pivotal study
# (n = 39): tablet Cmax 28.5 ng/mL at a median 0.75 h, AUC 117.4 ng.h/mL.
# Pilot crossover (n = 17): solution Cmax 37.9 ng/mL, AUC within 10% of the
# other formulations, so the liquid takes the tablet's bioavailability; its
# pilot AUC is not compared with the pivotal tablet's.  Simulated through the app's own path, getDrugPK() and
# simCpCe(), at the 70 kg reference man.
oralMorphine <- function(units, maximum = 24 * 60, time = 0, extra = NULL) {
  PK <- getDrugPK("morphine", 70, 170, 40, "male")
  dose <- rbind(data.frame(Drug = "morphine", Time = time, Dose = 30,
                           Units = units), extra)
  events <- data.frame(Time = numeric(0), Event = character(0))
  simCpCe(dose, events, PK, maximum, plotRecovery = TRUE)
}
plasma <- function(sim) {
  r <- sim$results
  r[r$Site == "Plasma", c("Time", "Y")]
}

test_that("an oral tablet reproduces the observed peak, its time and AUC", {
  cp <- plasma(oralMorphine("mg PO tablet"))
  expect_equal(max(cp$Y) * 1000, 28.5, tolerance = 0.01)
  expect_equal(cp$Time[which.max(cp$Y)], 45, tolerance = 0.05)
  # AUC to infinity is F x dose / CL: 0.290 x 30 mg / 74.03 L/h.
  PK <- getDrugPK("morphine", 70, 170, 40, "male")$PK$default
  expect_equal(0.290 * 30 / (PK$cl1 * 60) * 1000, 117.4, tolerance = 0.01)
})

test_that("the liquid peaks earlier and a third higher, with the same lag and F", {
  cp <- plasma(oralMorphine("mg PO liquid"))
  expect_equal(max(cp$Y) * 1000, 37.9, tolerance = 0.01)
  expect_equal(cp$Time[which.max(cp$Y)], 35, tolerance = 0.05)
  # Nothing is absorbed before the lag shared by both forms.
  expect_true(all(cp$Y[cp$Time < 17.6] == 0))
  # Same bioavailability as the tablet (pilot AUCs within 10%), so the same
  # exposure: the forms differ in rate only.
  PK <- getDrugPK("morphine", 70, 170, 40, "male")
  expect_equal(PK$oralFormulations$liquid$default$bioavailability_PO,
               PK$PK$default$bioavailability_PO)
})

test_that("tablet and liquid doses add, exactly, on their own absorption", {
  # 30 mg tablet at 0 and 10 mg liquid at 240, against the closed-form oral
  # curve of each built from its own PK set's coefficients, at every point the
  # simulation reports.
  later <- data.frame(Drug = "morphine", Time = 240, Dose = 10,
                      Units = "mg PO liquid")
  cp <- plasma(oralMorphine("mg PO tablet", extra = later))
  PK <- getDrugPK("morphine", 70, 170, 40, "male")
  oral <- function(set, dose, given, t) {
    s <- pmax(t - given - set$tlag_PO, 0)
    dose * (set$p_coef_PO_l1 * exp(-set$lambda_1 * s) +
            set$p_coef_PO_l2 * exp(-set$lambda_2 * s) +
            set$p_coef_PO_l3 * exp(-set$lambda_3 * s) +
            set$p_coef_PO_ka * exp(-set$ka_PO   * s))
  }
  expected <- oral(PK$PK$default, 30, 0, cp$Time) +
    oral(PK$oralFormulations$liquid$default, 10, 240, cp$Time)
  expect_equal(cp$Y, expected, tolerance = 1e-8)
})

test_that("plain mg PO and mg PO tablet are the same input", {
  expect_equal(plasma(oralMorphine("mg PO")), plasma(oralMorphine("mg PO tablet")))
})

test_that("an intravenous dose is unchanged by the oral formulations", {
  # The default run carries every non-oral dose; a liquid alongside must not
  # change what the intravenous dose alone contributes.
  iv <- data.frame(Drug = "morphine", Time = 0, Dose = 10, Units = "mg")
  events <- data.frame(Time = numeric(0), Event = character(0))
  PK <- getDrugPK("morphine", 70, 170, 40, "male")
  alone <- simCpCe(iv, events, PK, 600, plotRecovery = FALSE)
  noLiquid <- PK
  noLiquid$oralFormulations <- NULL
  expect_equal(alone$results, simCpCe(iv, events, noLiquid, 600, FALSE)$results)
})

test_that("a scheduled liquid repeats on the liquid's absorption", {
  sim <- oralMorphine("mg PO liquid qid", maximum = 24 * 60)
  expect_equal(sim$scheduled$Time, c(360, 720, 1080))
  expect_equal(unique(sim$scheduled$Units), "mg PO liquid")
  cp <- plasma(sim)
  first <- cp$Y[which.max(cp$Y[cp$Time < 360])]
  expect_equal(first * 1000, 37.9, tolerance = 0.02)
})

test_that("time until threshold is solved from the summed effect site", {
  later <- data.frame(Drug = "morphine", Time = 60, Dose = 30,
                      Units = "mg PO liquid")
  sim <- oralMorphine("mg PO tablet", extra = later)
  expect_true(any(sim$equiSpace$Recovery > 0, na.rm = TRUE))
  expect_false(is.null(sim$recoveryStates))
})

test_that("mg/kg PO liquid is the liquid scaled by the patient's weight", {
  # Paediatric dosing: 0.2 mg/kg in a 20 kg child is 4 mg of liquid.
  PK <- getDrugPK("morphine", 20, 115, 6, "female")
  events <- data.frame(Time = numeric(0), Event = character(0))
  run <- function(dose, units)
    simCpCe(data.frame(Drug = "morphine", Time = 0, Dose = dose, Units = units),
            events, PK, 12 * 60, plotRecovery = FALSE)$results
  expect_equal(run(0.2, "mg/kg PO liquid"), run(4, "mg PO liquid"))
  units <- getDrugDefaults("morphine")$Units[[1]]
  expect_true(all(c("mg/kg PO liquid", "mg/kg PO liquid qid") %in% units))
})

test_that("the Pregnant field applies the review's factor (R/pregnancy.R)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  CL1 x 1.7.
  # The values are those the model gives with the flag on; test-pregnancy.R
  # checks the ratio against the flag off and that nothing else moves.
  actual <- morphine(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(14.10666343, actual$v1)
  expect_equal_rounded(69.18605273, actual$v2)
  expect_equal_rounded(157.7093109, actual$v3)
  expect_equal_rounded(1.783688789, actual$cl1)
  expect_equal_rounded(1.895701042, actual$cl2)
  expect_equal_rounded(0.2850456403, actual$cl3)   # Q3 by BSA, 1.7710 m2
  expect_equal(morphine(70, 165, 30, "female")$PK$default,
               morphine(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(morphine(70, 165, 30, "female", pregnant = TRUE)$reference, "Ansari", fixed = TRUE)
  expect_no_match(morphine(70, 165, 30, "female")$reference, "Ansari")
})

test_that("the slow intercompartmental clearance follows Lotsch's body-surface-area covariate", {
  # Q3 = 19.5 L/h x (BSA / 1.83)^4, in both switch positions; at 2.2 m2 it is
  # 2.09 times its 1.83 m2 value.  Du Bois BSA for 70 kg and 171 cm is
  # 1.8174 m2 (bsaDuBois(), R/renalFunction.R).
  q3 <- function(...) morphine(...)$PK$default$cl3 * 60
  expect_equal(q3(70, 171, 50, "male", adjustToFFM = FALSE),
               19.5 * (bsaDuBois(70, 171) / 1.83)^4)
  expect_equal(q3(70, 171, 50, "male", adjustToFFM = TRUE),
               q3(70, 171, 50, "male", adjustToFFM = FALSE))
  expect_equal(q3(60, 175, 40, "male") / q3(70, 171, 50, "male"),
               (bsaDuBois(60, 175) / bsaDuBois(70, 171))^4)
  expect_equal((2.2 / 1.83)^4, 2.089, tolerance = 1e-3)
  # Bounded to the adult range, 1.5 to 2.2 m2 (MORPHINE_BSA_RANGE): a 120 kg
  # man (2.28 m2) gets the 2.2 m2 value and a 20 kg child (0.78 m2) the 1.5
  # m2 value, in both switch positions.
  expect_gt(bsaDuBois(120, 170), 2.2)
  expect_equal(q3(120, 170, 50, "male"), 19.5 * (2.2 / 1.83)^4)
  expect_lt(bsaDuBois(20, 110), 1.5)
  expect_equal(q3(20, 110, 5, "male"), 19.5 * (1.5 / 1.83)^4)
  expect_equal(q3(20, 110, 5, "male", adjustToFFM = FALSE), 19.5 * (1.5 / 1.83)^4)
  expect_equal(q3(20, 110, 5, "male"), 8.80, tolerance = 1e-3)
  # The other parameters are absolute with the switch off: the same at 50 kg
  off <- morphine(50, 160, 30, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(unlist(off[c("v1", "v2", "v3", "cl1", "cl2")]),
               c(v1 = 17.5, v2 = 85.82865, v3 = 195.646, cl1 = 74 / 60, cl2 = 133.7 / 60))
})
