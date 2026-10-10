test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- morphine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 17.5,
        v2 = 85.82865,
        v3 = 195.646,
        cl1 = 1.233848,
        cl2 = 2.228464,
        cl3 = 0.3195225,
        ka_PO = 0.013,
        bioavailability_PO = 0.29,
        tlag_PO = 17.6
      )
    ),
    tPeak = 93.8,
    MEAC = 0.008,
    typical = 0.0096,
    upperTypical = 0.0064,
    lowerTypical = 0.016,
    reference = paste0(
      "Lotsch J et al., Clin Pharmacol Ther 2002;72(2):151-162. ",
      "https://pubmed.ncbi.nlm.nih.gov/12189362/ (intravenous); ",
      "Atrux-Tallau N et al., Clin Drug Investig 2022;42:1101-1112. ",
      "https://pubmed.ncbi.nlm.nih.gov/36331670/ (oral tablet and liquid, ",
      "calibrated to fasted Cmax, tmax and AUC)"
    ),
    oralFormulations = list(
      liquid = list(ka_PO = 0.01798, bioavailability_PO = 0.301, tlag_PO = 17.6)
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- morphine(120, 170, 50, "male")
  expected <- list(
        v1 = 22.835866,
        v2 = 111.99838,
        v3 = 255.29977,
        cl1 = 1.5064206,
        cl2 = 2.7207598,
        cl3 = 0.39010905
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

# Oral tablet and liquid, against the source they were calibrated to:
# Atrux-Tallau 2022, 30 mg morphine sulfate in fasted adults.  Tablet Cmax
# 28.5 ng/mL at a median 0.75 h, AUC 117.4 ng.h/mL; solution Cmax 37.9 ng/mL,
# AUC 121.8 ng.h/mL.  Simulated through the app's own path, getDrugPK() and
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

test_that("the liquid peaks earlier and a third higher, with the same lag", {
  cp <- plasma(oralMorphine("mg PO liquid"))
  expect_equal(max(cp$Y) * 1000, 37.9, tolerance = 0.01)
  expect_equal(cp$Time[which.max(cp$Y)], 36, tolerance = 0.05)
  # Nothing is absorbed before the lag shared by both forms.
  expect_true(all(cp$Y[cp$Time < 17.6] == 0))
  PK <- getDrugPK("morphine", 70, 170, 40, "male")$PK$default
  expect_equal(0.301 * 30 / (PK$cl1 * 60) * 1000, 121.8, tolerance = 0.03)
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
