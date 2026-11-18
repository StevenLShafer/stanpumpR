test_that("returns the published parameters with the switch off", {
  actual <- bupivacaine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 33,
        v2 = 35,
        v3 = 1,
        cl1 = 0.52,
        cl2 = 0.3208219829,   # reconstructed Q, from the research handoff
        cl3 = 0,
        ka_RA = 0.011958,
        bioavailability_RA = 1,
        tlag_RA = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = bupivacaine(70, 171, 50, "male")$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # volumes x 1.3049067, clearances x 1.2209126 (docs/adding-a-drug.md),
  # divided by Burm's cohort factors (BUPIVACAINE_COHORT)
  kc <- cohortFactorsIndependent(BUPIVACAINE_COHORT)
  expect_equal(kc$volume, 1.079346, tolerance = 1e-6)
  expect_equal(kc$clearance, 1.058938, tolerance = 1e-6)
  actual <- bupivacaine(120, 170, 50, "male")
  expected <- list(v1 = 43.0619211 / kc$volume, v2 = 45.6717345 / kc$volume, v3 = 1,
                   cl1 = 0.634874552 / kc$clearance, cl2 = 0.3916956013 / kc$clearance,
                   cl3 = 0, ka_RA = 0.011958)
  expect_equal(actual$PK$default[names(expected)], expected, tolerance = 1e-7)
})

test_that("ka_RA reproduces Robison's 1.66 mcg/mL peak in that study's typical patient", {
  # Robison 1991's plain group (62 kg, 65 y, 5 men at 170.7 cm, 6 women at
  # 157.4 cm, the UK's heights from NCD-RisC) has the anchored disposition
  # scaled by its cohort factors over Burm's; 168.75 mg peaks at 1.66 mcg/mL.
  p <- bupivacaine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  kr <- cohortFactorsIndependent(BUPIVACAINE_KA_COHORT)
  kb <- cohortFactorsIndependent(BUPIVACAINE_COHORT)
  set <- p
  for (n in c("v1", "v2")) set[[n]] <- p[[n]] * kr$volume / kb$volume
  for (n in c("cl1", "cl2")) set[[n]] <- p[[n]] * kr$clearance / kb$clearance
  peak <- depotPeakReference(set, 168.75, p$ka_RA)
  expect_equal(peak[["peak"]], 1.66, tolerance = 1e-4)
  expect_equal(peak[["time"]], 61, tolerance = 5e-3)
})

test_that("keeps the intravenous terminal half-life and Vss", {
  # The published values, which the switch off gives everyone
  PK <- getDrugPK("bupivacaine", 70, 170, 35, "male",
                  getDrugDefaultsGlobal()[getDrugDefaultsGlobal()$Drug == "bupivacaine", ],
                  adjustToFFM = FALSE)
  s <- PK$PK$default
  expect_equal(log(2) / min(s$lambda_1, s$lambda_2), 143, tolerance = 1e-8)
  expect_equal(s$v1 + s$v2, 68, tolerance = 1e-8)
  expect_equal(s$ke0, 0)
})
