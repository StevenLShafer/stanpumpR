# Vinorelbine (R/drugs_vinorelbine.R): Variol 2002.  Drafted by Claude Code,
# 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table 3: Vss 1074 L, half-lives 7.0 min, 3.0 h and 36.9 h", {
  set <- getDrugPK("vinorelbine", 70, 170, 57, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60, 41.9)
  expect_equal_rounded(set$v1, 22.2)
  expect_equal(round(set$v1 + set$v2 + set$v3), 1074)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60)
  expect_equal(round(halfLives, c(3, 2, 1)), c(0.117, 3.01, 36.9))
  expect_equal(round(log(2) / (set$ka_PO * 60), 2), 1.44)
  expect_equal(set$bioavailability_PO, 0.36)
  expect_equal(set$tlag_PO, 9)
})

test_that("80 mg/m2 by mouth peaks near 157 ng/mL at 0.69 h; the AUCs follow F and CL", {
  dose <- 80 * 1.74
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "vinorelbine", Time = 0, Dose = dose, Units = "mg PO"),
    noEvents, 70, 170, 57, "male", 21 * 1440, FALSE, adjustToFFM = FALSE)$vinorelbine$wide
  expect_equal(max(w$Plasma), 157, tolerance = 0.02)
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 0.69, tolerance = 0.05)
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  expect_equal(auc, 0.36 * dose * 1000 / 41.9, tolerance = 0.01)
})

test_that("vinorelbine is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "vinorelbine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("vinorelbine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Variol")
  expect_identical(therapeuticRange("vinorelbine")$band_status, "not_applicable")
})

test_that("with the switch on the parameters are anchored at Variol's cohort", {
  # Variol 2002, Table 2: 68 kg, BSA 1.74 m2 (163.8 cm by Du Bois), 57 years,
  # 91 men and 84 women.
  h <- 163.8   # (1.74 / (0.007184 x 68^0.425))^(1 / 0.725), to 0.1 cm
  kc <- cohortFactorsIndependent(list(weight = 68, height = h, age = 57, men = 91, women = 84))
  off <- vinorelbine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  on <- vinorelbine(70, 170, 35, "male")$PK$default
  expect_equal_rounded(on$v1, off$v1 / kc$volume)
  expect_equal_rounded(on$cl1, off$cl1 / kc$clearance)
})
