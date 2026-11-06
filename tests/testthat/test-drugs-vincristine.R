# Vincristine (R/drugs_vincristine.R): Igarashi 2021.  Drafted by Claude Code,
# 2026-11-06.

# The typical patient: Mosteller BSA 1.60 m2 at 59 kg, age 60
htTypical <- 1.60^2 * 3600 / 59

test_that("Table 2 at the typical patient", {
  set <- getDrugPK("vincristine", 59, htTypical, 60, "female",
                   adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60, 31.1)
  expect_equal_rounded(set$v1, 13.2)
  expect_equal_rounded(set$cl2 * 60, 60.2)
  expect_equal_rounded(set$v2, 33.9)
  expect_equal_rounded(set$cl3 * 60, 81.1)
  expect_equal_rounded(set$v3, 544)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60)
  expect_equal(round(halfLives, 2), c(0.05, 0.61, 17.56))
})

test_that("the paper's covariate check: 1.7-fold for BSA 1.4 to 1.9, 0.8-fold for age 53 to 82", {
  cl <- function(bsa, age) vincristine(59, bsa^2 * 3600 / 59, age, "male",
                                       adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(round(cl(1.9, 60) / cl(1.4, 60), 3), 1.701)
  expect_equal(round(cl(1.6, 82) / cl(1.6, 53), 3), 0.809)
  expect_equal(cl(1.6, 0), cl(1.6, 18))   # the age floor
})

test_that("2 mg over 10 min: peak about 64 ng/mL and AUC about 64 ng h/mL", {
  d <- data.frame(Drug = "vincristine", Time = c(0, 10), Dose = c(12, 0),
                  Units = "mg/hr")
  wide <- simulateDrugsWithCovariates(
    d, data.frame(Time = double(), Event = character()), 59, htTypical, 60,
    "female", 72 * 60, FALSE, adjustToFFM = FALSE)$vincristine$wide
  expect_equal(max(wide$Plasma), 63.9, tolerance = 0.01)
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2) / 60
  expect_gt(auc, 0.9 * 64.3)                 # 72 h of the infinite AUC
  expect_lt(auc, 64.3)
})

test_that("vincristine is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "vincristine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("vincristine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Igarashi")
  expect_identical(therapeuticRange("vincristine")$band_status, "not_applicable")
})
