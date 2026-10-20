# Phenylephrine (R/drugs_phenylephrine.R): the mean of Hengstmann 1982's four
# intravenous two-compartment fits.  Drafted by Claude Code, 2026-10-19.

k <- PHENYLEPHRINE_HENGSTMANN

test_that("each subject's printed clearance and Vdss follow from his constants", {
  # Table 3: Vdss 426.3, 543.2, 184.1, 206.3 L; CL 2076, 3252, 1474, 1580 mL/min
  expect_lt(max(abs(k$V1 * (1 + k$k12 / k$k21) - c(426.3, 543.2, 184.1, 206.3))), 3)
  expect_lt(max(abs(k$V1 * k$kel * 1000 - c(2076, 3252, 1474, 1580))), 6)
})

test_that("the typical model reproduces the paper's mean clearance and Vdss", {
  p <- phenylephrineTypical()
  expect_lt(abs(p[["cl1"]] * 1000 / 2095 - 1), 0.005)
  expect_lt(abs((p[["v1"]] + p[["v2"]]) / 340 - 1), 0.01)
  expect_equal(round(p[["v1"]], 1), 41.1)   # Table 3 mean V1 41.2
  set <- getDrugPK("phenylephrine", 70, 175, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(p[["cl1"]], set$cl1)
  expect_equal_rounded(p[["v2"]], set$v2)
  # Its exponents match the paper's mean alpha and beta
  expect_lt(abs(set$lambda_1 / 0.1527 - 1), 0.05)
  expect_lt(abs(set$lambda_2 / 0.0046 - 1), 0.10)
  expect_identical(getDrugPK("phenylephrine", 70, 175, 30, "male")$Concentration.Units, "ng")
})

test_that("a 1 mg infusion over 15 minutes ends near the paper's 10 ng/mL", {
  # Table 2: 0.842 mg over 12.5-20 min gave 10.35 +/- 2.68 ng/mL at the end
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "phenylephrine", Time = c(0, 15), Dose = c(0.842 / 15, 0),
               Units = "mg/min"),
    data.frame(Time = double(), Event = character()),
    70, 175, 30, "male", 120, FALSE, adjustToFFM = FALSE)$phenylephrine$wide
  end <- wide$Plasma[wide$Time == 15][1]
  expect_gt(end, 10.35 - 2.68)
  expect_lt(end, 10.35 + 2.68)
})

test_that("phenylephrine is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "phenylephrine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(getDrugPK("phenylephrine", 70, 175, 30, "male")$tPeak, 0)
})
