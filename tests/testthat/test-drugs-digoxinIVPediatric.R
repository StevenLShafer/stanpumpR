# Intravenous digoxin in children (R/drugs_digoxinIVPediatric.R):
# el-Desoky 2005's two compartments with serum creatinine on clearance.
# Drafted by Claude Code, 2026-10-19.

test_that("the parameters are Table 4's final model, per kilogram", {
  set <- digoxinIVPediatric(18, 110, 6, "male", adjustToFFM = FALSE,
                            creatinine = 0.6)$PK$default
  expect_equal_rounded(0.38 * 18 / 60, set$cl1)
  expect_equal_rounded(1.38 * 18, set$v1)
  expect_equal_rounded(0.47 * 18 / 60, set$cl2)
  expect_equal_rounded(9.11 * 18, set$v2)
})

test_that("creatinine lowers clearance linearly, held to the studied 0.2-1.3 mg/dL", {
  at1 <- digoxinIVPediatric(18, 110, 6, "male", FALSE, creatinine = 1.0)$PK$default
  expect_equal_rounded(0.38 * (1 - 0.76 * 0.4) * 18 / 60, at1$cl1)
  high <- digoxinIVPediatric(18, 110, 6, "male", FALSE, creatinine = 3)$PK$default
  expect_equal_rounded(0.38 * (1 - 0.76 * 0.7) * 18 / 60, high$cl1)
  expect_gt(high$cl1, 0)
  low <- digoxinIVPediatric(18, 110, 6, "male", FALSE, creatinine = 0.1)$PK$default
  expect_equal_rounded(0.38 * (1 + 0.76 * 0.4) * 18 / 60, low$cl1)
})

test_that("a blank creatinine is the assumed normal value for age", {
  blank <- digoxinIVPediatric(18, 110, 6, "male", FALSE)$PK$default
  given <- digoxinIVPediatric(18, 110, 6, "male", FALSE,
                              creatinine = assumedCreatinine(6, "male"))$PK$default
  expect_equal(blank$cl1, given$cl1)
})

test_that("it is plasma only, with the 0.5-2 ng/mL band", {
  expect_equal(getDrugPK("digoxinIVPediatric", 18, 110, 6, "male")$tPeak, 0)
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "digoxinIVPediatric", ]
  expect_equal(c(row$Lower, row$Upper), c(0.5, 2))
})
