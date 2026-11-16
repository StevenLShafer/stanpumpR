# Pemetrexed (R/drugs_pemetrexed.R): Latz 2006.  Drafted by Claude Code,
# 2026-11-06.

# A man whose Cockcroft-Gault clearance is the median 92.6 mL/min and whose
# Mosteller BSA is the median 1.74 m2
scrMedian <- (140 - 60) * 70 / (72 * 92.6)
htMedian <- 1.74^2 * 3600 / 70

test_that("Table 3 at the medians: CL 90.2 mL/min, terminal half-life 3.5 h, Vss 16.1 L", {
  set <- getDrugPK("pemetrexed", 70, htMedian, 60, "male", adjustToFFM = FALSE,
                   creatinine = scrMedian)$PK$default
  expect_equal(round(set$cl1 * 1000, 1), 90.2)
  expect_equal_rounded(set$v1, 6.13 * 1.74^1.32)
  expect_equal_rounded(set$cl2 * 1000, 14.5)
  expect_equal_rounded(set$v2, 3.38)
  terminal <- log(2) / min(set$lambda_1, set$lambda_2) / 60
  expect_equal(round(terminal, 2), 3.50)
  expect_equal(round(set$v1 + set$v2, 1), 16.1)
})

test_that("clearance is linear in CrCL with a 43 mL/min intercept", {
  cl <- function(scr) pemetrexed(70, 170, 60, "male", adjustToFFM = FALSE,
                                 creatinine = scr)$PK$default$cl1 * 1000
  crcl <- function(scr) (140 - 60) * 70 / (72 * scr)
  expect_equal(cl(1.2), 43 + 47.2 * crcl(1.2) / 92.6)
  expect_equal(cl(3) - 43, (cl(1.5) - 43) / 2)
  # blank creatinine: the library's assumed 0.9 mg/dL
  expect_equal(pemetrexed(70, 170, 60, "male", adjustToFFM = FALSE)$PK$default$cl1,
               cl(0.9) / 1000)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only with a blank creatinine", {
  base <- pemetrexed(70, 165, 30, "female", adjustToFFM = FALSE)$PK$default$cl1 * 1000
  preg <- pemetrexed(70, 165, 30, "female", adjustToFFM = FALSE,
                     pregnant = TRUE)$PK$default$cl1 * 1000
  expect_equal(preg - 43, 1.5 * (base - 43))
  measured <- pemetrexed(70, 165, 30, "female", adjustToFFM = FALSE, creatinine = 0.6,
                         pregnant = TRUE)$PK$default$cl1
  expect_equal(measured, pemetrexed(70, 165, 30, "female", adjustToFFM = FALSE,
                                    creatinine = 0.6)$PK$default$cl1)
})

test_that("pemetrexed is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "pemetrexed", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("pemetrexed", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Latz")
  expect_identical(therapeuticRange("pemetrexed")$band_status, "not_applicable")
})
