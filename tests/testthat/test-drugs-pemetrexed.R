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

test_that("Latz's size-free parameters are anchored at the index set (A2-F4)", {
  # Tables 1 and 2: 66.0 kg, BSA 1.74 m2 (166.7 cm by Du Bois), 57.3 years,
  # 108 men and 101 women.  With the switch on, Q, V2 and the 43.0 mL/min
  # non-renal intercept are the published values divided by the cohort's
  # pooled factors, so that its men and women, pooled geometrically, receive
  # the published values; the Cockcroft-Gault slope is not anchored.  With the
  # switch off they are fixed.
  kc <- cohortFactorsIndependent(PEMETREXED_LATZ_COHORT)
  pk <- function(sex, ffm = TRUE)
    pemetrexed(66, 166.7, 57.3, sex, adjustToFFM = ffm, creatinine = 1)$PK$default
  intercept <- function(sex, ffm = TRUE) {
    w <- if (ffm) pkSizeFactors(66, 166.7, 57.3, sex)$pkWeight else 66
    pk(sex, ffm)$cl1 * 1000 - 47.2 * creatinineClearanceCG(w, 57.3, sex, 1) / 92.6
  }
  for (sex in c("male", "female")) {
    own <- pkSizeFactors(66, 166.7, 57.3, sex)
    on <- pk(sex)
    off <- pk(sex, FALSE)
    expect_equal(off$v2, 3.38, tolerance = 1e-12)
    expect_equal(off$cl2 * 1000, 14.5, tolerance = 1e-12)
    expect_equal(intercept(sex, FALSE), 43, tolerance = 1e-12)
    expect_equal(on$v2, 3.38 * own$volume / kc$volume, tolerance = 1e-6)
    expect_equal(on$cl2 * 1000, 14.5 * own$clearance / kc$clearance, tolerance = 1e-6)
    expect_equal(intercept(sex), 43 * own$clearance / kc$clearance, tolerance = 1e-6)
  }
  pooled <- function(f) exp((108 * log(f("male")) + 101 * log(f("female"))) / 209)
  expect_equal(pooled(function(s) pk(s)$v2), 3.38, tolerance = 1e-6)
  expect_equal(pooled(function(s) pk(s)$cl2 * 1000), 14.5, tolerance = 1e-6)
  expect_equal(pooled(intercept), 43, tolerance = 1e-6)
})
