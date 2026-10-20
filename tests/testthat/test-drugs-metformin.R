# metformin: see R/drugs_metformin.R (Chae 2012).
test_that("CL/F 52.6 L/h x (Cockcroft-Gault/106.5)^0.782, switch off", {
  pk <- metformin(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  crcl <- creatinineClearanceCG(70, 40, "male", adultEquivalentCreatinine(NULL, 40, "male"))
  expect_equal(pk$cl1, 52.6 * (crcl / 106.5)^0.782 / 60)
  expect_equal(pk$v1, 113)
  expect_equal(pk$ka_PO, 0.41 / 60)
})
test_that("a higher creatinine lowers clearance; switch on uses the pharmacokinetic weight", {
  a <- metformin(70, 170, 40, "male", creatinine = 0.9)$PK$default$cl1
  b <- metformin(70, 170, 40, "male", creatinine = 2.0)$PK$default$cl1
  expect_lt(b, a)
  size <- pkSizeFactors(100, 170, 40, "male", TRUE, legacyVolume = 1)
  on <- metformin(100, 170, 40, "male")$PK$default
  crcl <- creatinineClearanceCG(size$pkWeight, 40, "male", adultEquivalentCreatinine(NULL, 40, "male"))
  expect_equal(on$cl1, 52.6 * (crcl / 106.5)^0.782 / 60)
  expect_equal(on$v1, 113 * size$volume)
})

test_that("pregnancy: CL/F x 1.27 and V/F x 1.33, renal term not raised", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.27 and V/F x 1.33 (Liao
  # 2020); the Cockcroft-Gault term is NOT also raised, so the factor is the
  # same with a measured creatinine (R/drugs_metformin.R, PREGNANCY).
  on  <- getDrugPK("metformin", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("metformin", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("metformin", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, METFORMIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(a$v1 / b$v1, METFORMIN_PREGNANCY_V_FACTOR, tolerance = 1e-6)
  same <- c("v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  cr <- function(p) getDrugPK("metformin", 70, 165, 30, SEX_FEMALE, pregnant = p,
                              creatinine = 0.6)$PK[[1]]
  expect_equal(cr(TRUE)$cl1 / cr(FALSE)$cl1, METFORMIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_match(on$reference, "Liao", fixed = TRUE)
  expect_no_match(off$reference, "Liao", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- metformin(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(121.1480255, actual$v1)
  expect_equal_rounded(1, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(0.9895456604, actual$cl1)
  expect_equal_rounded(0, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})
