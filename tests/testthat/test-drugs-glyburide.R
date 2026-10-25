# glyburide: see R/drugs_glyburide.R (Rambiritch 2016).
test_that("Rambiritch's parameters, fixed with the switch off", {
  pk <- glyburide(95, 170, 54, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(c(pk$v1, pk$v2), c(11.70, 68.1))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(2.16, 3.84))
  expect_equal(pk$ka_PO, 0.53 / 60)
  size <- pkSizeFactors(95, 170, 54, "female", TRUE, legacyVolume = 1)
  on <- glyburide(95, 170, 54, "female")$PK$default
  expect_equal(on$v1, 11.70 * size$volume)
  expect_equal(on$cl2, 3.84 * size$clearance / 60)
})

test_that("pregnancy: CL/F x 1.9 and both apparent volumes x 1.57, Q/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.9, Vc/F and Vp/F x 1.57
  # (Hebert 2009); Q/F and absorption unchanged (R/drugs_glyburide.R).
  on  <- getDrugPK("glyburide", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("glyburide", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("glyburide", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, GLYBURIDE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(a$v1 / b$v1, GLYBURIDE_PREGNANCY_V_FACTOR, tolerance = 1e-6)
  expect_equal(a$v2 / b$v2, GLYBURIDE_PREGNANCY_V_FACTOR, tolerance = 1e-6)
  same <- c("v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  expect_match(on$reference, "Hebert", fixed = TRUE)
  expect_no_match(off$reference, "Hebert", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- glyburide(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(14.80716003, actual$v1)
  expect_equal_rounded(86.1852648, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(0.05818965653, actual$cl1)
  expect_equal_rounded(0.05444646225, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})
