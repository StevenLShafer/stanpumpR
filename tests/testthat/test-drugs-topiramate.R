# topiramate: see R/drugs_topiramate.R (Bamgboye 2026, 3C IV; F 1).
test_that("reference 70 kg, switch off", {
  a <- topiramate(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 9.84); expect_equal(a$PK$default$v2, 39.1)
  expect_equal(a$PK$default$v3, 9.01)
  expect_equal(a$PK$default$cl1, 1.31 / 60); expect_equal(a$PK$default$cl2, 197 / 60)
  expect_equal(a$PK$default$cl3, 0.6 / 60)
  expect_equal(a$PK$default$bioavailability_PO, 1)
})
test_that("allometric scaling, switch on", {
  a <- topiramate(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$cl1, 1.31 * (w / 70)^0.75 / 60)
  expect_equal(a$PK$default$v2, 39.1 * (w / 70))
})

test_that("pregnancy: elimination clearance x 1.6, nothing else", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_topiramate.R (Ohman 2009, Westin 2009).
  on  <- getDrugPK("topiramate", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("topiramate", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("topiramate", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               TOPIRAMATE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(TOPIRAMATE_PREGNANCY_CL_FACTOR, 1.6)
  # The distribution clearances are not changed
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  expect_equal_rounded(7.931975323, on$PK$default$v1)
  expect_equal_rounded(0.02971869398, on$PK$default$cl1)
  expect_match(on$reference, "Ohman")
  expect_no_match(off$reference, "Ohman")
})
