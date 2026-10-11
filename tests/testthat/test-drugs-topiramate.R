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
