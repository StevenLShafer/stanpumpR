# phenobarbital: see R/drugs_phenobarbital.R (Munich 2025, ideal body weight).
test_that("reference patient, switch off (total weight)", {
  a <- phenobarbital(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 34.3 * (70 / 68.8))
  expect_equal(a$PK$default$cl1, 0.38 * (70 / 68.8)^0.75 / 60)
  expect_equal(a$PK$default$bioavailability_PO, 0.96)
  expect_equal(a$PK$default$ka_PO, 1.9 / 60)
})
test_that("adult switch on uses ideal body weight", {
  a <- phenobarbital(70, 178, 40, "male", adjustToFFM = TRUE)
  ibw <- 50 + 2.3 * (178 / 2.54 - 60)   # Devine, shared idealBodyWeightDevine()
  expect_equal(a$PK$default$v1, 34.3 * (ibw / 68.8))
})
test_that("a child falls back to the pharmacokinetic weight", {
  a <- phenobarbital(20, 110, 5, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(20, 110, 5, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$v1, 34.3 * (w / 68.8))
})
