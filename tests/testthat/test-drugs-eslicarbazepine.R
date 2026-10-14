# eslicarbazepine: see R/drugs_eslicarbazepine.R (Falcao 2012 + poster).
test_that("reference patient, switch off", {
  a <- eslicarbazepine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 61.3)
  expect_equal(a$PK$default$cl1, 2.43 / 60)
  expect_equal(a$PK$default$ka_PO, 2.34 / 60)
  expect_equal(a$PK$default$bioavailability_PO, 254.3 / 296.3, tolerance = 1e-8)
})
test_that("allometric scaling, switch on", {
  a <- eslicarbazepine(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$cl1, 2.43 * (w / 70)^0.75 / 60)
})
