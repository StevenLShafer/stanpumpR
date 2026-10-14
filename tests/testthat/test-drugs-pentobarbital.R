# pentobarbital: see R/drugs_pentobarbital.R (J Clin Pharmacol 2026, 2C IV).
test_that("reference 70 kg, switch off", {
  a <- pentobarbital(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 37.4); expect_equal(a$PK$default$v2, 63.9)
  expect_equal(a$PK$default$cl1, 5.21 / 60); expect_equal(a$PK$default$cl2, 18.1 / 60)
})
test_that("allometric scaling, switch on", {
  a <- pentobarbital(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$cl1, 5.21 * (w / 70)^0.75 / 60)
  expect_equal(a$PK$default$v1, 37.4 * (w / 70))
})
