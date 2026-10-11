# zonisamide: see R/drugs_zonisamide.R (Silva 2025).
test_that("reference patient, switch off", {
  a <- zonisamide(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 48.10)
  expect_equal(a$PK$default$cl1, 0.761 / 60)
  expect_equal(a$PK$default$ka_PO, 0.671 / 60)
})
test_that("FFM scaling, switch on", {
  a <- zonisamide(120, 170, 50, "male", adjustToFFM = TRUE)
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$PK$default$cl1, 0.761 * s$clearance / 60)
})
