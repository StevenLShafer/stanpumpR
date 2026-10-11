# tiagabine: see R/drugs_tiagabine.R (Ingwersen 2000, monotherapy).
test_that("reference patient, switch off", {
  a <- tiagabine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 62.0)
  expect_equal(a$PK$default$cl1, 6.10 / 60)
  expect_equal(a$PK$default$ka_PO, 1.25 / 60)
})
test_that("FFM scaling, switch on", {
  a <- tiagabine(120, 170, 50, "male", adjustToFFM = TRUE)
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$PK$default$v1, 62.0 * s$volume)
})
