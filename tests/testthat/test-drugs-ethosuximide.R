# ethosuximide: see R/drugs_ethosuximide.R (Diezi 2023, 2C; FFM in place of
# the source's linear weight term).
test_that("reference patient, switch off", {
  a <- ethosuximide(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 31.3); expect_equal(a$PK$default$v2, 13.9)
  expect_equal(a$PK$default$cl1, 0.569 / 60); expect_equal(a$PK$default$cl2, 10.2 / 60)
  expect_equal(a$PK$default$ka_PO, 5.59 / 60)
})
test_that("FFM scaling, switch on", {
  a <- ethosuximide(120, 170, 50, "male", adjustToFFM = TRUE)
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$PK$default$v1, 31.3 * s$volume)
  expect_equal(a$PK$default$cl1, 0.569 * s$clearance / 60)
})
