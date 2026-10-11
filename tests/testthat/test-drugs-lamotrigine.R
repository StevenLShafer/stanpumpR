# lamotrigine: see R/drugs_lamotrigine.R (Milosheska 2016 base model).
test_that("reference patient, switch off", {
  a <- lamotrigine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 77.6)
  expect_equal(a$PK$default$cl1, 2.32 / 60)
  expect_equal(a$PK$default$ka_PO, 1.96 / 60)
})
test_that("FFM scaling, switch on", {
  a <- lamotrigine(120, 170, 50, "male", adjustToFFM = TRUE)
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$PK$default$v1, 77.6 * s$volume)
  expect_equal(a$PK$default$cl1, 2.32 * s$clearance / 60)
})
