# carbamazepine: see R/drugs_carbamazepine.R (Graves 1998, chronic state).
test_that("reference patient, switch off", {
  a <- carbamazepine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 1.97 * 70)
  expect_equal(a$PK$default$cl1, (0.0134 * 70 + 3.58) / 60)
  expect_equal(a$PK$default$ka_PO, 0.441 / 60)
  expect_equal(a$oralFormulations$ER$bioavailability_PO, 0.89)
})
test_that("age 70 and over lowers clearance", {
  young <- carbamazepine(70, 170, 40, "male", adjustToFFM = FALSE)
  old   <- carbamazepine(70, 170, 75, "male", adjustToFFM = FALSE)
  expect_equal(old$PK$default$cl1, young$PK$default$cl1 * 0.749)
})
