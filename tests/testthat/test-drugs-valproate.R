# valproate: see the header of R/drugs_valproate.R (Teixeira-da-Silva 2022,
# weight only; three oral inputs and IV).  Expected values written out.
test_that("reference patient, switch off", {
  a <- valproate(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 14)
  expect_equal(a$PK$default$cl1, 0.646 / 60)
  expect_equal(a$PK$default$ka_PO, 0.78 / 60)
  expect_equal(a$oralFormulations$liquid$ka_PO, 2.64 / 60)
  expect_equal(a$oralFormulations$ER$ka_PO, 0.38 / 60)
  expect_equal(a$oralFormulations$ER$bioavailability_PO, 0.89)
  expect_equal(a$tPeak, 0); expect_equal(a$MEAC, 0)
})
test_that("weight scaling, switch on", {
  a <- valproate(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$v1, 14 * (w / 70))
  expect_equal(a$PK$default$cl1, 0.646 * (w / 70)^0.75 / 60)
})
