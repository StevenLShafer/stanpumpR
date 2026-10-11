# levetiracetam: see R/drugs_levetiracetam.R (Rhee 2017 + renal split).
test_that("reference patient has total clearance 3.9 L/h", {
  a <- levetiracetam(70, 40, 40, "male", adjustToFFM = FALSE)
  # at the reference creatinine clearance, renal + non-renal = 3.9 L/h
  expect_equal(a$PK$default$cl1, 3.9 / 60, tolerance = 1e-8)
  expect_equal(a$PK$default$v1, 65.3)
  expect_equal(a$PK$default$ka_PO, 2.44 / 60)
})
test_that("clearance falls with renal function", {
  good <- levetiracetam(70, 170, 40, "male", creatinine = 0.8, adjustToFFM = FALSE)
  poor <- levetiracetam(70, 170, 40, "male", creatinine = 4.0, adjustToFFM = FALSE)
  expect_lt(poor$PK$default$cl1, good$PK$default$cl1)
  # non-renal floor: 34% of 3.9 at the reference remains even at very low CrCl
})
