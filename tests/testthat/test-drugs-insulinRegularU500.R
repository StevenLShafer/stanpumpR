# insulinRegularU500: see R/insulinTham.R (Tham 2017, Table 2).
test_that("U-500 shares U-100's disposition and absorbs at 0.185/h, whatever the dose", {
  a <- insulinRegularU500(80, 175, 40, "male", FALSE, dose = 75)$PK$default
  b <- insulinRegular(80, 175, 40, "male", FALSE, dose = 75)$PK$default
  expect_equal(a$v1, b$v1); expect_equal(a$cl1, b$cl1)
  expect_equal(a$cl1, 127 * 7.5^-0.282 / 60)   # the study's 75 U: CL/F 72 L/h
  expect_equal(a$ka_SC, 0.185 / 60)            # no dose effect on U-500's ka
  expect_false(any(getDrugDefaults("insulinRegularU500")$Units[[1]] %in% insulinRateUnits))
})
