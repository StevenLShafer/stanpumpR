# insulinRegularU500: see R/insulinTham.R (Tham 2017, Table 2).
test_that("U-500 shares U-100's disposition and absorbs at 0.185/h", {
  a <- insulinRegularU500(70, 170, 35, "male", FALSE)$PK$default
  b <- insulinRegular(70, 170, 35, "male", FALSE)$PK$default
  expect_equal(a$v1, b$v1); expect_equal(a$cl1, b$cl1)
  expect_equal(a$ka_SC, 0.185 / 60)
  expect_false(any(getDrugDefaults("insulinRegularU500")$Units[[1]] %in% insulinRateUnits))
})
