# insulinNPH: see R/insulinTham.R (Tham 2017, Table 2).
test_that("NPH absorbs at 0.129/h after a 0.378 h lag on the human-insulin disposition", {
  pk <- insulinNPH(70, 170, 35, "male", FALSE)$PK$default
  expect_equal(pk$ka_SC, 0.129 / 60)
  expect_equal(pk$tlag_SC, 0.378 * 60)
  expect_equal(pk$v1, 178); expect_equal(pk$cl1, 127 / 60)
})
