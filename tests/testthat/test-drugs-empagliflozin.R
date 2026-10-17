# empagliflozin: see R/drugs_empagliflozin.R (Baron 2016).
test_that("Baron's reference parameters, with the fixed 0.5 h lag", {
  pk <- empagliflozin(90, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(pk$v1, pk$v2), c(3.14, 70.6))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(10.6, 6.34))
  expect_equal(pk$ka_PO, 0.196 / 60)
  expect_equal(pk$tlag_PO, 30)
  size <- pkSizeFactors(90, 175, 50, "male", TRUE, legacyVolume = 1)
  on <- empagliflozin(90, 175, 50, "male")$PK$default
  expect_equal(on$v2, 70.6 * size$volume)
  expect_equal(on$cl1, 10.6 * size$clearance / 60)
})
