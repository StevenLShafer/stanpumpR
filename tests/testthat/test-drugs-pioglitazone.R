# pioglitazone: see R/drugs_pioglitazone.R (Kadam 2013, Table 3).
test_that("Table 3's non-carrier values, fixed with the switch off", {
  pk <- pioglitazone(80, 175, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(2.12, 0.349))
  expect_equal(c(pk$v1, pk$v2), c(21.4, 26.3))
  expect_equal(pk$ka_PO * 60, 1.82)
  size <- pkSizeFactors(80, 175, 30, "male", TRUE, legacyVolume = 1)
  on <- pioglitazone(80, 175, 30, "male")$PK$default
  expect_equal(on$v1, 21.4 * size$volume)
  expect_equal(on$cl1, 2.12 * size$clearance / 60)
})
test_that("the abstract's carrier percentages are the same model", {
  expect_equal(1.09 / 2.12, 0.52, tolerance = 0.02)
  expect_equal(1.03 / 0.349, 2.86, tolerance = 0.04)
})
