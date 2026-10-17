# glyburide: see R/drugs_glyburide.R (Rambiritch 2016).
test_that("Rambiritch's parameters, fixed with the switch off", {
  pk <- glyburide(95, 170, 54, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(c(pk$v1, pk$v2), c(11.70, 68.1))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(2.16, 3.84))
  expect_equal(pk$ka_PO, 0.53 / 60)
  size <- pkSizeFactors(95, 170, 54, "female", TRUE, legacyVolume = 1)
  on <- glyburide(95, 170, 54, "female")$PK$default
  expect_equal(on$v1, 11.70 * size$volume)
  expect_equal(on$cl2, 3.84 * size$clearance / 60)
})
