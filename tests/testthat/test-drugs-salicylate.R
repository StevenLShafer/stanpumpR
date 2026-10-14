# Salicylate, formed from aspirin: Koh 2025 two-compartment disposition.  See
# the header of R/drugs_salicylate.R.

test_that("returns Koh's parameters at the median weight with the switch off", {
  actual <- salicylate(68.35, 171, 30, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 7.5, v2 = 1.98, v3 = 1,
      cl1 = 2.76 / 60, cl2 = 0.08 / 60, cl3 = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("clearance carries its own weight term, the volumes the library's", {
  off <- salicylate(100, 171, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, 2.76 / 60 * (100 / 68.35)^1.42)
  expect_equal(off$v1, 7.5)
  on <- salicylate(100, 171, 30, "male")$PK$default
  size <- pkSizeFactors(100, 171, 30, "male", TRUE)
  expect_equal(on$cl1, 2.76 / 60 * (size$pkWeight / 68.35)^1.42)
  expect_equal(on$v1, 7.5 * size$volume)
  expect_equal(on$cl2, 0.08 / 60 * size$clearance)
})
