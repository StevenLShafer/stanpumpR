context("setLinetypes")

expect_linetype <- function(normalization, plasmaLinetype, effectsiteLinetype) {
  actual <- setLinetypes(normalization)
  expected <- list(plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype)
  expect_equal(actual, expected)
}

test_that("returns the correct value for Peak plasma", {
  expect_linetype("Peak plasma", "solid", "blank")
})

test_that("returns the correct value for Peak effect site", {
  expect_linetype("Peak effect site", "blank", "solid")
})

test_that("returns the correct value for MEAC", {
  expect_linetype("MEAC", "blank", "solid")
})

test_that("returns the correct value for none", {
  expect_linetype("none", "blank", "solid")
})

test_that("returns the correct value for empty parameters", {
  actual <- setLinetypes()
  expected <- list(plasmaLinetype = "blank", effectsiteLinetype = "solid")
  expect_equal(actual, expected)
})

test_that("returns the correct value for unknown", {
  expect_linetype("unknown", "blank", "solid")
})
