input <- list(plasmaLinetype="dashed",effectsiteLinetype="solid")

expect_linetype <- function(normalization, plasmaLinetype, effectsiteLinetype) {
  actual <- setLinetypes(normalization, input$plasmaLinetype, input$effectsiteLinetype)
  expected <- list(plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype)
  expect_equal(actual, expected)
}

test_that("returns the correct value for Peak plasma", {
  expect_linetype("Peak plasma", input$plasmaLinetype, "blank")
})

test_that("returns the correct value for Peak effect site", {
  expect_linetype("Peak effect site", "blank", input$effectsiteLinetype)
})

test_that("returns the correct value for MEAC", {
  expect_linetype("MEAC", "blank", input$effectsiteLinetype)
})

test_that("returns the correct value for none", {
  expect_linetype("none", input$plasmaLinetype, input$effectsiteLinetype)
})

#test_that("returns the correct value for empty parameters", {
#  actual <- setLinetypes()
#  expected <- list(plasmaLinetype = "blank", effectsiteLinetype = "solid")
#  expect_equal(actual, expected)
#})

test_that("returns the correct value for unknown", {
  expect_linetype("unknown", "blank", "solid")
})
