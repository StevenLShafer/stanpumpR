test_that("returns the correct calculations if weight is > 1", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- oxytocin(weight, height, age, sex)

  expected <- list(
    PK = list(default = list(
      v1 = 10.1,
      v2 = 7.03,
      v3 = 1,
      cl1 = 0.974,
      cl2 = 0.204,
      cl3 = 0
    )),
    tPeak = 5,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.2,
    lowerTypical = 0.05,
    reference = "Eisenach (unpublished data)"
  )
  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations if weight is <= 1", {
  # oxytocin() branches on weight alone (weight > 1 = human, else rat); height,
  # age and sex are accepted for signature consistency but unused. A sub-kilogram
  # weight is therefore what selects the rat parameter set.
  weight <- 0.9
  height <- 171
  age <- 50
  sex <- "male"
  actual <- oxytocin(weight, height, age, sex)

  expected <- list(
    PK = list(default = list(
      v1 = 0.10,
      v2 = 0.02,
      v3 = 1,
      cl1 = 0.017,
      cl2 = 0.000524,
      cl3 = 0
    )),
    tPeak = 5,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.2,
    lowerTypical = 0.05,
    reference = paste0(
      "Tanaka A et al. Delivery of oxytocin to the brain for the treatment of ",
      "autism spectrum disorder by nasal application. Mol Pharm ",
      "2018;15(3):1105-1111. PMID 29338251."
    )
  )
  expect_equal_rounded(actual, expected)
})
