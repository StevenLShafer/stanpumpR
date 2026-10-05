test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- naloxone(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 28.56,
        v2 = 44.52,
        v3 = 114.59,
        cl1 = 3.43,
        cl2 = 3.22,
        cl3 = 1.82
      )
    ),
    tPeak = 1,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = "Papathanasiou T et al., Br J Anaesth 2019;123(2):e204-e214. https://pubmed.ncbi.nlm.nih.gov/30915992/"
  )
  expect_equal_rounded(actual, expected)
})
