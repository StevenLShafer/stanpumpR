test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- lidocaine(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 6.16,
        v2 = 28.00002,
        v3 = 1,
        cl1 = 1.400002,
        cl2 = 3.920002,
        cl3 = 0
      )
    ),
    tPeak = 5,
    MEAC = 0,
    typical = 1,
    upperTypical = 1.5,
    lowerTypical = 0.5,
    reference = "Schnider TW et al., Anesthesiology 1996;84(5):1043-1050. https://pubmed.ncbi.nlm.nih.gov/8623997/"
  )
  expect_equal_rounded(actual, expected)
})
