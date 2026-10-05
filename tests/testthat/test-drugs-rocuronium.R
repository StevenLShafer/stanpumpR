test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- rocuronium(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 3.92,
        v2 = 16.06096,
        v3 = 1,
        cl1 = 0.684432,
        cl2 = 0.3934935,
        cl3 = 0
      )
    ),
    tPeak = 2.2,
    MEAC = 0,
    typical = 1.5,
    upperTypical = 2.2,
    lowerTypical = 1,
    reference = "Plaud B et al., Clin Pharmacol Ther 1995;58(2):185-191. https://pubmed.ncbi.nlm.nih.gov/7648768/"
  )

  expect_equal_rounded(actual, expected)
})
