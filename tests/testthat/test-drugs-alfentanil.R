test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- alfentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 2.1853,
        v2 = 6.698864,
        v3 = 14.52582,
        cl1 = 0.1988623,
        cl2 = 1.433557,
        cl3 = 0.2469389
      )
    ),
    tPeak = 1.4,
    MEAC = 39,
    typical = 46.8,
    upperTypical = 31.2,
    lowerTypical = 78,
    reference = "Scott JC, Stanski DR. J Pharmacol Exp Ther 1987;240(1):159-166. https://pubmed.ncbi.nlm.nih.gov/3100765/"
  )

  expect_equal_rounded(actual, expected)
})
