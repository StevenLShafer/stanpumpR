test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- ketamine(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.41,
        v2 = 10.56972,
        v3 = 178.2123,
        cl1 = 1.93158,
        cl2 = 2.61072,
        cl3 = 2.6019

      )
    ),
    tPeak = 3,
    MEAC = 0,
    typical = 0.12,
    upperTypical = 0.1,
    lowerTypical = 0.16,
    reference = "Domino EF et al., Clin Pharmacol Ther 1984;36(5):645-653. https://pubmed.ncbi.nlm.nih.gov/6488686/"
  )
  expect_equal_rounded(actual, expected)
})
