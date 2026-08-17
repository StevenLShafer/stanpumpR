test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- oxycodone(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 90.2,
        v2 = 68.9,
        v3 = 1,
        cl1 = 0.6233333,
        cl2 = 3.433333,
        cl3 = 0,
        ka_PO = 0.06,
        bioavailability_PO = 0.5,
        tlag_PO = 0
      )
    ),
    tPeak = 60,
    MEAC = 12,
    typical = 14.4,
    upperTypical = 9.6,
    lowerTypical = 24,
    reference = "Lamminsalo M et al., Expert Opin Drug Deliv 2019;16(6):649-656. https://pubmed.ncbi.nlm.nih.gov/31092024/"
  )
  expect_equal_rounded(actual, expected)
})
