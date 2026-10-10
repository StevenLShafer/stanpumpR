test_that("returns the published Vega-Villa parameters with the switch off", {
  # The switch off uses the absolute published values, unscaled.
  actual <- vecuronium(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 10.25,
        v2 = 9.7,
        v3 = 43.79,
        cl1 = 0.51,
        cl2 = 0.66,
        cl3 = 0.095
      )
    ),
    tPeak = 5.9,
    MEAC = 0,
    typical = 130,
    upperTypical = 155,
    lowerTypical = 100,
    reference = paste(
      "Vega-Villa KR et al., Br J Anaesth 2014;112(2):304-310 (non-burn",
      "reference values). https://pubmed.ncbi.nlm.nih.gov/24067332/ Effect",
      "site from Heier T et al., Anesth Analg 1994;78(5):973-977.",
      "https://pubmed.ncbi.nlm.nih.gov/7909212/"
    )
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the published values by hand).
  actual <- vecuronium(120, 170, 50, "male")
  expected <- list(
    v1 = 13.3752937,
    v2 = 12.657595,
    v3 = 57.1418644,
    cl1 = 0.62266543,
    cl2 = 0.80580232,
    cl3 = 0.1159867
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the band and threshold match the CSV and Heier's potency", {
  dd <- getDrugDefaults("vecuronium")
  ref <- vecuronium(70, 170, 40, "male")
  expect_equal(dd$Lower, ref$lowerTypical)
  expect_equal(dd$Upper, ref$upperTypical)
  expect_equal(dd$Typical, ref$typical)
  # Heier 1994: Css50 79 ng/mL, gamma 4.4.  T1 25% recovery = 75% block.
  expect_equal(dd$endCe, round(79 * 3^(1 / 4.4), -1))
})
