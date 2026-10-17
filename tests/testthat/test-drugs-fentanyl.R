test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- fentanyl(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 12.1,
        v2 = 35.7,
        v3 = 224,
        cl1 = 0.632,
        cl2 = 2.8,
        cl3 = 1.55
      )
    ),
    tPeak = 3.6,
    MEAC = 0.6,
    typical = 0.72,
    upperTypical = 1.2,
    lowerTypical = 0.48,
    reference = paste0(
      "Pooled parameter set from five fentanyl studies; the values are printed ",
      "in none of them. McClain DA, Hug CC. Clin Pharmacol Ther 1980;28:106-114. ",
      "https://pubmed.ncbi.nlm.nih.gov/7389247/ Scott JC, Stanski DR. ",
      "J Pharmacol Exp Ther 1987;240:159-166. ",
      "https://pubmed.ncbi.nlm.nih.gov/3100765/ Hudson RJ et al. ",
      "Anesthesiology 1986;64:334-338. https://pubmed.ncbi.nlm.nih.gov/3954128/ ",
      "Varvel JR et al. Anesthesiology 1989;70:928-934. ",
      "https://pubmed.ncbi.nlm.nih.gov/2729633/ Shafer SL et al. ",
      "Anesthesiology 1990;73:1091-1102. https://pubmed.ncbi.nlm.nih.gov/2248388/"
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- fentanyl(120, 170, 50, "male")
  expected <- list(
        v1 = 15.78937,
        v2 = 46.585167,
        v3 = 292.29909,
        cl1 = 0.77161678,
        cl2 = 3.4185553,
        cl3 = 1.8924146
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
