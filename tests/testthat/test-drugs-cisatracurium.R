test_that("returns Sorooshian's young-adult parameters with the switch off", {
  # The switch off uses the absolute published values, unscaled.  V2 is
  # Vss 9.7 - V1 4.76; Q 0.297 L/min is derived from the half-lives.
  actual <- cisatracurium(70, 171, 40, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.76,
        v2 = 4.94,
        v3 = 1,
        cl1 = 0.319,
        cl2 = 0.297,
        cl3 = 0
      )
    ),
    tPeak = 12.29,
    MEAC = 0,
    typical = 165,
    upperTypical = 200,
    lowerTypical = 130,
    reference = paste(
      "Sorooshian SS et al., Anesthesiology 1996;84(5):1083-1091; distribution",
      "clearance derived from its half-lives.",
      "https://pubmed.ncbi.nlm.nih.gov/8624002/"
    )
  )

  expect_equal_rounded(actual, expected)
})

test_that("the elderly have a 37% larger V2 and a later tPeak, interpolated from 50 to 65", {
  old <- cisatracurium(70, 170, 75, "male", adjustToFFM = FALSE)
  expect_equal_rounded(old$PK$default$v2, 6.7678)
  expect_equal_rounded(old$tPeak, 12.91)
  expect_equal_rounded(old$PK$default$cl1, 0.319)

  mid <- cisatracurium(70, 170, 57.5, "male", adjustToFFM = FALSE)
  expect_equal_rounded(mid$PK$default$v2, 5.8539)
  expect_equal_rounded(mid$tPeak, 12.6)

  # Below 50 there is no age effect (switch off: fat-free mass varies with age)
  expect_identical(cisatracurium(70, 170, 50, "male", adjustToFFM = FALSE)$PK,
                   cisatracurium(70, 170, 18, "male", adjustToFFM = FALSE)$PK)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the published values by hand).  v3 and cl3 are
  # placeholders for a missing compartment and are left alone.
  actual <- cisatracurium(120, 170, 50, "male")
  expected <- list(
    v1 = 6.2113559,
    v2 = 6.4462391,
    v3 = 1,
    cl1 = 0.38947112,
    cl2 = 0.36261104,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the derived distribution clearance reproduces the published half-lives", {
  # Sorooshian Table 2: t1/2 alpha 4.20 / 4.50 min, beta 28.4 / 36.3 min.
  halfLives <- function(p) {
    k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
    s <- k10 + k12 + k21
    d <- sqrt(s^2 - 4 * k10 * k21)
    log(2) / c((s + d) / 2, (s - d) / 2)
  }
  young <- halfLives(cisatracurium(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default)
  old <- halfLives(cisatracurium(70, 170, 75, "male", adjustToFFM = FALSE)$PK$default)
  expect_equal(young, c(4.20, 28.4), tolerance = 0.01)
  expect_equal(old, c(4.50, 36.3), tolerance = 0.01)
})

test_that("getDrugPK recovers Sorooshian's ke0 from the tPeak", {
  dd <- getDrugDefaults("cisatracurium")
  young <- getDrugPK("cisatracurium", 70, 170, 40, "male", dd, adjustToFFM = FALSE)
  old <- getDrugPK("cisatracurium", 70, 170, 75, "male", dd, adjustToFFM = FALSE)
  expect_equal(young$PK[[1]]$ke0, 0.071, tolerance = 0.005)
  expect_equal(old$PK[[1]]$ke0, 0.060, tolerance = 0.005)
})

test_that("the band and threshold match the CSV and Sorooshian's potency", {
  dd <- getDrugDefaults("cisatracurium")
  ref <- cisatracurium(70, 170, 40, "male")
  expect_equal(dd$Lower, ref$lowerTypical)
  expect_equal(dd$Upper, ref$upperTypical)
  expect_equal(dd$Typical, ref$typical)
  # C50 98 ng/mL, gamma 4.1 (young, 70 kg).  T1 25% recovery = 75% block.
  expect_equal(dd$endCe, round(98 * 3^(1 / 4.1), -1))
})
