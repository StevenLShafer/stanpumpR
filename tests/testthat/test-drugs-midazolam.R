test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- midazolam(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 3.3,
        v2 = 17.56348,
        v3 = 96.75715,
        cl1 = 0.5351973,
        cl2 = 2.014531,
        cl3 = 0.8321115
      )
    ),
    tPeak = 4,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.04,
    lowerTypical = 0.12,
    reference = paste0(
      "Buhrer M et al., Clin Pharmacol Ther 1990;48(5):544-554, the set ",
      "tabulated by Zomorodi K et al., Anesthesiology 1998;89(6):1418-1429 ",
      "(Table 3), who used it for target-controlled infusion. ",
      "https://pubmed.ncbi.nlm.nih.gov/2225713/"
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("matches the Buhrer set as Zomorodi 1998 tabulates it", {
  # Zomorodi K et al., Anesthesiology 1998;89:1418-1429, Table 3, column
  # "Buhrer" (Buhrer M et al., Clin Pharmacol Ther 1990;48:544-554): the
  # parameters that drove the STANPUMP midazolam TCI in that study.  Each value
  # is compared at the precision the table prints.
  pk <- midazolam(70, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(c(pk$v1, pk$v2, pk$v3), 2), c(3.3, 17.56, 96.76))
  expect_equal(round(c(pk$cl1, pk$cl2, pk$cl3), 2), c(0.54, 2.01, 0.83))

  # Exponents and fractional coefficients, from the rate-constant matrix
  # rather than from getDrugPK().
  k10 <- pk$cl1 / pk$v1
  k12 <- pk$cl2 / pk$v1
  k13 <- pk$cl3 / pk$v1
  k21 <- pk$cl2 / pk$v2
  k31 <- pk$cl3 / pk$v3
  K <- rbind(c(-(k10 + k12 + k13), k21, k31),
             c(k12, -k21, 0),
             c(k13, 0, -k31))
  lambda <- sort(-eigen(K)$values, decreasing = TRUE)
  fraction <- sapply(1:3, function(i) {
    other <- lambda[-i]
    (k21 - lambda[i]) * (k31 - lambda[i]) /
      ((other[1] - lambda[i]) * (other[2] - lambda[i]))
  })
  # Table 3 prints alpha as 1.097; the set as entered has 1.098 (below).
  expect_equal(lambda, c(1.097, 0.047, 0.0031), tolerance = 1e-3)
  expect_equal(round(fraction, c(2, 3, 3)), c(0.93, 0.056, 0.013))

  # The stored volumes and clearances are an exact conversion of these round
  # numbers, the form in which the set was entered.
  expect_equal(pk$v1, 3.3)
  expect_equal(c(k21, k31), c(0.1147, 0.0086), tolerance = 1e-6)
  expect_equal(lambda, c(1.098, 0.047, 0.0031), tolerance = 1e-6)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- midazolam(120, 170, 50, "male")
  expected <- list(
        v1 = 4.3061919,
        v2 = 22.918702,
        v3 = 126.25905,
        cl1 = 0.65342914,
        cl2 = 2.4595663,
        cl3 = 1.0159354
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
