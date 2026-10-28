test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off gives the refit to Domino's Table I at 70 kg: V1 0.0583
  # L/kg with fixed rate constants (see the drug file).
  actual <- ketamine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.081,
        v2 = 39.662973,
        v3 = 171.91943,
        cl1 = 0.922306,
        cl2 = 2.681217,
        cl3 = 0.840686
      )
    ),
    tPeak = 3,
    MEAC = 0,
    typical = 0.12,
    upperTypical = 0.16,
    lowerTypical = 0.1,
    reference = "Domino EF et al., Clin Pharmacol Ther 1984;36(5):645-653. https://pubmed.ncbi.nlm.nih.gov/6488686/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- ketamine(120, 170, 50, "male")
  expected <- list(
        v1 = 5.325324,
        v2 = 51.75648,
        v3 = 224.33881,
        cl1 = 1.126055,
        cl2 = 3.273532,
        cl3 = 1.026404
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the model reproduces Domino's Table I concentrations", {
  # Domino 1984, Table I, placebo (n = 8): mean plasma ketamine, ng/mL, after
  # 2.2 mg/kg over 1 min.  The parameters are a fit to these points (refit by
  # decision of Steven L. Shafer, 2026-10-12); the two earlier parameter sets
  # had root-mean-square log errors of 1.54 and 0.96 against them.
  obs <- data.frame(
    t = c(1, 2, 3, 4, 5, 10, 20, 30, 60, 120, 240, 300, 360, 480, 720, 1440),
    c = c(27600, 6370, 3600, 2280, 1910, 1270, 826, 610, 372, 288, 158, 116,
          86.2, 45.9, 30.4, 8.14))
  s <- ketamine(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  K <- matrix(c(-(s$cl1 + s$cl2 + s$cl3) / s$v1, s$cl2 / s$v2, s$cl3 / s$v3,
                s$cl2 / s$v1, -s$cl2 / s$v2, 0,
                s$cl3 / s$v1, 0, -s$cl3 / s$v3), 3, 3, byrow = TRUE)
  e <- eigen(K)
  a <- Re(e$vectors[1, ] * solve(e$vectors)[, 1]) / s$v1   # unit-bolus, 1/L
  l <- -Re(e$values)
  rate <- 2.2 * 70 * 1000   # ug/min for 1 min; ug/L = ng/mL
  pred <- sapply(obs$t, function(t)
    sum(rate * a / l * (1 - exp(-l * min(t, 1))) * exp(-l * max(t - 1, 0))))
  err <- log(pred / obs$c)
  expect_lt(sqrt(mean(err^2)), 0.22)
  expect_true(all(abs(pred / obs$c - 1) < 0.5))
  # Clearance close to Domino's measured 0.848 L/kg/h
  expect_equal(s$cl1 * 60 / 70, 0.79, tolerance = 0.01)
  # Half-lives about 0.61, 25 and 288 min
  expect_equal(sort(log(2) / l), c(0.61, 25.3, 288), tolerance = 0.02)
})
