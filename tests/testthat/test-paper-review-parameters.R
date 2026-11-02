# Steven L. Shafer's Convert.xls: five tabs, each closing a one-, two- or
# three-compartment model from a different set of inputs and listing every
# other parameter.  The examples are the spreadsheet's own, and the values
# pinned are the ones it computes.

convertThree <- list(v1 = 10, v2 = 100, v3 = 1000, cl1 = 3, cl2 = 2, cl3 = 1)
convertTwo <- list(v1 = 5, v2 = 50, cl1 = 3.5, cl2 = 2.5)
convertLambda3 <- c(0.606979569298565, 0.0132758444333374, 0.000744586268097897)
convertLambda2 <- c(1.22134302209383, 0.0286569779061718)

micro <- function(m) {
  k <- c(k10 = m$cl1 / m$v1, k12 = m$cl2 / m$v1, k21 = m$cl2 / m$v2)
  if (!is.null(m$v3)) k <- c(k, k13 = m$cl3 / m$v1, k31 = m$cl3 / m$v3)
  k
}

test_that("tab 3 (V1, Vdss, CL and the half-lives) recovers tab 1's model", {
  m <- threeCompartmentFromHalfLives(3, 10, 1110, 1.1419613041687, 52.2111556850848, 930.915879405947)
  expect_equal(unlist(m[names(convertThree)]), unlist(convertThree), tolerance = 1e-9)
  m <- twoCompartmentFromCentralVolume(3.5, 5, 0.567528669686619, 24.1877277788829)
  expect_equal(unlist(m[names(convertTwo)]), unlist(convertTwo), tolerance = 1e-9)
})

test_that("tab 4 (coefficients and exponents) reproduces the spreadsheet", {
  m <- threeCompartmentFromTriexponential(c(12.2, 3.76, 1.44), c(0.87, 0.12, 0.013))
  expect_equal(m$v1, 1 / 17.4)
  expect_equal(micro(m)[c("k10", "k12", "k13", "k21", "k31")],
               c(k10 = 0.111448765, k12 = 0.371947306, k13 = 0.153610825,
                 k21 = 0.328975813, k31 = 0.03701729), tolerance = 1e-8)
  m <- twoCompartmentFromBiexponential(c(0.196421015874573, 0.00357898412542674), convertLambda2)
  expect_equal(unlist(m[names(convertTwo)]), unlist(convertTwo), tolerance = 1e-9)
})

test_that("tab 5 (V1, the exponents, k21 and k31) reproduces the spreadsheet", {
  m <- compartmentsFromExponentsAndRates(10, convertLambda3, 0.02, 0.001)
  expect_equal(unlist(m[names(convertThree)]), unlist(convertThree), tolerance = 1e-9)
  m <- compartmentsFromExponentsAndRates(5, convertLambda2, 0.05)
  expect_equal(unlist(m[names(convertTwo)]), unlist(convertTwo), tolerance = 1e-9)
  expect_error(compartmentsFromExponentsAndRates(10, convertLambda3, 2, 0.001), "admissible")
})

test_that("every parameter of a model is listed as tab 1 lists it", {
  set <- c(convertThree, as.list(micro(convertThree)),
           list(lambda_1 = convertLambda3[1], lambda_2 = convertLambda3[2], lambda_3 = convertLambda3[3],
                p_coef_bolus_l1 = 0.098825768, p_coef_bolus_l2 = 0.001109493,
                p_coef_bolus_l3 = 6.47383e-05, ke0 = 0))
  t <- paperReviewPKTable(set, "min")
  value <- function(p) t$Value[t$Parameter == p]
  expect_equal(value("Vdss"), "1110")
  expect_equal(value("k13"), "0.1")
  expect_equal(value("t1/2 lambda2"), "52.21")
  expect_equal(value("t1/2 lambda3"), "15.52")           # 931 min, read in hours
  expect_equal(t$Units[t$Parameter == "t1/2 lambda3"], "h")
  expect_equal(value("A1 / sum(A)"), "98.83")
  expect_equal(value("A3 / sum(A)"), "0.06474")
  expect_false("ke0" %in% t$Parameter)
  # In hours, the rates and clearances are 60 times larger.
  expect_equal(paperReviewPKTable(set, "h")$Value[t$Parameter == "k10"], "18")
  # A two-compartment set lists no V3, k13 or k31.
  two <- c(convertTwo, as.list(micro(convertTwo)), list(v3 = 1, cl3 = 0, k13 = 0, k31 = 0,
           lambda_1 = convertLambda2[1], lambda_2 = convertLambda2[2], lambda_3 = 0,
           p_coef_bolus_l1 = 0.196421016, p_coef_bolus_l2 = 0.003578984, p_coef_bolus_l3 = 0))
  t2 <- paperReviewPKTable(two, "min")
  expect_false(any(c("V3", "k13", "k31", "lambda3") %in% t2$Parameter))
  expect_equal(t2$Value[t2$Parameter == "A1 / sum(A)"], "98.21")
})
