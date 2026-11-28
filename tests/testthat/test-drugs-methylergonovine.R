# methylergonovine: see R/drugs_methylergonovine.R.  de Groot 1995's
# intravenous values in nonpregnant women, with the label's intramuscular
# bioavailability, and no oral route.  What is protected here is that the model
# still reproduces what was read.

# Rate constants (1/h) and central volume of the unscaled model
methylergonovineMicro <- function(x)
{
  k10 <- x$cl1 / x$v1 * 60
  k12 <- x$cl2 / x$v1 * 60
  k21 <- x$cl2 / x$v2 * 60
  s <- k10 + k12 + k21
  a <- (s + sqrt(s^2 - 4 * k10 * k21)) / 2
  list(a = a, b = k10 * k21 / a, k21 = k21, v1 = x$v1)
}

# The curve after one first-order dose (mcg of maleate; times in hours,
# concentrations in ng/mL), by the closed form, computed independently of the
# engine
methylergonovineCurve <- function(dose, ka, f)
{
  m <- methylergonovineMicro(
    methylergonovine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default)
  co <- f * dose * ka / m$v1 *
    c((m$k21 - m$a) / ((ka - m$a) * (m$b - m$a)),
      (m$k21 - m$b) / ((ka - m$b) * (m$a - m$b)),
      (m$k21 - ka) / ((m$a - ka) * (m$b - ka)))
  lambda <- c(m$a, m$b, ka)
  cp <- function(t) vapply(t, function(u)
    if (u <= 0) 0 else sum(co * exp(-lambda * u)), 0)
  peak <- stats::optimize(cp, c(0, 6), maximum = TRUE, tol = 1e-9)
  list(cmax = peak$objective, tmax = peak$maximum, auc = sum(co / lambda))
}

test_that("methylergonovine reproduces de Groot's intravenous values", {
  # women: CL 22.2 L/h, Vss 50.8 L, half-lives 0.10 and 1.94 h, on the base;
  # 0.152 mg base per 0.2 mg of the labelled maleate
  x <- methylergonovine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60 * 0.76, 22.2)
  expect_equal((x$v1 + x$v2) * 0.76, 50.8)
  m <- methylergonovineMicro(x)
  expect_equal(log(2) / c(m$a, m$b), c(0.10, 1.94))
  expect_equal(c(x$v1, x$v2) * 0.76, c(13.6895, 37.1105), tolerance = 1e-5)
  expect_equal(x$cl2 * 60 * 0.76, 56.6737, tolerance = 1e-5)
  expect_equal(x$v3, 1)
  expect_equal(x$cl3, 0)
})

test_that("methylergonovine returns the absorption values it read", {
  actual <- methylergonovine(70, 170, 35, "male", adjustToFFM = FALSE)
  x <- actual$PK$default
  expect_equal(x$ka_IM, 1.7256 / 60)
  expect_equal(x$bioavailability_IM, 0.78)
  expect_equal(x$tlag_IM, 0)
  expect_equal(
    unlist(actual[c("tPeak", "MEAC", "typical", "upperTypical", "lowerTypical")],
           use.names = FALSE),
    rep(0, 5))
})

test_that("the intramuscular curve peaks at the label's time", {
  # label, 0.2 mg IM: peak at 0.41 h.  Its 5.9 ng/mL is not reproduced.
  im <- methylergonovineCurve(200, 1.7256, 0.78)
  expect_equal(im$tmax, 0.41, tolerance = 0.001)
  expect_equal(im$cmax, 1.746, tolerance = 0.001)
  expect_equal(im$auc, 0.78 * 152 / 22.2, tolerance = 1e-6)
})

test_that("methylergonovine is anchored at the study's typical woman", {
  published <- methylergonovine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  woman <- methylergonovine(61, 167, 41.2, "female")$PK$default
  expect_equal(woman, published)
  # the 70 kg reference man: volumes x 1.341332, clearances x 1.246385
  man <- methylergonovine(70, 170, 35, "male")$PK$default
  expect_equal_rounded(man$v1, published$v1 * 1.341332)
  expect_equal_rounded(man$v2, published$v2 * 1.341332)
  expect_equal_rounded(man$cl1, published$cl1 * 1.246385)
  expect_equal_rounded(man$cl2, published$cl2 * 1.246385)
  # switch off: no size scaling at all
  expect_equal(methylergonovine(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default,
               published)
})

test_that("scaling the women's model predicts the study's men", {
  # a check, not a fit: the men (75 kg, 181 cm, 35.7 y) had CL 32.2 (11.8) L/h
  # and Vss 71.5 (25.9) L
  men <- methylergonovine(75, 181, 35.7, "male")$PK$default
  expect_equal(men$cl1 * 60 * 0.76, 32.2, tolerance = 0.1)
  expect_equal((men$v1 + men$v2) * 0.76, 71.5, tolerance = 0.1)
})

test_that("methylergonovine is offered intravenously and intramuscularly only", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "methylergonovine"])
  expect_true(all(units %in% allUnits | units %in% scheduledUnits))
  expect_setequal(doseRoute(units), c(ROUTE_IV, ROUTE_IM))
  # no oral route (R/drugs_methylergonovine.R, THE ORAL ROUTE)
  x <- methylergonovine(70, 170, 35, "male")$PK$default
  expect_false(any(c("ka_PO", "bioavailability_PO", "tlag_PO") %in% names(x)))
})
