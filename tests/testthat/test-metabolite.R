# Tests for the active-metabolite closed form.
#
# The closed form is checked three independent ways: against the effect-site
# coefficients already in getDrugPK.R (which are its one-compartment special
# case), against a direct numerical convolution, and against an RK4 integration
# of the six-compartment parent/metabolite cascade written out by hand here.

pkFor <- function(drug, weight = 70, height = 170, age = 50, sex = "male") {
  getDrugPK(drug, weight, height, age, sex, getDrugDefaults(drug))$PK$default
}


test_that("dispositionTerms drops the compartments a model does not use", {
  three <- list(p_coef_bolus_l1 = 1, p_coef_bolus_l2 = 2, p_coef_bolus_l3 = 3,
                lambda_1 = 0.5, lambda_2 = 0.1, lambda_3 = 0.01)
  d <- dispositionTerms(three)
  expect_equal(d$coef, c(1, 2, 3))
  expect_equal(d$lambda, c(0.5, 0.1, 0.01))

  one <- list(p_coef_bolus_l1 = 1, p_coef_bolus_l2 = 0, p_coef_bolus_l3 = 0,
              lambda_1 = 0.5, lambda_2 = 0, lambda_3 = 0)
  d <- dispositionTerms(one)
  expect_equal(d$coef, 1)
  expect_equal(d$lambda, 0.5)
})


test_that("the one-compartment case reproduces getDrugPK's effect-site coefficients", {
  # The effect site is a metabolite with a single exponential, mu = ke0, and
  # K * m_1 = ke0.  If this derivation is right it must reproduce the
  # e_coef_bolus_* lines in getDrugPK.R exactly.
  parent <- pkFor("fentanyl")
  ke0 <- parent$ke0
  expect_gt(ke0, 0)

  # A one-compartment "metabolite" whose single coefficient is 1 and whose
  # eigenvalue is ke0.  Choose kForm so that K = ke0.
  effectSite <- list(p_coef_bolus_l1 = 1, p_coef_bolus_l2 = 0,
                     p_coef_bolus_l3 = 0,
                     lambda_1 = ke0, lambda_2 = 0, lambda_3 = 0)

  # K = kForm * v1, so kForm = ke0 / v1 makes K equal ke0
  co <- metaboliteCoefficients(parent, effectSite, kForm = ke0 / parent$v1)

  # Coefficients on the parent eigenvalues, in lambda order
  onLambda <- co$bolus[seq_along(dispositionTerms(parent)$lambda)]
  expected <- c(
    parent$e_coef_bolus_l1,
    parent$e_coef_bolus_l2,
    parent$e_coef_bolus_l3
  )
  expected <- expected[dispositionTerms(parent)$lambda > 0]

  expect_equal(onLambda, expected, tolerance = 1e-8)

  # And the coefficient on ke0 is minus the sum of the others, exactly as
  # e_coef_bolus_ke0 is written
  onKe0 <- co$bolus[length(co$bolus)]
  expect_equal(onKe0, -sum(onLambda), tolerance = 1e-8)
  expect_equal(onKe0, parent$e_coef_bolus_ke0, tolerance = 1e-8)
})


test_that("metabolite concentration starts at zero", {
  # Structurally the bolus coefficients must sum to exactly zero: no metabolite
  # exists at the instant the parent is given.
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")
  co <- metaboliteCoefficients(parent, metabolite, kForm = 0.01)

  expect_equal(sum(co$bolus), 0, tolerance = 1e-12)
  expect_equal(metaboliteAfterBolus(co, 10, 0), 0, tolerance = 1e-12)
})


test_that("metabolite exposure follows kForm and the metabolite's own clearance", {
  # AUC of the metabolite for a unit parent bolus is
  #     kForm * mwRatio / (k10_parent * CL_metabolite)
  # because every molecule formed must eventually clear through the metabolite's
  # own clearance.
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")
  CLm <- metabolite$k10 * metabolite$v1

  for (kf in c(0.005, 0.05)) {
    for (R in c(1, 285.34 / 299.36)) {
      co <- metaboliteCoefficients(parent, metabolite, kForm = kf, mwRatio = R)
      # AUC is the sum of coefficient/lambda, which is the infusion coefficients
      expect_equal(sum(co$infusion), kf * R / (parent$k10 * CLm),
                   tolerance = 1e-8, info = paste("kForm", kf, "R", R))
    }
  }
})


test_that("the closed form matches a direct numerical convolution", {
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")
  kf <- 0.01
  R  <- 285.34 / 299.36
  co <- metaboliteCoefficients(parent, metabolite, kForm = kf, mwRatio = R)

  P <- dispositionTerms(parent)
  M <- dispositionTerms(metabolite)
  K <- kf * parent$v1 * R

  Cp      <- function(t) sum(P$coef * exp(-P$lambda * t))
  CmUnit  <- function(t) sum(M$coef * exp(-M$lambda * t))

  # Convolution by fine-grained trapezoid
  convolved <- function(t, n = 20000) {
    s <- seq(0, t, length.out = n)
    y <- vapply(s, function(u) K * Cp(u) * CmUnit(t - u), numeric(1))
    sum((y[-1] + y[-n]) / 2 * diff(s))
  }

  for (t in c(5, 30, 120, 480)) {
    expect_equal(metaboliteAfterBolus(co, 1, t), convolved(t),
                 tolerance = 1e-5, info = paste("t =", t))
  }
})


test_that("the closed form matches an RK4 integration of the full cascade", {
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")
  kf   <- 0.01
  R    <- 285.34 / 299.36
  dose <- 2
  co <- metaboliteCoefficients(parent, metabolite, kForm = kf, mwRatio = R)

  # Six compartments written out by hand from the micro rate constants: three
  # for the parent, three for the metabolite, coupled by the formation term.
  #
  # Note the formation term is kForm * A1: a first order transfer out of the
  # parent's central compartment with its own rate constant, and it is NOT
  # subtracted from dA1/dt.  The parent's fitted clearance already subsumes the
  # metabolic loss, so removing it again would double-count it.
  deriv <- function(y) {
    A1 <- y[1]; A2 <- y[2]; A3 <- y[3]
    B1 <- y[4]; B2 <- y[5]; B3 <- y[6]
    with(list(), c(
      -(parent$k10 + parent$k12 + parent$k13) * A1 + parent$k21 * A2 + parent$k31 * A3,
        parent$k12 * A1 - parent$k21 * A2,
        parent$k13 * A1 - parent$k31 * A3,
        kf * R * A1 -
          (metabolite$k10 + metabolite$k12 + metabolite$k13) * B1 +
          metabolite$k21 * B2 + metabolite$k31 * B3,
        metabolite$k12 * B1 - metabolite$k21 * B2,
        metabolite$k13 * B1 - metabolite$k31 * B3
    ))
  }
  step <- function(y, h) {
    k1 <- deriv(y); k2 <- deriv(y + h/2 * k1)
    k3 <- deriv(y + h/2 * k2); k4 <- deriv(y + h * k3)
    y + h/6 * (k1 + 2*k2 + 2*k3 + k4)
  }

  y <- c(dose, 0, 0, 0, 0, 0)
  h <- 0.002
  checkpoints <- c(5, 30, 120)
  t <- 0
  for (tc in checkpoints) {
    while (t < tc - 1e-9) { y <- step(y, h); t <- t + h }
    numeric <- y[4] / metabolite$v1
    closed  <- metaboliteAfterBolus(co, dose, tc)
    expect_equal(closed, numeric, tolerance = 1e-5,
                 info = paste("t =", tc))
  }
})


test_that("no pathway means no metabolite", {
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")
  co <- metaboliteCoefficients(parent, metabolite, kForm = 0)

  expect_true(all(co$bolus == 0))
  expect_equal(metaboliteAfterBolus(co, 100, c(1, 10, 100)), c(0, 0, 0))
})


test_that("metabolite concentration scales linearly with dose and with kForm", {
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")

  a <- metaboliteCoefficients(parent, metabolite, kForm = 0.005)
  b <- metaboliteCoefficients(parent, metabolite, kForm = 0.010)
  expect_equal(b$bolus, 2 * a$bolus, tolerance = 1e-12)

  t <- c(1, 10, 60)
  expect_equal(metaboliteAfterBolus(a, 20, t),
               2 * metaboliteAfterBolus(a, 10, t), tolerance = 1e-12)
})


test_that("a shared eigenvalue is separated rather than dividing by zero", {
  # Two independently fitted drugs never share an eigenvalue exactly, but the
  # convolution divides by (mu - lambda), so the degenerate case must not blow
  # up if a caller constructs one.
  parent <- pkFor("hydromorphone")
  P <- dispositionTerms(parent)

  twin <- list(p_coef_bolus_l1 = 0.01, p_coef_bolus_l2 = 0, p_coef_bolus_l3 = 0,
               lambda_1 = P$lambda[1], lambda_2 = 0, lambda_3 = 0)

  co <- metaboliteCoefficients(parent, twin, kForm = 0.01)
  expect_true(all(is.finite(co$bolus)))
  expect_true(all(is.finite(co$infusion)))
  expect_equal(sum(co$bolus), 0, tolerance = 1e-6)

  # Still non-trivial and still rises from zero
  v <- metaboliteAfterBolus(co, 10, c(0, 5, 30))
  expect_equal(v[1], 0, tolerance = 1e-9)
  expect_gt(v[2], 0)
})


test_that("an invalid rate constant or weight ratio is refused", {
  parent     <- pkFor("hydromorphone")
  metabolite <- pkFor("morphine")
  expect_error(metaboliteCoefficients(parent, metabolite, kForm = -0.1))
  expect_error(metaboliteCoefficients(parent, metabolite, kForm = 0.01,
                                      mwRatio = 0))
  # A transfer rate constant has no upper bound, unlike a share of elimination.
  expect_no_error(metaboliteCoefficients(parent, metabolite,
                                         kForm = parent$k10 * 2))
})
