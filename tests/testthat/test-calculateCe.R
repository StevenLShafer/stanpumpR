test_that("it returns the correct array", {
  L <- 26
  Cp <- c(1:L)
  ke0 <- rep(0.2032826, L)
  dt <- c(0.0000000, 0.8522619, 0.1693822, 0.2030460, 0.2434002, 0.2917747, 0.3497632, 0.4192767, 0.5026056, 0.6024956, 0.7222382, 0.8657789, 1.0378476, 1.2441139, 1.4913745, 1.7877767, 2.1430873, 2.5690137, 3.0795907, 3.6916419, 4.4253349, 5.3048451, 6.3591531, 7.6229988, 9.1380267, 4.8831719)

  actual <- calculateCe(Cp, ke0, dt, L)
  expectedCe <- c(0.00000, 0.24090, 0.31746, 0.44629, 0.64218,
                  0.92222, 1.30544, 1.81212, 2.46250, 3.27492,
                  4.26317, 5.43341, 6.78087, 8.28703, 9.91848,
                  11.62843, 13.36164, 15.06278, 16.68619, 18.20415,
                  19.61008, 20.91533, 22.14102, 23.30932, 24.43790, 25.15763)

  expect_equal_rounded(actual, expectedCe)
})

test_that("plasma falling at exactly the ke0 rate gives the limiting value, not NaN", {
  # Cp = exp(-ke0 t) from Ce = 0 has the closed form Ce = ke0 t exp(-ke0 t).
  # Here the falling-plasma branch's log slope k equals -ke0, and the
  # expression y0 ke0 / (k + ke0) (exp(k dt) - exp(-ke0 dt)) is 0/0.
  ke0 <- 0.25
  L <- 11
  dt <- rep(0.5, L)
  t <- cumsum(dt) - dt[1]
  Cp <- exp(-ke0 * t)

  actual <- calculateCe(Cp, rep(ke0, L), dt, L)

  expect_false(anyNA(actual))
  expect_equal(actual, ke0 * t * exp(-ke0 * t), tolerance = 1e-10)
})

test_that("the limiting form joins the exact one continuously", {
  # Just outside the switch the exact expression is used; just inside, the
  # limit.  The two must agree to well under the plotting precision.
  ke0 <- 0.25
  dt <- 0.5
  y0 <- 2
  ce <- function(k) calculateCe(c(y0, y0 * exp(k * dt)), rep(ke0, 2), c(0, dt), 2)[2]
  inside  <- ce(-ke0 * (1 + 1e-9))
  outside <- ce(-ke0 * (1 + 1e-7))
  expect_equal(inside, outside, tolerance = 1e-6)
  expect_equal(inside, y0 * ke0 * dt * exp(-ke0 * dt), tolerance = 1e-7)
})
