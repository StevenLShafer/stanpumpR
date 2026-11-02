# The Duzitol editorial (Shafer and Stanski, Anesthesiology 1992;76:327-30)
# and the help page built on it, inst/help/models/why-simulate.md.  Duzitol is
# fictional; its Table 1 shows that a table of pharmacokinetic parameters
# cannot be read by eye: the hepatic failure column, every half-life four
# times longer, recovers fastest.  The models are checked against Shafer's own
# spreadsheets, and the decrement times against an independent solution (a
# matrix exponential by eigendecomposition, with the state at the stop found
# by numerical integration of the held infusion).

# The rate matrix of a three-compartment model, amounts in, amounts out.
duzitolRateMatrix <- function(m) {
  matrix(c(-(m$k10 + m$k12 + m$k13), m$k21, m$k31,
           m$k12, -m$k21, 0,
           m$k13, 0, -m$k31), nrow = 3, byrow = TRUE)
}

# exp(K t) x by eigendecomposition: independent of cube() and of the residues.
duzitolPropagate <- function(K, x, t) {
  e <- eigen(K)
  V <- e$vectors
  Re(V %*% diag(exp(e$values * t)) %*% solve(V, x))
}

# The peripheral amounts after holding the plasma amount at V1 for T minutes,
# by fourth-order Runge-Kutta (independent of the closed form).
duzitolHeldState <- function(m, T, h = 0.01) {
  a <- c(0, 0)
  d <- function(a) c(m$k12 * m$v1 - m$k21 * a[1], m$k13 * m$v1 - m$k31 * a[2])
  for (i in seq_len(round(T / h))) {
    s1 <- d(a); s2 <- d(a + h / 2 * s1); s3 <- d(a + h / 2 * s2); s4 <- d(a + h * s3)
    a <- a + h / 6 * (s1 + 2 * s2 + 2 * s3 + s4)
  }
  c(m$v1, a)
}

test_that("the three-compartment closure reproduces Shafer's spreadsheets", {
  # Micro constants and unit-bolus coefficients (1/L) from the "(3). V1,
  # Vdss, Cl, half-lives" sheets Shafer built Table 1's models with, to the
  # digits they print.
  sheet <- list(
    c(k21 = 0.217689586, k31 = 0.017097925, k13 = 0.104640961, k12 = 0.340491214,
      A = 0.098797945, B = 0.026195157, C = 0.006585846),
    c(k21 = 0.020938793, k31 = 0.002436378, k13 = 0.039707715, k12 = 0.027265457,
      A = 0.19456923, B = 0.004420758, C = 0.001010012),
    c(k21 = 0.125688153, k31 = 0.007110812, k13 = 0.300788494, k12 = 0.223624982,
      A = 0.150580618, B = 0.00719905, C = 0.000950491))
  ms <- duzitolModels()
  expect_equal(names(ms), DUZITOL_TABLE$model)
  for (i in seq_along(ms)) {
    m <- ms[[i]]; t <- DUZITOL_TABLE[i, ]
    expect_equal(c(k21 = m$k21, k31 = m$k31, k13 = m$k13, k12 = m$k12),
                 sheet[[i]][c("k21", "k31", "k13", "k12")], tolerance = 1e-7)
    l <- m$lambda
    bolus <- vapply(1:3, function(j)
      (m$k21 - l[j]) * (m$k31 - l[j]) / prod(l[-j] - l[j]) / m$v1, numeric(1))
    expect_equal(bolus, unname(sheet[[i]][c("A", "B", "C")]), tolerance = 1e-7)
    expect_equal(m$v1 + m$v2 + m$v3, t$Vss, tolerance = 1e-12)
    expect_equal(m$cl1, t$CL)
    # The exponents give back the printed half-lives, by stanpumpR's cube()
    # and by an eigendecomposition of the rate matrix.
    expect_equal(log(2) / l, c(t$t1, t$t2, t$t3), tolerance = 1e-9)
    expect_equal(sort(-eigen(duzitolRateMatrix(m))$values, decreasing = TRUE), l,
                 tolerance = 1e-9)
  }
  # The volumes and clearances the spreadsheets give (L, L/min)
  expect_equal_rounded(c(11.89, 46.51, 2.588, 0.7953),
                       round(c(ms[[1]]$v2, ms[[1]]$v3, ms[[1]]$cl2, ms[[1]]$cl3), 4),
                       tolerance = 1e-3)
  expect_equal_rounded(c(6.511, 81.49, 0.1363, 0.1985),
                       round(c(ms[[2]]$v2, ms[[2]]$v3, ms[[2]]$cl2, ms[[2]]$cl3), 4),
                       tolerance = 1e-3)
  expect_equal_rounded(c(11.21, 266.5, 1.409, 1.895),
                       round(c(ms[[3]]$v2, ms[[3]]$v3, ms[[3]]$cl2, ms[[3]]$cl3), 4),
                       tolerance = 1e-3)
  expect_error(threeCompartmentFromHalfLives(1, 10, 11, 1, 10, 100), "no physically admissible")
})

test_that("the decline after a held infusion agrees with an independent solution", {
  for (m in duzitolModels()) {
    K <- duzitolRateMatrix(m)
    for (T in c(30, 120)) {
      x <- duzitolHeldState(m, T)
      # The closed-form state at the stop
      expect_equal(x[2:3], c(m$v2 * (1 - exp(-m$k21 * T)), m$v3 * (1 - exp(-m$k31 * T))),
                   tolerance = 1e-8)
      ts <- c(0, 1, 5, 20, 60, 200)
      ref <- vapply(ts, function(t) duzitolPropagate(K, x, t)[1] / m$v1, numeric(1))
      expect_equal(plasmaAfterHeldInfusion(m, T, ts), ref, tolerance = 1e-8)
      # The decrement time is where the independent solution crosses 50%
      root <- stats::uniroot(function(t) duzitolPropagate(K, x, t)[1] / m$v1 - 0.5,
                             c(0, 1000), tol = 1e-12)$root
      expect_equal(heldInfusionDecrementTime(m, T), root, tolerance = 1e-7)
    }
    # Steady state: every compartment at the plasma concentration
    xs <- c(m$v1, m$v2, m$v3)
    root <- stats::uniroot(function(t) duzitolPropagate(K, xs, t)[1] / m$v1 - 0.5,
                           c(0, 1000), tol = 1e-12)$root
    expect_equal(heldInfusionDecrementTime(m, Inf), root, tolerance = 1e-7)
    # A held infusion of no length is a bolus into V1
    bolusRoot <- stats::uniroot(function(t) duzitolPropagate(K, c(m$v1, 0, 0), t)[1] / m$v1 - 0.5,
                                c(0, 1000), tol = 1e-12)$root
    expect_equal(heldInfusionDecrementTime(m, 0), bolusRoot, tolerance = 1e-7)
    # The concentration only falls, so the root found is the first crossing
    dense <- plasmaAfterHeldInfusion(m, Inf, seq(0, 3000, by = 0.25))
    expect_true(all(diff(dense) < 0))
    # The shares are the decline at the stop, and sum to the maintained level
    expect_equal(sum(heldInfusionCoefficients(m, Inf)), 1, tolerance = 1e-12)
    expect_equal(sum(heldInfusionCoefficients(m, 45)), 1, tolerance = 1e-12)
  }
})

test_that("the models give the editorial's decrement times, and the reversals the page describes", {
  ms <- duzitolModels()
  ss50 <- vapply(ms, heldInfusionDecrementTime, numeric(1), duration = Inf)
  ss80 <- vapply(ms, heldInfusionDecrementTime, numeric(1), duration = Inf, fraction = 0.8)
  expect_equal(unname(round(ss50, 1)), c(41.3, 10.1, 21.8))
  expect_equal(unname(round(ss80)), c(169, 235, 271))
  # Duzitol's two agree with the printed 41 and 10 to the printed digit
  expect_equal(unname(round(ss50[1:2])), DUZITOL_TABLE$decrement50[1:2])
  # Hepatic failure is fastest to a 50% fall and slower than normal to 80%
  expect_equal(unname(order(ss50)), c(2L, 3L, 1L))
  expect_equal(unname(order(ss80)), c(1L, 2L, 3L))
  # The shares of the steady-state concentration in each exponential
  shares <- lapply(ms, heldInfusionCoefficients, duration = Inf)
  expect_equal(round(shares[[1]], 3), c(0.097, 0.257, 0.646))
  expect_equal(round(shares[[2]], 3), c(0.573, 0.130, 0.297))
  expect_equal(round(shares[[3]], 3), c(0.330, 0.154, 0.516))
  # The context-sensitive half-time at 600 min, the end of Figure 1's axis
  expect_equal(unname(round(vapply(ms, heldInfusionDecrementTime, numeric(1), duration = 600), 1)),
               c(41.3, 8.3, 20.2))
})

test_that("propofol's printed 20 min lies within the rounding of its Table 1 column", {
  t <- DUZITOL_TABLE[3, ]
  centre <- c(t$t1, t$t2, t$t3, t$CL, t$V1, t$Vss)
  half <- c(0.05, 0.05, 0.5, 0.05, 0.05, 0.5)   # half the last printed digit
  corners <- as.matrix(expand.grid(rep(list(c(-1, 1)), 6)))
  times <- apply(corners, 1, function(s) {
    x <- centre + s * half
    m <- threeCompartmentFromHalfLives(x[4], x[5], x[6], x[1], x[2], x[3])
    m$k10 <- m$cl1 / m$v1; m$k12 <- m$cl2 / m$v1; m$k13 <- m$cl3 / m$v1
    m$k21 <- m$cl2 / m$v2; m$k31 <- m$cl3 / m$v3
    m$lambda <- cube(m$k10, m$k12, m$k13, m$k21, m$k31)
    heldInfusionDecrementTime(m, Inf)
  })
  expect_equal(round(range(times), 1), c(17.3, 26.8))
  expect_true(t$decrement50 > min(times) && t$decrement50 < max(times))
})

test_that("the help page's numbers come from the models", {
  html <- helpPageHTML("models/why-simulate")
  expect_match(html, "<svg", fixed = TRUE)
  expect_match(html, "Duzitol, hepatic failure", fixed = TRUE)
  ms <- duzitolModels()
  # Table 1 as typed on the page is DUZITOL_TABLE
  md <- paste(readLines(helpMarkdownPath("models/why-simulate"), warn = FALSE), collapse = "\n")
  for (i in seq_len(nrow(DUZITOL_TABLE))) {
    t <- DUZITOL_TABLE[i, ]
    expect_match(md, paste(format(t$t1, nsmall = 1), format(t$t2, nsmall = 1), t$t3, sep = ", "),
                 fixed = TRUE)
  }
  expect_match(md, "| Metabolic clearance (L/min) | 0.68 | 0.51 | 1.9 |", fixed = TRUE)
  expect_match(md, "| V1 (L) | 7.6 | 5.0 | 6.3 |", fixed = TRUE)
  expect_match(md, "| Vdss (L) | 66 | 93 | 284 |", fixed = TRUE)
  # The prose quotes these; each is checked against the models above
  for (s in c("41.3, 10.1 and 21.8", "169 minutes", "235 minutes", "271 for propofol",
              "about 57%", "about 65%", "about 48%", "17.3 to 26.8 minutes",
              "0.14 and 0.20 L/min against 2.6 and 0.80"))
    expect_match(md, s, fixed = TRUE)
  expect_equal(round(c(ms[[2]]$cl2, ms[[2]]$cl3, ms[[1]]$cl2, ms[[1]]$cl3), 2),
               c(0.14, 0.20, 2.59, 0.80))
  expect_equal(round(100 * c(heldInfusionCoefficients(ms[[2]], Inf)[1],
                             heldInfusionCoefficients(ms[[1]], Inf)[3],
                             sum(heldInfusionCoefficients(ms[[3]], Inf)[1:2]))),
               c(57, 65, 48))
  # "a half-time of about three minutes" for normal subjects' fast return,
  # "under 7 minutes" for the hepatic failure central volume
  expect_equal(round(log(2) / ms[[1]]$k21), 3)
  expect_lt(log(2) / ms[[2]]$k10, 7)
})
