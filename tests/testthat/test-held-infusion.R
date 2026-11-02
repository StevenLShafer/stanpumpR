# The decline after an infusion that held the plasma concentration, the one
# implementation the Duzitol help page and Single Paper Review share.

# An independent solution: the amounts propagated by the rate matrix's
# eigendecomposition.
heldReference <- function(v, cl, T, fraction) {
  n <- length(v)
  k10 <- cl[1] / v[1]
  K <- matrix(0, n, n)
  K[1, 1] <- -k10
  for (p in seq_len(n)[-1]) {
    k1p <- cl[p] / v[1]; kp1 <- cl[p] / v[p]
    K[1, 1] <- K[1, 1] - k1p; K[1, p] <- kp1; K[p, 1] <- k1p; K[p, p] <- -kp1
  }
  x0 <- c(v[1], v[-1] * (1 - exp(-cl[-1] / v[-1] * T)))
  e <- eigen(K)
  a <- solve(e$vectors, x0)
  plasma <- function(t) Re(sum(e$vectors[1, ] * a * exp(e$values * t))) / v[1]
  stats::uniroot(function(t) plasma(t) - (1 - fraction), c(0, 1e5), tol = 1e-12)$root
}

test_that("one, two and three compartments agree with an independent solution", {
  sets <- list(list(v = 20, cl = 1),
               list(v = c(10, 40), cl = c(1, 2)),
               list(v = c(7.6, 12, 46), cl = c(0.68, 2.6, 0.79)))
  for (s in sets) {
    model <- stats::setNames(as.list(c(s$v, s$cl)), c(paste0("v", seq_along(s$v)), paste0("cl", seq_along(s$cl))))
    for (T in c(0, 30, 240, 1e6))
      for (fraction in c(0.2, 0.5, 0.8))
        expect_equal(heldInfusionDecrementTime(model, T, fraction), heldReference(s$v, s$cl, T, fraction),
                     tolerance = 1e-7, info = paste(length(s$v), T, fraction))
    expect_equal(sum(heldInfusionCoefficients(model, Inf)), 1, tolerance = 1e-12)
    expect_equal(heldInfusionDecrementTime(model, Inf), heldInfusionDecrementTime(model, 1e9), tolerance = 1e-8)
  }
})

test_that("it reproduces stanpumpR's own plasma-target TCI through simCpCe()", {
  # The simulator's TCI steps the rate, so it holds the plasma concentration
  # nearly, not exactly: about 1% on these drugs.
  old <- options(ECHO_OUTPUT_COMMENTS = NA)
  on.exit(options(old), add = TRUE)
  dd <- getDrugDefaultsGlobal()
  for (drug in c("propofol", "fentanyl")) {
    pk <- getDrugPK(drug, 70, 170, 40, "male", dd[dd$Drug == drug, ])
    for (T in c(30, 120, 480)) {
      doses <- data.frame(Drug = drug, Time = c(0, T), Dose = c(1, 0), Units = TCI_UNIT_PLASMA)
      sim <- simCpCe(doses, data.frame(Time = numeric(0), Event = character(0)), pk, T + 600, FALSE)
      cp <- sim$results[sim$results$Site == "Plasma", c("Time", "Y")]
      after <- cp[cp$Time >= T, ]
      j <- which(after$Y <= after$Y[1] * 0.5)[1]
      simulated <- stats::approx(after$Y[(j - 1):j], after$Time[(j - 1):j], after$Y[1] * 0.5)$y - T
      expect_equal(heldInfusionDecrementTime(pk$PK$default, T), simulated, tolerance = 0.015,
                   info = paste(drug, T))
    }
  }
})
