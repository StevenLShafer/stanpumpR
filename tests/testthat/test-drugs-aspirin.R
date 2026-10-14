# Aspirin (enteric-coated tablet, low dose) with salicylate as its metabolite,
# Koh 2025.  See the header of R/drugs_aspirin.R.  The engine is checked
# against an independent Runge-Kutta integration of the model as implemented
# (two lagged depots, first-pass formation), and the implementation against
# Koh's exact structure (zero-order input into a pre-systemic compartment).

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the parameters at the median weight with the switch off", {
  a <- aspirin(68.35, 171, 30, "male", adjustToFFM = FALSE)
  d <- a$PK$default
  expect_equal(d$v1, 23.51)
  expect_equal(d$cl1 * 60, 2.97 * 23.51)              # 69.8 L/h, all to SA
  expect_equal(d$bioavailability_PO, 2.32 / 2.89)
  expect_equal(d$fraction_PO2, 0.31)
  expect_equal(d$ka_PO * 60, 1.032)
  expect_equal(d$tlag_PO / 60, 3.269)
  # the first-order path: mean and variance of depot + pre-systemic delay
  kp <- 2.89
  expect_equal(1 / (d$ka_PO2 * 60), sqrt(1 / 0.053^2 + 1 / kp^2))
  expect_equal(d$tlag_PO2 / 60, 2.81 + 1 / 0.053 + 1 / kp - sqrt(1 / 0.053^2 + 1 / kp^2))
  expect_identical(a$metabolite$name, "salicylate")
  expect_equal(a$metabolite$kFormation * 60, 2.97)
  expect_equal(a$metabolite$firstPassFraction, 0.57 / 2.89)
  expect_equal(a$metabolite$mwRatio, 138.12 / 180.16)
  expect_false(a$prodrug)
})

test_that("k34 carries the weight term, V3 the library's factors", {
  off <- aspirin(100, 171, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1 * 60, 2.97 * (100 / 68.35)^1.31 * 23.51)
  on <- aspirin(100, 171, 30, "male")$PK$default
  size <- pkSizeFactors(100, 171, 30, "male", TRUE)
  expect_equal(on$v1, 23.51 * size$volume)
  expect_equal(on$cl1 * 60, 2.97 * (size$pkWeight / 68.35)^1.31 * 23.51 * size$volume)
})

# The implemented model by fourth-order Runge-Kutta, in mg and hours: two
# lagged depots feeding ASA (fraction F) and, on first pass, SA (1 - F); ASA
# converted to SA at k34; SA two-compartment.
rk4Aspirin <- function(times, dose) {
  a <- aspirin(68.35, 171, 30, "male", adjustToFFM = FALSE)
  d <- a$PK$default; s <- salicylate(68.35, 171, 30, "male", FALSE)$PK$default
  F <- d$bioavailability_PO; f2 <- d$fraction_PO2
  ka1 <- d$ka_PO * 60; ka2 <- d$ka_PO2 * 60; lag1 <- d$tlag_PO / 60; lag2 <- d$tlag_PO2 / 60
  k34 <- d$cl1 / d$v1 * 60; mw <- a$metabolite$mwRatio
  V4 <- s$v1; V5 <- s$v2; CL <- s$cl1 * 60; Q <- s$cl2 * 60
  deriv <- function(y) {
    in1 <- ka1 * y[1]; in2 <- ka2 * y[2]
    c(-in1, -in2,
      F * (in1 + in2) - k34 * y[3],
      mw * ((1 - F) * (in1 + in2) + k34 * y[3]) - CL * y[4] / V4 - Q * (y[4] / V4 - y[5] / V5),
      Q * (y[4] / V4 - y[5] / V5))
  }
  y <- c(0, 0, 0, 0, 0); t <- 0; h <- 0.002
  g1 <- g2 <- FALSE
  out <- matrix(NA_real_, length(times), 2)
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      if (!g1 && t >= lag1 - 1e-12) { y[1] <- dose * (1 - f2); g1 <- TRUE }
      if (!g2 && t >= lag2 - 1e-12) { y[2] <- dose * f2; g2 <- TRUE }
      step <- min(h, times[i] - t, c(if (!g1) lag1, if (!g2) lag2) - t)
      k1 <- deriv(y); k2 <- deriv(y + step / 2 * k1); k3 <- deriv(y + step / 2 * k2)
      k4 <- deriv(y + step * k3)
      y <- y + step * (k1 + 2 * k2 + 2 * k3 + k4) / 6
      t <- t + step
    }
    out[i, ] <- c(y[3] / d$v1, y[4] / V4)
  }
  out
}

test_that("100 mg matches an independent integration of the implemented model", {
  x <- simulateDrugsWithCovariates(
    data.frame(Drug = "aspirin", Time = 0, Dose = 100, Units = "mg PO"),
    noEvents, 68.35, 171, 30, "male", 24 * 60, FALSE, adjustToFFM = FALSE)
  # compared at the engine's own time points (between them the plotted
  # series is interpolated)
  a <- x$aspirin$results;    a <- a[a$Site == "Plasma", ]
  s <- x$salicylate$results; s <- s[s$Site == "Plasma", ]
  keep <- a$Time >= 3 * 60
  ref <- rk4Aspirin(a$Time[keep] / 60, 100)
  expect_equal(a$Y[keep], ref[, 1], tolerance = 1e-5)
  keep <- s$Time >= 3 * 60
  ref <- rk4Aspirin(s$Time[keep] / 60, 100)
  expect_equal(s$Y[keep], ref[, 2], tolerance = 1e-5)
})

test_that("the approximation stays close to Koh's exact structure", {
  # Koh's structure solved numerically (Euler, 0.0005 h), 100 mg at 68.35 kg:
  # ASA peak 0.492 mg/L at 4.41 h, AUC 1.14; SA peak 4.645 mg/L at 5.05 h,
  # AUC 27.42 mg.h/L (computed by Claude Code, 2026-10-10)
  x <- simulateDrugsWithCovariates(
    data.frame(Drug = "aspirin", Time = 0, Dose = 100, Units = "mg PO"),
    noEvents, 68.35, 171, 30, "male", 72 * 60, FALSE, adjustToFFM = FALSE)
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  a <- x$aspirin$wide; s <- x$salicylate$wide
  expect_equal(max(a$Plasma), 0.492, tolerance = 0.05)
  expect_equal(auc(a), 1.14, tolerance = 0.02)
  expect_equal(max(s$Plasma), 4.645, tolerance = 0.15)   # 13% low; see header
  expect_equal(auc(s), 27.42, tolerance = 0.02)
})

test_that("the metabolite engine carries the second oral depot", {
  # Without the second depot the ASA would be absorbed from one path only;
  # with it the SA AUC equals dose x mw / CL whatever the split
  PK <- getDrugPK("aspirin", 68.35, 171, 30, "male", adjustToFFM = FALSE)
  expect_false(is.null(PK$PK$default$metabolite$coefs$PO2))
  r <- simCpCe(data.frame(Drug = "aspirin", Time = 0, Dose = 100, Units = "mg PO"),
               noEvents, PK, 200 * 60, FALSE)
  expect_false(is.null(r$metaboliteSeries))
})
