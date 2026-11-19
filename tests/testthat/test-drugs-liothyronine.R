# Liothyronine (R/drugs_liothyronine.R): Van Tassell 2019, Table 3, total T3,
# last dose at steady state.  Drafted by Claude Code, 2026-11-18.
# Expectations are computed here from the paper's Table 3, not from the drug
# file.

vt <- list(v1 = 8.67, ke = 0.12, k12 = 0.13, k21 = 0.08, ka = 1.94, lag = 0.62)
vtCl <- vt$ke * vt$v1               # L/h
vtQ  <- vt$k12 * vt$v1              # L/h
vtV2 <- vtQ / vt$k21                # L
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Van Tassell 2019 Table 3, switch off: the published model", {
  set <- liothyronine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$v1, 8.67)
  expect_equal(set$cl1 * 60, vtCl)
  expect_equal(set$cl2 * 60, vtQ)
  expect_equal(set$v2, vtV2)
  expect_equal_rounded(set$v2, 14.08875)
  expect_equal(set$ka_PO * 60, 1.94)
  expect_equal(set$tlag_PO, 0.62 * 60)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$cl3, 0)
  # no covariate: a 40 kg woman of 80 gets the same published model
  expect_equal(liothyronine(40, 150, 80, "female", FALSE)$PK, liothyronine(70, 170, 35, "male", FALSE)$PK)
  # the half-lives the means give: 2.33 h and 21.5 h (paper: 2.3 and 22.9,
  # means of the individual half-lives)
  a <- vt$ke + vt$k12 + vt$k21
  b <- vt$ke * vt$k21
  lambda <- c(a + sqrt(a^2 - 4 * b), a - sqrt(a^2 - 4 * b)) / 2
  expect_equal(round(log(2) / lambda, 2), c(2.33, 21.50))
})

# Van Tassell's cohort (Table 1): means 72.3 kg, BMI 25.7 (so 167.73 cm), 48.5
# years, 7 men and 7 women.  Its typical patient's factors, by the
# independent fat-free-mass formula in helpers.R; every published value is
# divided by them with the switch on (Shafer, 2026-11-18;
# docs/weight-adjustment.md, *Anchoring at the source's cohort*).
vtCohort <- cohortFactorsIndependent(list(weight = 72.3, bmi = 25.7, age = 48.5,
                                          men = 7, women = 7))

test_that("switch on: anchored at Van Tassell's cohort, scaled by fat-free mass", {
  expect_equal(round(vtCohort$volume, 6), 0.912146)
  expect_equal(round(vtCohort$clearance, 6), 0.933358)
  ffm <- ffmAlSallami(120, 170, 50, "male") / ffmAlSallami(70, 170, 35, "male")
  set <- liothyronine(120, 170, 50, "male")$PK$default
  expect_equal(set$v1, 8.67 * ffm / vtCohort$volume, tolerance = 1e-9)
  expect_equal(set$cl1 * 60, vtCl * ffm^0.75 / vtCohort$clearance, tolerance = 1e-9)
  # the reference man: 9.51 L and 26.75 L/day
  ref <- liothyronine(70, 170, 35, "male")$PK$default
  expect_equal(round(ref$v1, 2), 9.51)
  expect_equal(round(ref$cl1 * 1440, 2), 26.75)
})

test_that("the cohort's typical patient receives Table 3's published values", {
  h <- 100 * sqrt(72.3 / 25.7)
  mix <- function(f) exp((7 * log(f(72.3, h, 48.5, "male")) + 7 * log(f(72.3, h, 48.5, "female"))) / 14)
  expect_equal(mix(function(...) liothyronine(...)$PK$default$v1), 8.67, tolerance = 1e-12)
  expect_equal(mix(function(...) liothyronine(...)$PK$default$v2), vtV2, tolerance = 1e-12)
  expect_equal(mix(function(...) liothyronine(...)$PK$default$cl1) * 60, vtCl, tolerance = 1e-12)
  expect_equal(mix(function(...) liothyronine(...)$PK$default$cl2) * 60, vtQ, tolerance = 1e-12)
})

test_that("the source's patients at steady state: 18.8 mcg every 8 h, Cmax 320 +/- 60 ng/dL at 1.8 h", {
  dt <- data.frame(Drug = "liothyronine", Time = 0, Dose = 18.8, Units = "mcg PO tid")
  w <- simulateDrugsWithCovariates(dt, noEvents, 72.3, 170, 48, "male", 30 * 1440, FALSE,
                                   adjustToFFM = FALSE)$liothyronine$wide
  last <- w[w$Time >= 29 * 1440 + 16 * 60 & w$Time <= 30 * 1440, ]
  peak <- last[which.max(last$Plasma), ]
  # the closed form gives 305.7 ng/dL at 1.74 h and a trough of 156.2; the
  # simulation's time grid reads the peak within its spacing
  expect_equal(peak$Plasma * 100, 305.7, tolerance = 0.005)     # ng/dL
  expect_lt(abs((peak$Time - (29 * 1440 + 16 * 60)) / 60 - 1.74), 0.1)
  expect_equal(min(last$Plasma) * 100, 156.2, tolerance = 0.005)
})

test_that("the source's Figure 4: 50 mcg a day averages dose/CL, about 200 ng/dL (paper 206)", {
  css <- 50 / vtCl / 24 * 100                                   # ng/dL
  expect_equal(round(css), 200)
  dt <- data.frame(Drug = "liothyronine", Time = 0, Dose = 25, Units = "mcg PO bid")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 48, "male", 30 * 1440, FALSE,
                                   adjustToFFM = FALSE)$liothyronine$wide
  day <- seq(29 * 1440, 30 * 1440, by = 1)
  avg <- mean(approx(w$Time, w$Plasma, day)$y) * 100
  expect_equal(avg, css, tolerance = 5e-3)
})

test_that("clearance agrees with Inada 1975's intravenous tracer within 7%", {
  inada <- 22.2 * 72.3 / 60                                     # L/day at 72.3 kg
  expect_equal(round(inada, 1), 26.8)
  expect_equal(round(vtCl * 24, 1), 25.0)
  expect_lt(abs(vtCl * 24 / inada - 1), 0.07)
})

test_that("external checks, not fitted: the model's swings run 1.6-2x the measured ones", {
  # closed-form one-dose curve from Table 3 (h, mcg -> ng/dL)
  A <- matrix(0, 3, 3)
  A[1, 1] <- -vt$ka; A[2, 1] <- vt$ka
  A[2, 2] <- -(vt$ke + vt$k12); A[3, 2] <- vt$k12; A[2, 3] <- vt$k21; A[3, 3] <- -vt$k21
  one <- function(t, dose) vapply(t - vt$lag, function(s)
    if (s <= 0) 0 else (expm(A * s) %*% c(dose, 0, 0))[2] / vt$v1 * 100, numeric(1))
  expm <- function(M) {                   # matrix exponential by eigen-decomposition
    e <- eigen(M); Re(e$vectors %*% diag(exp(e$values)) %*% solve(e$vectors))
  }
  # Jonklaas 2015 Tables 2 and 4: 50 mcg, peak 346 over a predose 117.3
  # (mean of -0.5, -0.25 and 0 h); AUC(0-24 h) above baseline 1860
  g <- seq(0, 24, by = 0.01)
  y <- one(g, 50)
  expect_equal(round(max(y)), 428)
  expect_equal(round(sum(y) * 0.01 / 1860, 2), 1.76)
  expect_equal(round(max(y) / (346 - mean(c(120, 118, 114))), 2), 1.87)
  # Dumitrescu 2022 Table 2: LT3 minus placebo, 50 mcg, at 2, 3, 4, 24 h
  measured <- c(298 - 105, 304 - 102, 265 - 101, 122 - 99)
  model <- one(c(2, 3, 4, 24), 50)
  expect_equal(round(model), c(426, 366, 298, 50))
  expect_true(all(model / measured > 1.5 & model / measured < 2.3))
  # Jonklaas and Burman 2016: 38.3 mcg once a day at steady state, measured
  # rise 292.8 - 96.1 = 196.7 ng/dL
  t <- seq(0, 24, by = 0.01)
  ss <- Reduce(`+`, lapply(0:60, function(n) one(t + 24 * n, 115 / 3)))
  expect_equal(round(max(ss) - min(ss)), 325)
  expect_equal(round((max(ss) - min(ss)) / (292.8 - 96.1), 2), 1.65)
})

# Nicoloff 1972, Table I: six patients with primary hypothyroidism after an
# intravenous pulse of 125I-T3, one compartment.  Each patient's clearance is
# the distribution space times kT3, and the table's means are V 37.6 L and CL
# 18.5 L/day.  Their means are 70.6 kg, 161 cm and 52.2 years; the sexes are
# unprinted, so an even mix is assumed.
nicoloff <- data.frame(k = c(43, 41, 40, 66, 63, 46) / 100,
                       ds = c(38.3, 38.0, 41.7, 33.3, 35.3, 39.2),
                       cl = c(16.5, 15.6, 16.7, 22.0, 22.2, 18.0))
ivCohort <- cohortFactorsIndependent(list(weight = 70.6, height = 161, age = 52.2,
                                          men = 3, women = 3))

test_that("Nicoloff 1972 Table I: the rows close and give its means", {
  expect_equal(round(nicoloff$ds * nicoloff$k, 1), nicoloff$cl, tolerance = 0.06)
  expect_equal(round(mean(nicoloff$ds), 1), 37.6)
  expect_equal(round(mean(nicoloff$cl), 1), 18.5)
  expect_equal(round(log(2) / (18.5 / 37.6), 2), 1.41)                 # days
})

test_that("intravenous doses go to Nicoloff's one compartment, oral doses to Van Tassell's", {
  x <- liothyronine(70, 170, 35, "male", adjustToFFM = FALSE)
  expect_identical(x$routes, ROUTE_PO)
  expect_length(x$parallelSystems, 1)
  iv <- x$parallelSystems[[1]]
  expect_identical(iv$routes, ROUTE_IV)
  set <- iv$PK$default
  expect_equal(set$v1, 37.6)
  expect_equal(set$cl1 * 1440, 18.5)
  expect_equal(c(set$cl2, set$cl3), c(0, 0))
  # switch on: anchored at Nicoloff's patients, 0.867752 and 0.899077
  expect_equal(round(ivCohort$volume, 6), 0.867752)
  expect_equal(round(ivCohort$clearance, 6), 0.899077)
  ref <- liothyronine(70, 170, 35, "male")$parallelSystems[[1]]$PK$default
  expect_equal(ref$v1, 37.6 / ivCohort$volume, tolerance = 1e-9)
  expect_equal(ref$cl1 * 1440, 18.5 / ivCohort$clearance, tolerance = 1e-9)
  # and a patient like theirs receives the published values
  mix <- function(f) exp((3 * log(f(70.6, 161, 52.2, "male")) + 3 * log(f(70.6, 161, 52.2, "female"))) / 6)
  ivOf <- function(...) liothyronine(...)$parallelSystems[[1]]$PK$default
  expect_equal(mix(function(...) ivOf(...)$v1), 37.6, tolerance = 1e-12)
  expect_equal(mix(function(...) ivOf(...)$cl1) * 1440, 18.5, tolerance = 1e-12)
})

test_that("a 50 mcg injection: one exponential with a half-life of 1.41 days", {
  dt <- data.frame(Drug = "liothyronine", Time = 0, Dose = 50, Units = "mcg")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", 7 * 1440, FALSE,
                                   adjustToFFM = FALSE)$liothyronine$wide
  t <- c(1, 2, 4, 7) * 1440
  expected <- 50 / 37.6 * exp(-(18.5 / 37.6) * t / 1440)                # ng/mL
  expect_equal(approx(w$Time, w$Plasma, t)$y, expected, tolerance = 1e-4)
  # the oral system receives nothing: the same dose by mouth is a different curve
  po <- simulateDrugsWithCovariates(transform(dt, Units = "mcg PO"), noEvents, 70, 170,
                                    35, "male", 7 * 1440, FALSE,
                                    adjustToFFM = FALSE)$liothyronine$wide
  expect_gt(abs(approx(po$Time, po$Plasma, 1440)$y - expected[1]), 0.05)
})

test_that("clearance: Nicoloff's euthyroid 26.1 L/day against Van Tassell's 25.0", {
  expect_lt(abs(vtCl * 24 / 26.1 - 1), 0.05)
})

test_that("no band, no effect site; oral schedules and an intravenous bolus", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "liothyronine", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(row$Concentration.Units, "ng")
  expect_identical(row$Category, "Other")
  units <- unlist(row$Units)
  expect_setequal(units, c("mcg PO tid", "mcg PO bid", "mcg PO qd", "mcg PO", "mcg"))
  expect_identical(row$Default.Units, "mcg PO tid")
  expect_identical(therapeuticRange("liothyronine")$band_status, "none_established")
  expect_equal(liothyronine(70, 170, 40, "male")$tPeak, 0)
})
