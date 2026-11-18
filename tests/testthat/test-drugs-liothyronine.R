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

test_that("switch on: volumes by fat-free mass, clearances by its 0.75 power", {
  ffm <- ffmAlSallami(120, 170, 50, "male") / ffmAlSallami(70, 170, 35, "male")
  set <- liothyronine(120, 170, 50, "male")$PK$default
  expect_equal(set$v1, 8.67 * ffm)
  expect_equal(set$cl1 * 60, vtCl * ffm^0.75)
  # the reference man is the published model in both positions
  expect_equal(liothyronine(70, 170, 35, "male")$PK, liothyronine(70, 170, 35, "male", FALSE)$PK)
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

test_that("oral only, no band, no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "liothyronine", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(row$Concentration.Units, "ng")
  expect_identical(row$Category, "Other")
  units <- unlist(row$Units)
  expect_true(all(grepl("PO", units)))
  expect_identical(therapeuticRange("liothyronine")$band_status, "none_established")
  expect_equal(liothyronine(70, 170, 40, "male")$tPeak, 0)
})
