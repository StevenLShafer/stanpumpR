# Levothyroxine (R/drugs_levothyroxine.R): Inada 1975's one-compartment
# (extrapolation) analysis, Hays 1991's absorption rate and lag without his
# subject 1, and Surks 1973's bioavailability (Shafer, 2026-11-18).  Drafted by Claude Code, 2026-11-18.
# Expectations are computed here from the papers, not from the drug file.

mwT4 <- 776.87 / 798.86            # thyroxine per levothyroxine sodium
vInada  <- function(w) 8.4 * w / 60   # L, Inada Table II, per 60 kg
clInada <- function(w) 1.06 * w / 60  # L/day
# Hays 1991 Table 1, subjects 2-5: k12,7 + k0,7, and DT9
kaHays  <- mean(c(0.0087, 0.0077, 0.0098, 0.0113)) + mean(c(0.0042, 0.0028, 0.0040, 0.0040))
lagHays <- mean(c(4.6, 15.8, 8.2, 32.4))
fSurks  <- 0.480                       # Surks 1973 Table I, tablets at steady state

test_that("Inada 1975 and Hays 1991, switch off", {
  set <- levothyroxine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$v1, 9.8 / mwT4)
  expect_equal(set$cl1, 1.06 * 70 / 60 / mwT4 / 1440)
  expect_equal_rounded(set$v1, 10.077398)
  expect_equal_rounded(set$cl1, 0.00088310530)
  expect_equal(set$ka_PO, 0.013125)
  expect_equal(set$tlag_PO, 15.25)
  expect_equal(set$bioavailability_PO, 0.48)
  expect_equal(c(set$cl2, set$cl3), c(0, 0))
  # half-life 5.49 days, the ratio of the printed means
  expect_equal(round(log(2) * set$v1 / set$cl1 / 1440, 2), 5.49)
  # in proportion to weight, as Inada adjusted
  small <- levothyroxine(35, 150, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(small$v1, vInada(35) / mwT4)
  expect_equal(small$cl1, clInada(35) / mwT4 / 1440)
})

test_that("Surks 1973: one bioavailability at every age", {
  f <- function(age) levothyroxine(70, 170, age, "male")$PK$default$bioavailability_PO
  expect_equal(c(f(1), f(18), f(70), f(85)), rep(fSurks, 4))
})

test_that("switch on: volume and clearance by fat-free mass", {
  # 120 kg, 170 cm, 50 y man: volumes x 1.3049067, clearances x 1.2209126
  set <- levothyroxine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, 9.8 * 1.3049067 / mwT4)
  expect_equal_rounded(set$cl1, 1.06 * 70 / 60 * 1.2209126 / mwT4 / 1440)
})

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

test_that("an intravenous bolus starts at the T4 it delivers over the volume", {
  dt <- data.frame(Drug = "levothyroxine", Time = 0, Dose = 500, Units = "mcg")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 40, "male", 1440, FALSE,
                                   adjustToFFM = FALSE)$levothyroxine$wide
  k <- clInada(70) / vInada(70) / 1440
  at <- w$Time[w$Time > 0][1]
  expect_equal(w$Plasma[w$Time == at], 500 * mwT4 / vInada(70) * exp(-k * at),
               tolerance = 1e-6)
})

test_that("XP-8121's oral Synthroid arm (Fitch 2025, Table 2), not used in the model", {
  # 600 mcg at the part-1 mean weight, 77.7 kg: the closed form gives
  # Cmax 24.89 ng/mL at 6.66 h, against the measured 47.6 ng/mL (median tmax 3.1 h)
  dt <- data.frame(Drug = "levothyroxine", Time = 0, Dose = 600, Units = "mcg PO")
  w <- simulateDrugsWithCovariates(dt, events, 77.7, 170, 41, "male", 24 * 60, FALSE,
                                   adjustToFFM = FALSE)$levothyroxine$wide
  V <- vInada(77.7); k <- clInada(77.7) / V / 1440
  t <- seq(lagHays, 24 * 60, by = 0.5) - lagHays
  closed <- fSurks * 600 * mwT4 * kaHays / (V * (kaHays - k)) * (exp(-k * t) - exp(-kaHays * t))
  expect_equal(max(w$Plasma), max(closed), tolerance = 1e-3)
  expect_equal(round(max(closed), 1), 24.9)
  expect_equal(round(max(closed) / 47.6 - 1, 2), -0.48)
})

test_that("average steady state is bioavailable T4 per day over clearance", {
  # 100 mcg a day at 70 kg: 0.48 x 100 x 0.97247 / 1.2367 L/day = 37.7 ng/mL
  dt <- data.frame(Drug = "levothyroxine", Time = 0, Dose = 100, Units = "mcg PO qd")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 40, "male", 60 * 1440, FALSE,
                                   adjustToFFM = FALSE)$levothyroxine$wide
  css <- fSurks * 100 * mwT4 / clInada(70)
  expect_equal(round(css, 1), 37.7)
  day <- seq(59 * 1440, 60 * 1440, by = 1)
  avg <- mean(approx(w$Time, w$Plasma, day)$y)
  # day 59-60 is 10.7 half-lives in: within 0.1% of the plateau
  expect_equal(avg, css, tolerance = 2e-3)
})

test_that("Inada's T4-treated patients: 150 mcg a day, measured 9.5 mcg/dL", {
  # mean weight 46.3 kg (47, 44, 48), ages 52-60
  w <- mean(c(47, 44, 48))
  cssDL <- fSurks * 150 * mwT4 / clInada(w) / 10
  expect_equal(round(cssDL, 2), 8.55)
  expect_equal(round(fSurks * 9.5 / cssDL, 2), 0.53)   # the bioavailability it implies
})

test_that("no band, no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "levothyroxine", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(row$Concentration.Units, "ng")
  expect_identical(therapeuticRange("levothyroxine")$band_status, "none_established")
  expect_equal(levothyroxine(70, 170, 40, "male")$tPeak, 0)
})
