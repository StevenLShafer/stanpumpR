# vonicogAlfa: see R/drugs_vonicogAlfa.R and R/vwfFactorVIII.R (Bauer 2023,
# Table 4 and the Supplementary Methods).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
pkOf <- function(...) {
  p <- vonicogAlfa(...)$PK$default
  c(cl1 = p$cl1, v1 = p$v1, cl2 = p$cl2, v2 = p$v2)
}
bauer <- function(w) c(cl1 = 2.10 / 600 * (w / 75)^0.75, v1 = 4.35 * w / 75,
                       cl2 = 2.29 / 600 * (w / 75)^0.75, v2 = 1.58 * w / 75)

test_that("Table 4: the published values at any weight, switch off", {
  for (w in c(20, 50, 75, 120)) expect_equal(pkOf(w, 170, 40, "female", FALSE), bauer(w))
})

test_that("switch on: the source's weight terms at the pharmacokinetic weight", {
  size <- pkSizeFactors(110, 165, 50, "female", TRUE)
  expect_equal(pkOf(110, 165, 50, "female", TRUE), bauer(size$pkWeight))
  expect_equal(pkOf(70, 170, 35, "male", TRUE), bauer(70))
})

test_that("the checks in the header", {
  p <- pkOf(75, 175, 35, "male", FALSE)
  expect_equal(75 / (p[["v1"]] * 10), 1.724, tolerance = 1e-3)          # IU/dL per IU/kg
  expect_equal(p[["cl1"]] * 60 * 10 / 75, 0.028, tolerance = 1e-6)        # dL/kg/h
  PK <- getDrugPK("vonicogAlfa", 75, 175, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / c(PK$lambda_1, PK$lambda_2) / 60, c(3.254, 21.10), tolerance = 1e-3)
  r <- vonicogAlfa(75, 175, 35, "male", FALSE)$factorVIIIResponse
  k50 <- r$kout * 60 * (1 - r$imax * 50 / (r$ic50 + 50))
  expect_equal(k50, 0.0527, tolerance = 1e-3)                           # /h
  expect_equal(r$baseline, 0.5 * (0.4 / 0.39)^-0.571)
  expect_equal(r$baseline / (1 - r$imax * 0.5 / (r$ic50 + 0.5)), 4.174, tolerance = 1e-3)
})

test_that("the VWF baseline is plotted and drives factor VIII", {
  PK <- getDrugPK("vonicogAlfa", 75, 175, 35, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 0.5)
  expect_identical(PK$metaboliteName, "factorVIIIvwf")
  dose <- data.frame(Drug = "vonicogAlfa", Time = 60, Dose = 50, Units = "IU/kg")
  out <- simCpCe(dose, noEvents, PK, 600, FALSE)
  w <- out$wide
  expect_equal(w$Plasma[w$Time < 60], rep(0.5, sum(w$Time < 60)))
  expect_equal(max(w$Plasma), 0.5 + 3750 / 43.5, tolerance = 1e-6)
  f <- out$metaboliteSeries
  expect_equal(f$Cp[f$Time < 60], rep(4.1742, sum(f$Time < 60)), tolerance = 1e-4)
  PK2 <- getDrugPK("vonicogAlfa", 75, 175, 35, "male", adjustToFFM = FALSE,
                   factorBaselines = list(vonWillebrandFactorBaseline = 20))
  out2 <- simCpCe(dose, noEvents, PK2, 600, FALSE)
  expect_equal(out2$wide$Plasma - w$Plasma, rep(19.5, nrow(w)))
  r <- PK2$factorVIIIResponse
  expect_equal(out2$metaboliteSeries$Cp[1], r$baseline / (1 - r$imax * 20 / (r$ic50 + 20)))
  expect_true("vonicogAlfa" %in% patientFieldReaders("vonWillebrandFactorBaseline"))
})

test_that("factor VIII agrees with an independent Runge-Kutta integration", {
  PK <- getDrugPK("vonicogAlfa", 75, 175, 35, "male", adjustToFFM = FALSE)
  dose <- data.frame(Drug = "vonicogAlfa", Time = c(0, 1440), Dose = c(50, 30), Units = "IU/kg")
  out <- simCpCe(dose, noEvents, PK, 4320, FALSE)
  w <- out$wide; f <- out$metaboliteSeries
  # The VWF:RCo curve exactly, from the two-compartment solution on its own
  p <- PK$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  b <- k10 + k12 + k21
  l <- c((b + sqrt(b^2 - 4 * k10 * k21)) / 2, (b - sqrt(b^2 - 4 * k10 * k21)) / 2)
  A <- c((k21 - l[1]) / (l[2] - l[1]), (k21 - l[2]) / (l[1] - l[2])) / p$v1
  vwf <- function(t) {
    total <- 0.5
    for (i in seq_len(nrow(dose))) {
      s <- t - dose$Time[i]
      if (s >= 0) total <- total + dose$Dose[i] * 75 / 10 * sum(A * exp(-l * s))
    }
    total
  }
  expect_equal(vwf(10), approx(w$Time, w$Plasma, 10)$y, tolerance = 1e-6)
  r <- PK$factorVIIIResponse
  dF <- function(t, F) r$kout * r$baseline - r$kout * (1 - r$imax * vwf(t) / (r$ic50 + vwf(t))) * F
  h <- 0.05
  times <- seq(0, 4320, by = h)
  F <- numeric(length(times)); F[1] <- f$Cp[1]
  for (i in 2:length(times)) {
    t <- times[i - 1]; y <- F[i - 1]
    # a dose lands on a step's end, so the step's right side is taken just after it
    k1 <- dF(t + 1e-9, y); k2 <- dF(t + h / 2, y + h / 2 * k1)
    k3 <- dF(t + h / 2, y + h / 2 * k2); k4 <- dF(t + h, y + h * k3)
    F[i] <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  check <- c(30, 240, 1200, 1439, 1500, 2880, 4300)
  expect_equal(approx(f$Time, f$Cp, check)$y, approx(times, F, check)$y, tolerance = 2e-3)
})

test_that("factorVIIIvwf is drawn as a panel of its own and cannot be dosed", {
  dt <- data.frame(Drug = "vonicogAlfa", Time = 0, Dose = 50, Units = "IU/kg")
  o <- simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 96 * 60, FALSE,
                                   adjustToFFM = FALSE)
  expect_setequal(names(o), c("vonicogAlfa", "factorVIIIvwf"))
  f <- o$factorVIIIvwf$wide
  i <- which.max(f$Plasma)
  expect_equal(f$Plasma[i], 98.2, tolerance = 2e-3)
  expect_equal(f$Time[i] / 60, 28.4, tolerance = 0.02)
  expect_false(isDosableDrug("factorVIIIvwf"))
})

test_that("the source's 72-hour simulation: factor VIII stays above 40 IU/dL", {
  dt <- data.frame(Drug = "vonicogAlfa", Time = (0:5) * 72 * 60, Dose = 50, Units = "IU/kg")
  o <- simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 6 * 72 * 60, FALSE,
                                   adjustToFFM = FALSE)
  f <- o$factorVIIIvwf$wide
  troughs <- approx(f$Time, f$Plasma, (1:6) * 72 * 60 - 0.02)$y
  expect_true(all(troughs > 40))
  expect_equal(troughs[6], 45.2, tolerance = 2e-3)
})

test_that("a factorVIIIResponse block is checked", {
  ok <- list(name = "x", kout = 1, imax = 0.5, ic50 = 1, baseline = 1, vwfBaseline = 1)
  expect_identical(validateFactorVIIIResponse(ok, "d"), ok)
  expect_error(validateFactorVIIIResponse(modifyList(ok, list(imax = 1)), "d"), "below 1")
  expect_error(validateFactorVIIIResponse(modifyList(ok, list(kout = NA)), "d"), "kout")
})

# Devin's review of PR #1019 (2026-11-28), each failing before its fix
test_that("factor VIII does not depend on the length of the plot's grid", {
  dt <- data.frame(Drug = "vonicogAlfa", Time = 0, Dose = 50, Units = "IU/kg")
  run <- function(max) simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", max,
                                                    FALSE, adjustToFFM = FALSE)$factorVIIIvwf$wide
  short <- run(96 * 60)
  year <- run(365 * 1440)
  t <- year$Time[year$Time > 60 & year$Time < 90 * 60]
  expect_equal(year$Plasma[match(t, year$Time)], approx(short$Time, short$Plasma, t)$y,
               tolerance = 1e-3)
})

test_that("normalised factor VIII starts at zero, and its time until threshold is unavailable", {
  dt <- data.frame(Drug = "vonicogAlfa", Time = 60, Dose = 50, Units = "IU/kg")
  o <- simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 600, TRUE,
                                   adjustToFFM = FALSE,
                                   factorBaselines = list(vonWillebrandFactorBaseline = 20))
  r <- o$factorVIIIvwf$results
  norm <- r$Y[r$Site == "CpNormCp" & r$Time < 60]
  expect_true(length(norm) > 0)
  expect_equal(norm, rep(0, length(norm)))
  expect_true(all(is.na(o$factorVIIIvwf$wide$Recovery)))
})

test_that("the help pages: Vonvendi is not a prodrug, factor VIII names its parent and no PK table", {
  dd <- getDrugDefaultsGlobal()
  expect_identical(helpParentDrugs("factorVIIIvwf", dd), "vonicogAlfa")
  expect_false(grepl("prodrug", helpIvDrugPageHTML("vonicogAlfa", getDrugDefaults("vonicogAlfa"), dd)))
  page <- helpIvDrugPageHTML("factorVIIIvwf", getDrugDefaults("factorVIIIvwf"), dd)
  expect_true(grepl("placeholder that nothing simulates", page))
  expect_false(grepl("Volumes and clearances", page))
})
