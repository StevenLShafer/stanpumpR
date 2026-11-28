# humateP: see R/drugs_humateP.R and R/vwfFactorVIII.R (Bauer 2023, Table 4
# and the Supplementary Methods, the pdVWF/FVIII model).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
pkOf <- function(...) {
  p <- humateP(...)$PK$default
  c(cl1 = p$cl1, v1 = p$v1, cl2 = p$cl2, v2 = p$v2)
}
bauer <- function(w) c(cl1 = 4.14 / 600 * (w / 75)^0.75, v1 = 4.70 * w / 75,
                       cl2 = 4.47 / 600 * (w / 75)^0.75, v2 = 1.93 * w / 75)

test_that("Table 4: the published values at any weight, switch off", {
  for (w in c(20, 50, 75, 120)) {
    expect_equal(pkOf(w, 170, 40, "female", FALSE), bauer(w))
    expect_equal(humateP(w, 170, 40, "female", FALSE)$factorVIIIResponse$volume, 3.29)
  }
})

test_that("switch on: weight terms at the pharmacokinetic weight, FVIII V anchored at the cohort", {
  size <- pkSizeFactors(110, 165, 50, "female", TRUE)
  expect_equal(pkOf(110, 165, 50, "female", TRUE), bauer(size$pkWeight))
  # a patient like the cohort's (66.3 kg, 30 years) takes 32.9 dL, men and
  # women in the cohort's proportion
  m <- humateP(66.3, 177.5, 30, "male", TRUE)$factorVIIIResponse$volume
  f <- humateP(66.3, 163.9, 30, "female", TRUE)$factorVIIIResponse$volume
  expect_equal(exp((9 * log(m) + 11 * log(f)) / 20), 3.29, tolerance = 1e-9)
  ref <- pkSizeFactors(70, 170, 35, "male", TRUE)$volume
  expect_equal(humateP(70, 170, 35, "male", TRUE)$factorVIIIResponse$volume,
               3.29 * ref / cohortFactorsIndependent(HUMATEP_COHORT)$volume, tolerance = 1e-9)
})

test_that("the checks in the header", {
  p <- getDrugPK("humateP", 75, 175, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(75 / (p$v1 * 10), 1.596, tolerance = 1e-3)
  expect_equal(p$cl1 * 60 * 1000 / 75, 5.52, tolerance = 1e-6)                # mL/h/kg
  expect_equal((p$v1 + p$v2) * 1000 / 75, 88.4, tolerance = 1e-6)             # mL/kg
  expect_equal(log(2) / p$lambda_2 / 60, 12.16, tolerance = 1e-3)
  v <- getDrugPK("vonicogAlfa", 75, 175, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$lambda_2 / v$lambda_2, 1.736, tolerance = 1e-3)              # 1.74, Results
  r <- humateP(75, 175, 35, "male", FALSE)$factorVIIIResponse
  k50 <- r$kout * 60 * (1 - r$imax * 50 / (r$ic50 + 50))
  # 6.1 h with this model's Imax; the supplement's 11.5 h is the rVWF Imax's
  expect_equal(log(2) / k50, 6.10, tolerance = 2e-3)
})

test_that("the product's factor VIII enters FVIII V at once, then the protected pool rises", {
  dt <- data.frame(Drug = "humateP", Time = 0, Dose = 50, Units = "IU/kg")
  o <- simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 96 * 60, FALSE,
                                   adjustToFFM = FALSE)
  f <- o$factorVIIIvwf$wide
  r <- humateP(75, 175, 35, "male", FALSE)$factorVIIIResponse
  before <- r$baseline / (1 - r$imax * 0.5 / (r$ic50 + 0.5))
  expect_equal(f$Plasma[1], before + 3750 / 2.4 / 32.9, tolerance = 1e-9)
  i <- which.max(f$Plasma)
  expect_equal(f$Plasma[i], 61.3, tolerance = 2e-3)
  expect_equal(f$Time[i] / 60, 9.8, tolerance = 0.02)
})

test_that("an infusion of Humate-P agrees with an independent Runge-Kutta integration", {
  PK <- getDrugPK("humateP", 75, 175, 35, "male", adjustToFFM = FALSE)
  dose <- data.frame(Drug = "humateP", Time = c(0, 120, 720), Dose = c(40, 4, 0),
                     Units = c("IU/kg", "IU/kg/hr", "IU/kg/hr"))
  out <- simCpCe(dose, noEvents, PK, 1440, FALSE)
  f <- out$metaboliteSeries
  p <- PK$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  r <- PK$factorVIIIResponse
  # amounts (10 IU) in the two compartments and factor VIII (IU/dL)
  rateIn <- function(t) if (t >= 120 && t < 720) 4 * 75 / 10 / 60 else 0
  d <- function(t, y) {
    v <- 0.5 + y[1] / p$v1
    c(rateIn(t) - (k10 + k12) * y[1] + k21 * y[2],
      k12 * y[1] - k21 * y[2],
      r$kout * r$baseline + rateIn(t) / (r$ratio * r$volume) -
        r$kout * (1 - r$imax * v / (r$ic50 + v)) * y[3])
  }
  h <- 0.05
  times <- seq(0, 1440, by = h)
  y <- c(40 * 75 / 10, 0, f$Cp[1])
  Y <- matrix(NA, length(times), 3); Y[1, ] <- y
  for (i in 2:length(times)) {
    t <- times[i - 1] + 1e-9
    k1 <- d(t, y); k2 <- d(t + h / 2, y + h / 2 * k1)
    k3 <- d(t + h / 2, y + h / 2 * k2); k4 <- d(t + h, y + h * k3)
    y <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    Y[i, ] <- y
  }
  # at the simulation's own points, so that no interpolation of its curve enters
  check <- vapply(c(60, 119, 180, 600, 719, 800, 1400),
                  function(t) out$wide$Time[which.min(abs(out$wide$Time - t))], numeric(1))
  expect_equal(out$wide$Plasma[match(check, out$wide$Time)],
               0.5 + approx(times, Y[, 1], check)$y / p$v1, tolerance = 1e-4)
  expect_equal(f$Cp[match(check, f$Time)], approx(times, Y[, 3], check)$y,
               tolerance = 2e-3)
})

test_that("Vonvendi and Humate-P together are refused, saying why", {
  dt <- data.frame(Drug = c("humateP", "vonicogAlfa"), Time = 0, Dose = 50, Units = "IU/kg")
  expect_error(simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 600, FALSE),
               "one VWF concentrate at a time")
})

test_that("a concentrate left at 0 is not a second concentrate (Devin, PR #1023)", {
  run <- function(dt) simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 600,
                                                  FALSE)$factorVIIIvwf$wide
  alone <- run(data.frame(Drug = "humateP", Time = 0, Dose = 50, Units = "IU/kg"))
  # the startup menu's row at 0, either order
  for (drugs in list(c("humateP", "vonicogAlfa"), c("vonicogAlfa", "humateP")))
  {
    dt <- data.frame(Drug = drugs, Time = 0, Dose = ifelse(drugs == "humateP", 50, 0),
                     Units = "IU/kg")
    both <- run(dt)
    expect_equal(approx(both$Time, both$Plasma, c(60, 300, 590))$y,
                 approx(alone$Time, alone$Plasma, c(60, 300, 590))$y, tolerance = 1e-8)
  }
  # neither given: the patient's own factor VIII, drawn once
  none <- run(data.frame(Drug = c("humateP", "vonicogAlfa"), Time = 0, Dose = 0,
                         Units = "IU/kg"))
  expect_true(all(abs(none$Plasma - none$Plasma[1]) < 1e-8))
  # a second concentrate dosed only after the plot ends is not given in it
  late <- run(data.frame(Drug = c("humateP", "vonicogAlfa"), Time = c(0, 600),
                         Dose = 50, Units = "IU/kg"))
  expect_equal(approx(late$Time, late$Plasma, 300)$y,
               approx(alone$Time, alone$Plasma, 300)$y, tolerance = 1e-8)
})

test_that("two infusion rows at one time give the factor VIII of their sum (Devin, PR #1023)", {
  run <- function(dt) simulateDrugsWithCovariates(dt, noEvents, 75, 175, 35, "male", 600,
                                                  FALSE)$factorVIIIvwf$wide
  one <- run(data.frame(Drug = "humateP", Time = c(0, 240), Dose = c(18, 0),
                        Units = "IU/kg/hr"))
  two <- run(data.frame(Drug = "humateP", Time = c(0, 0, 240), Dose = c(12, 6, 0),
                        Units = "IU/kg/hr"))
  at <- c(60, 200, 400, 590)
  expect_equal(approx(two$Time, two$Plasma, at)$y, approx(one$Time, one$Plasma, at)$y,
               tolerance = 1e-8)
})
