# Shafer SL et al., Br J Anaesth 2025;134:1513-1522, Table 1 (read in full).

noEvents <- data.frame(Time = numeric(0), Event = character(0))

sim <- function(d, weight = 70, height = 170, age = 35, sex = "male",
                maximum = 240, adjustToFFM = TRUE) {
  x <- simulateDrugsWithCovariates(d, noEvents, weight, height, age, sex,
                                   maximum, FALSE,
                                   adjustToFFM = adjustToFFM)$oxytocin$results
  x[x$Site == "Plasma", ]
}
infusion <- function(dose, minutes)
  data.frame(Drug = "oxytocin", Time = c(0, minutes),
             Dose = c(dose / minutes, 0), Units = "mcg/min")
peak <- function(x) max(x$Y)

test_that("the intravenous model is Table 1's Model D at the paper's reference", {
  # 80 kg, 33 years, total weight: every factor is 1
  x <- oxytocin(80, 175, 33, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1, 0.86)
  expect_equal(x$v1, 8.2)
  expect_equal(x$cl2, 0.22)
  expect_equal(x$v2, 5.9)
  expect_equal(x$cl3, 0)
})

test_that("weight is proportional and age lowers clearance", {
  a <- oxytocin(60, 175, 33, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a$v1, 8.2 * 60 / 80)
  expect_equal(a$cl2, 0.22 * 60 / 80)
  b <- oxytocin(80, 175, 66, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(b$cl1, 0.86 * 2^-0.17)
  expect_equal(b$v1, 8.2)
})

test_that("the weight terms use the pharmacokinetic weight with the switch on", {
  # The reference man's pharmacokinetic weight is 70 kg
  x <- oxytocin(70, 170, 35, "male")$PK$default
  expect_equal_rounded(
    unlist(x[c("v1", "v2", "cl1", "cl2")]),
    c(v1 = 7.175, v2 = 5.1625, cl1 = 0.7450, cl2 = 0.1925))
  # A woman of the same weight has a lower pharmacokinetic weight
  w <- pkSizeFactors(70, 170, 35, "female")$pkWeight
  y <- oxytocin(70, 170, 35, "female")$PK$default
  expect_equal(y$v1, 8.2 * w / 80)
  expect_lt(y$v1, x$v1)
  # Off, total weight, and sex has no effect
  z <- oxytocin(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(z$v1, 8.2 * 70 / 80)
})

test_that("the age term is held at 18 and stays finite at 0", {
  a0  <- oxytocin(70, 170, 0, "male", adjustToFFM = FALSE)$PK$default
  a18 <- oxytocin(70, 170, 18, "male", adjustToFFM = FALSE)$PK$default
  expect_true(is.finite(a0$cl1))
  expect_equal(a0$cl1, a18$cl1)
  expect_equal(a18$cl1, 0.86 * 70 / 80 * (18 / 33)^-0.17)
})

test_that("the intranasal route is not offered", {
  # Shafer's decision of 2026-11-16: the paper's intranasal model is not used
  x <- oxytocin(70, 170, 35, "male")
  expect_null(x$parallelSystems)
  expect_null(x$PK$default$ka_IN)
  units <- getDrugDefaultsGlobal()
  units <- units$Units[units$Drug == "oxytocin"][[1]]
  expect_false(any(doseRoute(units) == ROUTE_IN))
})

test_that("a bolus gives dose / CL", {
  # A 10 mcg bolus: AUC = dose / CL of the intravenous model
  y <- sim(data.frame(Drug = "oxytocin", Time = 0, Dose = 10, Units = "mcg"),
           maximum = 600)
  p <- getDrugPK("oxytocin", 70, 170, 35, "male")$PK$default
  auc <- sum(diff(y$Time) * (head(y$Y, -1) + tail(y$Y, -1)) / 2) +
    y$Y[nrow(y)] / min(p$lambda_1, p$lambda_2)
  expect_equal(auc, 10 / p$cl1, tolerance = 0.01)
})

test_that("the model reproduces the check the paper prints", {
  # Discussion: 8.4 mcg IV over 30 s and over 5 min, the paper's simulations
  # 900 and 680 pg/mL (measured 1000 and 700).  At the paper's 80 kg,
  # 33-year-old reference with total weight, 10 to 11% above them.
  fast <- sim(infusion(8.4, 0.5), 80, 175, 33, adjustToFFM = FALSE)
  slow <- sim(infusion(8.4, 5), 80, 175, 33, adjustToFFM = FALSE)
  expect_equal(peak(fast), 0.9, tolerance = 0.12)
  expect_equal(peak(slow), 0.68, tolerance = 0.12)
  expect_equal(c(peak(fast), peak(slow)), c(0.99143, 0.75335), tolerance = 1e-5)
})

test_that("a weight of 1 kg or less selects the rat model", {
  x <- oxytocin(0.9, 171, 50, "male")
  expect_equal(x$PK$default,
               list(v1 = 0.10, v2 = 0.02, v3 = 1, cl1 = 0.017,
                    cl2 = 0.000524, cl3 = 0))
  expect_null(x$parallelSystems)
  expect_match(x$reference, "Tanaka A et al")
})

test_that("the effect site and band are unchanged", {
  x <- oxytocin(70, 170, 35, "male")
  expect_equal(x$tPeak, 5)
  expect_equal(c(x$lowerTypical, x$typical, x$upperTypical), c(0.05, 0.1, 0.2))
  expect_match(x$reference, "Shafer SL et al., Br J Anaesth 2025")
})

test_that("doses in USP units are the same doses in micrograms, 600 units per mg", {
  # Shafer's decision of 2026-11-16: oxytocin is dosed in USP units
  expect_equal(oxytocin(70, 170, 35, "male")$unitPotency, 1000 / 600)
  same <- function(a, b) {
    x <- sim(a); y <- sim(b)
    expect_equal(x$Y, y$Y, tolerance = 1e-10)
  }
  # 10 U bolus = 16.667 mcg
  same(data.frame(Drug = "oxytocin", Time = 0, Dose = 10, Units = "U"),
       data.frame(Drug = "oxytocin", Time = 0, Dose = 10000 / 600, Units = "mcg"))
  # 6 U/hr for 2 h = 10 mcg/hr = 1/6 mcg/min
  same(data.frame(Drug = "oxytocin", Time = c(0, 120), Dose = c(6, 0), Units = "U/hr"),
       data.frame(Drug = "oxytocin", Time = c(0, 120), Dose = c(1 / 6, 0), Units = "mcg/min"))
  # 2 mU/min, a labour infusion, = 2/600 mcg/min
  same(data.frame(Drug = "oxytocin", Time = c(0, 120), Dose = c(2, 0), Units = "mU/min"),
       data.frame(Drug = "oxytocin", Time = c(0, 120), Dose = c(2 / 600, 0), Units = "mcg/min"))
  # The units the dose table offers, U first
  d <- getDrugDefaultsGlobal()
  d <- d[d$Drug == "oxytocin", ]
  expect_equal(d$Default.Units, "U")
  expect_true(all(c("U", "U/hr", "U/min", "mU/min") %in% d$Units[[1]]))
})

test_that("potencyToMass converts only unit and milliunit doses", {
  x <- potencyToMass(c(10, 2, 5, 3), c("U", "mU/min", "mcg", "U/kg/hr"), 1000 / 600)
  expect_equal(x$Units, c("mcg", "mcg/min", "mcg", "mcg/kg/hr"))
  expect_equal(x$Dose, c(10000 / 600, 2 / 600, 5, 3000 / 600))
  y <- potencyToMass(c(10, 2), c("U", "mU/min"), NULL)
  expect_equal(y$Units, c("U", "mU/min"))
})

test_that("the rat model takes doses in units too, and mU is refused without a potency", {
  # CodeRabbit's review of PR #742: the rat model lacked unitPotency, so a
  # dose in U stopped the run; and a drug without one read mU/min as mass.
  rat <- function(units, dose)
    sim(data.frame(Drug = "oxytocin", Time = 0, Dose = dose, Units = units),
        weight = 0.3, height = 20, age = 0.2)
  expect_equal(rat("U", 10)$Y, rat("mcg", 10000 / 600)$Y, tolerance = 1e-10)
  pk <- getDrugPK("propofol", 70, 170, 35, "male")
  expect_error(
    simCpCe(data.frame(Drug = "propofol", Time = c(0, 10), Dose = c(5, 0),
                       Units = "mU/min"), noEvents, pk, 60, FALSE),
    "milliunits")
})
