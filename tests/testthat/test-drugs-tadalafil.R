# Tadalafil (R/drugs_tadalafil.R): Ferguson-Sells 2022's combined adult and
# paediatric model in pulmonary arterial hypertension.  Drafted by Claude
# Code, 2026-11-15.  Expectations are computed here from Table 2, not from the
# drug file.

KA <- 0.860; CLBOS <- 3.23; NOBOS <- -0.418; V <- 88.1

events <- data.frame(Time = double(), Event = character())
run <- function(dose, age = 54) simulateDrugsWithCovariates(
  data.frame(Drug = "tadalafil", Time = 0, Dose = dose, Units = "mg PO"),
  events, 70, 170, age, "male", 14400, FALSE, adjustToFFM = FALSE)$tadalafil$wide
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)

test_that("Table 2, switch off", {
  X <- tadalafil(55, 160, 52.4, "female", adjustToFFM = FALSE)
  set <- X$PK$default
  expect_equal(set$cl1 * 60, 1.88, tolerance = 0.001)   # "(calculated) 1.88"
  expect_equal(set$cl1, CLBOS * (1 + NOBOS) / 60)
  expect_equal(set$v1, V * 55 / 70)
  expect_equal(set$ka_PO, KA / 60)
  expect_equal(set$bioavailability_PO, 1)                # age 52.4
  expect_equal(X$oralSaturation, list(form = "power", Dref = 16.27, exponent = -0.227))
  # the age term
  expect_equal(tadalafil(70, 170, 80, "male", FALSE)$PK$default$bioavailability_PO,
               (80 / 52.4)^0.100)
})

test_that("the age term is held at 2.5 years, the youngest studied", {
  f <- function(age) tadalafil(15, 100, age, "male", FALSE)$PK$default$bioavailability_PO
  expect_equal(f(0), (2.5 / 52.4)^0.100)
  expect_equal(f(1), f(2.5))
  expect_gt(f(0), 0)
})

test_that("Table 3: a 70-kg adult of 54 on 40 mg (with bosentan) has CL/F 3.95", {
  F40 <- (40 / 16.27)^-0.227 * (54 / 52.4)^0.100
  expect_equal(round(CLBOS / F40, 2), 3.95)     # the source's simulated median 4.05
  expect_equal(round(V / F40), 108)             # 107
})

test_that("the simulated AUC is dose x F / CL", {
  set <- tadalafil(70, 170, 54, "male", FALSE)$PK$default
  for (dose in c(20, 40)) {
    Fd <- (dose / 16.27)^-0.227 * (54 / 52.4)^0.100
    expect_equal(auc(run(dose)) / 60, dose * 1000 * Fd / (set$cl1 * 60), tolerance = 0.01)
  }
  # half-life 32.5 h (Adcirca label: 35 h in pulmonary hypertension)
  expect_equal(round(log(2) * set$v1 / set$cl1 / 60, 1), 32.5)
})

test_that("switch on: the weight term at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 70 x 1.3049067
  set <- tadalafil(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, V * 1.3049067)
  expect_equal(set$cl1, CLBOS * (1 + NOBOS) / 60)
})
