# Hydroxychloroquine (R/drugs_hydroxychloroquine.R): Carmichael 2003, Table 2,
# fit (d).  Drafted by Claude Code, 2026-11-28.  Expectations are computed here
# from the paper and the label, not from the drug file.

hcqSim <- function(dose, units, minutes, weight = 70, height = 170, age = 35,
                   sex = "male", ...) {
  dt <- data.frame(Drug = "hydroxychloroquine", Time = 0, Dose = dose, Units = units)
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  simulateDrugsWithCovariates(dt, events, weight, height, age, sex, minutes, FALSE,
                              ...)$hydroxychloroquine$wide
}

dayAverage <- function(x, day) {
  h <- x$Time / 60
  k <- h >= (day - 1) * 24 & h <= day * 24
  sum(diff(h[k]) * (head(x$Plasma[k], -1) + tail(x$Plasma[k], -1)) / 2) / 24
}

test_that("Carmichael 2003, Table 2 (d), switch off: the published values for everyone", {
  for (w in c(45, 70, 130)) for (sex in c("male", "female")) {
    set <- hydroxychloroquine(w, 165, 60, sex, adjustToFFM = FALSE)$PK$default
    expect_equal(set$cl1, 9.89 / 60)
    expect_equal(set$v1, 605)
    expect_equal(set$ka_PO, 0.765 / 60)
    expect_equal(set$tlag_PO, 0.445 * 60)
    # F 0.746 of the base; a 200 mg tablet of the sulfate holds 155 mg of base
    expect_equal(set$bioavailability_PO, 0.746 * 155 / 200)
    expect_equal(c(set$v2, set$v3, set$cl2, set$cl3), c(1, 1, 0, 0))
  }
})

test_that("switch on: anchored at Carmichael's cohort (Table 1)", {
  cohort <- list(groups = list(
    list(weight = 69.2, height = 174.1, age = 53.9, men = 23, women = 0),
    list(weight = 69.2, height = 161.2, age = 53.9, men = 0, women = 51),
    list(weight = 71.0, height = 173.6, age = 56.8, men = 12, women = 0),
    list(weight = 71.0, height = 160.8, age = 56.8, men = 0, women = 37)))
  expect_equal(HYDROXYCHLOROQUINE_COHORT, cohort)
  kc <- cohortFactorsIndependent(cohort)
  # the reference man: his factors are 1, so he receives the published values
  # over the cohort's factors
  set <- hydroxychloroquine(70, 170, 35, "male")$PK$default
  expect_equal(set$v1, 605 / kc$volume)
  expect_equal(set$cl1, 9.89 / 60 / kc$clearance)
  expect_equal_rounded(set$v1, 717.01916)
  expect_equal_rounded(set$cl1 * 60, 11.23383)
  # a woman like the cohort's is close to the published values
  f <- hydroxychloroquine(70, 161, 55, "female")$PK$default
  expect_lt(abs(f$v1 / 605 - 1), 0.08)
  expect_lt(abs(f$cl1 * 60 / 9.89 - 1), 0.06)
  # ka, the lag and F are not size quantities
  expect_equal(c(set$ka_PO, set$tlag_PO, set$bioavailability_PO),
               c(0.765 / 60, 0.445 * 60, 0.746 * 0.775))
})

test_that("the paper's steady state: 487 and 974 ug/L at 155 and 310 mg of base a day", {
  # 200 mg of the sulfate is 155 mg of base (Discussion: 487 and 974 ug/L)
  expect_equal(round(0.746 * 155e3 / (9.89 * 24)), 487)
  one <- hcqSim(200, "mg PO qd", 40 * 1440, adjustToFFM = FALSE)
  two <- hcqSim(400, "mg PO qd", 40 * 1440, adjustToFFM = FALSE)
  expect_lt(abs(dayAverage(one, 40) / 487 - 1), 0.002)
  expect_lt(abs(dayAverage(two, 40) / 974 - 1), 0.002)
  # the steady state is linear in the dose
  expect_equal(dayAverage(two, 40), 2 * dayAverage(one, 40), tolerance = 1e-6)
})

test_that("the first dose against an independent closed form", {
  # one compartment, first-order absorption after a lag: Bateman
  ka <- 0.765; ke <- 9.89 / 605; lag <- 0.445
  amount <- 200 * 0.775 * 0.746 * 1e3 / 605          # ug/L = ng/mL
  bateman <- function(t) ifelse(t <= lag, 0,
    amount * ka / (ka - ke) * (exp(-ke * (t - lag)) - exp(-ka * (t - lag))))
  x <- hcqSim(200, "mg PO", 2880, adjustToFFM = FALSE)
  expect_equal(x$Plasma, bateman(x$Time / 60), tolerance = 1e-6)
  # the peak the header quotes: 176 ng/mL at 5.6 h (the label: 129.6 at 3.3 h)
  tmax <- lag + log(ka / ke) / (ka - ke)
  expect_equal(round(tmax, 1), 5.6)
  expect_equal(round(bateman(tmax)), 176)
  expect_equal(round(max(x$Plasma)), 176)
  # nothing before the lag
  expect_true(all(x$Plasma[x$Time < lag * 60] == 0))
})

test_that("the half-life is the paper's middle phase, 42.4 h", {
  set <- hydroxychloroquine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) * set$v1 / set$cl1 / 60, 1), 42.4)
})

test_that("mg/kg PO is the label's paediatric unit", {
  a <- hcqSim(6.5, "mg/kg PO", 1440, weight = 40, height = 150, age = 12)
  b <- hcqSim(260, "mg PO", 1440, weight = 40, height = 150, age = 12)
  expect_equal(a$Plasma, b$Plasma)
})

test_that("no band, no effect site", {
  X <- hydroxychloroquine(70, 170, 35, "male")
  expect_equal(c(X$tPeak, X$MEAC, X$typical, X$lowerTypical, X$upperTypical), rep(0, 5))
  expect_match(X$reference, "whole blood")
})
