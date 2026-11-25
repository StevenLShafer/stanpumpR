# emicizumab: see R/drugs_emicizumab.R (Retout 2020, Table 4 and its equations).
test_that("Table 4 at the reference person (70 kg, 45 g/L, 30 years)", {
  pk <- emicizumab(70, 175, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(pk$cl1 * 1440, 0.272)
  expect_equal(pk$v1, 10.4)
  expect_equal(pk$ka_SC * 1440, 0.536)
  expect_equal(pk$bioavailability_SC, 1)
  expect_equal(c(pk$cl2, pk$cl3), c(0, 0))
  # elimination and absorption half-lives (days)
  expect_equal(log(2) * pk$v1 / (pk$cl1 * 1440), 26.5, tolerance = 0.002)
  expect_equal(log(2) / (pk$ka_SC * 1440), 1.29, tolerance = 0.003)
})

test_that("the paper's covariate equations", {
  f <- function(w, a = 30, alb = NULL, on = FALSE)
    emicizumab(w, 175, a, "male", on, albumin = alb)$PK$default
  # weight, as fitted, at total weight with the switch off
  expect_equal(f(20)$cl1 * 1440, 0.272 * (20 / 70)^0.911)
  expect_equal(f(20)$v1, 10.4 * 20 / 70)
  # and at the pharmacokinetic weight with it on
  size <- pkSizeFactors(120, 175, 30, "male", TRUE)
  expect_equal(f(120, on = TRUE)$cl1 * 1440, 0.272 * (size$pkWeight / 70)^0.911)
  expect_equal(f(120, on = TRUE)$v1, 10.4 * size$pkWeight / 70)
  # albumin in g/dL from the Patient Profile, g/L in the equation
  expect_equal(f(70, alb = 3.3)$cl1 * 1440, 0.272 * (1 - 0.0157 * (33 - 45)))
  expect_equal(f(70, alb = 5.5)$cl1 * 1440, 0.272 * (1 - 0.0157 * (55 - 45)))
  # held within the studied 16.8-56.6 g/L
  expect_equal(f(70, alb = 0.5)$cl1, f(70, alb = 1.68)$cl1)
  expect_equal(f(70, alb = 6)$cl1, f(70, alb = 5.66)$cl1)
  # age on F above 30, held at 77
  expect_equal(f(70, a = 5)$bioavailability_SC, 1)
  expect_equal(f(70, a = 50)$bioavailability_SC, 1 - 0.00651 * 20)
  expect_equal(f(70, a = 77)$bioavailability_SC, 1 - 0.00651 * 47)
  expect_equal(f(70, a = 90)$bioavailability_SC, f(70, a = 77)$bioavailability_SC)
})

test_that("Results 3.2: Cav,ss on 1.5 mg/kg weekly", {
  cav <- function(alb = NULL, age = 30) {
    pk <- emicizumab(70, 175, age, "male", FALSE, albumin = alb)$PK$default
    105 * pk$bioavailability_SC / (pk$cl1 * 1440 * 7)
  }
  expect_equal(round(cav(), 1), 55.1)
  expect_equal(round(cav(alb = 3.3), 1), 46.4)
  # the paper's 57 g/L lies just past the studied maximum, 56.6, where the
  # model holds albumin: the equation itself gives 67.9 (paper 68.0)
  expect_equal(105 / (0.272 * (1 - 0.0157 * 12) * 7), 68.0, tolerance = 0.002)
  expect_equal(cav(alb = 5.7), cav(alb = 5.66))
  expect_equal(1 - cav(age = 77) / cav(), 0.31, tolerance = 0.02)
})

test_that("the simulated steady state matches Cav,ss and the label's troughs", {
  PK <- getDrugPK("emicizumab", 70, 175, 30, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "emicizumab", Time = 0, Dose = 1.5,
                          Units = "mg/kg SC qwk"),
               data.frame(Time = numeric(0), Event = character(0)), PK,
               30 * MINS_PER_WEEK, FALSE)$wide
  last <- w$Time >= 29 * MINS_PER_WEEK
  t <- w$Time[last]; cp <- w$Plasma[last]
  avg <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / diff(range(t))
  expect_equal(avg, 105 / (0.272 * 7), tolerance = 0.03)  # near steady state at 30 weeks
  expect_equal(min(cp), 51.2, tolerance = 0.15)            # label Table 4 mean trough
  expect_true("emicizumab" %in% LONG_TERM_DRUGS)
})

test_that("every two and every four weeks: the label's maintenance regimens", {
  units <- strsplit({dd <- getDrugDefaultsGlobal(FALSE); dd$Units[dd$Drug == "emicizumab"]}, ",")[[1]]
  expect_true(all(c("mg/kg SC q2wk", "mg/kg SC q4wk", "mg SC q2wk", "mg SC q4wk") %in% units))
  PK <- getDrugPK("emicizumab", 70, 175, 30, "male", adjustToFFM = FALSE)
  trough <- function(dose, unit, period) {
    w <- simCpCe(data.frame(Drug = "emicizumab", Time = c(0, 4 * MINS_PER_WEEK),
                            Dose = c(3, dose), Units = c("mg/kg SC qwk", unit)),
                 data.frame(Time = numeric(0), Event = character(0)), PK,
                 40 * MINS_PER_WEEK, FALSE)$wide
    last <- w$Time >= (40 - period) * MINS_PER_WEEK
    min(w$Plasma[last])
  }
  # label Table 4 mean steady-state troughs: 46.9 (3 mg/kg q2wk), 38.5 (6 mg/kg q4wk)
  expect_equal(trough(3, "mg/kg SC q2wk", 2), 46.9, tolerance = 0.15)
  expect_equal(trough(6, "mg/kg SC q4wk", 4), 38.5, tolerance = 0.15)
})
