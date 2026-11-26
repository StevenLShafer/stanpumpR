# antithrombin: see R/drugs_antithrombin.R (Moffett 2017, Table 3).
moffett <- function(w, base = 60, ufh = 0) {
  b <- min(max(base, 25), 93)
  list(cl = (0.917 * (w / 70)^0.75 + 0.129 * ufh / 173) / 10 / 60,   # L/min
       v = 67.9 * (w / 70) * (b / 60)^-0.389 / 10)                    # L
}
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 3 with the heparin term at 0, switch off", {
  for (w in c(3, 5.2, 70, 120)) {
    pk <- antithrombin(w, 100, 1, "male", FALSE)$PK$default
    ref <- moffett(w)
    expect_equal(c(pk$cl1, pk$v1), c(ref$cl, ref$v))
    expect_equal(c(pk$cl2, pk$cl3), c(0, 0))
  }
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 165, 40, "female", TRUE)
  pk <- antithrombin(110, 165, 40, "female", TRUE)$PK$default
  ref <- moffett(size$pkWeight)
  expect_equal(c(pk$cl1, pk$v1), c(ref$cl, ref$v))
})

test_that("the baseline moves the volume, held within 25 to 93 IU/dL", {
  v <- function(b) antithrombin(70, 175, 40, "male", FALSE, antithrombinBaseline = b)$PK$default$v1
  expect_equal(v(40), moffett(70, 40)$v)
  expect_equal(v(0), moffett(70, 25)$v)
  expect_equal(v(10), v(25))
  expect_equal(v(200), v(93))
  expect_true(is.finite(v(0)))
  expect_equal(antithrombin(70, 175, 40, "male", FALSE)$PK$default$v1, 6.79)
})

test_that("Table 5's median child, and the checks in the header", {
  # 5.2 kg, baseline 60: CL 2.51 mL/kg/h without heparin, 5.04 with 34 units/kg/h
  ref0 <- moffett(5.2); refH <- moffett(5.2, ufh = 34 * 5.2)
  expect_equal(ref0$cl * 60 * 1000 / 5.2, 2.509, tolerance = 1e-3)
  expect_equal(refH$cl * 60 * 1000 / 5.2, 5.040, tolerance = 1e-3)
  expect_equal(ref0$v * 1000 / 5.2, 97.0, tolerance = 1e-3)
  # half-lives, hours: 26.8 without heparin (the text's ~27), 13.3 with
  expect_equal(log(2) * ref0$v / ref0$cl / 60, 26.8, tolerance = 2e-3)
  expect_equal(log(2) * refH$v / refH$cl / 60, 13.33, tolerance = 2e-3)
  # with Table 3's log-normal variability the means are Table 5's
  expect_equal(2.509 * exp(0.475^2 / 2), 2.8, tolerance = 0.01)
  expect_equal(5.040 * exp(0.475^2 / 2), 5.7, tolerance = 0.015)
  expect_equal(97.0 * exp(0.285^2 / 2), 101, tolerance = 0.01)
  # an adult: half-life 51 hours, recovery 1.03 IU/dL per IU/kg
  pk <- antithrombin(70, 175, 40, "male", FALSE)$PK$default
  expect_equal(log(2) * pk$v1 / pk$cl1 / 60, 51.3, tolerance = 2e-3)
  expect_equal(70 / (pk$v1 * 10), 1.031, tolerance = 1e-3)
})

test_that("50 IU/kg: the patient's activity plus the infused, in IU/dL", {
  PK <- getDrugPK("antithrombin", 70, 175, 40, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 60)
  w <- simCpCe(data.frame(Drug = "antithrombin", Time = 60, Dose = 50, Units = "IU/kg"),
               noEvents, PK, 180, FALSE)$wide
  expect_equal(w$Plasma[w$Time < 60], rep(60, sum(w$Time < 60)))
  expect_equal(max(w$Plasma[w$Time == 60]), 60 + 3500 / 67.9, tolerance = 1e-6)
  PK2 <- getDrugPK("antithrombin", 70, 175, 40, "male", adjustToFFM = FALSE,
                   factorBaselines = list(antithrombinBaseline = 40))
  w2 <- simCpCe(data.frame(Drug = "antithrombin", Time = 60, Dose = 50, Units = "IU/kg"),
                noEvents, PK2, 180, FALSE)$wide
  # a lower baseline: drawn from 40, and a larger volume gives a smaller rise
  expect_equal(w2$Plasma[w2$Time < 60], rep(40, sum(w2$Time < 60)))
  expect_equal(max(w2$Plasma[w2$Time == 60]), 40 + 3500 / (67.9 * (40 / 60)^-0.389),
               tolerance = 1e-6)
  expect_true("antithrombin" %in% patientFieldReaders("antithrombinBaseline"))
})
