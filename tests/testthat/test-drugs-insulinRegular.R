# insulinRegular: see R/insulinTham.R (Tham 2017, Table 2 and the
# supplement's equations).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
test_that("Table 2 at the reference weight and dose, with Tham's covariates", {
  ref <- insulinRegular(80, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(ref$v1, 178); expect_equal(ref$cl1, 127 / 60)
  expect_equal(ref$ka_SC, 0.67 / 60); expect_equal(ref$tlag_SC, 0)
  heavy <- insulinRegular(110, 175, 40, "male", FALSE)$PK$default
  expect_equal(heavy$v1, 178 * (110 / 80)^1.62)
  expect_equal(insulinRegular(150, 175, 40, "male", FALSE)$PK$default$v1,
               178 * (124 / 80)^1.62)                  # held at the studied 124 kg
  d30 <- insulinRegular(80, 175, 40, "male", FALSE, dose = 30)$PK$default
  expect_equal(d30$cl1, 127 * 3^-0.282 / 60)
  expect_equal(d30$ka_SC, 0.67 * 3^-0.275 / 60)
  expect_equal(insulinRegular(80, 175, 40, "male", FALSE, dose = 500)$PK$default$cl1,
               127 * 10^-0.282 / 60)                   # held at the studied 100 U
  # switch on: the pharmacokinetic weight
  size <- pkSizeFactors(100, 175, 40, "male", TRUE)
  expect_equal(insulinRegular(100, 175, 40, "male")$PK$default$v1,
               178 * (size$pkWeight / 80)^1.62)
  expect_null(insulinRegular(80, 175, 40, "male")$zeroOrderSC)
  expect_true(insulinRegular(80, 175, 40, "male")$doseDependent)
})
test_that("each injection is simulated with the parameters for its size", {
  PK <- getDrugPK("insulinRegular", 80, 175, 40, "male", adjustToFFM = FALSE)
  bateman <- function(D, t0, t) {
    ka <- 0.67 * (D / 10)^-0.275 / 60; k <- 127 * (D / 10)^-0.282 / 60 / 178
    s <- pmax(t - t0, 0)
    D * 1000 * ka / (178 * (ka - k)) * (exp(-k * s) - exp(-ka * s))
  }
  w <- simCpCe(data.frame(Drug = "insulinRegular", Time = c(0, 240), Dose = c(10, 30),
                          Units = "U SC"), noEvents, PK, 1440, FALSE)$wide
  expect_equal(w$Plasma, bateman(10, 0, w$Time) + bateman(30, 240, w$Time), tolerance = 1e-9)
  # 10 U, the reference dose: a peak of about 20 mcU/mL some 87 minutes on
  one <- simCpCe(data.frame(Drug = "insulinRegular", Time = 0, Dose = 10, Units = "U SC"),
                 noEvents, PK, 600, FALSE)$wide
  expect_equal(max(one$Plasma), 20.0, tolerance = 0.01)
  # U/kg doses are sized in U: 0.25 U/kg at 80 kg is 20 U
  kg <- simCpCe(data.frame(Drug = "insulinRegular", Time = 0, Dose = 0.25, Units = "U/kg SC"),
                noEvents, PK, 600, FALSE)$wide
  expect_equal(kg$Plasma, bateman(20, 0, kg$Time), tolerance = 1e-9)
  units <- getDrugDefaults("insulinRegular")$Units[[1]]
  expect_false(any(units %in% c(insulinRateUnits, bolusUnits, infusionUnits)))
})
