# insulinHumanMix70: see R/insulinTham.R (Tham 2017: 7 U NPH + 3 U regular per 10 U).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
sim <- function(drug, dose) {
  PK <- getDrugPK(drug, 70, 170, 35, "male")
  simCpCe(data.frame(Drug = drug, Time = 0, Dose = dose, Units = "U SC"), noEvents, PK, 1440, FALSE)
}
test_that("two depots: regular fast with no lag, NPH slow with its lag", {
  pk <- insulinHumanMix70(80, 175, 35, "male", FALSE)$PK$default
  expect_equal(pk$ka_SC, 0.67 / 60); expect_equal(pk$tlag_SC, 0)
  expect_equal(pk$ka_SC_slow, 0.129 / 60); expect_equal(pk$tlag_SC_slow, 0.378 * 60)
  expect_equal(pk$fraction_SC_slow, 0.7)
})
test_that("10 U of 70/30 is 7 U NPH plus 3 U regular, cleared as one 10 U dose", {
  PK <- getDrugPK("insulinHumanMix70", 80, 175, 40, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinHumanMix70", Time = 0, Dose = 10, Units = "U SC"),
               noEvents, PK, 1440, FALSE)$wide
  # CL/F at the whole human-insulin dose; regular ka at its 3 U share
  k <- 127 / 60 / 178
  bateman <- function(D, ka, lag, t) {
    s <- pmax(t - lag, 0)
    D * 1000 * ka / (178 * (ka - k)) * (exp(-k * s) - exp(-ka * s))
  }
  exact <- bateman(7, 0.129 / 60, 0.378 * 60, w$Time) +
    bateman(3, 0.67 * 0.3^-0.275 / 60, 0, w$Time)
  expect_equal(w$Plasma, exact, tolerance = 1e-9)
})
