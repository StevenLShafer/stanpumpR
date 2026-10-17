# insulinHumanMix70: see R/insulinTham.R (Tham 2017: 7 U NPH + 3 U regular per 10 U).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
sim <- function(drug, dose) {
  PK <- getDrugPK(drug, 70, 170, 35, "male")
  simCpCe(data.frame(Drug = drug, Time = 0, Dose = dose, Units = "U SC"), noEvents, PK, 1440, FALSE)
}
test_that("two depots: regular fast with no lag, NPH slow with its lag", {
  pk <- insulinHumanMix70(70, 170, 35, "male", FALSE)$PK$default
  expect_equal(pk$ka_SC, 0.67 / 60); expect_equal(pk$tlag_SC, 0)
  expect_equal(pk$ka_SC_slow, 0.129 / 60); expect_equal(pk$tlag_SC_slow, 0.378 * 60)
  expect_equal(pk$fraction_SC_slow, 0.7)
})
test_that("10 U of 70/30 is exactly 7 U NPH plus 3 U regular", {
  mix <- sim("insulinHumanMix70", 10)
  nph <- sim("insulinNPH", 7)
  reg <- sim("insulinRegular", 3)
  at <- function(s, t) stats::approx(s$wide$Time, s$wide$Plasma, xout = t)$y
  # compared at the mix's own time points, where each curve is exact for the
  # mix; the components' grids differ, so compare at shared points
  t <- intersect(intersect(mix$wide$Time, nph$wide$Time), reg$wide$Time)
  expect_gt(length(t), 10)
  expect_equal(at(mix, t), at(nph, t) + at(reg, t), tolerance = 1e-9)
})
