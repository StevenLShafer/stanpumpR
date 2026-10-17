# dulaglutide: see R/drugs_dulaglutide.R (Geiser 2016, Table 6).
test_that("Table 6 at the 92.5 kg reference", {
  pk <- dulaglutide(92.5, 175, 55, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(pk$bioavailability_SC, 0.470)
  expect_equal(pk$ka_SC * 60, 0.00769)
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(0.0593, 0.0201))
  expect_equal(c(pk$v1, pk$v2), c(2.25, 3.75))
  # the abstract's apparent clearance
  expect_equal(pk$cl1 * 60 / pk$bioavailability_SC, 0.126, tolerance = 0.002)
})
test_that("weight acts on bioavailability, at the pharmacokinetic weight with the switch on", {
  f <- function(w, on = FALSE) dulaglutide(w, 175, 55, "male", on)$PK$default$bioavailability_SC
  expect_equal(f(120), 0.47 * exp(-0.00877 * (120 - 92.5)))
  size <- pkSizeFactors(120, 175, 55, "male", TRUE)
  expect_equal(f(120, TRUE), 0.47 * exp(-0.00877 * (size$pkWeight - 92.5)))
  expect_equal(f(3), 1)   # held at or below 1
})
test_that("1.5 mg weekly at 83 kg is close to the observed steady state", {
  PK <- getDrugPK("dulaglutide", 83, 175, 52, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "dulaglutide", Time = 0, Dose = 1.5, Units = "mg SC qwk"),
               data.frame(Time = numeric(0), Event = character(0)), PK,
               12 * MINS_PER_WEEK, FALSE)$wide
  last <- w$Time >= 11 * MINS_PER_WEEK
  t <- w$Time[last]; cp <- w$Plasma[last]
  auc <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / 60   # ng.h/mL
  expect_equal(auc, 12900, tolerance = 0.01)
  expect_equal(auc, 14000, tolerance = 0.1)     # observed, CV 30% (Table 4)
  expect_equal(max(cp), 114, tolerance = 0.25)  # observed, CV 35%
  expect_true("dulaglutide" %in% LONG_TERM_DRUGS)
})
