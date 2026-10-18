# oral semaglutide: see R/drugs_semaglutideOral.R (Overgaard 2021, ESM Table 1).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
halfLife <- function(p) {
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  s <- k10 + k12 + k21
  log(2) / ((s - sqrt(s^2 - 4 * k10 * k21)) / 2) / 60     # h
}

test_that("the 85 kg reference subject, switch off, has ESM Table 1's values", {
  p <- semaglutideOral(85, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 0.0348 * 1.09)
  expect_equal(p$cl2 * 60, 0.304)
  expect_equal(p$v1, 3.59)
  expect_equal(p$v2, 4.1)
  expect_equal(p$ka_PO * 60, 2.09)
  expect_equal(p$bioavailability_PO, 0.00795)
  expect_equal(p$tlag_PO, 0)
})

test_that("Table 2's typical values follow at the groups' mean weights", {
  # healthy subjects, 80.7 kg: the diabetes factor divided out
  h <- semaglutideOral(80.7, 175, 47, "male", adjustToFFM = FALSE)$PK$default
  h$cl1 <- h$cl1 / SEMAGLUTIDE_ORAL_T2D_CL
  expect_equal(round(h$cl1 * 60, 3), 0.033)
  expect_equal(round(h$v1, 1), 3.4)
  expect_equal(round(h$v2, 1), 3.9)
  expect_equal(round(h$cl2 * 60, 2), 0.29)          # paper 0.28
  expect_equal(round(100 * h$bioavailability_PO, 2), 0.76)
  expect_equal(halfLife(h), 158, tolerance = 0.01)
  # type 2 diabetes, 89.3 kg; the 0.841 factor on F is the separate analysis's
  d <- semaglutideOral(89.3, 175, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(d$cl1 * 60, 3), 0.040)          # paper 0.039
  expect_equal(round(d$v1, 1), 3.8)                  # paper 3.7
  expect_equal(round(100 * d$bioavailability_PO * 0.841, 2), 0.69)
  expect_equal(halfLife(d), 145, tolerance = 0.01)
})

test_that("weight: positive on F, held to the oral trials' 51.1-122 kg; switch on", {
  f <- function(w) semaglutideOral(w, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(f(60)$bioavailability_PO, 0.00795 * (60 / 85)^0.741)
  expect_equal(f(150)$bioavailability_PO, 0.00795 * (122 / 85)^0.741)
  expect_equal(f(40)$bioavailability_PO, 0.00795 * (51.1 / 85)^0.741)
  expect_equal(f(150)$cl1 * 60, 0.0348 * (150 / 85)^1.01 * 1.09)  # not held
  # exposure F / CL still falls with weight
  expect_lt(f(110)$bioavailability_PO / f(110)$cl1, f(60)$bioavailability_PO / f(60)$cl1)
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- semaglutideOral(120, 175, 50, "male")$PK$default
  expect_equal(on$v1, 3.59 * (size$pkWeight / 85)^0.923)
  expect_equal_rounded(on$cl1 * 60, 0.0421668)
})

test_that("14 mg daily: twice PIONEER's outpatient exposure (supervised dosing)", {
  PK <- getDrugPK("semaglutideOral", 88, 175, 60, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "semaglutideOral", Time = 0, Dose = 14, Units = "mg PO qd"),
               noEvents, PK, 70 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 69 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  nmol <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_DAY / 4.11358
  p <- PK$PK$default
  expect_equal(nmol, p$bioavailability_PO * 14e6 / (p$cl1 * MINS_PER_DAY) / 4113.58,
               tolerance = 0.005)                      # dose x F / CL
  expect_equal(nmol, 29.4, tolerance = 0.01)
  expect_equal(nmol / 14.6, 2.0, tolerance = 0.02)     # Cell Rep Med 2021, PIONEER
  expect_true(all(is.na(w$`Effect Site`)))
})

test_that("units: oral, daily; a long-term drug", {
  expect_equal(getDrugDefaults("semaglutideOral")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_true("semaglutideOral" %in% LONG_TERM_DRUGS)
})
