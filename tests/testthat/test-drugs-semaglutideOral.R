# oral semaglutide: see R/drugs_semaglutideOral.R (Overgaard 2021, Cell Rep
# Med, PIONEER outpatients; Methods S1 and Supplementary Table 1).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("the 85 kg reference woman, switch off, has Table S1's values", {
  p <- semaglutideOral(85, 165, 50, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 0.039)
  expect_equal(p$cl2 * 60, 0.304)
  expect_equal(p$v1, 3.59)
  expect_equal(p$v2, 4.1)
  expect_equal(p$ka_PO * 60, 2.09)
  expect_equal(p$bioavailability_PO, 0.00471)
  expect_equal(p$tlag_PO, 0)
  # the fixed clearance is Overgaard 2019's 0.0348 L/h in type 2 diabetes (x 1.12)
  expect_equal(0.0348 * 1.12, 0.039, tolerance = 0.001)
})

test_that("men: F x 0.814; weight on F to the power 0.374, not held", {
  f <- function(w, s) semaglutideOral(w, 170, 50, s, adjustToFFM = FALSE)$PK$default
  expect_equal(f(85, "male")$bioavailability_PO, 0.00471 * 0.814)
  expect_equal(f(85, "male")$cl1, f(85, "female")$cl1)
  expect_equal(f(40, "female")$bioavailability_PO, 0.00471 * (40 / 85)^0.374)
  expect_equal(f(180, "female")$bioavailability_PO, 0.00471 * (180 / 85)^0.374)
  expect_equal(f(180, "female")$cl1 * 60, 0.039 * (180 / 85)^1.01)
  expect_equal(f(180, "female")$v2, 4.1 * (180 / 85)^0.923)
  # exposure F / CL falls with weight
  expect_lt(f(120, "female")$bioavailability_PO / f(120, "female")$cl1,
            f(60, "female")$bioavailability_PO / f(60, "female")$cl1)
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- semaglutideOral(120, 175, 50, "male")$PK$default
  expect_equal(on$v1, 3.59 * (size$pkWeight / 85)^0.923)
  expect_equal(on$bioavailability_PO, 0.00471 * 0.814 * (size$pkWeight / 85)^0.374)
})

test_that("14 mg daily reproduces PIONEER's average concentration (14.6 nmol/L)", {
  cavg <- function(w, s) {
    PK <- getDrugPK("semaglutideOral", w, 170, 60, s, adjustToFFM = FALSE)
    out <- simCpCe(data.frame(Drug = "semaglutideOral", Time = 0, Dose = 14, Units = "mg PO qd"),
                   noEvents, PK, 70 * MINS_PER_DAY, FALSE)$wide
    last <- out$Time >= 69 * MINS_PER_DAY
    t <- out$Time[last]; cp <- out$Plasma[last]
    nmol <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_DAY / 4.11358
    p <- PK$PK$default
    expect_equal(nmol, p$bioavailability_PO * 14e6 / (p$cl1 * MINS_PER_DAY) / 4113.58,
                 tolerance = 0.005)                    # dose x F / CL
    expect_true(all(is.na(out$`Effect Site`)))
    nmol
  }
  woman <- cavg(85, "female"); man <- cavg(88, "male")
  expect_equal(woman, 17.1, tolerance = 0.01)
  expect_equal(man, 13.6, tolerance = 0.01)
  # the PIONEER geometric mean lies between them
  expect_gt(14.6, man); expect_lt(14.6, woman)
  # the supervised model this replaced (Overgaard 2021, Clin Pharmacokinet,
  # 0.795% x (88/85)^0.741 and CL 0.0348 x 1.09 x (88/85)^1.01) gave 29.4
  supervised <- 0.00795 * (88 / 85)^0.741 * 14 /
    (0.0348 * 1.09 * (88 / 85)^1.01 * 24) / 4113.58 * 1e6   # mg/L to nmol/L
  expect_equal(supervised, 29.4, tolerance = 0.01)
  expect_equal(man / supervised, 0.46, tolerance = 0.02)
})

test_that("units: oral, daily; a long-term drug", {
  expect_equal(getDrugDefaults("semaglutideOral")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_true("semaglutideOral" %in% LONG_TERM_DRUGS)
})

test_that("a named sex string selects the same parameters", {
  expect_identical(semaglutideOral(85, 170, 50, c(patient = "male"), FALSE)$PK,
                   semaglutideOral(85, 170, 50, "male", FALSE)$PK)
})
